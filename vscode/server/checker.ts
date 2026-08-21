// persistent bridge to the authoritative haskell checker; responses carry request versions.
import * as childProcess from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
class CompilerBridge {
  tongue: string | undefined;
  executable: string | undefined;
  session: any;
  pending: Map<number, any>;
  nextRequestId: number;
  output: string;
  responseFrame: any;
  constructor() {
    this.tongue = undefined;
    this.executable = undefined;
    this.session = undefined;
    this.pending = new Map();
    this.nextRequestId = 1;
    this.output = "";
    this.responseFrame = undefined;
  }
  configure(tongue) {
    if (tongue !== this.tongue) {
      this.dispose();
      this.executable = undefined;
    }
    this.tongue = tongue;
  }
  available() {
    return Boolean(this.findExecutable());
  }
  check(model, imports, version = 0) {
    return this.request("check", model, imports, "", version);
  }
  typeOf(model, imports, name, version = 0) {
    return this.request("type", model, imports, name, version);
  }
  request(command, model, imports, name, version) {
    const executable = this.findExecutable();
    if (!executable) {
      return {
        process: undefined,
        requestId: 0,
        version,
        result: Promise.resolve("checker is not built"),
        cancel() {},
      };
    }
    const process = this.ensureSession(executable);
    const requestId = this.nextRequestId++;
    let resolveResult;
    const result = new Promise<string>((resolve) => (resolveResult = resolve));
    this.pending.set(requestId, { resolve: resolveResult, version });
    process.stdin.write(
      sessionRequest(requestId, version, command, name, model.text, imports),
    );
    return {
      process,
      requestId,
      version,
      result,
      cancel: () => this.cancel(requestId, version),
    };
  }
  ensureSession(executable) {
    if (
      this.session && this.session.exitCode === null && !this.session.killed
    ) {
      return this.session;
    }
    const process = childProcess.spawn(executable, ["--editor-session"], {
      cwd: this.tongue,
      stdio: ["pipe", "pipe", "pipe"],
    });
    this.session = process;
    this.output = "";
    this.responseFrame = undefined;
    let stderr = "";
    process.stdout.setEncoding("utf8");
    process.stderr.setEncoding("utf8");
    process.stdout.on("data", (chunk) => this.consume(chunk));
    process.stderr.on("data", (chunk) => (stderr += chunk));
    process.on("error", (error) => {
      this.failSession(
        process,
        `could not run the haskell checker: ${error.message}`,
      );
    });
    process.on("close", () => {
      const detail = stderr.trim();
      this.failSession(
        process,
        detail
          ? `haskell checker session stopped: ${detail}`
          : "haskell checker session stopped",
      );
    });
    process.stdin.on("error", () => {});
    return process;
  }
  consume(chunk) {
    this.output += chunk;
    while (true) {
      if (!this.responseFrame) {
        const newline = this.output.indexOf("\n");
        if (newline < 0) return;
        const header = this.output.slice(0, newline).replace(/\r$/, "");
        this.output = this.output.slice(newline + 1);
        const fields = header.split("\t");
        if (fields.length !== 4 || fields[0] !== "tung-response") continue;
        this.responseFrame = {
          requestId: Number(fields[1]),
          version: Number(fields[2]),
          length: Number(fields[3]),
        };
      }
      const characters = Array.from(this.output);
      if (characters.length < this.responseFrame.length + 1) return;
      const response = characters.slice(0, this.responseFrame.length).join("");
      this.output = characters.slice(this.responseFrame.length).join("");
      if (this.output.startsWith("\r\n")) this.output = this.output.slice(2);
      else if (this.output.startsWith("\n")) this.output = this.output.slice(1);
      const { requestId, version } = this.responseFrame;
      this.responseFrame = undefined;
      const pending = this.pending.get(requestId);
      if (!pending || pending.version !== version) continue;
      this.pending.delete(requestId);
      pending.resolve(response);
    }
  }
  cancel(requestId, version) {
    const pending = this.pending.get(requestId);
    if (!pending) return;
    this.pending.delete(requestId);
    pending.resolve("checker request cancelled");
    if (this.session?.exitCode === null && !this.session.killed) {
      this.session.stdin.write(
        sessionRequest(requestId, version, "cancel", "", "", []),
      );
    }
  }
  failSession(process, message) {
    if (this.session !== process) return;
    this.session = undefined;
    this.output = "";
    this.responseFrame = undefined;
    for (const { resolve } of this.pending.values()) resolve(message);
    this.pending.clear();
  }
  dispose() {
    const process = this.session;
    this.session = undefined;
    for (const { resolve } of this.pending.values()) {
      resolve("checker request cancelled");
    }
    this.pending.clear();
    this.output = "";
    this.responseFrame = undefined;
    if (process && process.exitCode === null && !process.killed) process.kill();
  }
  findExecutable() {
    if (!this.tongue) return undefined;
    if (this.executable && fs.existsSync(this.executable)) {
      return this.executable;
    }
    this.executable = this.findCabalExecutable() ||
      this.findDistExecutable();
    return this.executable;
  }
  findCabalExecutable() {
    const result = childProcess.spawnSync(
      "cabal",
      ["list-bin", "exe:tung"],
      {
        cwd: this.tongue,
        encoding: "utf8",
      },
    );
    const found = result.status === 0 ? result.stdout.trim() : "";
    return found && fs.existsSync(found) ? found : undefined;
  }
  findDistExecutable() {
    const root = path.join(this.tongue, "dist-newstyle", "build");
    const found = findFile(
      root,
      (file) => file.endsWith(path.join("x", "tung", "build", "tung", "tung")),
    );
    return found && fs.existsSync(found) ? found : undefined;
  }
}
const findFile = (root, predicate) => {
  if (!fs.existsSync(root)) return undefined;
  const stack = [root];
  while (stack.length > 0) {
    const current = stack.pop();
    const stat = fs.statSync(current);
    if (stat.isFile() && predicate(current)) return current;
    if (stat.isDirectory()) {
      for (const entry of fs.readdirSync(current)) {
        stack.push(path.join(current, entry));
      }
    }
  }
  return undefined;
};
const parseCompilerDiagnostic = (output) => {
  const trimmed = output.trim();
  if (trimmed === "tung-ok") return undefined;
  const [header = "", ...body] = output.split(/\r?\n/);
  const fields = header.split("\t");
  if (fields.length === 5 && fields[0] === "tung-diagnostic") {
    const start = Number(fields[3]);
    const end = Number(fields[4]);
    return {
      kind: fields[1],
      path: fields[2] || undefined,
      start,
      end,
      message: body.join("\n").trim(),
    };
  }
  if (fields.length === 4 && fields[0] === "tung-diagnostic") {
    return {
      kind: fields[1],
      start: Number(fields[2]),
      end: Number(fields[3]),
      message: body.join("\n").trim(),
    };
  }
  const message = output
    .split(/\r?\n/)
    .map((line) => line.trim())
    .find((line) => /^(parse error|type error)\b/.test(line));
  return { message: message || trimmed || "checker gave no output" };
};
const sourceBundle = (source, imports) => {
  return `(${haskellString(source)},[${
    imports.map(([name, imported]) =>
      `(${haskellString(name)},${haskellString(imported)})`
    ).join(",")
  }])`;
};
const sessionRequest = (
  requestId,
  version,
  command,
  name,
  source,
  imports,
) => {
  return `(${requestId},${version},${haskellString(command)},${
    haskellString(name)
  },${sourceBundle(source, imports)})\n`;
};
const haskellString = (value) => {
  let result = '"';
  for (const character of value) {
    const code = character.codePointAt(0);
    if (character === '"') result += '\\"';
    else if (character === "\\") result += "\\\\";
    else if (character === "\n") result += "\\n";
    else if (character === "\r") result += "\\r";
    else if (character === "\t") result += "\\t";
    else if (code < 32 || code === 127) result += `\\${code}\\&`;
    else result += character;
  }
  return `${result}"`;
};
export {
  CompilerBridge,
  haskellString,
  parseCompilerDiagnostic,
  sessionRequest,
  sourceBundle,
};
