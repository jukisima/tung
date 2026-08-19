// bridge to the authoritative haskell checker; executable discovery is cached per tongue root.
import * as childProcess from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
class CompilerBridge {
  tongue: string | undefined;
  executable: string | undefined;
  constructor() {
    this.tongue = undefined;
    this.executable = undefined;
  }
  configure(tongue) {
    if (tongue !== this.tongue) this.executable = undefined;
    this.tongue = tongue;
  }
  available() {
    return Boolean(this.findExecutable());
  }
  check(model, imports) {
    return this.run(
      ["--check-editor-diagnostic-bundle-stdin"],
      sourceBundle(model.text, imports),
      true,
    );
  }
  typeOf(model, imports, name) {
    return this.run(
      ["--type-of-bundle-stdin", name],
      sourceBundle(model.text, imports),
      false,
    );
  }
  run(args, input, includeStderr) {
    const executable = this.findExecutable();
    if (!executable) {
      return {
        process: undefined,
        result: Promise.resolve("checker is not built"),
      };
    }
    const process = childProcess.spawn(executable, args, {
      cwd: this.tongue,
      stdio: ["pipe", "pipe", "pipe"],
    });
    const result = new Promise<string>((resolve) => {
      let output = "";
      let finished = false;
      const finish = (message = output) => {
        if (finished) return;
        finished = true;
        resolve(message);
      };
      process.stdout.on("data", (chunk) => (output += chunk));
      if (includeStderr) {
        process.stderr.on("data", (chunk) => (output += `\n${chunk}`));
      }
      process.on(
        "error",
        (error) =>
          finish(
            `could not run the haskell checker: ${error.message}`,
          ),
      );
      process.on("close", () => finish());
      process.stdin.on("error", () => {});
      process.stdin.end(input);
    });
    return { process, result };
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
  if (fields.length === 4 && fields[0] === "tung-diagnostic") {
    const start = Number(fields[2]);
    const end = Number(fields[3]);
    return {
      kind: fields[1],
      start,
      end,
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
export { CompilerBridge, haskellString, parseCompilerDiagnostic, sourceBundle };
