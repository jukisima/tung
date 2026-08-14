const childProcess = require("child_process");
const fs = require("fs");

class CompilerBridge {
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
    return this.run(["--check-editor-bundle-stdin"], sourceBundle(model.text, imports), true);
  }

  typeOf(model, imports, name) {
    return this.run(["--type-of-bundle-stdin", name], sourceBundle(model.text, imports), false);
  }

  run(args, input, includeStderr) {
    const executable = this.findExecutable();
    if (!executable) return { process: undefined, result: Promise.resolve("checker is not built") };
    const process = childProcess.spawn(executable, args, { cwd: this.tongue, stdio: ["pipe", "pipe", "pipe"] });
    const result = new Promise((resolve) => {
      let output = "";
      let finished = false;
      const finish = (message = output) => {
        if (finished) return;
        finished = true;
        resolve(message);
      };
      process.stdout.on("data", (chunk) => (output += chunk));
      if (includeStderr) process.stderr.on("data", (chunk) => (output += `\n${chunk}`));
      process.on("error", (error) => finish(`could not run the haskell checker: ${error.message}`));
      process.on("close", () => finish());
      process.stdin.on("error", () => {});
      process.stdin.end(input);
    });
    return { process, result };
  }

  findExecutable() {
    if (!this.tongue) return undefined;
    if (this.executable && fs.existsSync(this.executable)) return this.executable;
    const result = childProcess.spawnSync("cabal", ["list-bin", "exe:tung"], { cwd: this.tongue, encoding: "utf8" });
    const found = result.status === 0 ? result.stdout.trim() : "";
    this.executable = found && fs.existsSync(found) ? found : undefined;
    return this.executable;
  }
}

function sourceBundle(source, imports) {
  return `(${haskellString(source)},[${imports.map(([name, imported]) => `(${haskellString(name)},${haskellString(imported)})`).join(",")}])`;
}

function haskellString(value) {
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
}

module.exports = { CompilerBridge, haskellString, sourceBundle };
