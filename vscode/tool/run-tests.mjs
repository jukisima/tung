import { spawnSync } from "node:child_process";
import { readdirSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { compilerExecutable } from "./compiler-executable.mjs";

const directory = fileURLToPath(new URL("../out/test/", import.meta.url));
const files = readdirSync(directory)
  .filter((file) => file.endsWith(".test.js"))
  .map((file) => join(directory, file));
const executable = compilerExecutable();
const result = spawnSync(process.execPath, ["--test", ...files], {
  stdio: "inherit",
  env: executable
    ? { ...process.env, TUNG_EXECUTABLE: executable }
    : process.env,
});
if (result.error) throw result.error;
process.exitCode = result.status ?? 1;
