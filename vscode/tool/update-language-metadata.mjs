import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { compilerExecutable } from "./compiler-executable.mjs";

const target = fileURLToPath(new URL("../generated/language-names.json", import.meta.url));
const executable = compilerExecutable() || "tung";
const result = spawnSync(executable, ["--language-metadata"], {
  encoding: "utf8",
});

if (result.error || result.status !== 0) {
  const detail = result.error?.message || result.stderr?.trim() || `exit ${result.status}`;
  throw new Error(`could not get language metadata from ${executable}: ${detail}`);
}

const metadata = JSON.parse(result.stdout);
if (
  !Array.isArray(metadata.keywords) ||
  !Array.isArray(metadata.primitiveTypes) ||
  typeof metadata.specialNameChars !== "string"
) {
  throw new Error(`${executable} returned invalid language metadata`);
}

const current = readFileSync(target, "utf8");
if (current === result.stdout) {
  process.stdout.write("language metadata is current\n");
} else if (process.argv.includes("--check")) {
  throw new Error("language metadata differs from tung; run npm run update:metadata");
} else {
  writeFileSync(target, result.stdout);
  process.stdout.write("updated generated/language-names.json\n");
}
