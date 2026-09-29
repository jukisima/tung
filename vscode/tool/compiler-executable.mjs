import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";

const tongue = fileURLToPath(new URL("../../tongue/", import.meta.url));

export const compilerExecutable = () => {
  if (process.env.TUNG_EXECUTABLE) return process.env.TUNG_EXECUTABLE;
  try {
    const executable = execFileSync("cabal", ["list-bin", "exe:tung"], {
      cwd: tongue,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
    if (existsSync(executable)) return executable;
  } catch {
    // an installed compiler may still be available on PATH.
  }
  return undefined;
};
