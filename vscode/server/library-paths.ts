// library roots shared by editor indexing, checking, and running.
import * as childProcess from "node:child_process";
import * as path from "node:path";

const resolveLibraryPaths = (
  declared: string[],
  configured: unknown,
  inherited: string | undefined,
): string[] => {
  const settings = Array.isArray(configured)
    ? configured.filter((entry): entry is string =>
      typeof entry === "string" && Boolean(entry)
    )
    : [];
  const environment = inherited?.split(path.delimiter).filter(Boolean) || [];
  return [
    ...new Set(
      [...declared, ...settings, ...environment].map((root) =>
        path.resolve(root)
      ),
    ),
  ];
};

const readDeclaredLibraryPaths = (
  executable: string | undefined,
  projects: string[],
): string[] => {
  if (!executable) return [];
  const roots: string[] = [];
  for (const project of projects) {
    try {
      const output = childProcess.execFileSync(
        executable,
        ["--library-paths", project],
        { encoding: "utf8", timeout: 60_000 },
      );
      roots.push(...output.split(/\r?\n/u).filter(Boolean));
    } catch (error) {
      const output = error && typeof error === "object" && "stdout" in error
        ? String(error.stdout).trim()
        : "";
      throw new Error(
        `cannot resolve libraries for ${project}: ${output || String(error)}`,
      );
    }
  }
  return roots;
};

export { readDeclaredLibraryPaths, resolveLibraryPaths };
