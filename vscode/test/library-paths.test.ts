import assert from "node:assert/strict";
import * as path from "node:path";
import test from "node:test";
import { resolveLibraryPaths } from "../server/library-paths.ts";

test("library paths keep pinned roots before configured and inherited roots", () => {
  const pinned = path.resolve("pinned");
  const configured = path.resolve("configured");
  const inherited = path.resolve("inherited");
  assert.deepEqual(
    resolveLibraryPaths(
      [pinned],
      [configured, inherited],
      [inherited, pinned].join(path.delimiter),
    ),
    [pinned, configured, inherited],
  );
  assert.deepEqual(resolveLibraryPaths([], [], undefined), []);
});
