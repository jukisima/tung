import assert from "node:assert/strict";
import test from "node:test";
import { formatRangeEdit } from "../server/format.ts";

test("range formatting returneth requested full lines from formatted source", () => {
  const source = [
    "show kin a box {",
    "a box",
    "}",
    "let value = {",
    "x | x",
    "}",
    "",
  ].join("\n");
  const formatted = [
    "show kin a box {",
    "  a box",
    "}",
    "let value = {",
    "  x | x",
    "}",
    "",
  ].join("\n");
  assert.deepEqual(
    formatRangeEdit(source, formatted, {
      start: { line: 3, character: 0 },
      end: { line: 6, character: 0 },
    }),
    {
      range: {
        start: { line: 3, character: 0 },
        end: { line: 5, character: 1 },
      },
      newText: "let value = {\n  x | x\n}",
    },
  );
});

test("range formatting returneth null when the source is unchanged", () => {
  const source = "show kin a box {\n  a box\n}\n";
  assert.equal(
    formatRangeEdit(source, source, {
      start: { line: 0, character: 0 },
      end: { line: 3, character: 0 },
    }),
    null,
  );
});
