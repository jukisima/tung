import assert from "node:assert/strict";
import test from "node:test";
import { documentSelector } from "../client/options.ts";
test("language support covereþ file, untitled, source-control, and diff documents", () => {
  assert.deepEqual(documentSelector, [{ language: "tung" }]);
});
