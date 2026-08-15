import assert from "node:assert/strict";
import test from "node:test";
import { documentSelector } from "../client/options";
test("language support covers file, untitled, source-control, and diff documents", () => {
  assert.deepEqual(documentSelector, [{ language: "tung" }]);
});
