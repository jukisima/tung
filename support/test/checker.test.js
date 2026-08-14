const assert = require("node:assert/strict");
const test = require("node:test");
const { haskellString, sourceBundle } = require("../server/checker");

test("checker bundle preserves unicode, quotes, slashes, and control characters", () => {
  assert.equal(haskellString("𝟙\n\"\\\t"), '"𝟙\\n\\"\\\\\\t"');
  assert.equal(sourceBundle("main", [["dep.tung", "source"]]), '("main",[("dep.tung","source")])');
});
