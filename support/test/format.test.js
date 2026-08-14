const assert = require("node:assert/strict");
const test = require("node:test");
const { formatDocument, formatRange } = require("../server/format");

test("formatter indents nested declarations and keeps comments and strings stable", () => {
  const source = "show choose a box {\na box\n}\nlet text = '{not a block}'; # }\n";
  assert.equal(formatDocument(source, { tabSize: 2, insertSpaces: true }), "show choose a box {\n  a box\n}\nlet text = '{not a block}'; # }\n");
});

test("formatter is idempotent", () => {
  const source = "deed ask {\n  integer ask: integer\n}\n";
  assert.equal(formatDocument(formatDocument(source)), formatDocument(source));
});

test("formatter preserves current application syntax", () => {
  const source = [
    "show let (x: a option, f: a → 𝟚) filter: a option = x match {",
    "a some | a f $ if (a some) none",
    "none | none",
    "};",
    ""
  ].join("\n");
  assert.equal(formatDocument(source), [
    "show let (x: a option, f: a → 𝟚) filter: a option = x match {",
    "  a some | a f $ if (a some) none",
    "  none | none",
    "};",
    ""
  ].join("\n"));
});

test("range formatter returns only requested full lines with surrounding indentation context", () => {
  const source = [
    "show choose a box {",
    "a box",
    "}",
    "let value = {",
    "x | x",
    "};",
    ""
  ].join("\n");
  assert.deepEqual(formatRange(source, { start: { line: 3, character: 0 }, end: { line: 6, character: 0 } }), {
    range: { start: { line: 3, character: 0 }, end: { line: 5, character: 2 } },
    newText: "let value = {\n  x | x\n};"
  });
});

test("range formatter returns null when the range is already formatted", () => {
  const source = "show choose a box {\n  a box\n}\n";
  assert.equal(formatRange(source, { start: { line: 0, character: 0 }, end: { line: 3, character: 0 } }), null);
});
