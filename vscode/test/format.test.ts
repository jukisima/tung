import assert from "node:assert/strict";
import test from "node:test";
import { formatDocument, formatRange } from "../server/format.ts";
test("formatter indents nested declarations and keepeth comments and strings stable", () => {
  const source =
    "show kin a box {\na box\n}\nlet text = '{not a block}'; # }\n";
  assert.equal(
    formatDocument(source, { tabSize: 2, insertSpaces: true }),
    "show kin a box {\n  a box\n}\nlet text = '{not a block}'; # }\n",
  );
});
test("formatter is idempotent", () => {
  const source = "deed ask {\n  integer ask: integer\n}\n";
  assert.equal(formatDocument(formatDocument(source)), formatDocument(source));
});
test("formatter is idempotent for generated structural sources", () => {
  const declarations = [
    ["show kin a box {", "a box", "}"],
    ["let result =", "match value {", "yea | 1,", "nay | 0", "};"],
    ["let handled =", "try risky {", "message fail | 0", "};"],
    [
      "shape a identity {",
      "a identity: a;",
      "law (x: a):",
      "x identity ~ x",
      "}",
    ],
  ];
  const indentation = ["", " ", "  ", "\t"];
  const endings = ["", "\n"];
  for (const declaration of declarations) {
    for (const prefix of indentation) {
      for (const ending of endings) {
        const source = declaration.map((line) => prefix + line).join("\n") +
          ending;
        const once = formatDocument(source);
        assert.equal(formatDocument(once), once, source);
      }
    }
  }
});
test("formatter preserveth current application syntax", () => {
  const source = [
    "show let (x: a option, f: a → 𝟚) filter: a option = x match {",
    "a some | a f $ if (a some) none",
    "none | none",
    "};",
    "",
  ].join("\n");
  assert.equal(
    formatDocument(source),
    [
      "show let (x: a option, f: a → 𝟚) filter: a option = x match {",
      "  a some | a f $ if (a some) none",
      "  none | none",
      "};",
      "",
    ].join("\n"),
  );
});
test("formatter indents expression continuations after let equals", () => {
  const source = [
    "let picked =",
    "match value {",
    "yea | 1,",
    "nay | 0",
    "};",
    "let handled =",
    "try risky {",
    "message fail | 0",
    "};",
    "let id =",
    "{ x | x };",
    "",
  ].join("\n");
  assert.equal(
    formatDocument(source),
    [
      "let picked =",
      "  match value {",
      "    yea | 1,",
      "    nay | 0",
      "  };",
      "let handled =",
      "  try risky {",
      "    message fail | 0",
      "  };",
      "let id =",
      "  { x | x };",
      "",
    ].join("\n"),
  );
});
test("formatter indenteth latter lines of a split right side", () => {
  const source = [
    "fill (float complex) elementary {",
    "let (a complex b) sine =",
    "(((0.0 subtract-float b) complex a) exponent)",
    "sine-from-exponents ((b complex (0.0 subtract-float a)) exponent);",
    "}",
    "",
  ].join("\n");
  const expected = [
    "fill (float complex) elementary {",
    "  let (a complex b) sine =",
    "    (((0.0 subtract-float b) complex a) exponent)",
    "      sine-from-exponents ((b complex (0.0 subtract-float a)) exponent);",
    "}",
    "",
  ].join("\n");
  assert.equal(formatDocument(source), expected);
  assert.equal(formatDocument(expected), expected);
});
test("formatter indents split let annotations and definitions", () => {
  const source = [
    "show let (f0: a → b ! e0, f1: b → c ! e1) compose",
    ": a → c ! e0, e1",
    "= { a | a f0 $ f1 };",
    "",
  ].join("\n");
  const expected = [
    "show let (f0: a → b ! e0, f1: b → c ! e1) compose",
    "  : a → c ! e0, e1",
    "  = { a | a f0 $ f1 };",
    "",
  ].join("\n");
  assert.equal(formatDocument(source), expected);
  assert.equal(formatDocument(expected), expected);
});
test("formatter preserveth term type ascriptions", () => {
  const source = "let answer = (1 + 2: integer);\n";
  assert.equal(formatDocument(source), source);
});
test("formatter indents equations after multiline law headers", () => {
  const source = [
    "shape f applicative {",
    "law (a: a f):",
    "a apply (id pure) ~ a;",
    "law (a: a): a pure ~ a pure",
    "}",
    "",
  ].join("\n");
  assert.equal(
    formatDocument(source),
    [
      "shape f applicative {",
      "  law (a: a f):",
      "    a apply (id pure) ~ a;",
      "  law (a: a): a pure ~ a pure",
      "}",
      "",
    ].join("\n"),
  );
});
test("formatter ignoreth equals signs in strings and comments for continuation indentation", () => {
  const source = ["let text = 'a = b'; # =", "let next = 1;", ""].join("\n");
  assert.equal(formatDocument(source), source);
});
test("formatter treateth a decimal unicode terminator as part of its character", () => {
  const source = [
    "let result =",
    "match character {",
    "`\\32; | 1,",
    "_ | 0",
    "};",
    "",
  ].join("\n");
  assert.equal(
    formatDocument(source),
    [
      "let result =",
      "  match character {",
      "    `\\32; | 1,",
      "    _ | 0",
      "  };",
      "",
    ].join("\n"),
  );
});
test("formatter ignoreth delimiters in block comments", () => {
  const source = [
    "let x = 1 /* { = */",
    "let y = 2;",
    "/*",
    "  {",
    "*/",
    "let z = 3;",
    "",
  ].join(
    "\n",
  );
  assert.equal(formatDocument(source), source);
});
test("range formatter returneth only requested full lines with surrounding indentation context", () => {
  const source = [
    "show kin a box {",
    "a box",
    "}",
    "let value = {",
    "x | x",
    "};",
    "",
  ].join("\n");
  assert.deepEqual(
    formatRange(source, {
      start: { line: 3, character: 0 },
      end: { line: 6, character: 0 },
    }),
    {
      range: {
        start: { line: 3, character: 0 },
        end: { line: 5, character: 2 },
      },
      newText: "let value = {\n  x | x\n};",
    },
  );
});
test("range formatter returneth null when the range is already formatted", () => {
  const source = "show kin a box {\n  a box\n}\n";
  assert.equal(
    formatRange(source, {
      start: { line: 0, character: 0 },
      end: { line: 3, character: 0 },
    }),
    null,
  );
});
