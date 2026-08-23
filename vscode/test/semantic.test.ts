import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import test from "node:test";
import languageNames from "../generated/language-names.json";
import { buildSemanticRanges, tokenize } from "../server/semantic.ts";
import { WorkspaceIndex } from "../server/workspace.ts";
const semanticTypesOf = (source, name, resolveDefinition = undefined) => {
  const ranges = buildSemanticRanges(source, resolveDefinition);
  return tokenize(source)
    .filter((token) => token.text === name)
    .map(({ line, char }) => {
      const token = ranges.find(
        ({ line: tokenLine, char: tokenChar, length }) =>
          tokenLine === line && tokenChar === char && length === name.length,
      );
      return token && { type: token.type, modifiers: token.modifiers };
    });
};
const semanticLabelsOf = (source, name, resolveDefinition = undefined) => {
  return semanticTypesOf(source, name, resolveDefinition).map(
    (token) => token && `${token.type}:${token.modifiers.join(".")}`,
  );
};
const workspaceResolver = (source) => {
  const uri = "file:///semantic-test.tung";
  const document = { uri, getText: () => source };
  const workspace = new WorkspaceIndex({
    get: (wanted) => (wanted === uri ? document : undefined),
    all: () => [document],
  });
  workspace.configure([]);
  return (token) =>
    workspace.resolveAt(uri, { line: token.line, character: token.char })
      .definition;
};
test("semantic lexer consumeth generated tung language names", () => {
  for (const keyword of languageNames.keywords) {
    assert.equal(tokenize(keyword)[0]?.kind, "keyword", keyword);
  }
  for (const primitive of languageNames.primitiveTypes) {
    assert.deepEqual(
      semanticLabelsOf(`let value: ${primitive} = value`, primitive),
      [
        "type:",
      ],
    );
  }
});
test("semantic analysis marketh declarations and function position", () => {
  const ranges = buildSemanticRanges(
    "let-ilk count = integer graith a equal show let value map: count = value",
  );
  assert(
    ranges.some(({ type, modifiers }) =>
      type === "type" && modifiers.includes("declaration")
    ),
  );
  assert(
    ranges.some(({ type, modifiers }) =>
      type === "function" && modifiers.includes("declaration")
    ),
  );
});
test("semantic analysis marketh fremmed lets", () => {
  const source =
    "show let append-native: text → text → text = 'join-text' fremmed";
  assert.deepEqual(semanticLabelsOf(source, "append-native"), [
    "function:declaration",
  ]);
  assert.equal(
    tokenize(source).find(({ text }) => text === "fremmed").kind,
    "keyword",
  );
});
test("asynchronous task results are variable declarations", () => {
  const source = fs.readFileSync(
    path.resolve(__dirname, "..", "..", "..", "byspel", "asynchronous.tung"),
    "utf8",
  );
  for (const name of ["left", "right"]) {
    assert.deepEqual(semanticLabelsOf(source, name), [
      "variable:declaration",
      "variable:",
    ]);
  }
});
test("semantic analysis treateth graith heads as types", () => {
  const source =
    "graith a equal, (a list) monoid show let (list: a list) keep: a list = list";
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "type:",
    "type:",
    "type:",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "list"), [
    "type:",
    "parameter:declaration",
    "type:",
    "type:",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "equal"), ["shape:"]);
  assert.deepEqual(semanticLabelsOf(source, "monoid"), ["shape:"]);
});
test("semantic analysis separateth a required shape from its member", () => {
  const source =
    "shape f traverse { graith m applicative let (a f) traverse (a → b m): (b f) m }";
  assert.deepEqual(semanticLabelsOf(source, "applicative"), ["shape:"]);
  assert.deepEqual(semanticLabelsOf(source, "traverse"), [
    "shape:declaration",
    "method:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "a"), ["type:", "type:"]);
});
test("semantic lexer recogniseth law as a keyword", () => {
  const source =
    "shape a identity { let a identity: a law (x: a): x identity ~ x }";
  assert.equal(
    tokenize(source).find(({ text }) => text === "law").kind,
    "keyword",
  );
});
test("semantic lexer recogniseth single-quoted unicode text", () => {
  const literal = tokenize("let word: text = 'λ字\\n';").find(
    ({ kind }) => kind === "string",
  );
  assert.equal(literal.text, "'λ字\\n'");
});
test("semantic lexer keepeth one astral unicode code point together", () => {
  const literal = tokenize("let face: unicode = `😀;").find(
    ({ kind }) => kind === "character",
  );
  assert.equal(literal.text, "`😀");
});
test("semantic lexer keepeth a decimal unicode escape together", () => {
  const literal = tokenize("let letter: unicode = `\\65;;").find(
    ({ kind }) => kind === "character",
  );
  assert.equal(literal.text, "`\\65;");
});
test("semantic analysis highlighteth law binders, types, calls, and equation marker", () => {
  const source = [
    "shape f functor {",
    "  let (a f) map (a → b ! e): b f ! e",
    "  law (value: a f, morphism: a → b ! e): value map morphism ~ value map id",
    "}",
  ].join("\n");
  const labelsOf = (name) => semanticLabelsOf(source, name);
  assert.deepEqual(labelsOf("value"), [
    "parameter:declaration",
    undefined,
    undefined,
  ]);
  assert.deepEqual(labelsOf("morphism"), ["parameter:declaration", undefined]);
  assert.deepEqual(labelsOf("a"), ["type:", "type:", "type:", "type:"]);
  assert.deepEqual(labelsOf("b"), ["type:", "type:", "type:"]);
  assert.deepEqual(labelsOf("e"), ["type:", "type:", "type:"]);
  assert.deepEqual(labelsOf("map"), ["method:declaration", "call:", "call:"]);
  assert.deepEqual(labelsOf("id"), [undefined]);
  assert.deepEqual(labelsOf("~"), ["operator:"]);
});
test("semantic analysis treateth a type alias body as type syntax", () => {
  const source = "show let-ilk code-points = unicode list";
  assert.deepEqual(semanticLabelsOf(source, "unicode"), ["type:"]);
  assert.deepEqual(semanticLabelsOf(source, "list"), ["type:"]);
});
test("semantic analysis treateth a term ascription tail as type syntax", () => {
  const source = "kin a box { a box } let value = (1 box: integer box)";
  assert.deepEqual(semanticLabelsOf(source, "integer"), ["type:"]);
  assert.deepEqual(semanticLabelsOf(source, "box"), [
    "type:declaration",
    "function:declaration",
    "call:",
    "type:",
  ]);
});
test("semantic analysis marketh parameterised type alias headers", () => {
  const source = "show let-ilk a powerset = a func 𝟚";
  assert.deepEqual(semanticLabelsOf(source, "powerset"), ["type:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "type:declaration",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "func"), ["type:"]);
});
test("semantic analysis keepeth ilk names apart and normaliseth callable declarations", () => {
  const source =
    "deed a action { a act: 𝟙 } shape a mapped { let a map: a } let x plain = x";
  const ranges = buildSemanticRanges(source);
  const at = (name, occurrence = 0) => {
    const offset =
      [...source.matchAll(new RegExp(`\\b${name}\\b`, "gu"))][occurrence].index;
    return ranges.find(
      ({ line, char, length }) =>
        line === 0 && char === offset && length === name.length,
    );
  };
  assert.equal(at("action").type, "type");
  assert.equal(at("act").type, "method");
  assert.equal(at("map").type, "method");
  assert.equal(at("plain").type, "function");
});
test("semantic analysis coloureth data type names and constructor argument types", () => {
  const source = "kin a option { none, a some } let x: integer option = 1 some";
  assert.deepEqual(semanticLabelsOf(source, "option"), [
    "type:declaration",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "type:declaration",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "some"), [
    "function:declaration",
    "call:",
  ]);
});
test("semantic analysis keepeth defined types distinct from functions", () => {
  const source =
    "kin natural { zero }; let (x: natural) identity: natural = x; let value = zero identity;";
  const typesOf = (name) =>
    semanticTypesOf(source, name).map((token) => token?.type);
  assert.deepEqual(typesOf("natural"), ["type", "type", "type"]);
  assert.deepEqual(typesOf("identity"), ["function", "call"]);
  assert.notEqual(typesOf("natural").at(-1), typesOf("identity").at(-1));
});
test("semantic analysis doth not colour term occurrences as types by name alone", () => {
  const source = [
    "kin a list { empty, a cons (a list) };",
    "let (list: a list) take: a list = match list {",
    "  empty | empty,",
    "  _ | list",
    "}",
  ].join("\n");
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "list", resolve), [
    "type:declaration",
    "type:",
    "parameter:declaration",
    "type:",
    "type:",
    undefined,
    undefined,
  ]);
});
test("defined data types and type constructors share the primitive type role", () => {
  const source = [
    "kin natural { zero };",
    "kin a box { a box };",
    "let whole: integer = 1;",
    "let ratio: float = 1.0;",
    "let count: natural = zero;",
    "let wrapped: integer box = 1 box;",
  ].join("\n");
  const resolve = workspaceResolver(source);
  const typesOf = (name) =>
    semanticTypesOf(source, name, resolve).map((token) => token?.type);
  assert.deepEqual(typesOf("integer"), ["type", "type"]);
  assert.deepEqual(typesOf("float"), ["type"]);
  assert.deepEqual(typesOf("natural"), ["type", "type"]);
  assert.equal(typesOf("box")[0], "type", "data type declaration");
  assert.equal(typesOf("box")[2], "type", "type constructor in an annotation");
});
test("semantic analysis giveth function positions a distinct call token", () => {
  const source =
    "let x f = x; let n fizzbuzz = n; let start till stop = start; let xs each f = xs; let write-line = { text | text }; let called = 1 f; let held = f; let piped = 1 $f; let spread = 1 $(f 2); let out = 0 till 32 $map fizzbuzz $each write-line;";
  const typesOf = (name) =>
    semanticTypesOf(source, name).map((token) => token?.type);
  assert.deepEqual(typesOf("f"), [
    "function",
    "parameter",
    "call",
    "variable",
    "call",
    "call",
  ]);
  assert.deepEqual(typesOf("till"), ["function", "call"]);
  assert.deepEqual(typesOf("map"), ["call"]);
  assert.deepEqual(typesOf("fizzbuzz"), ["function", "variable"]);
  assert.deepEqual(typesOf("each"), ["function", "call"]);
  assert.deepEqual(typesOf("write-line"), ["function", "variable"]);
});
test("semantic analysis coloureth lambda byspel function positions", () => {
  const source = [
    "kin v lambda {",
    "  v var,",
    "  v abs (v lambda),",
    "  (v lambda) app (v lambda)",
    "}",
    "graith v equal let (target: v lambda, x: v, s: v lambda) subst: v lambda = match target {",
    "  y var | (x ≡ y) if s (y var),",
    "  y abs t | (x ≡ y) if (y abs t) (y abs (t subst x s)),",
    "  t0 app t1 | (t0 subst x s) app (t1 subst x s)",
    "};",
  ].join("\n");
  const typesOf = (name) =>
    semanticTypesOf(source, name).map((token) => token?.type);
  assert.deepEqual(typesOf("subst"), ["function", "call", "call", "call"]);
  assert.deepEqual(typesOf("abs"), ["function", "call", "call", "call"]);
  assert.deepEqual(typesOf("var"), ["function", "call", "call"]);
  assert.deepEqual(typesOf("app"), ["function", "call", "call"]);
});
test("semantic analysis coloureth shape and fill methods", () => {
  const source = [
    "show shape a equal {",
    "  let a ≡ a: 𝟚",
    "  let a ≢ b = (a ≡ b) ¬",
    "}",
    "show shape f functor {",
    "  let (a f) map (a → b ! e): b f ! e",
    "}",
    "fill 𝟚 equal {",
    "  let ≡ = { yea, yea | yea, _, _ | nay }",
    "  let ≢ = { yea, yea | nay, _, _ | yea }",
    "}",
    "fill list functor {",
    "  graith m applicative let it map f = it",
    "}",
    "let out = a ≡ b $map f",
  ].join("\n");
  const typesOf = (name) =>
    semanticTypesOf(source, name).map((token) => token?.type);
  assert.deepEqual(typesOf("≡"), ["method", "call", "method", "call"]);
  assert.deepEqual(typesOf("≢"), ["method", "method"]);
  assert.deepEqual(typesOf("map"), ["method", "method", "call"]);
  assert.deepEqual(typesOf("f"), [
    "type",
    "type",
    "type",
    "parameter",
    undefined,
  ]);
});
test("semantic analysis coloureth every fill target as a type", () => {
  const source = [
    "kin natural { zero, natural suc }",
    "kin a list { empty, (a, a list) cons }",
    "shape a semiring {}",
    "fill natural semiring {}",
    "fill (a list) semiring {}",
  ].join("\n");
  assert.deepEqual(semanticLabelsOf(source, "natural"), [
    "type:declaration",
    "type:",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "type:declaration",
    "type:",
    "type:",
    "type:declaration",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "list"), [
    "type:declaration",
    "type:",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "semiring"), [
    "shape:declaration",
    "shape:",
    "shape:",
  ]);
});
test("lsp presents type variables and concrete types as one theme category", () => {
  const source =
    "show let if: 𝟚 → a → a → a = { yea, then, _ | then, nay, _, otherwise | otherwise };";
  assert.deepEqual(
    semanticTypesOf(source, "𝟚").map((token) => token?.type),
    ["type"],
  );
  assert.deepEqual(
    semanticTypesOf(source, "a").map((token) => token?.type),
    ["type", "type", "type"],
  );
});
test("anonymous function patterns distinguish constructors, binders, and wildcards", () => {
  const source =
    "kin 𝟚 { yea, nay }; show let if: 𝟚 → a → a → a = { yea, then, _ | then, nay, _, otherwise | otherwise };";
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "yea", resolve), [
    "enumMember:declaration",
    "enumMember:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "nay", resolve), [
    "enumMember:declaration",
    "enumMember:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "then", resolve), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "otherwise", resolve), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "_", resolve), [
    undefined,
    undefined,
  ]);
});
test("semantic analysis coloureth bound names in match and anonymous functions", () => {
  const source =
    "kin option { none, integer some }; let picked = match value { x some | x, y | y }; let anon = { a, b some | a };";
  assert.deepEqual(semanticLabelsOf(source, "x"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "y"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "b"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "some"), [
    "function:declaration",
    "call:",
    "call:",
  ]);
});
test("handler operations in function position are calls rather than binders", () => {
  const source = "try value { _ fail | nay };";
  const resolve = (token) =>
    token.text === "fail"
      ? { role: "method", modifiers: ["declaration", "effect"] }
      : undefined;
  assert.deepEqual(semanticLabelsOf(source, "fail"), ["call:"]);
  assert.deepEqual(semanticLabelsOf(source, "fail", resolve), ["call:effect"]);
});
test("semantic analysis coloureth every argument in multi-arm anonymous functions", () => {
  const source =
    "kin option { none, integer some }; let f = { a, b, c | a, d some, e, f | d };";
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "b"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "c"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "d"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "e"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "f"), [
    "function:declaration",
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "some"), [
    "function:declaration",
    "call:",
  ]);
});
test("application position, not callable identity, selecteth the function theme role", () => {
  const source =
    "kin a option { none, a some }; let (in: a, f: a → 𝟚, if: 𝟚 → a option → a option → a option) select = (in f) if (in some) none;";
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "f", resolve), [
    "parameter:declaration",
    "call:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "if", resolve), [
    "parameter:declaration",
    "call:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "some", resolve), [
    "function:declaration",
    "call:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "in", resolve), [
    "parameter:declaration",
    undefined,
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "none", resolve), [
    "enumMember:declaration",
    "enumMember:",
  ]);
});
test("bound names are plain in bodies unless they are applied", () => {
  const source =
    "kin a option { none, a some }; let selected = { a some | (a f) if (a some) none, a some, f some | a f $ some };";
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "a", resolve), [
    "type:declaration",
    "type:",
    "parameter:declaration",
    undefined,
    undefined,
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "f", resolve), [
    "call:",
    "parameter:declaration",
    "call:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "if", resolve), ["call:"]);
  assert.deepEqual(semanticLabelsOf(source, "some", resolve), [
    "function:declaration",
    "call:",
    "call:",
    "call:",
    "call:",
    "call:",
  ]);
});
test("callable constructor declarations use the function role", () => {
  const source =
    "show kin a ∐ b { a inject₀, b inject₁ }; show kin a ∏ b { a ∏ b }; show kin 𝟚 { yea, nay }; let left = 1 inject₀; let pair = 1 ∏ 2; let held = inject₁;";
  assert.deepEqual(semanticLabelsOf(source, "inject₀"), [
    "function:declaration",
    "call:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "inject₁"), [
    "function:declaration",
    "enumMember:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "∏"), [
    "type:declaration",
    "function:declaration",
    "call:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "yea"), ["enumMember:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "nay"), ["enumMember:declaration"]);
});
test("workspace highlighting keepeth callable constructors coloured as calls", () => {
  const source =
    "show kin a ∏ b { a ∏ b }; let pair = (a f) ∏ (b g); let held = ∏;";
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "∏", resolve), [
    "type:declaration",
    "function:declaration",
    "call:",
    "enumMember:",
  ]);
});
test("workspace highlighting coloureth every anonymous-function argument and use", () => {
  const source = [
    "let select = {",
    "  first, second, third | first,",
    "  fourth, fifth, sixth | sixth",
    "};",
  ].join("\n");
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "first", resolve), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "second", resolve), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "third", resolve), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "fourth", resolve), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "fifth", resolve), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "sixth", resolve), [
    "parameter:declaration",
    undefined,
  ]);
});
test("workspace highlighting coloureth every binder in each form of function", () => {
  const source = [
    "kin natural { zero };",
    "shape a chooser {",
    "  let (shape-first: a, shape-middle, shape-last: a) select: a = shape-middle",
    "}",
    "fill natural chooser {",
    "  let fill-first select fill-middle fill-last: natural = fill-last",
    "}",
    "let (group-first: natural, group-middle, group-last: natural) group-pick: natural = group-middle",
    "let plain-first plain-pick plain-middle plain-last = plain-last",
    "let anonymous = { arm-first, arm-middle, arm-last | arm-middle }",
    "let selected = match anonymous { case-first, case-middle, case-last | case-last }",
  ].join("\n");
  const resolve = workspaceResolver(source);
  const declarationOnly = [
    "shape-first",
    "shape-last",
    "fill-first",
    "fill-middle",
    "group-first",
    "group-last",
    "plain-first",
    "plain-middle",
    "arm-first",
    "arm-last",
    "case-first",
    "case-middle",
  ];
  const declaredAndUsed = [
    "shape-middle",
    "fill-last",
    "group-middle",
    "plain-last",
    "arm-middle",
    "case-last",
  ];
  for (const name of declarationOnly) {
    assert.deepEqual(semanticLabelsOf(source, name, resolve), [
      "parameter:declaration",
    ], name);
  }
  for (const name of declaredAndUsed) {
    assert.deepEqual(
      semanticLabelsOf(source, name, resolve),
      ["parameter:declaration", undefined],
      name,
    );
  }
});
test("semantic analysis coloureth destructured let-header binders", () => {
  const source =
    "kin a ∏ b { a ∏ b } let (left ∏ right: integer ∏ text) swap: text ∏ integer = right ∏ left";
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "left", resolve), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "right", resolve), [
    "parameter:declaration",
    undefined,
  ]);
});
test("semantic analysis keepeth effects distinct from ordinary calls and values", () => {
  const source =
    "deed ask { 𝟙 ask: integer }; deed send { integer send: integer }; let run = null ask; let held = ask; let sent = 1 $send; let ordinary x = x; let out = 1 $ordinary ask;";
  const ranges = buildSemanticRanges(source);
  const tokenAt = (name, occurrence = 0) => {
    const matches = [...source.matchAll(new RegExp(`\\b${name}\\b`, "gu"))];
    const offset = matches[occurrence].index;
    return ranges.find(
      ({ line, char, length }) =>
        line === 0 && char === offset && length === name.length,
    );
  };
  assert.equal(tokenAt("ask", 0).type, "type");
  assert.deepEqual(tokenAt("ask", 0).modifiers, ["declaration", "effect"]);
  assert.equal(tokenAt("ask", 1).type, "method");
  assert.deepEqual(tokenAt("ask", 2).modifiers, ["effect"]);
  assert.equal(tokenAt("ask", 2).type, "call");
  assert.equal(tokenAt("ask", 3).type, "variable");
  assert.equal(tokenAt("send", 1).type, "method");
  assert.deepEqual(tokenAt("send", 2).modifiers, ["effect"]);
  assert.equal(tokenAt("send", 2).type, "call");
  assert.equal(tokenAt("ordinary", 1).type, "call");
  assert.equal(tokenAt("ask", 4).type, "variable");
});
test("n-ary fold names use ordinary function and call roles", () => {
  const source = [
    "show let (integer: integer) …+: integer = integer;",
    "show let (integer: integer) …×: integer = integer;",
    "let sum = 1 …+;",
    "let product = 2 …×;",
  ].join("\n");
  assert.deepEqual(semanticLabelsOf(source, "…+"), [
    "function:declaration",
    "call:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "…×"), [
    "function:declaration",
    "call:",
  ]);
});
test("semantic analysis leaveth bring paths to the textmate import scope", () => {
  const source = [
    "bring _foreign.tung;",
    "bring equal.tung;",
    "bring order.tung;",
    "bring algebra/arithmetic/field.tung;",
    "bring integer.tung;",
    "bring table.tung;",
  ].join("\n");
  const ranges = buildSemanticRanges(source);
  for (const [line, text] of source.split("\n").entries()) {
    const start = "bring ".length;
    const end = text.indexOf(";");
    assert.equal(
      ranges.some(
        (token) =>
          token.line === line && token.char < end &&
          token.char + token.length > start,
      ),
      false,
      text,
    );
  }
});
test("semantic analysis marketh a bring alias as a namespace", () => {
  const source = "bring data/list.tung list yield list@empty";
  assert.deepEqual(semanticLabelsOf(source, "list"), [
    "namespace:declaration",
  ]);
  const ranges = buildSemanticRanges(source);
  const use = source.lastIndexOf("list@empty");
  assert(
    ranges.some(({ char, length, type }) =>
      char === use && length === "list".length && type === "namespace"
    ),
  );
});
test("semantic analysis marketh a default basename alias as a namespace", () => {
  const source = "bring data/list.tung yield list@empty";
  const ranges = buildSemanticRanges(source);
  const use = source.lastIndexOf("list@empty");
  assert(
    ranges.some(({ char, length, type }) =>
      char === use && length === "list".length && type === "namespace"
    ),
  );
});
test("semantic analysis doth not expose a path-qualified namespace", () => {
  const source = "bring data/list.tung yield data/list@empty";
  const ranges = buildSemanticRanges(source);
  const use = source.lastIndexOf("data/list@empty");
  assert.equal(
    ranges.some(({ char, type }) => char === use && type === "namespace"),
    false,
  );
});
test("semantic analysis leaveth standalone export names plain", () => {
  const source = [
    "show write, write-line, read;",
    "show +, zero, ×, one, -, ∕, ÷;",
    "show-ilk integer, float;",
    "show let (text: text) write-line: 𝟙 ! console = text;",
  ].join("\n");
  const ranges = buildSemanticRanges(source);
  for (const line of [0, 1]) {
    assert.equal(
      ranges.some((token) => token.line === line),
      false,
      source.split("\n")[line],
    );
  }
  assert.deepEqual(
    ranges.filter((token) => token.line === 2).map((token) => token.type),
    ["type", "type"],
  );
  assert(
    ranges.some(
      (token) =>
        token.line === 3 && token.type === "function" &&
        token.modifiers.includes("declaration"),
    ),
  );
  assert(ranges.some((token) => token.line === 3 && token.type === "type"));
});
