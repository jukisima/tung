const assert = require("node:assert/strict");
const test = require("node:test");
const { buildSemanticRanges, tokenize } = require("../server/semantic");
const { WorkspaceIndex } = require("../server/workspace");

function semanticTypesOf(source, name, resolveDefinition) {
  const ranges = buildSemanticRanges(source, resolveDefinition);
  return tokenize(source).filter((token) => token.text === name).map(({ line, char }) => {
    const token = ranges.find(({ line: tokenLine, char: tokenChar, length }) => tokenLine === line && tokenChar === char && length === name.length);
    return token && { type: token.type, modifiers: token.modifiers };
  });
}

function semanticLabelsOf(source, name, resolveDefinition) {
  return semanticTypesOf(source, name, resolveDefinition).map((token) => token && `${token.type}:${token.modifiers.join(".")}`);
}

function workspaceResolver(source) {
  const uri = "file:///semantic-test.tung";
  const document = { uri, getText: () => source };
  const workspace = new WorkspaceIndex({
    get: (wanted) => wanted === uri ? document : undefined,
    all: () => [document]
  });
  workspace.configure([]);
  return (token) => workspace.resolveAt(uri, { line: token.line, character: token.char }).definition;
}

test("semantic analysis marks declarations and function position", () => {
  const ranges = buildSemanticRanges("let-ilk count = integer; graith a equal show let value map: count = value;");
  assert(ranges.some(({ type, modifiers }) => type === "type" && modifiers.includes("declaration")));
  assert(ranges.some(({ type, modifiers }) => type === "function" && modifiers.includes("declaration")));
});

test("semantic analysis treats a type alias body as type syntax", () => {
  const source = "show let-ilk string = character list;";

  assert.deepEqual(semanticLabelsOf(source, "character"), ["type:"]);
  assert.deepEqual(semanticLabelsOf(source, "list"), ["type:"]);
});

test("semantic analysis keeps ilk names apart and normalizes callable declarations", () => {
  const source = "deed a action { a act: 𝟙 }; shape a mapped { a map: a }; let x plain = x;";
  const ranges = buildSemanticRanges(source);
  const at = (name, occurrence = 0) => {
    const offset = [...source.matchAll(new RegExp(`\\b${name}\\b`, "gu"))][occurrence].index;
    return ranges.find(({ line, char, length }) => line === 0 && char === offset && length === name.length);
  };

  assert.equal(at("action").type, "type");
  assert.equal(at("act").type, "method");
  assert.equal(at("map").type, "method");
  assert.equal(at("plain").type, "function");
});

test("semantic analysis colors data type names and constructor argument types", () => {
  const source = "choose a option { none, a some }; let x: integer option = 1 some;";

  assert.deepEqual(semanticLabelsOf(source, "option"), ["type:declaration", "type:"]);
  assert.deepEqual(semanticLabelsOf(source, "a"), ["type:declaration", "type:"]);
  assert.deepEqual(semanticLabelsOf(source, "some"), ["function:declaration", "call:"]);
});

test("semantic analysis keeps defined types distinct from functions", () => {
  const source = "choose natural { zero }; let (x: natural) identity: natural = x; let value = zero identity;";
  const typesOf = (name) => semanticTypesOf(source, name).map((token) => token?.type);

  assert.deepEqual(typesOf("natural"), ["type", "type", "type"]);
  assert.deepEqual(typesOf("identity"), ["function", "call"]);
  assert.notEqual(typesOf("natural").at(-1), typesOf("identity").at(-1));
});

test("defined data types and type constructors share the primitive type role", () => {
  const source = [
    "choose natural { zero };",
    "choose a box { a box };",
    "let whole: integer = 1;",
    "let ratio: float = 1.0;",
    "let count: natural = zero;",
    "let wrapped: integer box = 1 box;"
  ].join("\n");
  const resolve = workspaceResolver(source);
  const typesOf = (name) => semanticTypesOf(source, name, resolve).map((token) => token?.type);

  assert.deepEqual(typesOf("integer"), ["type", "type"]);
  assert.deepEqual(typesOf("float"), ["type"]);
  assert.deepEqual(typesOf("natural"), ["type", "type"]);
  assert.equal(typesOf("box")[0], "type", "data type declaration");
  assert.equal(typesOf("box")[2], "type", "type constructor in an annotation");
});

test("semantic analysis gives function positions a distinct call token", () => {
  const source = "let x f = x; let n fizzbuzz = n; let start till stop = start; let xs each f = xs; let called = 1 f; let held = f; let piped = 1 $f; let spread = 1 $(f 2); let out = 0 till 32 $map fizzbuzz $each write-line;";
  const typesOf = (name) => semanticTypesOf(source, name).map((token) => token?.type);

  assert.deepEqual(typesOf("f"), ["function", "parameter", "call", "variable", "call", "call"]);
  assert.deepEqual(typesOf("till"), ["function", "call"]);
  assert.deepEqual(typesOf("map"), ["call"]);
  assert.deepEqual(typesOf("fizzbuzz"), ["function", "variable"]);
  assert.deepEqual(typesOf("each"), ["function", "call"]);
  assert.deepEqual(typesOf("write-line"), ["variable"]);
});

test("semantic analysis colors lambda sample function positions", () => {
  const source = [
    "choose v lambda {",
    "  v var,",
    "  v abs (v lambda),",
    "  (v lambda) app (v lambda)",
    "}",
    "graith v equal let (target: v lambda, x: v, s: v lambda) subst: v lambda = match target {",
    "  y var | (x ≡ y) if s (y var),",
    "  y abs t | (x ≡ y) if (y abs t) (y abs (t subst x s)),",
    "  t0 app t1 | (t0 subst x s) app (t1 subst x s)",
    "};"
  ].join("\n");
  const typesOf = (name) => semanticTypesOf(source, name).map((token) => token?.type);

  assert.deepEqual(typesOf("subst"), ["function", "call", "call", "call"]);
  assert.deepEqual(typesOf("abs"), ["function", "call", "call", "call"]);
  assert.deepEqual(typesOf("var"), ["function", "call", "call"]);
  assert.deepEqual(typesOf("app"), ["function", "call", "call"]);
});

test("semantic analysis colors shape and fill methods", () => {
  const source = [
    "show shape a equal {",
    "  a ≡ a: 𝟚;",
    "  let a ≢ b = (a ≡ b) ¬",
    "}",
    "show shape f functor {",
    "  (a f) map (a → b ! e): b f ! e",
    "}",
    "fill 𝟚 equal {",
    "  let ≡ = { yea, yea | yea, _, _ | nay }",
    "}",
    "fill list functor {",
    "  let it map f = it",
    "}",
    "let out = a ≡ b $map f;"
  ].join("\n");
  const typesOf = (name) => semanticTypesOf(source, name).map((token) => token?.type);

  assert.deepEqual(typesOf("≡"), ["method", "call", "method", "call"]);
  assert.deepEqual(typesOf("≢"), ["method"]);
  assert.deepEqual(typesOf("map"), ["method", "method", "call"]);
  assert.deepEqual(typesOf("f"), ["type", "type", "type", "parameter", undefined]);
});

test("lsp presents type variables and concrete types as one theme category", () => {
  const source = "show let if: 𝟚 → a → a → a = { yea, then, _ | then, nay, _, otherwise | otherwise };";

  assert.deepEqual(semanticTypesOf(source, "𝟚").map((token) => token?.type), ["type"]);
  assert.deepEqual(semanticTypesOf(source, "a").map((token) => token?.type), ["type", "type", "type"]);
});

test("anonymous function patterns distinguish constructors, binders, and wildcards", () => {
  const source = "show let if: 𝟚 → a → a → a = { yea, then, _ | then, nay, _, otherwise | otherwise };";
  const resolve = workspaceResolver(source);

  assert.deepEqual(semanticLabelsOf(source, "yea", resolve), ["enumMember:defaultLibrary"]);
  assert.deepEqual(semanticLabelsOf(source, "nay", resolve), ["enumMember:defaultLibrary"]);
  assert.deepEqual(semanticLabelsOf(source, "then", resolve), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "otherwise", resolve), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "_", resolve), [undefined, undefined]);
});

test("semantic analysis colors bound names in match and anonymous functions", () => {
  const source = "choose option { none, integer some }; let picked = match value { x some | x, y | y }; let anon = { a, b some | a };";

  assert.deepEqual(semanticLabelsOf(source, "x"), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "y"), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "a"), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "b"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "some"), ["function:declaration", "call:", "call:"]);
});

test("semantic analysis colors every argument in multi-arm anonymous functions", () => {
  const source = "choose option { none, integer some }; let f = { a, b, c | a, d some, e, f | d };";

  assert.deepEqual(semanticLabelsOf(source, "a"), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "b"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "c"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "d"), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "e"), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "f"), ["function:declaration", "parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "some"), ["function:declaration", "call:"]);
});

test("application position, not callable identity, selects the function theme role", () => {
  const source = "choose a option { none, a some }; let (in: a, f: a → 𝟚, if: 𝟚 → a option → a option → a option) select = (in f) if (in some) none;";
  const resolve = workspaceResolver(source);

  assert.deepEqual(semanticLabelsOf(source, "f", resolve), ["parameter:declaration", "call:"]);
  assert.deepEqual(semanticLabelsOf(source, "if", resolve), ["parameter:declaration", "call:"]);
  assert.deepEqual(semanticLabelsOf(source, "some", resolve), ["function:declaration", "call:"]);
  assert.deepEqual(semanticLabelsOf(source, "in", resolve), ["parameter:declaration", undefined, undefined]);
  assert.deepEqual(semanticLabelsOf(source, "none", resolve), ["enumMember:declaration", "enumMember:"]);
});

test("bound names are plain in bodies unless they are applied", () => {
  const source = "choose a option { none, a some }; let selected = { a some | (a f) if (a some) none, a some, f some | a f $ some };";
  const resolve = workspaceResolver(source);

  assert.deepEqual(semanticLabelsOf(source, "a", resolve), ["type:declaration", "type:", "parameter:declaration", undefined, undefined, "parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "f", resolve), ["call:", "parameter:declaration", "call:"]);
  assert.deepEqual(semanticLabelsOf(source, "if", resolve), ["call:"]);
  assert.deepEqual(semanticLabelsOf(source, "some", resolve), ["function:declaration", "call:", "call:", "call:", "call:", "call:"]);
});

test("callable constructor declarations use the function role", () => {
  const source = "show choose a ∐ b { a inject₀, b inject₁ }; show choose a ∏ b { a ∏ b }; show choose 𝟚 { yea, nay }; let left = 1 inject₀; let held = inject₁;";

  assert.deepEqual(semanticLabelsOf(source, "inject₀"), ["function:declaration", "call:"]);
  assert.deepEqual(semanticLabelsOf(source, "inject₁"), ["function:declaration", "enumMember:"]);
  assert.deepEqual(semanticLabelsOf(source, "∏"), ["type:declaration", "function:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "yea"), ["enumMember:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "nay"), ["enumMember:declaration"]);
});

test("workspace highlighting colors every anonymous-function argument and use", () => {
  const source = [
    "let select = {",
    "  first, second, third | first,",
    "  fourth, fifth, sixth | sixth",
    "};"
  ].join("\n");
  const resolve = workspaceResolver(source);

  assert.deepEqual(semanticLabelsOf(source, "first", resolve), ["parameter:declaration", undefined]);
  assert.deepEqual(semanticLabelsOf(source, "second", resolve), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "third", resolve), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "fourth", resolve), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "fifth", resolve), ["parameter:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "sixth", resolve), ["parameter:declaration", undefined]);
});

test("workspace highlighting colors every binder in each form of function", () => {
  const source = [
    "choose natural { zero };",
    "shape a chooser {",
    "  let (shape-first: a, shape-middle, shape-last: a) select: a = shape-middle;",
    "}",
    "fill natural chooser {",
    "  let fill-first select fill-middle fill-last: natural = fill-last;",
    "}",
    "let (group-first: natural, group-middle, group-last: natural) group-pick: natural = group-middle;",
    "let plain-first plain-pick plain-middle plain-last = plain-last;",
    "let anonymous = { arm-first, arm-middle, arm-last | arm-middle };",
    "let selected = match anonymous { case-first, case-middle, case-last | case-last };"
  ].join("\n");
  const resolve = workspaceResolver(source);
  const declarationOnly = [
    "shape-first", "shape-last", "fill-first", "fill-middle", "group-first", "group-last",
    "plain-first", "plain-middle", "arm-first", "arm-last", "case-first", "case-middle"
  ];
  const declaredAndUsed = [
    "shape-middle", "fill-last", "group-middle", "plain-last", "arm-middle", "case-last"
  ];

  for (const name of declarationOnly) {
    assert.deepEqual(semanticLabelsOf(source, name, resolve), ["parameter:declaration"], name);
  }
  for (const name of declaredAndUsed) {
    assert.deepEqual(semanticLabelsOf(source, name, resolve), ["parameter:declaration", undefined], name);
  }
});

test("semantic analysis keeps effects distinct from ordinary calls and values", () => {
  const source = "deed ask { ask: integer }; let run = 1 $ask; let held = ask; let ordinary x = x; let out = 1 $ordinary ask;";
  const ranges = buildSemanticRanges(source);
  const tokenAt = (name, occurrence = 0) => {
    const matches = [...source.matchAll(new RegExp(`\\b${name}\\b`, "gu"))];
    const offset = matches[occurrence].index;
    return ranges.find(({ line, char, length }) => line === 0 && char === offset && length === name.length);
  };

  assert.equal(tokenAt("ask", 0).type, "type");
  assert.deepEqual(tokenAt("ask", 0).modifiers, ["declaration", "effect"]);
  assert.equal(tokenAt("ask", 1).type, "method");
  assert.deepEqual(tokenAt("ask", 2).modifiers, ["effect"]);
  assert.equal(tokenAt("ask", 2).type, "call");
  assert.deepEqual(tokenAt("ask", 3).modifiers, ["effect"]);
  assert.equal(tokenAt("ask", 3).type, "variable");
  assert.equal(tokenAt("ordinary", 1).type, "call");
  assert.equal(tokenAt("ask", 4).type, "variable");
});

test("semantic analysis leaves bring paths to the textmate import scope", () => {
  const source = [
    "bring foreign.tung;",
    "bring equal.tung;",
    "bring order.tung;",
    "bring algebra/field.tung;",
    "bring integer.tung;",
    "bring map.tung;"
  ].join("\n");
  const ranges = buildSemanticRanges(source);

  for (const [line, text] of source.split("\n").entries()) {
    const start = "bring ".length;
    const end = text.indexOf(";");
    assert.equal(ranges.some((token) => token.line === line && token.char < end && token.char + token.length > start), false, text);
  }
});

test("semantic analysis leaves standalone export names plain", () => {
  const source = [
    "show write, write-line, read;",
    "show +, zero, ×, one, -, ÷;",
    "show-ilk integer, float;",
    "show let (text: string) write-line: 𝟙 ! console = text;"
  ].join("\n");
  const ranges = buildSemanticRanges(source);

  for (const line of [0, 1]) {
    assert.equal(ranges.some((token) => token.line === line), false, source.split("\n")[line]);
  }
  assert.deepEqual(ranges.filter((token) => token.line === 2).map((token) => token.type), ["type", "type"]);
  assert(ranges.some((token) => token.line === 3 && token.type === "function" && token.modifiers.includes("declaration")));
  assert(ranges.some((token) => token.line === 3 && token.type === "type"));
});
