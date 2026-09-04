import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import test from "node:test";
import languageNames from "../generated/language-names.json";
import { buildSemanticRanges } from "../server/semantic.ts";
import {
  findMatching,
  isOpen,
  patternArmRegions,
  tokenize,
} from "../server/syntax.ts";
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
test("semantic lexer consumeþ generated tung language names", () => {
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
test("semantic analysis markeþ control keywords as keywords", () => {
  const source = "yield try value { yielded @ yield yielded }";
  assert.deepEqual(semanticLabelsOf(source, "yield"), [
    "keyword:",
    "keyword:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "try"), ["keyword:"]);
});
test("semantic lexer keepeþ special openers as single delimiters", () => {
  const tokens = tokenize("r(value = <(f, a, >(g, b, c)))");
  const opens = tokens.filter(({ text }) => isOpen(text));
  assert.deepEqual(opens.map(({ text }) => text), ["r(", "<(", ">("]);
  assert.deepEqual(
    opens.map(({ index }) => tokens[findMatching(tokens, index)]?.text),
    [")", ")", ")"],
  );
  assert(opens.every(({ kind }) => kind === "punctuation"));
});
test("semantic analysis distinguishþ a record term from field symbols", () => {
  const source =
    "let record = r(child = r(value = 1)) yield record.child.value";
  assert.deepEqual(semanticLabelsOf(source, "record"), [
    "variable:declaration",
    "variable:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "child"), [undefined, "property:"]);
  assert.deepEqual(semanticLabelsOf(source, "value"), [undefined, "property:"]);
});
test("semantic analysis leaveþ the record opener unclassified", () => {
  const source =
    "let r = value let record = r(field = r) let applied = r (field)";
  assert.deepEqual(semanticLabelsOf(source, "r"), [
    "variable:declaration",
    "variable:",
    "variable:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "r("), [undefined]);
});
test("semantic analysis leaveþ association openers unclassified", () => {
  const source =
    "let left = <(+, 1, 2) let less = 1 < 2 let right = >(-, 3, 2) let greater = 3 > 2";
  assert.deepEqual(semanticLabelsOf(source, "<"), ["call:applied"]);
  assert.deepEqual(semanticLabelsOf(source, ">"), ["call:applied"]);
  assert.deepEqual(semanticLabelsOf(source, "<("), [undefined]);
  assert.deepEqual(semanticLabelsOf(source, ">("), [undefined]);
});
test("semantic analysis markeþ declarations and function position", () => {
  const ranges = buildSemanticRanges(
    "let-ilk count = ℤ graiþ a equal show let value map: count = value",
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
test("semantic analysis markeþ fremmed lets", () => {
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
test("semantic analysis treateþ graiþ heads as types", () => {
  const source =
    "graiþ a equal, (a list) monoid show let (list: a list) keep: a list = list";
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "type:",
    "type:",
    "type:",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "list"), [
    "type:applied",
    "parameter:declaration",
    "type:applied",
    "type:applied",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "equal"), ["frame:applied"]);
  assert.deepEqual(semanticLabelsOf(source, "monoid"), ["frame:applied"]);
});
test("semantic analysis separateþ a required frame from its member", () => {
  const source =
    "frame f traverse { graiþ m applicative let (a f) traverse (a → b m): (b f) m }";
  assert.deepEqual(semanticLabelsOf(source, "applicative"), [
    "frame:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "traverse"), [
    "frame:declaration.typeFunction",
    "method:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "a"), ["type:", "type:"]);
});
test("semantic lexer recogniseþ single-quoted unicode text", () => {
  const literal = tokenize("let word: text = 'λ字\\n'").find(
    ({ kind }) => kind === "string",
  );
  assert.equal(literal.text, "'λ字\\n'");
});
test("semantic lexer keepeþ one astral unicode code point together", () => {
  const literal = tokenize("let face: unicode = `😀").find(
    ({ kind }) => kind === "character",
  );
  assert.equal(literal.text, "`😀");
});
test("semantic lexer keepeþ a decimal unicode escape together", () => {
  const literal = tokenize("let letter: unicode = `\\65;").find(
    ({ kind }) => kind === "character",
  );
  assert.equal(literal.text, "`\\65;");
});
test("semantic analysis highlighteþ law binders, types, calls, and equation marker", () => {
  const source = [
    "frame f functor {",
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
  assert.deepEqual(labelsOf("map"), [
    "method:declaration",
    "call:applied",
    "call:applied",
  ]);
  assert.deepEqual(labelsOf("id"), [undefined]);
  assert.deepEqual(labelsOf("~"), ["operator:"]);
});
test("semantic analysis treateþ a type alias body as type syntax", () => {
  const source = "show let-ilk code-points = unicode list";
  assert.deepEqual(semanticLabelsOf(source, "unicode"), ["type:"]);
  assert.deepEqual(semanticLabelsOf(source, "list"), ["type:applied"]);
});
test("semantic analysis treateþ a term ascription tail as type syntax", () => {
  const source = "ilk a box { a box } let value = (1 box: ℤ box)";
  assert.deepEqual(semanticLabelsOf(source, "ℤ"), ["type:"]);
  assert.deepEqual(semanticLabelsOf(source, "box"), [
    "type:declaration.typeFunction",
    "function:declaration",
    "call:applied",
    "type:applied",
  ]);
});
test("semantic analysis markeþ parameterised type alias headers", () => {
  const source = "show let-ilk a powerset = a func 𝟚";
  assert.deepEqual(semanticLabelsOf(source, "powerset"), [
    "type:declaration.typeFunction",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "type:declaration",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "func"), ["type:applied"]);
});
test("parameterised ilk heads are type functions", () => {
  const source = "ilk a list { empty }";
  assert.deepEqual(semanticLabelsOf(source, "list"), [
    "type:declaration.typeFunction",
  ]);
});
test("semantic analysis keepeþ ilk names apart and normaliseþ callable declarations", () => {
  const source =
    "deed a action { a act: 𝟙 } frame a mapped { let a map: a } let x plain = x";
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
test("semantic analysis coloureþ data type names and constructor argument types", () => {
  const source = "ilk a option { none, a some } let x: ℤ option = 1 some";
  assert.deepEqual(semanticLabelsOf(source, "option"), [
    "type:declaration.typeFunction",
    "type:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "a"), [
    "type:declaration",
    "type:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "some"), [
    "function:declaration",
    "call:applied",
  ]);
});
test("semantic analysis keepeþ defined types distinct from functions", () => {
  const source =
    "ilk ℕ { zero } let (x: ℕ) identity: ℕ = x let value = zero identity";
  const typesOf = (name) =>
    semanticTypesOf(source, name).map((token) => token?.type);
  assert.deepEqual(typesOf("ℕ"), ["type", "type", "type"]);
  assert.deepEqual(typesOf("identity"), ["function", "call"]);
  assert.notEqual(typesOf("ℕ").at(-1), typesOf("identity").at(-1));
});
test("semantic analysis doth not colour term occurrences as types by name alone", () => {
  const source = [
    "ilk a list { empty, a cons (a list) }",
    "let (list: a list) take: a list = match list {",
    "  empty @ empty,",
    "  _ @ list",
    "}",
  ].join("\n");
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "list", resolve), [
    "type:declaration.typeFunction",
    "type:applied",
    "parameter:declaration",
    "type:applied",
    "type:applied",
    undefined,
    undefined,
  ]);
});
test("defined data types and type constructors share the primitive type role", () => {
  const source = [
    "ilk ℕ { zero }",
    "ilk a box { a box }",
    "let whole: ℤ = 1",
    "let ratio: float = 1.0",
    "let count: ℕ = zero",
    "let wrapped: ℤ box = 1 box",
  ].join("\n");
  const resolve = workspaceResolver(source);
  const typesOf = (name) =>
    semanticTypesOf(source, name, resolve).map((token) => token?.type);
  assert.deepEqual(typesOf("ℤ"), ["type", "type"]);
  assert.deepEqual(typesOf("float"), ["type"]);
  assert.deepEqual(typesOf("ℕ"), ["type", "type"]);
  assert.equal(typesOf("box")[0], "type", "data type declaration");
  assert.equal(typesOf("box")[2], "type", "type constructor in an annotation");
});
test("workspace refinement preserveþ applied type modifiers", () => {
  const source = [
    "ilk a list { empty }",
    "frame a inhold b {}",
    "graiþ (a list) inhold a",
  ].join("\n");
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "list", resolve), [
    "type:declaration.typeFunction",
    "type:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "inhold", resolve), [
    "frame:declaration.typeFunction",
    "frame:applied",
  ]);
});
test("semantic analysis giveþ function positions a distinct call token", () => {
  const source =
    "let x f = x let n fizzbuzz = n let start till stop = start let xs each f = xs let write-line = { text @ text } let called = 1 f let held = f let piped = 1 $f let spread = 1 $(f 2) let out = 0 till 32 $map fizzbuzz $each write-line";
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
test("semantic analysis coloureþ lambda byspel function positions", () => {
  const source = [
    "ilk v lambda {",
    "  v var,",
    "  v abs (v lambda),",
    "  (v lambda) app (v lambda)",
    "}",
    "graiþ v equal let (target: v lambda, x: v, s: v lambda) subst: v lambda = match target {",
    "  y var @ (x ≡ y) if s (y var),",
    "  y abs t @ (x ≡ y) if (y abs t) (y abs (t subst x s)),",
    "  t0 app t1 @ (t0 subst x s) app (t1 subst x s)",
    "}",
  ].join("\n");
  const typesOf = (name) =>
    semanticTypesOf(source, name).map((token) => token?.type);
  assert.deepEqual(typesOf("subst"), ["function", "call", "call", "call"]);
  assert.deepEqual(typesOf("abs"), ["function", "call", "call", "call"]);
  assert.deepEqual(typesOf("var"), ["function", "call", "call"]);
  assert.deepEqual(typesOf("app"), ["function", "call", "call"]);
});
test("semantic analysis coloureþ frame and fill methods", () => {
  const source = [
    "show frame a equal {",
    "  let a ≡ a: 𝟚",
    "  let a ≢ b = (a ≡ b) ¬",
    "}",
    "show frame f functor {",
    "  let (a f) map (a → b ! e): b f ! e",
    "}",
    "fill 𝟚 equal {",
    "  let ≡ = { yea, yea @ yea, _, _ @ nay }",
    "  let ≢ = { yea, yea @ nay, _, _ @ yea }",
    "}",
    "fill list functor {",
    "  graiþ m applicative let it map f = it",
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
test("semantic analysis coloureþ every fill target as a type", () => {
  const source = [
    "ilk ℕ { zero, ℕ suc }",
    "ilk a list { empty, (a, a list) cons }",
    "frame a semiring {}",
    "fill ℕ semiring {}",
    "fill (a list) semiring {}",
  ].join("\n");
  assert.deepEqual(semanticLabelsOf(source, "ℕ"), [
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
    "type:declaration.typeFunction",
    "type:applied",
    "type:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "semiring"), [
    "frame:declaration.typeFunction",
    "frame:applied",
    "frame:applied",
  ]);
});
test("lsp presents type variables and concrete types as one theme category", () => {
  const source =
    "show let if: 𝟚 → a → a → a = { yea, then, _ @ then, nay, _, otherwise @ otherwise }";
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
    "ilk 𝟚 { yea, nay } show let if: 𝟚 → a → a → a = { yea, then, _ @ then, nay, _, otherwise @ otherwise }";
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
test("semantic analysis coloureþ bound names in match and anonymous functions", () => {
  const source =
    "ilk option { none, ℤ some } let picked = match value { x some @ x, y @ y } let anon = { a, b some @ a }";
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
    "call:applied",
    "call:applied",
  ]);
});
test("handler operations in function position are calls rather than binders", () => {
  const source = "try value { _ fail @ nay }";
  const resolve = (token) =>
    token.text === "fail"
      ? { role: "method", modifiers: ["declaration", "effect"] }
      : undefined;
  assert.deepEqual(semanticLabelsOf(source, "fail"), ["call:applied"]);
  assert.deepEqual(semanticLabelsOf(source, "fail", resolve), [
    "call:effect.applied",
  ]);
});
test("semantic analysis coloureþ every argument in multi-arm anonymous functions", () => {
  const source =
    "ilk option { none, ℤ some } let f = { a, b, c @ a, d some, e, f @ d }";
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
    "call:applied",
  ]);
});
test("semantic analysis coloureþ alternative pattern binders and separator", () => {
  const source = "let choose = { first | second @ first }";
  assert.deepEqual(semanticLabelsOf(source, "first"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "second"), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "|"), ["operator:"]);
});
test("semantic analysis recovereþ a pattern without its closing brace", () => {
  const source = "let choose = { item @ item";
  const tokens = tokenize(source);
  assert.equal(patternArmRegions(tokens)[0].bodyEnd, tokens.length);
  assert.deepEqual(semanticLabelsOf(source, "item"), [
    "parameter:declaration",
    undefined,
  ]);
});
test("semantic analysis recovereþ an unfinished second arm", () => {
  const source = "let choose = { first @ first, second, third @";
  assert.deepEqual(semanticLabelsOf(source, "first"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "second"), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "third"), [
    "parameter:declaration",
  ]);
});
test("semantic analysis skippeþ nested constructor-pattern delimiters", () => {
  const source =
    "ilk a option { none, a some } let choose = { (left some), ((right some)) @ left }";
  assert.deepEqual(semanticLabelsOf(source, "left"), [
    "parameter:declaration",
    undefined,
  ]);
  assert.deepEqual(semanticLabelsOf(source, "right"), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "some"), [
    "function:declaration",
    "call:applied",
    "call:applied",
  ]);
});
test("semantic analysis localiseþ an unmatched inner delimiter", () => {
  const source = "let choose = { (left, right @ right";
  const tokens = tokenize(source);
  const arm = patternArmRegions(tokens)[0];
  assert.equal(tokens[arm.patternStart].text, "left");
  assert.deepEqual(semanticLabelsOf(source, "left"), [
    "parameter:declaration",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "right"), [
    "parameter:declaration",
    undefined,
  ]);
});
test("application position, not callable identity, selecteþ the function theme role", () => {
  const source =
    "ilk a option { none, a some } let (in: a, f: a → 𝟚, if: 𝟚 → a option → a option → a option) select = (in f) if (in some) none";
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "f", resolve), [
    "parameter:declaration",
    "call:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "if", resolve), [
    "parameter:declaration",
    "call:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "some", resolve), [
    "function:declaration",
    "call:applied",
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
    "ilk a option { none, a some } let selected = { a some @ (a f) if (a some) none, a some, f some @ a f $ some }";
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
    "call:applied",
    "parameter:declaration",
    "call:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "if", resolve), ["call:applied"]);
  assert.deepEqual(semanticLabelsOf(source, "some", resolve), [
    "function:declaration",
    "call:applied",
    "call:applied",
    "call:applied",
    "call:applied",
    "call:applied",
  ]);
});
test("callable constructor declarations use the function role", () => {
  const source =
    "show ilk a ∐ b { a inject₀, b inject₁ } show ilk a ∏ b { a ∏ b } show ilk 𝟚 { yea, nay } let left = 1 inject₀ let pair = 1 ∏ 2 let held = inject₁";
  assert.deepEqual(semanticLabelsOf(source, "inject₀"), [
    "function:declaration",
    "call:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "inject₁"), [
    "function:declaration",
    "enumMember:",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "∏"), [
    "type:declaration.typeFunction",
    "function:declaration",
    "call:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "yea"), ["enumMember:declaration"]);
  assert.deepEqual(semanticLabelsOf(source, "nay"), ["enumMember:declaration"]);
});
test("workspace highlighting keepeþ callable constructors coloured as calls", () => {
  const source =
    "show ilk a ∏ b { a ∏ b } let pair = (a f) ∏ (b g) let held = ∏";
  const resolve = workspaceResolver(source);
  assert.deepEqual(semanticLabelsOf(source, "∏", resolve), [
    "type:declaration.typeFunction",
    "function:declaration",
    "call:applied",
    "enumMember:",
  ]);
});
test("workspace highlighting coloureþ every anonymous-function argument and use", () => {
  const source = [
    "let select = {",
    "  first, second, third @ first,",
    "  fourth, fifth, sixth @ sixth",
    "}",
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
test("workspace highlighting coloureþ every binder in each form of function", () => {
  const source = [
    "ilk ℕ { zero }",
    "frame a chooser {",
    "  let (frame-first: a, frame-middle, frame-last: a) select: a = frame-middle",
    "}",
    "fill ℕ chooser {",
    "  let fill-first select fill-middle fill-last: ℕ = fill-last",
    "}",
    "let (group-first: ℕ, group-middle, group-last: ℕ) group-pick: ℕ = group-middle",
    "let plain-first plain-pick plain-middle plain-last = plain-last",
    "let anonymous = { arm-first, arm-middle, arm-last @ arm-middle }",
    "let selected = match anonymous { case-first, case-middle, case-last @ case-last }",
  ].join("\n");
  const resolve = workspaceResolver(source);
  const declarationOnly = [
    "frame-first",
    "frame-last",
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
    "frame-middle",
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
test("semantic analysis coloureþ destructured let-header binders", () => {
  const source =
    "ilk a ∏ b { a ∏ b } let (left ∏ right: ℤ ∏ text) swap: text ∏ ℤ = right ∏ left";
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
test("semantic analysis keepeþ effects distinct from ordinary calls and values", () => {
  const source =
    "deed ask { 𝟙 ask: ℤ } deed send { ℤ send: ℤ } let run = only ask let held = ask let sent = 1 $send let ordinary x = x let out = 1 $ordinary ask";
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
  assert.deepEqual(tokenAt("ask", 2).modifiers, ["effect", "applied"]);
  assert.equal(tokenAt("ask", 2).type, "call");
  assert.equal(tokenAt("ask", 3).type, "variable");
  assert.equal(tokenAt("send", 1).type, "method");
  assert.deepEqual(tokenAt("send", 2).modifiers, ["effect", "applied"]);
  assert.equal(tokenAt("send", 2).type, "call");
  assert.equal(tokenAt("ordinary", 1).type, "call");
  assert.equal(tokenAt("ask", 4).type, "variable");
});
test("n-ary fold names use ordinary function and call roles", () => {
  const source = [
    "show let (integer: ℤ) …+: ℤ = integer",
    "show let (integer: ℤ) …×: ℤ = integer",
    "let sum = 1 …+",
    "let product = 2 …×",
  ].join("\n");
  assert.deepEqual(semanticLabelsOf(source, "…+"), [
    "function:declaration",
    "call:applied",
  ]);
  assert.deepEqual(semanticLabelsOf(source, "…×"), [
    "function:declaration",
    "call:applied",
  ]);
});
test("semantic analysis leaveþ use paths to the textmate import scope", () => {
  const source = [
    "use _foreign.tung",
    "use equal.tung",
    "use order.tung",
    "use algebra/arithmetic/field.tung",
    "use integer.tung",
    "use table.tung",
  ].join("\n");
  const ranges = buildSemanticRanges(source);
  for (const [line, text] of source.split("\n").entries()) {
    const start = "use ".length;
    const end = text.length;
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
test("semantic analysis markeþ a use alias as a namespace", () => {
  const source = "use ilk/list.tung list yield list~empty";
  assert.deepEqual(semanticLabelsOf(source, "list"), [
    "namespace:declaration",
  ]);
  const ranges = buildSemanticRanges(source);
  const use = source.lastIndexOf("list~empty");
  assert(
    ranges.some(({ char, length, type }) =>
      char === use && length === "list".length && type === "namespace"
    ),
  );
});
test("semantic analysis markeþ a default basename alias as a namespace", () => {
  for (const member of ["empty", "_*"]) {
    const source = `use ilk/list.tung yield list~${member}`;
    const ranges = buildSemanticRanges(source);
    const use = source.lastIndexOf(`list~${member}`);
    assert(
      ranges.some(({ char, length, type }) =>
        char === use && length === "list".length && type === "namespace"
      ),
    );
  }
});
test("semantic analysis markeþ a qualified name before import resolution", () => {
  const source = "let value = list~empty";
  const ranges = buildSemanticRanges(source);
  const use = source.indexOf("list~empty");
  assert(
    ranges.some(({ char, length, type }) =>
      char === use && length === "list".length && type === "namespace"
    ),
  );
  assert(
    ranges.some(({ char, length, type }) =>
      char === use + "list~".length && length === "empty".length &&
      type === "variable"
    ),
  );
});
test("semantic analysis doth not expose a path-qualified namespace", () => {
  const source = "use ilk/list.tung yield ilk/list~empty";
  const ranges = buildSemanticRanges(source);
  const use = source.lastIndexOf("ilk/list~empty");
  assert.equal(
    ranges.some(({ char, type }) => char === use && type === "namespace"),
    false,
  );
});
test("semantic analysis leaveþ standalone export names plain", () => {
  const source = [
    "show write, write-line, read",
    "show +, zero, ×, one, -, ÷, %",
    "show-ilk ℤ, float",
    "show let (text: text) write-line: 𝟙 ! console = text",
  ].join("\n");
  const ranges = buildSemanticRanges(source);
  const afterKeyword = (line, keyword) =>
    ranges.filter((token) =>
      token.line === line && token.char > keyword.length
    );
  for (const line of [0, 1]) {
    assert.equal(
      afterKeyword(line, "show").length,
      0,
      source.split("\n")[line],
    );
  }
  assert.deepEqual(
    afterKeyword(2, "show-ilk").map((token) => token.type),
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
