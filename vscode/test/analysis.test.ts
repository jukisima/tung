import assert from "node:assert/strict";
import test from "node:test";
import {
  analyzeDocument,
  findDefinition,
  tokenAtPosition,
} from "../server/analysis.ts";
test("analysis recordeþ shown owners, members, parameters, and imports", () => {
  const source =
    "use ground.tung graiþ a equal show frame a order-partial { let a ≤ a: 𝟚 let a < b = a ≤ b }";
  const model = analyzeDocument(source, "file:///model.tung");
  assert.deepEqual(
    model.imports.map(({ path }) => path),
    ["ground.tung"],
  );
  assert(
    model.definitions.some(
      ({ name, role, exported }) =>
        name === "order-partial" && role === "frame" && exported,
    ),
  );
  assert(
    model.definitions.some(
      ({ name, role, exported, containerName }) =>
        name === "≤" && role === "method" && exported &&
        containerName === "order-partial",
    ),
  );
  assert(
    model.definitions.some(
      ({ name, role, local }) => name === "a" && role === "parameter" && local,
    ),
  );
});
test("analysis recordeþ foreign import paths", () => {
  const source = "use _foreign.tung\nuse frame/algebra/arithmetic/field.tung";
  const model = analyzeDocument(source, "file:///imports.tung");
  assert.deepEqual(
    model.imports.map(({ path }) => path),
    ["_foreign.tung", "frame/algebra/arithmetic/field.tung"],
  );
});
test("analysis recordeþ an optional import alias separately from its path", () => {
  const model = analyzeDocument(
    "use ilk/list.tung list yield list~empty",
    "file:///imports.tung",
  );
  assert.deepEqual(
    model.imports.map(({ path, namespace, alias }) => ({
      path,
      namespace,
      alias,
    })),
    [
      {
        path: "ilk/list.tung",
        namespace: "list",
        alias: "list",
      },
    ],
  );
});
test("analysis defaulteþ an import namespace to its last path segment", () => {
  const model = analyzeDocument(
    "use ilk/list.tung yield list~empty",
    "file:///imports.tung",
  );
  assert.deepEqual(
    model.imports.map(({ namespace, alias }) => ({ namespace, alias })),
    [
      {
        namespace: "list",
        alias: undefined,
      },
    ],
  );
});
test("analysis ignoreþ dots in directories when defaulting a namespace", () => {
  const model = analyzeDocument(
    "use pkg.one/query.tung yield query~answer",
    "file:///imports.tung",
  );
  assert.equal(model.imports[0].namespace, "query");
});
test("analysis keepeþ term and type re-exports distinct", () => {
  const model = analyzeDocument(
    "show value show-ilk value",
    "file:///exports.tung",
  );
  assert.deepEqual(model.reexports, [
    { name: "value", kind: "term" },
    { name: "value", kind: "type" },
  ]);
});
test("analysis attacheþ doc comments to following shown declarations", () => {
  const source = [
    "## identity value.",
    "graiþ a equal show let (x: a) identity: a = x",
    "",
    "/**",
    " * boxed data.",
    " */",
    "show ilk a box { a box }",
  ].join("\n");
  const model = analyzeDocument(source, "file:///docs.tung");
  assert.equal(
    model.definitions.find(({ name }) => name === "identity").documentation,
    "identity value.",
  );
  assert.equal(
    model.definitions.find(({ name, role }) =>
      name === "box" && role === "type"
    ).documentation,
    "boxed data.",
  );
});
test("local resolution preferreþ the narrowest binder scope", () => {
  const source = "let (x: ℤ) keep: ℤ = match x { x @ x }";
  const model = analyzeDocument(source, "file:///scope.tung");
  const use = model.tokens.filter(({ text }) => text === "x").at(-1);
  const definition = findDefinition(model, use, use.offset);
  assert.equal(definition.role, "parameter");
  assert.equal(
    definition.token.offset,
    model.tokens.filter(({ text }) => text === "x").at(-2).offset,
  );
});
test("malformed pattern scope recovereþ through the end of the buffer", () => {
  const source = "let choose = { (left, right @ right";
  const model = analyzeDocument(source, "file:///incomplete-pattern.tung");
  const occurrences = model.tokens.filter(({ text }) => text === "right");
  const use = occurrences.at(-1);
  const definition = findDefinition(model, use, use.offset);
  assert.equal(definition.token.offset, occurrences[0].offset);
  assert.equal(definition.scopeEnd, source.length);
});
test("an unmatched inner delimiter doth not leak a pattern scope", () => {
  const source = "let choose = { (left, right @ right } let outside = right";
  const model = analyzeDocument(source, "file:///recovered-pattern.tung");
  const occurrences = model.tokens.filter(({ text }) => text === "right");
  const innerDefinition = findDefinition(
    model,
    occurrences[1],
    occurrences[1].offset,
  );
  assert.equal(innerDefinition.token.offset, occurrences[0].offset);
  assert.equal(innerDefinition.scopeEnd, source.indexOf("}"));
  assert.equal(
    findDefinition(model, occurrences[2], occurrences[2].offset),
    undefined,
  );
});
test("token lookup excludeþ the character after a token", () => {
  const model = analyzeDocument("let answer = 42", "file:///token.tung");
  assert.equal(
    tokenAtPosition(model, { line: 0, character: 4 }).text,
    "answer",
  );
  assert.equal(tokenAtPosition(model, { line: 0, character: 10 }), undefined);
});
