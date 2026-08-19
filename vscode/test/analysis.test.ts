import assert from "node:assert/strict";
import test from "node:test";
import {
  analyzeDocument,
  findDefinition,
  tokenAtPosition,
} from "../server/analysis.ts";
test("analysis recordeth shown owners, members, parameters, and imports", () => {
  const source =
    "bring ground.tung; graith a equal show shape a order-partial { a ≤ a: 𝟚; let a < b = a ≤ b };";
  const model = analyzeDocument(source, "file:///model.tung");
  assert.deepEqual(
    model.imports.map(({ path }) => path),
    ["ground.tung"],
  );
  assert(
    model.definitions.some(
      ({ name, role, exported }) =>
        name === "order-partial" && role === "shape" && exported,
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
test("analysis recordeth foreign import paths", () => {
  const source = "bring _foreign.tung;\nbring algebra/arithmetic/field.tung;";
  const model = analyzeDocument(source, "file:///imports.tung");
  assert.deepEqual(
    model.imports.map(({ path }) => path),
    ["_foreign.tung", "algebra/arithmetic/field.tung"],
  );
});
test("analysis keepeth term and type re-exports distinct", () => {
  const model = analyzeDocument(
    "show value; show-ilk value;",
    "file:///exports.tung",
  );
  assert.deepEqual(model.reexports, [
    { name: "value", kind: "term" },
    { name: "value", kind: "type" },
  ]);
});
test("analysis attacheth doc comments to following shown declarations", () => {
  const source = [
    "## identity value.",
    "graith a equal show let (x: a) identity: a = x;",
    "",
    "/**",
    " * boxed data.",
    " */",
    "show kin a box { a box };",
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
test("local resolution preferreth the narrowest binder scope", () => {
  const source = "let (x: integer) keep: integer = match x { x | x };";
  const model = analyzeDocument(source, "file:///scope.tung");
  const use = model.tokens.filter(({ text }) => text === "x").at(-1);
  const definition = findDefinition(model, use, use.offset);
  assert.equal(definition.role, "parameter");
  assert.equal(
    definition.token.offset,
    model.tokens.filter(({ text }) => text === "x").at(-2).offset,
  );
});
test("token lookup excludeth the character after a token", () => {
  const model = analyzeDocument("let answer = 42;", "file:///token.tung");
  assert.equal(
    tokenAtPosition(model, { line: 0, character: 4 }).text,
    "answer",
  );
  assert.equal(tokenAtPosition(model, { line: 0, character: 10 }), undefined);
});
