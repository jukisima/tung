const assert = require("node:assert/strict");
const test = require("node:test");
const { analyzeDocument, findDefinition, tokenAtPosition } = require("../server/analysis");

test("analysis records shown owners, members, parameters, and imports", () => {
  const source = "bring ground.tung; graith a equal show shape a order { a ≤ a: 𝟚; let a < b = a ≤ b };";
  const model = analyzeDocument(source, "file:///model.tung");
  assert.deepEqual(model.imports.map(({ path }) => path), ["ground.tung"]);
  assert(model.definitions.some(({ name, role, exported }) => name === "order" && role === "shape" && exported));
  assert(model.definitions.some(({ name, role, exported, containerName }) => name === "≤" && role === "method" && exported && containerName === "order"));
  assert(model.definitions.some(({ name, role, local }) => name === "a" && role === "parameter" && local));
});

test("analysis records foreign import paths", () => {
  const source = "bring foreign.tung;\nbring algebra/field.tung;";
  const model = analyzeDocument(source, "file:///imports.tung");

  assert.deepEqual(model.imports.map(({ path }) => path), ["foreign.tung", "algebra/field.tung"]);
});

test("analysis keeps term and type re-exports distinct", () => {
  const model = analyzeDocument("show value; show-ilk value;", "file:///exports.tung");
  assert.deepEqual(model.reexports, [
    { name: "value", kind: "term" },
    { name: "value", kind: "type" }
  ]);
});

test("local resolution prefers the narrowest binder scope", () => {
  const source = "let (x: integer) keep: integer = match x { x | x };";
  const model = analyzeDocument(source, "file:///scope.tung");
  const use = model.tokens.filter(({ text }) => text === "x").at(-1);
  const definition = findDefinition(model, use, use.offset);
  assert.equal(definition.role, "parameter");
  assert.equal(definition.token.offset, model.tokens.filter(({ text }) => text === "x").at(-2).offset);
});

test("token lookup excludes the character after a token", () => {
  const model = analyzeDocument("let answer = 42;", "file:///token.tung");
  assert.equal(tokenAtPosition(model, { line: 0, character: 4 }).text, "answer");
  assert.equal(tokenAtPosition(model, { line: 0, character: 10 }), undefined);
});
