const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const oniguruma = require("vscode-oniguruma");
const textmate = require("vscode-textmate");

const grammar = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "syntaxes", "tung.tmLanguage.json"), "utf8"));
const manifest = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "package.json"), "utf8"));

test("textmate fallback scopes bring paths as one import string", () => {
  const pattern = grammar.repository["import-path"].patterns[0];
  const match = "bring algebra/foreign.tung;".match(new RegExp(pattern.match, "u"));

  assert.equal(match[1], "bring");
  assert.equal(match[3], "algebra/foreign.tung");
  assert.equal(pattern.captures["1"].name, "keyword.declaration.tung");
  assert.equal(pattern.captures["3"].name, "string.unquoted.import-path.tung");
});

test("textmate gives a whole bring path one scope", async () => {
  const loaded = await loadGrammar();

  for (const line of ["bring ground.tung;", "bring algebra/field.tung;", "bring foreign.tung;", "bring equal.tung;", "bring order.tung;"]) {
    const start = "bring ".length;
    const end = line.indexOf(";");
    const pathTokens = loaded.tokenizeLine(line).tokens.filter((token) => token.startIndex < end && token.endIndex > start);

    assert.deepEqual(pathTokens.map(({ startIndex, endIndex }) => [startIndex, endIndex]), [[start, end]], line);
    assert(pathTokens[0].scopes.includes("string.unquoted.import-path.tung"), line);
  }
});

test("textmate keeps standalone export names plain", async () => {
  const loaded = await loadGrammar();
  const cases = [
    ["show write, write-line, read;", ["write", "write-line", "read"]],
    ["show +, zero, ×, one, -, ÷;", ["+", "zero", "×", "one", "-", "÷"]]
  ];
  for (const [line, names] of cases) {
    const tokens = loaded.tokenizeLine(line).tokens;
    for (const name of names) {
      const offset = line.indexOf(name, line.indexOf(" "));
      const token = tokens.find(({ startIndex, endIndex }) => startIndex <= offset && offset < endIndex);
      assert.deepEqual(token.scopes, ["source.tung", "meta.export.tung"], `${line} (${name})`);
    }
  }
});

test("textmate gives standalone show-ilk names the type scope", async () => {
  const line = "show-ilk integer, item-box, 𝟚;";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  for (const name of ["integer", "item-box", "𝟚"]) {
    const offset = line.indexOf(name);
    const token = tokens.find(({ startIndex, endIndex }) => startIndex <= offset && offset < endIndex);
    assert(token.scopes.includes("support.type.tung"), name);
  }
});

test("textmate still highlights declarations prefixed by show", async () => {
  const line = "show let value: integer = 1;";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const at = (name) => tokens.find(({ startIndex, endIndex }) => startIndex <= line.indexOf(name) && line.indexOf(name) < endIndex);

  assert(at("integer").scopes.includes("support.type.tung"));
  assert(at("value").scopes.includes("variable.other.tung"));
  assert.equal(at("value").scopes.includes("meta.export.tung"), false);
});

test("textmate gives data declarations and annotations the theme type scope", async () => {
  const loaded = await loadGrammar();
  const cases = [
    ["show choose 𝟚 {", ["𝟚"]],
    ["show choose a option {", ["option"]],
    ["show let ¬: 𝟚 → 𝟚 = {", ["𝟚", "𝟚"]],
    ["let value: integer → float = 1;", ["integer", "float"]]
  ];

  for (const [line, names] of cases) {
    const tokens = loaded.tokenizeLine(line).tokens;
    const typeNames = tokens
      .filter(({ scopes }) => scopes.includes("support.type.tung"))
      .map(({ startIndex, endIndex }) => line.slice(startIndex, endIndex));
    assert.deepEqual(typeNames, names, line);
  }
});

test("textmate recognizes choose and deed as declaration keywords", async () => {
  const loaded = await loadGrammar();
  for (const [line, keyword] of [["choose option {", "choose"], ["deed ask {", "deed"]]) {
    const token = loaded.tokenizeLine(line).tokens.find(({ startIndex, endIndex }) => startIndex === 0 && endIndex === keyword.length);
    assert(token.scopes.includes("keyword.declaration.tung"), line);
  }
});

test("textmate fallback keeps type and term categories distinct", async () => {
  const line = "let value: integer = item;";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const scopesOf = (name) => {
    const start = line.indexOf(name);
    return tokens.find(({ startIndex, endIndex }) => startIndex <= start && start < endIndex).scopes;
  };

  assert(scopesOf("integer").includes("support.type.tung"));
  assert(scopesOf("item").includes("variable.other.tung"));
  assert.equal(scopesOf("integer").includes("variable.other.tung"), false);
});

test("manifest maps semantic roles to theme scopes", () => {
  const defaults = manifest.contributes.configurationDefaults["[tung]"];
  const scopes = manifest.contributes.semanticTokenScopes[0].scopes;
  const semanticTypes = Object.fromEntries(manifest.contributes.semanticTokenTypes.map(({ id, superType }) => [id, superType]));

  assert.equal(defaults["editor.semanticHighlighting.enabled"], true);
  assert.equal(semanticTypes.shape, "type");
  assert.equal(semanticTypes.call, "function");
  assert(scopes.type.includes("support.type"));
  assert(scopes.function.includes("entity.name.function"));
  assert(scopes.parameter.includes("variable.parameter"));
  assert.deepEqual(scopes.shape, scopes.type, "shape names use the theme's type color");
  assert.deepEqual(scopes.call, scopes.function, "function positions use the theme's function color");
  assert.equal(scopes.type.some((scope) => scopes.function.includes(scope)), false);
  assert.equal(scopes.type.some((scope) => scopes.parameter.includes(scope)), false);
});

test("textmate fallback does not treat library data types as primitives", () => {
  const pattern = new RegExp(grammar.repository["primitive-type"].patterns[0].match, "u");

  assert.equal(pattern.test("integer"), true);
  assert.equal(pattern.test("𝟘"), false);
  assert.equal(pattern.test("𝟙"), false);
  assert.equal(pattern.test("𝟚"), false);
});

let loadedGrammar;

async function loadGrammar() {
  if (loadedGrammar) return loadedGrammar;
  const wasm = fs.readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm"));
  await oniguruma.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));
  const registry = new textmate.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
      createOnigString: (source) => new oniguruma.OnigString(source)
    }),
    loadGrammar: async (scopeName) => scopeName === grammar.scopeName
      ? textmate.parseRawGrammar(JSON.stringify(grammar), "tung.tmLanguage.json")
      : null
  });
  loadedGrammar = registry.loadGrammar(grammar.scopeName);
  return loadedGrammar;
}
