import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import test from "node:test";
import * as oniguruma from "vscode-oniguruma";
import * as textmate from "vscode-textmate";
import { tokenModifiers } from "../server/semantic.ts";
const extensionRoot = path.resolve(__dirname, "..", "..");
const grammar = JSON.parse(
  fs.readFileSync(
    path.join(extensionRoot, "syntaxes", "tung.tmLanguage.json"),
    "utf8",
  ),
);
const manifest = JSON.parse(
  fs.readFileSync(path.join(extensionRoot, "package.json"), "utf8"),
);
test("textmate fallback scopeth bring paths as one import string", () => {
  const pattern = grammar.repository["import-path"].patterns[0];
  const match = "bring algebra/foreign.tung;".match(
    new RegExp(pattern.match, "u"),
  );
  assert.equal(match[1], "bring");
  assert.equal(match[3], "algebra/foreign.tung");
  assert.equal(pattern.captures["1"].name, "keyword.declaration.tung");
  assert.equal(pattern.captures["3"].name, "string.unquoted.import-path.tung");
});
test("textmate giveth a whole bring path one scope", async () => {
  const loaded = await loadGrammar();
  for (
    const line of [
      "bring ground.tung;",
      "bring algebra/arithmetic/field.tung;",
      "bring _foreign.tung;",
      "bring equal.tung;",
      "bring order.tung;",
    ]
  ) {
    const start = "bring ".length;
    const end = line.indexOf(";");
    const pathTokens = loaded
      .tokenizeLine(line)
      .tokens.filter((token) =>
        token.startIndex < end && token.endIndex > start
      );
    assert.deepEqual(
      pathTokens.map(({ startIndex, endIndex }) => [startIndex, endIndex]),
      [[start, end]],
      line,
    );
    assert(
      pathTokens[0].scopes.includes("string.unquoted.import-path.tung"),
      line,
    );
  }
});
test("textmate scopeth block comments", async () => {
  const tokens =
    (await loadGrammar()).tokenizeLine("let x = 1; /* hidden { = */").tokens;
  const comment = tokens.find(({ scopes }) =>
    scopes.includes("comment.block.tung")
  );
  assert(comment);
});
test("textmate scopeth single-quoted unicode text", async () => {
  const line = "let word: text = 'λ字\\n';";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const literal = tokens.filter(({ scopes }) =>
    scopes.includes("string.quoted.single.tung")
  );
  assert.equal(
    line.slice(literal[0].startIndex, literal.at(-1).endIndex),
    "'λ字\\n'",
  );
});
test("textmate scopeth decimal unicode escapes", async () => {
  const line = "let letter: unicode = `\\65;; let word: text = '\\23383;';";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const character = tokens.find(({ scopes }) =>
    scopes.includes("constant.character.tung")
  );
  const textEscape = tokens.find(({ scopes }) =>
    scopes.includes("constant.character.escape.tung")
  );
  assert.equal(line.slice(character.startIndex, character.endIndex), "`\\65;");
  assert.equal(
    line.slice(textEscape.startIndex, textEscape.endIndex),
    "\\23383;",
  );
});
test("textmate keepeth standalone export names plain", async () => {
  const loaded = await loadGrammar();
  const cases: Array<[string, string[]]> = [
    ["show write, write-line, read;", ["write", "write-line", "read"]],
    ["show +, zero, ×, one, -, ∕, ÷;", [
      "+",
      "zero",
      "×",
      "one",
      "-",
      "∕",
      "÷",
    ]],
  ];
  for (const [line, names] of cases) {
    const tokens = loaded.tokenizeLine(line).tokens;
    for (const name of names) {
      const offset = line.indexOf(name, line.indexOf(" "));
      const token = tokens.find(
        ({ startIndex, endIndex }) => startIndex <= offset && offset < endIndex,
      );
      assert.deepEqual(
        token.scopes,
        ["source.tung", "meta.export.tung"],
        `${line} (${name})`,
      );
    }
  }
});
test("textmate giveth standalone show-ilk names the type scope", async () => {
  const line = "show-ilk integer, item-box, 𝟚;";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  for (const name of ["integer", "item-box", "𝟚"]) {
    const offset = line.indexOf(name);
    const token = tokens.find(
      ({ startIndex, endIndex }) => startIndex <= offset && offset < endIndex,
    );
    assert(token.scopes.includes("support.type.tung"), name);
  }
});
test("textmate still highlighteth declarations prefixed by show", async () => {
  const line = "show let value: integer = 1;";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const at = (name) =>
    tokens.find(
      ({ startIndex, endIndex }) =>
        startIndex <= line.indexOf(name) && line.indexOf(name) < endIndex,
    );
  assert(at("integer").scopes.includes("support.type.tung"));
  assert(at("value").scopes.includes("variable.other.tung"));
  assert.equal(at("value").scopes.includes("meta.export.tung"), false);
});
test("textmate giveth data declarations and annotations the theme type scope", async () => {
  const loaded = await loadGrammar();
  const cases: Array<[string, string[]]> = [
    ["show kin 𝟚 {", ["𝟚"]],
    ["show kin a option {", ["option"]],
    ["show let ¬: 𝟚 → 𝟚 = {", ["𝟚", "𝟚"]],
    ["let value: integer → float = 1;", ["integer", "float"]],
    ["let value: text = 'word';", ["text"]],
    ["let value: unicode = `😀;", ["unicode"]],
  ];
  for (const [line, names] of cases) {
    const tokens = loaded.tokenizeLine(line).tokens;
    const typeNames = tokens
      .filter(({ scopes }) => scopes.includes("support.type.tung"))
      .map(({ startIndex, endIndex }) => line.slice(startIndex, endIndex));
    assert.deepEqual(typeNames, names, line);
  }
});
test("textmate recogniseth declaration and member keywords", async () => {
  const loaded = await loadGrammar();
  for (
    const [line, keyword] of [
      ["kin option {", "kin"],
      ["deed ask {", "deed"],
      ["law (x: a): x ~ x", "law"],
    ]
  ) {
    const token = loaded
      .tokenizeLine(line)
      .tokens.find(({ startIndex, endIndex }) =>
        startIndex === 0 && endIndex === keyword.length
      );
    assert(token.scopes.includes("keyword.declaration.tung"), line);
  }
});
test("textmate recogniseth the foreign let-body marker", async () => {
  const line = "show let join-text: text → text → text = foreign;";
  const token = (await loadGrammar()).tokenizeLine(line).tokens.find(
    ({ startIndex, endIndex }) =>
      line.slice(startIndex, endIndex) === "foreign",
  );
  assert(token.scopes.includes("keyword.other.tung"));
});
test("textmate recogniseth the law equation marker", async () => {
  const line = "law (x: a): x identity ~ x";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const marker = tokens.find(
    ({ startIndex, endIndex }) => line.slice(startIndex, endIndex) === "~",
  );
  assert(marker.scopes.includes("keyword.operator.tung"));
});
test("textmate recogniseth a term type ascription", async () => {
  const line = "let answer = (1: integer);";
  const token = (await loadGrammar()).tokenizeLine(line).tokens.find(
    ({ startIndex, endIndex }) =>
      line.slice(startIndex, endIndex) === "integer",
  );
  assert(token.scopes.includes("support.type.tung"));
});
test("textmate keepeth law expressions distinct from parameter types", async () => {
  const line = "law (a: a): a * ∅ ~ a";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const occurrences = tokens.filter(
    ({ startIndex, endIndex }) => line.slice(startIndex, endIndex) === "a",
  );
  assert.equal(occurrences.length, 4);
  assert(
    occurrences[1].scopes.includes("support.type.tung"),
    "the parameter annotation is a type",
  );
  for (const occurrence of [occurrences[0], occurrences[2], occurrences[3]]) {
    assert(
      occurrence.scopes.includes("variable.other.tung"),
      "law binders and expressions are terms",
    );
    assert.equal(
      occurrence.scopes.includes("support.type.tung"),
      false,
      "law terms are not types",
    );
  }
});
test("textmate fallback keepeth type and term categories distinct", async () => {
  const line = "let value: integer = item;";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const scopesOf = (name) => {
    const start = line.indexOf(name);
    return tokens.find(({ startIndex, endIndex }) =>
      startIndex <= start && start < endIndex
    )
      .scopes;
  };
  assert(scopesOf("integer").includes("support.type.tung"));
  assert(scopesOf("item").includes("variable.other.tung"));
  assert.equal(scopesOf("integer").includes("variable.other.tung"), false);
});
test("manifest mapeth semantic roles to theme scopes", () => {
  const defaults = manifest.contributes.configurationDefaults["[tung]"];
  const scopes = manifest.contributes.semanticTokenScopes[0].scopes;
  const semanticTypes = Object.fromEntries(
    manifest.contributes.semanticTokenTypes.map((
      { id, superType },
    ) => [id, superType]),
  );
  assert.equal(defaults["editor.semanticHighlighting.enabled"], true);
  assert.equal(semanticTypes.shape, "type");
  assert.equal(semanticTypes.call, "function");
  assert(scopes.type.includes("support.type"));
  assert(scopes.function.includes("entity.name.function"));
  assert(scopes.parameter.includes("variable.parameter"));
  assert.deepEqual(
    scopes.shape,
    scopes.type,
    "shape names use the theme's type colour",
  );
  assert.deepEqual(
    scopes.call,
    scopes.function,
    "function positions use the theme's function colour",
  );
  assert.deepEqual(
    scopes["call.effect"],
    scopes.call,
    "effect calls keep the theme's function colour",
  );
  assert.deepEqual(
    scopes["method.effect"],
    scopes.method,
    "effect declarations keep the theme's method colour",
  );
  assert.deepEqual(
    scopes["variable.effect"],
    scopes.variable,
    "unapplied effect operations keep the theme's variable colour",
  );
  assert.equal(
    scopes.type.some((scope) => scopes.function.includes(scope)),
    false,
  );
  assert.equal(
    scopes.type.some((scope) => scopes.parameter.includes(scope)),
    false,
  );
});
test("semantic modifiers use standard lsp names or manifest contributions", () => {
  const standard = new Set([
    "declaration",
    "definition",
    "readonly",
    "static",
    "deprecated",
    "abstract",
    "async",
    "modification",
    "documentation",
    "defaultLibrary",
  ]);
  const contributed = new Set(
    manifest.contributes.semanticTokenModifiers.map(({ id }) => id),
  );
  for (const modifier of tokenModifiers) {
    assert(standard.has(modifier) || contributed.has(modifier), modifier);
  }
});
test("textmate fallback doth not treat bookhoard data types as primitives", () => {
  const pattern = new RegExp(
    grammar.repository["primitive-type"].patterns[0].match,
    "u",
  );
  assert.equal(pattern.test("integer"), true);
  assert.equal(pattern.test("𝟘"), false);
  assert.equal(pattern.test("𝟙"), false);
  assert.equal(pattern.test("𝟚"), false);
});
let loadedGrammar;
const loadGrammar = async () => {
  if (loadedGrammar) return loadedGrammar;
  const wasm = fs.readFileSync(
    require.resolve("vscode-oniguruma/release/onig.wasm"),
  );
  await oniguruma.loadWASM(
    wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength),
  );
  const registry = new textmate.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
      createOnigString: (source) => new oniguruma.OnigString(source),
    }),
    loadGrammar: async (scopeName) =>
      scopeName === grammar.scopeName
        ? textmate.parseRawGrammar(
          JSON.stringify(grammar),
          "tung.tmLanguage.json",
        )
        : null,
  });
  loadedGrammar = registry.loadGrammar(grammar.scopeName);
  return loadedGrammar;
};
