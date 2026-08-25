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
const languageConfiguration = JSON.parse(
  fs.readFileSync(
    path.join(extensionRoot, "language-configuration.json"),
    "utf8",
  ),
);
test("textmate fallback scopeþ bring paths as one import string", () => {
  const pattern = grammar.repository["import-path"].patterns[0];
  const match = "bring algebra/foreign.tung".match(
    new RegExp(pattern.match, "u"),
  );
  assert.equal(match[1], "bring");
  assert.equal(match[3], "algebra/foreign.tung");
  assert.equal(pattern.captures["1"].name, "keyword.declaration.tung");
  assert.equal(pattern.captures["3"].name, "string.unquoted.import-path.tung");
});
test("textmate fallback scopeþ a bring alias as a namespace", () => {
  const pattern = grammar.repository["import-path"].patterns[0];
  const match = "bring data/list.tung list".match(
    new RegExp(pattern.match, "u"),
  );
  assert.equal(match[3], "data/list.tung");
  assert.equal(match[5], "list");
  assert.equal(pattern.captures["5"].name, "entity.name.namespace.tung");
});
test("textmate giveþ a whole bring path one scope", async () => {
  const loaded = await loadGrammar();
  for (
    const line of [
      "bring ground.tung",
      "bring algebra/arithmetic/field.tung",
      "bring _foreign.tung",
      "bring equal.tung",
      "bring order.tung",
    ]
  ) {
    const start = "bring ".length;
    const end = line.length;
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
test("textmate scopeþ block comments", async () => {
  const tokens =
    (await loadGrammar()).tokenizeLine("let x = 1 /* hidden { = */").tokens;
  const comment = tokens.find(({ scopes }) =>
    scopes.includes("comment.block.tung")
  );
  assert(comment);
});
test("textmate scopeþ single-quoted unicode text", async () => {
  const line = "let word: text = 'λ字\\n'";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const literal = tokens.filter(({ scopes }) =>
    scopes.includes("string.quoted.single.tung")
  );
  assert.equal(
    line.slice(literal[0].startIndex, literal.at(-1).endIndex),
    "'λ字\\n'",
  );
});
test("textmate scopeþ decimal unicode escapes", async () => {
  const line = "let letter: unicode = `\\65; let word: text = '\\23383;'";
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
test("textmate keepeþ standalone export names plain", async () => {
  const loaded = await loadGrammar();
  const cases: Array<[string, string[]]> = [
    ["show write, write-line, read", ["write", "write-line", "read"]],
    ["show +, zero, ×, one, -, ∕, ÷", [
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
test("textmate giveþ standalone show-ilk names the type scope", async () => {
  const line = "show-ilk integer, item-box, 𝟚";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  for (const name of ["integer", "item-box", "𝟚"]) {
    const offset = line.indexOf(name);
    const token = tokens.find(
      ({ startIndex, endIndex }) => startIndex <= offset && offset < endIndex,
    );
    assert(token.scopes.includes("support.type.tung"), name);
  }
});
test("textmate still highlighteþ declarations prefixed by show", async () => {
  const line = "show let value: integer = 1";
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
test("textmate giveþ data declarations and annotations the theme type scope", async () => {
  const loaded = await loadGrammar();
  const cases: Array<[string, string[]]> = [
    ["show kin 𝟚 {", ["𝟚"]],
    ["show kin a option {", ["option"]],
    ["show let ¬: 𝟚 → 𝟚 = {", ["𝟚", "𝟚"]],
    ["let value: integer → float = 1", ["integer", "float"]],
    ["let value: text = 'word'", ["text"]],
    ["let value: unicode = `😀", ["unicode"]],
  ];
  for (const [line, names] of cases) {
    const tokens = loaded.tokenizeLine(line).tokens;
    const typeNames = tokens
      .filter(({ scopes }) => scopes.includes("support.type.tung"))
      .map(({ startIndex, endIndex }) => line.slice(startIndex, endIndex));
    assert.deepEqual(typeNames, names, line);
  }
});
test("textmate recogniseþ declaration and member keywords", async () => {
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
test("textmate recogniseþ the control keywords", async () => {
  const loaded = await loadGrammar();
  for (const keyword of ["yield", "try"]) {
    const line = `${keyword} value`;
    const token = loaded.tokenizeLine(line).tokens.find(
      ({ startIndex, endIndex }) =>
        line.slice(startIndex, endIndex) === keyword,
    );
    assert(token.scopes.includes("keyword.control.tung"), keyword);
  }
});
test("textmate recogniseþ the eftgin continuation keyword", async () => {
  const line = "eftgin";
  const token = (await loadGrammar()).tokenizeLine(line).tokens.find(
    ({ startIndex, endIndex }) => startIndex === 0 && endIndex === line.length,
  );
  assert(token.scopes.includes("keyword.control.tung"));
});
test("textmate recogniseþ the fremmed let-body marker", async () => {
  const line =
    "show let append-native: text → text → text = 'join-text' fremmed";
  const token = (await loadGrammar()).tokenizeLine(line).tokens.find(
    ({ startIndex, endIndex }) =>
      line.slice(startIndex, endIndex) === "fremmed",
  );
  assert(token.scopes.includes("keyword.other.tung"));
});
test("textmate recogniseþ the record opening delimiter", async () => {
  const line = "let record = r(field = r) let applied = r (field)";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const opener = tokens.find(({ startIndex, endIndex }) =>
    line.slice(startIndex, endIndex) === "r("
  );
  assert(opener.scopes.includes("punctuation.section.parens.begin.tung"));
  const names = tokens.filter(({ startIndex, endIndex }) =>
    line.slice(startIndex, endIndex) === "r"
  );
  assert(names.every(({ scopes }) => scopes.includes("variable.other.tung")));
});
test("textmate recogniseþ association opening delimiters", async () => {
  const loaded = await loadGrammar();
  for (const marker of ["<", ">"]) {
    const syntax = `${marker}(+, 1, 2)`;
    const syntaxToken = loaded.tokenizeLine(syntax).tokens.find(
      ({ startIndex, endIndex }) =>
        syntax.slice(startIndex, endIndex) === `${marker}(`,
    );
    assert(
      syntaxToken.scopes.includes("punctuation.section.parens.begin.tung"),
      syntax,
    );

    const separated = `${marker} (+, 1, 2)`;
    const separatedToken = loaded.tokenizeLine(separated).tokens.find(
      ({ startIndex, endIndex }) =>
        separated.slice(startIndex, endIndex) === marker,
    );
    assert(separatedToken.scopes.includes("variable.other.tung"), separated);

    const name = `1 ${marker} 2`;
    const nameToken = loaded.tokenizeLine(name).tokens.find(
      ({ startIndex, endIndex }) => name.slice(startIndex, endIndex) === marker,
    );
    assert(nameToken.scopes.includes("variable.other.tung"), name);
  }
});
test("language configuration keepeþ every special opener whole", () => {
  assert.deepEqual(languageConfiguration.brackets, [
    ["r(", ")"],
    ["<(", ")"],
    [">(", ")"],
    ["(", ")"],
    ["{", "}"],
  ]);
});
test("textmate recogniseþ the law equation marker", async () => {
  const line = "law (x: a): x identity ~ x";
  const tokens = (await loadGrammar()).tokenizeLine(line).tokens;
  const marker = tokens.find(
    ({ startIndex, endIndex }) => line.slice(startIndex, endIndex) === "~",
  );
  assert(marker.scopes.includes("keyword.operator.tung"));
});
test("textmate recogniseþ a term type ascription", async () => {
  const line = "let answer = (1: integer)";
  const token = (await loadGrammar()).tokenizeLine(line).tokens.find(
    ({ startIndex, endIndex }) =>
      line.slice(startIndex, endIndex) === "integer",
  );
  assert(token.scopes.includes("support.type.tung"));
});
test("textmate keepeþ law expressions distinct from parameter types", async () => {
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
test("textmate fallback keepeþ type and term categories distinct", async () => {
  const line = "let value: integer = item";
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
test("manifest mapeþ semantic roles to theme scopes", () => {
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
  assert(scopes.keyword.includes("keyword.control"));
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
test("manifest exposeþ running a tung file from the editor", () => {
  const command = manifest.contributes.commands.find(
    ({ command }) => command === "tung.runFile",
  );
  const title = manifest.contributes.menus["editor/title"].find(
    ({ command }) => command === "tung.runFile",
  );
  assert.equal(command.title, "Run Tung File");
  assert.equal(command.icon, "$(play)");
  assert.equal(title.when, "resourceLangId == tung");
  assert(manifest.activationEvents.includes("onCommand:tung.runFile"));
  assert.equal(
    manifest.contributes.configuration.properties["tung.executablePath"]
      .default,
    "",
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
    loadGrammar: (scopeName) =>
      Promise.resolve(
        scopeName === grammar.scopeName
          ? textmate.parseRawGrammar(
            JSON.stringify(grammar),
            "tung.tmLanguage.json",
          )
          : null,
      ),
  });
  loadedGrammar = registry.loadGrammar(grammar.scopeName);
  return loadedGrammar;
};
