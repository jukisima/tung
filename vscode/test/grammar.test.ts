import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import test from "node:test";
import * as oniguruma from "vscode-oniguruma";
import * as textmate from "vscode-textmate";
import { tokenModifiers, tokenTypes } from "../server/highlight.ts";
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
test("textmate immediately scopeþ lexical syntax", async () => {
  const loaded = await loadGrammar();
  for (const [source, name, scope] of [
    [
      "use flock/functor.tung functor",
      "flock/functor.tung",
      "string.unquoted.import-path.tung",
    ],
    ["use flock/functor.tung functor", "functor", "entity.name.namespace.tung"],
    ["let value [] '𝟙\\128512;'", "'𝟙", "string.quoted.single.tung"],
    ["let value [] `\\128512;", "`\\128512;", "constant.character.tung"],
    ["let value [] 12.5", "12.5", "constant.numeric.float.tung"],
    ["# a comment", "#", "comment.line.number-sign.tung"],
    ["/* a comment */", "a comment", "comment.block.tung"],
    ["show let identity [x:ℤ, ℤ] x", "let", "keyword.declaration.tung"],
    ["match value {x ^ x}", "match", "keyword.control.tung"],
    ["a → b", "→", "support.type.tung"],
    ["@a:*", "*", "keyword.other.tung"],
    ["@a:*", "@", "keyword.other.tung"],
    ["byzen a equal", "byzen", "keyword.declaration.tung"],
    ["r{field = 1}", "r{", "punctuation.section.braces.begin.tung"],
  ]) {
    const position = source.lastIndexOf(name);
    const token = loaded
      .tokenizeLine(source)
      .tokens.find(
        ({ startIndex, endIndex }) =>
          startIndex <= position && position < endIndex,
      );
    assert(token?.scopes.includes(scope), `${source}: ${scope}`);
  }
});
test("textmate leaveþ name roles to the compiler", async () => {
  const loaded = await loadGrammar();
  let state = textmate.INITIAL;
  for (const source of [
    "show flock functor [f:[*, *]] {",
    "  let map [@a:*, @b:*, @e:*, a f, [a, b; e], b f; e]",
    "}",
    "show let void [@a:*, @f:[*, *], af:a f, 𝟙 f]",
    "  af map {_ ^ only}",
    "show let read-paþ [text paþ:paþ, text; file, text fail]",
    "show let join-paþ [text₀ paþ:paþ, text₁ paþ:paþ, paþ]",
    "x _* xs · list~_*",
  ]) {
    const result = loaded.tokenizeLine(source, state);
    state = result.ruleStack;
    assert(
      result.tokens.every(
        ({ startIndex, endIndex, scopes }) =>
          source.slice(startIndex, endIndex) === "*" ||
          !scopes.some(
            (scope) =>
              scope.startsWith("support.type") || scope.startsWith("variable."),
          ),
      ),
      source,
    );
  }
});
test("special openers remain paired in language configuration", () => {
  for (const opener of ["r{", "{", "<{", ">{"]) {
    assert(
      languageConfiguration.brackets.some(
        ([open, close]) => open === opener && close === "}",
      ),
    );
  }
});
test("manifest mapeþ semantic roles to theme scopes", () => {
  const defaults = manifest.contributes.configurationDefaults["[tung]"];
  const scopes = manifest.contributes.semanticTokenScopes[0].scopes;
  const semanticDefaults =
    manifest.contributes.configurationDefaults[
      "editor.semanticTokenColorCustomizations"
    ];
  const semanticTypes = Object.fromEntries(
    manifest.contributes.semanticTokenTypes.map(({ id, superType }) => [
      id,
      superType,
    ]),
  );
  assert.equal(defaults["editor.semanticHighlighting.enabled"], true);
  assert(tokenTypes.includes("class"));
  assert.equal(semanticDefaults.rules["parameter:tung"].italic, false);
  assert.equal(semanticDefaults.rules["type:tung"].italic, false);
  assert.equal(semanticDefaults.rules["class:tung"].italic, false);
  for (const selector of [
    "call.applied:tung",
    "type.applied:tung",
    "class.applied:tung",
    "typeParameter.applied:tung",
    "type.typeFunction:tung",
    "class.typeFunction:tung",
  ]) {
    assert.equal(semanticDefaults.rules[selector].italic, true);
  }
  assert.equal(semanticTypes.call, "variable");
  assert(
    manifest.contributes.semanticTokenModifiers.some(
      ({ id }) => id === "applied",
    ),
  );
  assert(scopes.type.includes("support.type"));
  assert(scopes.keyword.includes("keyword.control"));
  assert(scopes.function.includes("entity.name.function"));
  assert(scopes.namespace.includes("entity.name.namespace"));
  assert(scopes.parameter.includes("variable.parameter"));
  assert.deepEqual(
    scopes.class,
    scopes.type,
    "flock names use the theme's type colour",
  );
  assert.deepEqual(
    scopes.call,
    scopes.variable,
    "applied functions use the theme's ordinary variable colour",
  );
  assert.deepEqual(
    scopes["call.effect"],
    scopes.call,
    "effect calls keep the theme's ordinary variable colour",
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
  assert.equal(
    manifest.contributes.configuration.properties["tung.tonguePath"].default,
    "",
  );
  assert.deepEqual(
    manifest.contributes.configuration.properties["tung.libraryPaths"].default,
    [],
  );
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
