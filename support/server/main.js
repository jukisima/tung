const fs = require("fs");
const path = require("path");
const { fileURLToPath } = require("url");
const {
  CodeActionKind,
  CompletionItemKind,
  createConnection,
  DiagnosticSeverity,
  DocumentHighlightKind,
  InsertTextFormat,
  MarkupKind,
  ProposedFeatures,
  SemanticTokensBuilder,
  SymbolKind,
  TextDocuments,
  TextDocumentSyncKind
} = require("vscode-languageserver/node");
const { TextDocument } = require("vscode-languageserver-textdocument");
const { keywordHelp, languageNames, nameRange, tokenAtPosition, tokenRange } = require("./analysis");
const { CompilerBridge } = require("./checker");
const { formatDocument, formatRange } = require("./format");
const { WorkspaceIndex, toFilePath } = require("./workspace");
const { buildSemanticRanges, tokenModifiers, tokenTypes } = require("./semantic");

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);
const workspace = new WorkspaceIndex(documents);
const compiler = new CompilerBridge();
const timers = new Map();
const checks = new Map();
const diagnostics = new Map();
const typeCache = new Map();
let workspaceRoots = [];
let tongueDir;
let canRegisterWatchedFiles = false;
let canRefreshSemanticTokens = false;
let configuredTongue;
let semanticRefreshTimer;

connection.onInitialize((params) => {
  configuredTongue = params.initializationOptions?.tonguePath;
  canRegisterWatchedFiles = Boolean(params.capabilities?.workspace?.didChangeWatchedFiles?.dynamicRegistration);
  canRefreshSemanticTokens = Boolean(params.capabilities?.workspace?.semanticTokens?.refreshSupport);
  workspaceRoots = unique([
    ...(params.workspaceFolders || []).map(({ uri }) => filePath(uri)),
    filePath(params.rootUri)
  ].filter(Boolean));
  tongueDir = findTongueDir(configuredTongue);
  compiler.configure(tongueDir);
  workspace.configure(workspaceRoots, tongueDir);
  return {
    capabilities: {
      textDocumentSync: { openClose: true, change: TextDocumentSyncKind.Incremental, save: { includeText: true } },
      semanticTokensProvider: { legend: { tokenTypes, tokenModifiers }, full: true, range: true },
      completionProvider: { triggerCharacters: ["@", "/", "."] },
      hoverProvider: true,
      definitionProvider: true,
      declarationProvider: true,
      typeDefinitionProvider: true,
      implementationProvider: true,
      referencesProvider: true,
      documentHighlightProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      renameProvider: { prepareProvider: true },
      foldingRangeProvider: true,
      selectionRangeProvider: true,
      documentLinkProvider: { resolveProvider: false },
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      signatureHelpProvider: { triggerCharacters: [" ", "$"] },
      codeActionProvider: { codeActionKinds: [CodeActionKind.QuickFix] },
      workspace: { workspaceFolders: { supported: true, changeNotifications: true } }
    },
    serverInfo: { name: "tung-language-server", version: "0.1.0" }
  };
});

connection.onInitialized(() => {
  if (!canRegisterWatchedFiles) return;
  connection.client.register("workspace/didChangeWatchedFiles", {
    watchers: [{ globPattern: "**/*.tung" }]
  }).catch(() => {});
});

connection.onNotification("workspace/didChangeWorkspaceFolders", ({ event }) => {
  const { added = [], removed = [] } = event || {};
  const removedPaths = new Set(removed.map(({ uri }) => filePath(uri)).filter(Boolean));
  workspaceRoots = unique([...workspaceRoots.filter((root) => !removedPaths.has(root)), ...added.map(({ uri }) => filePath(uri)).filter(Boolean)]);
  tongueDir = findTongueDir(configuredTongue);
  compiler.configure(tongueDir);
  workspace.configure(workspaceRoots, tongueDir);
  checkAllOpenDocuments();
  scheduleSemanticRefresh();
});

connection.onDidChangeWatchedFiles(({ changes }) => {
  for (const { uri } of changes) workspace.invalidate(uri);
  workspace.invalidateFiles();
  checkAllOpenDocuments();
  scheduleSemanticRefresh();
});

connection.languages.semanticTokens.on(({ textDocument }) => semanticTokens(textDocument.uri));
connection.languages.semanticTokens.onRange(({ textDocument, range }) => semanticTokens(textDocument.uri, range));

connection.onCompletion(({ textDocument, position }) => {
  const model = workspace.model(textDocument.uri);
  if (!model) return [];
  if (inBringPath(model, position)) {
    return workspace.modulePaths(textDocument.uri).map((label) => ({ label, kind: CompletionItemKind.Module, detail: "tung module" }));
  }

  const prefix = completionPrefix(model, position);
  const definitions = [...workspace.completions(textDocument.uri, position), ...workspace.primitiveCompletions()];
  const definitionItems = definitions
    .map((definition) => definitionCompletion(definition))
    .filter(({ label }) => !prefix || label.startsWith(prefix) || label.includes(`@${prefix}`));
  const keywordItems = languageNames.keywords
    .filter((name) => !prefix || name.startsWith(prefix))
    .map((label) => ({ label, kind: CompletionItemKind.Keyword, detail: keywordHelp[label] || "tung keyword" }));
  return unique([...definitionItems, ...keywordItems], ({ label }) => label);
});

connection.onHover(async ({ textDocument, position }) => {
  const resolved = workspace.resolveAt(textDocument.uri, position);
  if (resolved.definition) {
    const definition = resolved.definition;
    const header = definition.detail || `${definition.role} ${definition.bareName}`;
    const inferred = await inspectType(definition);
    const typeText = inferred ? `\n\n**inferred type**\n\n\`\`\`tung\n${definition.bareName}: ${inferred}\n\`\`\`` : "";
    return {
      range: nameRange(resolved.token),
      contents: {
        kind: MarkupKind.Markdown,
        value: `\`\`\`tung\n${header}\n\`\`\`\n\n${roleDescription(definition)}${typeText}`
      }
    };
  }
  if (resolved.token?.kind === "keyword" && keywordHelp[resolved.token.text]) {
    return { range: tokenRange(resolved.token), contents: { kind: MarkupKind.Markdown, value: keywordHelp[resolved.token.text] } };
  }
  const primitive = workspace.primitiveCompletions().find(({ bareName }) => bareName === resolved.token?.text);
  return primitive ? { range: nameRange(resolved.token), contents: { kind: MarkupKind.Markdown, value: primitive.detail } } : null;
});

connection.onDefinition(({ textDocument, position }) => definitionLocation(textDocument.uri, position));
connection.onDeclaration(({ textDocument, position }) => definitionLocation(textDocument.uri, position));
connection.onTypeDefinition(({ textDocument, position }) => definitionLocation(textDocument.uri, position));
connection.onImplementation(({ textDocument, position }) => {
  const { definition } = workspace.resolveAt(textDocument.uri, position);
  return workspace.implementations(definition);
});

connection.onReferences(({ textDocument, position, context }) => {
  const { definition } = workspace.resolveAt(textDocument.uri, position);
  return workspace.references(definition, context.includeDeclaration);
});

connection.onDocumentHighlight(({ textDocument, position }) => {
  const { definition } = workspace.resolveAt(textDocument.uri, position);
  return workspace.references(definition, true)
    .filter(({ uri }) => uri === textDocument.uri)
    .map(({ range }) => ({ range, kind: DocumentHighlightKind.Read }));
});

connection.onDocumentSymbol(({ textDocument }) => {
  const model = workspace.model(textDocument.uri);
  if (!model) return [];
  const visible = model.definitions.filter((definition) => !definition.local && definition.topLevel);
  return visible.map((definition) => ({
    name: definition.bareName,
    detail: definition.detail,
    kind: symbolKind(definition.role),
    range: declarationRange(model, definition),
    selectionRange: definition.selectionRange
  }));
});

connection.onWorkspaceSymbol(({ query }) => {
  const wanted = query.toLocaleLowerCase();
  return workspace.models().flatMap((model) => model.definitions
    .filter((definition) => !definition.local && (!wanted || definition.bareName.toLocaleLowerCase().includes(wanted)))
    .map((definition) => ({
      name: definition.bareName,
      kind: symbolKind(definition.role),
      location: { uri: definition.uri, range: definition.selectionRange },
      containerName: definition.containerName
    })));
});

connection.onPrepareRename(({ textDocument, position }) => {
  const { token, definition, candidates } = workspace.resolveAt(textDocument.uri, position);
  if (!token || !definition || definition.primitive) return null;
  if (candidates?.length > 1) throw new Error(`cannot rename ambiguous name '${token.text}'`);
  return { range: nameRange(token), placeholder: definition.bareName };
});

connection.onRenameRequest(({ textDocument, position, newName }) => {
  if (!validName(newName)) throw new Error(`invalid tung name '${newName}'`);
  const { definition, candidates } = workspace.resolveAt(textDocument.uri, position);
  if (!definition || definition.primitive) return null;
  if (candidates?.length > 1) throw new Error("cannot rename an ambiguous name");
  const changes = {};
  for (const { uri, range } of workspace.references(definition, true)) {
    (changes[uri] ||= []).push({ range, newText: newName });
  }
  return { changes };
});

connection.onFoldingRanges(({ textDocument }) => workspace.model(textDocument.uri)?.folds || []);

connection.onSelectionRanges(({ textDocument, positions }) => {
  const model = workspace.model(textDocument.uri);
  if (!model) return [];
  return positions.map((position) => selectionRange(model, position));
});

connection.onDocumentLinks(({ textDocument }) => {
  const model = workspace.model(textDocument.uri);
  if (!model) return [];
  return model.imports.flatMap((imported) => {
    const target = workspace.resolveImport(model, imported);
    return target ? [{ range: imported.range, target: target.uri, tooltip: `open ${imported.path}` }] : [];
  });
});

connection.onDocumentFormatting(({ textDocument, options }) => {
  const document = documents.get(textDocument.uri);
  if (!document) return [];
  const source = document.getText();
  const formatted = formatDocument(source, options);
  return formatted === source ? [] : [{ range: fullRange(document), newText: formatted }];
});

connection.onDocumentRangeFormatting(({ textDocument, range, options }) => {
  const document = documents.get(textDocument.uri);
  if (!document) return [];
  const source = document.getText();
  const edit = formatRange(source, range, options);
  return edit ? [edit] : [];
});

connection.onSignatureHelp(({ textDocument, position }) => {
  const model = workspace.model(textDocument.uri);
  if (!model) return null;
  const before = model.tokens.filter((token) => token.line < position.line || token.line === position.line && token.char < position.character).at(-1);
  if (!before) return null;
  const resolved = workspace.resolveAt(textDocument.uri, { line: before.line, character: before.char });
  if (!resolved.definition || !["function", "method"].includes(resolved.definition.role)) return null;
  return { signatures: [{ label: resolved.definition.detail || resolved.definition.bareName }], activeSignature: 0, activeParameter: 0 };
});

connection.onCodeAction(({ textDocument, context }) => {
  const actions = [];
  for (const diagnostic of context.diagnostics) {
    if (/expected ';' after bring/.test(diagnostic.message)) {
      const document = documents.get(textDocument.uri);
      if (!document) continue;
      actions.push({
        title: "add bring semicolon",
        kind: CodeActionKind.QuickFix,
        diagnostics: [diagnostic],
        isPreferred: true,
        edit: { changes: { [textDocument.uri]: [{ range: { start: document.positionAt(document.getText().length), end: document.positionAt(document.getText().length) }, newText: ";" }] } }
      });
    }
  }
  return actions;
});

documents.onDidOpen(({ document }) => {
  workspace.invalidate(document.uri);
  clearTypeCache(document.uri);
  scheduleCheck(document);
});
documents.onDidChangeContent(({ document }) => {
  workspace.invalidate(document.uri);
  clearTypeCache(document.uri);
  scheduleCheck(document);
  for (const dependent of documents.all()) if (dependent.uri !== document.uri) scheduleCheck(dependent, 0);
  scheduleSemanticRefresh();
});
documents.onDidSave(({ document }) => {
  workspace.invalidate(document.uri);
  clearTypeCache(document.uri);
  scheduleCheck(document, 0);
  scheduleSemanticRefresh(0);
});
documents.onDidClose(({ document }) => {
  cancelCheck(document.uri);
  workspace.invalidate(document.uri);
  diagnostics.delete(document.uri);
  connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });
});

function semanticTokens(uri, requestedRange) {
  const model = workspace.model(uri);
  const builder = new SemanticTokensBuilder();
  const resolve = (token) => workspace.resolveAt(uri, { line: token.line, character: token.char }).definition;
  for (const token of buildSemanticRanges(model?.text || "", resolve)) {
    if (requestedRange && !rangeContainsLine(requestedRange, token.line)) continue;
    const type = tokenTypes.indexOf(token.type);
    if (type < 0) continue;
    builder.push(token.line, token.char, token.length, type, token.modifiers.reduce((bits, name) => {
      const index = tokenModifiers.indexOf(name);
      return index < 0 ? bits : bits | (1 << index);
    }, 0));
  }
  return builder.build();
}

function scheduleSemanticRefresh(delay = 50) {
  clearTimeout(semanticRefreshTimer);
  semanticRefreshTimer = setTimeout(() => {
    semanticRefreshTimer = undefined;
    connection.sendNotification("tung/semanticTokensChanged", {});
    if (canRefreshSemanticTokens) Promise.resolve(connection.languages.semanticTokens.refresh()).catch(() => {});
  }, delay);
}

function scheduleCheck(document, delay = 300) {
  cancelCheck(document.uri);
  timers.set(document.uri, setTimeout(() => runCheck(document), delay));
}

function cancelCheck(uri) {
  clearTimeout(timers.get(uri));
  timers.delete(uri);
  checks.get(uri)?.kill();
  checks.delete(uri);
}

function checkAllOpenDocuments() {
  for (const document of documents.all()) scheduleCheck(document, 0);
}

function runCheck(document) {
  timers.delete(document.uri);
  if (!tongueDir) {
    publish(document, "could not find the haskell tung project");
    return;
  }
  if (!compiler.available()) {
    publish(document, "tung is not built; run `cabal build exe:tung` in the tongue folder");
    return;
  }

  const model = workspace.model(document.uri);
  const running = compiler.check(model, workspace.importSources(model));
  checks.set(document.uri, running.process);
  running.result.then((output) => {
    if (checks.get(document.uri) !== running.process) return;
    checks.delete(document.uri);
    const current = documents.get(document.uri);
    if (!current || current.version !== document.version) return;
    const message = checkerMessage(output);
    publish(current, message === "type ok" ? undefined : message);
  });
}

function checkerMessage(output) {
  const lines = output.split(/\r?\n/).map((line) => line.trim()).filter(Boolean);
  return lines.find((line) => /^(parse error|type error|type ok)\b/.test(line)) || lines[0] || "checker gave no output";
}

function inspectType(definition) {
  if (!definition?.uri || definition.primitive || !tongueDir) return Promise.resolve(undefined);
  const model = workspace.model(definition.uri);
  if (!model) return Promise.resolve(undefined);
  const key = `${definition.uri}\0${definition.bareName}\0${model.text}`;
  if (typeCache.has(key)) return typeCache.get(key);
  const promise = new Promise((resolve) => {
    const running = compiler.typeOf(model, workspace.importSources(model), definition.bareName);
    running.result.then((output) => resolve(output.split(/\r?\n/).find((line) => line.startsWith("type: "))?.slice(6)));
  });
  typeCache.set(key, promise);
  return promise;
}

function clearTypeCache(uri) {
  for (const key of typeCache.keys()) if (key.startsWith(`${uri}\0`)) typeCache.delete(key);
}

function publish(document, message) {
  const items = message ? [{ range: errorRange(document, message), severity: DiagnosticSeverity.Error, source: "tung", message }] : [];
  diagnostics.set(document.uri, items);
  connection.sendDiagnostics({ uri: document.uri, version: document.version, diagnostics: items });
}

function errorRange(document, message) {
  const parseAt = /^parse error:\s+source:(\d+):(\d+):/m.exec(message);
  if (parseAt) {
    const start = { line: Math.max(0, Number(parseAt[1]) - 1), character: Math.max(0, Number(parseAt[2]) - 1) };
    return { start, end: { line: start.line, character: start.character + 1 } };
  }
  const name = /^type error: in '([^']+)'/.exec(message)?.[1];
  const found = name ? document.getText().indexOf(name) : -1;
  const offset = Math.max(0, found);
  const start = document.positionAt(offset);
  return { start, end: document.positionAt(offset + (found < 0 ? 1 : name.length)) };
}

function definitionLocation(uri, position) {
  const { definition } = workspace.resolveAt(uri, position);
  return definition?.uri ? { uri: definition.uri, range: definition.selectionRange } : null;
}

function definitionCompletion(definition) {
  const label = definition.completionName || definition.bareName;
  return {
    label,
    kind: completionKind(definition.role),
    detail: definition.detail,
    insertText: label,
    insertTextFormat: InsertTextFormat.PlainText,
    sortText: definition.local ? `0-${label}` : definition.primitive ? `2-${label}` : `1-${label}`
  };
}

function completionKind(role) {
  return {
    namespace: CompletionItemKind.Module,
    type: CompletionItemKind.TypeParameter,
    shape: CompletionItemKind.Interface,
    function: CompletionItemKind.Function,
    method: CompletionItemKind.Method,
    variable: CompletionItemKind.Variable,
    parameter: CompletionItemKind.Variable,
    property: CompletionItemKind.Field,
    enumMember: CompletionItemKind.EnumMember
  }[role] || CompletionItemKind.Text;
}

function symbolKind(role) {
  return {
    namespace: SymbolKind.Namespace,
    type: SymbolKind.TypeParameter,
    shape: SymbolKind.Interface,
    function: SymbolKind.Function,
    method: SymbolKind.Method,
    variable: SymbolKind.Variable,
    parameter: SymbolKind.Variable,
    property: SymbolKind.Field,
    enumMember: SymbolKind.EnumMember
  }[role] || SymbolKind.String;
}

function roleDescription(definition) {
  const where = definition.uri && toFilePath(definition.uri) ? `defined in \`${path.basename(toFilePath(definition.uri))}\`` : "";
  const visibility = definition.exported ? "shown" : definition.local ? "local" : "private";
  return [visibility, definition.role, where].filter(Boolean).join(" · ");
}

function declarationRange(model, definition) {
  const region = model.regions
    .filter(({ startOffset, endOffset }) => startOffset <= definition.token.offset && definition.token.offset <= endOffset)
    .sort((a, b) => (a.endOffset - a.startOffset) - (b.endOffset - b.startOffset))[0];
  return region?.range || definition.range;
}

function selectionRange(model, position) {
  const token = tokenAtPosition(model, position);
  const ranges = [token ? tokenRange(token) : { start: position, end: position }];
  const containing = [...model.pairs.entries()]
    .filter(([open, close]) => model.tokens[open].offset <= (token?.offset ?? model.text.length) && (token?.endOffset ?? model.text.length) <= model.tokens[close].endOffset)
    .sort(([a], [b]) => b - a);
  for (const [open, close] of containing) ranges.push(tokenRange(model.tokens[open], model.tokens[close]));
  return ranges.reduceRight((parent, range) => ({ range, ...(parent ? { parent } : {}) }), undefined);
}

function inBringPath(model, position) {
  const offset = offsetAt(model.text, position);
  return model.imports.some(({ range }) => positionInRange(position, range))
    || /(?:^|\s)bring\s+[^;\s]*$/.test(model.text.slice(0, offset));
}

function completionPrefix(model, position) {
  const offset = offsetAt(model.text, position);
  const before = model.text.slice(0, offset);
  return before.match(/[^\s#(){}\[\]:,|!=;.$→`']+$/u)?.[0] || "";
}

function fullRange(document) {
  return { start: { line: 0, character: 0 }, end: document.positionAt(document.getText().length) };
}

function rangeContainsLine(range, line) {
  return range.start.line <= line && line <= range.end.line;
}

function positionInRange(position, range) {
  return comparePosition(range.start, position) <= 0 && comparePosition(position, range.end) <= 0;
}

function comparePosition(left, right) {
  return left.line - right.line || left.character - right.character;
}

function offsetAt(text, position) {
  const lines = text.split(/\r\n|\r|\n/);
  let offset = 0;
  for (let line = 0; line < Math.min(position.line, lines.length); line += 1) offset += lines[line].length + 1;
  return offset + position.character;
}

function validName(name) {
  return Boolean(name) && !/[\s#(){}\[\]:,|!=;.$→`']/u.test(name);
}

function findTongueDir(configured) {
  const candidates = [
    configured && path.resolve(configured),
    process.env.TUNG_TONGUE && path.resolve(process.env.TUNG_TONGUE),
    ...workspaceRoots,
    ...workspaceRoots.map((root) => path.join(root, "tongue")),
    path.resolve(__dirname, "..", "..", "tongue")
  ];
  return candidates.filter(Boolean).find((candidate) => fs.existsSync(path.join(candidate, "tung.cabal")));
}

function filePath(uri) {
  try {
    return uri?.startsWith("file:") ? fileURLToPath(uri) : undefined;
  } catch {
    return undefined;
  }
}

function unique(items, keyOf = (item) => item) {
  const seen = new Set();
  return items.filter((item) => {
    const key = keyOf(item);
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

documents.listen(connection);
connection.listen();
