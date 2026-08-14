const assert = require("node:assert/strict");
const childProcess = require("node:child_process");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const test = require("node:test");
const { once } = require("node:events");
const { pathToFileURL } = require("node:url");
const { createMessageConnection, StreamMessageReader, StreamMessageWriter } = require("vscode-jsonrpc/node");

test("server implements the editor workflow over stdio", async (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-lsp-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const depPath = path.join(root, "dep.tung");
  const mainPath = path.join(root, "main.tung");
  const depText = "show choose natural { zero };\nshow choose a parcel { a wrap };\nshow let (x: integer) identity: integer = x;\nshow let answer: integer = 42;\nshow shape a convert { a convert: a };\n";
  const mainText = "bring dep.tung;\nlet value: integer = answer;\nlet count: natural = zero;\nlet ratio: float = 1.5;\nlet shipment: integer parcel = 1 wrap;\nlet same: integer = 1 identity; # \"unicode 𝟙\\n\"\nlet (left: integer, middle: integer, right: integer) select: integer = middle;\nlet picker = { first, second, third | second };\nfill integer convert { let x convert = x };\nshow choose a box {\na box\n}\n";
  fs.writeFileSync(depPath, depText);
  fs.writeFileSync(mainPath, mainText);
  const depUri = pathToFileURL(depPath).href;
  const mainUri = pathToFileURL(mainPath).href;

  const server = childProcess.spawn(process.execPath, [path.join(__dirname, "..", "server", "main.js"), "--stdio"], { stdio: ["pipe", "pipe", "pipe"] });
  context.after(() => server.kill());
  let stderr = "";
  server.stderr.on("data", (chunk) => (stderr += chunk));
  const connection = createMessageConnection(new StreamMessageReader(server.stdout), new StreamMessageWriter(server.stdin));
  const semanticRefresh = requestQueue(connection, "workspace/semanticTokens/refresh");
  const semanticChanged = notificationQueue(connection, "tung/semanticTokensChanged");
  connection.listen();

  const initialized = await connection.sendRequest("initialize", {
    processId: process.pid,
    rootUri: pathToFileURL(root).href,
    workspaceFolders: [{ uri: pathToFileURL(root).href, name: "fixture" }],
    capabilities: { workspace: { semanticTokens: { refreshSupport: true } } }
  });
  const capabilities = initialized.capabilities;
  assert.equal(capabilities.textDocumentSync.change, 2);
  for (const capability of ["hoverProvider", "definitionProvider", "declarationProvider", "implementationProvider", "referencesProvider", "documentSymbolProvider", "workspaceSymbolProvider", "foldingRangeProvider", "documentFormattingProvider", "documentRangeFormattingProvider"]) {
    assert.equal(capabilities[capability], true, capability);
  }
  assert.equal(capabilities.semanticTokensProvider.full, true);
  assert.equal(capabilities.semanticTokensProvider.range, true);

  connection.sendNotification("initialized", {});
  const cleanDepDiagnostics = nextDiagnostics(connection, depUri);
  connection.sendNotification("textDocument/didOpen", { textDocument: { uri: depUri, languageId: "tung", version: 1, text: depText } });
  assert.deepEqual((await cleanDepDiagnostics).diagnostics, []);
  const cleanDiagnostics = nextDiagnostics(connection, mainUri);
  connection.sendNotification("textDocument/didOpen", { textDocument: { uri: mainUri, languageId: "tung", version: 1, text: mainText } });
  assert.deepEqual((await cleanDiagnostics).diagnostics, []);

  const staleDiskGuard = nextDiagnostics(connection, mainUri, ({ diagnostics }) => diagnostics.length > 0);
  const changedHighlight = semanticRefresh.next();
  const changedFallback = semanticChanged.next();
  connection.sendNotification("textDocument/didChange", {
    textDocument: { uri: depUri, version: 2 },
    contentChanges: [{ text: depText.replace("answer: integer = 42", "answer: string = 'changed'") }]
  });
  await changedHighlight;
  await changedFallback;
  assert.match((await staleDiskGuard).diagnostics[0].message, /^type error:/);
  const restoredImport = nextDiagnostics(connection, mainUri, ({ diagnostics }) => diagnostics.length === 0);
  const restoredHighlight = semanticRefresh.next();
  const restoredFallback = semanticChanged.next();
  connection.sendNotification("textDocument/didChange", { textDocument: { uri: depUri, version: 3 }, contentChanges: [{ text: depText }] });
  await restoredHighlight;
  await restoredFallback;
  assert.deepEqual((await restoredImport).diagnostics, []);
  const savedHighlight = semanticRefresh.next();
  const savedFallback = semanticChanged.next();
  connection.sendNotification("textDocument/didSave", { textDocument: { uri: depUri }, text: depText });
  await savedHighlight;
  await savedFallback;

  const semantic = await request("textDocument/semanticTokens/full", { textDocument: { uri: mainUri } });
  assert(semantic.data.length > 0);
  const semanticAt = decodedSemanticTokens(semantic.data, capabilities.semanticTokensProvider.legend.tokenTypes);
  for (const typeName of ["integer", "float", "natural", "parcel"]) {
    assert.equal(semanticAt.get(positionKey(positionOf(mainText, typeName))), "type", typeName);
  }
  assert.equal(semanticAt.get(positionKey(positionOf(mainText, "identity"))), "call");
  assert.equal(semanticAt.get(positionKey(positionOf(mainText, "answer"))), undefined);
  assert.notEqual(semanticAt.get(positionKey(positionOf(mainText, "natural"))), semanticAt.get(positionKey(positionOf(mainText, "identity"))));
  assert.equal(semanticAt.get(positionKey(positionOf(mainText, "convert", 1))), "method");
  for (const name of ["left", "middle", "right", "first", "second", "third"]) {
    assert.equal(semanticAt.get(positionKey(positionOf(mainText, name))), "parameter", name);
  }
  assert.equal(semanticAt.get(positionKey(positionOf(mainText, "middle", 1))), undefined);
  assert.equal(semanticAt.get(positionKey(positionOf(mainText, "second", 1))), undefined);
  for (let character = "bring ".length; character < "bring dep.tung".length; character += 1) {
    assert.equal(semanticAt.has(positionKey({ line: 0, character })), false);
  }
  const rangedSemantic = await request("textDocument/semanticTokens/range", { textDocument: { uri: mainUri }, range: { start: { line: 1, character: 0 }, end: { line: 2, character: 0 } } });
  assert(rangedSemantic.data.length > 0);
  const moduleCompletion = await request("textDocument/completion", { textDocument: { uri: mainUri }, position: { line: 0, character: 10 } });
  assert(moduleCompletion.some(({ label }) => label === "dep.tung"));

  const answerPosition = positionOf(mainText, "answer");
  const identityPosition = positionOf(mainText, "identity");
  const completion = await request("textDocument/completion", { textDocument: { uri: mainUri }, position: answerPosition });
  assert(completion.some(({ label }) => label === "answer"));
  assert(completion.some(({ label }) => label === "dep@answer"));

  const definition = await request("textDocument/definition", { textDocument: { uri: mainUri }, position: answerPosition });
  assert.equal(definition.uri, depUri);
  const hover = await request("textDocument/hover", { textDocument: { uri: mainUri }, position: answerPosition });
  assert.match(hover.contents.value, /let answer: integer/);
  assert.match(hover.contents.value, /inferred type/);

  const references = await request("textDocument/references", { textDocument: { uri: mainUri }, position: answerPosition, context: { includeDeclaration: true } });
  assert.deepEqual(new Set(references.map(({ uri }) => uri)), new Set([depUri, mainUri]));
  const rename = await request("textDocument/rename", { textDocument: { uri: mainUri }, position: answerPosition, newName: "result" });
  assert.equal(rename.changes[depUri][0].newText, "result");
  assert.equal(rename.changes[mainUri][0].newText, "result");
  await assert.rejects(request("textDocument/rename", { textDocument: { uri: mainUri }, position: answerPosition, newName: "not a name" }), /invalid tung name/);
  const primitiveRename = await request("textDocument/prepareRename", { textDocument: { uri: mainUri }, position: positionOf(mainText, "integer") });
  assert.equal(primitiveRename, null);

  const symbols = await request("textDocument/documentSymbol", { textDocument: { uri: mainUri } });
  assert(symbols.some(({ name }) => name === "value"));
  assert(symbols.some(({ name }) => name === "box"));
  const workspaceSymbols = await request("workspace/symbol", { query: "answer" });
  assert(workspaceSymbols.some(({ location }) => location.uri === depUri));
  const convertPosition = positionOf(mainText, "convert");
  const implementations = await request("textDocument/implementation", { textDocument: { uri: mainUri }, position: convertPosition });
  assert(implementations.some(({ uri }) => uri === mainUri));

  const links = await request("textDocument/documentLink", { textDocument: { uri: mainUri } });
  assert.deepEqual(links.map(({ target }) => target), [depUri]);
  const folds = await request("textDocument/foldingRange", { textDocument: { uri: mainUri } });
  assert(folds.some(({ startLine }) => startLine === positionOf(mainText, "show").line));
  const selections = await request("textDocument/selectionRange", { textDocument: { uri: mainUri }, positions: [positionOf(mainText, "box", 1)] });
  assert(selections[0].parent);

  const signature = await request("textDocument/signatureHelp", { textDocument: { uri: mainUri }, position: { line: identityPosition.line, character: identityPosition.character + "identity".length } });
  assert.match(signature.signatures[0].label, /identity/);

  const formatted = await request("textDocument/formatting", { textDocument: { uri: mainUri }, options: { tabSize: 2, insertSpaces: true } });
  assert.match(formatted[0].newText, /show choose a box \{\n  a box\n\}/);
  const boxStartLine = positionOf(mainText, "show choose a box").line;
  const rangeFormatted = await request("textDocument/rangeFormatting", { textDocument: { uri: mainUri }, range: { start: { line: boxStartLine, character: 0 }, end: { line: mainText.split("\n").length - 1, character: 0 } }, options: { tabSize: 2, insertSpaces: true } });
  assert.match(rangeFormatted[0].newText, /  a box/);

  const badDiagnostics = nextDiagnostics(connection, mainUri, ({ diagnostics }) => diagnostics.length > 0);
  const badText = mainText.replace("let value: integer = answer;", "let value: string = answer;");
  connection.sendNotification("textDocument/didChange", { textDocument: { uri: mainUri, version: 2 }, contentChanges: [{ text: badText }] });
  assert.match((await badDiagnostics).diagnostics[0].message, /^type error:/);

  const missingSemicolon = "bring dep.tung";
  const parseDiagnostics = nextDiagnostics(connection, mainUri, ({ diagnostics }) => diagnostics.length > 0 && /expected ';' after bring/.test(diagnostics[0].message));
  connection.sendNotification("textDocument/didChange", { textDocument: { uri: mainUri, version: 3 }, contentChanges: [{ text: missingSemicolon }] });
  const parseResult = await parseDiagnostics;
  const actions = await request("textDocument/codeAction", { textDocument: { uri: mainUri }, range: parseResult.diagnostics[0].range, context: { diagnostics: parseResult.diagnostics } });
  assert.equal(actions[0].title, "add bring semicolon");

  connection.sendNotification("textDocument/didClose", { textDocument: { uri: mainUri } });
  connection.sendNotification("textDocument/didClose", { textDocument: { uri: depUri } });
  await connection.sendRequest("shutdown");
  connection.sendNotification("exit");
  await once(server, "exit");
  connection.dispose();
  assert.equal(stderr, "");

  function request(method, params) {
    return connection.sendRequest(method, params);
  }
});

test("server semantic tokens color real shape methods", async (context) => {
  const root = path.resolve(__dirname, "..", "..");
  const file = path.join(root, "tongue", "library", "collection", "functor.tung");
  const text = fs.readFileSync(file, "utf8");
  const uri = pathToFileURL(file).href;
  const server = childProcess.spawn(process.execPath, [path.join(__dirname, "..", "server", "main.js"), "--stdio"], { stdio: ["pipe", "pipe", "pipe"] });
  context.after(() => server.kill());
  const connection = createMessageConnection(new StreamMessageReader(server.stdout), new StreamMessageWriter(server.stdin));
  connection.listen();

  const initialized = await connection.sendRequest("initialize", {
    processId: process.pid,
    rootUri: pathToFileURL(root).href,
    workspaceFolders: [{ uri: pathToFileURL(root).href, name: "prog" }],
    capabilities: {}
  });
  connection.sendNotification("initialized", {});
  connection.sendNotification("textDocument/didOpen", { textDocument: { uri, languageId: "tung", version: 1, text } });

  const semantic = await connection.sendRequest("textDocument/semanticTokens/full", { textDocument: { uri } });
  const semanticAt = decodedSemanticTokens(semantic.data, initialized.capabilities.semanticTokensProvider.legend.tokenTypes);
  assert.equal(semanticAt.get(positionKey(positionOf(text, "map"))), "method");

  connection.sendNotification("textDocument/didClose", { textDocument: { uri } });
  await connection.sendRequest("shutdown");
  connection.sendNotification("exit");
  await once(server, "exit");
  connection.dispose();
});

test("server sends semantic fallback notification without refresh capability", async (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-lsp-fallback-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const file = path.join(root, "main.tung");
  const text = "let value: integer = 1;\n";
  fs.writeFileSync(file, text);
  const uri = pathToFileURL(file).href;
  const server = childProcess.spawn(process.execPath, [path.join(__dirname, "..", "server", "main.js"), "--stdio"], { stdio: ["pipe", "pipe", "pipe"] });
  context.after(() => server.kill());
  const connection = createMessageConnection(new StreamMessageReader(server.stdout), new StreamMessageWriter(server.stdin));
  const semanticChanged = notificationQueue(connection, "tung/semanticTokensChanged");
  connection.listen();

  await connection.sendRequest("initialize", {
    processId: process.pid,
    rootUri: pathToFileURL(root).href,
    workspaceFolders: [{ uri: pathToFileURL(root).href, name: "fixture" }],
    capabilities: {}
  });
  connection.sendNotification("initialized", {});
  connection.sendNotification("textDocument/didOpen", { textDocument: { uri, languageId: "tung", version: 1, text } });
  const changed = semanticChanged.next();
  connection.sendNotification("textDocument/didSave", { textDocument: { uri }, text });
  await changed;

  connection.sendNotification("textDocument/didClose", { textDocument: { uri } });
  await connection.sendRequest("shutdown");
  connection.sendNotification("exit");
  await once(server, "exit");
  connection.dispose();
});

function nextDiagnostics(connection, uri, accept = () => true) {
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error(`timed out waiting for diagnostics for ${uri}`)), 30000);
    const disposable = connection.onNotification("textDocument/publishDiagnostics", (params) => {
      if (params.uri !== uri || !accept(params)) return;
      clearTimeout(timeout);
      disposable.dispose();
      resolve(params);
    });
  });
}

function requestQueue(connection, method) {
  const queued = [];
  const waiting = [];
  connection.onRequest(method, () => {
    const resolve = waiting.shift();
    if (resolve) resolve();
    else queued.push(true);
    return null;
  });
  return {
    next() {
      if (queued.length) {
        queued.shift();
        return Promise.resolve();
      }
      return new Promise((resolve, reject) => {
        const timeout = setTimeout(() => reject(new Error(`timed out waiting for ${method}`)), 30000);
        waiting.push(() => {
          clearTimeout(timeout);
          resolve();
        });
      });
    }
  };
}

function notificationQueue(connection, method) {
  const queued = [];
  const waiting = [];
  connection.onNotification(method, () => {
    const resolve = waiting.shift();
    if (resolve) resolve();
    else queued.push(true);
  });
  return {
    next() {
      if (queued.length) {
        queued.shift();
        return Promise.resolve();
      }
      return new Promise((resolve, reject) => {
        const timeout = setTimeout(() => reject(new Error(`timed out waiting for ${method}`)), 30000);
        waiting.push(() => {
          clearTimeout(timeout);
          resolve();
        });
      });
    }
  };
}

function positionOf(text, needle, occurrence = 0) {
  let offset = -1;
  for (let found = 0; found <= occurrence; found += 1) offset = text.indexOf(needle, offset + 1);
  assert(offset >= 0, needle);
  const before = text.slice(0, offset).split("\n");
  return { line: before.length - 1, character: before.at(-1).length };
}

function decodedSemanticTokens(data, tokenTypes) {
  const result = new Map();
  let line = 0;
  let character = 0;
  for (let index = 0; index < data.length; index += 5) {
    line += data[index];
    character = data[index] === 0 ? character + data[index + 1] : data[index + 1];
    result.set(`${line}:${character}`, tokenTypes[data[index + 3]]);
  }
  return result;
}

function positionKey({ line, character }) {
  return `${line}:${character}`;
}
