import assert from "node:assert/strict";
import * as childProcess from "node:child_process";
import { once } from "node:events";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";
import { pathToFileURL } from "node:url";
import {
  createMessageConnection,
  StreamMessageReader,
  StreamMessageWriter,
} from "vscode-jsonrpc/node";
// lsp results are deliberately heterogeneous in this integration harness.
// deno-lint-ignore no-explicit-any
type LspTestResult = any;
test("server implementeþ the editor workflow over stdio", async (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-lsp-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const depPath = path.join(root, "dep.tung");
  const mainPath = path.join(root, "main.tung");
  const depText =
    "show ilk natural { zero }\nshow ilk a parcel { a wrap }\nshow let (x: integer) identity: integer = x\nshow let answer: integer = 42\nshow frame a convert { let a convert: a }\n";
  const mainText =
    'use dep.tung\nlet value: integer = answer\nlet count: natural = zero\nlet ratio: float = 1.5\nlet shipment: integer parcel = 1 wrap\nlet same: integer = 1 identity # "unicode 𝟙\\n"\nlet (left: integer, middle: integer, right: integer) select: integer = middle\nlet picker = { first, second, third | second }\nfill integer convert { let x convert = x }\nshow ilk a box {\na box\n}\n';
  fs.writeFileSync(depPath, depText);
  fs.writeFileSync(mainPath, mainText);
  const depUri = pathToFileURL(depPath).href;
  const mainUri = pathToFileURL(mainPath).href;
  const server = childProcess.spawn(
    process.execPath,
    [path.join(__dirname, "..", "server", "main.js"), "--stdio"],
    { stdio: ["pipe", "pipe", "pipe"] },
  );
  context.after(() => server.kill());
  let stderr = "";
  server.stderr.on("data", (chunk) => (stderr += chunk));
  const connection = createMessageConnection(
    new StreamMessageReader(server.stdout),
    new StreamMessageWriter(server.stdin),
  );
  const semanticRefresh = requestQueue(
    connection,
    "workspace/semanticTokens/refresh",
  );
  connection.listen();
  const request = (method, params) =>
    connection.sendRequest<LspTestResult>(method, params);
  const initialized = await connection.sendRequest<LspTestResult>(
    "initialize",
    {
      processId: process.pid,
      rootUri: pathToFileURL(root).href,
      workspaceFolders: [{ uri: pathToFileURL(root).href, name: "fixture" }],
      capabilities: { workspace: { semanticTokens: { refreshSupport: true } } },
    },
  );
  const capabilities = initialized.capabilities;
  assert.equal(capabilities.textDocumentSync.change, 2);
  for (
    const capability of [
      "hoverProvider",
      "definitionProvider",
      "declarationProvider",
      "implementationProvider",
      "referencesProvider",
      "documentSymbolProvider",
      "workspaceSymbolProvider",
      "foldingRangeProvider",
      "documentFormattingProvider",
      "documentRangeFormattingProvider",
    ]
  ) {
    assert.equal(capabilities[capability], true, capability);
  }
  assert.equal(capabilities.semanticTokensProvider.full, true);
  assert.equal(capabilities.semanticTokensProvider.range, true);
  connection.sendNotification("initialized", {});
  const cleanDepDiagnostics = nextDiagnostics(connection, depUri);
  connection.sendNotification("textDocument/didOpen", {
    textDocument: {
      uri: depUri,
      languageId: "tung",
      version: 1,
      text: depText,
    },
  });
  assert.deepEqual((await cleanDepDiagnostics).diagnostics, []);
  const cleanDiagnostics = nextDiagnostics(connection, mainUri);
  connection.sendNotification("textDocument/didOpen", {
    textDocument: {
      uri: mainUri,
      languageId: "tung",
      version: 1,
      text: mainText,
    },
  });
  assert.deepEqual((await cleanDiagnostics).diagnostics, []);
  const staleDiskGuard = nextDiagnostics(
    connection,
    mainUri,
    ({ diagnostics }) => diagnostics.length > 0,
  );
  const changedHighlight = semanticRefresh.next();
  connection.sendNotification("textDocument/didChange", {
    textDocument: { uri: depUri, version: 2 },
    contentChanges: [
      {
        text: depText.replace(
          "answer: integer = 42",
          "answer: text = 'changed'",
        ),
      },
    ],
  });
  await changedHighlight;
  assert.match((await staleDiskGuard).diagnostics[0].message, /^type error:/);
  const restoredImport = nextDiagnostics(
    connection,
    mainUri,
    ({ diagnostics }) => diagnostics.length === 0,
  );
  const restoredHighlight = semanticRefresh.next();
  connection.sendNotification("textDocument/didChange", {
    textDocument: { uri: depUri, version: 3 },
    contentChanges: [{ text: depText }],
  });
  await restoredHighlight;
  assert.deepEqual((await restoredImport).diagnostics, []);
  connection.sendNotification("textDocument/didSave", {
    textDocument: { uri: depUri },
    text: depText,
  });
  const semantic = await request("textDocument/semanticTokens/full", {
    textDocument: { uri: mainUri },
  });
  assert(semantic.data.length > 0);
  const semanticAt = decodedSemanticTokens(
    semantic.data,
    capabilities.semanticTokensProvider.legend.tokenTypes,
  );
  for (const typeName of ["integer", "float", "natural", "parcel"]) {
    assert.equal(
      semanticAt.get(positionKey(positionOf(mainText, typeName))),
      "type",
      typeName,
    );
  }
  assert.equal(
    semanticAt.get(positionKey(positionOf(mainText, "identity"))),
    "call",
  );
  assert.equal(
    semanticAt.get(positionKey(positionOf(mainText, "answer"))),
    undefined,
  );
  assert.notEqual(
    semanticAt.get(positionKey(positionOf(mainText, "natural"))),
    semanticAt.get(positionKey(positionOf(mainText, "identity"))),
  );
  assert.equal(
    semanticAt.get(positionKey(positionOf(mainText, "convert", 1))),
    "method",
  );
  for (const name of ["left", "middle", "right", "first", "second", "third"]) {
    assert.equal(
      semanticAt.get(positionKey(positionOf(mainText, name))),
      "parameter",
      name,
    );
  }
  assert.equal(
    semanticAt.get(positionKey(positionOf(mainText, "middle", 1))),
    undefined,
  );
  assert.equal(
    semanticAt.get(positionKey(positionOf(mainText, "second", 1))),
    undefined,
  );
  for (
    let character = "use ".length;
    character < "use dep.tung".length;
    character += 1
  ) {
    assert.equal(semanticAt.has(positionKey({ line: 0, character })), false);
  }
  const rangedSemantic = await request("textDocument/semanticTokens/range", {
    textDocument: { uri: mainUri },
    range: { start: { line: 1, character: 0 }, end: { line: 2, character: 0 } },
  });
  assert(rangedSemantic.data.length > 0);
  const moduleCompletion = await request("textDocument/completion", {
    textDocument: { uri: mainUri },
    position: { line: 0, character: 10 },
  });
  assert(moduleCompletion.some(({ label }) => label === "dep.tung"));
  const answerPosition = positionOf(mainText, "answer");
  const identityPosition = positionOf(mainText, "identity");
  const completion = await request("textDocument/completion", {
    textDocument: { uri: mainUri },
    position: answerPosition,
  });
  assert(completion.some(({ label }) => label === "answer"));
  assert(completion.some(({ label }) => label === "dep@answer"));
  const definition = await request("textDocument/definition", {
    textDocument: { uri: mainUri },
    position: answerPosition,
  });
  assert.equal(definition.uri, depUri);
  const hover = await request("textDocument/hover", {
    textDocument: { uri: mainUri },
    position: answerPosition,
  });
  assert.match(hover.contents.value, /let answer: integer/);
  assert.match(hover.contents.value, /inferred type/);
  const references = await request("textDocument/references", {
    textDocument: { uri: mainUri },
    position: answerPosition,
    context: { includeDeclaration: true },
  });
  assert.deepEqual(
    new Set(references.map(({ uri }) => uri)),
    new Set([depUri, mainUri]),
  );
  const rename = await request("textDocument/rename", {
    textDocument: { uri: mainUri },
    position: answerPosition,
    newName: "result",
  });
  assert.equal(rename.changes[depUri][0].newText, "result");
  assert.equal(rename.changes[mainUri][0].newText, "result");
  await assert.rejects(
    request("textDocument/rename", {
      textDocument: { uri: mainUri },
      position: answerPosition,
      newName: "not a name",
    }),
    /invalid tung name/,
  );
  const primitiveRename = await request("textDocument/prepareRename", {
    textDocument: { uri: mainUri },
    position: positionOf(mainText, "integer"),
  });
  assert.equal(primitiveRename, null);
  const symbols = await request("textDocument/documentSymbol", {
    textDocument: { uri: mainUri },
  });
  assert(symbols.some(({ name }) => name === "value"));
  assert(symbols.some(({ name }) => name === "box"));
  const workspaceSymbols = await request("workspace/symbol", {
    query: "answer",
  });
  assert(workspaceSymbols.some(({ location }) => location.uri === depUri));
  const convertPosition = positionOf(mainText, "convert");
  const implementations = await request("textDocument/implementation", {
    textDocument: { uri: mainUri },
    position: convertPosition,
  });
  assert(implementations.some(({ uri }) => uri === mainUri));
  const links = await request("textDocument/documentLink", {
    textDocument: { uri: mainUri },
  });
  assert.deepEqual(
    links.map(({ target }) => target),
    [depUri],
  );
  const folds = await request("textDocument/foldingRange", {
    textDocument: { uri: mainUri },
  });
  assert(
    folds.some(({ startLine }) =>
      startLine === positionOf(mainText, "show").line
    ),
  );
  const selections = await request("textDocument/selectionRange", {
    textDocument: { uri: mainUri },
    positions: [positionOf(mainText, "box", 1)],
  });
  assert(selections[0].parent);
  const signature = await request("textDocument/signatureHelp", {
    textDocument: { uri: mainUri },
    position: {
      line: identityPosition.line,
      character: identityPosition.character + "identity".length,
    },
  });
  assert.match(signature.signatures[0].label, /identity/);
  const formatted = await request("textDocument/formatting", {
    textDocument: { uri: mainUri },
    options: { tabSize: 2, insertSpaces: true },
  });
  assert.match(formatted[0].newText, /show ilk a box \{\n[ ]{2}a box\n\}/);
  const boxStartLine = positionOf(mainText, "show ilk a box").line;
  const rangeFormatted = await request("textDocument/rangeFormatting", {
    textDocument: { uri: mainUri },
    range: {
      start: { line: boxStartLine, character: 0 },
      end: { line: mainText.split("\n").length - 1, character: 0 },
    },
    options: { tabSize: 2, insertSpaces: true },
  });
  assert.match(rangeFormatted[0].newText, /[ ]{2}a box/);
  connection.sendNotification("textDocument/didClose", {
    textDocument: { uri: depUri },
  });
  const importedSource = depText.replace(
    "answer: integer = 42",
    "answer: integer = 'wrong'",
  );
  const importedDiagnostics = nextDiagnostics(
    connection,
    depUri,
    ({ diagnostics }) => diagnostics.length > 0,
  );
  fs.writeFileSync(depPath, importedSource);
  connection.sendNotification("workspace/didChangeWatchedFiles", {
    changes: [{ uri: depUri, type: 2 }],
  });
  const importedResult = await importedDiagnostics;
  assert.match(importedResult.diagnostics[0].message, /^type error:/);
  assert.deepEqual(
    importedResult.diagnostics[0].range.start,
    positionOf(importedSource, "'wrong'"),
  );
  const clearedImportedDiagnostics = nextDiagnostics(
    connection,
    depUri,
    ({ diagnostics }) => diagnostics.length === 0,
  );
  fs.writeFileSync(depPath, depText);
  connection.sendNotification("workspace/didChangeWatchedFiles", {
    changes: [{ uri: depUri, type: 2 }],
  });
  assert.deepEqual((await clearedImportedDiagnostics).diagnostics, []);
  const badDiagnostics = nextDiagnostics(
    connection,
    mainUri,
    ({ diagnostics }) => diagnostics.length > 0,
  );
  const badText = mainText.replace(
    "let value: integer = answer",
    "let value: text = answer",
  );
  connection.sendNotification("textDocument/didChange", {
    textDocument: { uri: mainUri, version: 2 },
    contentChanges: [{ text: badText }],
  });
  const badResult = await badDiagnostics;
  assert.match(badResult.diagnostics[0].message, /^type error:/);
  assert.deepEqual(
    badResult.diagnostics[0].range.start,
    positionOf(badText, "answer"),
  );
  const completeUse = "use dep.tung";
  const useDiagnostics = nextDiagnostics(
    connection,
    mainUri,
    ({ diagnostics }) => diagnostics.length === 0,
  );
  connection.sendNotification("textDocument/didChange", {
    textDocument: { uri: mainUri, version: 3 },
    contentChanges: [{ text: completeUse }],
  });
  assert.deepEqual((await useDiagnostics).diagnostics, []);
  connection.sendNotification("textDocument/didClose", {
    textDocument: { uri: mainUri },
  });
  await connection.sendRequest("shutdown");
  connection.sendNotification("exit");
  await once(server, "exit");
  connection.dispose();
  assert.equal(stderr, "");
});
const nextDiagnostics = (
  connection,
  uri,
  accept: (params: LspTestResult) => boolean = () => true,
) => {
  return new Promise<LspTestResult>((resolve, reject) => {
    const timeout = setTimeout(
      () => reject(new Error(`timed out waiting for diagnostics for ${uri}`)),
      30000,
    );
    const disposable = connection.onNotification(
      "textDocument/publishDiagnostics",
      (params) => {
        if (params.uri !== uri || !accept(params)) return;
        clearTimeout(timeout);
        disposable.dispose();
        resolve(params);
      },
    );
  });
};
const requestQueue = (connection, method) => {
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
      return new Promise<void>((resolve, reject) => {
        const timeout = setTimeout(
          () => reject(new Error(`timed out waiting for ${method}`)),
          30000,
        );
        waiting.push(() => {
          clearTimeout(timeout);
          resolve();
        });
      });
    },
  };
};
const positionOf = (text, needle, occurrence = 0) => {
  let offset = -1;
  for (let found = 0; found <= occurrence; found += 1) {
    offset = text.indexOf(needle, offset + 1);
  }
  assert(offset >= 0, needle);
  const before = text.slice(0, offset).split("\n");
  return { line: before.length - 1, character: before.at(-1).length };
};
const decodedSemanticTokens = (data, tokenTypes) => {
  const result = new Map();
  let line = 0;
  let character = 0;
  for (let index = 0; index < data.length; index += 5) {
    line += data[index];
    character = data[index] === 0
      ? character + data[index + 1]
      : data[index + 1];
    result.set(`${line}:${character}`, tokenTypes[data[index + 3]]);
  }
  return result;
};
const positionKey = ({ line, character }) => {
  return `${line}:${character}`;
};
