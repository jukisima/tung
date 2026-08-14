const path = require("path");
const vscode = require("vscode");
const { LanguageClient, TransportKind } = require("vscode-languageclient/node");

let client;

function activate(context) {
  const server = context.asAbsolutePath(path.join("server", "main.js"));
  const watcher = vscode.workspace.createFileSystemWatcher("**/*.tung");
  context.subscriptions.push(watcher);
  client = new LanguageClient(
    "tung",
    "tung language server",
    { run: { module: server, transport: TransportKind.ipc }, debug: { module: server, transport: TransportKind.ipc } },
    {
      documentSelector: [
        { language: "tung", scheme: "file" },
        { language: "tung", scheme: "untitled" }
      ],
      initializationOptions: {
        tonguePath: vscode.workspace.getConfiguration("tung").get("tonguePath")
      },
      synchronize: { fileEvents: watcher }
    }
  );
  context.subscriptions.push(client);
  return client.start().then(() => {
    context.subscriptions.push(client.onNotification("tung/semanticTokensChanged", refreshOpenSemanticTokens));
  });
}

function deactivate() {
  return client?.stop();
}

function refreshOpenSemanticTokens() {
  for (const document of vscode.workspace.textDocuments) {
    if (document.languageId !== "tung" || document.isClosed) continue;
    vscode.commands.executeCommand("vscode.provideDocumentSemanticTokens", document.uri).then(undefined, () => {});
  }
}

module.exports = { activate, deactivate, refreshOpenSemanticTokens };
