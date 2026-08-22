// vscode entry point: starteth the lsp client and refresheth semantic tokens.
import * as path from "node:path";
import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";
import { documentSelector } from "./options.ts";
let client;
export const activate = (context) => {
  const server = context.asAbsolutePath(
    path.join("out", "server", "main.js"),
  );
  const watcher = vscode.workspace.createFileSystemWatcher("**/*.tung");
  context.subscriptions.push(watcher);
  client = new LanguageClient(
    "tung",
    "tung language server",
    {
      run: { module: server, transport: TransportKind.ipc },
      debug: { module: server, transport: TransportKind.ipc },
    },
    {
      documentSelector,
      initializationOptions: {
        tonguePath: vscode.workspace.getConfiguration("tung").get(
          "tonguePath",
        ),
      },
      synchronize: { fileEvents: watcher },
    },
  );
  context.subscriptions.push(client);
  return client.start().then(() => {
    context.subscriptions.push(
      client.onNotification(
        "tung/semanticTokensChanged",
        refreshOpenSemanticTokens,
      ),
    );
  });
};
export const deactivate = () => {
  return client?.stop();
};
export const refreshOpenSemanticTokens = (
  params: {
    uri?: string;
  } = {},
) => {
  const wanted = params.uri && vscode.Uri.parse(params.uri).toString();
  for (const document of vscode.workspace.textDocuments) {
    if (document.languageId !== "tung" || document.isClosed) continue;
    if (wanted && document.uri.toString() !== wanted) continue;
    vscode.commands
      .executeCommand(
        "vscode.provideDocumentSemanticTokens",
        document.uri,
      )
      .then(undefined, () => {});
  }
};
