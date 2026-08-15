// vscode entry point: starts the lsp client and keeps structural formatting
// available locally when the server has not registered a formatter.
import * as path from "node:path";
import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";
import { formatDocument, formatRange } from "../server/format";
import { documentSelector } from "./options";
let client;
export const activate = (context) => {
  const server = context.asAbsolutePath(path.join("out", "server", "main.js"));
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
        tonguePath: vscode.workspace.getConfiguration("tung").get("tonguePath"),
      },
      synchronize: { fileEvents: watcher },
      middleware: {
        provideDocumentFormattingEdits: formatDocumentInClient,
        provideDocumentRangeFormattingEdits: formatRangeInClient,
      },
    },
  );
  context.subscriptions.push(client);
  return client.start().then(() => {
    context.subscriptions.push(
      client.onNotification("tung/semanticTokensChanged", refreshOpenSemanticTokens),
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
      .executeCommand("vscode.provideDocumentSemanticTokens", document.uri)
      .then(undefined, () => {});
  }
};
const formatDocumentInClient = (document, options) => {
  const source = document.getText();
  const formatted = formatDocument(source, options);
  return formatted === source ? [] : [vscode.TextEdit.replace(fullRange(document), formatted)];
};
const formatRangeInClient = (document, range, options) => {
  const edit = formatRange(document.getText(), toPlainRange(range), options);
  return edit ? [vscode.TextEdit.replace(toVscodeRange(edit.range), edit.newText)] : [];
};
const fullRange = (document) => {
  const lastLine = Math.max(0, document.lineCount - 1);
  return new vscode.Range(0, 0, lastLine, document.lineAt(lastLine).text.length);
};
const toPlainRange = (range) => {
  return {
    start: { line: range.start.line, character: range.start.character },
    end: { line: range.end.line, character: range.end.character },
  };
};
const toVscodeRange = (range) => {
  return new vscode.Range(
    range.start.line,
    range.start.character,
    range.end.line,
    range.end.character,
  );
};
