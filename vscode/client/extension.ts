// vscode entry point: starteth the lsp client and refresheth semantic tokens.
import * as path from "node:path";
import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";
import { documentSelector } from "./options.ts";
let client: LanguageClient;
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
  context.subscriptions.push(
    client,
    vscode.commands.registerCommand("tung.runFile", runFile),
  );
  return client.start().then(() => {
    context.subscriptions.push(
      client.onNotification(
        "tung/semanticTokensChanged",
        refreshOpenSemanticTokens,
      ),
    );
  });
};
const runFile = async (resource?: vscode.Uri) => {
  const document = await runnableDocument(resource);
  if (!document) return;
  if (!(await document.save())) {
    vscode.window.showErrorMessage("save the tung file before running it");
    return;
  }
  const configured = vscode.workspace.getConfiguration("tung").get<string>(
    "executablePath",
  )?.trim();
  const discovered = configured
    ? undefined
    : await client.sendRequest<string | undefined>("tung/executable").catch(
      () => undefined,
    );
  const executable = configured || discovered || "tung";
  const folder = vscode.workspace.getWorkspaceFolder(document.uri);
  const task = new vscode.Task(
    { type: "tung", file: document.uri.fsPath },
    folder || vscode.TaskScope.Global,
    `run ${path.basename(document.uri.fsPath)}`,
    "tung",
    new vscode.ProcessExecution(executable, [document.uri.fsPath], {
      cwd: folder?.uri.fsPath || path.dirname(document.uri.fsPath),
    }),
  );
  task.presentationOptions = {
    clear: true,
    focus: false,
    panel: vscode.TaskPanelKind.Dedicated,
    reveal: vscode.TaskRevealKind.Always,
  };
  await vscode.tasks.executeTask(task);
};
const runnableDocument = async (resource?: vscode.Uri) => {
  const document = resource
    ? await vscode.workspace.openTextDocument(resource)
    : vscode.window.activeTextEditor?.document;
  if (!document || document.languageId !== "tung") {
    vscode.window.showErrorMessage("open a tung file to run it");
    return undefined;
  }
  if (document.uri.scheme !== "file") {
    vscode.window.showErrorMessage(
      "save the tung file to disk before running it",
    );
    return undefined;
  }
  return document;
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
