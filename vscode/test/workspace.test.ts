import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";
import { pathToFileURL } from "node:url";
import { WorkspaceIndex } from "../server/workspace.ts";
test("workspace resolveth only shown names across a bring", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-workspace-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const dep = path.join(root, "dep.tung");
  const main = path.join(root, "main.tung");
  fs.writeFileSync(dep, "show let answer: integer = 42; let hidden = 0;");
  fs.writeFileSync(main, "bring dep.tung; let value: integer = answer;");
  const documents = { get: () => undefined, all: () => [] };
  const workspace = new WorkspaceIndex(documents);
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisible(model, "answer").length, 1);
  assert.equal(workspace.resolveVisible(model, "hidden").length, 0);
  assert.equal(workspace.resolveVisible(model, "dep@answer").length, 1);
  assert.equal(
    workspace.importModel(model, "dep.tung")?.uri,
    pathToFileURL(dep).href,
  );
});
test("workspace leaveth duplicate imported bare names ambiguous", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-ambiguous-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  fs.writeFileSync(path.join(root, "left.tung"), "show let value = 1;");
  fs.writeFileSync(path.join(root, "right.tung"), "show let value = 2;");
  const main = path.join(root, "main.tung");
  fs.writeFileSync(
    main,
    "bring left.tung; bring right.tung; let answer = value;",
  );
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisible(model, "value").length, 2);
  assert.equal(workspace.resolveVisible(model, "left@value").length, 1);
});
test("workspace re-exports same-spelled terms and types separately", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-namespaces-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  fs.writeFileSync(
    path.join(root, "type-only.tung"),
    "let-ilk token = integer; let token = 1; show-ilk token;",
  );
  fs.writeFileSync(
    path.join(root, "term-only.tung"),
    "let-ilk token = integer; let token = 1; show token;",
  );
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const typeOnly = workspace.publicDefinitions(
    workspace.model(pathToFileURL(path.join(root, "type-only.tung")).href),
  );
  const termOnly = workspace.publicDefinitions(
    workspace.model(pathToFileURL(path.join(root, "term-only.tung")).href),
  );
  assert.deepEqual(
    typeOnly.map(({ role }) => role),
    ["type"],
  );
  assert(termOnly.length > 0);
  assert(termOnly.every(({ role }) => role !== "type"));
});
test("workspace re-exports deeds through the type namespace", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-deed-namespace-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  fs.writeFileSync(
    path.join(root, "ask-base.tung"),
    "show deed ask { integer ask: integer };",
  );
  fs.writeFileSync(
    path.join(root, "ask-middle.tung"),
    "bring ask-base.tung; show-ilk ask-base@ask; show ask-base@ask;",
  );
  const main = path.join(root, "main.tung");
  fs.writeFileSync(
    main,
    "bring ask-middle.tung; let run: integer → integer ! ask = { x | x ask };",
  );
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisibleRole(model, "ask", "type").length, 1);
  assert.equal(workspace.resolveVisibleRole(model, "ask", "method").length, 1);
});
test("workspace default completions come from the bundled ground bookhoard", () => {
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([], path.resolve(__dirname, "..", "..", "..", "tongue"));
  const completions = workspace.primitiveCompletions();
  const labels = new Set(
    completions.map(({ completionName, bareName }) =>
      completionName || bareName
    ),
  );
  assert(labels.has("lift₂"));
  assert(labels.has("write-line"));
  assert(labels.has("console"));
  assert(!labels.has("add-integer"));
});
test("bookhoard module paths keep their exact file names", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-module-paths-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const tongue = path.join(root, "tongue");
  const bookhoard = path.join(root, "bookhoard");
  fs.mkdirSync(tongue);
  fs.mkdirSync(bookhoard, { recursive: true });
  fs.writeFileSync(
    path.join(bookhoard, "_foreign.tung"),
    "show let value = 1;",
  );
  const main = path.join(root, "main.tung");
  fs.writeFileSync(main, "bring foreign.tung;");
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root], tongue);
  const model = workspace.model(pathToFileURL(main).href);
  assert(workspace.modulePaths(model.uri).includes("_foreign.tung"));
  assert(!workspace.modulePaths(model.uri).includes("foreign.tung"));
  assert.equal(workspace.resolveImport(model, model.imports[0]), undefined);
});
