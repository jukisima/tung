import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";
import { pathToFileURL } from "node:url";
import { WorkspaceIndex } from "../server/workspace.ts";
test("workspace resolveþ only shown names across an import", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-workspace-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const dep = path.join(root, "dep.tung");
  const main = path.join(root, "main.tung");
  fs.writeFileSync(dep, "show let answer [ℤ] 42 let hidden = 0");
  fs.writeFileSync(main, "use dep.tung let value [ℤ] answer");
  const documents = { get: () => undefined, all: () => [] };
  const workspace = new WorkspaceIndex(documents);
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisible(model, "answer").length, 1);
  assert.equal(workspace.resolveVisible(model, "hidden").length, 0);
  assert.equal(workspace.resolveVisible(model, "dep~answer").length, 1);
  assert.equal(
    workspace.importModel(model, "dep.tung")?.uri,
    pathToFileURL(dep).href,
  );
});
test("workspace explicit import alias replaceeþ the default namespace", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-use-alias-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const dep = path.join(root, "data", "dep.tung");
  const main = path.join(root, "main.tung");
  fs.mkdirSync(path.dirname(dep), { recursive: true });
  fs.writeFileSync(dep, "show let answer [ℤ] 42");
  fs.writeFileSync(main, "use data/dep.tung d yield d~answer");
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisible(model, "d~answer").length, 1);
  assert.equal(workspace.resolveVisible(model, "dep~answer").length, 0);
  const labels = new Set(
    workspace.completions(model.uri, { line: 0, character: 40 }).map(
      ({ completionName, bareName }) => completionName || bareName,
    ),
  );
  assert(labels.has("d~answer"));
  assert(!labels.has("dep~answer"));
});
test("workspace resolveþ and completeþ a default basename alias", (context) => {
  const root = fs.mkdtempSync(
    path.join(os.tmpdir(), "tung-default-use-alias-"),
  );
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const dep = path.join(root, "data", "dep.tung");
  const main = path.join(root, "main.tung");
  fs.mkdirSync(path.dirname(dep), { recursive: true });
  fs.writeFileSync(dep, "show let answer [ℤ] 42");
  fs.writeFileSync(main, "use data/dep.tung yield dep~answer");
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisible(model, "dep~answer").length, 1);
  assert.equal(workspace.resolveVisible(model, "ilk/dep~answer").length, 0);
  const labels = new Set(
    workspace.completions(model.uri, { line: 0, character: 40 }).map(
      ({ completionName, bareName }) => completionName || bareName,
    ),
  );
  assert(labels.has("dep~answer"));
  assert(!labels.has("ilk/dep~answer"));
});
test("workspace default alias ignoreþ dots in import directories", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-dotted-use-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const dep = path.join(root, "pkg.one", "query.tung");
  const main = path.join(root, "main.tung");
  fs.mkdirSync(path.dirname(dep), { recursive: true });
  fs.writeFileSync(dep, "show let answer [ℤ] 42");
  fs.writeFileSync(main, "use pkg.one/query.tung yield query~answer");
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisible(model, "query~answer").length, 1);
  assert.equal(workspace.resolveVisible(model, "pkg~answer").length, 0);
});
test("workspace leaveþ duplicate imported bare names ambiguous", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-ambiguous-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  fs.writeFileSync(path.join(root, "left.tung"), "show let value = 1");
  fs.writeFileSync(path.join(root, "right.tung"), "show let value = 2");
  const main = path.join(root, "main.tung");
  fs.writeFileSync(
    main,
    "use left.tung use right.tung let answer = value",
  );
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisible(model, "value").length, 2);
  assert.equal(workspace.resolveVisible(model, "left~value").length, 1);
});
test("workspace re-exports same-spelled terms and types separately", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-namespaces-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  fs.writeFileSync(
    path.join(root, "type-only.tung"),
    "let-ilk token = ℤ let token = 1 show-ilk token",
  );
  fs.writeFileSync(
    path.join(root, "term-only.tung"),
    "let-ilk token = ℤ let token = 1 show token",
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
    "show deed ask { ask [ℤ, ℤ] }",
  );
  fs.writeFileSync(
    path.join(root, "ask-middle.tung"),
    "use ask-base.tung show-ilk ask-base~ask show ask-base~ask",
  );
  const main = path.join(root, "main.tung");
  fs.writeFileSync(
    main,
    "use ask-middle.tung let run [ℤ, ℤ; ask] { x ^ x ask }",
  );
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(workspace.resolveVisibleRole(model, "ask", "type").length, 1);
  assert.equal(workspace.resolveVisibleRole(model, "ask", "method").length, 1);
});
test("library modules do not become an implicit prelude", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-ground-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const library = path.join(root, "library");
  fs.mkdirSync(library);
  fs.writeFileSync(
    path.join(library, "ground.tung"),
    "show let offered = 1 let hidden = 2",
  );
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([], [library]);
  const completions = workspace.primitiveCompletions();
  const labels = new Set(
    completions.map(({ completionName, bareName }) =>
      completionName || bareName
    ),
  );
  assert(!labels.has("offered"));
  assert(!labels.has("hidden"));
  assert(labels.has("ℤ"));
});
test("library module paths keep their exact file names", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-module-paths-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const library = path.join(root, "library");
  fs.mkdirSync(library, { recursive: true });
  fs.writeFileSync(
    path.join(library, "_foreign.tung"),
    "show let value = 1",
  );
  const main = path.join(root, "main.tung");
  fs.writeFileSync(main, "use foreign.tung");
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([root], [library]);
  const model = workspace.model(pathToFileURL(main).href);
  assert(workspace.modulePaths(model.uri).includes("_foreign.tung"));
  assert(!workspace.modulePaths(model.uri).includes("foreign.tung"));
  assert.equal(workspace.resolveImport(model, model.imports[0]), undefined);
});
test("library roots resolve exact paths and duplicate names stay ambiguous", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-library-roots-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const project = path.join(root, "project");
  const first = path.join(root, "first");
  const second = path.join(root, "second");
  for (const directory of [project, first, second]) fs.mkdirSync(directory);
  const main = path.join(project, "main.tung");
  const module = path.join(first, "extra.tung");
  fs.writeFileSync(main, "use extra.tung yield extra~value");
  fs.writeFileSync(module, "show let value = 1");
  const workspace = new WorkspaceIndex({ get: () => undefined, all: () => [] });
  workspace.configure([project], [first, second]);
  const model = workspace.model(pathToFileURL(main).href);
  assert.equal(
    workspace.resolveImport(model, model.imports[0])?.uri,
    pathToFileURL(module).href,
  );
  assert(workspace.modulePaths(model.uri).includes("extra.tung"));
  fs.writeFileSync(path.join(second, "extra.tung"), "show let value = 2");
  workspace.invalidateFiles();
  assert.equal(workspace.resolveImport(model, model.imports[0]), undefined);
  fs.writeFileSync(path.join(project, "extra.tung"), "show let value = 3");
  workspace.invalidateFiles();
  assert.equal(
    workspace.resolveImport(model, model.imports[0])?.uri,
    pathToFileURL(path.join(project, "extra.tung")).href,
  );
});
