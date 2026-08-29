// workspace-wide import and visibility index over tolerant document models.
// text and file changes invalidate models, resolution, and public-name caches.
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import {
  analyzeDocument,
  findDefinition,
  nameRange,
  tokenAtPosition,
} from "./analysis.ts";
import { languageNames } from "./syntax.ts";
type DocumentModel = ReturnType<typeof analyzeDocument>;
type CachedModel = { text: string; model: DocumentModel };
type DocumentLike = { uri: string; getText(): string };
type DocumentStore = {
  get(uri: string): DocumentLike | undefined;
  all(): DocumentLike[];
};
type Definition = DocumentModel["definitions"][number];
const skippedDirectories = new Set([".git", "dist-newstyle", "node_modules"]);
const termRoles = new Set([
  "function",
  "method",
  "variable",
  "enumMember",
  "operator",
]);
class WorkspaceIndex {
  documents: DocumentStore;
  roots: string[];
  bookhoard: string | undefined;
  cache: Map<string, CachedModel>;
  fileUris: string[];
  filesDirty: boolean;
  modelsCache: DocumentModel[] | undefined;
  resolveImportCache: Map<string, DocumentModel | undefined>;
  publicDefinitionsCache: Map<string, Definition[]>;
  constructor(documents: DocumentStore) {
    this.documents = documents;
    this.roots = [];
    this.bookhoard = undefined;
    this.cache = new Map();
    this.fileUris = [];
    this.filesDirty = true;
    this.modelsCache = undefined;
    this.resolveImportCache = new Map();
    this.publicDefinitionsCache = new Map();
  }
  configure(roots: string[], tongue: string | undefined = undefined) {
    this.roots = [
      ...new Set(roots.filter(Boolean).map((root) => path.resolve(root))),
    ];
    this.bookhoard = tongue && path.resolve(tongue, "..", "bookhoard");
    this.invalidateFiles();
  }
  invalidate(uri) {
    if (uri) this.cache.delete(uri);
    this.invalidateDerived();
  }
  invalidateFiles() {
    this.filesDirty = true;
    this.invalidateDerived();
  }
  invalidateDerived() {
    this.modelsCache = undefined;
    this.resolveImportCache.clear();
    this.publicDefinitionsCache.clear();
  }
  model(uri) {
    const open = this.documents.get(uri);
    if (open) return this.modelForText(uri, open.getText());
    const file = toFilePath(uri);
    if (!file || !fs.existsSync(file)) return undefined;
    return this.modelForText(uri, fs.readFileSync(file, "utf8"));
  }
  modelForText(uri, text) {
    const cached = this.cache.get(uri);
    if (cached?.text === text) return cached.model;
    const model = analyzeDocument(text, uri);
    model.fills.forEach((fill) => (fill.uri = uri));
    this.cache.set(uri, { text, model });
    this.invalidateDerived();
    return model;
  }
  models() {
    if (this.modelsCache) return this.modelsCache;
    const uris = new Set(this.workspaceFileUris());
    for (const document of this.documents.all()) uris.add(document.uri);
    this.modelsCache = [...uris].map((uri) => this.model(uri)).filter(
      Boolean,
    );
    return this.modelsCache;
  }
  workspaceFileUris() {
    if (!this.filesDirty) return this.fileUris;
    const roots = new Set(this.roots);
    if (this.bookhoard) roots.add(this.bookhoard);
    const files = [];
    for (const root of roots) collectFiles(root, files);
    this.fileUris = [
      ...new Set(files.map((file) => pathToFileURL(file).href)),
    ];
    this.filesDirty = false;
    return this.fileUris;
  }
  resolveImport(model, imported) {
    const cacheKey = `${model?.uri || ""}\0${imported.path}`;
    if (this.resolveImportCache.has(cacheKey)) {
      return this.resolveImportCache.get(cacheKey);
    }
    const models = this.models();
    const byPath = new Map(
      models.map((
        candidate,
      ) => [normalPath(toFilePath(candidate.uri)), candidate]),
    );
    const candidates = [];
    const from = toFilePath(model.uri);
    if (from) {
      candidates.push(path.resolve(path.dirname(from), imported.path));
    }
    for (const root of this.roots) {
      candidates.push(path.resolve(root, imported.path));
    }
    if (this.bookhoard) {
      candidates.push(path.resolve(this.bookhoard, imported.path));
    }
    for (const candidate of candidates) {
      const found = byPath.get(normalPath(candidate));
      if (found) return this.cacheResolvedImport(cacheKey, found);
    }
    const wanted = path.basename(imported.path);
    const suffix = normalPath(imported.path);
    return this.cacheResolvedImport(
      cacheKey,
      models.find((candidate) => {
        const file = normalPath(toFilePath(candidate.uri));
        if (!file) return false;
        const base = path.basename(file);
        return file.endsWith(`/${suffix}`) || base === wanted;
      }),
    );
  }
  cacheResolvedImport(key, model) {
    this.resolveImportCache.set(key, model);
    return model;
  }
  // public definitions follow shown declarations, shown imports, and explicit
  // re-exports while the seen set preventeþ import cycles.
  publicDefinitions(model, seen = new Set()) {
    if (!model || seen.has(model.uri)) return [];
    if (seen.size === 0 && this.publicDefinitionsCache.has(model.uri)) {
      return this.publicDefinitionsCache.get(model.uri);
    }
    const nextSeen = new Set(seen).add(model.uri);
    const own = model.definitions.filter(({ exported }) => exported);
    const shownImports = model.imports
      .filter(({ exported }) => exported)
      .flatMap((imported) =>
        this.publicDefinitions(
          this.resolveImport(model, imported),
          nextSeen,
        )
      );
    const reexported = model.reexports.flatMap(({ name, kind }) =>
      this.resolveVisible(model, name, nextSeen).filter(({ role }) =>
        kind === "type" ? role === "type" : termRoles.has(role)
      )
    );
    const definitions = uniqueByKey([
      ...own,
      ...shownImports,
      ...reexported,
    ], definitionKey);
    if (seen.size === 0) {
      this.publicDefinitionsCache.set(model.uri, definitions);
    }
    return definitions;
  }
  resolveVisible(model, name, seen = new Set(), role = undefined) {
    const visibleRole = (definition) => !role || definition.role === role;
    const split = name.lastIndexOf("@");
    if (split >= 0) {
      const qualifier = name.slice(0, split);
      const bare = name.slice(split + 1);
      const imported = model.imports.find(({ namespace }) =>
        namespace === qualifier
      );
      if (imported) {
        return this.publicDefinitions(
          this.resolveImport(model, imported),
          seen,
        ).filter((definition) =>
          definition.bareName === bare && visibleRole(definition)
        );
      }
      return model.definitions.filter(
        ({ bareName, containerName }) =>
          bareName === bare && containerName === qualifier,
      ).filter(
        visibleRole,
      );
    }
    const own = model.definitions.filter(
      (definition) =>
        !definition.local && definition.bareName === name &&
        visibleRole(definition),
    );
    if (own.length) return own;
    return uniqueByKey(
      model.imports.flatMap((imported) =>
        this.publicDefinitions(
          this.resolveImport(model, imported),
          seen,
        ).filter((definition) =>
          definition.bareName === name && visibleRole(definition)
        )
      ),
      definitionKey,
    );
  }
  resolveAt(uri, position) {
    const model = this.model(uri);
    const token = model && tokenAtPosition(model, position);
    if (!model || !token || token.kind !== "name") {
      return { model, token, definition: undefined };
    }
    if (model.fills.some((fill) => fill.token.offset === token.offset)) {
      const visibleShapes = this.resolveVisibleRole(
        model,
        token.text,
        "frame",
      );
      return {
        model,
        token,
        definition: visibleShapes.length === 1 ? visibleShapes[0] : undefined,
        candidates: visibleShapes,
      };
    }
    const local = findDefinition(model, token, token.offset);
    if (local && !token.text.includes("@")) {
      return { model, token, definition: local };
    }
    const visible = this.resolveVisible(model, token.text);
    const role = model.roles.get(token.index)?.type;
    const matching = role
      ? visible.filter((definition) => definition.role === role)
      : visible;
    const candidates = matching.length ? matching : visible;
    return {
      model,
      token,
      definition: candidates.length === 1 ? candidates[0] : local,
      candidates,
    };
  }
  resolveVisibleRole(model, name, role) {
    return this.resolveVisible(model, name, new Set(), role);
  }
  references(definition, includeDeclaration = true) {
    if (!definition) return [];
    const key = definitionKey(definition);
    const locations = [];
    for (const model of this.models()) {
      for (const token of model.occurrences) {
        const resolved = this.resolveAt(model.uri, {
          line: token.line,
          character: token.char,
        }).definition;
        if (resolved && definitionKey(resolved) === key) {
          locations.push({ uri: model.uri, range: nameRange(token) });
        }
      }
    }
    if (
      includeDeclaration &&
      !locations.some(
        ({ uri, range }) =>
          uri === definition.uri &&
          sameRange(range, definition.selectionRange),
      )
    ) {
      locations.unshift({
        uri: definition.uri,
        range: definition.selectionRange,
      });
    }
    return locations;
  }
  implementations(definition) {
    if (!definition || definition.role !== "frame") return [];
    return this.models().flatMap((model) =>
      model.fills
        .filter(({ shapeName }) => shapeName === definition.bareName)
        .map(({ uri, range }) => ({ uri, range }))
    );
  }
  importSources(model, seen = new Set()) {
    if (!model || seen.has(model.uri)) return [];
    const nextSeen = new Set(seen).add(model.uri);
    const imports = [];
    for (const imported of model.imports) {
      const target = this.resolveImport(model, imported);
      if (!target) continue;
      imports.push([imported.path, target.text]);
      imports.push(...this.importSources(target, nextSeen));
    }
    return uniqueByKey(imports, ([importPath]) => importPath);
  }
  importModel(model, importPath, seen = new Set()) {
    if (!model || seen.has(model.uri)) return undefined;
    const nextSeen = new Set(seen).add(model.uri);
    for (const imported of model.imports) {
      const target = this.resolveImport(model, imported);
      if (!target) continue;
      if (imported.path === importPath) return target;
      const nested = this.importModel(target, importPath, nextSeen);
      if (nested) return nested;
    }
    return undefined;
  }
  completions(uri, position) {
    const model = this.model(uri);
    if (!model) return [];
    const token = tokenAtPosition(model, position);
    const offset = token?.offset ?? model.text.length;
    const local = model.definitions.filter(
      ({ scopeStart, scopeEnd }) => scopeStart <= offset && offset <= scopeEnd,
    );
    const imported = model.imports.flatMap((entry) => {
      const definitions = this.publicDefinitions(
        this.resolveImport(model, entry),
      );
      return definitions.flatMap((definition) => [
        definition,
        {
          ...definition,
          completionName: `${entry.namespace}@${definition.bareName}`,
        },
      ]);
    });
    return uniqueByKey(
      [...local, ...imported],
      (definition) => definition.completionName || definition.bareName,
    );
  }
  modulePaths(uri) {
    const current = toFilePath(uri);
    const result = new Set<string>();
    for (const model of this.models()) {
      const file = toFilePath(model.uri);
      if (!file || file === current) continue;
      if (current) {
        result.add(
          normalPath(path.relative(path.dirname(current), file)),
        );
      }
      if (this.bookhoard && file.startsWith(this.bookhoard)) {
        result.add(
          normalPath(path.relative(this.bookhoard, file)),
        );
      }
    }
    return [...result]
      .filter((name) =>
        name && (!name.startsWith("../") || name.endsWith(".tung"))
      )
      .sort();
  }
  primitiveCompletions() {
    const ground = this.groundModel();
    if (ground) {
      return this.publicDefinitions(ground).map((definition) => ({
        ...definition,
        modifiers: [
          ...new Set([
            ...(definition.modifiers || []),
            "defaultLibrary",
          ]),
        ],
        detail: definition.detail ||
          `bookhoard ${definition.role} ${definition.bareName}`,
      }));
    }
    return [
      ...languageNames.primitiveTypes.map((name) => primitive(name, "type")),
    ];
  }
  groundModel() {
    if (!this.bookhoard) return undefined;
    return this.model(
      pathToFileURL(path.join(this.bookhoard, "ground.tung")).href,
    );
  }
}
const collectFiles = (root, result) => {
  if (!root || !fs.existsSync(root)) return;
  let entries;
  try {
    entries = fs.readdirSync(root, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    if (entry.isDirectory() && skippedDirectories.has(entry.name)) continue;
    const file = path.join(root, entry.name);
    if (entry.isDirectory()) collectFiles(file, result);
    if (entry.isFile() && entry.name.endsWith(".tung")) result.push(file);
  }
};
const primitive = (name, role) => {
  return {
    name,
    bareName: name,
    completionName: name,
    role,
    modifiers: ["defaultLibrary"],
    detail: `built-in ${role} ${name}`,
    primitive: true,
  };
};
const definitionKey = (definition) => {
  return definition.id ||
    `${definition.uri}:${definition.token?.offset}:${definition.bareName}`;
};
const uniqueByKey = (items, keyOf = (item) => item) => {
  const seen = new Set();
  return items.filter((item) => {
    const key = keyOf(item);
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};
const sameRange = (left, right) => {
  return (
    left.start.line === right.start.line &&
    left.start.character === right.start.character &&
    left.end.line === right.end.line &&
    left.end.character === right.end.character
  );
};
const normalPath = (file) => {
  return file ? path.normalize(file).split(path.sep).join("/") : undefined;
};
const toFilePath = (uri) => {
  try {
    return uri?.startsWith("file:") ? fileURLToPath(uri) : undefined;
  } catch {
    return undefined;
  }
};
export { toFilePath, uniqueByKey, WorkspaceIndex };
