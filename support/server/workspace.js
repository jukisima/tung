const fs = require("fs");
const path = require("path");
const { fileURLToPath, pathToFileURL } = require("url");
const {
  analyzeDocument,
  findDefinition,
  languageNames,
  lastQualifiedSegment,
  nameRange,
  tokenAtPosition
} = require("./analysis");

const skippedDirectories = new Set([".git", "dist-newstyle", "node_modules"]);
const termRoles = new Set(["function", "method", "variable", "enumMember", "operator"]);

class WorkspaceIndex {
  constructor(documents) {
    this.documents = documents;
    this.roots = [];
    this.tongue = undefined;
    this.cache = new Map();
    this.fileUris = [];
    this.filesDirty = true;
  }

  configure(roots, tongue) {
    this.roots = [...new Set(roots.filter(Boolean).map((root) => path.resolve(root)))];
    this.tongue = tongue;
    this.invalidateFiles();
  }

  invalidate(uri) {
    if (uri) this.cache.delete(uri);
  }

  invalidateFiles() {
    this.filesDirty = true;
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
    return model;
  }

  models() {
    const uris = new Set(this.workspaceFileUris());
    for (const document of this.documents.all()) uris.add(document.uri);
    return [...uris].map((uri) => this.model(uri)).filter(Boolean);
  }

  workspaceFileUris() {
    if (!this.filesDirty) return this.fileUris;
    const roots = new Set(this.roots);
    if (this.tongue) roots.add(path.join(this.tongue, "library"));
    const files = [];
    for (const root of roots) collectFiles(root, files);
    this.fileUris = [...new Set(files.map((file) => pathToFileURL(file).href))];
    this.filesDirty = false;
    return this.fileUris;
  }

  resolveImport(model, imported) {
    const models = this.models();
    const byPath = new Map(models.map((candidate) => [normalPath(toFilePath(candidate.uri)), candidate]));
    const candidates = [];
    const from = toFilePath(model.uri);
    if (from) candidates.push(path.resolve(path.dirname(from), imported.path));
    for (const root of this.roots) candidates.push(path.resolve(root, imported.path));
    if (this.tongue) candidates.push(path.resolve(this.tongue, "library", imported.path));
    for (const candidate of candidates) {
      const found = byPath.get(normalPath(candidate));
      if (found) return found;
    }

    const wanted = path.basename(imported.path);
    const suffix = normalPath(imported.path);
    return models.find((candidate) => {
      const file = normalPath(toFilePath(candidate.uri));
      if (!file) return false;
      const base = path.basename(file);
      return file.endsWith(`/${suffix}`) || base === wanted || base.replace(/^_/, "") === wanted;
    });
  }

  publicDefinitions(model, seen = new Set()) {
    if (!model || seen.has(model.uri)) return [];
    const nextSeen = new Set(seen).add(model.uri);
    const own = model.definitions.filter(({ exported }) => exported);
    const shownImports = model.imports
      .filter(({ exported }) => exported)
      .flatMap((imported) => this.publicDefinitions(this.resolveImport(model, imported), nextSeen));
    const reexported = model.reexports.flatMap(({ name, kind }) =>
      this.resolveVisible(model, name, nextSeen).filter(({ role }) => kind === "type" ? role === "type" : termRoles.has(role))
    );
    return uniqueByKey([...own, ...shownImports, ...reexported], definitionKey);
  }

  resolveVisible(model, name, seen = new Set()) {
    const split = name.lastIndexOf("@");
    if (split >= 0) {
      const qualifier = name.slice(0, split);
      const bare = name.slice(split + 1);
      const imported = model.imports.find(({ namespace }) => namespace === qualifier);
      if (imported) {
        return this.publicDefinitions(this.resolveImport(model, imported), seen).filter(({ bareName }) => bareName === bare);
      }
      return model.definitions.filter(({ bareName, containerName }) => bareName === bare && containerName === qualifier);
    }

    const own = model.definitions.filter((definition) => !definition.local && definition.bareName === name);
    if (own.length) return own;
    return uniqueByKey(
      model.imports.flatMap((imported) => this.publicDefinitions(this.resolveImport(model, imported), seen).filter(({ bareName }) => bareName === name)),
      definitionKey
    );
  }

  resolveAt(uri, position) {
    const model = this.model(uri);
    const token = model && tokenAtPosition(model, position);
    if (!model || !token || token.kind !== "name") return { model, token, definition: undefined };
    if (model.fills.some((fill) => fill.token.offset === token.offset)) {
      const visibleShapes = this.resolveVisibleRole(model, token.text, "shape");
      return { model, token, definition: visibleShapes.length === 1 ? visibleShapes[0] : undefined, candidates: visibleShapes };
    }
    const local = findDefinition(model, token, token.offset);
    if (local && !token.text.includes("@")) return { model, token, definition: local };
    const visible = this.resolveVisible(model, token.text);
    const role = model.roles.get(token.index)?.type;
    const matching = role ? visible.filter((definition) => definition.role === role) : visible;
    const candidates = matching.length ? matching : visible;
    return { model, token, definition: candidates.length === 1 ? candidates[0] : local, candidates };
  }

  resolveVisibleRole(model, name, role) {
    const split = name.lastIndexOf("@");
    if (split >= 0) return this.resolveVisible(model, name).filter((definition) => definition.role === role);
    const own = model.definitions.filter((definition) => !definition.local && definition.bareName === name && definition.role === role);
    if (own.length) return own;
    return uniqueByKey(
      model.imports.flatMap((imported) => this.publicDefinitions(this.resolveImport(model, imported)).filter((definition) => definition.bareName === name && definition.role === role)),
      definitionKey
    );
  }

  references(definition, includeDeclaration = true) {
    if (!definition) return [];
    const key = definitionKey(definition);
    const locations = [];
    for (const model of this.models()) {
      for (const token of model.occurrences) {
        const resolved = this.resolveAt(model.uri, { line: token.line, character: token.char }).definition;
        if (resolved && definitionKey(resolved) === key) {
          locations.push({ uri: model.uri, range: nameRange(token) });
        }
      }
    }
    if (includeDeclaration && !locations.some(({ uri, range }) => uri === definition.uri && sameRange(range, definition.selectionRange))) {
      locations.unshift({ uri: definition.uri, range: definition.selectionRange });
    }
    return locations;
  }

  implementations(definition) {
    if (!definition || definition.role !== "shape") return [];
    return this.models().flatMap((model) => model.fills
      .filter(({ shapeName }) => shapeName === definition.bareName)
      .map(({ uri, range }) => ({ uri, range })));
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

  completions(uri, position) {
    const model = this.model(uri);
    if (!model) return [];
    const token = tokenAtPosition(model, position);
    const offset = token?.offset ?? model.text.length;
    const local = model.definitions.filter(({ scopeStart, scopeEnd }) => scopeStart <= offset && offset <= scopeEnd);
    const imported = model.imports.flatMap((entry) => {
      const definitions = this.publicDefinitions(this.resolveImport(model, entry));
      return definitions.flatMap((definition) => [definition, { ...definition, completionName: `${entry.namespace}@${definition.bareName}` }]);
    });
    return uniqueByKey([...local, ...imported], (definition) => definition.completionName || definition.bareName);
  }

  modulePaths(uri) {
    const current = toFilePath(uri);
    const result = new Set();
    for (const model of this.models()) {
      const file = toFilePath(model.uri);
      if (!file || file === current) continue;
      if (current) result.add(normalPath(path.relative(path.dirname(current), file)));
      if (this.tongue && file.startsWith(path.join(this.tongue, "library"))) {
        result.add(normalPath(path.relative(path.join(this.tongue, "library"), file)));
        result.add(path.basename(file).replace(/^_/, ""));
      }
    }
    return [...result].filter((name) => name && (!name.startsWith("../") || name.endsWith(".tung"))).sort();
  }

  primitiveCompletions() {
    return [
      ...languageNames.primitiveTypes.map((name) => primitive(name, "type")),
      ...languageNames.primitiveConstructors.map((name) => primitive(name, "enumMember")),
      ...languageNames.primitiveEffects.map((name) => primitive(name, "type", "effect")),
      ...languageNames.primitiveFunctions.map((name) => primitive(name, "function")),
      ...languageNames.primitiveShapes.map((name) => primitive(name, "shape"))
    ];
  }
}

function collectFiles(root, result) {
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
}

function primitive(name, role, modifier) {
  return {
    name,
    bareName: name,
    completionName: name,
    role,
    modifiers: ["defaultLibrary", ...(modifier ? [modifier] : [])],
    detail: `built-in ${modifier || role} ${name}`,
    primitive: true
  };
}

function definitionKey(definition) {
  return definition.id || `${definition.uri}:${definition.token?.offset}:${definition.bareName}`;
}

function uniqueByKey(items, keyOf) {
  const seen = new Set();
  return items.filter((item) => {
    const key = keyOf(item);
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function sameRange(left, right) {
  return left.start.line === right.start.line
    && left.start.character === right.start.character
    && left.end.line === right.end.line
    && left.end.character === right.end.character;
}

function normalPath(file) {
  return file ? path.normalize(file).split(path.sep).join("/") : undefined;
}

function toFilePath(uri) {
  try {
    return uri?.startsWith("file:") ? fileURLToPath(uri) : undefined;
  } catch {
    return undefined;
  }
}

module.exports = { WorkspaceIndex, definitionKey, toFilePath, _test: { collectFiles, normalPath } };
