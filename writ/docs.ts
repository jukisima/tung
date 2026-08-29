// buildeþ a self-contained standard-bookhoard wiki from public declarations.
import * as fs from "node:fs";
import * as path from "node:path";
import { smallestRegion } from "../vscode/server/analysis.ts";
import { toFilePath, WorkspaceIndex } from "../vscode/server/workspace.ts";
const generateBookhoardHtml = (
  repository = path.resolve(__dirname, "..", "..", ".."),
) => {
  return renderPage(bookhoardModules(repository));
};
const bookhoardModules = (repository) => {
  const workspace = new WorkspaceIndex({
    get: () => undefined,
    all: () => [],
  });
  const root = path.join(repository, "bookhoard");
  workspace.configure([root]);
  const modules = workspace
    .workspaceFileUris()
    .filter((uri) => toFilePath(uri)?.endsWith(".tung"))
    .map((uri) => workspace.model(uri))
    .filter(Boolean)
    .map((model) => ({
      name: moduleName(root, model),
      model,
      definitions: workspace
        .publicDefinitions(model)
        .filter((definition) =>
          definition.uri === model.uri && !definition.local
        )
        .sort(
          (a, b) =>
            roleOrder(a.role) - roleOrder(b.role) ||
            a.bareName.localeCompare(b.bareName),
        ),
    }))
    .filter(({ definitions, model }) =>
      definitions.length > 0 || model.fills.length > 0
    )
    .sort((a, b) => a.name.localeCompare(b.name));
  return crossLinkModules(workspace, modules);
};
const crossLinkModules = (workspace, modules) => {
  const definitions = new Map();
  for (const module of modules) {
    const definitionEntries = module.definitions.map((definition, index) => {
      const entry = makeEntry({
        id: definitionId(module.name, definition, index),
        kind: "definition",
        name: definition.bareName,
        role: definition.role,
        detail: definition.detail,
        documentation: definition.documentation,
        definition,
        module,
      });
      definitions.set(definition.id, entry);
      return entry;
    });
    const fills = fillEntries(module);
    module.entries = [...definitionEntries, ...fills].sort(
      (a, b) =>
        roleOrder(a.role) - roleOrder(b.role) ||
        a.name.localeCompare(b.name),
    );
  }
  for (const module of modules) {
    for (const entry of module.entries) {
      if (entry.kind === "fill") {
        linkFill(workspace, definitions, entry);
      } else {
        linkDefinition(workspace, definitions, entry);
      }
    }
  }
  return modules;
};
const makeEntry = (entry) => ({
  ...entry,
  relations: {
    owner: [],
    members: [],
    frame: [],
    targets: [],
    fills: [],
    references: [],
    referencedBy: [],
  },
});
const fillEntries = (module) => {
  return module.model.fills.map((fill, index) => {
    const region = smallestRegion(
      module.model.regions,
      fill.token.offset,
      "fill",
    );
    const detail = declarationHeader(module.model, region) ||
      `fill ${fill.shapeName}`;
    return makeEntry({
      id: fillId(module.name, detail, index),
      kind: "fill",
      name: detail,
      role: "fill",
      detail,
      documentation: region?.doc,
      fill,
      region,
      module,
    });
  });
};
const linkDefinition = (workspace, definitions, entry) => {
  const { definition, module } = entry;
  if (definition.containerName) {
    const owner = module.entries.find(
      (candidate) =>
        candidate.kind === "definition" &&
        candidate.name === definition.containerName &&
        ["type", "frame"].includes(candidate.role),
    );
    if (owner) {
      addRelation(entry, "owner", owner);
      addRelation(owner, "members", entry);
    }
  }
  if (!definition.topLevel) return;
  for (const token of headerTokens(module.model, definition.token.offset)) {
    if (token.kind !== "name") continue;
    const resolved = resolveHeaderReference(workspace, module.model, token);
    const target = resolved && definitions.get(resolved.id);
    if (!target || target === entry) continue;
    addRelation(entry, "references", target);
    addRelation(target, "referencedBy", entry);
  }
};
const linkFill = (workspace, definitions, entry) => {
  const { fill, module, region } = entry;
  const frame = workspace.resolveAt(module.model.uri, fill.range.start)
    .definition;
  const shapeEntry = frame && definitions.get(frame.id);
  if (shapeEntry) {
    addRelation(entry, "frame", shapeEntry);
    addRelation(shapeEntry, "fills", entry);
  }
  if (!region) return;
  for (
    const token of module.model.tokens.slice(
      region.startIndex + 1,
      fill.token.index,
    )
  ) {
    if (token.kind !== "name") continue;
    const resolved = workspace.resolveVisibleRole(
      module.model,
      token.text,
      "type",
    );
    const target = resolved.length === 1 && definitions.get(resolved[0].id);
    if (!target || target.role !== "type") continue;
    addRelation(entry, "targets", target);
    addRelation(target, "fills", entry);
  }
};
const resolveHeaderReference = (workspace, model, token) => {
  const analyzedRole = model.roles.get(token.index)?.type;
  const role = analyzedRole === "typeParameter" ? "type" : analyzedRole;
  if (role) {
    const matching = workspace.resolveVisibleRole(model, token.text, role);
    if (matching.length === 1) return matching[0];
  }
  return workspace.resolveAt(model.uri, {
    line: token.line,
    character: token.char,
  }).definition;
};
const addRelation = (entry, relation, target) => {
  if (!entry.relations[relation].some(({ id }) => id === target.id)) {
    entry.relations[relation].push(target);
  }
};
const headerTokens = (model, offset) => {
  const region = smallestRegion(model.regions, offset);
  if (!region) return [];
  const start = declarationPrefixStart(model, region);
  return model.tokens.slice(start, region.headerEnd + 1);
};
const declarationPrefixStart = (model, region) => {
  let start = region.startIndex;
  while (start > 0) {
    const previous = model.tokens[start - 1];
    const depth = model.depths[previous.index];
    if (depth < region.depth) break;
    if (previous.text === "}") break;
    start -= 1;
  }
  return start;
};
const declarationHeader = (model, region) => {
  if (!region) return "";
  const end = model.tokens[region.headerEnd]?.offset ?? region.endOffset;
  return oneLine(model.text.slice(region.startOffset, end));
};
const renderPage = (modules) => {
  const count = modules.reduce(
    (total, module) => total + module.entries.length,
    0,
  );
  const navigation = modules
    .map(
      (module) =>
        `<a href="#${moduleId(module.name)}" data-module-link="${
          html(module.name.toLowerCase())
        }">${html(module.name)}</a>`,
    )
    .join("\n");
  const content = modules.map(renderModule).join("\n");
  return `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>tung standard bookhoard</title>
  <style>${styles}</style>
</head>
<body>
  <aside>
    <header><a href="#top">tung bookhoard</a></header>
    <label for="search">search ${count} shown names and fills</label>
    <input id="search" type="search" autocomplete="off" placeholder="name, kind, or wording">
    <output id="result-count" aria-live="polite"></output>
    <nav aria-label="bookhoard modules">${navigation}</nav>
  </aside>
  <main id="top">
    <header class="page-head">
      <p class="eyebrow">standard bookhoard</p>
      <h1>tung names and types</h1>
      <p>generated from shown declarations, fills, their relationships, and attached doc comments.</p>
    </header>
    ${content}
  </main>
  <script>${searchScript}</script>
</body>
</html>\n`;
};
const renderModule = (module) => {
  const search = [
    module.name,
    ...module.entries.flatMap(entrySearch),
  ]
    .join(" ")
    .toLowerCase();
  return `<section class="module" id="${moduleId(module.name)}" data-module="${
    html(module.name.toLowerCase())
  }" data-search="${html(search)}">
  <header class="module-head">
    <h2>${html(module.name)}</h2>
    <a href="${sourceHref(module.name)}">source</a>
  </header>
  ${module.entries.map(renderEntry).join("\n  ")}
</section>`;
};
const renderEntry = (entry) => {
  const detail = entry.detail
    ? `<pre><code>${html(oneLine(entry.detail))}</code></pre>`
    : "";
  return `<article id="${entry.id}" data-definition data-search="${
    html(entrySearch(entry).join(" ").toLowerCase())
  }">
    <h3><a href="#${entry.id}">${html(entry.name)}</a></h3>
    <dl>
      <div><dt>kind</dt><dd>${html(entry.role)}</dd></div>
      <div><dt>module</dt><dd>${
    entryLink(entry.module, entry.module.name, "module")
  }</dd></div>
      ${renderRelations(entry)}
    </dl>
    ${detail}
    ${renderDocumentation(entry.documentation)}
  </article>`;
};
const renderRelations = (entry) => {
  return [
    ["owner", "owner"],
    ["members", "members"],
    ["frame", "frame"],
    ["targets", "targets"],
    ["fills", "fills"],
    ["references", "references"],
    ["referencedBy", "referenced by"],
  ].map(([relation, label]) => {
    const targets = entry.relations[relation];
    if (!targets.length) return "";
    return `<div><dt>${label}</dt><dd class="cross-links">${
      targets.map((target) =>
        entryLink(target, entryLabel(entry, target), relation)
      ).join(", ")
    }</dd></div>`;
  }).join("");
};
const entryLink = (target, label, relation) => {
  const id = target.id || moduleId(target.name);
  return `<a href="#${id}" data-cross-link="${html(relation)}">${
    html(label)
  }</a>`;
};
const entryLabel = (source, target) => {
  return source.module.name === target.module.name
    ? target.name
    : `${target.module.name}@${target.name}`;
};
const renderDocumentation = (documentation) => {
  if (!documentation) return "";
  return documentation
    .trim()
    .split(/\n\s*\n/)
    .map((paragraph) => `<p>${html(paragraph).replaceAll("\n", "<br>")}</p>`)
    .join("\n    ");
};
const entrySearch = (entry) => {
  return [
    entry.name,
    entry.role,
    entry.detail,
    entry.documentation,
    ...(Object.values(entry.relations) as any[][]).flatMap((targets) =>
      targets.map(({ name, module }) => `${module.name} ${name}`)
    ),
  ].filter(Boolean);
};
const roleOrder = (role) => {
  return {
    type: 0,
    frame: 1,
    fill: 2,
    enumMember: 3,
    method: 4,
    function: 5,
    variable: 6,
  }[role] ?? 9;
};
const moduleName = (bookhoard, model) => {
  return path.relative(bookhoard, toFilePath(model.uri)).replaceAll(
    path.sep,
    "/",
  );
};
const moduleId = (name) => {
  return `module-${Buffer.from(name).toString("base64url")}`;
};
const definitionId = (moduleName, definition, index) => {
  return `name-${
    Buffer.from(
      `${moduleName}:${definition.role}:${definition.bareName}:${index}`,
    ).toString("base64url")
  }`;
};
const fillId = (moduleName, detail, index) => {
  return `fill-${
    Buffer.from(`${moduleName}:${detail}:${index}`).toString("base64url")
  }`;
};
const sourceHref = (name) => {
  return `../../bookhoard/${name.split("/").map(encodeURIComponent).join("/")}`;
};
const oneLine = (text) => {
  return text.replace(/\s+/g, " ").trim();
};
const html = (value) => {
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#39;");
};
const searchScript = `
const input = document.querySelector("#search");
const output = document.querySelector("#result-count");
const modules = [...document.querySelectorAll(".module")];
const links = new Map([...document.querySelectorAll("[data-module-link]")].map(link => [link.getAttribute("href").slice(1), link]));
const filter = () => {
  const query = input.value.trim().toLocaleLowerCase();
  let visible = 0;
  for (const module of modules) {
    const moduleMatch = module.dataset.module.includes(query);
    const definitions = [...module.querySelectorAll("[data-definition]")];
    for (const definition of definitions) {
      definition.hidden = Boolean(query) && !moduleMatch && !definition.dataset.search.includes(query);
      if (!definition.hidden) visible += 1;
    }
    module.hidden = definitions.every(definition => definition.hidden);
    links.get(module.id).hidden = module.hidden;
  }
  output.value = query ? visible + " matching names" : "";
};
input.addEventListener("input", filter);
`;
const styles = `
:root { color-scheme: light dark; font-family: ui-sans-serif, system-ui, sans-serif; line-height: 1.55; --line: color-mix(in srgb, currentColor 18%, transparent); --muted: color-mix(in srgb, currentColor 68%, transparent); --accent: #087f8c; }
* { box-sizing: border-box; }
html { scroll-behavior: smooth; }
body { margin: 0; color: CanvasText; background: Canvas; }
a { color: inherit; text-decoration-color: var(--muted); text-underline-offset: .2em; }
aside { position: fixed; inset: 0 auto 0 0; width: 19rem; overflow: auto; padding: 1.5rem; border-right: 1px solid var(--line); background: Canvas; }
aside header { margin-bottom: 1.5rem; font-size: 1.15rem; font-weight: 700; }
label { display: block; margin-bottom: .35rem; color: var(--muted); font-size: .8rem; }
input { width: 100%; padding: .65rem .75rem; border: 1px solid var(--line); border-radius: 4px; color: inherit; background: Canvas; font: inherit; }
output { display: block; min-height: 1.8rem; padding-top: .35rem; color: var(--muted); font-size: .8rem; }
nav { display: grid; gap: .2rem; }
nav a { overflow-wrap: anywhere; padding: .25rem 0; color: var(--muted); text-decoration: none; }
nav a:hover { color: CanvasText; }
main { width: min(58rem, calc(100% - 19rem)); margin-left: 19rem; padding: 3rem clamp(1.5rem, 5vw, 5rem) 6rem; }
.page-head { padding-bottom: 2.5rem; border-bottom: 1px solid var(--line); }
.eyebrow { margin: 0; color: var(--accent); font-size: .8rem; font-weight: 700; text-transform: uppercase; }
h1 { margin: .25rem 0; font-size: 3rem; line-height: 1.1; }
h2, h3 { scroll-margin-top: 1.5rem; }
.module { padding-top: 3rem; }
.module-head { display: flex; align-items: baseline; justify-content: space-between; gap: 1rem; border-bottom: 2px solid var(--line); }
.module-head h2 { margin: 0 0 .6rem; overflow-wrap: anywhere; font-size: 1.35rem; }
.module-head a { color: var(--muted); font-size: .8rem; }
article { padding: 1.4rem 0; border-bottom: 1px solid var(--line); }
article h3 { margin: 0 0 .6rem; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 1.05rem; }
dl { display: flex; flex-wrap: wrap; gap: .4rem 1.2rem; margin: 0 0 .8rem; color: var(--muted); font-size: .8rem; }
dl div { display: flex; gap: .35rem; }
dt { font-weight: 700; }
dd { margin: 0; }
.cross-links a { white-space: nowrap; }
pre { overflow-x: auto; margin: .7rem 0; padding: .8rem; border-left: 3px solid var(--accent); background: color-mix(in srgb, CanvasText 5%, Canvas); }
code { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
article p { margin: .7rem 0 0; max-width: 44rem; }
[hidden] { display: none !important; }
@media (max-width: 760px) { aside { position: static; width: auto; max-height: 45vh; border-right: 0; border-bottom: 1px solid var(--line); } main { width: auto; margin: 0; padding-top: 2rem; } h1 { font-size: 2.25rem; } }
`;
if (require.main === module) {
  const repository = path.resolve(
    process.argv[2] || path.join(__dirname, "..", "..", ".."),
  );
  const output = process.argv[3] && path.resolve(process.argv[3]);
  const docs = generateBookhoardHtml(repository);
  if (output) {
    fs.mkdirSync(path.dirname(output), { recursive: true });
    fs.writeFileSync(output, docs);
  } else {
    process.stdout.write(docs);
  }
}
export { generateBookhoardHtml };
