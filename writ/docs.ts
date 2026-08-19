// buildeth a self-contained standard-bookhoard wiki from public declarations.
import * as fs from "node:fs";
import * as path from "node:path";
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
  return workspace
    .workspaceFileUris()
    .filter((uri) => toFilePath(uri)?.endsWith(".tung"))
    .map((uri) => workspace.model(uri))
    .filter(Boolean)
    .map((model) => ({
      name: moduleName(root, model),
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
    .filter(({ definitions }) => definitions.length > 0)
    .sort((a, b) => a.name.localeCompare(b.name));
};
const renderPage = (modules) => {
  const count = modules.reduce(
    (total, module) => total + module.definitions.length,
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
    <label for="search">search ${count} shown names</label>
    <input id="search" type="search" autocomplete="off" placeholder="name, kind, or wording">
    <output id="result-count" aria-live="polite"></output>
    <nav aria-label="bookhoard modules">${navigation}</nav>
  </aside>
  <main id="top">
    <header class="page-head">
      <p class="eyebrow">standard bookhoard</p>
      <h1>tung names and types</h1>
      <p>generated from shown declarations and their attached doc comments.</p>
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
    ...module.definitions.flatMap(definitionSearch),
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
  ${
    module.definitions.map((definition, index) =>
      renderDefinition(module.name, definition, index)
    ).join("\n  ")
  }
</section>`;
};
const renderDefinition = (moduleName, definition, index) => {
  const id = definitionId(moduleName, definition, index);
  const owner = definition.containerName
    ? `<div><dt>owner</dt><dd>${html(definition.containerName)}</dd></div>`
    : "";
  const detail = definition.detail
    ? `<pre><code>${html(oneLine(definition.detail))}</code></pre>`
    : "";
  return `<article id="${id}" data-definition data-search="${
    html(definitionSearch(definition).join(" ").toLowerCase())
  }">
    <h3><a href="#${id}">${html(definition.bareName)}</a></h3>
    <dl><div><dt>kind</dt><dd>${html(definition.role)}</dd></div>${owner}</dl>
    ${detail}
    ${renderDocumentation(definition.documentation)}
  </article>`;
};
const renderDocumentation = (documentation) => {
  if (!documentation) return "";
  return documentation
    .trim()
    .split(/\n\s*\n/)
    .map((paragraph) => `<p>${html(paragraph).replaceAll("\n", "<br>")}</p>`)
    .join("\n    ");
};
const definitionSearch = (definition) => {
  return [
    definition.bareName,
    definition.role,
    definition.containerName,
    definition.detail,
    definition.documentation,
  ].filter(Boolean);
};
const roleOrder = (role) => {
  return {
    type: 0,
    shape: 1,
    enumMember: 2,
    method: 3,
    function: 4,
    variable: 5,
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
