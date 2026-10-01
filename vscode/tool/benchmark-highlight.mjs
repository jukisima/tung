// run after building the compiler and editor. timings include protocol transport,
// document analysis, name refinement, and token mapping, without type checking.
import { readFileSync } from "node:fs";
import { performance } from "node:perf_hooks";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const { CompilerBridge } = require("../out/server/checker.js");
const { HighlightCache, buildHighlightRanges } = require("../out/server/highlight.js");
const { WorkspaceIndex } = require("../out/server/workspace.js");
const root = fileURLToPath(new URL("../../", import.meta.url));
const compiler = new CompilerBridge();
compiler.configure(`${root}/tongue`, [], process.env.TUNG_EXECUTABLE);
const cache = new HighlightCache(compiler);
const samples = [
  ["path", readFileSync(`${root}/bookhoard/ilk/path.tung`, "utf8")],
  ["mockingbird", readFileSync(`${root}/bookhoard/mockingbird.tung`, "utf8")],
  ...[1000, 5000].flatMap(size => [false, true].map(broken => [
    `${size} ${broken ? "incomplete" : "complete"}`,
    Array.from({ length: size }, (_,i) => `let value${i} [ℤ${broken ? "" : "] 1"}`).join("\n"),
  ])),
  ["1000 functions", Array.from({ length: 1000 }, (_,i) => `let f${i} [x:ℤ, ℤ] x`).join("\n")],
];
const median = values => values.sort((a,b) => a-b)[Math.floor(values.length / 2)].toFixed(1);
try {
  const started = performance.now();
  if (!await compiler.highlight("").result) throw new Error("build a compiler with highlighting support");
  console.log(`compiler discovery and startup: ${(performance.now()-started).toFixed(1)} ms`);
  for (const [name, source] of samples) {
    const uri = `file://${root}/bookhoard/ilk/benchmark.tung`;
    const document = { uri, getText: () => source };
    const workspace = new WorkspaceIndex({ get: key => key === uri ? document : undefined, all: () => [document] });
    workspace.configure([root], [`${root}/bookhoard`]);
    const elapsed = [], cached = [];
    let count = 0;
    for (let version = 1; version <= 3; version++) {
      workspace.invalidate(uri);
      const start = performance.now();
      const roles = await cache.get(uri, source, version);
      const resolve = token => workspace.resolveAt(uri, { line: token.line, character: token.char }).definition;
      count = buildHighlightRanges(source, roles, resolve).length;
      elapsed.push(performance.now() - start);
      const cacheStart = performance.now();
      buildHighlightRanges(source, await cache.get(uri, source, version), resolve);
      cached.push(performance.now() - cacheStart);
    }
    console.log(`${name}: ${source.length} UTF-16 units, ${count} roles; fresh ${median(elapsed)} ms, cached ${median(cached)} ms`);
  }
} finally {
  cache.clear();
  compiler.dispose();
}
