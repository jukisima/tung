import assert from "node:assert/strict";
import * as path from "node:path";
import test from "node:test";
import { CompilerBridge, SourceRole } from "../server/checker.ts";
import { HighlightCache, buildHighlightRanges } from "../server/highlight.ts";
import { WorkspaceIndex } from "../server/workspace.ts";

const compiler = new CompilerBridge();
compiler.configure(path.resolve(__dirname, "../../../tongue"), [], process.env.TUNG_EXECUTABLE);
test.after(() => compiler.dispose());
const labels = async (source: string, name: string) => {
  const uri = "file:///highlight-test.tung";
  const document = { uri, getText: () => source };
  const workspace = new WorkspaceIndex({ get: () => document, all: () => [document] });
  const model = workspace.model(uri);
  const roles = await compiler.highlight(source).result;
  assert(roles, "the compiler must support the highlight request");
  const ranges = buildHighlightRanges(source, roles, token => workspace.resolveAt(uri, { line: token.line, character: token.char }).definition);
  return model.tokens.filter(token => token.text === name).map(token => {
    const range = ranges.find(range => range.line === token.line && range.char === token.char);
    return range && `${range.type}:${range.modifiers.join(".")}`;
  });
};
test("compiler roles keep types, header binders and application positions consistent", async () => {
  const source = "show let constant [@a:ilk, @b:ilk, x:a, _:b, a] x\n" +
    "show let read-paþ [text paþ:paþ, text; file, text fail] text paþ\n" +
    "show let join-paþ [text₀ paþ:paþ, text₁ paþ:paþ, paþ] text₀ * '/' $ * text₁ $ paþ";
  assert.deepEqual(await labels(source, "a"), ["type:declaration", "type:", "type:"]);
  assert.deepEqual(await labels(source, "x"), [undefined, undefined]);
  assert.deepEqual(await labels(source, "_"), [undefined]);
  assert.deepEqual(await labels(source, "text"), [undefined, "type:", "type:effect", undefined]);
  assert.deepEqual(await labels(source, "text₀"), [undefined, undefined]);
  assert.deepEqual(await labels(source, "*"), ["call:applied", "call:applied"]);
  assert.deepEqual(await labels(source, "@"), [undefined, undefined]);
});
test("compiler roles use GADT, flock and pattern grammar", async () => {
  const source = "show ilk tag [a:ilk] { integer [ℤ tag], boolean [𝟚 tag] }\n" +
    "show flock functor [f:[ilk, ilk]] ( let map [a f, [a, b; e], b f; e] law [af:a f] af map sameness = af )\n" +
    "graiþ f functor show let void [af:a f, 𝟙 f] af map {_ ^ only}\n" +
    "let read [ℤ tag, ℤ] { x integer ^ x }";
  assert.deepEqual(await labels(source, "tag"), ["type:declaration.typeFunction", "type:applied", "type:applied", "type:applied"]);
  assert.deepEqual(await labels(source, "functor"), ["class:declaration.typeFunction", "class:applied"]);
  assert.deepEqual(await labels(source, "integer"), ["enumMember:declaration", "enumMember:"]);
  assert.deepEqual(await labels(source, "_"), [undefined]);
  assert.deepEqual(await labels(source, "map"), ["call:declaration.applied", "call:applied", "call:applied"]);
});
test("compiler highlighting recovereþ incomplete syntax without accepting it", async () => {
  const source = "let before [ℤ] 1\nlet broken [x:ℤ\nlet after [text] 'ok'";
  assert.deepEqual(await labels(source, "ℤ"), ["type:", undefined]);
  assert.deepEqual(await labels(source, "text"), ["type:"]);
  assert.deepEqual(await labels("let before [ℤ] 1\nlet broken ≔ 'unfinished", "ℤ"), ["type:"]);
  assert.match(await compiler.check({ text: source }, []).result, /^tung-diagnostic\tparse/);
});
test("compiler spans map unicode and line endings without eager name resolution", () => {
  const resolve = () => { throw new Error("navigation analysis was unnecessary"); };
  assert.deepEqual(buildHighlightRanges("𝟙\r\nℤ", [[0, 2, "type", []], [4, 5, "type", []]], resolve), [
    { line: 0, char: 0, length: 2, type: "type", modifiers: [] },
    { line: 1, char: 0, length: 1, type: "type", modifiers: [] },
  ]);
  assert.deepEqual(buildHighlightRanges("f x", [[0, 1, "call", ["applied"]], [2, 3, "term", []]], resolve), [
    { line: 0, char: 0, length: 1, type: "call", modifiers: ["applied"] },
  ]);
});
test("highlight cache shareþ requests and discardeth obsolete results", async () => {
  const requests = [];
  const cache = new HighlightCache({ highlight: (text, version) => {
    let resolve;
    const result = new Promise<SourceRole[]>(resolveResult => resolve = resolveResult);
    const request = { text, version, result, resolve, cancelled: false, cancel() { this.cancelled = true; } };
    requests.push(request);
    return request;
  } });
  const first = cache.get("uri", "old", 1);
  const same = cache.get("uri", "old", 1);
  assert.equal(requests.length, 1);
  const latest = cache.get("uri", "new", 2);
  assert.equal(requests[0].cancelled, true);
  requests[0].resolve([[0, 3, "type", []]]);
  assert.deepEqual(await first, []);
  assert.deepEqual(await same, []);
  requests[1].resolve([[0, 3, "call", ["applied"]]]);
  assert.deepEqual(await latest, [[0, 3, "call", ["applied"]]]);
  assert.deepEqual(await cache.get("uri", "new", 2), await latest);
  assert.equal(requests.length, 2);
  cache.clear();
  assert.equal(requests[1].cancelled, true);
});
