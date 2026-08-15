import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";
import { generateBookhoardHtml } from "../server/docs";
test("generated bookhoard wiki lists shown declarations and doc comments", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-docs-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const bookhoard = path.join(root, "bookhoard");
  fs.mkdirSync(bookhoard);
  fs.writeFileSync(path.join(root, "tung.cabal"), "");
  fs.writeFileSync(
    path.join(bookhoard, "item.tung"),
    ["## an answer.", "show let answer: integer = 42;", "let hidden = 0;"].join("\n"),
  );
  const docs = generateBookhoardHtml(root);
  assert.match(docs, /<!doctype html>/);
  assert.match(docs, /<nav aria-label="bookhoard modules">/);
  assert.match(docs, />item\.tung<\/a>/);
  assert.match(docs, />answer<\/a>/);
  assert.match(docs, /an answer\./);
  assert.doesNotMatch(docs, />hidden<\/a>/);
  assert.match(docs, /type="search"/);
  assert.match(docs, /href="\.\.\/\.\.\/tongue\/bookhoard\/item\.tung"/);
});
test("generated bookhoard wiki escapes source content", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-docs-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const bookhoard = path.join(root, "bookhoard");
  fs.mkdirSync(bookhoard);
  fs.writeFileSync(path.join(root, "tung.cabal"), "");
  fs.writeFileSync(
    path.join(bookhoard, "unsafe.tung"),
    "## <script>alert('no')</script>\nshow let answer = 42;",
  );
  const docs = generateBookhoardHtml(root);
  assert.doesNotMatch(docs, /<script>alert/);
  assert.match(docs, /&lt;script&gt;alert\(&#39;no&#39;\)&lt;\/script&gt;/);
});
