import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";
import { generateBookhoardHtml } from "../docs.ts";
test("generated bookhoard wiki lists shown declarations and doc comments", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-docs-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const bookhoard = path.join(root, "bookhoard");
  fs.mkdirSync(bookhoard);
  fs.writeFileSync(
    path.join(bookhoard, "item.tung"),
    ["## an answer.", "show let answer: integer = 42", "let hidden = 0"].join(
      "\n",
    ),
  );
  const docs = generateBookhoardHtml(root);
  assert.match(docs, /<!doctype html>/);
  assert.match(docs, /<nav aria-label="bookhoard modules">/);
  assert.match(docs, />item\.tung<\/a>/);
  assert.match(docs, />answer<\/a>/);
  assert.match(docs, /an answer\./);
  assert.doesNotMatch(docs, />hidden<\/a>/);
  assert.match(docs, /type="search"/);
  assert.match(docs, /href="\.\.\/\.\.\/bookhoard\/item\.tung"/);
});
test("generated bookhoard wiki escapeth source content", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-docs-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const bookhoard = path.join(root, "bookhoard");
  fs.mkdirSync(bookhoard);
  fs.writeFileSync(
    path.join(bookhoard, "unsafe.tung"),
    "## <script>alert('no')</script>\nshow let answer = 42",
  );
  const docs = generateBookhoardHtml(root);
  assert.doesNotMatch(docs, /<script>alert/);
  assert.match(docs, /&lt;script&gt;alert\(&#39;no&#39;\)&lt;\/script&gt;/);
});
test("generated bookhoard wiki cross-linketh declarations, fills, and modules", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-docs-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const bookhoard = path.join(root, "bookhoard");
  fs.mkdirSync(bookhoard);
  fs.writeFileSync(
    path.join(bookhoard, "declarations.tung"),
    [
      "show kin a box { a box }",
      "show shape a identity { let a identity: a }",
      "show let (value: integer box) unwrap: integer = match value { x box | x }",
    ].join("\n"),
  );
  fs.writeFileSync(
    path.join(bookhoard, "fills.tung"),
    [
      "bring declarations.tung",
      "fill (integer box) identity { let value identity = value }",
    ].join("\n"),
  );
  const docs = generateBookhoardHtml(root);
  assert.match(docs, />fills\.tung<\/a>/);
  assert.match(docs, /kind<\/dt><dd>fill<\/dd>/);
  assert.match(docs, /data-cross-link="module">fills\.tung<\/a>/);
  assert.match(docs, /data-cross-link="owner">box<\/a>/);
  assert.match(docs, /data-cross-link="members">box<\/a>/);
  assert.match(docs, /data-cross-link="references">box<\/a>/);
  assert.match(
    docs,
    /data-cross-link="shape">declarations\.tung@identity<\/a>/,
  );
  assert.match(
    docs,
    /data-cross-link="targets">declarations\.tung@box<\/a>/,
  );
  assert.match(
    docs,
    /data-cross-link="fills">fills\.tung@fill \(integer box\) identity<\/a>/,
  );
  const ids = new Set(
    [...docs.matchAll(/\sid="([^"]+)"/g)].map((match) => match[1]),
  );
  const missingTargets = [...docs.matchAll(/href="#([^"]+)"/g)]
    .map((match) => match[1])
    .filter((id) => !ids.has(id));
  assert.deepEqual(missingTargets, []);
});
