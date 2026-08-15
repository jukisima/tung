import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import test from "node:test";
const root = path.resolve(__dirname, "..", "..", "..");
const ignored = new Set([".git", "dist-newstyle", "node_modules"]);
const sourceExtensions = new Set([".hs", ".sh", ".ts", ".tung"]);
const sentenceStart = /(?:^|[.!?]\s+)([A-Z][a-z]+)\b/;
const americanSpelling =
  /\b(?:behavior|behaviors|behavioral|color|colors|colored|coloring|favor|favored|favorite|generalization|generalized|gray|honor|honored|initialize|initialized|initializing|license|modeled|modeling|neighbor|neighbors|normalizes|optimization|optimize|optimized|organize|organized|parameterized|parenthesized|recognize|recognized|recognizes|tokenization|tokenized|unparenthesized)\b/i;
test("prose and source comments use lowercase sentence starts", () => {
  const failures = proseFiles(root).flatMap((file) =>
    proseFragments(file).flatMap((fragment) => {
      const match = sentenceStart.exec(fragment.text);
      return match ? [`${path.relative(root, file)}:${fragment.line}: ${match[1]}`] : [];
    }),
  );
  assert.deepEqual(failures, []);
});
test("prose and source comments use british spelling", () => {
  const failures = proseFiles(root).flatMap((file) =>
    proseFragments(file).flatMap((fragment) => {
      const match = americanSpelling.exec(fragment.text);
      return match ? [`${path.relative(root, file)}:${fragment.line}: ${match[0]}`] : [];
    }),
  );
  assert.deepEqual(failures, []);
});
const proseFiles = (directory) => {
  return fs.readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const generatedWiki = directory === path.join(root, "writ") && entry.name === "bookhoard";
    if (ignored.has(entry.name) || entry.name === "package-lock.json" || generatedWiki) return [];
    const file = path.join(directory, entry.name);
    if (entry.isDirectory()) return proseFiles(file);
    return entry.name.endsWith(".md") || sourceExtensions.has(path.extname(entry.name))
      ? [file]
      : [];
  });
};
const proseFragments = (file) => {
  const text = fs.readFileSync(file, "utf8");
  if (file.endsWith(".md")) return markdownFragments(text);
  const lineMarker = file.endsWith(".hs")
    ? /^\s*--+\s*\|?\s?(.*)$/
    : file.endsWith(".ts")
      ? /^\s*\/\/\s?(.*)$/
      : /^\s*#+\s?(.*)$/;
  const lines = text.split(/\r?\n/).flatMap((line, index) => {
    const match = lineMarker.exec(line);
    return match ? [{ line: index + 1, text: match[1] }] : [];
  });
  const blockPattern = file.endsWith(".hs") ? /\{-(?!#)([\s\S]*?)-\}/g : /\/\*([\s\S]*?)\*\//g;
  const blocks = [...text.matchAll(blockPattern)].map((match) => ({
    line: text.slice(0, match.index).split(/\r?\n/).length,
    text: match[1].replace(/^\s*[|*]\s?/gm, "").trim(),
  }));
  return [...lines, ...blocks];
};
const markdownFragments = (text) => {
  let fenced = false;
  return text.split(/\r?\n/).flatMap((line, index) => {
    if (/^\s*```/.test(line)) {
      fenced = !fenced;
      return [];
    }
    if (fenced || !line.trim()) return [];
    return [
      {
        line: index + 1,
        text: line.replace(/^\s*(?:#{1,6}|[-*+]|\d+\.|>)\s*/, ""),
      },
    ];
  });
};
