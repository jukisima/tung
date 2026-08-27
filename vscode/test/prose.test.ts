import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import test from "node:test";
const root = path.resolve(__dirname, "..", "..", "..");
const ignored = new Set([".git", "dist-newstyle", "node_modules"]);
const documentationExtensions = new Set([".adoc"]);
const sourceExtensions = new Set([".hs", ".sh", ".ts", ".tung"]);
const sentenceStart = /(?:^|[.!?]\s+)([A-Z][a-z]+)\b/;
const americanSpelling =
  /\b(?:behavior|behaviors|behavioral|color|colors|colored|coloring|favor|favored|favorite|generalization|generalized|gray|honor|honored|initialize|initialized|initializing|license|modeled|modeling|neighbor|neighbors|normalizes|optimization|optimize|optimized|organize|organized|parameterized|parenthesized|recognize|recognized|recognizes|tokenization|tokenized|unparenthesized)\b/i;
const modernThirdPerson =
  /\b(?:has|does|needs|requires|uses|provides|resolves|supports|returns|fails|contains|shows|adds|owns|belongs|chooses|takes|keeps|follows|sends|asks|falls|remains|refreshes|defines|exposes|joins|consumes|continues|receives|supplies|derives|lifts|performs|schedules|discards|builds|installs|finishes|generates|reuses|understands|treats|separates|recognises|gives|leaves|selects|normalises|finds|rejects|accepts|carries|satisfies|negates|yields|delegates|dispatches|replaces|drops|ignores|compares|excludes|includes|becomes|prefers|collapses|drives|converts|computes|repeats|preserves|discovers|embeds|stays|attaches|declares|allows|enforces|covers|produces|collects|removes|inserts|looks|binds|combines|closes|enters|hides|constructs|resumes|restricts|deduplicates|picks|waits|cancels|throws|reports|formats|publishes|acts|sees|witnesses|solves|emits|wins|reads|handles|means|evaluates|forbids|verifies|permits|conflicts|qualifies|applies|tracks|rebuilds|implements|discharges|exists|represents|specialises|absorbs|contributes|decorates|establishes|distinguishes|survives|erases|proves|writes|infers|prevents|depends)\b/i;
const secondPersonObjectAsSubject =
  /(?:^|[.!?]\s+|\b(?:and|but|if|when|while|because|unless|or)\s+)(?:thee|you)\b/i;
const secondPersonSubjectAsPrepositionalObject =
  /\b(?:to|for|with|from|at|by|before|after|against|among|around|beside|between|near|through|toward|without)\s+(?:thou|ye)\b/i;
test("prose and source comments use lowercase sentence starts", () => {
  const failures = proseFiles(root).flatMap((file) =>
    proseFragments(file).flatMap((fragment) => {
      const match = sentenceStart.exec(fragment.text);
      return match
        ? [`${path.relative(root, file)}:${fragment.line}: ${match[1]}`]
        : [];
    })
  );
  assert.deepEqual(failures, []);
});
test("prose and source comments use british spelling", () => {
  const failures = proseFiles(root).flatMap((file) =>
    proseFragments(file).flatMap((fragment) => {
      const match = americanSpelling.exec(fragment.text);
      return match
        ? [`${path.relative(root, file)}:${fragment.line}: ${match[0]}`]
        : [];
    })
  );
  assert.deepEqual(failures, []);
});
test("prose and source comments use third-person -eþ forms", () => {
  const failures = proseFiles(root).flatMap((file) =>
    proseFragments(file).flatMap((fragment) => {
      const prose = fragment.text.replace(/`[^`]*`/g, "");
      const match = modernThirdPerson.exec(prose);
      return match
        ? [`${path.relative(root, file)}:${fragment.line}: ${match[0]}`]
        : [];
    })
  );
  assert.deepEqual(failures, []);
});
test("prose distinguisheþ second-person number and case", () => {
  const failures = proseFiles(root).flatMap((file) =>
    proseFragments(file).flatMap((fragment) => {
      const prose = fragment.text.replace(/`[^`]*`/g, "");
      const subject = secondPersonObjectAsSubject.exec(prose);
      const object = secondPersonSubjectAsPrepositionalObject.exec(prose);
      const match = subject ?? object;
      return match
        ? [`${path.relative(root, file)}:${fragment.line}: ${match[0].trim()}`]
        : [];
    })
  );
  assert.deepEqual(failures, []);
});
test("project documentation useþ asciidoc", () => {
  assert.deepEqual(
    repositoryFiles(root).filter((file) => file.endsWith(".md")),
    [],
  );
});
const proseFiles = (directory) => {
  return repositoryFiles(directory).filter((file) =>
    documentationExtensions.has(path.extname(file)) ||
    sourceExtensions.has(path.extname(file))
  );
};
const repositoryFiles = (directory) => {
  return fs.readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const generatedWiki = directory === path.join(root, "writ") &&
      entry.name === "bookhoard";
    if (
      ignored.has(entry.name) || entry.name === "package-lock.json" ||
      generatedWiki
    ) return [];
    const file = path.join(directory, entry.name);
    return entry.isDirectory() ? repositoryFiles(file) : [file];
  });
};
const proseFragments = (file) => {
  const text = fs.readFileSync(file, "utf8");
  if (file.endsWith(".adoc")) return asciidocFragments(text);
  const lineMarker = file.endsWith(".hs")
    ? /^\s*--+\s*\|?\s?(.*)$/
    : file.endsWith(".ts")
    ? /^\s*\/\/\s?(.*)$/
    : /^\s*#+\s?(.*)$/;
  const lines = text.split(/\r?\n/).flatMap((line, index) => {
    const match = lineMarker.exec(line);
    return match ? [{ line: index + 1, text: match[1] }] : [];
  });
  const blockPattern = file.endsWith(".hs")
    ? /\{-(?!#)([\s\S]*?)-\}/g
    : /\/\*([\s\S]*?)\*\//g;
  const blocks = [...text.matchAll(blockPattern)].map((match) => ({
    line: text.slice(0, match.index).split(/\r?\n/).length,
    text: match[1].replace(/^\s*[|*]\s?/gm, "").trim(),
  }));
  return [...lines, ...blocks];
};
const asciidocFragments = (text) => {
  let delimiter;
  return text.split(/\r?\n/).flatMap((line, index) => {
    const trimmed = line.trim();
    if (delimiter) {
      if (trimmed === delimiter) delimiter = undefined;
      return [];
    }
    if (trimmed === "|===") return [];
    if (["----", "...."].includes(trimmed)) {
      delimiter = trimmed;
      return [];
    }
    if (!trimmed || /^\[[^\]]*\]$/.test(trimmed)) return [];
    return [
      {
        line: index + 1,
        text: line.replace(/^\s*(?:={1,6}|\*+|\d+\.|>|\|)\s*/, ""),
      },
    ];
  });
};
