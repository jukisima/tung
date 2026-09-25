import assert from "node:assert/strict";
import test from "node:test";
import {
  findMatching,
  findOpening,
  tokenDepths,
  tokenize,
} from "../server/syntax.ts";

test("token depths recover from an unmatched close", () => {
  const tokens = tokenize("} { (value) }");
  assert.deepEqual(tokenDepths(tokens), [0, 0, 1, 2, 2, 1]);
});

test("bracket lookups agree for nested and unmatched delimiters", () => {
  const tokens = tokenize("(r(value) { unmatched ) } )");
  const opening = tokens.filter(({ text }) => ["(", "r(", "{"].includes(text));
  const closing = tokens.filter(({ text }) => [")", "}"].includes(text));

  assert.deepEqual(
    opening.map(({ index }) => findMatching(tokens, index)),
    [closing.at(-1)?.index, closing[0].index, closing[2].index],
  );
  assert.deepEqual(
    closing.map(({ index }) => findOpening(tokens, index)),
    [opening[1].index, -1, opening[2].index, opening[0].index],
  );
});
