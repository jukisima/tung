// shared tolerant source structure for colouring and document analysis.
import generatedLanguageNames from "../generated/language-names.json";

const keywords = new Set(generatedLanguageNames.keywords);
const primitiveTypes = new Set(generatedLanguageNames.primitiveTypes);
const specialNameChars = new Set([...generatedLanguageNames.specialNameChars]);
const declarationKeywords = new Set([
  "let",
  "let-ilk",
  "kin",
  "deed",
  "shape",
  "fill",
]);
const fileDeclarationKeywords = new Set([
  "bring",
  "graith",
  "yield",
  "show",
  "show-ilk",
  ...declarationKeywords,
]);
const closeForOpen = new Map([
  ["(", ")"],
  ["r(", ")"],
  ["<(", ")"],
  [">(", ")"],
  ["{", "}"],
]);
const closingDelimiters = new Set(closeForOpen.values());
const specialOpenPrefixes = new Set(["r", "<", ">"]);

const tokenize = (text) => {
  const tokens = [];
  let offset = 0;
  let line = 0;
  let char = 0;
  const current = () => text[offset];
  const next = () => text[offset + 1];
  const atEnd = () => offset >= text.length;
  const position = () => [offset, line, char];
  const advance = () => {
    const ch = text[offset];
    if (ch === "\r" && next() === "\n") {
      offset += 2;
      line += 1;
      char = 0;
    } else if (ch === "\n" || ch === "\r") {
      offset += 1;
      line += 1;
      char = 0;
    } else {
      offset += 1;
      char += 1;
    }
  };
  const advanceCodePoint = () => {
    const width = (text.codePointAt(offset) || 0) > 0xffff ? 2 : 1;
    for (let index = 0; index < width && !atEnd(); index += 1) advance();
  };
  const push = (kind, startOffset, startLine, startChar) => {
    tokens.push({
      index: tokens.length,
      kind,
      text: text.slice(startOffset, offset),
      offset: startOffset,
      line: startLine,
      char: startChar,
      endOffset: offset,
      endLine: line,
      endChar: char,
    });
  };
  const consumeName = () => {
    const [startOffset, startLine, startChar] = position();
    if (current() === "." && next() === "*") {
      advance();
      advance();
    }
    while (!atEnd() && isNameChar(current())) advance();
    const value = text.slice(startOffset, offset);
    push(
      keywords.has(value) ? "keyword" : "name",
      startOffset,
      startLine,
      startChar,
    );
  };
  const consumeString = () => {
    const [startOffset, startLine, startChar] = position();
    advance();
    while (!atEnd()) {
      if (current() === "\\") {
        advance();
        if (!atEnd()) advance();
      } else if (current() === "'") {
        advance();
        break;
      } else {
        advance();
      }
    }
    push("string", startOffset, startLine, startChar);
  };
  const consumeCharacter = () => {
    const [startOffset, startLine, startChar] = position();
    advance();
    if (!atEnd() && current() === "\\") {
      advance();
      if (!atEnd() && /[0-9]/.test(current())) {
        while (!atEnd() && /[0-9]/.test(current())) advance();
        if (!atEnd() && current() === ";") advance();
      } else if (!atEnd()) {
        advance();
      }
    } else if (!atEnd()) {
      advanceCodePoint();
    }
    push("character", startOffset, startLine, startChar);
  };
  const consumeNumber = () => {
    const [startOffset, startLine, startChar] = position();
    if (current() === "-") advance();
    while (!atEnd() && /[0-9]/.test(current())) advance();
    if (!atEnd() && current() === "." && /[0-9]/.test(next() || "")) {
      advance();
      while (!atEnd() && /[0-9]/.test(current())) advance();
    }
    push("number", startOffset, startLine, startChar);
  };
  const consumeSpecialOpen = () => {
    const [startOffset, startLine, startChar] = position();
    advance();
    advance();
    push("punctuation", startOffset, startLine, startChar);
  };
  while (!atEnd()) {
    const ch = current();
    if (/\s/.test(ch)) {
      advance();
    } else if (ch === "#") {
      while (!atEnd() && current() !== "\n" && current() !== "\r") advance();
    } else if (ch === "/" && next() === "*") {
      advance();
      advance();
      while (!atEnd() && !(current() === "*" && next() === "/")) advance();
      if (!atEnd()) {
        advance();
        advance();
      }
    } else if (ch === "'") {
      consumeString();
    } else if (ch === "`") {
      consumeCharacter();
    } else if (specialOpenPrefixes.has(ch) && next() === "(") {
      consumeSpecialOpen();
    } else if (ch === "." && next() === "*") {
      consumeName();
    } else if (isNumberStart(text, offset)) {
      consumeNumber();
    } else if (specialNameChars.has(ch)) {
      const [startOffset, startLine, startChar] = position();
      advance();
      push("punctuation", startOffset, startLine, startChar);
    } else {
      consumeName();
    }
  }
  return tokens;
};

const isNumberStart = (text, offset) => {
  const ch = text[offset];
  const nextCh = text[offset + 1];
  return /[0-9]/.test(ch) || (ch === "-" && /[0-9]/.test(nextCh || ""));
};
const isNameChar = (ch) => {
  return ch !== undefined && !/\s/.test(ch) && !specialNameChars.has(ch);
};
const isOpen = (text) => closeForOpen.has(text);
const isClose = (text) => closingDelimiters.has(text);
const matchingClose = (text) => closeForOpen.get(text);

const tokenDepths = (tokens) => {
  const depths = [];
  let depth = 0;
  for (const token of tokens) {
    depths.push(depth);
    if (isOpen(token.text)) depth += 1;
    if (isClose(token.text)) depth = Math.max(0, depth - 1);
  }
  return depths;
};

const bracketPairs = (tokens) => {
  const pairs = new Map();
  const stack = [];
  for (const token of tokens) {
    if (isOpen(token.text)) {
      stack.push(token.index);
    } else if (isClose(token.text)) {
      const open = stack.at(-1);
      if (
        open !== undefined && matchingClose(tokens[open].text) === token.text
      ) {
        stack.pop();
        pairs.set(open, token.index);
      }
    }
  }
  return pairs;
};

const findMatching = (tokens, openIndex) => {
  return bracketPairs(tokens).get(openIndex) ?? -1;
};

const findOpening = (tokens, closeIndex) => {
  for (const [open, close] of bracketPairs(tokens)) {
    if (close === closeIndex) return open;
  }
  return -1;
};

const splitTopLevel = (tokens, start, end, separators) => {
  const segments = [];
  let depth = 0;
  let segmentStart = start;
  for (let index = start; index < end; index += 1) {
    const value = tokens[index].text;
    if (isOpen(value)) depth += 1;
    else if (isClose(value)) depth -= 1;
    else if (depth === 0 && separators.has(value)) {
      if (segmentStart < index) {
        segments.push({ start: segmentStart, end: index });
      }
      segmentStart = index + 1;
    }
  }
  if (segmentStart < end) segments.push({ start: segmentStart, end });
  return segments;
};

// a leading graith and its following let form one member declaration.
const splitDeclarations = (
  tokens,
  start,
  end,
  heads = new Set(["let", "graith"]),
) => {
  const depths = tokenDepths(tokens);
  const baseDepth = depths[start] ?? 0;
  const segments = [];
  let segmentStart = -1;
  let awaitingGraithLet = false;
  for (let index = start; index < end; index += 1) {
    const token = tokens[index];
    if (
      depths[index] !== baseDepth || token.kind !== "keyword" ||
      !heads.has(token.text)
    ) continue;
    if (token.text === "graith") {
      if (segmentStart >= 0) segments.push({ start: segmentStart, end: index });
      segmentStart = index;
      awaitingGraithLet = true;
    } else if (token.text === "let" && awaitingGraithLet) {
      awaitingGraithLet = false;
    } else {
      if (segmentStart >= 0) segments.push({ start: segmentStart, end: index });
      segmentStart = index;
    }
  }
  if (segmentStart >= 0) segments.push({ start: segmentStart, end });
  return segments;
};

const findFileDeclarationBoundary = (
  tokens,
  start,
  depths = tokenDepths(tokens),
  baseDepth = depths[start] ?? 0,
) => {
  for (let index = start; index < tokens.length; index += 1) {
    if (depths[index] !== baseDepth) continue;
    if (isClose(tokens[index].text)) return index;
    if (
      tokens[index].kind === "keyword" &&
      fileDeclarationKeywords.has(tokens[index].text)
    ) return index;
  }
  return tokens.length;
};

const bringParts = (tokens, bringIndex, depths = tokenDepths(tokens)) => {
  const end = findFileDeclarationBoundary(
    tokens,
    bringIndex + 1,
    depths,
    depths[bringIndex] ?? 0,
  );
  const pathTokens = [];
  let index = bringIndex + 1;
  if (index < end && ["name", "keyword"].includes(tokens[index]?.kind)) {
    pathTokens.push(tokens[index]);
    index += 1;
    while (
      index + 1 < end && tokens[index]?.text === "." &&
      ["name", "keyword"].includes(tokens[index + 1]?.kind)
    ) {
      pathTokens.push(tokens[index], tokens[index + 1]);
      index += 2;
    }
  }
  const aliasToken = index < end && tokens[index]?.kind === "name"
    ? tokens[index]
    : undefined;
  return { end, pathTokens, aliasToken };
};

const bringNamespace = (path, alias) => {
  const canonical = path.split(".")[0];
  return alias || canonical.split("/").at(-1) || canonical;
};

const languageNames = {
  keywords: [...keywords],
  primitiveTypes: [...primitiveTypes],
};

export {
  bracketPairs,
  bringNamespace,
  bringParts,
  declarationKeywords,
  findFileDeclarationBoundary,
  findMatching,
  findOpening,
  isClose,
  isOpen,
  languageNames,
  primitiveTypes,
  splitDeclarations,
  splitTopLevel,
  tokenDepths,
  tokenize,
};
