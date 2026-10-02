// shared tolerant source structure for colouring and document analysis.
import generatedLanguageNames from "../generated/language-names.json";

const keywords = new Set(generatedLanguageNames.keywords);
const primitiveTypes = new Set(generatedLanguageNames.primitiveTypes);
const specialNameChars = new Set([...generatedLanguageNames.specialNameChars]);
const declarationKeywords = new Set(["let", "ilk", "deed", "flock", "bizen"]);
const fileDeclarationKeywords = new Set([
  "use",
  "law",
  "yield",
  "show",
  "show-ilk",
  ...declarationKeywords,
]);
const closeForOpen = new Map([
  ["(", ")"],
  ["[", "]"],
  ["r{", "}"],
  ["<{", "}"],
  [">{", "}"],
  ["{", "}"],
]);
const closingDelimiters = new Set(closeForOpen.values());

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
    while (!atEnd() && isNameChar(current())) advance();
    const value = text.slice(startOffset, offset);
    push(
      value === "*" ? "punctuation" : keywords.has(value) ? "keyword" : "name",
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
    } else if (closeForOpen.has(ch + next())) {
      consumeSpecialOpen();
    } else if (isNumberStart(text, offset)) {
      consumeNumber();
    } else if (ch === "→") {
      const [startOffset, startLine, startChar] = position();
      advance();
      push("name", startOffset, startLine, startChar);
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

// token arrays are complete after tokenisation; semantic and structural
// lookups share one scan for depths and matching delimiters.
const structureCache = new WeakMap();
const tokenStructure = (tokens) => {
  const cached = structureCache.get(tokens);
  if (cached) return cached;
  const depths = [];
  const forward = new Map();
  const backward = new Map();
  const stack = [];
  let depth = 0;
  for (const token of tokens) {
    depths.push(depth);
    if (isOpen(token.text)) {
      depth += 1;
      stack.push(token.index);
    } else if (isClose(token.text)) {
      depth = Math.max(0, depth - 1);
      const open = stack.at(-1);
      if (
        open !== undefined &&
        matchingClose(tokens[open].text) === token.text
      ) {
        stack.pop();
        forward.set(open, token.index);
        backward.set(token.index, open);
      }
    }
  }
  const structure = { depths, forward, backward };
  structureCache.set(tokens, structure);
  return structure;
};
const tokenDepths = (tokens) => tokenStructure(tokens).depths;
const bracketPairs = (tokens) => tokenStructure(tokens).forward;

const findMatching = (tokens, openIndex) => {
  return tokenStructure(tokens).forward.get(openIndex) ?? -1;
};

const findOpening = (tokens, closeIndex) => {
  return tokenStructure(tokens).backward.get(closeIndex) ?? -1;
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

// pattern arms recover independently in incomplete buffers: an unmatched
// opener begins a local pattern, and a missing body terminator extends to EOF.
const patternArmRegions = (tokens) => {
  const regions = [];
  const scopes = [{ patternStart: 0, openArms: [] }];
  const closeArms = (scope, bodyEnd) => {
    for (const arm of scope.openArms) arm.bodyEnd = bodyEnd;
    scope.openArms = [];
  };
  for (const token of tokens) {
    if (isOpen(token.text)) {
      scopes.push({ patternStart: token.index + 1, openArms: [] });
      continue;
    }
    const scope = scopes.at(-1);
    if (token.text === "^") {
      const arm = {
        patternStart: scope.patternStart,
        pipe: token,
        bodyEnd: tokens.length,
      };
      regions.push(arm);
      scope.openArms.push(arm);
      scope.patternStart = token.index + 1;
    } else if (token.text === "," && scope.openArms.length) {
      closeArms(scope, token.index);
      scope.patternStart = token.index + 1;
    } else if (isClose(token.text)) {
      closeArms(scope, token.index);
      if (scopes.length > 1) scopes.pop();
    }
  }
  return regions;
};

const splitDeclarations = (tokens, start, end, heads = new Set(["let"])) => {
  const depths = tokenDepths(tokens);
  const baseDepth = depths[start] ?? 0;
  const segments = [];
  let segmentStart = -1;
  for (let index = start; index < end; index += 1) {
    const token = tokens[index];
    if (
      depths[index] !== baseDepth ||
      token.kind !== "keyword" ||
      !heads.has(token.text)
    )
      continue;
    if (segmentStart >= 0) segments.push({ start: segmentStart, end: index });
    segmentStart = index;
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
    )
      return index;
  }
  return tokens.length;
};

const findInstanceHeader = (tokens, start) => {
  let open;
  let close;
  let colon = start;
  if (tokens[start]?.text === "[") {
    open = start;
    close = findMatching(tokens, start);
    if (close < 0) return undefined;
    colon = close + 1;
  }
  if (tokens[colon]?.text !== ":") return undefined;
  const end = findFileDeclarationBoundary(tokens, start);
  const head = colon + 1;
  for (let index = head; index < end; index += 1) {
    if (tokens[index].text === "{") return { open, close, head, body: index };
    if (!isOpen(tokens[index].text)) continue;
    const matching = findMatching(tokens, index);
    if (matching < 0) return undefined;
    index = matching;
  }
  return undefined;
};

const useParts = (tokens, useIndex, depths = tokenDepths(tokens)) => {
  const end = findFileDeclarationBoundary(
    tokens,
    useIndex + 1,
    depths,
    depths[useIndex] ?? 0,
  );
  const pathTokens = [];
  let index = useIndex + 1;
  if (index < end && ["name", "keyword"].includes(tokens[index]?.kind)) {
    pathTokens.push(tokens[index]);
    index += 1;
    while (
      index + 1 < end &&
      tokens[index]?.text === "." &&
      ["name", "keyword"].includes(tokens[index + 1]?.kind)
    ) {
      pathTokens.push(tokens[index], tokens[index + 1]);
      index += 2;
    }
  }
  const aliasToken =
    index < end && tokens[index]?.kind === "name" ? tokens[index] : undefined;
  return { pathTokens, aliasToken };
};

const lastQualifiedSegment = (name) => {
  const separator = name.lastIndexOf("~");
  return separator > 0 && separator + 1 < name.length
    ? name.slice(separator + 1)
    : name;
};

const useNamespace = (path, alias) => {
  const basename = path.split("/").at(-1) || path;
  return alias || basename.split(".")[0];
};

const languageNames = generatedLanguageNames;

export {
  bracketPairs,
  declarationKeywords,
  findFileDeclarationBoundary,
  findInstanceHeader,
  findMatching,
  findOpening,
  isClose,
  isOpen,
  languageNames,
  lastQualifiedSegment,
  patternArmRegions,
  primitiveTypes,
  splitDeclarations,
  splitTopLevel,
  tokenDepths,
  tokenize,
  useNamespace,
  useParts,
};
