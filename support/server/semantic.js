const tokenTypes = [
  "namespace",
  "type",
  "class",
  "shape",
  "function",
  "method",
  "variable",
  "parameter",
  "property",
  "enumMember",
  "operator",
  "call"
];

const tokenModifiers = ["declaration", "definition", "defaultLibrary", "effect"];

const keywords = new Set([
  "let",
  "graith",
  "bring",
  "let-ilk",
  "choose",
  "deed",
  "shape",
  "fill",
  "class",
  "foreign",
  "show",
  "show-ilk",
  "match",
  "try",
  "return",
  "resume"
]);

const primitiveTypes = new Set(["integer", "float", "string", "character", "func"]);
const primitiveConstructors = new Set(["yea", "nay", "null", "done"]);
const primitiveEffects = new Set(["console", "random", "async", "file", "fail", "state", "divergent"]);
const primitiveFunctions = new Set([
  "to-string",
  "from-string",
  "⌊",
  "≤",
  "≡",
  "≢",
  "+",
  "-",
  "×",
  "÷",
  "exponent",
  "logarithm",
  "sine",
  "cosine",
  "tangent",
  "write",
  "write-line",
  "read",
  "sleep",
  "random",
  "read-file",
  "write-file",
  "append-file",
  "map",
  "pure",
  "apply",
  "map-flat",
  "flat",
  "fold",
  "foldr",
  "foldl",
  "filter",
  "size"
]);
const primitiveShapes = new Set([
  "equal",
  "order",
  "semigroup",
  "monoid",
  "add",
  "zero",
  "subtract",
  "multiply",
  "one",
  "semiring",
  "ring",
  "divide",
  "field",
  "functor",
  "applicative",
  "monad",
  "cata",
  "size",
  "filter"
]);

const specialNameChars = new Set(["#", "(", ")", "{", "}", "[", "]", ":", ",", "|", "!", "=", ";", ".", "$", "→", "`", "'"]);
const applicationStartTexts = new Set(["(", "{", "[", ":", ",", "|", "=", ";"]);
const declarationKeywords = new Set(["let", "let-ilk", "choose", "deed", "shape", "fill", "class"]);
const typeRoles = new Set(["type", "typeParameter", "shape"]);
const callableRoles = new Set(["function", "method", "operator"]);
const separators = {
  comma: new Set([","]),
  semicolon: new Set([";"]),
  member: new Set([";", ","])
};
const closeForOpen = new Map([["(", ")"], ["{", "}"], ["[", "]"]]);
const openForClose = new Map([...closeForOpen].map(([open, close]) => [close, open]));
const declarationCollectors = {
  bring: collectImport,
  "let-ilk": collectTypeAlias,
  choose: collectData,
  deed: collectEffect,
  shape: collectShape,
  fill: collectFill,
  let: collectLet,
  class: collectForeign
};

function buildSemanticRanges(text, resolveDefinition = () => undefined) {
  const tokens = tokenize(text);
  const info = analyze(tokens);
  const emitted = [];

  for (const token of tokens) {
    if (info.importTokens.has(token.index) || info.exportTokens.has(token.index)) continue;
    const definition = resolveDefinition(token);
    const semantic = classifyToken(token, tokens, info, definition);
    if (!semantic) {
      continue;
    }
    emitted.push(...semanticRanges(token, semantic, info));
  }

  return emitted
    .filter((item) => item.length > 0)
    .sort((a, b) => a.line - b.line || a.char - b.char || a.length - b.length);
}

function classifyToken(token, tokens, info, definition) {
  const analyzed = info.semantic.get(token.index);
  const semantic = (analyzed?.modifiers.includes("declaration") && analyzed)
    || (typeRoles.has(definition?.role) && semanticFromDefinition(definition))
    || analyzed
    || functionPositionSemantic(token, tokens, info, definition)
    || semanticFromDefinition(definition)
    || inferredSemantic(token, tokens, info);

  if (semantic?.type === "typeParameter") return { ...semantic, type: "type" };
  if (semantic?.type === "enumMember" && semantic.modifiers.includes("declaration")
    && info.callableConstructors.has(lastQualifiedSegment(token.text))) {
    return { ...semantic, type: "function" };
  }
  return semantic;
}

function functionPositionSemantic(token, tokens, info, definition) {
  if (!isFunctionPosition(tokens, token.index)) return undefined;
  const name = lastQualifiedSegment(token.text);
  const role = definition?.role;
  if (typeRoles.has(role)) return undefined;
  if (info.types.has(name) || info.shapes.has(name) || info.effects.has(name) && !info.effectOps.has(name)) return undefined;
  const modifiers = definition?.modifiers?.includes("effect") || info.effectOps.has(name) ? ["effect"] : [];
  return { type: "call", modifiers };
}

function semanticFromDefinition(definition) {
  if (!definition) return undefined;
  if (["function", "method", "operator", "parameter", "variable"].includes(definition.role)) return undefined;
  const type = definition.role === "typeParameter" ? "type" : callableRoles.has(definition.role) ? "variable" : definition.role;
  if (!tokenTypes.includes(type)) return undefined;
  return {
    type,
    modifiers: (definition.modifiers || []).filter((modifier) => !["declaration", "definition"].includes(modifier))
  };
}

function semanticRole(token, tokens, info) {
  return info.semantic.get(token.index) || inferredSemantic(token, tokens, info);
}

function tokenize(text) {
  const tokens = [];
  let offset = 0;
  let line = 0;
  let char = 0;

  const current = () => text[offset];
  const next = () => text[offset + 1];
  const atEnd = () => offset >= text.length;

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
      endChar: char
    });
  };

  const consumeName = () => {
    const startOffset = offset;
    const startLine = line;
    const startChar = char;
    if (current() === "." && next() === "*") {
      advance();
      advance();
    }
    while (!atEnd() && isNameChar(current())) {
      advance();
    }
    const value = text.slice(startOffset, offset);
    push(keywords.has(value) ? "keyword" : "name", startOffset, startLine, startChar);
  };

  while (!atEnd()) {
    const ch = current();
    if (/\s/.test(ch)) {
      advance();
    } else if (ch === "#") {
      while (!atEnd() && current() !== "\n" && current() !== "\r") {
        advance();
      }
    } else if (ch === "'") {
      consumeString();
    } else if (ch === "`") {
      consumeCharacter();
    } else if (ch === "." && next() === "*") {
      consumeName();
    } else if (isNumberStart(text, offset)) {
      consumeNumber();
    } else if (specialNameChars.has(ch)) {
      const startOffset = offset;
      const startLine = line;
      const startChar = char;
      advance();
      push("punctuation", startOffset, startLine, startChar);
    } else {
      consumeName();
    }
  }

  return tokens;

  function consumeString() {
    const startOffset = offset;
    const startLine = line;
    const startChar = char;
    advance();
    while (!atEnd()) {
      if (current() === "\\") {
        advance();
        if (!atEnd()) {
          advance();
        }
      } else if (current() === "'") {
        advance();
        break;
      } else {
        advance();
      }
    }
    push("string", startOffset, startLine, startChar);
  }

  function consumeCharacter() {
    const startOffset = offset;
    const startLine = line;
    const startChar = char;
    advance();
    if (!atEnd() && current() === "\\") {
      advance();
      if (!atEnd() && current() === "{") {
        advance();
        while (!atEnd() && /[0-9]/.test(current())) {
          advance();
        }
        if (!atEnd() && current() === "}") {
          advance();
        }
      } else if (!atEnd()) {
        advance();
      }
    } else if (!atEnd() && current() === "{") {
      advance();
      while (!atEnd() && /[0-9]/.test(current())) {
        advance();
      }
      if (!atEnd() && current() === "}") {
        advance();
      }
    } else if (!atEnd()) {
      advance();
    }
    push("character", startOffset, startLine, startChar);
  }

  function consumeNumber() {
    const startOffset = offset;
    const startLine = line;
    const startChar = char;
    if (current() === "-") {
      advance();
    }
    while (!atEnd() && /[0-9]/.test(current())) {
      advance();
    }
    if (!atEnd() && current() === "." && /[0-9]/.test(next() || "")) {
      advance();
      while (!atEnd() && /[0-9]/.test(current())) {
        advance();
      }
    }
    push("number", startOffset, startLine, startChar);
  }
}

function isNumberStart(text, offset) {
  const ch = text[offset];
  const nextCh = text[offset + 1];
  return /[0-9]/.test(ch) || (ch === "-" && /[0-9]/.test(nextCh || ""));
}

function isNameChar(ch) {
  return ch !== undefined && !/\s/.test(ch) && !specialNameChars.has(ch);
}

function analyze(tokens) {
  const info = {
    semantic: new Map(),
    types: new Set(primitiveTypes),
    constructors: new Set(primitiveConstructors),
    callableConstructors: new Set(),
    effects: new Set(primitiveEffects),
    effectOps: new Set(["write", "read", "sleep", "random", "read-file", "write-file", "append-file"]),
    functions: new Set(primitiveFunctions),
    shapes: new Set(primitiveShapes),
    imports: new Set(),
    importTokens: new Set(),
    exportTokens: new Set(),
    priorities: new Map()
  };

  for (const token of tokens) {
    declarationCollectors[token.text]?.(tokens, token.index, info);
  }
  collectExportLists(tokens, info);
  collectPatternBindings(tokens, info);

  return info;
}

function collectExportLists(tokens, info) {
  for (const token of tokens) {
    if (!["show", "show-ilk"].includes(token.text)) continue;
    const next = tokens[token.index + 1];
    if (!next || next.kind === "keyword" || next.text === ";") continue;
    for (let i = next.index; i < tokens.length && tokens[i].text !== ";"; i += 1) {
      if (token.text === "show-ilk" && tokens[i].kind === "name") {
        mark(info, i, "type", [], 110);
      } else if (token.text === "show") {
        info.exportTokens.add(i);
      }
    }
  }
}

function collectImport(tokens, index, info) {
  const path = tokens[index + 1];
  if (!path || !["name", "keyword"].includes(path.kind)) {
    return;
  }
  for (let i = index + 1; i < tokens.length && tokens[i].text !== ";"; i += 1) {
    info.importTokens.add(i);
  }
  const namespace = path.text.split(".")[0];
  if (namespace) {
    info.imports.add(namespace);
  }
}

function collectTypeAlias(tokens, index, info) {
  const name = tokens[index + 1];
  if (name && name.kind === "name") {
    info.types.add(name.text);
    mark(info, name.index, "type", ["declaration"], 100);
  }
  const equals = findNextText(tokens, index + 2, "=");
  if (equals < 0) return;
  const semicolon = findNextText(tokens, equals + 1, ";");
  markTypeTokens(tokens, equals + 1, semicolon < 0 ? tokens.length : semicolon, info);
}

function collectData(tokens, index, info) {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const terms = headerTerms(tokens, index + 1, open);
  const header = defaultHeader(terms);
  markOwnerHeader(info, header, info.types, "type");

  for (const segment of splitTopLevel(tokens, open + 1, close, separators.comma)) {
    const terms = headerTerms(tokens, segment.start, segment.end);
    const ctorHeader = defaultHeader(terms);
    if (ctorHeader.name) {
      markTypeTerms(terms, info, new Set([ctorHeader.name.index]));
      info.constructors.add(ctorHeader.name.text);
      if (terms.length > 1) info.callableConstructors.add(ctorHeader.name.text);
      mark(info, ctorHeader.name.index, "enumMember", ["declaration"], 100);
    }
  }
}

function collectEffect(tokens, index, info) {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const terms = headerTerms(tokens, index + 1, open);
  const header = defaultHeader(terms);
  markOwnerHeader(info, header, info.effects, "type", ["declaration", "effect"]);

  for (const segment of splitTopLevel(tokens, open + 1, close, separators.comma)) {
    collectTypedMember(tokens, segment, info, "method", ["declaration", "effect"], true);
  }
}

function collectShape(tokens, index, info) {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const header = trailingHeader(headerTerms(tokens, index + 1, open));
  markOwnerHeader(info, header, info.shapes, "shape");

  for (const segment of splitTopLevel(tokens, open + 1, close, separators.member)) {
    if (tokens[segment.start] && tokens[segment.start].text === "let") {
      collectMemberLet(tokens, segment, info);
    } else {
      collectTypedMember(tokens, segment, info, "method", ["declaration"]);
    }
  }
}

function collectFill(tokens, index, info) {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const header = trailingHeader(headerTerms(tokens, index + 1, open));
  if (header.name) mark(info, header.name.index, "shape", [], 70);
  for (const segment of splitTopLevel(tokens, open + 1, close, separators.semicolon)) {
    if (tokens[segment.start] && tokens[segment.start].text === "let") {
      collectMemberLet(tokens, segment, info);
    }
  }
}

function declarationBody(tokens, start) {
  const open = findNextText(tokens, start, "{");
  const close = open < 0 ? -1 : findMatching(tokens, open);
  return close < 0 ? undefined : { open, close };
}

function markOwnerHeader(info, header, names, role, modifiers = ["declaration"]) {
  if (header.name) {
    names.add(header.name.text);
    mark(info, header.name.index, role, modifiers, 100);
  }
  for (const param of header.params) {
    mark(info, param.index, "typeParameter", ["declaration"], 100);
  }
}

function collectTypedMember(tokens, segment, info, role, modifiers, effectOperation = false, markInputs = true) {
  const colon = findTopLevelText(tokens, segment.start, segment.end, ":");
  if (colon < 0) return;
  const terms = headerTerms(tokens, segment.start, colon);
  const member = defaultHeader(terms);
  if (member.name && markInputs) markTypeTerms(terms, info, new Set([member.name.index]));
  markTypeTokens(tokens, colon + 1, segment.end, info);
  if (!member.name) return;

  info.functions.add(member.name.text);
  if (effectOperation) info.effectOps.add(member.name.text);
  mark(info, member.name.index, role, modifiers, 100);
}

function collectMemberLet(tokens, segment, info) {
  collectValueLet(tokens, segment.start + 1, segment.end, info, true);
}

function collectLet(tokens, index, info) {
  collectValueLet(tokens, index + 1, tokens.length, info, false);
}

function collectValueLet(tokens, start, end, info, member) {
  const equals = findTopLevelText(tokens, start, end, "=");
  if (equals < 0) return;
  const colon = findTopLevelText(tokens, start, equals, ":");
  const terms = headerTerms(tokens, start, colon < 0 ? equals : colon);
  const header = defaultHeader(terms);
  if (colon >= 0) markTypeTokens(tokens, colon + 1, equals, info);
  if (!header.name) return;

  info.functions.add(header.name.text);
  const role = member ? "method" : terms.length > 1 || containsAnonymousFunction(tokens, equals + 1) ? "function" : "variable";
  mark(info, header.name.index, role, ["declaration"], member ? 110 : 100);
  markValueParameters(terms, header, info);
}

function markValueParameters(terms, header, info) {
  for (const param of header.params) {
    mark(info, param.index, "parameter", ["declaration"], 90);
  }
  for (const term of terms) {
    markTypedGroupParams(term, info);
  }
}

function collectForeign(tokens, index, info) {
  const foreign = tokens[index + 1];
  if (foreign?.text !== "foreign") return;
  const body = declarationBody(tokens, foreign.index + 1);
  if (!body) return;
  for (const segment of splitTopLevel(tokens, body.open + 1, body.close, separators.semicolon)) {
    collectTypedMember(tokens, segment, info, "function", ["declaration"], false, false);
  }
}

function collectPatternBindings(tokens, info) {
  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].text !== "|") {
      continue;
    }
    const start = armStart(tokens, i - 1);
    for (const segment of splitTopLevel(tokens, start, i, separators.comma)) {
      markPatternSegment(tokens, segment, info);
    }
  }
}

function armStart(tokens, index) {
  let depth = 0;
  for (let i = index; i >= 0; i -= 1) {
    const text = tokens[i].text;
    if (isClose(text)) {
      depth += 1;
    } else if (isOpen(text)) {
      if (depth === 0) {
        return i + 1;
      }
      depth -= 1;
    } else if (depth === 0 && text === "|") {
      const comma = findTopLevelText(tokens, i + 1, index + 1, ",");
      return comma < 0 ? i + 1 : comma + 1;
    } else if (depth === 0 && text === ";") {
      return i + 1;
    }
  }
  return 0;
}

function markPatternSegment(tokens, segment, info) {
  for (let i = segment.start; i < segment.end; i += 1) {
    const token = tokens[i];
    if (!token || token.kind !== "name" || token.text === "_") {
      continue;
    }
    const name = lastQualifiedSegment(token.text);
    if (primitiveConstructors.has(name) || info.constructors.has(name)) {
      continue;
    }
    if (isFunctionPosition(tokens, token.index) && (primitiveFunctions.has(name) || info.functions.has(name) || info.effectOps.has(name))) {
      continue;
    }
    mark(info, token.index, "parameter", ["declaration"], 95);
  }
}

function inferredSemantic(token, tokens, info) {
  if (token.kind !== "name") {
    return undefined;
  }

  const name = lastQualifiedSegment(token.text);
  if (primitiveTypes.has(name)) {
    return { type: "type", modifiers: ["defaultLibrary"] };
  }
  if (primitiveEffects.has(name)) {
    return { type: "type", modifiers: ["defaultLibrary", "effect"] };
  }
  if (primitiveConstructors.has(name)) {
    return { type: "enumMember", modifiers: ["defaultLibrary"] };
  }
  if (primitiveFunctions.has(name)) {
    return { type: "variable", modifiers: ["defaultLibrary"].concat(info.effectOps.has(name) ? ["effect"] : []) };
  }
  if (primitiveShapes.has(name)) {
    return { type: "shape", modifiers: ["defaultLibrary"] };
  }
  if (info.constructors.has(name)) {
    return { type: "enumMember", modifiers: [] };
  }
  if (info.types.has(name)) {
    return { type: "type", modifiers: [] };
  }
  if (info.effectOps.has(name) && !isTypePosition(tokens, token.index)) {
    return { type: "variable", modifiers: ["effect"] };
  }
  if (info.effects.has(name)) {
    return { type: "type", modifiers: ["effect"] };
  }
  if (info.shapes.has(name)) {
    return { type: "shape", modifiers: [] };
  }
  if (info.functions.has(name)) {
    return { type: "variable", modifiers: [] };
  }
  if (isFunctionPosition(tokens, token.index)) {
    return { type: "call", modifiers: [] };
  }

  return undefined;
}

function semanticRanges(token, semantic, info) {
  const type = semantic.type;
  if (!token.text.includes("@")) {
    return [rangeFor(token, 0, token.text.length, type, semantic.modifiers)];
  }

  const split = token.text.lastIndexOf("@");
  const qualifier = token.text.slice(0, split);
  const name = token.text.slice(split + 1);
  if (!qualifier || !name) {
    return [rangeFor(token, 0, token.text.length, type, semantic.modifiers)];
  }

  if (info.imports.has(qualifier.split("@")[0])) {
    return [
      rangeFor(token, 0, qualifier.length, "namespace", []),
      rangeFor(token, split + 1, name.length, type, semantic.modifiers)
    ];
  }

  return [
    rangeFor(token, 0, qualifier.length, "variable", []),
    rangeFor(token, split + 1, name.length, type === "call" ? "call" : "property", [])
  ];
}

function rangeFor(token, startDelta, length, type, modifiers) {
  return {
    line: token.line,
    char: token.char + startDelta,
    length,
    type,
    modifiers
  };
}

function mark(info, tokenIndex, type, modifiers, priority) {
  const oldPriority = info.priorities.get(tokenIndex) || 0;
  if (priority < oldPriority) {
    return;
  }
  info.priorities.set(tokenIndex, priority);
  info.semantic.set(tokenIndex, { type, modifiers });
}

function findNextText(tokens, start, text) {
  for (let i = start; i < tokens.length; i += 1) {
    if (tokens[i].text === text) {
      return i;
    }
    if (tokens[i].text === ";") {
      return -1;
    }
  }
  return -1;
}

function findTopLevelText(tokens, start, end, text) {
  let depth = 0;
  for (let i = start; i < end; i += 1) {
    const value = tokens[i].text;
    if (isOpen(value)) {
      depth += 1;
    } else if (isClose(value)) {
      depth -= 1;
    } else if (depth === 0 && value === text) {
      return i;
    }
  }
  return -1;
}

function findMatching(tokens, openIndex) {
  const open = tokens[openIndex] && tokens[openIndex].text;
  const close = matchingClose(open);
  if (!close) {
    return -1;
  }
  let depth = 0;
  for (let i = openIndex; i < tokens.length; i += 1) {
    if (tokens[i].text === open) {
      depth += 1;
    } else if (tokens[i].text === close) {
      depth -= 1;
      if (depth === 0) {
        return i;
      }
    }
  }
  return -1;
}

function splitTopLevel(tokens, start, end, separators) {
  const segments = [];
  let depth = 0;
  let segmentStart = start;
  for (let i = start; i < end; i += 1) {
    const value = tokens[i].text;
    if (isOpen(value)) {
      depth += 1;
    } else if (isClose(value)) {
      depth -= 1;
    } else if (depth === 0 && separators.has(value)) {
      if (segmentStart < i) {
        segments.push({ start: segmentStart, end: i });
      }
      segmentStart = i + 1;
    }
  }
  if (segmentStart < end) {
    segments.push({ start: segmentStart, end });
  }
  return segments;
}

function headerTerms(tokens, start, end) {
  const terms = [];
  for (let i = start; i < end; i += 1) {
    const token = tokens[i];
    if (!token || token.kind === "keyword" && declarationKeywords.has(token.text)) {
      break;
    }
    if (token.text === "(") {
      const close = findMatching(tokens, i);
      const termEnd = close >= 0 && close < end ? close + 1 : i + 1;
      terms.push({ tokens: tokens.slice(i, termEnd), name: undefined });
      i = termEnd - 1;
    } else if (token.kind === "name") {
      terms.push({ tokens: [token], name: token });
    }
  }
  return terms;
}

function defaultHeader(terms) {
  if (terms.length === 1) {
    return { name: terms[0].name, params: [] };
  }
  if (terms.length > 1) {
    return { name: terms[1].name, params: terms.filter((_, index) => index !== 1).map((term) => term.name).filter(Boolean) };
  }
  return { name: undefined, params: [] };
}

function trailingHeader(terms) {
  if (terms.length === 0) {
    return { name: undefined, params: [] };
  }
  return {
    name: terms[terms.length - 1].name,
    params: terms.slice(0, -1).map((term) => term.name).filter(Boolean)
  };
}

function markTypedGroupParams(term, info) {
  if (!term.tokens.length || term.tokens[0].text !== "(") {
    return;
  }
  const inner = term.tokens.slice(1, -1);
  for (const segment of splitTopLevel(inner, 0, inner.length, separators.comma)) {
    const first = inner.slice(segment.start, segment.end).find((token) => token.kind === "name");
    if (first) {
      mark(info, first.index, "parameter", ["declaration"], 90);
    }
    const colon = findTopLevelText(inner, segment.start, segment.end, ":");
    if (colon >= 0) {
      markTypeTokens(inner, colon + 1, segment.end, info);
    }
  }
}

function markTypeTerms(terms, info, skip) {
  for (const term of terms) {
    markTypeTokens(term.tokens, 0, term.tokens.length, info, skip);
  }
}

function markTypeTokens(tokens, start, end, info, skip = new Set()) {
  for (let i = start; i < end; i += 1) {
    const token = tokens[i];
    if (!token || token.kind !== "name" || skip.has(token.index)) {
      continue;
    }
    const name = lastQualifiedSegment(token.text);
    if (primitiveTypes.has(name) || primitiveEffects.has(name) || info.types.has(name) || info.effects.has(name)) {
      mark(info, token.index, "type", primitiveEffects.has(name) || info.effects.has(name) ? ["effect"] : [], 80);
    } else if (primitiveShapes.has(name) || info.shapes.has(name)) {
      mark(info, token.index, "shape", [], 80);
    } else {
      mark(info, token.index, "typeParameter", [], 80);
    }
  }
}

function containsAnonymousFunction(tokens, start) {
  for (let i = start; i < Math.min(tokens.length, start + 4); i += 1) {
    if (tokens[i].text === "{") {
      return true;
    }
    if (tokens[i].text === ";") {
      return false;
    }
  }
  return false;
}

function isFunctionPosition(tokens, index) {
  const token = tokens[index];
  const prev = tokens[index - 1];
  if (!token || token.kind !== "name") {
    return false;
  }
  if (prev?.text === "$" || prev?.text === "(" && tokens[index - 2]?.text === "$") return true;

  const firstAtom = previousAtomStart(tokens, index);
  if (firstAtom < 0) return false;
  const boundary = tokens[firstAtom - 1];
  if (!boundary) return true;
  if (boundary.text === "$" || boundary.text === "(" && tokens[firstAtom - 2]?.text === "$") return false;
  if (applicationStartTexts.has(boundary.text) || boundary.kind === "keyword") return true;

  const dollarFunction = previousAtomStart(tokens, firstAtom);
  return dollarFunction >= 0 && tokens[dollarFunction - 1]?.text === "$";
}

function previousAtomStart(tokens, before) {
  const end = before - 1;
  const token = tokens[end];
  if (!token) return -1;
  if (isClose(token.text)) return matchingOpen(tokens, end);
  return isSimpleAtom(token) ? end : -1;
}

function matchingOpen(tokens, closeIndex) {
  const close = tokens[closeIndex].text;
  const open = openForClose.get(close);
  let depth = 0;
  for (let index = closeIndex; index >= 0; index -= 1) {
    if (tokens[index].text === close) depth += 1;
    if (tokens[index].text === open && --depth === 0) return index;
  }
  return -1;
}

function isSimpleAtom(token) {
  return ["name", "number", "string", "character"].includes(token.kind);
}

function isTypePosition(tokens, index) {
  const prev = tokens[index - 1];
  return prev && [":", "!", "→"].includes(prev.text);
}

function isOpen(text) {
  return closeForOpen.has(text);
}

function isClose(text) {
  return openForClose.has(text);
}

function matchingClose(text) {
  return closeForOpen.get(text);
}

function lastQualifiedSegment(name) {
  const at = name.lastIndexOf("@");
  return at >= 0 ? name.slice(at + 1) : name;
}

module.exports = {
  tokenTypes,
  tokenModifiers,
  buildSemanticRanges,
  semanticRole,
  tokenize,
  analyzeTokens: analyze,
  languageNames: {
    keywords: [...keywords],
    primitiveTypes: [...primitiveTypes],
    primitiveConstructors: [...primitiveConstructors],
    primitiveEffects: [...primitiveEffects],
    primitiveFunctions: [...primitiveFunctions],
    primitiveShapes: [...primitiveShapes]
  },
  _test: { tokenize, analyze, buildSemanticRanges }
};
