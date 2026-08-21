// tolerant lexical and structural role inference. it never consults imports or
// types; workspace definitions may refine a role after this pass.
import generatedLanguageNames from "../generated/language-names.json";
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
  "call",
];
const tokenModifiers = [
  "declaration",
  "definition",
  "defaultLibrary",
  "effect",
];
const keywords = new Set(generatedLanguageNames.keywords);
const primitiveTypes = new Set(generatedLanguageNames.primitiveTypes);
const specialNameChars = new Set([...generatedLanguageNames.specialNameChars]);
const applicationStartTexts = new Set([
  "(",
  "{",
  "[",
  ":",
  ",",
  "|",
  "=",
  ";",
  "~",
]);
const declarationKeywords = new Set([
  "let",
  "let-ilk",
  "kin",
  "deed",
  "shape",
  "fill",
]);
const graithEndKeywords = new Set(["let", "shape", "fill", "show", "show-ilk"]);
const typeRoles = new Set(["type", "typeParameter", "shape"]);
const callableRoles = new Set(["function", "method", "operator"]);
const separators = {
  comma: new Set([","]),
  semicolon: new Set([";"]),
  member: new Set([";", ","]),
};
const closeForOpen = new Map([
  ["(", ")"],
  ["{", "}"],
  ["[", "]"],
]);
const openForClose = new Map(
  [...closeForOpen].map(([open, close]) => [close, open]),
);
const buildSemanticRanges = (
  text,
  resolveDefinition = (_token) => undefined,
) => {
  const tokens = tokenize(text);
  const info = analyze(tokens);
  const emitted = [];
  for (const token of tokens) {
    if (
      info.importTokens.has(token.index) || info.exportTokens.has(token.index)
    ) continue;
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
};
const classifyToken = (token, tokens, info, definition) => {
  const analyzed = info.semantic.get(token.index);
  const semantic = (analyzed?.modifiers.includes("declaration") && analyzed) ||
    (typeRoles.has(definition?.role) &&
      isAnalyzedTypeRole(analyzed) &&
      semanticFromDefinition(definition)) ||
    analyzed ||
    functionPositionSemantic(token, tokens, info, definition) ||
    callableConstructorSemantic(token, tokens, info, definition) ||
    nonTypeSemanticFromDefinition(definition) ||
    inferredSemantic(token, tokens, info);
  if (semantic?.type === "typeParameter") return { ...semantic, type: "type" };
  if (
    semantic?.type === "enumMember" &&
    semantic.modifiers.includes("declaration") &&
    info.callableConstructors.has(lastQualifiedSegment(token.text))
  ) {
    return { ...semantic, type: "function" };
  }
  return semantic;
};
const isAnalyzedTypeRole = (semantic) => {
  return semantic && typeRoles.has(semantic.type);
};
const functionPositionSemantic = (token, tokens, info, definition) => {
  if (!isFunctionPosition(tokens, token.index)) return undefined;
  const name = lastQualifiedSegment(token.text);
  const role = definition?.role;
  if (typeRoles.has(role)) return undefined;
  if (
    info.types.has(name) ||
    info.shapes.has(name) ||
    (info.effects.has(name) && !info.effectOps.has(name))
  ) {
    return undefined;
  }
  const modifiers =
    definition?.modifiers?.includes("effect") || info.effectOps.has(name)
      ? ["effect"]
      : [];
  return { type: "call", modifiers };
};
const callableConstructorSemantic = (token, tokens, info, definition) => {
  if (!isFunctionPosition(tokens, token.index)) return undefined;
  if (definition?.role !== "enumMember") return undefined;
  const name = lastQualifiedSegment(token.text);
  if (info.callableConstructors.has(name) || definition.callableConstructor) {
    return { type: "call", modifiers: [] };
  }
  return undefined;
};
const semanticFromDefinition = (definition) => {
  if (!definition) return undefined;
  if (
    ["function", "method", "operator", "parameter", "variable"].includes(
      definition.role,
    )
  ) {
    return undefined;
  }
  const type = definition.role === "typeParameter"
    ? "type"
    : callableRoles.has(definition.role)
    ? "variable"
    : definition.role;
  if (!tokenTypes.includes(type)) return undefined;
  return {
    type,
    modifiers: (definition.modifiers || []).filter(
      (modifier) => !["declaration", "definition"].includes(modifier),
    ),
  };
};
const nonTypeSemanticFromDefinition = (definition) => {
  return typeRoles.has(definition?.role)
    ? undefined
    : semanticFromDefinition(definition);
};
const semanticRole = (token, tokens, info) => {
  return info.semantic.get(token.index) ||
    inferredSemantic(token, tokens, info);
};
const tokenize = (text) => {
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
    push(
      keywords.has(value) ? "keyword" : "name",
      startOffset,
      startLine,
      startChar,
    );
  };
  const consumeString = () => {
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
  };
  const consumeCharacter = () => {
    const startOffset = offset;
    const startLine = line;
    const startChar = char;
    advance();
    if (!atEnd() && current() === "\\") {
      advance();
      if (!atEnd() && /[0-9]/.test(current())) {
        while (!atEnd() && /[0-9]/.test(current())) {
          advance();
        }
        if (!atEnd() && current() === ";") {
          advance();
        }
      } else if (!atEnd()) {
        advance();
      }
    } else if (!atEnd()) {
      advanceCodePoint();
    }
    push("character", startOffset, startLine, startChar);
  };
  const consumeNumber = () => {
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
  };
  while (!atEnd()) {
    const ch = current();
    if (/\s/.test(ch)) {
      advance();
    } else if (ch === "#") {
      while (!atEnd() && current() !== "\n" && current() !== "\r") {
        advance();
      }
    } else if (ch === "/" && next() === "*") {
      advance();
      advance();
      while (!atEnd() && !(current() === "*" && next() === "/")) {
        advance();
      }
      if (!atEnd()) {
        advance();
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
};
const isNumberStart = (text, offset) => {
  const ch = text[offset];
  const nextCh = text[offset + 1];
  return /[0-9]/.test(ch) || (ch === "-" && /[0-9]/.test(nextCh || ""));
};
const isNameChar = (ch) => {
  return ch !== undefined && !/\s/.test(ch) && !specialNameChars.has(ch);
};
// declaration collectors establish known roles first; a later inference pass
// colours use sites from syntax position and lexical scope.
const analyze = (tokens) => {
  const info = {
    semantic: new Map(),
    types: new Set(primitiveTypes),
    constructors: new Set(),
    callableConstructors: new Set(),
    effects: new Set(),
    effectOps: new Set(),
    functions: new Set(),
    shapes: new Set(),
    imports: new Set(),
    importTokens: new Set(),
    exportTokens: new Set(),
    priorities: new Map(),
  };
  for (const token of tokens) {
    declarationCollectors[token.text]?.(tokens, token.index, info);
  }
  collectTermAscriptions(tokens, info);
  collectExportLists(tokens, info);
  collectPatternBindings(tokens, info);
  return info;
};
const collectTermAscriptions = (tokens, info) => {
  for (const token of tokens) {
    if (token.text !== "(") continue;
    const close = findMatching(tokens, token.index);
    if (close < 0) continue;
    const colon = findTopLevelText(tokens, token.index + 1, close, ":");
    if (colon >= 0) markTypeTokens(tokens, colon + 1, close, info);
  }
};
const collectExportLists = (tokens, info) => {
  for (const token of tokens) {
    if (!["show", "show-ilk"].includes(token.text)) continue;
    const next = tokens[token.index + 1];
    if (!next || next.kind === "keyword" || next.text === ";") continue;
    for (
      let i = next.index;
      i < tokens.length && tokens[i].text !== ";";
      i += 1
    ) {
      if (token.text === "show-ilk" && tokens[i].kind === "name") {
        mark(info, i, "type", [], 110);
      } else if (token.text === "show") {
        info.exportTokens.add(i);
      }
    }
  }
};
const collectImport = (tokens, index, info) => {
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
};
const collectGraith = (tokens, index, info) => {
  let end = tokens.length;
  let depth = 0;
  for (let i = index + 1; i < tokens.length; i += 1) {
    const value = tokens[i].text;
    if (isOpen(value)) depth += 1;
    else if (isClose(value)) depth -= 1;
    else if (
      depth === 0 && (graithEndKeywords.has(value) || value === ":")
    ) {
      end = i;
      break;
    }
  }
  for (
    const segment of splitTopLevel(tokens, index + 1, end, separators.comma)
  ) {
    const terms = headerTerms(tokens, segment.start, segment.end);
    const requirement = trailingHeader(terms);
    markTypeTerms(terms.slice(0, -1), info, new Set());
    if (requirement.name) mark(info, requirement.name.index, "shape", [], 85);
  }
};
const collectTypeAlias = (tokens, index, info) => {
  const equals = findNextText(tokens, index + 1, "=");
  if (equals < 0) return;
  const terms = headerTerms(tokens, index + 1, equals);
  markOwnerHeader(info, defaultHeader(terms), info.types, "type");
  const semicolon = findNextText(tokens, equals + 1, ";");
  markTypeTokens(
    tokens,
    equals + 1,
    semicolon < 0 ? tokens.length : semicolon,
    info,
  );
};
const collectData = (tokens, index, info) => {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const terms = headerTerms(tokens, index + 1, open);
  const header = defaultHeader(terms);
  markOwnerHeader(info, header, info.types, "type");
  for (
    const segment of splitTopLevel(tokens, open + 1, close, separators.comma)
  ) {
    const terms = headerTerms(tokens, segment.start, segment.end);
    const ctorHeader = defaultHeader(terms);
    if (ctorHeader.name) {
      markTypeTerms(terms, info, new Set([ctorHeader.name.index]));
      info.constructors.add(ctorHeader.name.text);
      if (terms.length > 1) info.callableConstructors.add(ctorHeader.name.text);
      mark(info, ctorHeader.name.index, "enumMember", ["declaration"], 100);
    }
  }
};
const collectEffect = (tokens, index, info) => {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const terms = headerTerms(tokens, index + 1, open);
  const header = defaultHeader(terms);
  markOwnerHeader(info, header, info.effects, "type", [
    "declaration",
    "effect",
  ]);
  for (
    const segment of splitTopLevel(tokens, open + 1, close, separators.comma)
  ) {
    collectTypedMember(tokens, segment, info, "method", [
      "declaration",
      "effect",
    ], true);
  }
};
const collectShape = (tokens, index, info) => {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const header = trailingHeader(headerTerms(tokens, index + 1, open));
  markOwnerHeader(info, header, info.shapes, "shape");
  for (
    const segment of splitTopLevel(tokens, open + 1, close, separators.member)
  ) {
    if (tokens[segment.start]?.text === "graith") {
      collectGraithShapeMember(tokens, segment, info);
    } else if (tokens[segment.start] && tokens[segment.start].text === "let") {
      collectMemberLet(tokens, segment, info);
    } else if (tokens[segment.start] && tokens[segment.start].text === "law") {
      collectShapeLaw(tokens, segment, info);
    } else {
      collectTypedMember(tokens, segment, info, "method", ["declaration"]);
    }
  }
};
const collectGraithShapeMember = (tokens, segment, info) => {
  const colon = findTopLevelText(tokens, segment.start, segment.end, ":");
  if (colon < 0) return;
  const parts = splitTopLevel(
    tokens,
    segment.start + 1,
    colon,
    separators.comma,
  );
  if (parts.length === 0) return;
  for (const requirement of parts.slice(0, -1)) {
    markGraithTerms(
      headerTerms(tokens, requirement.start, requirement.end),
      info,
    );
  }
  const tail = headerTerms(
    tokens,
    parts[parts.length - 1].start,
    parts[parts.length - 1].end,
  );
  const candidates = [];
  for (let split = 1; split < tail.length; split += 1) {
    const requirement = trailingHeader(tail.slice(0, split));
    const member = defaultHeader(tail.slice(split));
    if (requirement.name && member.name) {
      candidates.push({ split, member });
    }
  }
  if (candidates.length !== 1) return;
  const { split, member } = candidates[0];
  markGraithTerms(tail.slice(0, split), info);
  const memberTerms = tail.slice(split);
  markTypeTerms(memberTerms, info, new Set([member.name.index]));
  markTypeTokens(tokens, colon + 1, segment.end, info);
  info.functions.add(member.name.text);
  mark(info, member.name.index, "method", ["declaration"], 110);
};
const markGraithTerms = (terms, info) => {
  const requirement = trailingHeader(terms);
  markTypeTerms(terms.slice(0, -1), info, new Set());
  if (requirement.name) mark(info, requirement.name.index, "shape", [], 95);
};
const collectShapeLaw = (tokens, segment, info) => {
  const open = segment.start + 1;
  if (tokens[open]?.text !== "(") return;
  const close = findMatching(tokens, open);
  if (close < 0 || close >= segment.end) return;
  markTypedParameters(tokens, open + 1, close, info);
  const body = findTopLevelText(tokens, close + 1, segment.end, ":");
  const equation = body < 0
    ? -1
    : findTopLevelText(tokens, body + 1, segment.end, "~");
  if (equation >= 0) mark(info, equation, "operator", [], 100);
};
const collectFill = (tokens, index, info) => {
  const body = declarationBody(tokens, index + 1);
  if (!body) return;
  const { open, close } = body;
  const terms = headerTerms(tokens, index + 1, open);
  const header = trailingHeader(terms);
  markTypeTerms(terms.slice(0, -1), info, new Set());
  if (header.name) mark(info, header.name.index, "shape", [], 70);
  for (
    const segment of splitTopLevel(
      tokens,
      open + 1,
      close,
      separators.semicolon,
    )
  ) {
    if (tokens[segment.start] && tokens[segment.start].text === "let") {
      collectMemberLet(tokens, segment, info);
    }
  }
};
const declarationBody = (tokens, start) => {
  const open = findNextText(tokens, start, "{");
  const close = open < 0 ? -1 : findMatching(tokens, open);
  return close < 0 ? undefined : { open, close };
};
const markOwnerHeader = (
  info,
  header,
  names,
  role,
  modifiers = ["declaration"],
) => {
  if (header.name) {
    names.add(header.name.text);
    mark(info, header.name.index, role, modifiers, 100);
  }
  for (const param of header.params) {
    mark(info, param.index, "typeParameter", ["declaration"], 100);
  }
};
const collectTypedMember = (
  tokens,
  segment,
  info,
  role,
  modifiers,
  effectOperation = false,
  markInputs = true,
) => {
  const colon = findTopLevelText(tokens, segment.start, segment.end, ":");
  if (colon < 0) return;
  const terms = headerTerms(tokens, segment.start, colon);
  const member = defaultHeader(terms);
  if (member.name && markInputs) {
    markTypeTerms(terms, info, new Set([member.name.index]));
  }
  markTypeTokens(tokens, colon + 1, segment.end, info);
  if (!member.name) return;
  info.functions.add(member.name.text);
  if (effectOperation) info.effectOps.add(member.name.text);
  mark(info, member.name.index, role, modifiers, 100);
};
const collectMemberLet = (tokens, segment, info) => {
  collectValueLet(tokens, segment.start + 1, segment.end, info, true);
};
const collectLet = (tokens, index, info) => {
  collectValueLet(tokens, index + 1, tokens.length, info, false);
};
const collectValueLet = (tokens, start, end, info, member) => {
  const equals = findTopLevelText(tokens, start, end, "=");
  if (equals < 0) return;
  const colon = findTopLevelText(tokens, start, equals, ":");
  const terms = headerTerms(tokens, start, colon < 0 ? equals : colon);
  const header = defaultHeader(terms);
  if (colon >= 0) markTypeTokens(tokens, colon + 1, equals, info);
  if (!header.name) return;
  info.functions.add(header.name.text);
  const annotatedFunction = colon >= 0 &&
    findTopLevelText(tokens, colon + 1, equals, "→") >= 0;
  const role = member ? "method" : terms.length > 1 || annotatedFunction ||
      isAnonymousFunctionValue(tokens, equals + 1, end)
    ? "function"
    : "variable";
  mark(info, header.name.index, role, ["declaration"], member ? 110 : 100);
  markValueParameters(terms, header, info);
};
const markValueParameters = (terms, header, info) => {
  for (const param of header.params) {
    mark(info, param.index, "parameter", ["declaration"], 90);
  }
  for (const term of terms) {
    markTypedGroupParams(term, info);
  }
};
const declarationCollectors = {
  bring: collectImport,
  graith: collectGraith,
  "let-ilk": collectTypeAlias,
  kin: collectData,
  deed: collectEffect,
  shape: collectShape,
  fill: collectFill,
  let: collectLet,
};
const collectPatternBindings = (tokens, info) => {
  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].text !== "|") {
      continue;
    }
    const start = armStart(tokens, i - 1);
    for (const segment of splitTopLevel(tokens, start, i, separators.comma)) {
      markPatternSegment(tokens, segment, info);
    }
  }
};
const armStart = (tokens, index) => {
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
};
const markPatternSegment = (tokens, segment, info) => {
  for (let i = segment.start; i < segment.end; i += 1) {
    const token = tokens[i];
    if (!token || token.kind !== "name" || token.text === "_") {
      continue;
    }
    const name = lastQualifiedSegment(token.text);
    if (info.constructors.has(name)) {
      continue;
    }
    if (isFunctionPosition(tokens, token.index)) {
      continue;
    }
    mark(info, token.index, "parameter", ["declaration"], 95);
  }
};
// unresolved names are classified conservatively. function position is useful
// editor information, but a spelling alone must never turn a term into a type.
const inferredSemantic = (token, tokens, info) => {
  if (token.kind !== "name") {
    return undefined;
  }
  const name = lastQualifiedSegment(token.text);
  if (primitiveTypes.has(name)) {
    return { type: "type", modifiers: ["defaultLibrary"] };
  }
  if (
    info.constructors.has(name) &&
    info.callableConstructors.has(name) &&
    isFunctionPosition(tokens, token.index)
  ) {
    return { type: "call", modifiers: [] };
  }
  if (info.constructors.has(name)) {
    return { type: "enumMember", modifiers: [] };
  }
  if (info.types.has(name) && isTypePosition(tokens, token.index)) {
    return { type: "type", modifiers: [] };
  }
  if (info.effectOps.has(name) && !isTypePosition(tokens, token.index)) {
    return { type: "variable", modifiers: ["effect"] };
  }
  if (info.effects.has(name) && isTypePosition(tokens, token.index)) {
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
};
const semanticRanges = (token, semantic, info) => {
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
      rangeFor(token, split + 1, name.length, type, semantic.modifiers),
    ];
  }
  return [
    rangeFor(token, 0, qualifier.length, "variable", []),
    rangeFor(
      token,
      split + 1,
      name.length,
      type === "call" ? "call" : "property",
      [],
    ),
  ];
};
const rangeFor = (token, startDelta, length, type, modifiers) => {
  return {
    line: token.line,
    char: token.char + startDelta,
    length,
    type,
    modifiers,
  };
};
const mark = (info, tokenIndex, type, modifiers, priority) => {
  const oldPriority = info.priorities.get(tokenIndex) || 0;
  if (priority < oldPriority) {
    return;
  }
  info.priorities.set(tokenIndex, priority);
  info.semantic.set(tokenIndex, { type, modifiers });
};
const findNextText = (tokens, start, text) => {
  for (let i = start; i < tokens.length; i += 1) {
    if (tokens[i].text === text) {
      return i;
    }
    if (tokens[i].text === ";") {
      return -1;
    }
  }
  return -1;
};
const findTopLevelText = (tokens, start, end, text) => {
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
};
const findMatching = (tokens, openIndex) => {
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
};
const splitTopLevel = (tokens, start, end, separators) => {
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
};
const headerTerms = (tokens, start, end) => {
  const terms = [];
  for (let i = start; i < end; i += 1) {
    const token = tokens[i];
    if (
      !token ||
      (token.kind === "keyword" && declarationKeywords.has(token.text))
    ) {
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
};
const defaultHeader = (terms) => {
  if (terms.length === 1) {
    return { name: terms[0].name, params: [] };
  }
  if (terms.length > 1) {
    return {
      name: terms[1].name,
      params: terms
        .filter((_, index) => index !== 1)
        .map((term) => term.name)
        .filter(Boolean),
    };
  }
  return { name: undefined, params: [] };
};
const trailingHeader = (terms) => {
  if (terms.length === 0) {
    return { name: undefined, params: [] };
  }
  return {
    name: terms[terms.length - 1].name,
    params: terms
      .slice(0, -1)
      .map((term) => term.name)
      .filter(Boolean),
  };
};
const markTypedGroupParams = (term, info) => {
  if (term.tokens[0]?.text === "(") {
    markTypedParameters(term.tokens, 1, term.tokens.length - 1, info);
  }
};
const markTypedParameters = (tokens, start, end, info) => {
  for (const segment of splitTopLevel(tokens, start, end, separators.comma)) {
    const colon = findTopLevelText(tokens, segment.start, segment.end, ":");
    const first = tokens
      .slice(segment.start, colon < 0 ? segment.end : colon)
      .find((token) => token.kind === "name");
    if (first) mark(info, first.index, "parameter", ["declaration"], 90);
    if (colon >= 0) {
      markTypeTokens(tokens, colon + 1, segment.end, info);
    }
  }
};
const markTypeTerms = (terms, info, skip) => {
  for (const term of terms) {
    markTypeTokens(term.tokens, 0, term.tokens.length, info, skip);
  }
};
const markTypeTokens = (tokens, start, end, info, skip = new Set()) => {
  for (let i = start; i < end; i += 1) {
    const token = tokens[i];
    if (!token || token.kind !== "name" || skip.has(token.index)) {
      continue;
    }
    const name = lastQualifiedSegment(token.text);
    if (
      primitiveTypes.has(name) || info.types.has(name) || info.effects.has(name)
    ) {
      mark(
        info,
        token.index,
        "type",
        info.effects.has(name) ? ["effect"] : [],
        80,
      );
    } else if (info.shapes.has(name)) {
      mark(info, token.index, "shape", [], 80);
    } else {
      mark(info, token.index, "typeParameter", [], 80);
    }
  }
};
const isAnonymousFunctionValue = (tokens, start, end) => {
  const semicolon = findTopLevelText(tokens, start, end, ";");
  let expressionEnd = semicolon < 0 ? end : semicolon;
  while (
    tokens[start]?.text === "(" &&
    findMatching(tokens, start) === expressionEnd - 1
  ) {
    start += 1;
    expressionEnd -= 1;
  }
  return tokens[start]?.text === "{" &&
    findMatching(tokens, start) === expressionEnd - 1;
};
const isFunctionPosition = (tokens, index) => {
  const token = tokens[index];
  const prev = tokens[index - 1];
  if (!token || token.kind !== "name") {
    return false;
  }
  if (
    prev?.text === "$" ||
    (prev?.text === "(" && tokens[index - 2]?.text === "$")
  ) return true;
  const firstAtom = previousAtomStart(tokens, index);
  if (firstAtom < 0) return false;
  const boundary = tokens[firstAtom - 1];
  if (!boundary) return true;
  if (
    boundary.text === "$" ||
    (boundary.text === "(" && tokens[firstAtom - 2]?.text === "$")
  ) {
    return false;
  }
  if (applicationStartTexts.has(boundary.text) || boundary.kind === "keyword") {
    return true;
  }
  const dollarFunction = previousAtomStart(tokens, firstAtom);
  return dollarFunction >= 0 && tokens[dollarFunction - 1]?.text === "$";
};
const previousAtomStart = (tokens, before) => {
  const end = before - 1;
  const token = tokens[end];
  if (!token) return -1;
  if (isClose(token.text)) return matchingOpen(tokens, end);
  return isSimpleAtom(token) ? end : -1;
};
const matchingOpen = (tokens, closeIndex) => {
  const close = tokens[closeIndex].text;
  const open = openForClose.get(close);
  let depth = 0;
  for (let index = closeIndex; index >= 0; index -= 1) {
    if (tokens[index].text === close) depth += 1;
    if (tokens[index].text === open && --depth === 0) return index;
  }
  return -1;
};
const isSimpleAtom = (token) => {
  return ["name", "number", "string", "character"].includes(token.kind);
};
const isTypePosition = (tokens, index) => {
  const prev = tokens[index - 1];
  return prev && [":", "!", "→"].includes(prev.text);
};
const isOpen = (text) => {
  return closeForOpen.has(text);
};
const isClose = (text) => {
  return openForClose.has(text);
};
const matchingClose = (text) => {
  return closeForOpen.get(text);
};
const lastQualifiedSegment = (name) => {
  const at = name.lastIndexOf("@");
  return at >= 0 ? name.slice(at + 1) : name;
};
const languageNames = {
  keywords: [...keywords],
  primitiveTypes: [...primitiveTypes],
};
export {
  analyze as analyzeTokens,
  buildSemanticRanges,
  languageNames,
  semanticRole,
  tokenize,
  tokenModifiers,
  tokenTypes,
};
