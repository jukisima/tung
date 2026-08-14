const { analyzeTokens, languageNames, semanticRole, tokenize } = require("./semantic");

const declarationKeywords = new Set(["let", "let-ilk", "choose", "deed", "shape", "fill", "class"]);
const ownedKinds = new Set(["choose", "deed", "shape", "class"]);
const localRoles = new Set(["parameter", "typeParameter"]);

const keywordHelp = {
  bring: "bring a tung file into scope; imported names may be qualified with `namespace@name`.",
  show: "publish a declaration or re-export a visible term.",
  "show-ilk": "re-export a visible type.",
  graith: "state the shapes required by a declaration.",
  shape: "declare a type class and its members.",
  fill: "provide evidence and member definitions for a shape.",
  deed: "declare an algebraic effect and its operations.",
  try: "handle effect operations for an expression.",
  resume: "continue the handled computation from an operation clause.",
  match: "match one or more values against exhaustive pattern rows.",
  let: "bind a value or curried function.",
  choose: "declare an algebraic data type.",
  "let-ilk": "declare a type alias.",
  return: "handle the returned value of a `try` expression."
};

function analyzeDocument(text, uri = "") {
  const tokens = tokenize(text);
  const semantic = analyzeTokens(tokens);
  const roles = new Map(tokens.map((token) => [token.index, semanticRole(token, tokens, semantic)]).filter(([, role]) => role));
  const depths = tokenDepths(tokens);
  const pairs = bracketPairs(tokens);
  const regions = declarationRegions(text, tokens, depths, pairs);
  const imports = collectImports(tokens, depths);
  const reexports = collectReexports(tokens, depths);
  const definitions = semanticDefinitions(text, tokens, semantic, depths, regions);
  definitions.push(...patternDefinitions(tokens, semantic, depths));
  definitions.forEach((definition, id) => {
    definition.id = `${uri}:${definition.token.offset}:${id}`;
    definition.uri = uri;
  });

  return {
    uri,
    text,
    tokens,
    semantic,
    roles,
    depths,
    pairs,
    regions,
    imports,
    reexports,
    fills: collectFills(tokens, semantic, regions),
    definitions,
    occurrences: tokens.filter((token) => token.kind === "name"),
    folds: [...pairs.entries()]
      .map(([open, close]) => ({ startLine: tokens[open].line, endLine: tokens[close].line - 1 }))
      .filter(({ startLine, endLine }) => endLine > startLine)
  };
}

function collectFills(tokens, semantic, regions) {
  return regions.filter(({ kind }) => kind === "fill").flatMap((region) => {
    for (let index = region.headerEnd - 1; index > region.startIndex; index -= 1) {
      const role = semantic.semantic.get(index);
      if (role?.type === "shape" && tokens[index]?.kind === "name") {
        return [{ shapeName: lastQualifiedSegment(tokens[index].text), uri: "", range: tokenRange(tokens[index]), token: tokens[index] }];
      }
    }
    return [];
  });
}

function tokenDepths(tokens) {
  const depths = [];
  let depth = 0;
  for (const token of tokens) {
    depths.push(depth);
    if (isOpen(token.text)) depth += 1;
    if (isClose(token.text)) depth = Math.max(0, depth - 1);
  }
  return depths;
}

function bracketPairs(tokens) {
  const pairs = new Map();
  const stack = [];
  for (const token of tokens) {
    if (isOpen(token.text)) {
      stack.push(token.index);
    } else if (isClose(token.text)) {
      const open = stack.at(-1);
      if (open !== undefined && matchingClose(tokens[open].text) === token.text) {
        stack.pop();
        pairs.set(open, token.index);
      }
    }
  }
  return pairs;
}

function declarationRegions(text, tokens, depths, pairs) {
  const regions = [];
  for (const token of tokens) {
    if (token.kind !== "keyword" || !declarationKeywords.has(token.text)) continue;
    if (token.text === "class" && tokens[token.index + 1]?.text !== "foreign") continue;

    const baseDepth = depths[token.index];
    const open = ownedKinds.has(token.text) ? findAtDepth(tokens, depths, token.index + 1, "{", baseDepth) : -1;
    const endIndex = open >= 0 && pairs.has(open)
      ? pairs.get(open)
      : findDeclarationEnd(tokens, depths, token.index + 1, baseDepth);
    const endToken = tokens[endIndex] || tokens.at(-1) || token;
    const headerEnd = findHeaderEnd(tokens, depths, token.index + 1, endIndex + 1, baseDepth);
    const shown = tokens[token.index - 1]?.text === "show";
    regions.push({
      kind: token.text,
      token,
      startOffset: token.offset,
      endOffset: endToken.endOffset,
      startIndex: token.index,
      endIndex,
      headerEnd,
      depth: baseDepth,
      shown,
      range: offsetsRange(text, token.offset, endToken.endOffset)
    });
  }
  return regions;
}

function findDeclarationEnd(tokens, depths, start, baseDepth) {
  for (let index = start; index < tokens.length; index += 1) {
    if (tokens[index].text === ";" && depths[index] === baseDepth) return index;
    if (isClose(tokens[index].text) && depths[index] === baseDepth) return Math.max(start - 1, index - 1);
    if (index > start && depths[index] === baseDepth && tokens[index].kind === "keyword" && declarationKeywords.has(tokens[index].text)) {
      return index - 1;
    }
  }
  return tokens.length - 1;
}

function findHeaderEnd(tokens, depths, start, end, baseDepth) {
  for (let index = start; index < end; index += 1) {
    if (depths[index] === baseDepth && ["=", "{", ";"].includes(tokens[index].text)) return index;
  }
  return Math.max(start, end - 1);
}

function collectImports(tokens, depths) {
  const imports = [];
  for (const token of tokens) {
    if (token.text !== "bring") continue;
    const end = findText(tokens, token.index + 1, ";");
    if (end < 0) continue;
    const pathTokens = tokens.slice(token.index + 1, end);
    const importPath = pathTokens.map(({ text }) => text).join("");
    if (!importPath) continue;
    imports.push({
      path: importPath,
      namespace: importPath.split(".")[0],
      exported: tokens[token.index - 1]?.text === "show",
      range: tokenRange(pathTokens[0], pathTokens.at(-1)),
      depth: depths[token.index]
    });
  }
  return imports;
}

function collectReexports(tokens, depths) {
  const reexports = [];
  for (const token of tokens) {
    if (!["show", "show-ilk"].includes(token.text) || depths[token.index] !== 0) continue;
    const next = tokens[token.index + 1];
    if (!next || next.kind !== "name") continue;
    for (let index = next.index; index < tokens.length && tokens[index].text !== ";"; index += 1) {
      if (tokens[index].kind === "name") reexports.push({ name: tokens[index].text, kind: token.text === "show-ilk" ? "type" : "term" });
    }
  }
  return reexports;
}

function semanticDefinitions(text, tokens, semantic, depths, regions) {
  const definitions = [];
  for (const [index, role] of semantic.semantic.entries()) {
    if (!role.modifiers.includes("declaration")) continue;
    const token = tokens[index];
    if (!token || token.kind !== "name" || token.text === "_" || role.type === "namespace") continue;
    const region = smallestRegion(regions, token.offset);
    const owner = region && ownedKinds.has(region.kind) && token.index > region.headerEnd
      ? region
      : enclosingOwner(regions, region, token.offset);
    const local = localRoles.has(role.type);
    const primary = region && token.index > region.startIndex && token.index <= region.headerEnd;
    const exported = !local && Boolean(
      region?.shown && primary || owner?.shown && ["enumMember", "method", "function"].includes(role.type)
    );
    const scope = definitionScope(tokens, region, local);
    definitions.push({
      name: token.text,
      bareName: lastQualifiedSegment(token.text),
      role: role.type,
      modifiers: role.modifiers,
      token,
      range: tokenRange(token),
      selectionRange: tokenRange(token),
      scopeStart: scope.start,
      scopeEnd: scope.end,
      exported,
      local,
      topLevel: depths[token.index] === 0,
      detail: declarationDetail(text, tokens, token, region, role.type),
      containerName: ownerName(tokens, semantic, owner)
    });
  }
  return uniqueDefinitions(definitions);
}

function patternDefinitions(tokens, semantic, depths) {
  const definitions = [];
  for (const pipe of tokens) {
    if (pipe.text !== "|") continue;
    const depth = depths[pipe.index];
    const start = patternArmStart(tokens, depths, pipe.index, depth);
    const endBoundary = patternArmEnd(tokens, depths, pipe.index, depth);
    const endOffset = tokens[endBoundary]?.offset ?? tokens.at(-1)?.endOffset ?? pipe.endOffset;
    for (const token of tokens.slice(start, pipe.index)) {
      const role = semantic.semantic.get(token.index);
      if (token.kind !== "name" || role?.type !== "parameter" || !role.modifiers.includes("declaration")) continue;
      const name = lastQualifiedSegment(token.text);
      definitions.push({
        name: token.text,
        bareName: name,
        role: "parameter",
        modifiers: ["declaration"],
        token,
        range: tokenRange(token),
        selectionRange: tokenRange(token),
        scopeStart: pipe.endOffset,
        scopeEnd: endOffset,
        exported: false,
        local: true,
        topLevel: false,
        detail: `parameter ${name}`
      });
    }
  }
  return uniqueDefinitions(definitions);
}

function patternArmStart(tokens, depths, pipeIndex, depth) {
  for (let index = pipeIndex - 1; index >= 0; index -= 1) {
    if (tokens[index].text === "|" && depths[index] === depth) {
      for (let body = index + 1; body < pipeIndex; body += 1) {
        if (tokens[body].text === "," && depths[body] === depth) return body + 1;
      }
      return index + 1;
    }
    if (tokens[index].text === "{" && depths[index] === depth - 1) return index + 1;
    if (tokens[index].text === ";" && depths[index] === depth) return index + 1;
  }
  return 0;
}

function patternArmEnd(tokens, depths, pipeIndex, depth) {
  for (let index = pipeIndex + 1; index < tokens.length; index += 1) {
    if (tokens[index].text === "," && depths[index] === depth) return index;
    if (tokens[index].text === "}" && depths[index] === depth) return index;
  }
  return tokens.length;
}

function definitionScope(tokens, region, local) {
  const documentEnd = tokens.at(-1)?.endOffset || 0;
  if (!local) return { start: 0, end: documentEnd };
  if (!region) return { start: 0, end: documentEnd };
  const equals = findText(tokens, region.startIndex, "=", region.endIndex + 1);
  return { start: equals >= 0 ? tokens[equals].endOffset : region.startOffset, end: region.endOffset };
}

function declarationDetail(text, tokens, token, region, role) {
  if (!region) return `${role} ${lastQualifiedSegment(token.text)}`;
  const end = tokens[region.headerEnd]?.offset ?? token.endOffset;
  const header = text.slice(region.startOffset, end).replace(/\s+/g, " ").trim();
  return header || `${role} ${lastQualifiedSegment(token.text)}`;
}

function smallestRegion(regions, offset) {
  return regions
    .filter((region) => region.startOffset <= offset && offset <= region.endOffset)
    .sort((a, b) => (a.endOffset - a.startOffset) - (b.endOffset - b.startOffset))[0];
}

function enclosingOwner(regions, region, offset) {
  return regions
    .filter((candidate) => ownedKinds.has(candidate.kind) && candidate.startOffset <= offset && offset <= candidate.endOffset && candidate !== region)
    .sort((a, b) => (a.endOffset - a.startOffset) - (b.endOffset - b.startOffset))[0];
}

function ownerName(tokens, semantic, owner) {
  if (!owner) return undefined;
  for (let index = owner.startIndex + 1; index <= owner.headerEnd; index += 1) {
    const role = semantic.semantic.get(index);
    if (role?.modifiers.includes("declaration") && !localRoles.has(role.type)) return tokens[index].text;
  }
  return undefined;
}

function findDefinition(model, token, offset = token?.offset) {
  if (!token || token.kind !== "name") return undefined;
  const name = lastQualifiedSegment(token.text);
  return model.definitions
    .filter((definition) => definition.bareName === name && definition.scopeStart <= offset && offset <= definition.scopeEnd)
    .sort((a, b) => {
      const aWidth = a.scopeEnd - a.scopeStart;
      const bWidth = b.scopeEnd - b.scopeStart;
      if (aWidth !== bWidth) return aWidth - bWidth;
      return Math.abs(offset - a.token.offset) - Math.abs(offset - b.token.offset);
    })[0];
}

function tokenAtPosition(model, position) {
  return model.tokens.find((token) => token.line === position.line && token.char <= position.character && token.endChar > position.character);
}

function tokenRange(first, last = first) {
  return {
    start: { line: first.line, character: first.char },
    end: { line: last.endLine, character: last.endChar }
  };
}

function nameRange(token) {
  const split = token.text.lastIndexOf("@");
  const delta = split < 0 ? 0 : split + 1;
  return {
    start: { line: token.line, character: token.char + delta },
    end: { line: token.endLine, character: token.endChar }
  };
}

function offsetsRange(text, start, end) {
  return { start: positionAt(text, start), end: positionAt(text, end) };
}

function positionAt(text, offset) {
  const before = text.slice(0, offset);
  const lines = before.split(/\r\n|\r|\n/);
  return { line: lines.length - 1, character: lines.at(-1).length };
}

function findAtDepth(tokens, depths, start, text, depth) {
  for (let index = start; index < tokens.length; index += 1) {
    if (depths[index] === depth && tokens[index].text === text) return index;
    if (depths[index] === depth && tokens[index].text === ";") return -1;
  }
  return -1;
}

function findText(tokens, start, text, end = tokens.length) {
  for (let index = start; index < end; index += 1) {
    if (tokens[index].text === text) return index;
  }
  return -1;
}

function uniqueDefinitions(definitions) {
  const seen = new Set();
  return definitions.filter((definition) => {
    const key = `${definition.token.offset}:${definition.role}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function isOpen(text) {
  return ["(", "{", "["].includes(text);
}

function isClose(text) {
  return [")", "}", "]"].includes(text);
}

function matchingClose(text) {
  return { "(": ")", "{": "}", "[": "]" }[text];
}

function lastQualifiedSegment(name) {
  return name.slice(name.lastIndexOf("@") + 1);
}

module.exports = {
  analyzeDocument,
  findDefinition,
  keywordHelp,
  languageNames,
  lastQualifiedSegment,
  nameRange,
  tokenAtPosition,
  tokenRange,
  _test: { bracketPairs, collectImports, declarationRegions, patternDefinitions, tokenDepths }
};
