// tolerant structural model for incomplete buffers. it joineþ semantic roles,
// declaration regions, docs, imports, and scopes without attempting type checks.
import { analyzeTokens, semanticRole } from "./semantic.ts";
import {
  bracketPairs,
  declarationKeywords,
  findFileDeclarationBoundary,
  findInstanceHeader,
  findMatching,
  lastQualifiedSegment,
  patternArmRegions,
  splitTopLevel,
  tokenDepths,
  tokenize,
  useNamespace,
  useParts,
} from "./syntax.ts";
const ownedKinds = new Set(["ilk", "deed", "flock"]);
const localRoles = new Set(["parameter", "typeParameter"]);
const keywordHelp = {
  use: "import a tung file; its last path segment is the default namespace, and an optional alias overrideeþ it.",
  show: "publish a declaration or re-export a visible term.",
  "show-ilk": "re-export a visible type.",
  yield:
    "introduce a block result or handle the normal result of a `try` expression.",
  flock: "declare a flock and its members.",
  fremmed:
    "bind an annotated file-level let to the host function named by a preceding text key.",
  bizen: "provide evidence and member definitions for a flock.",
  byzen: "require a flock instance; its arguments determine parameter kinds.",
  law: "state a type-checked equation required of a flock; equivalence is not proved.",
  deed: "declare an algebraic effect and its operations.",
  try: "handle effect operations for an expression.",
  eftgin: "continue the handled computation from an operation clause.",
  match: "match one or more values against exhaustive pattern rows.",
  let: "bind a value, curried function, or type alias.",
  ilk: "declare an algebraic data type.",
};
const analyzeDocument = (text, uri = "") => {
  const tokens = tokenize(text);
  const semantic = analyzeTokens(tokens);
  const roles = new Map(
    tokens.flatMap((token) => {
      const role = semanticRole(token, tokens, semantic);
      return role ? [[token.index, role] as const] : [];
    }),
  );
  const depths = tokenDepths(tokens);
  const pairs = bracketPairs(tokens);
  const regions = declarationRegions(tokens, depths, pairs);
  const docs = collectDocComments(text);
  attachDocs(regions, docs, tokens);
  const imports = collectImports(tokens, depths);
  const reexports = collectReexports(tokens, depths);
  const definitions = uniqueDefinitions([
    ...patternDefinitions(tokens, semantic),
    ...semanticDefinitions(text, tokens, semantic, depths, regions),
  ]);
  collectRequirementDefinitions(tokens, roles, semantic, regions, definitions);
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
    instances: collectInstances(tokens, semantic, regions),
    definitions,
    occurrences: tokens.filter((token) => token.kind === "name"),
    folds: [...pairs.entries()]
      .map(([open, close]) => ({
        startLine: tokens[open].line,
        endLine: tokens[close].line - 1,
      }))
      .filter(({ startLine, endLine }) => endLine > startLine),
  };
};
const collectInstances = (tokens, semantic, regions) => {
  return regions
    .filter(({ kind }) => kind === "bizen")
    .flatMap((region) => {
      for (
        let index = region.headerEnd - 1;
        index > region.startIndex;
        index -= 1
      ) {
        const role = semantic.semantic.get(index);
        if (role?.type === "class" && tokens[index]?.kind === "name") {
          return [
            {
              className: lastQualifiedSegment(tokens[index].text),
              uri: "",
              range: tokenRange(tokens[index]),
              token: tokens[index],
            },
          ];
        }
      }
      return [];
    });
};
const collectDocComments = (text) => {
  const docs = [];
  const linePattern = /^[ \t]*##[ \t]?(.*)$/;
  let offset = 0;
  const lines = text.split(/(\r\n|\r|\n)/);
  for (let index = 0; index < lines.length; index += 2) {
    const line = lines[index];
    const newline = lines[index + 1] || "";
    const match = line.match(linePattern);
    if (match) {
      const startOffset = offset;
      const parts = [match[1]];
      let endOffset = offset + line.length + newline.length;
      offset = endOffset;
      while (index + 2 < lines.length) {
        const nextLine = lines[index + 2];
        const nextNewline = lines[index + 3] || "";
        const nextMatch = nextLine.match(linePattern);
        if (!nextMatch) break;
        parts.push(nextMatch[1]);
        index += 2;
        endOffset += nextLine.length + nextNewline.length;
        offset = endOffset;
      }
      docs.push({
        startOffset,
        endOffset,
        text: cleanDoc(parts.join("\n")),
      });
      continue;
    }
    offset += line.length + newline.length;
  }
  for (const match of text.matchAll(/\/\*\*([\s\S]*?)\*\//g)) {
    docs.push({
      startOffset: match.index,
      endOffset: match.index + match[0].length,
      text: cleanBlockDoc(match[1]),
    });
  }
  return docs
    .filter(({ text }) => text)
    .sort((a, b) => a.startOffset - b.startOffset);
};
const cleanBlockDoc = (text) => {
  return cleanDoc(
    text
      .split(/\r?\n/)
      .map((line) => line.replace(/^\s*\* ?/, ""))
      .join("\n"),
  );
};
const cleanDoc = (text) => {
  return text
    .split(/\r?\n/)
    .map((line) => line.trim())
    .join("\n")
    .replace(/^\n+|\n+$/g, "");
};
const attachDocs = (regions, docs, tokens) => {
  for (const region of regions) {
    const doc = [...docs]
      .reverse()
      .find(
        (candidate) =>
          candidate.endOffset <= region.startOffset &&
          !hasDeclarationBetween(
            tokens,
            candidate.endOffset,
            region.startOffset,
          ),
      );
    if (doc) region.doc = doc.text;
  }
};
const hasDeclarationBetween = (tokens, startOffset, endOffset) => {
  return tokens.some(
    (token) =>
      token.offset >= startOffset &&
      token.offset < endOffset &&
      token.kind === "keyword" &&
      declarationKeywords.has(token.text),
  );
};
// regions supply ownership and source extents for symbols, folding, docs, and
// local scope. unmatched delimiters fall back to the document end.
const declarationRegions = (tokens, depths, pairs) => {
  const regions = [];
  for (const token of tokens) {
    if (token.kind !== "keyword" || !declarationKeywords.has(token.text)) {
      continue;
    }
    const baseDepth = depths[token.index];
    const open =
      token.text === "bizen"
        ? (findInstanceHeader(tokens, token.index + 1)?.body ?? -1)
        : ownedKinds.has(token.text)
          ? findAtDepth(tokens, depths, token.index + 1, "{", baseDepth)
          : -1;
    const boundary = findFileDeclarationBoundary(
      tokens,
      token.index + 1,
      depths,
      baseDepth,
    );
    const endIndex =
      open >= 0 && pairs.has(open)
        ? pairs.get(open)
        : Math.max(token.index, boundary - 1);
    const endToken = tokens[endIndex] || tokens.at(-1) || token;
    const headerEnd =
      token.text === "bizen" && open >= 0
        ? open
        : findHeaderEnd(
            tokens,
            depths,
            token.index + 1,
            endIndex + 1,
            baseDepth,
          );
    const shown = tokens[token.index - 1]?.text === "show";
    regions.push({
      kind: token.text,
      startOffset: token.offset,
      endOffset: endToken.endOffset,
      startIndex: token.index,
      endIndex,
      headerEnd,
      depth: baseDepth,
      shown,
      range: tokenRange(token, endToken),
    });
  }
  return regions;
};
const findHeaderEnd = (tokens, depths, start, end, baseDepth) => {
  for (let index = start; index < end; index += 1) {
    if (depths[index] === baseDepth && tokens[index].text === "[") {
      const close = findMatching(tokens, index);
      if (close >= 0) return close + 1;
    }
    if (depths[index] === baseDepth && tokens[index].text === "{") return index;
  }
  return Math.max(start, end - 1);
};
const collectImports = (tokens, depths) => {
  const imports = [];
  for (const token of tokens) {
    if (token.text !== "use") continue;
    const { pathTokens, aliasToken } = useParts(tokens, token.index, depths);
    const importPath = pathTokens.map(({ text }) => text).join("");
    if (!importPath) continue;
    const namespace = useNamespace(importPath, aliasToken?.text);
    imports.push({
      path: importPath,
      namespace,
      alias: aliasToken?.text,
      exported: tokens[token.index - 1]?.text === "show",
      range: tokenRange(pathTokens[0], pathTokens.at(-1)),
    });
  }
  return imports;
};
const collectReexports = (tokens, depths) => {
  const reexports = [];
  for (const token of tokens) {
    if (!["show", "show-ilk"].includes(token.text) || depths[token.index] !== 0)
      continue;
    const next = tokens[token.index + 1];
    if (!next || next.kind !== "name") continue;
    const end = findFileDeclarationBoundary(
      tokens,
      next.index,
      depths,
      depths[token.index],
    );
    for (let index = next.index; index < end; index += 1) {
      if (tokens[index].kind === "name") {
        reexports.push({
          name: tokens[index].text,
          kind: token.text === "show-ilk" ? "type" : "term",
        });
      }
    }
  }
  return reexports;
};
const semanticDefinitions = (text, tokens, semantic, depths, regions) => {
  const definitions = [];
  for (const [index, role] of semantic.semantic.entries()) {
    if (!role.modifiers.includes("declaration")) continue;
    const token = tokens[index];
    if (
      !token ||
      token.kind !== "name" ||
      token.text === "_" ||
      role.type === "namespace"
    ) {
      continue;
    }
    const region = smallestRegion(regions, token.offset);
    const owner =
      region && ownedKinds.has(region.kind) && token.index > region.headerEnd
        ? region
        : enclosingOwner(regions, region, token.offset);
    const local = localRoles.has(role.type);
    const primary =
      region &&
      token.index > region.startIndex &&
      token.index <= region.headerEnd;
    const exported =
      !local &&
      Boolean(
        (region?.shown && primary) ||
        (owner?.shown &&
          ["enumMember", "method", "function"].includes(role.type)),
      );
    const scope = definitionScope(tokens, region, role.type);
    definitions.push({
      name: token.text,
      bareName: lastQualifiedSegment(token.text),
      role: role.type,
      modifiers: role.modifiers,
      callableConstructor:
        role.type === "enumMember" &&
        semantic.callableConstructors.has(lastQualifiedSegment(token.text)),
      token,
      range: tokenRange(token),
      selectionRange: tokenRange(token),
      scopeStart: scope.start,
      scopeEnd: scope.end,
      exported,
      local,
      topLevel: depths[token.index] === 0,
      detail: declarationDetail(text, tokens, token, region, role.type),
      documentation: region?.doc,
      containerName: ownerName(tokens, semantic, owner),
    });
  }
  return definitions;
};
const patternDefinitions = (tokens, semantic) => {
  const definitions = [];
  for (const { patternStart, pipe, bodyEnd } of patternArmRegions(tokens)) {
    const endOffset =
      tokens[bodyEnd]?.offset ?? tokens.at(-1)?.endOffset ?? pipe.endOffset;
    for (const token of tokens.slice(patternStart, pipe.index)) {
      const role = semantic.semantic.get(token.index);
      if (
        token.kind !== "name" ||
        role?.type !== "parameter" ||
        !role.modifiers.includes("declaration")
      ) {
        continue;
      }
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
        detail: `parameter ${name}`,
      });
    }
  }
  return definitions;
};
const definitionScope = (tokens, region, role) => {
  const documentEnd = tokens.at(-1)?.endOffset || 0;
  if (!localRoles.has(role)) return { start: 0, end: documentEnd };
  if (!region) return { start: 0, end: documentEnd };
  return {
    start:
      role === "typeParameter"
        ? region.startOffset
        : (tokens[region.headerEnd]?.offset ?? region.startOffset),
    end: region.endOffset,
  };
};
// requirements introduce syntactic binder candidates. workspace lookup resolveþ
// imported concrete types before these candidates; no kind checking is needed.
const collectRequirementDefinitions = (
  tokens,
  roles,
  semantic,
  regions,
  definitions,
) => {
  const byName = new Map();
  for (const definition of definitions) {
    const entries = byName.get(definition.name) || [];
    entries.push(definition);
    byName.set(definition.name, entries);
  }
  for (const region of regions) {
    let open = region.startIndex + 1;
    while (open < region.headerEnd && tokens[open]?.text !== "[") open += 1;
    if (open >= region.headerEnd) continue;
    const close = findMatching(tokens, open);
    if (close < 0) continue;
    for (const { start, end } of splitTopLevel(
      tokens,
      open + 1,
      close,
      new Set([",", ";"]),
    )) {
      if (tokens[start]?.text !== "byzen") continue;
      for (let next = start + 1; next < end; next += 1) {
        const token = tokens[next];
        if (
          token.kind !== "name" ||
          token.text.includes("~") ||
          roles.get(token.index)?.type !== "typeParameter" ||
          semantic.types.has(token.text) ||
          semantic.effects.has(token.text) ||
          (byName.get(token.text) || []).some(
            (definition) =>
              definition.name === token.text &&
              ["typeParameter", "type", "class"].includes(definition.role) &&
              definition.scopeStart <= token.offset &&
              token.offset <= definition.scopeEnd,
          )
        )
          continue;
        roles.set(token.index, {
          type: "typeParameter",
          modifiers: ["declaration"],
        });
        const definition = {
          name: token.text,
          bareName: token.text,
          role: "typeParameter",
          modifiers: ["declaration"],
          token,
          range: tokenRange(token),
          selectionRange: tokenRange(token),
          scopeStart: region.startOffset,
          scopeEnd: region.endOffset,
          local: true,
          exported: false,
          topLevel: false,
          inferredRequirement: true,
          detail: "type parameter from byzen",
        };
        definitions.push(definition);
        const entries = byName.get(token.text) || [];
        entries.push(definition);
        byName.set(token.text, entries);
      }
    }
  }
};
const declarationDetail = (text, tokens, token, region, role) => {
  if (!region) return `${role} ${lastQualifiedSegment(token.text)}`;
  const end = tokens[region.headerEnd]?.offset ?? token.endOffset;
  const header = text
    .slice(region.startOffset, end)
    .replace(/\s+/g, " ")
    .trim();
  return header || `${role} ${lastQualifiedSegment(token.text)}`;
};
const smallestRegion = (regions, offset, kind = undefined) => {
  return regions
    .filter(
      (region) =>
        (!kind || region.kind === kind) &&
        region.startOffset <= offset &&
        offset <= region.endOffset,
    )
    .sort(
      (a, b) => a.endOffset - a.startOffset - (b.endOffset - b.startOffset),
    )[0];
};
const enclosingOwner = (regions, region, offset) => {
  return regions
    .filter(
      (candidate) =>
        ownedKinds.has(candidate.kind) &&
        candidate.startOffset <= offset &&
        offset <= candidate.endOffset &&
        candidate !== region,
    )
    .sort(
      (a, b) => a.endOffset - a.startOffset - (b.endOffset - b.startOffset),
    )[0];
};
const ownerName = (tokens, semantic, owner) => {
  if (!owner) return undefined;
  for (let index = owner.startIndex + 1; index <= owner.headerEnd; index += 1) {
    const role = semantic.semantic.get(index);
    if (role?.modifiers.includes("declaration") && !localRoles.has(role.type)) {
      return tokens[index].text;
    }
  }
  return undefined;
};
const findDefinition = (model, token, offset = token?.offset) => {
  if (!token || token.kind !== "name") return undefined;
  const name = lastQualifiedSegment(token.text);
  return model.definitions
    .filter(
      (definition) =>
        definition.bareName === name &&
        definition.scopeStart <= offset &&
        offset <= definition.scopeEnd,
    )
    .sort((a, b) => {
      const aWidth = a.scopeEnd - a.scopeStart;
      const bWidth = b.scopeEnd - b.scopeStart;
      if (aWidth !== bWidth) return aWidth - bWidth;
      return (
        Math.abs(offset - a.token.offset) - Math.abs(offset - b.token.offset)
      );
    })[0];
};
const tokenAtPosition = (model, position) => {
  return model.tokens.find(
    (token) =>
      token.line === position.line &&
      token.char <= position.character &&
      token.endChar > position.character,
  );
};
const tokenRange = (first, last = first) => {
  return {
    start: { line: first.line, character: first.char },
    end: { line: last.endLine, character: last.endChar },
  };
};
const nameRange = (token) => {
  const split = token.text.indexOf("~");
  const delta = split < 0 ? 0 : split + 1;
  return {
    start: { line: token.line, character: token.char + delta },
    end: { line: token.endLine, character: token.endChar },
  };
};
const findAtDepth = (tokens, depths, start, text, depth) => {
  for (let index = start; index < tokens.length; index += 1) {
    if (depths[index] === depth && tokens[index].text === text) {
      return index;
    }
  }
  return -1;
};
const uniqueDefinitions = (definitions) => {
  const seen = new Set();
  return definitions.filter((definition) => {
    const key = `${definition.token.offset}:${definition.role}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};
export {
  analyzeDocument,
  findDefinition,
  keywordHelp,
  nameRange,
  smallestRegion,
  tokenAtPosition,
  tokenRange,
};
