// tolerant structural model for incomplete buffers. it joineþ semantic roles,
// declaration regions, docs, imports, and scopes without attempting type checks.
import { analyzeTokens, semanticRole } from "./semantic.ts";
import {
  bracketPairs,
  bringNamespace,
  bringParts,
  declarationKeywords,
  findFileDeclarationBoundary,
  lastQualifiedSegment,
  patternArmRegions,
  tokenDepths,
  tokenize,
} from "./syntax.ts";
const ownedKinds = new Set(["kin", "deed", "shape"]);
const localRoles = new Set(["parameter", "typeParameter"]);
const keywordHelp = {
  bring:
    "bring a tung file into scope; its last path segment is the default namespace, and a third element overrideeþ it.",
  show: "publish a declaration or re-export a visible term.",
  "show-ilk": "re-export a visible type.",
  graiþ: "state the shapes required by a declaration.",
  yield:
    "introduce a block result or handle the normal result of a `try` expression.",
  shape: "declare a shape and its members.",
  fremmed:
    "bind an annotated file-level let to the host function named by a preceding text key.",
  fill: "provide evidence and member definitions for a shape.",
  law:
    "state a type-checked equation required of a shape; equivalence is not proved.",
  deed: "declare an algebraic effect and its operations.",
  try: "handle effect operations for an expression.",
  eftgin: "continue the handled computation from an operation clause.",
  match: "match one or more values against exhaustive pattern rows.",
  let: "bind a value or curried function.",
  kin: "declare an algebraic data type.",
  "let-ilk": "declare a type alias.",
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
      .map(([open, close]) => ({
        startLine: tokens[open].line,
        endLine: tokens[close].line - 1,
      }))
      .filter(({ startLine, endLine }) => endLine > startLine),
  };
};
const collectFills = (tokens, semantic, regions) => {
  return regions
    .filter(({ kind }) => kind === "fill")
    .flatMap((region) => {
      for (
        let index = region.headerEnd - 1;
        index > region.startIndex;
        index -= 1
      ) {
        const role = semantic.semantic.get(index);
        if (role?.type === "shape" && tokens[index]?.kind === "name") {
          return [
            {
              shapeName: lastQualifiedSegment(tokens[index].text),
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
  return docs.filter(({ text }) => text).sort((a, b) =>
    a.startOffset - b.startOffset
  );
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
    const open = ownedKinds.has(token.text)
      ? findAtDepth(tokens, depths, token.index + 1, "{", baseDepth)
      : -1;
    const boundary = findFileDeclarationBoundary(
      tokens,
      token.index + 1,
      depths,
      baseDepth,
    );
    const endIndex = open >= 0 && pairs.has(open)
      ? pairs.get(open)
      : Math.max(token.index, boundary - 1);
    const endToken = tokens[endIndex] || tokens.at(-1) || token;
    const headerEnd = findHeaderEnd(
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
    if (
      depths[index] === baseDepth &&
      ["=", "{"].includes(tokens[index].text)
    ) return index;
  }
  return Math.max(start, end - 1);
};
const collectImports = (tokens, depths) => {
  const imports = [];
  for (const token of tokens) {
    if (token.text !== "bring") continue;
    const { pathTokens, aliasToken } = bringParts(tokens, token.index, depths);
    const importPath = pathTokens.map(({ text }) => text).join("");
    if (!importPath) continue;
    const namespace = bringNamespace(importPath, aliasToken?.text);
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
    if (
      !["show", "show-ilk"].includes(token.text) ||
      depths[token.index] !== 0
    ) continue;
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
      !token || token.kind !== "name" || token.text === "_" ||
      role.type === "namespace"
    ) {
      continue;
    }
    const region = smallestRegion(regions, token.offset);
    const owner = region && ownedKinds.has(region.kind) &&
        token.index > region.headerEnd
      ? region
      : enclosingOwner(regions, region, token.offset);
    const local = localRoles.has(role.type);
    const primary = region && token.index > region.startIndex &&
      token.index <= region.headerEnd;
    const exported = !local &&
      Boolean(
        (region?.shown && primary) ||
          (owner?.shown &&
            ["enumMember", "method", "function"].includes(
              role.type,
            )),
      );
    const scope = definitionScope(tokens, region, local);
    definitions.push({
      name: token.text,
      bareName: lastQualifiedSegment(token.text),
      role: role.type,
      modifiers: role.modifiers,
      callableConstructor: role.type === "enumMember" &&
        semantic.callableConstructors.has(
          lastQualifiedSegment(token.text),
        ),
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
    const endOffset = tokens[bodyEnd]?.offset ??
      tokens.at(-1)?.endOffset ?? pipe.endOffset;
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
const definitionScope = (tokens, region, local) => {
  const documentEnd = tokens.at(-1)?.endOffset || 0;
  if (!local) return { start: 0, end: documentEnd };
  if (!region) return { start: 0, end: documentEnd };
  const equals = findText(
    tokens,
    region.startIndex,
    "=",
    region.endIndex + 1,
  );
  return {
    start: equals >= 0 ? tokens[equals].endOffset : region.startOffset,
    end: region.endOffset,
  };
};
const declarationDetail = (text, tokens, token, region, role) => {
  if (!region) return `${role} ${lastQualifiedSegment(token.text)}`;
  const end = tokens[region.headerEnd]?.offset ?? token.endOffset;
  const header = text.slice(region.startOffset, end).replace(/\s+/g, " ")
    .trim();
  return header || `${role} ${lastQualifiedSegment(token.text)}`;
};
const smallestRegion = (regions, offset, kind = undefined) => {
  return regions
    .filter((region) =>
      (!kind || region.kind === kind) &&
      region.startOffset <= offset && offset <= region.endOffset
    )
    .sort((a, b) =>
      a.endOffset - a.startOffset - (b.endOffset - b.startOffset)
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
    .sort((a, b) =>
      a.endOffset - a.startOffset - (b.endOffset - b.startOffset)
    )[0];
};
const ownerName = (tokens, semantic, owner) => {
  if (!owner) return undefined;
  for (
    let index = owner.startIndex + 1;
    index <= owner.headerEnd;
    index += 1
  ) {
    const role = semantic.semantic.get(index);
    if (
      role?.modifiers.includes("declaration") &&
      !localRoles.has(role.type)
    ) {
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
      return Math.abs(offset - a.token.offset) -
        Math.abs(offset - b.token.offset);
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
  const split = token.text.lastIndexOf("@");
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
const findText = (tokens, start, text, end = tokens.length) => {
  for (let index = start; index < end; index += 1) {
    if (tokens[index].text === text) return index;
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
