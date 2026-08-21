// fast structural formatter: it tracketh delimiters and continuations without invoking the compiler.
interface FormatOptions {
  insertSpaces?: boolean;
  tabSize?: number;
}
const formatDocument = (text, options: FormatOptions = {}) => {
  const indentUnit = indent(options);
  const lines = text.split(/\r?\n/);
  let depth = 0;
  let inString = false;
  let inBlockComment = false;
  let continuation = 0;
  const continuationBlocks = [];
  const formatted = [];
  for (const line of lines) {
    const content = line.trim();
    if (inBlockComment) {
      const balance = delimiterBalance(line, inString, inBlockComment);
      inString = balance.inString;
      inBlockComment = balance.inBlockComment;
      continuation = 0;
      formatted.push(line);
      continue;
    }
    if (!content) {
      formatted.push("");
      continue;
    }
    const closes = leadingClosures(content);
    const lineContinuation = closes > 0 ? 0 : continuation;
    const lineDepth = Math.max(0, depth - closes + lineContinuation);
    const balance = delimiterBalance(content, inString, inBlockComment);
    inString = balance.inString;
    inBlockComment = balance.inBlockComment;
    const oldDepth = depth;
    depth = Math.max(
      0,
      depth + balance.delta +
        (lineContinuation && balance.delta > 0 ? lineContinuation : 0),
    );
    if (lineContinuation && balance.delta > 0) {
      continuationBlocks.push(oldDepth + lineContinuation);
    }
    if (balance.delta < 0) {
      while (
        continuationBlocks.length &&
        continuationBlocks[continuationBlocks.length - 1] === depth
      ) {
        continuationBlocks.pop();
        depth = Math.max(0, depth - 1);
      }
    }
    if (closes > 0) continuation = 0;
    else if (lineContinues(content, lineContinuation > 0)) continuation = 1;
    else if (keepsContinuation(content, lineContinuation > 0, balance.delta)) {
      continuation = Math.max(2, lineContinuation);
    } else continuation = 0;
    formatted.push(indentUnit.repeat(lineDepth) + content);
  }
  const result = formatted.join("\n");
  return text.endsWith("\n") ? `${result.replace(/\n*$/, "")}\n` : result;
};
const formatRange = (text, range, options: FormatOptions = {}) => {
  const formatted = formatDocument(text, options);
  if (formatted === text) return null;
  const originalLines = text.split(/\r?\n/);
  const formattedLines = formatted.split(/\r?\n/);
  const lastLine = Math.max(
    range.start.line,
    range.end.character === 0 ? range.end.line - 1 : range.end.line,
  );
  return {
    range: {
      start: { line: range.start.line, character: 0 },
      end: {
        line: lastLine,
        character: originalLines[lastLine]?.length || 0,
      },
    },
    newText: formattedLines.slice(range.start.line, lastLine + 1).join(
      "\n",
    ),
  };
};
const indent = (options: FormatOptions = {}) => {
  const tabSize = Number.isInteger(options.tabSize) ? options.tabSize : 2;
  return options.insertSpaces === false ? "\t" : " ".repeat(tabSize);
};
const leadingClosures = (line) => {
  let count = 0;
  for (const char of line) {
    if (char === "}" || char === ")" || char === "]") count += 1;
    else break;
  }
  return count;
};
const lineContinues = (line, continued = false) => {
  const code = codeBeforeComment(line).trimEnd();
  return (
    code.endsWith("=") ||
    /^law\b.*:$/.test(code) ||
    (continued && code.startsWith(":")) ||
    (/(?:^|\s)let(?:\s|$)/.test(code) && !code.includes("="))
  );
};
const keepsContinuation = (line, continued, delimiterDelta) => {
  const code = codeBeforeComment(line).trimEnd();
  return continued && delimiterDelta === 0 && !code.endsWith(";");
};
const codeBeforeComment = (line) => {
  let inString = false;
  let escaped = false;
  for (let index = 0; index < line.length; index += 1) {
    const char = line[index];
    if (!inString && char === "#") {
      return line.slice(0, index);
    }
    if (
      !inString && char === "/" && line[index + 1] === "*"
    ) {
      return line.slice(0, index);
    }
    if (escaped) {
      escaped = false;
      continue;
    }
    if (inString && char === "\\") {
      escaped = true;
      continue;
    }
    if (char === "'") {
      inString = !inString;
      continue;
    }
    if (!inString && char === "`") {
      index = characterLiteralEnd(line, index);
    }
  }
  return line;
};
const characterLiteralEnd = (line, start) => {
  const character = start + 1;
  if (character >= line.length) return start;
  if (line[character] !== "\\") {
    return character + ((line.codePointAt(character) || 0) > 0xffff ? 1 : 0);
  }
  const escaped = character + 1;
  if (escaped >= line.length || !/[0-9]/.test(line[escaped])) return escaped;
  let end = escaped;
  while (end + 1 < line.length && /[0-9]/.test(line[end + 1])) end += 1;
  return line[end + 1] === ";" ? end + 1 : end;
};
const delimiterBalance = (
  line,
  startedInString = false,
  startedInBlockComment = false,
) => {
  let delta = 0;
  let inString = startedInString;
  let inBlockComment = startedInBlockComment;
  let escaped = false;
  for (let index = 0; index < line.length; index += 1) {
    const char = line[index];
    if (inBlockComment) {
      if (char === "*" && line[index + 1] === "/") {
        inBlockComment = false;
        index += 1;
      }
      continue;
    }
    if (!inString && char === "#") break;
    if (
      !inString && char === "/" && line[index + 1] === "*"
    ) {
      inBlockComment = true;
      index += 1;
      continue;
    }
    if (escaped) {
      escaped = false;
      continue;
    }
    if (inString && char === "\\") {
      escaped = true;
      continue;
    }
    if (char === "'") {
      inString = !inString;
      continue;
    }
    if (!inString && char === "`") {
      index = characterLiteralEnd(line, index);
      continue;
    }
    if (inString) continue;
    if (char === "{" || char === "(" || char === "[") delta += 1;
    if (char === "}" || char === ")" || char === "]") delta -= 1;
  }
  return { delta, inString, inBlockComment };
};
export { formatDocument, formatRange };
