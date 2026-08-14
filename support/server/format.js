function formatDocument(text, options = {}) {
  const indentUnit = indent(options);
  const lines = text.split(/\r?\n/);
  let depth = 0;
  let inString = false;

  const formatted = lines.map((line) => {
    const content = line.trim();
    if (!content) return "";
    const closes = leadingClosures(content);
    const lineDepth = Math.max(0, depth - closes);
    const balance = delimiterBalance(content, inString);
    inString = balance.inString;
    depth = Math.max(0, depth + balance.delta);
    return indentUnit.repeat(lineDepth) + content;
  }).join("\n");

  return text.endsWith("\n") ? `${formatted.replace(/\n*$/, "")}\n` : formatted;
}

function formatRange(text, range, options = {}) {
  const formatted = formatDocument(text, options);
  if (formatted === text) return null;
  const originalLines = text.split(/\r?\n/);
  const formattedLines = formatted.split(/\r?\n/);
  const lastLine = Math.max(range.start.line, range.end.character === 0 ? range.end.line - 1 : range.end.line);
  return {
    range: {
      start: { line: range.start.line, character: 0 },
      end: { line: lastLine, character: originalLines[lastLine]?.length || 0 }
    },
    newText: formattedLines.slice(range.start.line, lastLine + 1).join("\n")
  };
}

function indent(options = {}) {
  const tabSize = Number.isInteger(options.tabSize) ? options.tabSize : 2;
  return options.insertSpaces === false ? "\t" : " ".repeat(tabSize);
}

function leadingClosures(line) {
  let count = 0;
  for (const char of line) {
    if (["}", ")", "]"].includes(char)) count += 1;
    else break;
  }
  return count;
}

function delimiterBalance(line, startedInString = false) {
  let delta = 0;
  let inString = startedInString;
  let inCharacter = false;
  let escaped = false;
  for (let index = 0; index < line.length; index += 1) {
    const char = line[index];
    if (!inString && !inCharacter && char === "#") break;
    if (escaped) {
      escaped = false;
      continue;
    }
    if ((inString || inCharacter) && char === "\\") {
      escaped = true;
      continue;
    }
    if (!inCharacter && char === "'") {
      inString = !inString;
      continue;
    }
    if (!inString && char === "`") {
      inCharacter = true;
      continue;
    }
    if (inCharacter) {
      if (char === "{" && line[index - 1] === "\\") continue;
      inCharacter = false;
      continue;
    }
    if (inString) continue;
    if (["{", "(", "["].includes(char)) delta += 1;
    if (["}", ")", "]"].includes(char)) delta -= 1;
  }
  return { delta, inString };
}

module.exports = { formatDocument, formatRange, _test: { delimiterBalance, indent, leadingClosures } };
