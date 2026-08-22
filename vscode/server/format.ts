// range edits surround the authoritative two-space tung formatter.
const formatRangeEdit = (source, formatted, range) => {
  if (formatted === source) return null;
  const originalLines = source.split(/\r?\n/);
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
    newText: formattedLines.slice(range.start.line, lastLine + 1).join("\n"),
  };
};
export { formatRangeEdit };
