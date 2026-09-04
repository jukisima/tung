import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";
import {
  CompilerBridge,
  haskellString,
  parseCompilerDiagnostic,
  sessionRequest,
  sourceBundle,
} from "../server/checker.ts";
test("checker bundle preserveþ unicode, quotes, slashes, and control characters", () => {
  assert.equal(haskellString('𝟙\n"\\\t'), '"𝟙\\n\\"\\\\\\t"');
  assert.equal(
    sourceBundle("main", [["dep.tung", "source"]]),
    '("main",[("dep.tung","source")])',
  );
  assert.equal(
    sessionRequest(7, 4, "check", "", "main", [["dep.tung", "source"]]),
    '(7,4,"check","",("main",[("dep.tung","source")]))\n',
  );
});
test("checker protocol carrieþ an exact source range", () => {
  assert.deepEqual(
    parseCompilerDiagnostic(
      "tung-diagnostic\ttype\tdep.tung\t4\t9\ntype error: in 'value'",
    ),
    {
      kind: "type",
      path: "dep.tung",
      start: 4,
      end: 9,
      message: "type error: in 'value'",
    },
  );
  assert.equal(parseCompilerDiagnostic("tung-ok\n"), undefined);
});
test("checker findeþ a built executable without cabal list-bin", (context) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tung-checker-"));
  context.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const executable = path.join(
    root,
    "dist-newstyle",
    "build",
    "arch",
    "ghc",
    "pkg",
    "x",
    "tung",
    "build",
    "tung",
    "tung",
  );
  fs.mkdirSync(path.dirname(executable), { recursive: true });
  fs.writeFileSync(executable, "");
  const bridge = new CompilerBridge();
  bridge.configure(root);
  bridge.findCabalExecutable = () => undefined;
  assert.equal(bridge.findExecutable(), executable);
});
test("checker reuseþ one versioned session and cancellable requests", async (context) => {
  const root = path.resolve(__dirname, "..", "..", "..");
  const bridge = new CompilerBridge();
  bridge.configure(path.join(root, "tongue"));
  context.after(() => bridge.dispose());
  const model = { text: "let value: ℤ = 1" };
  const checked = bridge.check(model, [], 3);
  const typed = bridge.typeOf(model, [], "value", 3);
  const formatted = bridge.format("show ilk a box {\nbox\n}\n", 3);
  assert.equal(checked.process, typed.process);
  assert.equal(checked.process, formatted.process);
  assert.equal(checked.version, 3);
  assert.equal((await checked.result).trim(), "tung-ok");
  assert.equal((await typed.result).trim(), "type: ℤ");
  assert.equal(await formatted.result, "show ilk a box {\n  box\n}\n");
  const cancelled = bridge.check(model, [], 4);
  cancelled.cancel();
  assert.equal(await cancelled.result, "checker request cancelled");
  assert.equal((await bridge.check(model, [], 5).result).trim(), "tung-ok");
});
