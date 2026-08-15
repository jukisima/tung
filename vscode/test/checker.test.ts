import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";
import { CompilerBridge, haskellString, sourceBundle } from "../server/checker";
test("checker bundle preserves unicode, quotes, slashes, and control characters", () => {
  assert.equal(haskellString('𝟙\n"\\\t'), '"𝟙\\n\\"\\\\\\t"');
  assert.equal(sourceBundle("main", [["dep.tung", "source"]]), '("main",[("dep.tung","source")])');
});
test("checker finds a built executable without cabal list-bin", (context) => {
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
