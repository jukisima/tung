@Test
def testEvaluate(): Unit \ Assert =
    assertEvalOk("integer addition", "1 + 2", "3");
    Assert.assertEq(expected = "eval ok: 3", evaluateWithImports("bring file.jbini; 2 inc", ("file.jbini", "let inc = func x ↦ x + 1;") :: Nil));
    assertEvalOk("curried let function", "let inc = func x ↦ x + 1; 41 inc", "42");
    assertEvalOk("curried partial application", "let f = func a, b ↦ a + b; 2 (1 f)", "3");
    assertEvalOk("second-term function definition", "let a add-two b = a + b; 1 add-two 2", "3");
    assertEvalOk("float native reciprocal", "2.0 /", "0.5");
    assertEvalOk("constructor match", "data two { yea, nay } match yea { yea ↦ 1, nay ↦ 0 }", "1");
    assertEvalOk("bone default body", "data 𝟚 { yea, nay } let ¬: 𝟚 → 𝟚 = match { yea ↦ nay, nay ↦ yea }; bone a equal { a ≡ a: 𝟚, let a ≢ b = (a ≡ b) ¬ } flesh 𝟚 equal { a ≡ b = match a { yea ↦ b, nay ↦ b ¬ } } yea ≢ nay", "yea");
    assertEvalOk("record update", "let person = [name = 'naoki', age = 35]; [= person, age = 36]", "[name = 'naoki', age = 36]");
    assertEvalOk("handled effect", "data one { null } try 'hello' write-line { console ↦ null }", "null");
    assertEvalOk("console write-line is runner-provided", "data 𝟙 { null } 'hello' write-line", "null");
    Assert.assertEq(expected = "eval ok: 2", evaluateWithImports("bring prelude.jbini; bring list.jbini; 4 ÷ 2", ("prelude.jbini", nestedPreludeSource()) :: ("list.jbini", "bring prelude.jbini; let anything = 0;") :: Nil));
    assertEvalErr("unknown variable", "missing");
    assertEvalErr("cannot apply integer", "1 2");
    assertEvalErr("non exhaustive match", "data two { yea, nay } match yea { nay ↦ 0 }");
    assertEvalErr("unhandled effect", "data one { null } effect io { ping: one → one } null ping")

def assertEvalOk(_name: String, source: String, expectedValue: String): Unit \ Assert =
    Assert.assertEq(expected = "eval ok: " + expectedValue, evaluate(source))

def assertEvalErr(name: String, source: String): Unit \ Assert =
    let result = evaluate(source);
    if (String.startsWith({ prefix = "eval error:" }, result)) {
        ()
    } else {
        Assert.fail("expected eval error for '${name}', got ${result}")
    }

def nestedPreludeSource(): String =
    "bone a reciprocalise { a /: a } bone a divide need a reciprocalise { let (a: a, b: a) ÷: a = b / }"
