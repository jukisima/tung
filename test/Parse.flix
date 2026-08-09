@Test
def testParse(): Unit \ Assert =
    assertParseOk("final semicolon may be omitted", "let answer: integer = 42");
    assertParseOk("multi scrutinee match", "match 1, 2 { a, b ↦ a }");
    Assert.assertEq(expected = "curried plus", parseShape("1 + 2"));
    Assert.assertEq(expected = "curried plus", parseShape("$+ 1 2"));
    Assert.assertEq(expected = "curried plus", parseShape("1 2 $+"));
    assertParseOk("explicit data/type operator marker", "data a b $pair { a b $pair } let made: integer string $pair = 1 'x' $pair");
    assertParseOk("explicit function header marker", "let a b $combine: integer → integer → integer = a + b; 1 combine 2");
    assertParseOk("typed arguments before function name", "let (x: integer, y: integer) add: integer = x + y; 1 add 2");
    assertParseOk("parameterized effect declaration", "effect a state { get: 𝟙 → a, set: a → 𝟙 }");
    assertParseOk("name may use non-special characters", "let rate% = 1; rate%");
    assertParseOk("bone default body", "data 𝟚 { yea, nay } let ¬: 𝟚 → 𝟚 = match { yea ↦ nay, nay ↦ yea }; bone a equal { a ≡ a: 𝟚, let a ≢ b = (a ≡ b) ¬ }");
    assertParseOk("typed bone default body", "bone a subtract need a add { let (a: a, b: a) -: a = a }");
    assertParseOk("flesh member supports constructor patterns", "data two { yea, nay } data a complex { a complex a } bone a add { a + a: a } flesh (two complex) add { (a complex b) + (c complex d) = (a + c) complex (b + d) }");
    assertParseErr("match case needs ↦", "match 1 { _ 1 }")

def assertParseOk(name: String, source: String): Unit \ Assert =
    match parse(source) {
        case ParseResult.Ok(_, Nil) => ()
        case ParseResult.Ok(_, _) => Assert.fail("parse failed for '${name}': parser left trailing tokens")
        case ParseResult.Err(msg) => Assert.fail("parse failed for '${name}': ${msg}")
    }

def assertParseErr(name: String, source: String): Unit \ Assert =
    match parse(source) {
        case ParseResult.Err(_) => ()
        case ParseResult.Ok(_, _) => Assert.fail("parse unexpectedly succeeded for '${name}'")
    }

def parseShape(source: String): String =
    match parse(source) {
        case ParseResult.Ok(Program.Decls(Decl.Let("_", None, Expr.Apply(Expr.Apply(Expr.Var("+"), Expr.Integer(1) :: Nil), Expr.Integer(2) :: Nil)) :: Nil), Nil) => "curried plus"
        case ParseResult.Ok(_, Nil) => "other"
        case ParseResult.Ok(_, _) => "trailing"
        case ParseResult.Err(msg) => "error: ${msg}"
    }
