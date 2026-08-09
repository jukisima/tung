@Test
def testPatternChecking(): Unit \ Assert =
    assertTypeOk("qualified patterns", "data two { yea, nay } let not-qualified-pattern: two → two = match { two@yea ↦ two@nay, two@nay ↦ two@yea };", Nil);
    assertTypeOk("multi pattern flesh member", "data two { yea, nay } bone a sum { a + a: a } flesh two sum { a + b = match a, b { yea, nay ↦ yea, nay, yea ↦ yea, _, _ ↦ nay } }", Nil);
    assertTypeOk("complex-like flesh member", "data two { yea, nay } data a complex { a complex a } bone a add { a + a: a } flesh two add { a + b = a } flesh (two complex) add { (a complex b) + (c complex d) = (a + c) complex (b + d) }", Nil);
    assertTypeOk("nested recursive match is exhaustive", "data a list { empty, a .* (a list) } let ok: integer list → integer = match { empty ↦ 0, x .* xs ↦ 1 };", Nil);
    assertTypeErr("single scrutinee match must cover every constructor", "data two { yea, nay } let bad: two → integer = match { yea ↦ 1 };", Nil);
    assertTypeErr("multi scrutinee match must cover every product case", "data two { yea, nay } let bad: two → two → integer = match { yea, yea ↦ 1, nay, yea ↦ 2 };", Nil);
    assertTypeErr("nested match must cover constructor fields", "data two { yea, nay } data a option { none, a some } let bad: two option → integer = match { none ↦ 0, yea some ↦ 1 };", Nil)
