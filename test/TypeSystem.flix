@Test
def testTypeSystem(): Unit \ Assert =
    Assert.assertEq(expected = "integer", showTy(Ty.Con("integer")));
    Assert.assertEq(expected = true, sameEffects("console" :: Nil, "e" :: Nil));
    Assert.assertEq(expected = false, sameEffects("console" :: Nil, "zero-divideth" :: Nil));
    assertTypeOk("native functions have expected types", nativeSample(), Nil)

def nativeSample(): String =
    "data two { yea, nay } type string = character list; " +
    "let math-to-string: string = 3.7 to-string; " +
    "let math-floor: integer = 3.7 ⌊; " +
    "let math-order: two = 1.0 ≤ 2.0; " +
    "let math-add: float = 1.0 + 2.0; " +
    "let math-negate: float = 3.0 ¯; " +
    "let math-multiply: float = 2.0 × 3.0; " +
    "let math-reciprocal: float ! zero-divideth = 2.0 /; " +
    "let math-exp: float = 1.0 exp; " +
    "let math-log: float = 2.718281828459045 log; " +
    "let math-sin: float = 1.0 sin; " +
    "let math-cos: float = 1.0 cos;"
