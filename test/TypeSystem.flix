@Test
def testTypeSystem(): Unit \ Assert =
    Assert.assertEq(expected = "integer", showTy(Ty.Con("integer")));
    Assert.assertEq(expected = true, sameEffects(effect0("console") :: Nil, Ty.Var("e") :: Nil));
    Assert.assertEq(expected = false, sameEffects(effect0("console") :: Nil, Ty.App("fail", Ty.Con("string") :: Nil) :: Nil));
    Assert.assertEq(expected = false, sameEffects(Ty.App("fail", Ty.Con("string") :: Nil) :: Nil, Ty.App("fail", Ty.Con("integer") :: Nil) :: Nil));
    Assert.assertEq(expected = true, sameEffects(Ty.App("state", Ty.Con("integer") :: Nil) :: Nil, Ty.App("state", Ty.Con("integer") :: Nil) :: Nil));
    Assert.assertEq(expected = false, sameEffects(Ty.App("state", Ty.Con("integer") :: Nil) :: Nil, Ty.App("state", Ty.Con("string") :: Nil) :: Nil));
    assertTypeOk("native functions have expected types", nativeSample(), Nil)

def effect0(name: String): Ty =
    Ty.App(name, Nil)

def nativeSample(): String =
    "data two { yea, nay } data 𝟙 { null } type string = character list; " +
    "let math-to-string: string = 3.7 to-string; " +
    "let parsed-float: float ! string fail = '0.5' from-string; " +
    "let math-floor: integer = 3.7 ⌊; " +
    "let math-order: 𝟚 = 1.0 ≤ 2.0; " +
    "let math-add: float = 1.0 + 2.0; " +
    "let math-negate: float = 3.0 ¯; " +
    "let math-multiply: float = 2.0 × 3.0; " +
    "let math-reciprocal: float ! string fail = 2.0 /; " +
    "let math-exp: float = 1.0 exp; " +
    "let math-log: float = 2.718281828459045 log; " +
    "let math-sin: float = 1.0 sin; " +
    "let math-cos: float = 1.0 cos; " +
    "let random-float-value: float ! random = null random-float; " +
    "let random-integer-value: integer ! random = null random-integer; " +
    "effect a state { get: 𝟙 → a, set: a → 𝟙 } " +
    "let state-integer-value: integer ! integer state = null get;"
