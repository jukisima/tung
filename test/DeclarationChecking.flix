@Test
def testDeclarationChecking(): Unit \ Assert + IO =
    match readSource("library/prelude.jbini") {
        case Ok(prelude) =>
            let imports = ("prelude.jbini", prelude) :: Nil;
            assertTypeOk("polymorphic let is reusable", "bring prelude.jbini; let id: a → a = func x ↦ x; let one: integer = 1 id; let text: string = 'hi' id;", imports);
            assertTypeOk("typed second-term function definition", "bring prelude.jbini; let a add-two b: integer → integer → integer = a + b; let got: integer = 1 add-two 2;", imports);
            assertTypeOk("typed argument function definition", "bring prelude.jbini; let (x: integer, y: integer) add-two: integer = x + y; let got: integer = 1 add-two 2;", imports);
            assertTypeOk("prelude bone default member", "bring prelude.jbini; let got: 𝟚 = yea ≢ nay;", imports);
            assertTypeOk("operator flesh member", "data two { yea, nay } bone a sum { a + a: a } flesh two sum { a + b = match a { nay ↦ b, yea ↦ yea } }", Nil);
            assertTypeErr("literal type mismatch", "bring prelude.jbini; let bad: integer = 'hello';", imports)
        case Err(_) => Assert.fail("could not read library/prelude.jbini")
    }
