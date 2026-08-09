@Test
def testExpressionInference(): Unit \ Assert + IO =
    match readSource("library/prelude.jbini") {
        case Ok(prelude) =>
            let imports = ("prelude.jbini", prelude) :: Nil;
            assertTypeOk("record update changes type", "bring prelude.jbini; let person: [name: string, age: integer] = [name = 'naoki', age = 35]; let public: [name: string] = [= person, -age];", imports);
            assertTypeOk("handler removes effect", "bring prelude.jbini; data one { null } effect zero-divideth {} let effectful: one → integer ! zero-divideth = func _ ↦ 1 divide 0; let handled: one → integer = func _ ↦ try null effectful { zero-divideth ↦ 0 };", imports);
            assertTypeErr("record removal must exist", "bring prelude.jbini; let person: [name: string] = [name = 'naoki']; let bad = [= person, -age];", imports)
        case Err(_) => Assert.fail("could not read library/prelude.jbini")
    }
