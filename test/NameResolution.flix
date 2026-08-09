@Test
def testNameResolution(): Unit \ Assert + IO =
    match readSource("library/prelude.jbini") {
        case Ok(prelude) =>
            assertTypeOk("unique imported bare name is allowed", "bring file.jbini; let ok: integer = foo;", ("file.jbini", "let foo: integer = 1;") :: Nil);
            assertTypeOk("unique imported bare type is allowed", "bring file.jbini; let ok: file-box = box;", ("file.jbini", "data file-box { box }") :: Nil);
            assertTypeErr("ambiguous imported bare name is rejected", "bring file.jbini; bring other.jbini; let bad: integer = foo;", ("file.jbini", "let foo: integer = 1;") :: ("other.jbini", "let foo: integer = 2;") :: Nil);
            assertTypeOk("qualified duplicate member", "bone a first { a same: a } bone a second { a same: a } let ok: integer → integer = first@same;", Nil);
            assertTypeOk("bare constructor", "data two { yea, nay } let chosen-bare: two = yea;", Nil);
            assertTypeOk("bare effect operation", "effect io { trace: integer → integer } let traced-bare: integer ! io = 1 trace;", Nil);
            assertImportSamples(prelude);
            assertImportErrorSamples(prelude)
        case Err(_) => Assert.fail("could not read library/prelude.jbini")
    }

def assertImportSamples(prelude: String): Unit \ Assert =
    assertImportSample(prelude, "bring file.jbini; let imported-value: integer = file@foo;");
    assertImportSample(prelude, "bring file.jbini; let imported-call: integer = 2 file@inc;");
    assertImportSample(prelude, "bring file.jbini; let imported-bare-value: integer = foo;");
    assertImportSample(prelude, "bring file.jbini; let imported-bare-call: integer = 2 inc;");
    assertTypeOk("qualified imported value", "bring file.jbini; bring other.jbini; let imported-qualified-value: integer = file@foo;", ("file.jbini", importedSource()) :: ("other.jbini", otherSource()) :: ("prelude.jbini", prelude) :: Nil)

def assertImportErrorSamples(prelude: String): Unit \ Assert =
    assertTypeErr("bad effect qualification", "bring prelude.jbini; effect io { trace: integer → integer } let bad: integer ! console = 1 io@trace;", ("prelude.jbini", prelude) :: Nil);
    assertTypeErr("ambiguous local member", "bring prelude.jbini; bone a first { a same: a } bone a second { a same: a } let bad: integer → integer = same;", ("prelude.jbini", prelude) :: Nil);
    assertTypeErr("ambiguous effect operation", "bring prelude.jbini; effect io { trace: integer → integer } effect log { trace: integer → integer } let bad: integer ! io = 1 trace;", ("prelude.jbini", prelude) :: Nil);
    assertTypeErr("ambiguous imported bare name", "bring file.jbini; bring other.jbini; let imported-ambiguous-value: integer = foo;", ("file.jbini", importedSource()) :: ("other.jbini", otherSource()) :: ("prelude.jbini", prelude) :: Nil)

def assertImportSample(prelude: String, sample: String): Unit \ Assert =
    assertTypeOk("import sample", sample, ("file.jbini", importedSource()) :: ("prelude.jbini", prelude) :: Nil)

def importedSource(): String =
    "bring prelude.jbini; let foo: integer = 41; let inc: integer → integer = func x ↦ x + 1;"

def otherSource(): String =
    "bring prelude.jbini; let foo: integer = 7;"
