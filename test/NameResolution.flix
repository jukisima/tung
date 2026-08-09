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
            assertLookupTyExact("bare imported value lookup", "bring file.jbini;", ("file.jbini", importedSource()) :: ("prelude.jbini", prelude) :: Nil, "foo", "integer");
            assertLookupTyExact("qualified imported value lookup", "bring file.jbini;", ("file.jbini", importedSource()) :: ("prelude.jbini", prelude) :: Nil, "file@foo", "integer");
            assertLookupTyExact("local value shadows imported bare value", "bring file.jbini; let foo: integer = 9;", ("file.jbini", importedSource()) :: ("prelude.jbini", prelude) :: Nil, "foo", "integer");
            assertLookupMissingExact("missing qualified value lookup", "bring file.jbini;", ("file.jbini", importedSource()) :: ("prelude.jbini", prelude) :: Nil, "other@foo");
            assertLookupAmbiguous("ambiguous imported value lookup", "bring file.jbini; bring other.jbini;", ("file.jbini", importedSource()) :: ("other.jbini", otherSource()) :: ("prelude.jbini", prelude) :: Nil, "foo");
            assertLookupTyExact("first qualified ambiguous candidate lookup", "bring file.jbini; bring other.jbini;", ("file.jbini", importedSource()) :: ("other.jbini", otherSource()) :: ("prelude.jbini", prelude) :: Nil, "file@foo", "integer");
            assertLookupTyExact("second qualified ambiguous candidate lookup", "bring file.jbini; bring other.jbini;", ("file.jbini", importedSource()) :: ("other.jbini", otherSource()) :: ("prelude.jbini", prelude) :: Nil, "other@foo", "integer");
            assertCanonicalTypeExact("bare imported type canonicalization", "bring file.jbini;", ("file.jbini", "data file-box { box }") :: Nil, "file-box", "file@file-box");
            assertCanonicalTypeExact("qualified imported type canonicalization", "bring file.jbini;", ("file.jbini", "data file-box { box }") :: Nil, "file@file-box", "file@file-box");
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

def assertLookupTyExact(testName: String, source: String, imports: List[(String, String)], name: String, expectedTy: String): Unit \ Assert =
    match checkedContext(testName, source, imports) {
        case None => ()
        case Some(ctx) =>
            match lookupEnv(name, ctx) {
                case EnvLookup.Found(scheme) => Assert.assertEq(expected = expectedTy, schemeTyText(scheme))
                case EnvLookup.Missing => Assert.fail("expected '${name}' to resolve in '${testName}'")
                case EnvLookup.Ambiguous(msg) => Assert.fail("expected '${name}' to resolve in '${testName}', got ambiguity: ${msg}")
            }
    }

def assertLookupMissingExact(testName: String, source: String, imports: List[(String, String)], name: String): Unit \ Assert =
    match checkedContext(testName, source, imports) {
        case None => ()
        case Some(ctx) =>
            match lookupEnv(name, ctx) {
                case EnvLookup.Missing => ()
                case EnvLookup.Found(scheme) => Assert.fail("expected '${name}' to be missing in '${testName}', got " + schemeTyText(scheme))
                case EnvLookup.Ambiguous(msg) => Assert.fail("expected '${name}' to be missing in '${testName}', got ambiguity: ${msg}")
            }
    }

def assertLookupAmbiguous(testName: String, source: String, imports: List[(String, String)], name: String): Unit \ Assert =
    match checkedContext(testName, source, imports) {
        case None => ()
        case Some(ctx) =>
            match lookupEnv(name, ctx) {
                case EnvLookup.Ambiguous(_) => ()
                case EnvLookup.Missing => Assert.fail("expected '${name}' to be ambiguous in '${testName}', got missing")
                case EnvLookup.Found(scheme) => Assert.fail("expected '${name}' to be ambiguous in '${testName}', got " + schemeTyText(scheme))
            }
    }

def assertCanonicalTypeExact(testName: String, source: String, imports: List[(String, String)], name: String, expected: String): Unit \ Assert =
    match checkedContext(testName, source, imports) {
        case None => ()
        case Some(ctx) =>
            match canonicalTypeName(name, ctx) {
                case Some(actual) => Assert.assertEq(expected = expected, actual)
                case None => Assert.fail("expected type '${name}' to resolve in '${testName}'")
            }
    }

def checkedContext(testName: String, source: String, imports: List[(String, String)]): Option[TcContext] \ Assert =
    match parse(source) {
        case ParseResult.Err(msg) =>
            Assert.fail("parse failed for '${testName}': ${msg}");
            None
        case ParseResult.Ok(program, _) =>
            match inferProgramContext(program, imports, TcContext.Context(baseEnv(), Nil, Nil)) {
                case TcResult.Err(msg) =>
                    Assert.fail("type checking context failed for '${testName}': ${msg}");
                    None
                case TcResult.Ok(ctx, _) => Some(ctx)
            }
    }

def schemeTyText(scheme: Scheme): String =
    match scheme {
        case Scheme.Forall(_, ty) => showTy(ty)
    }
