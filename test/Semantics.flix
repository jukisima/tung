@Test
def testSemanticsSamples(): Unit \ Assert + IO =
    match readSource("library/prelude.jbini") {
        case Ok(prelude) =>
            assertTypeOk("final semicolon may be omitted", "let final-without-semicolon: integer = 1", Nil);
            assertSamplesWithPrelude(prelude, samples())
        case Err(_) => Assert.fail("could not read library/prelude.jbini")
    }

@Test
def testLibraryFiles(): Unit \ Assert + IO =
    match (readSource("library/prelude.jbini"), readSource("library/algebra.jbini"), readSource("library/monad.jbini"), readSource("library/option.jbini")) {
        case (Ok(prelude), Ok(algebra), Ok(monad), Ok(option)) =>
            let preludeImport = ("prelude.jbini", prelude) :: Nil;
            let algebraImport = ("algebra.jbini", algebra) :: preludeImport;
            let monadImport = ("monad.jbini", monad) :: preludeImport;
            let optionImport = ("option.jbini", option) :: monadImport;
            assertFileTypeOk("library/prelude.jbini", Nil);
            assertFileTypeOk("library/combinator.jbini", Nil);
            assertFileTypeOk("library/algebra.jbini", Nil);
            assertFileTypeOk("library/constant.jbini", Nil);
            assertFileTypeOk("library/natural.jbini", preludeImport);
            assertFileTypeOk("library/list.jbini", algebraImport);
            assertFileTypeOk("library/monad.jbini", preludeImport);
            assertFileTypeOk("library/option.jbini", monadImport);
            Assert.assertEq(expected = "eval ok: (2 some)", evaluateWithImports("bring prelude.jbini; bring monad.jbini; bring option.jbini; let value: integer option = 1 some; let got: integer option = value bind (func x ↦ (x + 1) some); got", optionImport));
            assertFileTypeOk("library/complex.jbini", preludeImport)
        case (Err(_), _, _, _) => Assert.fail("could not read library/prelude.jbini")
        case (_, Err(_), _, _) => Assert.fail("could not read library/algebra.jbini")
        case (_, _, Err(_), _) => Assert.fail("could not read library/monad.jbini")
        case (_, _, _, Err(_)) => Assert.fail("could not read library/option.jbini")
    }

@Test
def testSampleFiles(): Unit \ Assert + IO =
    match (readSource("library/prelude.jbini"), readSource("library/algebra.jbini"), readSource("library/list.jbini"), readSource("sample/lambda.jbini"), readSource("sample/fizzbuzz.jbini")) {
        case (Ok(prelude), Ok(algebra), Ok(list), Ok(lambda), Ok(fizzbuzz)) =>
            assertTypeOk("sample/lambda.jbini", lambda, ("prelude.jbini", prelude) :: Nil);
            assertTypeOk("sample/fizzbuzz.jbini", fizzbuzz, ("algebra.jbini", algebra) :: ("list.jbini", list) :: ("prelude.jbini", prelude) :: Nil)
        case (Err(_), _, _, _, _) => Assert.fail("could not read library/prelude.jbini")
        case (_, Err(_), _, _, _) => Assert.fail("could not read library/algebra.jbini")
        case (_, _, Err(_), _, _) => Assert.fail("could not read library/list.jbini")
        case (_, _, _, Err(_), _) => Assert.fail("could not read sample/lambda.jbini")
        case (_, _, _, _, Err(_)) => Assert.fail("could not read sample/fizzbuzz.jbini")
    }

def assertTypeOk(_name: String, source: String, imports: List[(String, String)]): Unit \ Assert =
    Assert.assertEq(expected = "type ok", checkWithImports(source, imports))

def assertTypeErr(name: String, source: String, imports: List[(String, String)]): Unit \ Assert =
    let result = checkWithImports(source, imports);
    if (String.startsWith({ prefix = "type error:" }, result)) {
        ()
    } else {
        Assert.fail("expected type error for '${name}', got ${result}")
    }

def assertSamplesWithPrelude(prelude: String, xs: List[String]): Unit \ Assert =
    match xs {
        case Nil => ()
        case sample :: rest =>
            assertTypeOk("sample", "bring prelude.jbini; " + sample, ("prelude.jbini", prelude) :: Nil);
            assertSamplesWithPrelude(prelude, rest)
    }

def assertFileTypeOk(path: String, imports: List[(String, String)]): Unit \ Assert + IO =
    match readSource(path) {
        case Ok(source) => assertTypeOk(path, source, imports)
        case Err(_) => Assert.fail("could not read ${path}")
    }

def readSource(path: String): Result[IoError, String] \ IO =
    Fs.FileRead.runWithIO(() -> Fs.FileRead.read(path))

def samples(): List[String] =
    "let implicitly-typed = 205;" ::
    "let explicitly-typed: integer = -205;" ::
    "let inferred-float = 3.14;" ::
    "let explicitly-float: float = -2.5;" ::
    "let tau: float = 6.283185;" ::
    "data two { yea, nay } type string = character list; let math-to-string: string = 3.7 to-string; let math-floor: integer = 3.7 ⌊; let math-order: 𝟚 = 1.0 ≤ 2.0; let math-add: float = 1.0 + 2.0; let math-negate: float = 3.0 ¯; let math-multiply: float = 2.0 × 3.0; let math-reciprocal: float ! zero-divideth = 2.0 /; let math-exp: float = 1.0 exp; let math-log: float = 2.718281828459045 log; let math-sin: float = 1.0 sin; let math-cos: float = 1.0 cos;" ::
    "type string = character list;" ::
    "let person: [name: string, age: integer] = [ name = 'naoki', age = 35 ];" ::
    "let person: [name: string, age: integer] = [ name = 'naoki', age = 35 ]; let without-age: [name: string] = [= person, -age];" ::
    "let person: [name: string, age: integer] = [ name = 'naoki', age = 35 ]; let with-place: [name: string, age: integer, place: string] = [= person, place = 'japan'];" ::
    "data one { null } effect console { write: string → one, read: one → string } effect zero-divideth {} let effectful: one → integer ! console, zero-divideth = func _ ↦ ( let _ = try 'hello' write; let constant = 1 ÷ 0; constant );" ::
    "data one { null } effect console { write: string → one, read: one → string } effect zero-divideth {} let effectful: one → integer ! console, zero-divideth = func _ ↦ ( let _ = try 'hello' write; let constant = 1 ÷ 0; constant ); let no-zero-divideth: one → integer ! console = func _ ↦ try null effectful { zero-divideth ↦ ( let _ = try 'division by zero' write; 0 ) };" ::
    "let write-result: 𝟙 ! console = 'hello' write-line;" ::
    "let read-result: string ! console = null read-line;" ::
    "effect io { trace: integer → integer } let traced: integer → integer ! io = trace;" ::
    "effect io { trace: integer → integer } let uses-effect: integer ! io = 1 trace;" ::
    "let id-explicit-abstraction : a → a = func a ↦ a;" ::
    "let const-explicit-abstraction : a → b → a = func a, b ↦ a;" ::
    "let add-one: integer → integer = 1 +;" ::
    "let add-three: integer = 3 (1 +);" ::
    "let flat-add-three: integer = 1 + 2;" ::
    "let nested-add-three: integer = 2 (1 +);" ::
    "let float-sum: float = 1.5 + 2.25;" ::
    "let float-product: float = 1.5 × 2.0;" ::
    "let negated: integer = 3 ¯;" ::
    "data a list { empty, a .* (a list) } let single: integer list → integer list = 1 .*;" ::
    "let sequence-call : integer = (3 + 2) × 4;" ::
    "data two { yea, nay } let same: two → two = func a ↦ a;" ::
    "data two { yea, nay } let chosen: two = yea;" ::
    "data two { yea, nay } let not: two → two = match { yea ↦ nay, nay ↦ yea };" ::
    "data two { yea, nay } let choose: two → two → two = match { yea, b ↦ b, nay, _ ↦ nay };" ::
    "data two { yea, nay } let chosen: two = match yea, nay { yea, b ↦ b, nay, _ ↦ nay };" ::
    "data two { yea, nay } let not-short : two → two = match { yea ↦ nay, nay ↦ yea };" ::
    "bone a add { a + a: a } bone a negate { a ¯: a } bone a multiply { a × a: a } bone a reciprocalise { a /: a ! zero-divideth }" ::
    "data two { yea, nay } let not: two → two = match { yea ↦ nay, nay ↦ yea }; bone a equal { a equal a: two, let a equal-not b = (a equal b) not }" ::
    "data two { yea, nay } let not: two → two = match { yea ↦ nay, nay ↦ yea }; bone a equal { a equal a: two, let a equal-not b = (a equal b) not } flesh two equal { a equal b = match a { yea ↦ b, nay ↦ b not } } let got: two = yea equal-not nay;" ::
    "data two { yea, nay } bone a equal { a equal a: two } bone a order need a equal { a less-equal a: two, let a greater-equal b = b less-equal a } flesh two order { a less-equal b = match a { yea ↦ b, nay ↦ yea } }" ::
    "let swap: (a → b → c ! e) → b → a → c ! e = func f ↦ func a, b ↦ b f a;" ::
    "data one { null } effect console { write: string → one } effect zero-divideth {} let effectful: one → integer ! console, zero-divideth = func _ ↦ ( let _ = try 'hello' write; let constant = 1 ÷ 0; constant ); let no-zero-divideth: one → integer ! console = func _ ↦ try null effectful { zero-divideth ↦ 0 };" ::
    "data v lambda { v var, v abs (v lambda), (v lambda) app (v lambda) } let subst: v → v lambda → v lambda → v lambda = func x, s, target ↦ match target { y var ↦ s, y abs t ↦ y abs t, t0 app t1 ↦ t0 app t1 };" ::
    "bone f functor { (a f) map (a → b ! e): b f ! e }" ::
    Nil
