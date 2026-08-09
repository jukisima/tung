@Test
def testExpressionInference(): Unit \ Assert + IO =
    match readSource("library/prelude.jbini") {
        case Ok(prelude) =>
            let imports = ("prelude.jbini", prelude) :: Nil;
            assertTypeOk("record update changes type", "bring prelude.jbini; let person: [name: string, age: integer] = [name = 'naoki', age = 35]; let public: [name: string] = [= person, -age];", imports);
            assertTypeOk("handler removes fail effect", "bring prelude.jbini; data one { null } let effectful: one → integer ! string fail = func _ ↦ 1 divide 0; let handled: one → integer = func _ ↦ try null effectful { fail ↦ 0 };", imports);
            assertTypeOk("fail operation is extensible by payload type", "bring prelude.jbini; let abort-string: integer ! string fail = 'bad' fail; let abort-integer: string ! integer fail = 1 fail;", imports);
            assertTypeErr("fail payload type mismatch", "bring prelude.jbini; let bad: integer ! integer fail = 'bad' fail;", imports);
            assertTypeOk("state effect carries type parameter", "bring prelude.jbini; let got: integer ! integer state = null get;", imports);
            assertTypeErr("state effect parameter mismatch", "bring prelude.jbini; let got: integer ! string state = null get;", imports);
            assertTypeErr("record removal must exist", "bring prelude.jbini; let person: [name: string] = [name = 'naoki']; let bad = [= person, -age];", imports);
            assertInferSourceExact("floor inference", "3.7 ⌊", "integer", "");
            assertInferSourceExact("from-string inference carries fail", "'0.5' from-string", "float", "string fail");
            assertInferSourceExact("write-line inference carries console", "'hello' write-line", "𝟙", "console");
            assertInferSourceExact("lambda application inference", "1 (func x ↦ x)", "integer", "");
            assertInferExprExact("record remove inference", recordRemoveExpr(), "[name: string]", "");
            assertInferExprErr("record remove missing field", Expr.Update(Expr.Record(("name", Expr.String("naoki")) :: Nil), RecordUpdate.Remove("age") :: Nil))
        case Err(_) => Assert.fail("could not read library/prelude.jbini")
    }

def recordRemoveExpr(): Expr =
    Expr.Update(
        Expr.Record(("name", Expr.String("naoki")) :: ("age", Expr.Integer(35)) :: Nil),
        RecordUpdate.Remove("age") :: Nil
    )

def assertInferSourceExact(name: String, source: String, expectedTy: String, expectedEffects: String): Unit \ Assert =
    match parse(source) {
        case ParseResult.Ok(Program.Decls(Decl.Let("_", None, expr) :: Nil), Nil) => assertInferExprExact(name, expr, expectedTy, expectedEffects)
        case ParseResult.Ok(_, Nil) => Assert.fail("expected expression source for '${name}'")
        case ParseResult.Ok(_, _) => Assert.fail("parse left trailing tokens for '${name}'")
        case ParseResult.Err(msg) => Assert.fail("parse failed for '${name}': ${msg}")
    }

def assertInferExprExact(name: String, expr: Expr, expectedTy: String, expectedEffects: String): Unit \ Assert =
    match inferExpr(expr, TcContext.Context(baseEnv(), Nil, Nil), TcState.State(0, Nil, Nil)) {
        case TcResult.Err(msg) => Assert.fail("inference failed for '${name}': ${msg}")
        case TcResult.Ok(Inferred.Value(ty, effects), st) =>
            Assert.assertEq(expected = expectedTy, showTy(applyState(ty, st)));
            Assert.assertEq(expected = expectedEffects, showEffects(applyStateEffects(effects, st)))
    }

def assertInferExprErr(name: String, expr: Expr): Unit \ Assert =
    match inferExpr(expr, TcContext.Context(baseEnv(), Nil, Nil), TcState.State(0, Nil, Nil)) {
        case TcResult.Err(_) => ()
        case TcResult.Ok(Inferred.Value(ty, effects), st) => Assert.fail("expected inference error for '${name}', got " + showTy(applyState(ty, st)) + " ! " + showEffects(applyStateEffects(effects, st)))
    }
