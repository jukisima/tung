def inferExpr(e: Expr, ctx: TcContext, st: TcState): TcResult[Inferred] =
    match e {
        case Expr.Integer(_) => TcResult.Ok(Inferred.Value(Ty.Con("integer"), Nil), st)
        case Expr.Float(_) => TcResult.Ok(Inferred.Value(Ty.Con("float"), Nil), st)
        case Expr.Char(_) => TcResult.Ok(Inferred.Value(Ty.Con("character"), Nil), st)
        case Expr.String(_) => TcResult.Ok(Inferred.Value(Ty.Con("string"), Nil), st)
        case Expr.Var(name) => inferVar(name, ctx, st)
        case Expr.Apply(f, args) => inferApply(f, args, ctx, st)
        case Expr.Record(fields) => inferRecordFields(fields, ctx, st, Nil, Nil)
        case Expr.Update(base, updates) => inferRecordUpdate(base, updates, ctx, st)
        case Expr.Try(body, cases) => inferTryHandle(body, cases, ctx, st)
        case Expr.Lambda(ps, body) => inferLambda(ps, body, ctx, st)
        case Expr.Match(scrutinees, cases) => inferMatch(scrutinees, cases, ctx, st)
        case Expr.Block(ds, body) => inferBlock(ds, body, ctx, st)
    }

def inferRecordFields(fields: List[(String, Expr)], ctx: TcContext, st: TcState, acc: List[(String, Ty)], effects: List[String]): TcResult[Inferred] =
    match fields {
        case Nil => TcResult.Ok(Inferred.Value(Ty.Record(List.reverse(acc)), effects), st)
        case (name, expr) :: rest =>
            match inferExpr(expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(Inferred.Value(t, fieldEffects), st2) =>
                    inferRecordFields(rest, ctx, st2, (name, t) :: acc, unionEffects(effects, fieldEffects))
            }
    }

def inferRecordUpdate(base: Expr, updates: List[RecordUpdate], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match inferExpr(base, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(Inferred.Value(baseTy, baseEffects), st1) =>
            match normalizeFun(applyState(baseTy, st1)) {
                case Ty.Record(fields) => inferRecordUpdates(updates, fields, ctx, st1, baseEffects)
                case other => TcResult.Err("record update expected record, found ${showTy(other)}")
            }
    }

def inferRecordUpdates(updates: List[RecordUpdate], fields: List[(String, Ty)], ctx: TcContext, st: TcState, effects: List[String]): TcResult[Inferred] =
    match updates {
        case Nil => TcResult.Ok(Inferred.Value(Ty.Record(fields), effects), st)
        case RecordUpdate.Remove(name) :: rest =>
            match removeRecordField(name, fields, Nil) {
                case None => TcResult.Err("unknown record field '${name}'")
                case Some(newFields) => inferRecordUpdates(rest, newFields, ctx, st, effects)
            }
        case RecordUpdate.Set(name, expr) :: rest =>
            match inferExpr(expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(Inferred.Value(fieldTy, fieldEffects), st2) =>
                    inferRecordUpdates(rest, upsertRecordField(name, fieldTy, fields), ctx, st2, unionEffects(effects, fieldEffects))
            }
    }

def removeRecordField(name: String, fields: List[(String, Ty)], acc: List[(String, Ty)]): Option[List[(String, Ty)]] =
    match fields {
        case Nil => None
        case (fieldName, _) :: rest if fieldName == name => Some(appendList(List.reverse(acc), rest))
        case field :: rest => removeRecordField(name, rest, field :: acc)
    }

def upsertRecordField(name: String, ty: Ty, fields: List[(String, Ty)]): List[(String, Ty)] =
    upsertRecordFieldLoop(name, ty, fields, Nil)

def upsertRecordFieldLoop(name: String, ty: Ty, fields: List[(String, Ty)], acc: List[(String, Ty)]): List[(String, Ty)] =
    match fields {
        case Nil => appendList(List.reverse(acc), (name, ty) :: Nil)
        case (fieldName, _) :: rest if fieldName == name => appendList(List.reverse(acc), (name, ty) :: rest)
        case field :: rest => upsertRecordFieldLoop(name, ty, rest, field :: acc)
    }

def inferBlock(ds: List[Decl], body: Expr, ctx: TcContext, st: TcState): TcResult[Inferred] =
    match checkLocalDecls(ds, ctx, st, Nil) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok((ctx2, declEffects), st2) =>
            match inferExpr(body, ctx2, st2) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(Inferred.Value(bodyTy, bodyEffects), st3) => TcResult.Ok(Inferred.Value(bodyTy, unionEffects(declEffects, bodyEffects)), st3)
            }
    }

def inferVar(name: String, ctx: TcContext, st: TcState): TcResult[Inferred] =
    match lookupEnv(name, ctx) {
        case EnvLookup.Missing => TcResult.Err("unknown name '${name}'")
        case EnvLookup.Ambiguous(msg) => TcResult.Err(msg)
        case EnvLookup.Found(scheme) =>
            match instantiate(scheme, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(t, st2) => TcResult.Ok(Inferred.Value(t, Nil), st2)
            }
    }

def inferApply(f: Expr, args: List[Expr], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match flattenApply(f, args) {
        case (Expr.Var(name), allArgs) => inferNamedApply(name, allArgs, ctx, st)
        case (other, allArgs) =>
            match inferExpr(other, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(inferred, st1) => inferCurriedApply(inferred, allArgs, ctx, st1)
            }
    }

def flattenApply(f: Expr, args: List[Expr]): (Expr, List[Expr]) =
    match f {
        case Expr.Apply(inner, innerArgs) =>
            let (base, baseArgs) = flattenApply(inner, innerArgs);
            (base, appendList(baseArgs, args))
        case _ => (f, args)
    }

def inferNamedApply(name: String, args: List[Expr], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match lookupEnvChoices(name, ctx) {
        case EnvChoices.Missing => TcResult.Err("unknown name '${name}'")
        case EnvChoices.Ambiguous(msg) => TcResult.Err(msg)
        case EnvChoices.Choices(schemes) => tryApplySchemes(name, args, schemes, ctx, st, None)
    }

def tryApplySchemes(name: String, args: List[Expr], schemes: List[Scheme], ctx: TcContext, st: TcState, firstError: Option[String]): TcResult[Inferred] =
    match schemes {
        case Nil =>
            match firstError {
                case None => TcResult.Err("unknown name '${name}'")
                case Some(msg) => TcResult.Err(msg)
            }
        case scheme :: rest =>
            match instantiate(scheme, st) {
                case TcResult.Err(msg) => tryApplySchemes(name, args, rest, ctx, st, keepFirstError(firstError, msg))
                case TcResult.Ok(t, st1) =>
                    match inferCurriedApply(Inferred.Value(t, Nil), args, ctx, st1) {
                        case TcResult.Ok(ok, st2) => TcResult.Ok(ok, st2)
                        case TcResult.Err(msg) => tryApplySchemes(name, args, rest, ctx, st, keepFirstError(firstError, msg))
                    }
            }
    }

def keepFirstError(firstError: Option[String], msg: String): Option[String] =
    match firstError {
        case None => Some(msg)
        case Some(_) => firstError
    }

def inferCurriedApply(fn: Inferred, args: List[Expr], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match args {
        case Nil => TcResult.Ok(fn, st)
        case arg :: rest =>
            match fn {
                case Inferred.Value(fTy, fEffs) =>
                    match inferExpr(arg, ctx, st) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(Inferred.Value(argTy, argEffs), st1) =>
                            match fresh(st1) {
                                case TcResult.Err(msg) => TcResult.Err(msg)
                                case TcResult.Ok(retTy, st2) =>
                                    match unifyApplyFunction(fTy, argTy, retTy, st2) {
                                        case TcResult.Err(msg) => TcResult.Err(msg)
                                        case TcResult.Ok(latentEffects, st3) =>
                                            let effects = unionEffects(fEffs, unionEffects(argEffs, latentEffects));
                                            inferCurriedApply(Inferred.Value(applyState(retTy, st3), effects), rest, ctx, st3)
                                    }
                            }
                    }
            }
    }

def unifyApplyFunction(fTy: Ty, argTy: Ty, retTy: Ty, st: TcState): TcResult[List[String]] =
    match normalizeFun(applyState(fTy, st)) {
        case Ty.Fun(param :: restParams, effects, ret) =>
            let remaining = match restParams {
                case Nil => ret
                case _ => curriedFunction(restParams, effects, ret)
            };
            match unify(param, argTy, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) =>
                    match unify(remaining, retTy, st2) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(_, st3) => TcResult.Ok(effects, st3)
                    }
            }
        case Ty.Fun(Nil, effects, ret) =>
            match unify(ret, retTy, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => TcResult.Ok(effects, st2)
            }
        case _ =>
            match unify(fTy, Ty.Fun(argTy :: Nil, Nil, retTy), st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => TcResult.Ok(Nil, st2)
            }
    }

def inferExprList(args: List[Expr], ctx: TcContext, st: TcState, acc: List[Inferred]): TcResult[List[Inferred]] =
    match args {
        case Nil => TcResult.Ok(List.reverse(acc), st)
        case x :: xs =>
            match inferExpr(x, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(inferred, st2) => inferExprList(xs, ctx, st2, inferred :: acc)
            }
    }

def inferredTypes(xs: List[Inferred]): List[Ty] =
    match xs {
        case Nil => Nil
        case Inferred.Value(t, _) :: rest => t :: inferredTypes(rest)
    }

def inferredEffects(xs: List[Inferred]): List[String] =
    match xs {
        case Nil => Nil
        case Inferred.Value(_, effects) :: rest => unionEffects(effects, inferredEffects(rest))
    }

def inferTryHandle(body: Expr, cases: List[HandlerCase], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match inferExpr(body, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(Inferred.Value(bodyTy, bodyEffects), st1) =>
            inferHandlerCases(cases, applyState(bodyTy, st1), bodyEffects, ctx, st1)
    }

def inferHandlerCases(cases: List[HandlerCase], bodyTy: Ty, effects: List[String], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match cases {
        case Nil => TcResult.Ok(Inferred.Value(applyState(bodyTy, st), effects), st)
        case HandlerCase.Case(effectName, handlerBody) :: rest =>
            if (containsString(effectName, effects)) {
                match inferExpr(handlerBody, ctx, st) {
                    case TcResult.Err(msg) => TcResult.Err(msg)
                    case TcResult.Ok(Inferred.Value(handlerTy, handlerEffects), st1) =>
                        match unify(bodyTy, handlerTy, st1) {
                            case TcResult.Err(msg) => TcResult.Err("in handler for '${effectName}': ${msg}")
                            case TcResult.Ok(_, st2) =>
                                let remainingEffects = removeEffect(effectName, effects);
                                inferHandlerCases(rest, applyState(bodyTy, st2), unionEffects(remainingEffects, handlerEffects), ctx, st2)
                        }
                }
            } else {
                TcResult.Err("handler for absent effect '${effectName}'")
            }
    }
def inferLambda(ps: List[Pattern], body: Expr, ctx: TcContext, st: TcState): TcResult[Inferred] =
    match freshMany(List.length(ps), st, Nil) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(paramTys, st1) =>
            match bindPatterns(ps, List.reverse(paramTys), ctx, st1) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(ctx2, st2) =>
                    match inferExpr(body, ctx2, st2) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(Inferred.Value(bodyTy, bodyEffects), st3) => TcResult.Ok(Inferred.Value(curriedFunction(applyStateAll(List.reverse(paramTys), st3), bodyEffects, applyState(bodyTy, st3)), Nil), st3)
                    }
            }
    }

def inferMatch(scrutinees: List[Expr], cases: List[MatchCase], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match scrutinees {
        case Nil =>
            match matchCaseArity(cases) {
                case None => TcResult.Err("empty match")
                case Some(arity) =>
                    match freshMany(arity, st, Nil) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(scrutTys0, st1) =>
                            let scrutTys = List.reverse(scrutTys0);
                            match inferMatchCases(cases, scrutTys, None, ctx, st1) {
                                case TcResult.Err(msg) => TcResult.Err(msg)
                                case TcResult.Ok(Inferred.Value(branchTy, branchEffects), st2) =>
                                    match checkExhaustive(applyStateAll(scrutTys, st2), cases, ctx, st2) {
                                        case TcResult.Err(msg) => TcResult.Err(msg)
                                        case TcResult.Ok(_, st3) => TcResult.Ok(Inferred.Value(curriedFunction(applyStateAll(scrutTys, st3), branchEffects, branchTy), Nil), st3)
                                    }
                            }
                    }
            }
        case _ =>
            match inferExprList(scrutinees, ctx, st, Nil) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(inferred, st1) =>
                    let scrutTys = inferredTypes(inferred);
                    let scrutEffects = inferredEffects(inferred);
                    match inferMatchCases(cases, scrutTys, None, ctx, st1) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(Inferred.Value(branchTy, branchEffects), st2) =>
                            match checkExhaustive(applyStateAll(scrutTys, st2), cases, ctx, st2) {
                                case TcResult.Err(msg) => TcResult.Err(msg)
                                case TcResult.Ok(_, st3) => TcResult.Ok(Inferred.Value(branchTy, unionEffects(scrutEffects, branchEffects)), st3)
                            }
                    }
            }
    }

def matchCaseArity(cases: List[MatchCase]): Option[Int32] =
    match cases {
        case Nil => None
        case MatchCase.Case(ps, _) :: _ => Some(List.length(ps))
    }

def inferMatchCases(cases: List[MatchCase], scrutTys: List[Ty], branchTy: Option[Ty], ctx: TcContext, st: TcState): TcResult[Inferred] =
    match cases {
        case Nil =>
            match branchTy {
                case None => TcResult.Err("empty match")
                case Some(t) => TcResult.Ok(Inferred.Value(applyState(t, st), Nil), st)
            }
        case MatchCase.Case(ps, e) :: rest =>
            match bindPatterns(ps, scrutTys, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(ctx2, st1) =>
                    match inferExpr(e, ctx2, st1) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(Inferred.Value(t, effects), st2) =>
                            match branchTy {
                                case None =>
                                    match inferMatchCases(rest, applyStateAll(scrutTys, st2), Some(t), ctx, st2) {
                                        case TcResult.Err(msg) => TcResult.Err(msg)
                                        case TcResult.Ok(Inferred.Value(restTy, restEffects), st3) => TcResult.Ok(Inferred.Value(restTy, unionEffects(effects, restEffects)), st3)
                                    }
                                case Some(old) =>
                                    match unify(old, t, st2) {
                                        case TcResult.Err(msg) => TcResult.Err(msg)
                                        case TcResult.Ok(_, st3) =>
                                            match inferMatchCases(rest, applyStateAll(scrutTys, st3), Some(applyState(t, st3)), ctx, st3) {
                                                case TcResult.Err(msg) => TcResult.Err(msg)
                                                case TcResult.Ok(Inferred.Value(restTy, restEffects), st4) => TcResult.Ok(Inferred.Value(restTy, unionEffects(effects, restEffects)), st4)
                                            }
                                    }
                            }
                    }
            }
    }
