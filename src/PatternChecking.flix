def bindPatterns(ps: List[Pattern], tys: List[Ty], ctx: TcContext, st: TcState): TcResult[TcContext] =
    match (ps, tys) {
        case (Nil, Nil) => TcResult.Ok(ctx, st)
        case (p :: prest, t :: trest) =>
            match bindPattern(p, t, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(ctx2, st2) => bindPatterns(prest, trest, ctx2, st2)
            }
        case _ => TcResult.Err("pattern arity mismatch")
    }

def bindPattern(p: Pattern, expected: Ty, ctx: TcContext, st: TcState): TcResult[TcContext] =
    match p {
        case Pattern.Var("_") => TcResult.Ok(ctx, st)
        case Pattern.Var(name) => bindBarePatternName(name, expected, ctx, st)
        case Pattern.Con(name, args) =>
            match inferVar(name, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(Inferred.Value(conTy, _), st1) =>
                    match normalizeFun(conTy) {
                        case Ty.Fun(fields, _, result) =>
                            match unify(result, expected, st1) {
                                case TcResult.Err(msg) => TcResult.Err(msg)
                                case TcResult.Ok(_, st2) => bindPatterns(args, fields, ctx, st2)
                            }
                        case _ =>
                            match args {
                                case Nil =>
                                    match unify(conTy, expected, st1) {
                                        case TcResult.Err(msg) => TcResult.Err(msg)
                                        case TcResult.Ok(_, st2) => TcResult.Ok(ctx, st2)
                                    }
                                case _ => TcResult.Err("constructor '${name}' is not a function")
                            }
                    }
            }
    }

def bindBarePatternName(name: String, expected: Ty, ctx: TcContext, st: TcState): TcResult[TcContext] =
    match lookupEnv(name, ctx) {
        case EnvLookup.Missing => bindPatternVariable(name, expected, ctx, st)
        case EnvLookup.Ambiguous(msg) => TcResult.Err(msg)
        case EnvLookup.Found(scheme) =>
            match instantiate(scheme, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(t, st1) =>
                    match normalizeFun(t) {
                        case Ty.Fun(_ :: _, _, _) => bindPatternVariable(name, expected, ctx, st)
                        case Ty.Fun(Nil, _, ret) => bindNullaryPatternOrVariable(name, ret, expected, ctx, st, st1)
                        case _ => bindNullaryPatternOrVariable(name, t, expected, ctx, st, st1)
                    }
            }
    }

def bindNullaryPatternOrVariable(name: String, actual: Ty, expected: Ty, ctx: TcContext, st: TcState, st1: TcState): TcResult[TcContext] =
    match unify(actual, expected, st1) {
        case TcResult.Ok(_, st2) => TcResult.Ok(ctx, st2)
        case TcResult.Err(_) => bindPatternVariable(name, expected, ctx, st)
    }

def bindPatternVariable(name: String, expected: Ty, ctx: TcContext, st: TcState): TcResult[TcContext] =
    TcResult.Ok(addEnv(name, Scheme.Forall(Nil, expected), ctx), st)

def checkExhaustive(scrutTys: List[Ty], cases: List[MatchCase], ctx: TcContext, st: TcState): TcResult[Unit] =
    match uncoveredPatterns(applyStateAll(scrutTys, st), matchRows(cases), ctx, st) {
        case None => TcResult.Ok((), st)
        case Some(witness) => TcResult.Err("non-exhaustive match; missing case " + showPatternList(witness))
    }

def matchRows(cases: List[MatchCase]): List[List[Pattern]] =
    match cases {
        case Nil => Nil
        case MatchCase.Case(ps, _) :: rest => ps :: matchRows(rest)
    }

def uncoveredPatterns(tys: List[Ty], rows: List[List[Pattern]], ctx: TcContext, st: TcState): Option[List[Pattern]] =
    match tys {
        case Nil => if (hasEmptyPatternRow(rows)) None else Some(Nil)
        case ty :: restTys =>
            match constructorsForType(ty, ctx, st) {
                case None => prependWildcard(uncoveredPatterns(restTys, defaultRows(rows), ctx, st))
                case Some(Nil) => None
                case Some(ctors) =>
                    match uncoveredPatterns(restTys, defaultRowsForConstructors(ctorNames(ctors), rows), ctx, st) {
                        case None => None
                        case Some(_) => uncoveredConstructor(ctors, ctorNames(ctors), restTys, rows, ctx, st)
                    }
            }
    }

def uncoveredConstructor(ctors: List[(String, List[Ty])], allCtorNames: List[String], restTys: List[Ty], rows: List[List[Pattern]], ctx: TcContext, st: TcState): Option[List[Pattern]] =
    match ctors {
        case Nil => None
        case (ctorName, fieldTys) :: rest =>
            match uncoveredPatterns(appendList(fieldTys, restTys), specializeRows(ctorName, List.length(fieldTys), allCtorNames, rows), ctx, st) {
                case None => uncoveredConstructor(rest, allCtorNames, restTys, rows, ctx, st)
                case Some(witness) =>
                    let (args, restWitness) = splitPatternPrefix(List.length(fieldTys), witness, Nil);
                    Some(Pattern.Con(lastQualifiedSegment(ctorName), args) :: restWitness)
            }
    }

def ctorNames(ctors: List[(String, List[Ty])]): List[String] =
    match ctors {
        case Nil => Nil
        case (name, _) :: rest => name :: ctorNames(rest)
    }

def constructorsForType(ty: Ty, ctx: TcContext, st: TcState): Option[List[(String, List[Ty])]] =
    match typeHead(applyState(ty, st)) {
        case None => None
        case Some((name, args)) =>
            match findTypeInfo(name, ctx) {
                case Some(TypeInfo.Data(_, params, ctors)) => Some(specializeCtorInfos(params, args, ctors, ctx))
                case _ => None
            }
    }

def typeHead(ty: Ty): Option[(String, List[Ty])] =
    match normalizeFun(ty) {
        case Ty.Con(name) => Some((name, Nil))
        case Ty.App(name, args) => Some((name, args))
        case _ => None
    }

def findTypeInfo(name: String, ctx: TcContext): Option[TypeInfo] =
    match ctx {
        case TcContext.Context(_, types, _) => findTypeInfoIn(name, types)
    }

def findTypeInfoIn(name: String, types: List[(String, TypeInfo)]): Option[TypeInfo] =
    match types {
        case Nil => None
        case (n, info) :: _ if n == name => Some(info)
        case _ :: rest => findTypeInfoIn(name, rest)
    }

def specializeCtorInfos(params: List[String], args: List[Ty], ctors: List[(String, List[TypeExpr])], ctx: TcContext): List[(String, List[Ty])] =
    match ctors {
        case Nil => Nil
        case (name, fields) :: rest =>
            let repls = zipTypeArgs(params, args, Nil);
            let fieldTys = List.map(t -> replaceTyVarsForCoverage(convertTypeExpr(t, ctx), repls), fields);
            (name, fieldTys) :: specializeCtorInfos(params, args, rest, ctx)
    }

def zipTypeArgs(params: List[String], args: List[Ty], acc: List[(String, Ty)]): List[(String, Ty)] =
    match (params, args) {
        case (p :: prest, a :: arest) => zipTypeArgs(prest, arest, (p, a) :: acc)
        case _ => acc
    }

def replaceTyVarsForCoverage(ty: Ty, repls: List[(String, Ty)]): Ty =
    match ty {
        case Ty.Var(name) =>
            match lookupCoverageTy(name, repls) {
                case None => ty
                case Some(replacement) => replacement
            }
        case Ty.App(name, args) => Ty.App(name, List.map(t -> replaceTyVarsForCoverage(t, repls), args))
        case Ty.Record(fields) => Ty.Record(List.map(p -> match p { case (name, t) => (name, replaceTyVarsForCoverage(t, repls)) }, fields))
        case Ty.Fun(args, effects, ret) => Ty.Fun(List.map(t -> replaceTyVarsForCoverage(t, repls), args), List.map(t -> replaceTyVarsForCoverageEffect(t, repls), effects), replaceTyVarsForCoverage(ret, repls))
        case _ => ty
    }

def replaceTyVarsForCoverageEffect(effect: Ty, repls: List[(String, Ty)]): Ty =
    if (isEffectVarTy(effect)) effect else replaceTyVarsForCoverage(effect, repls)

def lookupCoverageTy(name: String, repls: List[(String, Ty)]): Option[Ty] =
    match repls {
        case Nil => None
        case (n, t) :: _ if n == name => Some(t)
        case _ :: rest => lookupCoverageTy(name, rest)
    }

def specializeRows(ctorName: String, arity: Int32, allCtorNames: List[String], rows: List[List[Pattern]]): List[List[Pattern]] =
    specializeRowsLoop(ctorName, arity, allCtorNames, rows, Nil)

def specializeRowsLoop(ctorName: String, arity: Int32, allCtorNames: List[String], rows: List[List[Pattern]], acc: List[List[Pattern]]): List[List[Pattern]] =
    match rows {
        case Nil => List.reverse(acc)
        case row :: rest =>
            match specializeRow(ctorName, arity, allCtorNames, row) {
                case None => specializeRowsLoop(ctorName, arity, allCtorNames, rest, acc)
                case Some(specialized) => specializeRowsLoop(ctorName, arity, allCtorNames, rest, specialized :: acc)
            }
    }

def specializeRow(ctorName: String, arity: Int32, allCtorNames: List[String], row: List[Pattern]): Option[List[Pattern]] =
    match row {
        case Nil => None
        case Pattern.Var(name) :: rest if isNullaryConstructorPatternFor(name, ctorName, arity) => Some(rest)
        case Pattern.Var(name) :: _ if isConstructorPatternName(name, allCtorNames) => None
        case Pattern.Var(_) :: rest => Some(appendList(wildcardPatterns(arity), rest))
        case Pattern.Con(name, args) :: rest if constructorNamesMatch(name, ctorName) => Some(appendList(args, rest))
        case _ => None
    }

def defaultRows(rows: List[List[Pattern]]): List[List[Pattern]] =
    defaultRowsLoop(rows, Nil)

def defaultRowsLoop(rows: List[List[Pattern]], acc: List[List[Pattern]]): List[List[Pattern]] =
    match rows {
        case Nil => List.reverse(acc)
        case row :: rest =>
            match row {
                case Pattern.Var(_) :: ps => defaultRowsLoop(rest, ps :: acc)
                case _ => defaultRowsLoop(rest, acc)
            }
    }

def defaultRowsForConstructors(ctorNames0: List[String], rows: List[List[Pattern]]): List[List[Pattern]] =
    defaultRowsForConstructorsLoop(ctorNames0, rows, Nil)

def defaultRowsForConstructorsLoop(ctorNames0: List[String], rows: List[List[Pattern]], acc: List[List[Pattern]]): List[List[Pattern]] =
    match rows {
        case Nil => List.reverse(acc)
        case row :: rest =>
            match row {
                case Pattern.Var(name) :: _ if isConstructorPatternName(name, ctorNames0) => defaultRowsForConstructorsLoop(ctorNames0, rest, acc)
                case Pattern.Var(_) :: ps => defaultRowsForConstructorsLoop(ctorNames0, rest, ps :: acc)
                case _ => defaultRowsForConstructorsLoop(ctorNames0, rest, acc)
            }
    }

def wildcardPatterns(n: Int32): List[Pattern] =
    if (n <= 0) Nil else Pattern.Var("_") :: wildcardPatterns(n - 1)

def constructorNamesMatch(patternName: String, ctorName: String): Bool =
    patternName == ctorName or lastQualifiedSegment(patternName) == lastQualifiedSegment(ctorName)

def isNullaryConstructorPatternFor(patternName: String, ctorName: String, arity: Int32): Bool =
    arity == 0 and isConstructorPatternName(patternName, ctorName :: Nil)

def isConstructorPatternName(patternName: String, ctorNames0: List[String]): Bool =
    match ctorNames0 {
        case Nil => false
        case ctorName :: _ if constructorNamesMatch(patternName, ctorName) => true
        case _ :: rest => isConstructorPatternName(patternName, rest)
    }

def hasEmptyPatternRow(rows: List[List[Pattern]]): Bool =
    match rows {
        case Nil => false
        case Nil :: _ => true
        case _ :: rest => hasEmptyPatternRow(rest)
    }

def prependWildcard(result: Option[List[Pattern]]): Option[List[Pattern]] =
    match result {
        case None => None
        case Some(ps) => Some(Pattern.Var("_") :: ps)
    }

def splitPatternPrefix(n: Int32, xs: List[Pattern], acc: List[Pattern]): (List[Pattern], List[Pattern]) =
    if (n <= 0) {
        (List.reverse(acc), xs)
    } else {
        match xs {
            case Nil => (List.reverse(acc), Nil)
            case x :: rest => splitPatternPrefix(n - 1, rest, x :: acc)
        }
    }

def showPatternList(ps: List[Pattern]): String =
    match ps {
        case Nil => "()"
        case p :: Nil => showPattern(p)
        case p :: rest => showPattern(p) + ", " + showPatternList(rest)
    }

def showPattern(p: Pattern): String =
    match p {
        case Pattern.Var(name) => name
        case Pattern.Con(name, Nil) => name
        case Pattern.Con(name, args) => "$" + name + " " + showPatternList(args)
    }
