def curriedFunction(args: List[Ty], effects: List[Ty], ret: Ty): Ty =
    match args {
        case Nil =>
            match effects {
                case Nil => ret
                case _ => Ty.Fun(Nil, effects, ret)
            }
        case arg :: rest => Ty.Fun(arg :: Nil, effects, curriedFunction(rest, effects, ret))
    }

def splitComputationType(ty: Ty): (Ty, List[Ty]) =
    match normalizeFun(ty) {
        case Ty.Fun(Nil, effects, ret) => (ret, effects)
        case other => (other, Nil)
    }

def applyStateEffects(effects: List[Ty], st: TcState): List[Ty] =
    List.map(e -> if (isEffectVarTy(e)) e else applyState(e, st), effects)

def unionEffects(xs: List[Ty], ys: List[Ty]): List[Ty] =
    match xs {
        case Nil => ys
        case x :: rest => unionEffects(rest, addEffect(x, ys))
    }

def removeEffect(x: String, xs: List[Ty]): List[Ty] =
    match xs {
        case Nil => Nil
        case y :: rest if effectHasName(x, y) => removeEffect(x, rest)
        case y :: rest => y :: removeEffect(x, rest)
    }

def containsEffectName(x: String, xs: List[Ty]): Bool =
    match xs {
        case Nil => false
        case y :: _ if effectHasName(x, y) => true
        case _ :: rest => containsEffectName(x, rest)
    }

def addEffect(x: Ty, xs: List[Ty]): List[Ty] =
    if (containsEffect(x, xs)) xs else x :: xs

def sameEffects(xs: List[Ty], ys: List[Ty]): Bool =
    match unifyEffects(xs, ys, TcState.State(0, Nil, Nil)) {
        case TcResult.Ok(_, _) => true
        case TcResult.Err(_) => false
    }

def unifyEffects(xs: List[Ty], ys: List[Ty], st: TcState): TcResult[Unit] =
    let xLabels = concreteEffects(xs);
    let yLabels = concreteEffects(ys);
    let xOpen = hasEffectVar(xs);
    let yOpen = hasEffectVar(ys);
    match unifyCommonEffects(xLabels, yLabels, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(_, st1) =>
            let xFitsY = allEffectsIn(xLabels, yLabels) or yOpen;
            let yFitsX = allEffectsIn(yLabels, xLabels) or xOpen;
            if (xFitsY and yFitsX) TcResult.Ok((), st1) else TcResult.Err("cannot unify effects {" + showEffects(xs) + "} with {" + showEffects(ys) + "}")
    }

def concreteEffects(xs: List[Ty]): List[Ty] =
    match xs {
        case Nil => Nil
        case x :: rest if isEffectVarTy(x) => concreteEffects(rest)
        case x :: rest => x :: concreteEffects(rest)
    }

def hasEffectVar(xs: List[Ty]): Bool =
    match xs {
        case Nil => false
        case x :: _ if isEffectVarTy(x) => true
        case _ :: rest => hasEffectVar(rest)
    }

def isEffectVarTy(x: Ty): Bool =
    match x {
        case Ty.Var(name) => isEffectVarName(name)
        case _ => false
    }

def isEffectVarName(x: String): Bool =
    match String.toList(x) {
        case 'e' :: Nil => true
        case 'e' :: rest => allEffectVarNameChars(rest)
        case Nil => false
        case _ => false
    }

def allEffectVarNameChars(cs: List[Char]): Bool =
    match cs {
        case Nil => true
        case c :: rest => isEffectDigit(c) and allEffectVarNameChars(rest)
    }

def isEffectDigit(c: Char): Bool =
    c >= '0' and c <= '9'


def isLowerAlpha(c: Char): Bool =
    c >= 'a' and c <= 'z'

def allEffectsIn(xs: List[Ty], ys: List[Ty]): Bool =
    match xs {
        case Nil => true
        case x :: rest => containsEffectHead(x, ys) and allEffectsIn(rest, ys)
    }

def unifyCommonEffects(xs: List[Ty], ys: List[Ty], st: TcState): TcResult[Unit] =
    match xs {
        case Nil => TcResult.Ok((), st)
        case x :: rest =>
            match findEffectByHead(x, ys) {
                case None => unifyCommonEffects(rest, ys, st)
                case Some(y) =>
                    match unifyEffect(x, y, st) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(_, st1) => unifyCommonEffects(rest, ys, st1)
                    }
            }
    }

def unifyEffect(x: Ty, y: Ty, st: TcState): TcResult[Unit] =
    match (effectNameAndArgs(x), effectNameAndArgs(y)) {
        case (Some((xName, xArgs)), Some((yName, yArgs))) if xName == yName => unifyLists(xArgs, yArgs, st)
        case _ => TcResult.Err("cannot unify effect " + showEffect(x) + " with " + showEffect(y))
    }

def findEffectByHead(x: Ty, ys: List[Ty]): Option[Ty] =
    match ys {
        case Nil => None
        case y :: _ if sameEffectHead(x, y) => Some(y)
        case _ :: rest => findEffectByHead(x, rest)
    }

def containsEffect(x: Ty, ys: List[Ty]): Bool =
    match ys {
        case Nil => false
        case y :: _ if sameEffectTy(x, y) => true
        case _ :: rest => containsEffect(x, rest)
    }

def containsEffectHead(x: Ty, ys: List[Ty]): Bool =
    match ys {
        case Nil => false
        case y :: _ if sameEffectHead(x, y) => true
        case _ :: rest => containsEffectHead(x, rest)
    }

def sameEffectHead(x: Ty, y: Ty): Bool =
    match (effectNameAndArgs(x), effectNameAndArgs(y)) {
        case (Some((xName, _)), Some((yName, _))) => xName == yName
        case _ => false
    }

def sameEffectTy(x: Ty, y: Ty): Bool =
    match (x, y) {
        case (Ty.Meta(xId), Ty.Meta(yId)) => xId == yId
        case (Ty.Var(xName), Ty.Var(yName)) => xName == yName
        case (Ty.Con(xName), Ty.Con(yName)) => xName == yName
        case (Ty.App(xName, xs), Ty.App(yName, ys)) => xName == yName and sameTyList(xs, ys)
        case (Ty.Record(xs), Ty.Record(ys)) => sameRecordTyList(xs, ys)
        case (Ty.Fun(xArgs, xEffs, xRet), Ty.Fun(yArgs, yEffs, yRet)) => sameTyList(xArgs, yArgs) and sameTyList(xEffs, yEffs) and sameEffectTy(xRet, yRet)
        case _ => false
    }

def sameTyList(xs: List[Ty], ys: List[Ty]): Bool =
    match (xs, ys) {
        case (Nil, Nil) => true
        case (x :: xr, y :: yr) => sameEffectTy(x, y) and sameTyList(xr, yr)
        case _ => false
    }

def sameRecordTyList(xs: List[(String, Ty)], ys: List[(String, Ty)]): Bool =
    match (xs, ys) {
        case (Nil, Nil) => true
        case ((xName, xTy) :: xr, (yName, yTy) :: yr) => xName == yName and sameEffectTy(xTy, yTy) and sameRecordTyList(xr, yr)
        case _ => false
    }

def effectHasName(name: String, effect: Ty): Bool =
    match effectNameAndArgs(effect) {
        case Some((effectName, _)) => name == effectName
        case None => false
    }

def effectNameAndArgs(effect: Ty): Option[(String, List[Ty])] =
    match effect {
        case Ty.App(name, args) => Some((name, args))
        case Ty.Con(name) => Some((name, Nil))
        case _ => None
    }

def showEffects(xs: List[Ty]): String =
    match xs {
        case Nil => ""
        case x :: Nil => showEffect(x)
        case x :: rest => showEffect(x) + ", " + showEffects(rest)
    }

def showEffect(effect: Ty): String =
    match effect {
        case Ty.Var(name) => name
        case Ty.App(name, Nil) => name
        case Ty.App(name, args) => showTyList(args) + " " + name
        case other => showTy(other)
    }
def unify(a: Ty, b: Ty, st: TcState): TcResult[Unit] =
    let aa = normalizeFun(applyState(a, st));
    let bb = normalizeFun(applyState(b, st));
    match (aa, bb) {
        case (Ty.Meta(x), Ty.Meta(y)) if x == y => TcResult.Ok((), st)
        case (Ty.Meta(x), t) => bindMeta(x, t, st)
        case (t, Ty.Meta(x)) => bindMeta(x, t, st)
        case (Ty.Var(x), Ty.Var(y)) if x == y => TcResult.Ok((), st)
        case (Ty.Con(x), Ty.Con(y)) if x == y => TcResult.Ok((), st)
        case (Ty.App(x, xs), Ty.App(y, ys)) if x == y => unifyLists(xs, ys, st)
        case (Ty.App(x, xs), Ty.App(y, ys)) if isTypeVarName(x) => unifyTypeHead(x, y, xs, ys, st)
        case (Ty.App(x, xs), Ty.App(y, ys)) if isTypeVarName(y) => unifyTypeHead(y, x, ys, xs, st)
        case (Ty.Record(xs), Ty.Record(ys)) => unifyRecordFields(xs, ys, st)
        case (Ty.Fun(xs, xEffs, x), Ty.Fun(ys, yEffs, y)) =>
            match unifyEffects(xEffs, yEffs, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st1) =>
                    match unifyLists(xs, ys, st1) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(_, st2) => unify(x, y, st2)
                    }
            }
        case _ => TcResult.Err("cannot unify ${showTy(aa)} with ${showTy(bb)}")
    }

def normalizeFun(ty: Ty): Ty =
    match ty {
        case Ty.Fun(args, effects, ret) =>
            match normalizeFun(ret) {
                case Ty.Fun(moreArgs, moreEffects, finalRet) => Ty.Fun(appendList(args, moreArgs), unionEffects(effects, moreEffects), finalRet)
                case other => Ty.Fun(args, effects, other)
            }
        case Ty.App(name, args) => Ty.App(name, List.map(normalizeFun, args))
        case Ty.Record(fields) => Ty.Record(List.map(p -> match p { case (name, t) => (name, normalizeFun(t)) }, fields))
        case _ => ty
    }

def unifyLists(xs: List[Ty], ys: List[Ty], st: TcState): TcResult[Unit] =
    match (xs, ys) {
        case (Nil, Nil) => TcResult.Ok((), st)
        case (x :: xr, y :: yr) =>
            match unify(x, y, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => unifyLists(xr, yr, st2)
            }
        case _ => TcResult.Err("arity mismatch: [" + showTyList(xs) + "] vs [" + showTyList(ys) + "]")
    }

def unifyTypeHead(varName: String, actualName: String, varArgs: List[Ty], actualArgs: List[Ty], st: TcState): TcResult[Unit] =
    match lookupTypeHead(varName, st) {
        case Some(boundName) if boundName == actualName => unifyLists(varArgs, actualArgs, st)
        case Some(boundName) => TcResult.Err("cannot unify ${boundName} with ${actualName}")
        case None =>
            if (isTypeVarName(actualName)) {
                unifyLists(varArgs, actualArgs, st)
            } else {
                match bindTypeHead(varName, actualName, st) {
                    case TcResult.Err(msg) => TcResult.Err(msg)
                    case TcResult.Ok(_, st2) => unifyLists(varArgs, actualArgs, st2)
                }
            }
    }

def lookupTypeHead(name: String, st: TcState): Option[String] =
    match st {
        case TcState.State(_, _, heads) => lookupTypeHeadIn(name, heads)
    }

def lookupTypeHeadIn(name: String, heads: List[(String, String)]): Option[String] =
    match heads {
        case Nil => None
        case (n, value) :: _ if n == name => Some(value)
        case _ :: rest => lookupTypeHeadIn(name, rest)
    }

def bindTypeHead(name: String, value: String, st: TcState): TcResult[Unit] =
    match st {
        case TcState.State(next, subst, heads) => TcResult.Ok((), TcState.State(next, subst, (name, value) :: heads))
    }

def unifyRecordFields(xs: List[(String, Ty)], ys: List[(String, Ty)], st: TcState): TcResult[Unit] =
    match (xs, ys) {
        case (Nil, Nil) => TcResult.Ok((), st)
        case ((xName, xTy) :: xr, (yName, yTy) :: yr) if xName == yName =>
            match unify(xTy, yTy, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => unifyRecordFields(xr, yr, st2)
            }
        case ((xName, _) :: _, (yName, _) :: _) => TcResult.Err("record field mismatch '${xName}' vs '${yName}'")
        case _ => TcResult.Err("record arity mismatch")
    }

def bindMeta(id: Int32, ty: Ty, st: TcState): TcResult[Unit] =
    if (occursMeta(id, ty, st)) {
        TcResult.Err("recursive type")
    } else {
        match st {
            case TcState.State(next, subst, heads) => TcResult.Ok((), TcState.State(next, (id, ty) :: subst, heads))
        }
    }

def occursMeta(id: Int32, ty: Ty, st: TcState): Bool =
    match applyState(ty, st) {
        case Ty.Meta(x) => x == id
        case Ty.Var(_) => false
        case Ty.Con(_) => false
        case Ty.App(_, args) => occursMetaList(id, args, st)
        case Ty.Record(fields) => occursMetaRecordFields(id, fields, st)
        case Ty.Fun(args, effects, ret) => occursMetaList(id, args, st) or occursMetaList(id, concreteEffects(effects), st) or occursMeta(id, ret, st)
    }

def occursMetaRecordFields(id: Int32, fields: List[(String, Ty)], st: TcState): Bool =
    match fields {
        case Nil => false
        case (_, t) :: rest => occursMeta(id, t, st) or occursMetaRecordFields(id, rest, st)
    }

def occursMetaList(id: Int32, tys: List[Ty], st: TcState): Bool =
    match tys {
        case Nil => false
        case t :: rest => occursMeta(id, t, st) or occursMetaList(id, rest, st)
    }

def applyState(ty: Ty, st: TcState): Ty =
    match st {
        case TcState.State(_, subst, heads) => applyTypeHeadSubst(applySubst(ty, subst), heads)
    }

def applyStateAll(tys: List[Ty], st: TcState): List[Ty] =
    List.map(t -> applyState(t, st), tys)

def applySubst(ty: Ty, subst: List[(Int32, Ty)]): Ty =
    match ty {
        case Ty.Meta(id) =>
            match lookupSubst(id, subst) {
                case None => ty
                case Some(t) => applySubst(t, subst)
            }
        case Ty.App(name, args) => Ty.App(name, List.map(t -> applySubst(t, subst), args))
        case Ty.Record(fields) => Ty.Record(List.map(p -> match p { case (name, t) => (name, applySubst(t, subst)) }, fields))
        case Ty.Fun(args, effects, ret) => Ty.Fun(List.map(t -> applySubst(t, subst), args), List.map(t -> applySubstEffect(t, subst), effects), applySubst(ret, subst))
        case _ => ty
    }

def applySubstEffect(effect: Ty, subst: List[(Int32, Ty)]): Ty =
    if (isEffectVarTy(effect)) effect else applySubst(effect, subst)

def applyTypeHeadSubst(ty: Ty, heads: List[(String, String)]): Ty =
    match ty {
        case Ty.App(name, args) =>
            let name2 = match lookupTypeHeadIn(name, heads) {
                case None => name
                case Some(boundName) => boundName
            };
            Ty.App(name2, List.map(t -> applyTypeHeadSubst(t, heads), args))
        case Ty.Record(fields) => Ty.Record(List.map(p -> match p { case (name, t) => (name, applyTypeHeadSubst(t, heads)) }, fields))
        case Ty.Fun(args, effects, ret) => Ty.Fun(List.map(t -> applyTypeHeadSubst(t, heads), args), List.map(t -> applyTypeHeadSubstEffect(t, heads), effects), applyTypeHeadSubst(ret, heads))
        case _ => ty
    }

def applyTypeHeadSubstEffect(effect: Ty, heads: List[(String, String)]): Ty =
    if (isEffectVarTy(effect)) effect else applyTypeHeadSubst(effect, heads)

def lookupSubst(id: Int32, subst: List[(Int32, Ty)]): Option[Ty] =
    match subst {
        case Nil => None
        case (x, t) :: _ if x == id => Some(t)
        case _ :: rest => lookupSubst(id, rest)
    }

def fresh(st: TcState): TcResult[Ty] =
    match st {
        case TcState.State(next, subst, heads) => TcResult.Ok(Ty.Meta(next), TcState.State(next + 1, subst, heads))
    }

def freshMany(n: Int32, st: TcState, acc: List[Ty]): TcResult[List[Ty]] =
    if (n <= 0) {
        TcResult.Ok(acc, st)
    } else {
        match fresh(st) {
            case TcResult.Err(msg) => TcResult.Err(msg)
            case TcResult.Ok(t, st2) => freshMany(n - 1, st2, t :: acc)
        }
    }

def instantiate(s: Scheme, st: TcState): TcResult[Ty] =
    match s {
        case Scheme.Forall(vars, ty) =>
            match freshForNames(vars, st, Nil) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(repls, st2) => TcResult.Ok(replaceVars(ty, repls), st2)
            }
    }

def freshForNames(vars: List[String], st: TcState, acc: List[(String, Ty)]): TcResult[List[(String, Ty)]] =
    match vars {
        case Nil => TcResult.Ok(acc, st)
        case v :: rest =>
            match fresh(st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(t, st2) => freshForNames(rest, st2, (v, t) :: acc)
            }
    }

def replaceVars(ty: Ty, repls: List[(String, Ty)]): Ty =
    match ty {
        case Ty.Var(v) =>
            match lookupNameTy(v, repls) {
                case None => ty
                case Some(t) => t
            }
        case Ty.App(name, args) => Ty.App(name, List.map(t -> replaceVars(t, repls), args))
        case Ty.Record(fields) => Ty.Record(List.map(p -> match p { case (name, t) => (name, replaceVars(t, repls)) }, fields))
        case Ty.Fun(args, effects, ret) => Ty.Fun(List.map(t -> replaceVars(t, repls), args), List.map(t -> replaceVarsInEffect(t, repls), effects), replaceVars(ret, repls))
        case _ => ty
    }

def replaceVarsInEffect(effect: Ty, repls: List[(String, Ty)]): Ty =
    if (isEffectVarTy(effect)) effect else replaceVars(effect, repls)

def lookupNameTy(name: String, xs: List[(String, Ty)]): Option[Ty] =
    match xs {
        case Nil => None
        case (n, t) :: _ if n == name => Some(t)
        case _ :: rest => lookupNameTy(name, rest)
    }

def typeExprScheme(t: TypeExpr, ctx: TcContext): Scheme =
    let ty = convertTypeExpr(t, ctx);
    generalizeTy(ty)

def typeExprBindingScheme(t: TypeExpr, ctx: TcContext): Scheme =
    let ty = convertTypeExpr(t, ctx);
    let (value, _) = splitComputationType(ty);
    generalizeTy(value)

def convertTypeExpr(t: TypeExpr, ctx: TcContext): Ty =
    match t {
        case TypeExpr.Name(name) => atomTypeName(name, ctx)
        case TypeExpr.Apply(name, args) => atomAppliedTypeName(name, List.map(x -> convertTypeExpr(x, ctx), args), ctx)
        case TypeExpr.Record(fields) => Ty.Record(List.map(p -> match p { case (name, fieldTy) => (name, convertTypeExpr(fieldTy, ctx)) }, fields))
        case TypeExpr.Arrow(args, effects, ret) => curriedFunction(List.map(x -> convertTypeExpr(x, ctx), args), List.map(e -> convertEffectExpr(e, ctx), effects), convertTypeExpr(ret, ctx))
        case TypeExpr.Forall(_, body) => convertTypeExpr(body, ctx)
    }

def convertEffectExpr(t: TypeExpr, ctx: TcContext): Ty =
    match t {
        case TypeExpr.Name(name) if isEffectVarName(name) => Ty.Var(name)
        case TypeExpr.Name(name) => Ty.App(canonicalEffectName(name, ctx), Nil)
        case TypeExpr.Apply(name, args) => Ty.App(canonicalEffectName(name, ctx), List.map(x -> convertTypeExpr(x, ctx), args))
        case _ => convertTypeExpr(t, ctx)
    }

def canonicalEffectName(name: String, _ctx: TcContext): String =
    name

def typeExprFromTy(ty: Ty, st: TcState): TypeExpr =
    typeExprFromAppliedTy(normalizeFun(applyState(ty, st)))

def typeExprFromAppliedTy(ty: Ty): TypeExpr =
    match ty {
        case Ty.Meta(id) => TypeExpr.Name("t${id}")
        case Ty.Var(name) => TypeExpr.Name(name)
        case Ty.Con(name) => TypeExpr.Name(name)
        case Ty.App(name, args) => TypeExpr.Apply(name, List.map(typeExprFromAppliedTy, args))
        case Ty.Record(fields) => TypeExpr.Record(List.map(p -> match p { case (name, fieldTy) => (name, typeExprFromAppliedTy(fieldTy)) }, fields))
        case Ty.Fun(args, effects, ret) => TypeExpr.Arrow(List.map(typeExprFromAppliedTy, args), List.map(typeExprFromEffectTy, effects), typeExprFromAppliedTy(ret))
    }

def typeExprFromEffectTy(ty: Ty): TypeExpr =
    typeExprFromAppliedTy(ty)

def specializeBoneType(boneParams: List[String], fleshType: TypeExpr, memberType: TypeExpr): TypeExpr =
    match boneParams {
        case param :: _ => substituteTypeExpr(param, fleshType, memberType)
        case Nil => memberType
    }

def substituteTypeExpr(param: String, replacement: TypeExpr, t: TypeExpr): TypeExpr =
    match t {
        case TypeExpr.Name(name) if name == param => replacement
        case TypeExpr.Name(_) => t
        case TypeExpr.Apply(name, args) if name == param => applyReplacementType(replacement, List.map(x -> substituteTypeExpr(param, replacement, x), args))
        case TypeExpr.Apply(name, args) => TypeExpr.Apply(name, List.map(x -> substituteTypeExpr(param, replacement, x), args))
        case TypeExpr.Record(fields) => TypeExpr.Record(List.map(p -> match p { case (name, fieldTy) => (name, substituteTypeExpr(param, replacement, fieldTy)) }, fields))
        case TypeExpr.Arrow(args, effects, ret) => TypeExpr.Arrow(List.map(x -> substituteTypeExpr(param, replacement, x), args), List.map(x -> substituteTypeExpr(param, replacement, x), effects), substituteTypeExpr(param, replacement, ret))
        case TypeExpr.Forall(params, body) => TypeExpr.Forall(params, substituteTypeExpr(param, replacement, body))
    }

def applyReplacementType(replacement: TypeExpr, args: List[TypeExpr]): TypeExpr =
    match replacement {
        case TypeExpr.Name(name) => TypeExpr.Apply(name, args)
        case TypeExpr.Apply(name, existing) => TypeExpr.Apply(name, appendList(existing, args))
        case _ => replacement
    }

def atomTypeName(name: String, ctx: TcContext): Ty =
    if (isPrimitiveConcreteTypeName(name)) {
        Ty.Con(name)
    } else {
        match canonicalTypeName(name, ctx) {
            case Some(canonical) => Ty.Con(canonical)
            case None => Ty.Var(name)
        }
    }

def atomAppliedTypeName(name: String, args: List[Ty], ctx: TcContext): Ty =
    match canonicalTypeName(name, ctx) {
        case Some(canonical) => Ty.App(canonical, args)
        case None => Ty.App(name, args)
    }

def isConcreteTypeName(name: String, ctx: TcContext): Bool =
    isPrimitiveConcreteTypeName(name) or hasTypeInfo(name, ctx)

def isPrimitiveConcreteTypeName(name: String): Bool =
    name == "integer" or name == "float" or name == "char" or name == "character" or name == "string" or name == "𝟘" or name == "𝟙" or name == "𝟚"

def canonicalTypeName(name: String, ctx: TcContext): Option[String] =
    match findTypeInfoForTypeSystem(name, ctx) {
        case Some(TypeInfo.Alias(canonical, _)) => Some(canonical)
        case Some(TypeInfo.Data(canonical, _, _)) => Some(canonical)
        case None => None
    }

def hasTypeInfo(name: String, ctx: TcContext): Bool =
    match findTypeInfoForTypeSystem(name, ctx) {
        case Some(_) => true
        case None => false
    }

def findTypeInfoForTypeSystem(name: String, ctx: TcContext): Option[TypeInfo] =
    match ctx {
        case TcContext.Context(_, types, _) => findTypeInfoInTypeSystem(name, types)
    }

def findTypeInfoInTypeSystem(name: String, types: List[(String, TypeInfo)]): Option[TypeInfo] =
    match types {
        case Nil => None
        case (n, info) :: _ if n == name => Some(info)
        case _ :: rest => findTypeInfoInTypeSystem(name, rest)
    }

def typeVarsInTy(ty: Ty, acc: List[String]): List[String] =
    match ty {
        case Ty.Var(v) => addUnique(v, acc)
        case Ty.App(_, args) => typeVarsInList(args, acc)
        case Ty.Record(fields) => typeVarsInRecordFields(fields, acc)
        case Ty.Fun(args, effects, ret) => typeVarsInTy(ret, typeVarsInEffects(effects, typeVarsInList(args, acc)))
        case _ => acc
    }

def typeVarsInEffects(effects: List[Ty], acc: List[String]): List[String] =
    match effects {
        case Nil => acc
        case effect :: rest if isEffectVarTy(effect) => typeVarsInEffects(rest, acc)
        case effect :: rest => typeVarsInEffects(rest, typeVarsInTy(effect, acc))
    }

def typeVarsInRecordFields(fields: List[(String, Ty)], acc: List[String]): List[String] =
    match fields {
        case Nil => acc
        case (_, t) :: rest => typeVarsInRecordFields(rest, typeVarsInTy(t, acc))
    }

def typeVarsInList(tys: List[Ty], acc: List[String]): List[String] =
    match tys {
        case Nil => acc
        case t :: rest => typeVarsInList(rest, typeVarsInTy(t, acc))
    }

def addUnique(x: String, xs: List[String]): List[String] =
    if (containsString(x, xs)) xs else x :: xs

def containsString(x: String, xs: List[String]): Bool =
    match xs {
        case Nil => false
        case y :: _ if x == y => true
        case _ :: rest => containsString(x, rest)
    }
def showTy(ty: Ty): String =
    match ty {
        case Ty.Meta(id) => "?${id}"
        case Ty.Var(v) => v
        case Ty.Con(c) => c
        case Ty.App(name, args) => name + "<" + showTyList(args) + ">"
        case Ty.Record(fields) => "[" + showRecordFields(fields) + "]"
        case Ty.Fun(args, effects, ret) =>
            let effectText = match effects {
                case Nil => ""
                case _ => "{" + showEffects(effects) + "} "
            };
            "(" + showTyList(args) + " \\" + effectText + showTy(ret) + ")"
    }

def showRecordFields(fields: List[(String, Ty)]): String =
    match fields {
        case Nil => ""
        case (name, ty) :: Nil => name + ": " + showTy(ty)
        case (name, ty) :: rest => name + ": " + showTy(ty) + ", " + showRecordFields(rest)
    }

def showTyList(tys: List[Ty]): String =
    match tys {
        case Nil => ""
        case t :: Nil => showTy(t)
        case t :: rest => showTy(t) + ", " + showTyList(rest)
    }
