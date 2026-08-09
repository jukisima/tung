def checkDecls(ds: List[Decl], ctx: TcContext, st: TcState): TcResult[Program] =
    match checkDeclsContext(ds, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(_, st2) => TcResult.Ok(Program.Decls(Nil), st2)
    }

def checkDeclsContext(ds: List[Decl], ctx: TcContext, st: TcState): TcResult[TcContext] =
    match ds {
        case Nil => TcResult.Ok(ctx, st)
        case Decl.Let(name, None, expr) :: rest =>
            match inferUnannotatedBinding(name, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((ctx2, _), st2) => checkDeclsContext(rest, ctx2, st2)
            }
        case d :: rest =>
            match checkDecl(d, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => checkDeclsContext(rest, ctx, st2)
            }
    }

def checkDecl(d: Decl, ctx: TcContext, st: TcState): TcResult[Decl] =
    match d {
        case Decl.Import(_) => TcResult.Ok(d, st)
        case Decl.Let(name, ann, expr) => checkLet(name, ann, expr, ctx, st)
        case Decl.TypeAlias(_, _) => TcResult.Ok(d, st)
        case Decl.Data(_, _, _) => TcResult.Ok(d, st)
        case Decl.Effect(_, _) => TcResult.Ok(d, st)
        case Decl.Bone(_, boneName, members) => checkBoneDefaults(boneName, members, ctx, st, d)
        case Decl.Flesh(ty, boneName, members) => checkFlesh(ty, boneName, members, ctx, st)
    }

def inferBoneDefaultDecls(ds: List[Decl], ctx: TcContext, st: TcState, acc: List[Decl]): TcResult[(List[Decl], TcContext)] =
    match ds {
        case Nil => TcResult.Ok((List.reverse(acc), ctx), st)
        case Decl.Bone(params, boneName, members) :: rest =>
            let knownCtx = addBoneMembers(boneName, params, members, ctx);
            match inferBoneDefaultMembers(params, boneName, members, knownCtx, st, Nil) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(typedMembers, st2) =>
                    let typedDecl = Decl.Bone(params, boneName, List.reverse(typedMembers));
                    let ctx2 = addBoneMembers(boneName, params, List.reverse(typedMembers), ctx);
                    inferBoneDefaultDecls(rest, ctx2, st2, typedDecl :: acc)
            }
        case d :: rest =>
            inferBoneDefaultDecls(rest, collectDeclContext(d, ctx), st, d :: acc)
    }

def inferBoneDefaultMembers(boneParams: List[String], boneName: String, members: List[BoneMember], ctx: TcContext, st: TcState, acc: List[BoneMember]): TcResult[List[BoneMember]] =
    match members {
        case Nil => TcResult.Ok(acc, st)
        case BoneMember.Spec(name, t) :: rest =>
            let member = BoneMember.Spec(name, t);
            inferBoneDefaultMembers(boneParams, boneName, rest, ctx, st, member :: acc)
        case BoneMember.Default(name, Some(t), expr) :: rest =>
            let member = BoneMember.Default(name, Some(t), expr);
            let ctx2 = addBoneMembersToEnv(boneName, member :: Nil, ctx);
            inferBoneDefaultMembers(boneParams, boneName, rest, ctx2, st, member :: acc)
        case BoneMember.Default(name, None, expr) :: rest =>
            match inferBoneDefaultType(boneParams, name, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err("in bone default '${name}': ${msg}")
                case TcResult.Ok(t, st2) =>
                    let member = BoneMember.Default(name, Some(t), expr);
                    let ctx2 = addBoneMembersToEnv(boneName, member :: Nil, ctx);
                    inferBoneDefaultMembers(boneParams, boneName, rest, ctx2, st2, member :: acc)
            }
    }

def inferBoneDefaultType(boneParams: List[String], name: String, expr: Expr, ctx: TcContext, st: TcState): TcResult[TypeExpr] =
    match expr {
        case Expr.Lambda(ps, body) =>
            let argTypeExprs = defaultBoneArgumentTypes(boneParams, List.length(ps), Nil);
            let argTys = List.map(t -> convertTypeExpr(t, ctx), argTypeExprs);
            match bindPatterns(ps, argTys, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(ctx2, st1) =>
                    match inferNamed(name, body, ctx2, st1) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(Inferred.Value(ret, effects), st2) =>
                            TcResult.Ok(typeExprFromTy(curriedFunction(argTys, effects, ret), st2), st2)
                    }
            }
        case _ =>
            match inferNamed(name, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(Inferred.Value(t, effects), st1) =>
                    let valueTy = match effects {
                        case Nil => t
                        case _ => Ty.Fun(Nil, effects, t)
                    };
                    TcResult.Ok(typeExprFromTy(valueTy, st1), st1)
            }
    }

def defaultBoneArgumentTypes(boneParams: List[String], n: Int32, acc: List[TypeExpr]): List[TypeExpr] =
    if (n <= 0) {
        List.reverse(acc)
    } else {
        defaultBoneArgumentTypes(boneParams, n - 1, defaultBoneArgumentType(boneParams) :: acc)
    }

def defaultBoneArgumentType(boneParams: List[String]): TypeExpr =
    match boneParams {
        case param :: _ => TypeExpr.Name(param)
        case Nil => TypeExpr.Name("t0")
    }

def checkBoneDefaults(boneName: String, members: List[BoneMember], ctx: TcContext, st: TcState, whole: Decl): TcResult[Decl] =
    match members {
        case Nil => TcResult.Ok(whole, st)
        case BoneMember.Spec(_, _) :: rest => checkBoneDefaults(boneName, rest, ctx, st, whole)
        case BoneMember.Default(name, Some(t), expr) :: rest =>
            match checkRecursiveLetAnnotatedAlias(name, boneName + "@" + name, t, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => checkBoneDefaults(boneName, rest, ctx, st2, whole)
            }
        case BoneMember.Default(name, None, _) :: _ => TcResult.Err("bone default '${name}' has no inferred type")
    }

def checkLet(name: String, ann: Option[TypeExpr], expr: Expr, ctx: TcContext, st: TcState): TcResult[Decl] =
    match ann {
        case None =>
            match inferNamed(name, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => TcResult.Ok(Decl.Let(name, ann, expr), st2)
            }
        case Some(tExpr) =>
            match instantiateAnnotation(tExpr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((expectedValue, expectedEffects), st1) =>
                    match checkExprAgainst(name, expectedValue, expectedEffects, expr, ctx, st1) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(_, st2) => TcResult.Ok(Decl.Let(name, ann, expr), st2)
                    }
            }
    }

def checkFlesh(tyExpr: TypeExpr, boneName: String, members: List[Decl], ctx: TcContext, st: TcState): TcResult[Decl] =
    match findBoneInfo(boneName, ctx) {
        case None => checkLooseFleshMembers(members, ctx, st, Decl.Flesh(tyExpr, boneName, members))
        case Some(BoneInfo.Info(params, ms)) => checkFleshMembers(members, boneName, params, tyExpr, ms, ctx, st, Decl.Flesh(tyExpr, boneName, members))
    }

def checkLooseFleshMembers(members: List[Decl], ctx: TcContext, st: TcState, whole: Decl): TcResult[Decl] =
    match members {
        case Nil => TcResult.Ok(whole, st)
        case Decl.Let(name, _, expr) :: rest =>
            match checkRecursiveLet(name, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => checkLooseFleshMembers(rest, ctx, st2, whole)
            }
        case _ :: _ => TcResult.Err("flesh member must be a let")
    }

def checkFleshMembers(members: List[Decl], boneName: String, boneParams: List[String], fleshType: TypeExpr, specs: List[BoneMember], ctx: TcContext, st: TcState, whole: Decl): TcResult[Decl] =
    match members {
        case Nil => TcResult.Ok(whole, st)
        case Decl.Let(name, _, expr) :: rest =>
            match findBoneMember(name, specs) {
                case None => TcResult.Err("flesh defines unknown member '${name}'")
                case Some(specType) =>
                    let memberType = specializeBoneType(boneParams, fleshType, specType);
                    match checkRecursiveLetAnnotatedAlias(name, boneName + "@" + name, memberType, expr, ctx, st) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(_, st2) => checkFleshMembers(rest, boneName, boneParams, fleshType, specs, ctx, st2, whole)
                    }
            }
        case _ :: _ => TcResult.Err("flesh member must be a let")
    }

def checkLocalDecls(ds: List[Decl], ctx: TcContext, st: TcState, effects: List[String]): TcResult[(TcContext, List[String])] =
    match ds {
        case Nil => TcResult.Ok((ctx, effects), st)
        case Decl.Let(name, ann, expr) :: rest =>
            match inferLocalLet(name, ann, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((ctx2, letEffects), st2) => checkLocalDecls(rest, ctx2, st2, unionEffects(effects, letEffects))
            }
        case _ :: rest => checkLocalDecls(rest, ctx, st, effects)
    }

def inferLocalLet(name: String, ann: Option[TypeExpr], expr: Expr, ctx: TcContext, st: TcState): TcResult[(TcContext, List[String])] =
    match ann {
        case None => inferUnannotatedBinding(name, expr, ctx, st)
        case Some(tExpr) =>
            match instantiateAnnotation(tExpr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((expectedValue, expectedEffects), st1) =>
                    match checkExprAgainst(name, expectedValue, expectedEffects, expr, ctx, st1) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok((checkedValue, effects), st2) => TcResult.Ok((addEnv(name, generalizeTy(checkedValue), ctx), effects), st2)
                    }
            }
    }

def inferUnannotatedBinding(name: String, expr: Expr, ctx: TcContext, st: TcState): TcResult[(TcContext, List[String])] =
    match inferNamed(name, expr, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(Inferred.Value(t, effects), st2) => TcResult.Ok((addEnv(name, generalizeApplied(t, st2), ctx), effects), st2)
    }

def inferNamed(name: String, expr: Expr, ctx: TcContext, st: TcState): TcResult[Inferred] =
    match inferExpr(expr, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err("in '${name}': ${msg}")
        case TcResult.Ok(inferred, st2) => TcResult.Ok(inferred, st2)
    }

def instantiateAnnotation(tExpr: TypeExpr, ctx: TcContext, st: TcState): TcResult[(Ty, List[String])] =
    match instantiate(typeExprScheme(tExpr, ctx), st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(expected, st1) => TcResult.Ok(splitComputationType(expected), st1)
    }

def checkExprAgainst(name: String, expectedValue: Ty, expectedEffects: List[String], expr: Expr, ctx: TcContext, st: TcState): TcResult[(Ty, List[String])] =
    match inferNamed(name, expr, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(Inferred.Value(actual, effects), st1) =>
            match unify(actual, expectedValue, st1) {
                case TcResult.Err(msg) => TcResult.Err("in '${name}': ${msg}; actual ${showTy(actual)}, expected ${showTy(expectedValue)}")
                case TcResult.Ok(_, st2) =>
                    if (sameEffects(effects, expectedEffects)) {
                        TcResult.Ok((applyState(expectedValue, st2), effects), st2)
                    } else {
                        TcResult.Err("in '${name}': cannot unify effects {" + showEffects(effects) + "} with {" + showEffects(expectedEffects) + "}")
                    }
            }
    }

def generalizeApplied(ty: Ty, st: TcState): Scheme =
    generalizeTy(applyState(ty, st))

def generalizeTy(ty: Ty): Scheme =
    Scheme.Forall(typeVarsInTy(ty, Nil), ty)

def checkRecursiveLet(name: String, expr: Expr, ctx: TcContext, st: TcState): TcResult[Decl] =
    match fresh(st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(selfTy, st1) =>
            let ctx2 = addEnv(name, Scheme.Forall(Nil, selfTy), ctx);
            match inferNamed(name, expr, ctx2, st1) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(Inferred.Value(actual, _), st2) =>
                    match unify(selfTy, actual, st2) {
                        case TcResult.Err(msg) => TcResult.Err("in '${name}': ${msg}")
                        case TcResult.Ok(_, st3) => TcResult.Ok(Decl.Let(name, None, expr), st3)
                    }
            }
    }

def checkRecursiveLetAnnotated(name: String, expectedExpr: TypeExpr, expr: Expr, ctx: TcContext, st: TcState): TcResult[Decl] =
    checkRecursiveLetAnnotatedAlias(name, name, expectedExpr, expr, ctx, st)

def checkRecursiveLetAnnotatedAlias(name: String, qualifiedName: String, expectedExpr: TypeExpr, expr: Expr, ctx: TcContext, st: TcState): TcResult[Decl] =
    match instantiateAnnotation(expectedExpr, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok((expectedValue, expectedEffects), st1) =>
            let selfScheme = generalizeTy(expectedValue);
            let ctx2 = addSelfEnv(name, qualifiedName, selfScheme, ctx);
            match checkExprAgainst(name, expectedValue, expectedEffects, expr, ctx2, st1) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => TcResult.Ok(Decl.Let(name, Some(expectedExpr), expr), st2)
            }
    }

def addSelfEnv(name: String, qualifiedName: String, scheme: Scheme, ctx: TcContext): TcContext =
    if (name == qualifiedName) addEnv(name, scheme, ctx) else addEnvWith(qualifiedName, scheme, localEnvRank(), qualifiedName, addEnvWith(name, scheme, selfAliasEnvRank(), qualifiedName, ctx))
