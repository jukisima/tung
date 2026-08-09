def checkDecls(ds: List[Decl], ctx: TcContext, st: TcState): TcResult[Program] =
    match checkDeclsContext(ds, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(_, st2) => TcResult.Ok(Program.Decls(Nil), st2)
    }

def checkRunnableDecls(ds: List[Decl], ctx: TcContext, st: TcState): TcResult[Program] =
    match inferRunnableDecls(ds, ctx, st, Nil, Ty.Con("𝟙")) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok((value, effects), st2) => checkRunnableType(value, effects, st2)
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

def inferRunnableDecls(ds: List[Decl], ctx: TcContext, st: TcState, effects: List[Ty], fileValue: Ty): TcResult[(Ty, List[Ty])] =
    match ds {
        case Nil => TcResult.Ok((fileValue, effects), st)
        case Decl.Let(name, ann, expr) :: rest =>
            match inferRunnableLet(name, ann, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((ctx2, value, letEffects), st2) =>
                    let nextValue = if (name == "_") value else fileValue;
                    inferRunnableDecls(rest, ctx2, st2, unionEffects(effects, letEffects), nextValue)
            }
        case d :: rest =>
            match checkDecl(d, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(_, st2) => inferRunnableDecls(rest, ctx, st2, effects, fileValue)
            }
    }

def inferRunnableLet(name: String, ann: Option[TypeExpr], expr: Expr, ctx: TcContext, st: TcState): TcResult[(TcContext, Ty, List[Ty])] =
    match ann {
        case None =>
            match inferNamed(name, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(Inferred.Value(value, effects), st2) =>
                    TcResult.Ok((addEnv(name, generalizeApplied(value, st2), ctx), value, effects), st2)
            }
        case Some(tExpr) =>
            match instantiateAnnotation(tExpr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((expectedValue, expectedEffects), st1) =>
                    match checkExprAgainst(name, expectedValue, expectedEffects, expr, ctx, st1) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok((checkedValue, effects), st2) => TcResult.Ok((ctx, checkedValue, effects), st2)
                    }
            }
    }

def checkRunnableType(value: Ty, effects: List[Ty], st: TcState): TcResult[Program] =
    let actualValue = applyState(value, st);
    match unify(actualValue, Ty.Con("𝟙"), st) {
        case TcResult.Err(_) => TcResult.Err("runnable file must return 𝟙, found ${showTy(actualValue)}")
        case TcResult.Ok(_, st2) =>
            let actualEffects = applyStateEffects(effects, st2);
            if (allRunnableEffects(actualEffects)) {
                TcResult.Ok(Program.Decls(Nil), st2)
            } else {
                TcResult.Err("runnable file has unhandled effects {" + showEffects(actualEffects) + "}; only console and random may remain")
            }
    }

def allRunnableEffects(effects: List[Ty]): Bool =
    match effects {
        case Nil => true
        case effect :: rest => isRunnableEffect(effect) and allRunnableEffects(rest)
    }

def isRunnableEffect(effect: Ty): Bool =
    match effectNameAndArgs(effect) {
        case Some(("console", Nil)) => true
        case Some(("random", Nil)) => true
        case _ => false
    }

def checkDecl(d: Decl, ctx: TcContext, st: TcState): TcResult[Decl] =
    match d {
        case Decl.Import(_) => TcResult.Ok(d, st)
        case Decl.Let(name, ann, expr) => checkLet(name, ann, expr, ctx, st)
        case Decl.TypeAlias(_, _) => TcResult.Ok(d, st)
        case Decl.Data(_, _, _) => TcResult.Ok(d, st)
        case Decl.Effect(_, _, _) => TcResult.Ok(d, st)
        case Decl.Bone(_, boneName, needs, members) => checkBoneDecl(boneName, needs, members, ctx, st, d)
        case Decl.Flesh(ty, boneName, members) => checkFlesh(ty, boneName, members, ctx, st)
    }

def inferBoneDefaultDecls(ds: List[Decl], ctx: TcContext, st: TcState, acc: List[Decl]): TcResult[(List[Decl], TcContext)] =
    match ds {
        case Nil => TcResult.Ok((List.reverse(acc), ctx), st)
        case Decl.Bone(params, boneName, needs, members) :: rest =>
            let knownCtx = addBoneMembers(boneName, params, needs, members, ctx);
            match inferBoneDefaultMembers(params, boneName, members, knownCtx, st, Nil) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok(typedMembers, st2) =>
                    let typedDecl = Decl.Bone(params, boneName, needs, List.reverse(typedMembers));
                    let ctx2 = addBoneMembers(boneName, params, needs, List.reverse(typedMembers), ctx);
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

def checkBoneDecl(boneName: String, needs: List[BoneNeed], members: List[BoneMember], ctx: TcContext, st: TcState, whole: Decl): TcResult[Decl] =
    match checkBoneNeeds(boneName, needs, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(_, st1) => checkBoneDefaults(boneName, members, ctx, st1, whole)
    }

def checkBoneNeeds(boneName: String, needs: List[BoneNeed], ctx: TcContext, st: TcState): TcResult[Unit] =
    match needs {
        case Nil => TcResult.Ok((), st)
        case BoneNeed.Need(args, neededName) :: rest =>
            match findBoneInfo(neededName, ctx) {
                case None => TcResult.Err("bone '${boneName}' needs unknown bone '${neededName}'")
                case Some(BoneInfo.Info(params, _, _)) =>
                    let expected = List.length(params);
                    let actual = List.length(args);
                    if (actual == expected) {
                        checkBoneNeeds(boneName, rest, ctx, st)
                    } else {
                        TcResult.Err("bone '${boneName}' needs '${neededName}' with ${actual} arguments, but '${neededName}' has ${expected} parameters")
                    }
            }
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
        case Some(_) =>
            match fleshSpecsForBone(boneName, tyExpr, ctx, Nil) {
                case None => TcResult.Err("flesh '${boneName}' has an unknown required bone")
                case Some(specs) => checkFleshMembers(members, specs, ctx, st, Decl.Flesh(tyExpr, boneName, members))
            }
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

enum FleshSpec {
    case Spec(String, String, TypeExpr)
}

def fleshSpecsForBone(boneName: String, fleshType: TypeExpr, ctx: TcContext, seen: List[String]): Option[List[FleshSpec]] =
    if (containsString(boneName, seen)) {
        Some(Nil)
    } else {
        match findBoneInfo(boneName, ctx) {
            case None => None
            case Some(BoneInfo.Info(params, needs, members)) =>
                match fleshSpecsForNeeds(needs, params, fleshType, ctx, boneName :: seen) {
                    case None => None
                    case Some(inherited) => Some(appendFleshSpecs(fleshMemberSpecs(boneName, params, fleshType, members), inherited))
                }
        }
    }

def fleshSpecsForNeeds(needs: List[BoneNeed], currentParams: List[String], currentFleshType: TypeExpr, ctx: TcContext, seen: List[String]): Option[List[FleshSpec]] =
    match needs {
        case Nil => Some(Nil)
        case BoneNeed.Need(args, neededName) :: rest =>
            let specializedArgs = List.map(t -> specializeBoneType(currentParams, currentFleshType, t), args);
            match specializedArgs {
                case Nil => None
                case neededFleshType :: _ =>
                    match (fleshSpecsForBone(neededName, neededFleshType, ctx, seen), fleshSpecsForNeeds(rest, currentParams, currentFleshType, ctx, seen)) {
                        case (Some(left), Some(right)) => Some(appendFleshSpecs(left, right))
                        case _ => None
                    }
            }
    }

def fleshMemberSpecs(boneName: String, boneParams: List[String], fleshType: TypeExpr, members: List[BoneMember]): List[FleshSpec] =
    match members {
        case Nil => Nil
        case member :: rest =>
            match boneMemberSignature(member) {
                case None => fleshMemberSpecs(boneName, boneParams, fleshType, rest)
                case Some((name, t)) => FleshSpec.Spec(name, boneName, specializeBoneType(boneParams, fleshType, t)) :: fleshMemberSpecs(boneName, boneParams, fleshType, rest)
            }
    }

def appendFleshSpecs(xs: List[FleshSpec], ys: List[FleshSpec]): List[FleshSpec] =
    match xs {
        case Nil => ys
        case x :: rest => x :: appendFleshSpecs(rest, ys)
    }

def findFleshSpec(name: String, specs: List[FleshSpec]): Option[FleshSpec] =
    match specs {
        case Nil => None
        case FleshSpec.Spec(n, ownerName, memberType) :: _ if n == name => Some(FleshSpec.Spec(n, ownerName, memberType))
        case _ :: rest => findFleshSpec(name, rest)
    }

def checkFleshMembers(members: List[Decl], specs: List[FleshSpec], ctx: TcContext, st: TcState, whole: Decl): TcResult[Decl] =
    match members {
        case Nil => TcResult.Ok(whole, st)
        case Decl.Let(name, _, expr) :: rest =>
            match findFleshSpec(name, specs) {
                case None => TcResult.Err("flesh defines unknown member '${name}'")
                case Some(FleshSpec.Spec(_, ownerName, memberType)) =>
                    match checkRecursiveLetAnnotatedAlias(name, ownerName + "@" + name, memberType, expr, ctx, st) {
                        case TcResult.Err(msg) => TcResult.Err(msg)
                        case TcResult.Ok(_, st2) => checkFleshMembers(rest, specs, ctx, st2, whole)
                    }
            }
        case _ :: _ => TcResult.Err("flesh member must be a let")
    }

def checkLocalDecls(ds: List[Decl], ctx: TcContext, st: TcState, effects: List[Ty]): TcResult[(TcContext, List[Ty])] =
    match ds {
        case Nil => TcResult.Ok((ctx, effects), st)
        case Decl.Let(name, ann, expr) :: rest =>
            match inferLocalLet(name, ann, expr, ctx, st) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((ctx2, letEffects), st2) => checkLocalDecls(rest, ctx2, st2, unionEffects(effects, letEffects))
            }
        case _ :: rest => checkLocalDecls(rest, ctx, st, effects)
    }

def inferLocalLet(name: String, ann: Option[TypeExpr], expr: Expr, ctx: TcContext, st: TcState): TcResult[(TcContext, List[Ty])] =
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

def inferUnannotatedBinding(name: String, expr: Expr, ctx: TcContext, st: TcState): TcResult[(TcContext, List[Ty])] =
    match inferNamed(name, expr, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(Inferred.Value(t, effects), st2) => TcResult.Ok((addEnv(name, generalizeApplied(t, st2), ctx), effects), st2)
    }

def inferNamed(name: String, expr: Expr, ctx: TcContext, st: TcState): TcResult[Inferred] =
    match inferExpr(expr, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err("in '${name}': ${msg}")
        case TcResult.Ok(inferred, st2) => TcResult.Ok(inferred, st2)
    }

def instantiateAnnotation(tExpr: TypeExpr, ctx: TcContext, st: TcState): TcResult[(Ty, List[Ty])] =
    match instantiate(typeExprScheme(tExpr, ctx), st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(expected, st1) => TcResult.Ok(splitComputationType(expected), st1)
    }

def checkExprAgainst(name: String, expectedValue: Ty, expectedEffects: List[Ty], expr: Expr, ctx: TcContext, st: TcState): TcResult[(Ty, List[Ty])] =
    match inferNamed(name, expr, ctx, st) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(Inferred.Value(actual, effects), st1) =>
            match unify(actual, expectedValue, st1) {
                case TcResult.Err(msg) => TcResult.Err("in '${name}': ${msg}; actual ${showTy(actual)}, expected ${showTy(expectedValue)}")
                case TcResult.Ok(_, st2) =>
                    let actualEffects = applyStateEffects(effects, st2);
                    let expectedEffects2 = applyStateEffects(expectedEffects, st2);
                    match unifyEffects(actualEffects, expectedEffects2, st2) {
                        case TcResult.Ok(_, st3) => TcResult.Ok((applyState(expectedValue, st3), applyStateEffects(actualEffects, st3)), st3)
                        case TcResult.Err(_) => TcResult.Err("in '${name}': cannot unify effects {" + showEffects(actualEffects) + "} with {" + showEffects(expectedEffects2) + "}")
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
