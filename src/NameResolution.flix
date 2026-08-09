def importedVisibleContext(ns: String, ctx: TcContext): TcContext =
    namespaceImportContext(ns, ctx)

def lookupImport(path: String, imports: List[(String, String)]): Option[String] =
    match imports {
        case Nil => None
        case (p, source) :: _ if p == path => Some(source)
        case _ :: rest => lookupImport(path, rest)
    }

def importNamespace(path: String): String =
    firstPathPart(path)

def firstPathPart(path: String): String =
    charsToNamespace(String.toList(path), Nil)

def charsToNamespace(cs: List[Char], acc: List[Char]): String =
    match cs {
        case Nil => charsToString(List.reverse(acc))
        case '.' :: _ => charsToString(List.reverse(acc))
        case c :: rest => charsToNamespace(rest, c :: acc)
    }

def namespaceImportContext(ns: String, ctx: TcContext): TcContext =
    match ctx {
        case TcContext.Context(env, types, bones) =>
            TcContext.Context(namespaceImportEnv(ns, env), namespaceImportTypes(ns, types), namespaceImportBones(ns, bones))
    }

def namespaceImportEnv(ns: String, env: List[(String, Scheme, Int32, String)]): List[(String, Scheme, Int32, String)] =
    match env {
        case Nil => Nil
        case (name, _scheme, _, origin) :: rest if isBaseEnvBinding(name, origin) => namespaceImportEnv(ns, rest)
        case (name, scheme, rank, origin) :: rest =>
            let scheme2 = namespaceScheme(ns, scheme);
            let origin2 = ns + "@" + origin;
            let exact = (ns + "@" + name, scheme2, importEnvRank(), origin2);
            if (rank == localEnvRank()) {
                let bare = (lastQualifiedSegment(name), scheme2, aliasEnvRank(), origin2);
                bare :: exact :: namespaceImportEnv(ns, rest)
            } else {
                exact :: namespaceImportEnv(ns, rest)
            }
    }

def namespaceImportTypes(ns: String, types: List[(String, TypeInfo)]): List[(String, TypeInfo)] =
    match types {
        case Nil => Nil
        case (name, info) :: rest =>
            let info2 = namespaceTypeInfo(ns, info);
            let exact = (ns + "@" + name, info2);
            if (typeInfoCanonicalName(info) == name and not isQualifiedEnvName(name)) {
                (lastQualifiedSegment(name), info2) :: exact :: namespaceImportTypes(ns, rest)
            } else {
                exact :: namespaceImportTypes(ns, rest)
            }
    }

def namespaceTypeInfo(ns: String, info: TypeInfo): TypeInfo =
    match info {
        case TypeInfo.Alias(name, params) => TypeInfo.Alias(ns + "@" + name, params)
        case TypeInfo.Data(name, params, ctors) => TypeInfo.Data(ns + "@" + name, params, namespaceCtorInfos(ns, ctors))
    }

def typeInfoCanonicalName(info: TypeInfo): String =
    match info {
        case TypeInfo.Alias(name, _) => name
        case TypeInfo.Data(name, _, _) => name
    }

def namespaceCtorInfos(ns: String, ctors: List[(String, List[TypeExpr])]): List[(String, List[TypeExpr])] =
    match ctors {
        case Nil => Nil
        case (name, fields) :: rest => (ns + "@" + name, List.map(t -> namespaceTypeExpr(ns, t), fields)) :: namespaceCtorInfos(ns, rest)
    }

def namespaceImportBones(ns: String, bones: List[(String, BoneInfo)]): List[(String, BoneInfo)] =
    match bones {
        case Nil => Nil
        case (name, BoneInfo.Info(params, needs, members)) :: rest =>
            let info = BoneInfo.Info(params, namespaceBoneNeeds(ns, needs), namespaceBoneMembers(ns, members));
            (lastQualifiedSegment(name), info) :: (ns + "@" + name, info) :: namespaceImportBones(ns, rest)
    }

def namespaceBoneNeeds(ns: String, needs: List[BoneNeed]): List[BoneNeed] =
    match needs {
        case Nil => Nil
        case BoneNeed.Need(args, name) :: rest => BoneNeed.Need(List.map(t -> namespaceTypeExpr(ns, t), args), namespaceBoneName(ns, name)) :: namespaceBoneNeeds(ns, rest)
    }

def namespaceBoneName(ns: String, name: String): String =
    if (isQualifiedEnvName(name)) name else ns + "@" + name

def namespaceBoneMembers(ns: String, members: List[BoneMember]): List[BoneMember] =
    match members {
        case Nil => Nil
        case BoneMember.Spec(name, t) :: rest => BoneMember.Spec(name, namespaceTypeExpr(ns, t)) :: namespaceBoneMembers(ns, rest)
        case BoneMember.Default(name, Some(t), expr) :: rest => BoneMember.Default(name, Some(namespaceTypeExpr(ns, t)), expr) :: namespaceBoneMembers(ns, rest)
        case BoneMember.Default(name, None, expr) :: rest => BoneMember.Default(name, None, expr) :: namespaceBoneMembers(ns, rest)
    }

def namespaceScheme(ns: String, s: Scheme): Scheme =
    match s {
        case Scheme.Forall(vars, ty) => Scheme.Forall(vars, namespaceTy(ns, ty))
    }

def namespaceTy(ns: String, ty: Ty): Ty =
    match ty {
        case Ty.Con(name) if isPrimitiveTypeName(name) => ty
        case Ty.Con(name) if isQualifiedEnvName(name) => ty
        case Ty.Con(name) => Ty.Con(ns + "@" + name)
        case Ty.App(name, args) if isPrimitiveTypeName(name) => Ty.App(name, List.map(t -> namespaceTy(ns, t), args))
        case Ty.App(name, args) if isTypeVarName(name) => Ty.App(name, List.map(t -> namespaceTy(ns, t), args))
        case Ty.App(name, args) if isQualifiedEnvName(name) => Ty.App(name, List.map(t -> namespaceTy(ns, t), args))
        case Ty.App(name, args) => Ty.App(ns + "@" + name, List.map(t -> namespaceTy(ns, t), args))
        case Ty.Record(fields) => Ty.Record(List.map(p -> match p { case (name, t) => (name, namespaceTy(ns, t)) }, fields))
        case Ty.Fun(args, effects, ret) => Ty.Fun(List.map(t -> namespaceTy(ns, t), args), namespaceEffects(ns, effects), namespaceTy(ns, ret))
        case _ => ty
    }

def namespaceEffects(ns: String, effects: List[Ty]): List[Ty] =
    match effects {
        case Nil => Nil
        case e :: rest => namespaceEffectTy(ns, e) :: namespaceEffects(ns, rest)
    }

def namespaceEffectTy(ns: String, effect: Ty): Ty =
    match effect {
        case Ty.Var(name) if isEffectVarName(name) => effect
        case Ty.App(name, args) if isBaseEffectName(name) => Ty.App(name, List.map(t -> namespaceTy(ns, t), args))
        case Ty.App(name, args) if isQualifiedEnvName(name) => Ty.App(name, List.map(t -> namespaceTy(ns, t), args))
        case Ty.App(name, args) => Ty.App(ns + "@" + name, List.map(t -> namespaceTy(ns, t), args))
        case Ty.Con(name) if isBaseEffectName(name) => Ty.App(name, Nil)
        case Ty.Con(name) if isQualifiedEnvName(name) => Ty.App(name, Nil)
        case Ty.Con(name) => Ty.App(ns + "@" + name, Nil)
        case _ => namespaceTy(ns, effect)
    }

def namespaceTypeExpr(ns: String, t: TypeExpr): TypeExpr =
    match t {
        case TypeExpr.Name(name) if isPrimitiveTypeName(name) => t
        case TypeExpr.Name(name) if isTypeVarName(name) => t
        case TypeExpr.Name(name) if isQualifiedEnvName(name) => t
        case TypeExpr.Name(name) => TypeExpr.Name(ns + "@" + name)
        case TypeExpr.Apply(name, args) if isPrimitiveTypeName(name) => TypeExpr.Apply(name, List.map(x -> namespaceTypeExpr(ns, x), args))
        case TypeExpr.Apply(name, args) if isTypeVarName(name) => TypeExpr.Apply(name, List.map(x -> namespaceTypeExpr(ns, x), args))
        case TypeExpr.Apply(name, args) if isQualifiedEnvName(name) => TypeExpr.Apply(name, List.map(x -> namespaceTypeExpr(ns, x), args))
        case TypeExpr.Apply(name, args) => TypeExpr.Apply(ns + "@" + name, List.map(x -> namespaceTypeExpr(ns, x), args))
        case TypeExpr.Record(fields) => TypeExpr.Record(List.map(p -> match p { case (name, fieldTy) => (name, namespaceTypeExpr(ns, fieldTy)) }, fields))
        case TypeExpr.Arrow(args, effects, ret) => TypeExpr.Arrow(List.map(x -> namespaceTypeExpr(ns, x), args), List.map(e -> namespaceEffectTypeExpr(ns, e), effects), namespaceTypeExpr(ns, ret))
        case TypeExpr.Forall(params, body) => TypeExpr.Forall(params, namespaceTypeExpr(ns, body))
    }

def namespaceEffectTypeExpr(ns: String, t: TypeExpr): TypeExpr =
    match t {
        case TypeExpr.Name(name) if isEffectVarName(name) => t
        case TypeExpr.Name(name) if isBaseEffectName(name) => t
        case TypeExpr.Name(name) if isQualifiedEnvName(name) => t
        case TypeExpr.Name(name) => TypeExpr.Name(ns + "@" + name)
        case TypeExpr.Apply(name, args) if isBaseEffectName(name) => TypeExpr.Apply(name, List.map(x -> namespaceTypeExpr(ns, x), args))
        case TypeExpr.Apply(name, args) if isQualifiedEnvName(name) => TypeExpr.Apply(name, List.map(x -> namespaceTypeExpr(ns, x), args))
        case TypeExpr.Apply(name, args) => TypeExpr.Apply(ns + "@" + name, List.map(x -> namespaceTypeExpr(ns, x), args))
        case _ => namespaceTypeExpr(ns, t)
    }

def mergeContext(left: TcContext, right: TcContext): TcContext =
    match (left, right) {
        case (TcContext.Context(env1, types1, bones1), TcContext.Context(env2, types2, bones2)) =>
            TcContext.Context(appendList(env1, env2), appendList(types1, types2), appendList(bones1, bones2))
    }

def appendList(xs: List[a], ys: List[a]): List[a] =
    match xs {
        case Nil => ys
        case x :: rest => x :: appendList(rest, ys)
    }

def isBaseEnvName(name: String): Bool =
    name == "to-string" or name == "from-string" or name == "⌊" or name == "≤" or name == "+" or name == "-" or name == "¯" or name == "×" or name == "÷" or name == "/" or name == "exp" or name == "log" or name == "sin" or name == "cos" or name == "write-line" or name == "read-line" or name == "random-float" or name == "random-integer" or name == "divide"

def isBaseEnvBinding(name: String, origin: String): Bool =
    isBaseEnvName(name) and origin == "base@" + name

def isBaseEffectName(name: String): Bool =
    name == "io" or name == "console" or name == "random" or name == "fail" or name == "state"

def isPrimitiveTypeName(name: String): Bool =
    name == "integer" or name == "float" or name == "char" or name == "character" or name == "string" or name == "𝟘" or name == "𝟙" or name == "𝟚"

def isTypeVarName(name: String): Bool =
    match String.toList(name) {
        case c :: Nil => isLowerAlpha(c)
        case c :: rest => isLowerAlpha(c) and allTypeVarNameDigits(rest)
        case _ => false
    }

def allTypeVarNameDigits(cs: List[Char]): Bool =
    match cs {
        case Nil => true
        case c :: rest => c >= '0' and c <= '9' and allTypeVarNameDigits(rest)
    }

def collectProgramContext(ds: List[Decl], ctx: TcContext): TcContext =
    match ds {
        case Nil => ctx
        case d :: rest => collectProgramContext(rest, collectDeclContext(d, ctx))
    }

def collectDeclContext(d: Decl, ctx: TcContext): TcContext =
    match d {
        case Decl.Import(_) => ctx
        case Decl.Data(params, name, ctors) => collectConstructors(name, params, ctors, addDataTypeInfo(name, params, dataCtorInfos(name, ctors), ctx))
        case Decl.TypeAlias(name, _) => addTypeAliasInfo(name, Nil, ctx)
        case Decl.Effect(params, name, ops) => collectEffectOps(params, name, ops, ctx)
        case Decl.Bone(params, name, needs, members) => addBoneMembers(name, params, needs, members, ctx)
        case Decl.Let(name, Some(t), _) => addEnv(name, typeExprBindingScheme(t, ctx), ctx)
        case Decl.Let(_, None, _) => ctx
        case Decl.Flesh(_, _, _) => ctx
    }

def dataCtorInfos(dataName: String, ctors: List[Ctor]): List[(String, List[TypeExpr])] =
    match ctors {
        case Nil => Nil
        case Ctor.Case(name, fields) :: rest => (dataName + "@" + name, fields) :: dataCtorInfos(dataName, rest)
    }

def collectEffectOps(params: List[String], effectName: String, ops: List[EffectOp], ctx: TcContext): TcContext =
    match ops {
        case Nil => ctx
        case EffectOp.Op(opName, t) :: rest =>
            let opTy = addLatentEffect(params, effectName, convertTypeExpr(t, ctx));
            let scheme = generalizeTy(opTy);
            collectEffectOps(params, effectName, rest, addEnvAlias(opName, effectName + "@" + opName, scheme, addEnv(effectName + "@" + opName, scheme, ctx)))
    }

def addLatentEffect(params: List[String], effectName: String, ty: Ty): Ty =
    let effect = Ty.App(effectName, List.map(Ty.Var, params));
    match normalizeFun(ty) {
        case Ty.Fun(args, effects, ret) => Ty.Fun(args, addEffect(effect, effects), ret)
        case other => Ty.Fun(Nil, effect :: Nil, other)
    }

def collectConstructors(dataName: String, params: List[String], ctors: List[Ctor], ctx: TcContext): TcContext =
    match ctors {
        case Nil => ctx
        case Ctor.Case(name, fields) :: rest =>
            let result = dataResultType(dataName, params);
            let fieldTys = List.map(f -> convertTypeExpr(f, ctx), fields);
            let ty = match fieldTys {
                case Nil => result
                case _ => curriedFunction(fieldTys, Nil, result)
            };
            let scheme = Scheme.Forall(params, ty);
            let ctx2 = addEnvAlias(name, dataName + "@" + name, scheme, addEnv(dataName + "@" + name, scheme, ctx));
            collectConstructors(dataName, params, rest, ctx2)
    }

def dataResultType(name: String, params: List[String]): Ty =
    match params {
        case Nil => Ty.Con(name)
        case _ => Ty.App(name, List.map(Ty.Var, params))
    }

def addTypeAliasInfo(name: String, params: List[String], ctx: TcContext): TcContext =
    match ctx {
        case TcContext.Context(env, types, bones) => TcContext.Context(env, (name, TypeInfo.Alias(name, params)) :: types, bones)
    }

def addDataTypeInfo(name: String, params: List[String], ctors: List[(String, List[TypeExpr])], ctx: TcContext): TcContext =
    match ctx {
        case TcContext.Context(env, types, bones) => TcContext.Context(env, (name, TypeInfo.Data(name, params, ctors)) :: types, bones)
    }

def addEnv(name: String, scheme: Scheme, ctx: TcContext): TcContext =
    addEnvWith(name, scheme, localEnvRank(), name, ctx)

def addEnvAlias(name: String, qualifiedName: String, scheme: Scheme, ctx: TcContext): TcContext =
    addEnvWith(name, scheme, localEnvRank(), qualifiedName, ctx)

def addEnvWith(name: String, scheme: Scheme, rank: Int32, origin: String, ctx: TcContext): TcContext =
    match ctx {
        case TcContext.Context(env, types, bones) => TcContext.Context((name, scheme, rank, origin) :: env, types, bones)
    }

def localEnvRank(): Int32 = 0

def aliasEnvRank(): Int32 = 1

def importEnvRank(): Int32 = 1

def baseEnvRank(): Int32 = 2

def selfAliasEnvRank(): Int32 = 3

def lookupEnv(name: String, ctx: TcContext): EnvLookup =
    match lookupEnvChoices(name, ctx) {
        case EnvChoices.Missing => EnvLookup.Missing
        case EnvChoices.Ambiguous(msg) => EnvLookup.Ambiguous(msg)
        case EnvChoices.Choices(Nil) => EnvLookup.Missing
        case EnvChoices.Choices(scheme :: _) => EnvLookup.Found(scheme)
    }

def lookupEnvChoices(name: String, ctx: TcContext): EnvChoices =
    match ctx {
        case TcContext.Context(env, _, _) =>
            if (isQualifiedEnvName(name)) {
                match lookupEnvExact(name, env) {
                    case None => EnvChoices.Missing
                    case Some(scheme) => EnvChoices.Choices(scheme :: Nil)
                }
            } else {
                selectEnvChoices(name, List.reverse(collectEnvCandidates(name, env, Nil)))
            }
    }

def lookupEnvExact(name: String, env: List[(String, Scheme, Int32, String)]): Option[Scheme] =
    match env {
        case Nil => None
        case (n, s, _, _) :: _ if n == name => Some(s)
        case _ :: rest => lookupEnvExact(name, rest)
    }

def collectEnvCandidates(name: String, env: List[(String, Scheme, Int32, String)], acc: List[(Scheme, Int32, String)]): List[(Scheme, Int32, String)] =
    match env {
        case Nil => acc
        case (n, s, rank, origin) :: rest if n == name => collectEnvCandidates(name, rest, (s, rank, origin) :: acc)
        case _ :: rest => collectEnvCandidates(name, rest, acc)
    }

def selectEnvChoices(name: String, candidates: List[(Scheme, Int32, String)]): EnvChoices =
    match minimumEnvRank(candidates, None) {
        case None => EnvChoices.Missing
        case Some(bestRank) =>
            match firstOriginAtRank(bestRank, candidates) {
                case None => EnvChoices.Missing
                case Some(bestOrigin) =>
                    match ambiguousOriginsAtRank(bestRank, bestOrigin, candidates, Nil) {
                        case Nil =>
                            let bestChoices = choicesAtRankOrigin(bestRank, bestOrigin, candidates, Nil);
                            EnvChoices.Choices(appendList(bestChoices, fallbackChoicesAfterLocal(bestRank, candidates)))
                        case ambiguousOrigins =>
                            EnvChoices.Ambiguous("ambiguous name '${name}'; qualify it as one of: " + showOrigins(bestOrigin :: ambiguousOrigins))
                    }
            }
    }

def minimumEnvRank(candidates: List[(Scheme, Int32, String)], best: Option[Int32]): Option[Int32] =
    match candidates {
        case Nil => best
        case (_, rank, _) :: rest =>
            match best {
                case None => minimumEnvRank(rest, Some(rank))
                case Some(old) if rank < old => minimumEnvRank(rest, Some(rank))
                case _ => minimumEnvRank(rest, best)
            }
    }

def firstOriginAtRank(rank: Int32, candidates: List[(Scheme, Int32, String)]): Option[String] =
    match candidates {
        case Nil => None
        case (_, r, origin) :: _ if r == rank => Some(origin)
        case _ :: rest => firstOriginAtRank(rank, rest)
    }

def ambiguousOriginsAtRank(rank: Int32, bestOrigin: String, candidates: List[(Scheme, Int32, String)], acc: List[String]): List[String] =
    match candidates {
        case Nil => acc
        case (_, r, origin) :: rest if r == rank and origin != bestOrigin and not containsString(origin, acc) =>
            ambiguousOriginsAtRank(rank, bestOrigin, rest, origin :: acc)
        case _ :: rest => ambiguousOriginsAtRank(rank, bestOrigin, rest, acc)
    }

def choicesAtRankOrigin(rank: Int32, origin: String, candidates: List[(Scheme, Int32, String)], acc: List[Scheme]): List[Scheme] =
    match candidates {
        case Nil => List.reverse(acc)
        case (scheme, r, o) :: rest if r == rank and o == origin => choicesAtRankOrigin(rank, origin, rest, scheme :: acc)
        case _ :: rest => choicesAtRankOrigin(rank, origin, rest, acc)
    }

def fallbackChoicesAfterLocal(bestRank: Int32, candidates: List[(Scheme, Int32, String)]): List[Scheme] =
    choicesBelowRank(bestRank, candidates, Nil)

def choicesBelowRank(rank: Int32, candidates: List[(Scheme, Int32, String)], acc: List[Scheme]): List[Scheme] =
    match candidates {
        case Nil => List.reverse(acc)
        case (scheme, r, _) :: rest if r > rank => choicesBelowRank(rank, rest, scheme :: acc)
        case _ :: rest => choicesBelowRank(rank, rest, acc)
    }

def showOrigins(origins: List[String]): String =
    match origins {
        case Nil => ""
        case x :: Nil => x
        case x :: rest => x + ", " + showOrigins(rest)
    }

def isQualifiedEnvName(name: String): Bool =
    containsAtChar(String.toList(name))

def containsAtChar(cs: List[Char]): Bool =
    match cs {
        case Nil => false
        case '@' :: _ => true
        case _ :: rest => containsAtChar(rest)
    }

def lastQualifiedSegment(name: String): String =
    charsToString(lastQualifiedSegmentChars(String.toList(name), Nil))

def lastQualifiedSegmentChars(cs: List[Char], acc: List[Char]): List[Char] =
    match cs {
        case Nil => List.reverse(acc)
        case '@' :: rest => lastQualifiedSegmentChars(rest, Nil)
        case c :: rest => lastQualifiedSegmentChars(rest, c :: acc)
    }

def baseEnv(): List[(String, Scheme, Int32, String)] =
    baseBinding("to-string", native1("integer", Nil, "string")) ::
    baseBinding("to-string", native1("float", Nil, "string")) ::
    baseBinding("from-string", native1("string", nativeEffect1("fail", "string") :: Nil, "float")) ::
    baseBinding("⌊", native1("float", Nil, "integer")) ::
    baseBinding("≤", native2("float", "float", Nil, "𝟚")) ::
    baseBinding("+", native2("float", "float", Nil, "float")) ::
    baseBinding("-", native1("float", Nil, "float")) ::
    baseBinding("¯", native1("float", Nil, "float")) ::
    baseBinding("×", native2("float", "float", Nil, "float")) ::
    baseBinding("÷", native2("integer", "integer", nativeEffect1("fail", "string") :: Nil, "integer")) ::
    baseBinding("/", native1("float", nativeEffect1("fail", "string") :: Nil, "float")) ::
    baseBinding("exp", native1("float", Nil, "float")) ::
    baseBinding("log", native1("float", Nil, "float")) ::
    baseBinding("sin", native1("float", Nil, "float")) ::
    baseBinding("cos", native1("float", Nil, "float")) ::
    baseBinding("write-line", native1("string", nativeEffect("console") :: Nil, "𝟙")) ::
    baseBinding("read-line", native1("𝟙", nativeEffect("console") :: Nil, "string")) ::
    baseBinding("random-float", native1("𝟙", nativeEffect("random") :: Nil, "float")) ::
    baseBinding("random-integer", native1("𝟙", nativeEffect("random") :: Nil, "integer")) ::
    baseBinding("divide", native2("integer", "integer", nativeEffect1("fail", "string") :: Nil, "integer")) ::
    Nil

def baseBinding(name: String, scheme: Scheme): (String, Scheme, Int32, String) =
    (name, scheme, baseEnvRank(), "base@" + name)

def nativeEffect(name: String): Ty =
    Ty.App(name, Nil)

def nativeEffect1(name: String, arg: String): Ty =
    Ty.App(name, Ty.Con(arg) :: Nil)

def native1(arg: String, effects: List[Ty], ret: String): Scheme =
    Scheme.Forall(Nil, curriedFunction(Ty.Con(arg) :: Nil, effects, Ty.Con(ret)))

def native2(arg0: String, arg1: String, effects: List[Ty], ret: String): Scheme =
    Scheme.Forall(Nil, curriedFunction(Ty.Con(arg0) :: Ty.Con(arg1) :: Nil, effects, Ty.Con(ret)))

def addBoneMembers(boneName: String, params: List[String], needs: List[BoneNeed], members: List[BoneMember], ctx: TcContext): TcContext =
    addBoneMembersToEnv(boneName, members, addBoneInfo(boneName, params, needs, members, ctx))

def addBoneInfo(boneName: String, params: List[String], needs: List[BoneNeed], members: List[BoneMember], ctx: TcContext): TcContext =
    match ctx {
        case TcContext.Context(env, types, bones) => TcContext.Context(env, types, (boneName, BoneInfo.Info(params, needs, members)) :: bones)
    }

def addBoneMembersToEnv(boneName: String, members: List[BoneMember], ctx: TcContext): TcContext =
    match members {
        case Nil => ctx
        case member :: rest =>
            match boneMemberSignature(member) {
                case None => addBoneMembersToEnv(boneName, rest, ctx)
                case Some((name, t)) =>
                    let scheme = typeExprScheme(t, ctx);
                    addBoneMembersToEnv(boneName, rest, addEnvAlias(name, boneName + "@" + name, scheme, addEnv(boneName + "@" + name, scheme, ctx)))
            }
    }

def boneMemberSignature(member: BoneMember): Option[(String, TypeExpr)] =
    match member {
        case BoneMember.Spec(name, t) => Some((name, t))
        case BoneMember.Default(name, Some(t), _) => Some((name, t))
        case BoneMember.Default(_, None, _) => None
    }

def findBoneInfo(boneName: String, ctx: TcContext): Option[BoneInfo] =
    match ctx {
        case TcContext.Context(_, _, bones) => findBoneInfoIn(boneName, bones)
    }

def findBoneInfoIn(boneName: String, bones: List[(String, BoneInfo)]): Option[BoneInfo] =
    match bones {
        case Nil => None
        case (name, info) :: _ if name == boneName => Some(info)
        case _ :: rest => findBoneInfoIn(boneName, rest)
    }

def findBoneMember(name: String, members: List[BoneMember]): Option[TypeExpr] =
    match members {
        case Nil => None
        case member :: rest =>
            match boneMemberSignature(member) {
                case Some((n, t)) if n == name => Some(t)
                case _ => findBoneMember(name, rest)
            }
    }
