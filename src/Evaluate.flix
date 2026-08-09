import java.lang.{System => JSystem}
import java.util.{Random => JRandom}
import java.util.{Scanner => JScanner}

enum RuntimeValue {
    case Unit
    case Integer(Int32)
    case Float(String)
    case Character(String)
    case Text(String)
    case Record(List[(String, RuntimeValue)])
    case Closure(List[Pattern], Expr, RuntimeEnv)
    case RecursiveClosure(String, List[Pattern], Expr, RuntimeEnv)
    case Matcher(List[MatchCase], RuntimeEnv, List[RuntimeValue])
    case RecursiveMatcher(String, List[MatchCase], RuntimeEnv, List[RuntimeValue])
    case Constructor(String, String, Int32, List[RuntimeValue])
    case Data(String, String, List[RuntimeValue])
    case Native(String, Int32, List[RuntimeValue])
    case EffectOp(String, String, Int32, List[RuntimeValue])
}

enum RuntimeEnv {
    case Env(List[(String, RuntimeValue)], List[(String, BoneInfo)])
}

enum RuntimeError {
    case Message(String)
    case Effect(String)
}

enum RuntimeResult[a] {
    case Ok(a)
    case Err(RuntimeError)
}

enum PatternBind {
    case Matched(RuntimeEnv)
    case NoMatch
}

pub def evaluate(source: String): String =
    match parse(source) {
        case ParseResult.Err(msg) => "parse error: ${msg}"
        case ParseResult.Ok(program, _) =>
            match evalProgram(program) {
                case RuntimeResult.Ok(value) => "eval ok: " + showRuntimeValue(value)
                case RuntimeResult.Err(err) => "eval error: " + showRuntimeError(err)
            }
    }

pub def evaluateWithImports(source: String, imports: List[(String, String)]): String =
    match parse(source) {
        case ParseResult.Err(msg) => "parse error: ${msg}"
        case ParseResult.Ok(program, _) =>
            match evalProgramWithImports(program, imports) {
                case RuntimeResult.Ok(value) => "eval ok: " + showRuntimeValue(value)
                case RuntimeResult.Err(err) => "eval error: " + showRuntimeError(err)
            }
    }

def evalProgram(program: Program): RuntimeResult[RuntimeValue] =
    evalProgramWithImports(program, Nil)

def evalProgramWithImports(program: Program, imports: List[(String, String)]): RuntimeResult[RuntimeValue] =
    match evalProgramEnvWithImports(program, imports) {
        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
        case RuntimeResult.Ok((_, last)) => RuntimeResult.Ok(last)
    }

def evalImportedProgram(program: Program, imports: List[(String, String)]): RuntimeResult[RuntimeEnv] =
    match evalProgramEnvWithImports(program, imports) {
        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
        case RuntimeResult.Ok((env, _)) => RuntimeResult.Ok(env)
    }

def evalProgramEnvWithImports(program: Program, imports: List[(String, String)]): RuntimeResult[(RuntimeEnv, RuntimeValue)] =
    match program {
        case Program.Decls(ds) => evalDeclsWithImports(ds, imports, baseRuntimeEnv(), RuntimeValue.Unit)
    }

def evalDeclsWithImports(ds: List[Decl], imports: List[(String, String)], env: RuntimeEnv, last: RuntimeValue): RuntimeResult[(RuntimeEnv, RuntimeValue)] =
    match ds {
        case Nil => RuntimeResult.Ok((env, last))
        case Decl.Import(path) :: rest =>
            match evalImport(path, imports) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(importedEnv) =>
                    let env2 = mergeRuntimeEnv(importedVisibleRuntimeEnv(importNamespace(path), importedEnv), env);
                    evalDeclsWithImports(rest, imports, env2, last)
            }
        case d :: rest =>
            match evalDecl(d, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok((env2, None)) => evalDeclsWithImports(rest, imports, env2, last)
                case RuntimeResult.Ok((env2, Some(value))) => evalDeclsWithImports(rest, imports, env2, value)
            }
    }

def evalImport(path: String, imports: List[(String, String)]): RuntimeResult[RuntimeEnv] =
    match lookupImport(path, imports) {
        case None => RuntimeResult.Err(RuntimeError.Message("missing bring '${path}'"))
        case Some(source) =>
            match parse(source) {
                case ParseResult.Err(msg) => RuntimeResult.Err(RuntimeError.Message("in bring '${path}': ${msg}"))
                case ParseResult.Ok(program, _) =>
                    match evalImportedProgram(program, imports) {
                        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                        case RuntimeResult.Ok(env) => RuntimeResult.Ok(env)
                    }
            }
    }

def importedVisibleRuntimeEnv(ns: String, env: RuntimeEnv): RuntimeEnv =
    namespaceRuntimeImportEnv(ns, env)

def namespaceRuntimeImportEnv(ns: String, env: RuntimeEnv): RuntimeEnv =
    match env {
        case RuntimeEnv.Env(values, bones) => RuntimeEnv.Env(namespaceRuntimeImportValues(ns, values), namespaceImportBones(ns, bones))
    }

def namespaceRuntimeImportValues(ns: String, values: List[(String, RuntimeValue)]): List[(String, RuntimeValue)] =
    match values {
        case Nil => Nil
        case (name, _) :: rest if isBaseRuntimeImportName(name) => namespaceRuntimeImportValues(ns, rest)
        case (name, value) :: rest =>
            let value2 = namespaceRuntimeValue(ns, value);
            (lastQualifiedSegment(name), value2) :: (ns + "@" + name, value2) :: namespaceRuntimeImportValues(ns, rest)
    }

def namespaceRuntimeValue(ns: String, value: RuntimeValue): RuntimeValue =
    match value {
        case RuntimeValue.Data(qualifiedName, displayName, args) => RuntimeValue.Data(ns + "@" + qualifiedName, displayName, args)
        case RuntimeValue.Constructor(qualifiedName, displayName, arity, args) => RuntimeValue.Constructor(ns + "@" + qualifiedName, displayName, arity, args)
        case RuntimeValue.EffectOp(effectName, _, _, _) if isBaseRuntimeEffectName(effectName) => value
        case RuntimeValue.EffectOp(effectName, opName, arity, args) => RuntimeValue.EffectOp(ns + "@" + effectName, opName, arity, args)
        case _ => value
    }

def mergeRuntimeEnv(left: RuntimeEnv, right: RuntimeEnv): RuntimeEnv =
    match (left, right) {
        case (RuntimeEnv.Env(leftValues, leftBones), RuntimeEnv.Env(rightValues, rightBones)) => RuntimeEnv.Env(appendList(leftValues, rightValues), appendList(leftBones, rightBones))
    }

def evalDecl(d: Decl, env: RuntimeEnv): RuntimeResult[(RuntimeEnv, Option[RuntimeValue])] =
    match d {
        case Decl.Import(path) => RuntimeResult.Err(RuntimeError.Message("evaluation does not support bring '${path}'"))
        case Decl.TypeAlias(_, _) => RuntimeResult.Ok((env, None))
        case Decl.Data(_, name, ctors) => RuntimeResult.Ok((addConstructors(name, ctors, env), None))
        case Decl.Effect(_, name, ops) => RuntimeResult.Ok((addEffectOps(name, ops, env), None))
        case Decl.Bone(params, boneName, needs, members) => evalBoneDefaultMembers(boneName, members, addRuntimeBoneInfo(boneName, params, needs, members, env))
        case Decl.Flesh(_, boneName, members) => evalFleshMembers(boneName, members, env)
        case Decl.Let(name, _, expr) =>
            match evalBoundValue(name, expr, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(value) =>
                    let last = if (name == "_") Some(value) else None;
                    RuntimeResult.Ok((addValue(name, value, env), last))
            }
    }

def evalBoundValue(name: String, expr: Expr, env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match expr {
        case Expr.Lambda(ps, body) => RuntimeResult.Ok(RuntimeValue.RecursiveClosure(name, ps, body, env))
        case Expr.Match(Nil, cases) => RuntimeResult.Ok(RuntimeValue.RecursiveMatcher(name, cases, env, Nil))
        case _ => evalExpr(expr, env)
    }

def evalBoneDefaultMembers(boneName: String, members: List[BoneMember], env: RuntimeEnv): RuntimeResult[(RuntimeEnv, Option[RuntimeValue])] =
    match members {
        case Nil => RuntimeResult.Ok((env, None))
        case BoneMember.Spec(_, _) :: rest => evalBoneDefaultMembers(boneName, rest, env)
        case BoneMember.Default(name, _, expr) :: rest =>
            match evalBoundValue(name, expr, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(value) =>
                    let env2 = addValue(boneName + "@" + name, value, addValue(name, value, env));
                    match evalBoneDefaultMembers(boneName, rest, env2) {
                        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                        case RuntimeResult.Ok((env3, _)) => RuntimeResult.Ok((env3, Some(value)))
                    }
            }
    }

def evalFleshMembers(boneName: String, members: List[Decl], env: RuntimeEnv): RuntimeResult[(RuntimeEnv, Option[RuntimeValue])] =
    match members {
        case Nil => RuntimeResult.Ok((env, None))
        case Decl.Let(name, _, expr) :: rest =>
            match evalBoundValue(name, expr, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(value) =>
                    let env2 = addFleshValue(boneName, name, value, env);
                    match evalFleshMembers(boneName, rest, env2) {
                        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                        case RuntimeResult.Ok((env3, _)) => RuntimeResult.Ok((env3, Some(value)))
                    }
            }
        case _ :: _ => RuntimeResult.Err(RuntimeError.Message("flesh member must be a let"))
    }

def evalExpr(expr: Expr, env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match expr {
        case Expr.Integer(i) => RuntimeResult.Ok(RuntimeValue.Integer(i))
        case Expr.Float(s) => RuntimeResult.Ok(RuntimeValue.Float(s))
        case Expr.Char(c) => RuntimeResult.Ok(RuntimeValue.Character(c))
        case Expr.String(s) => RuntimeResult.Ok(RuntimeValue.Text(s))
        case Expr.Var(name) => evalVar(name, env)
        case Expr.Apply(f, args) => evalApply(f, args, env)
        case Expr.Record(fields) => evalRecord(fields, env, Nil)
        case Expr.Update(base, updates) => evalRecordUpdate(base, updates, env)
        case Expr.Try(body, cases) => evalTry(body, cases, env)
        case Expr.Lambda(ps, body) => RuntimeResult.Ok(RuntimeValue.Closure(ps, body, env))
        case Expr.Match(scrutinees, cases) => evalMatchExpr(scrutinees, cases, env)
        case Expr.Block(ds, body) => evalBlock(ds, body, env)
    }

def evalVar(name: String, env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match lookupValue(name, env) {
        case None => RuntimeResult.Err(RuntimeError.Message("unknown name '${name}'"))
        case Some(value) => RuntimeResult.Ok(value)
    }

def evalApply(f: Expr, args: List[Expr], env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match evalExpr(f, env) {
        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
        case RuntimeResult.Ok(fn) => evalApplyArgs(fn, args, env)
    }

def evalApplyArgs(fn: RuntimeValue, args: List[Expr], env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match args {
        case Nil => RuntimeResult.Ok(fn)
        case arg :: rest =>
            match evalExpr(arg, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(argValue) =>
                    match applyOne(fn, argValue) {
                        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                        case RuntimeResult.Ok(nextFn) => evalApplyArgs(nextFn, rest, env)
                    }
            }
    }

def applyOne(fn: RuntimeValue, arg: RuntimeValue): RuntimeResult[RuntimeValue] =
    match fn {
        case RuntimeValue.Closure(Nil, _, _) => RuntimeResult.Err(RuntimeError.Message("cannot apply value with no parameters"))
        case RuntimeValue.Closure(p :: rest, body, closureEnv) =>
            match bindRuntimePattern(p, arg, closureEnv) {
                case PatternBind.NoMatch => RuntimeResult.Err(RuntimeError.Message("function argument did not match pattern"))
                case PatternBind.Matched(boundEnv) =>
                    match rest {
                        case Nil => evalExpr(body, boundEnv)
                        case _ => RuntimeResult.Ok(RuntimeValue.Closure(rest, body, boundEnv))
                    }
            }
        case RuntimeValue.RecursiveClosure(_, Nil, _, _) => RuntimeResult.Err(RuntimeError.Message("cannot apply value with no parameters"))
        case RuntimeValue.RecursiveClosure(name, p :: rest, body, closureEnv) =>
            let self = RuntimeValue.RecursiveClosure(name, p :: rest, body, closureEnv);
            match bindRuntimePattern(p, arg, addValue(name, self, closureEnv)) {
                case PatternBind.NoMatch => RuntimeResult.Err(RuntimeError.Message("function argument did not match pattern"))
                case PatternBind.Matched(boundEnv) =>
                    match rest {
                        case Nil => evalExpr(body, boundEnv)
                        case _ => RuntimeResult.Ok(RuntimeValue.Closure(rest, body, boundEnv))
                    }
            }
        case RuntimeValue.Matcher(cases, matchEnv, supplied) =>
            let supplied2 = appendRuntimeValue(supplied, arg);
            match matchCaseArityRuntime(cases) {
                case None => RuntimeResult.Err(RuntimeError.Message("empty match"))
                case Some(arity) if List.length(supplied2) < arity => RuntimeResult.Ok(RuntimeValue.Matcher(cases, matchEnv, supplied2))
                case Some(arity) if List.length(supplied2) == arity => evalMatchCases(cases, supplied2, matchEnv)
                case _ => RuntimeResult.Err(RuntimeError.Message("too many arguments for match"))
            }
        case RuntimeValue.RecursiveMatcher(name, cases, matchEnv, supplied) =>
            let supplied2 = appendRuntimeValue(supplied, arg);
            let self = RuntimeValue.RecursiveMatcher(name, cases, matchEnv, Nil);
            match matchCaseArityRuntime(cases) {
                case None => RuntimeResult.Err(RuntimeError.Message("empty match"))
                case Some(arity) if List.length(supplied2) < arity => RuntimeResult.Ok(RuntimeValue.RecursiveMatcher(name, cases, matchEnv, supplied2))
                case Some(arity) if List.length(supplied2) == arity => evalMatchCases(cases, supplied2, addValue(name, self, matchEnv))
                case _ => RuntimeResult.Err(RuntimeError.Message("too many arguments for match"))
            }
        case RuntimeValue.Constructor(qualifiedName, displayName, arity, supplied) =>
            let supplied2 = appendRuntimeValue(supplied, arg);
            if (List.length(supplied2) < arity) {
                RuntimeResult.Ok(RuntimeValue.Constructor(qualifiedName, displayName, arity, supplied2))
            } else {
                RuntimeResult.Ok(RuntimeValue.Data(qualifiedName, displayName, supplied2))
            }
        case RuntimeValue.Native(name, arity, supplied) =>
            let supplied2 = appendRuntimeValue(supplied, arg);
            if (List.length(supplied2) < arity) RuntimeResult.Ok(RuntimeValue.Native(name, arity, supplied2)) else evalNative(name, supplied2)
        case RuntimeValue.EffectOp(effectName, opName, arity, supplied) =>
            let supplied2 = appendRuntimeValue(supplied, arg);
            if (List.length(supplied2) < arity) RuntimeResult.Ok(RuntimeValue.EffectOp(effectName, opName, arity, supplied2)) else evalEffectOp(effectName, opName, supplied2)
        case _ => RuntimeResult.Err(RuntimeError.Message("cannot apply " + showRuntimeValue(fn)))
    }

def evalRecord(fields: List[(String, Expr)], env: RuntimeEnv, acc: List[(String, RuntimeValue)]): RuntimeResult[RuntimeValue] =
    match fields {
        case Nil => RuntimeResult.Ok(RuntimeValue.Record(List.reverse(acc)))
        case (name, expr) :: rest =>
            match evalExpr(expr, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(value) => evalRecord(rest, env, (name, value) :: acc)
            }
    }

def evalRecordUpdate(base: Expr, updates: List[RecordUpdate], env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match evalExpr(base, env) {
        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
        case RuntimeResult.Ok(RuntimeValue.Record(fields)) => evalRecordUpdates(updates, fields, env)
        case RuntimeResult.Ok(value) => RuntimeResult.Err(RuntimeError.Message("record update expected record, found " + showRuntimeValue(value)))
    }

def evalRecordUpdates(updates: List[RecordUpdate], fields: List[(String, RuntimeValue)], env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match updates {
        case Nil => RuntimeResult.Ok(RuntimeValue.Record(fields))
        case RecordUpdate.Remove(name) :: rest =>
            match removeRuntimeField(name, fields, Nil) {
                case None => RuntimeResult.Err(RuntimeError.Message("unknown record field '${name}'"))
                case Some(fields2) => evalRecordUpdates(rest, fields2, env)
            }
        case RecordUpdate.Set(name, expr) :: rest =>
            match evalExpr(expr, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(value) => evalRecordUpdates(rest, upsertRuntimeField(name, value, fields), env)
            }
    }

def evalTry(body: Expr, cases: List[HandlerCase], env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match evalExpr(body, env) {
        case RuntimeResult.Ok(value) => RuntimeResult.Ok(value)
        case RuntimeResult.Err(RuntimeError.Effect(effectName)) =>
            match findHandler(effectName, cases) {
                case None => RuntimeResult.Err(RuntimeError.Effect(effectName))
                case Some(handlerBody) => evalExpr(handlerBody, env)
            }
        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
    }

def findHandler(effectName: String, cases: List[HandlerCase]): Option[Expr] =
    match cases {
        case Nil => None
        case HandlerCase.Case(name, body) :: _ if name == effectName => Some(body)
        case _ :: rest => findHandler(effectName, rest)
    }

def evalMatchExpr(scrutinees: List[Expr], cases: List[MatchCase], env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match scrutinees {
        case Nil => RuntimeResult.Ok(RuntimeValue.Matcher(cases, env, Nil))
        case _ =>
            match evalExprList(scrutinees, env, Nil) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(values) => evalMatchCases(cases, values, env)
            }
    }

def evalExprList(exprs: List[Expr], env: RuntimeEnv, acc: List[RuntimeValue]): RuntimeResult[List[RuntimeValue]] =
    match exprs {
        case Nil => RuntimeResult.Ok(List.reverse(acc))
        case expr :: rest =>
            match evalExpr(expr, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(value) => evalExprList(rest, env, value :: acc)
            }
    }

def evalMatchCases(cases: List[MatchCase], values: List[RuntimeValue], env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match cases {
        case Nil => RuntimeResult.Err(RuntimeError.Message("non-exhaustive match"))
        case MatchCase.Case(patterns, body) :: rest =>
            match bindRuntimePatterns(patterns, values, env) {
                case PatternBind.NoMatch => evalMatchCases(rest, values, env)
                case PatternBind.Matched(boundEnv) => evalExpr(body, boundEnv)
            }
    }

def evalBlock(ds: List[Decl], body: Expr, env: RuntimeEnv): RuntimeResult[RuntimeValue] =
    match evalLocalDecls(ds, env) {
        case RuntimeResult.Err(err) => RuntimeResult.Err(err)
        case RuntimeResult.Ok(env2) => evalExpr(body, env2)
    }

def evalLocalDecls(ds: List[Decl], env: RuntimeEnv): RuntimeResult[RuntimeEnv] =
    match ds {
        case Nil => RuntimeResult.Ok(env)
        case Decl.Let(name, _, expr) :: rest =>
            match evalExpr(expr, env) {
                case RuntimeResult.Err(err) => RuntimeResult.Err(err)
                case RuntimeResult.Ok(value) => evalLocalDecls(rest, addValue(name, value, env))
            }
        case _ :: rest => evalLocalDecls(rest, env)
    }

def bindRuntimePatterns(patterns: List[Pattern], values: List[RuntimeValue], env: RuntimeEnv): PatternBind =
    match (patterns, values) {
        case (Nil, Nil) => PatternBind.Matched(env)
        case (p :: prest, v :: vrest) =>
            match bindRuntimePattern(p, v, env) {
                case PatternBind.NoMatch => PatternBind.NoMatch
                case PatternBind.Matched(env2) => bindRuntimePatterns(prest, vrest, env2)
            }
        case _ => PatternBind.NoMatch
    }

def bindRuntimePattern(pattern: Pattern, value: RuntimeValue, env: RuntimeEnv): PatternBind =
    match pattern {
        case Pattern.Var("_") => PatternBind.Matched(env)
        case Pattern.Var(name) =>
            if (isNullaryConstructorPattern(name, env)) {
                if (runtimeConstructorMatches(name, value)) PatternBind.Matched(env) else PatternBind.NoMatch
            } else {
                PatternBind.Matched(addValue(name, value, env))
            }
        case Pattern.Con(name, args) =>
            match value {
                case RuntimeValue.Data(_, _, fields) if runtimeConstructorMatches(name, value) => bindRuntimePatterns(args, fields, env)
                case _ => PatternBind.NoMatch
            }
    }

def isNullaryConstructorPattern(name: String, env: RuntimeEnv): Bool =
    match lookupValue(name, env) {
        case Some(RuntimeValue.Data(_, _, Nil)) => true
        case _ => false
    }

def runtimeConstructorMatches(patternName: String, value: RuntimeValue): Bool =
    match value {
        case RuntimeValue.Data(qualifiedName, displayName, _) =>
            if (isRuntimeQualifiedName(patternName)) patternName == qualifiedName else patternName == displayName
        case _ => false
    }

def evalNative(name: String, args: List[RuntimeValue]): RuntimeResult[RuntimeValue] =
    match (name, args) {
        case ("+", RuntimeValue.Integer(a) :: RuntimeValue.Integer(b) :: Nil) => RuntimeResult.Ok(RuntimeValue.Integer(a + b))
        case ("+", RuntimeValue.Float(a) :: RuntimeValue.Float(b) :: Nil) => evalFloatBinary("+", a, b)
        case ("-", RuntimeValue.Integer(a) :: RuntimeValue.Integer(b) :: Nil) => RuntimeResult.Ok(RuntimeValue.Integer(a - b))
        case ("-", RuntimeValue.Float(a) :: Nil) => evalFloatNegate(a)
        case ("-", RuntimeValue.Float(a) :: RuntimeValue.Float(b) :: Nil) => evalFloatBinary("-", a, b)
        case ("×", RuntimeValue.Integer(a) :: RuntimeValue.Integer(b) :: Nil) => RuntimeResult.Ok(RuntimeValue.Integer(a * b))
        case ("×", RuntimeValue.Float(a) :: RuntimeValue.Float(b) :: Nil) => evalFloatBinary("×", a, b)
        case ("¯", RuntimeValue.Integer(a) :: Nil) => RuntimeResult.Ok(RuntimeValue.Integer(-a))
        case ("¯", RuntimeValue.Float(a) :: Nil) => evalFloatNegate(a)
        case ("÷", RuntimeValue.Integer(_) :: RuntimeValue.Integer(0) :: Nil) => RuntimeResult.Err(RuntimeError.Effect("fail"))
        case ("÷", RuntimeValue.Integer(a) :: RuntimeValue.Integer(b) :: Nil) => RuntimeResult.Ok(RuntimeValue.Integer(a / b))
        case ("divide", RuntimeValue.Integer(_) :: RuntimeValue.Integer(0) :: Nil) => RuntimeResult.Err(RuntimeError.Effect("fail"))
        case ("divide", RuntimeValue.Integer(a) :: RuntimeValue.Integer(b) :: Nil) => RuntimeResult.Ok(RuntimeValue.Integer(a / b))
        case ("≡", a :: b :: Nil) => RuntimeResult.Ok(runtimeBool(runtimeEquals(a, b)))
        case ("≤", RuntimeValue.Integer(a) :: RuntimeValue.Integer(b) :: Nil) => RuntimeResult.Ok(runtimeBool(a <= b))
        case ("≤", RuntimeValue.Float(a) :: RuntimeValue.Float(b) :: Nil) => evalFloatLessEqual(a, b)
        case ("/", RuntimeValue.Float(a) :: Nil) => evalFloatReciprocal(a)
        case ("⌊", RuntimeValue.Float(a) :: Nil) => evalFloatFloor(a)
        case ("exp", RuntimeValue.Float(a) :: Nil) => evalFloatExp(a)
        case ("from-string", RuntimeValue.Text(text) :: Nil) => floatFromString(text)
        case ("to-string", value :: Nil) => RuntimeResult.Ok(RuntimeValue.Text(showRuntimeValue(value)))
        case _ => RuntimeResult.Err(RuntimeError.Message("native '${name}' does not support these arguments"))
    }

def evalEffectOp(effectName: String, opName: String, args: List[RuntimeValue]): RuntimeResult[RuntimeValue] =
    match (effectName, opName, args) {
        case ("console", "write-line", RuntimeValue.Text(text) :: Nil) => writeConsoleLine(text)
        case ("console", "read-line", RuntimeValue.Data(_, "null", Nil) :: Nil) => RuntimeResult.Ok(RuntimeValue.Text(readConsoleLine()))
        case ("random", "random-float", RuntimeValue.Data(_, "null", Nil) :: Nil) => RuntimeResult.Ok(RuntimeValue.Float(Float64.toString(nextRandomFloat())))
        case ("random", "random-integer", RuntimeValue.Data(_, "null", Nil) :: Nil) => RuntimeResult.Ok(RuntimeValue.Integer(nextRandomInteger()))
        case _ => RuntimeResult.Err(RuntimeError.Effect(effectName))
    }

def writeConsoleLine(text: String): RuntimeResult[RuntimeValue] =
    unsafe IO {
        println(text);
        RuntimeResult.Ok(RuntimeValue.Data("𝟙@null", "null", Nil))
    }

def readConsoleLine(): String =
    unsafe IO {
        let scanner = new JScanner(JSystem.in);
        if (scanner.hasNextLine()) scanner.nextLine() else ""
    }

def nextRandomFloat(): Float64 =
    unsafe IO {
        let rnd = new JRandom();
        rnd.nextDouble()
    }

def nextRandomInteger(): Int32 =
    unsafe IO {
        let rnd = new JRandom();
        rnd.nextInt()
    }

def evalFloatNegate(text: String): RuntimeResult[RuntimeValue] =
    match Float64.fromString(text) {
        case None => RuntimeResult.Err(RuntimeError.Message("invalid float '${text}'"))
        case Some(x) => RuntimeResult.Ok(RuntimeValue.Float(Float64.toString(-x)))
    }

def evalFloatBinary(name: String, left: String, right: String): RuntimeResult[RuntimeValue] =
    match (Float64.fromString(left), Float64.fromString(right)) {
        case (Some(x), Some(y)) => RuntimeResult.Ok(RuntimeValue.Float(Float64.toString(evalFloatBinaryValue(name, x, y))))
        case _ => RuntimeResult.Err(RuntimeError.Message("invalid float"))
    }

def evalFloatBinaryValue(name: String, x: Float64, y: Float64): Float64 =
    match name {
        case "+" => x + y
        case "-" => x - y
        case "×" => x * y
        case _ => x
    }

def evalFloatLessEqual(left: String, right: String): RuntimeResult[RuntimeValue] =
    match (Float64.fromString(left), Float64.fromString(right)) {
        case (Some(x), Some(y)) => RuntimeResult.Ok(runtimeBool(x <= y))
        case _ => RuntimeResult.Err(RuntimeError.Message("invalid float"))
    }

def evalFloatReciprocal(text: String): RuntimeResult[RuntimeValue] =
    match Float64.fromString(text) {
        case Some(0.0f64) => RuntimeResult.Err(RuntimeError.Effect("fail"))
        case Some(x) => RuntimeResult.Ok(RuntimeValue.Float(Float64.toString(1.0f64 / x)))
        case None => RuntimeResult.Err(RuntimeError.Message("invalid float '${text}'"))
    }

def evalFloatFloor(text: String): RuntimeResult[RuntimeValue] =
    match Float64.fromString(text) {
        case Some(x) => RuntimeResult.Ok(RuntimeValue.Integer(Float64.truncateToInt32(Float64.floor(x))))
        case None => RuntimeResult.Err(RuntimeError.Message("invalid float '${text}'"))
    }

def evalFloatExp(text: String): RuntimeResult[RuntimeValue] =
    match Float64.fromString(text) {
        case Some(x) => RuntimeResult.Ok(RuntimeValue.Float(Float64.toString(Float64.exp(x))))
        case None => RuntimeResult.Err(RuntimeError.Message("invalid float '${text}'"))
    }

def floatFromString(text: String): RuntimeResult[RuntimeValue] =
    match Float64.fromString(text) {
        case Some(x) => RuntimeResult.Ok(RuntimeValue.Float(Float64.toString(x)))
        case None => RuntimeResult.Err(RuntimeError.Effect("fail"))
    }

def runtimeBool(b: Bool): RuntimeValue =
    if (b) RuntimeValue.Data("𝟚@yea", "yea", Nil) else RuntimeValue.Data("𝟚@nay", "nay", Nil)

def runtimeEquals(a: RuntimeValue, b: RuntimeValue): Bool =
    match (a, b) {
        case (RuntimeValue.Unit, RuntimeValue.Unit) => true
        case (RuntimeValue.Integer(x), RuntimeValue.Integer(y)) => x == y
        case (RuntimeValue.Float(x), RuntimeValue.Float(y)) => x == y
        case (RuntimeValue.Character(x), RuntimeValue.Character(y)) => x == y
        case (RuntimeValue.Text(x), RuntimeValue.Text(y)) => x == y
        case (RuntimeValue.Data(xName, _, xArgs), RuntimeValue.Data(yName, _, yArgs)) => xName == yName and runtimeEqualsList(xArgs, yArgs)
        case (RuntimeValue.Record(xs), RuntimeValue.Record(ys)) => runtimeEqualsFields(xs, ys)
        case _ => false
    }

def runtimeEqualsList(xs: List[RuntimeValue], ys: List[RuntimeValue]): Bool =
    match (xs, ys) {
        case (Nil, Nil) => true
        case (x :: xr, y :: yr) => runtimeEquals(x, y) and runtimeEqualsList(xr, yr)
        case _ => false
    }

def runtimeEqualsFields(xs: List[(String, RuntimeValue)], ys: List[(String, RuntimeValue)]): Bool =
    match (xs, ys) {
        case (Nil, Nil) => true
        case ((xName, xValue) :: xr, (yName, yValue) :: yr) => xName == yName and runtimeEquals(xValue, yValue) and runtimeEqualsFields(xr, yr)
        case _ => false
    }

def addConstructors(dataName: String, ctors: List[Ctor], env: RuntimeEnv): RuntimeEnv =
    match ctors {
        case Nil => env
        case Ctor.Case(name, fields) :: rest =>
            let arity = List.length(fields);
            let qualifiedName = dataName + "@" + name;
            let value = if (arity == 0) RuntimeValue.Data(qualifiedName, name, Nil) else RuntimeValue.Constructor(qualifiedName, name, arity, Nil);
            addConstructors(dataName, rest, addValue(name, value, addValue(qualifiedName, value, env)))
    }

def addEffectOps(effectName: String, ops: List[EffectOp], env: RuntimeEnv): RuntimeEnv =
    match ops {
        case Nil => env
        case EffectOp.Op(name, t) :: rest =>
            let value = RuntimeValue.EffectOp(effectName, name, typeArity(t), Nil);
            addEffectOps(effectName, rest, addValue(name, value, addValue(effectName + "@" + name, value, env)))
    }

def typeArity(t: TypeExpr): Int32 =
    match t {
        case TypeExpr.Arrow(args, _, _) => List.length(args)
        case _ => 0
    }

def baseRuntimeEnv(): RuntimeEnv =
    addBaseEffectOps(addBaseNatives(RuntimeEnv.Env(Nil, Nil)))

def isBaseRuntimeName(name: String): Bool =
    name == "to-string" or name == "from-string" or name == "⌊" or name == "≤" or name == "≡" or name == "÷" or name == "divide" or name == "/" or name == "¯" or name == "×" or name == "-" or name == "+" or name == "exp" or name == "log" or name == "sin" or name == "cos" or name == "read-line" or name == "write-line" or name == "random-float" or name == "random-integer"

def isBaseRuntimeImportName(name: String): Bool =
    isBaseRuntimeName(name) or isBaseRuntimeName(lastQualifiedSegment(name))

def isBaseRuntimeEffectName(name: String): Bool =
    name == "io" or name == "console" or name == "random" or name == "fail" or name == "state"

def addBaseNatives(env: RuntimeEnv): RuntimeEnv =
    addNatives(baseNativeSpecs(), env)

def baseNativeSpecs(): List[(String, Int32)] =
    ("to-string", 1) ::
    ("from-string", 1) ::
    ("⌊", 1) ::
    ("≤", 2) ::
    ("≡", 2) ::
    ("÷", 2) ::
    ("divide", 2) ::
    ("/", 1) ::
    ("exp", 1) ::
    ("log", 1) ::
    ("sin", 1) ::
    ("cos", 1) ::
    ("¯", 1) ::
    ("×", 2) ::
    ("-", 2) ::
    ("+", 2) ::
    Nil

def addNatives(specs: List[(String, Int32)], env: RuntimeEnv): RuntimeEnv =
    match specs {
        case Nil => env
        case (name, arity) :: rest => addNatives(rest, addNative(name, arity, env))
    }

def addBaseEffectOps(env: RuntimeEnv): RuntimeEnv =
    addBaseEffectOp("random", "random-integer", 1,
    addBaseEffectOp("random", "random-float", 1,
    addBaseEffectOp("console", "read-line", 1,
    addBaseEffectOp("console", "write-line", 1, env))))

def addNative(name: String, arity: Int32, env: RuntimeEnv): RuntimeEnv =
    addValue(name, RuntimeValue.Native(name, arity, Nil), env)

def addBaseEffectOp(effectName: String, opName: String, arity: Int32, env: RuntimeEnv): RuntimeEnv =
    addValue(opName, RuntimeValue.EffectOp(effectName, opName, arity, Nil), env)

def addValue(name: String, value: RuntimeValue, env: RuntimeEnv): RuntimeEnv =
    match env {
        case RuntimeEnv.Env(values, bones) => RuntimeEnv.Env((name, value) :: values, bones)
    }

def lookupValue(name: String, env: RuntimeEnv): Option[RuntimeValue] =
    match env {
        case RuntimeEnv.Env(values, _) => lookupValueIn(name, values)
    }

def lookupValueIn(name: String, values: List[(String, RuntimeValue)]): Option[RuntimeValue] =
    match values {
        case Nil => None
        case (n, v) :: _ if n == name => Some(v)
        case _ :: rest => lookupValueIn(name, rest)
    }

def addRuntimeBoneInfo(boneName: String, params: List[String], needs: List[BoneNeed], members: List[BoneMember], env: RuntimeEnv): RuntimeEnv =
    match env {
        case RuntimeEnv.Env(values, bones) => RuntimeEnv.Env(values, (boneName, BoneInfo.Info(params, needs, members)) :: bones)
    }

def addFleshValue(boneName: String, memberName: String, value: RuntimeValue, env: RuntimeEnv): RuntimeEnv =
    addFleshOwnerValues(memberName, runtimeMemberOwnersOrDirect(boneName, memberName, env), value, addValue(memberName, value, env))

def addFleshOwnerValues(memberName: String, owners: List[String], value: RuntimeValue, env: RuntimeEnv): RuntimeEnv =
    match owners {
        case Nil => env
        case owner :: rest => addFleshOwnerValues(memberName, rest, value, addValue(owner + "@" + memberName, value, env))
    }

def runtimeMemberOwnersOrDirect(boneName: String, memberName: String, env: RuntimeEnv): List[String] =
    match runtimeMemberOwners(boneName, memberName, env) {
        case Nil => boneName :: Nil
        case owners => owners
    }

def runtimeMemberOwners(boneName: String, memberName: String, env: RuntimeEnv): List[String] =
    match env {
        case RuntimeEnv.Env(_, bones) => runtimeMemberOwnersIn(boneName, memberName, bones, Nil)
    }

def runtimeMemberOwnersIn(boneName: String, memberName: String, bones: List[(String, BoneInfo)], seen: List[String]): List[String] =
    if (containsString(boneName, seen)) {
        Nil
    } else {
        match findBoneInfoIn(boneName, bones) {
            case None => Nil
            case Some(BoneInfo.Info(_, needs, members)) =>
                let own = match findBoneMember(memberName, members) {
                    case None => Nil
                    case Some(_) => boneName :: Nil
                };
                appendList(own, runtimeMemberOwnersInNeeds(needs, memberName, bones, boneName :: seen))
        }
    }

def runtimeMemberOwnersInNeeds(needs: List[BoneNeed], memberName: String, bones: List[(String, BoneInfo)], seen: List[String]): List[String] =
    match needs {
        case Nil => Nil
        case BoneNeed.Need(_, neededName) :: rest => appendList(runtimeMemberOwnersIn(neededName, memberName, bones, seen), runtimeMemberOwnersInNeeds(rest, memberName, bones, seen))
    }

def appendRuntimeValue(xs: List[RuntimeValue], x: RuntimeValue): List[RuntimeValue] =
    match xs {
        case Nil => x :: Nil
        case y :: rest => y :: appendRuntimeValue(rest, x)
    }

def removeRuntimeField(name: String, fields: List[(String, RuntimeValue)], acc: List[(String, RuntimeValue)]): Option[List[(String, RuntimeValue)]] =
    match fields {
        case Nil => None
        case (fieldName, _) :: rest if fieldName == name => Some(appendList(List.reverse(acc), rest))
        case field :: rest => removeRuntimeField(name, rest, field :: acc)
    }

def upsertRuntimeField(name: String, value: RuntimeValue, fields: List[(String, RuntimeValue)]): List[(String, RuntimeValue)] =
    match fields {
        case Nil => (name, value) :: Nil
        case (fieldName, _) :: rest if fieldName == name => (name, value) :: rest
        case field :: rest => field :: upsertRuntimeField(name, value, rest)
    }

def matchCaseArityRuntime(cases: List[MatchCase]): Option[Int32] =
    match cases {
        case Nil => None
        case MatchCase.Case(ps, _) :: _ => Some(List.length(ps))
    }

def isRuntimeQualifiedName(name: String): Bool =
    runtimeContainsAt(String.toList(name))

def runtimeContainsAt(chars: List[Char]): Bool =
    match chars {
        case Nil => false
        case '@' :: _ => true
        case _ :: rest => runtimeContainsAt(rest)
    }

def showRuntimeError(err: RuntimeError): String =
    match err {
        case RuntimeError.Message(msg) => msg
        case RuntimeError.Effect(effectName) => "unhandled effect '${effectName}'"
    }

def showRuntimeValue(value: RuntimeValue): String =
    match value {
        case RuntimeValue.Unit => "()"
        case RuntimeValue.Integer(i) => "${i}"
        case RuntimeValue.Float(s) => s
        case RuntimeValue.Character(c) => "`" + c
        case RuntimeValue.Text(s) => "'" + s + "'"
        case RuntimeValue.Record(fields) => "[" + showRuntimeFields(fields) + "]"
        case RuntimeValue.Data(_, displayName, Nil) => displayName
        case RuntimeValue.Data(_, displayName, args) => "(" + showRuntimeValues(args) + " " + displayName + ")"
        case RuntimeValue.Closure(_, _, _) => "<function>"
        case RuntimeValue.RecursiveClosure(_, _, _, _) => "<function>"
        case RuntimeValue.Matcher(_, _, _) => "<function>"
        case RuntimeValue.RecursiveMatcher(_, _, _, _) => "<function>"
        case RuntimeValue.Constructor(_, displayName, _, _) => "<constructor " + displayName + ">"
        case RuntimeValue.Native(name, _, _) => "<native " + name + ">"
        case RuntimeValue.EffectOp(_, opName, _, _) => "<effect " + opName + ">"
    }

def showRuntimeValues(values: List[RuntimeValue]): String =
    match values {
        case Nil => ""
        case x :: Nil => showRuntimeValue(x)
        case x :: rest => showRuntimeValue(x) + " " + showRuntimeValues(rest)
    }

def showRuntimeFields(fields: List[(String, RuntimeValue)]): String =
    match fields {
        case Nil => ""
        case (name, value) :: Nil => name + " = " + showRuntimeValue(value)
        case (name, value) :: rest => name + " = " + showRuntimeValue(value) + ", " + showRuntimeFields(rest)
    }
