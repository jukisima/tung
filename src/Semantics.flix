pub def check(source: String): String =
    checkWithImports(source, Nil)

pub def checkWithImports(source: String, imports: List[(String, String)]): String =
    match parse(source) {
        case ParseResult.Err(msg) => "parse error: ${msg}"
        case ParseResult.Ok(p, _) =>
            match inferProgramWithImports(p, imports) {
                case TcResult.Err(msg) => "type error: ${msg}"
                case TcResult.Ok(_, _) => "type ok"
            }
    }

def inferProgram(p: Program): TcResult[Program] =
    inferProgramWithImports(p, Nil)

def inferProgramWithImports(p: Program, imports: List[(String, String)]): TcResult[Program] =
    match p {
        case Program.Decls(ds) =>
            match prepareDecls(ds, imports, TcContext.Context(baseEnv(), Nil, Nil)) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((typedDs, ctx), st) => checkDecls(typedDs, ctx, st)
            }
    }

def inferProgramContext(p: Program, imports: List[(String, String)], baseCtx: TcContext): TcResult[TcContext] =
    match p {
        case Program.Decls(ds) =>
            match prepareDecls(ds, imports, baseCtx) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((typedDs, ctx), st) => checkDeclsContext(typedDs, ctx, st)
            }
    }

def prepareDecls(ds: List[Decl], imports: List[(String, String)], baseCtx: TcContext): TcResult[(List[Decl], TcContext)] =
    match collectImportContexts(ds, imports, baseCtx) {
        case TcResult.Err(msg) => TcResult.Err(msg)
        case TcResult.Ok(importCtx, st0) =>
            match inferBoneDefaultDecls(ds, importCtx, st0, Nil) {
                case TcResult.Err(msg) => TcResult.Err(msg)
                case TcResult.Ok((typedDs, _), st1) => TcResult.Ok((typedDs, collectProgramContext(typedDs, importCtx)), st1)
            }
    }

def collectImportContexts(ds: List[Decl], imports: List[(String, String)], ctx: TcContext): TcResult[TcContext] =
    match ds {
        case Nil => TcResult.Ok(ctx, TcState.State(0, Nil, Nil))
        case Decl.Import(path) :: rest =>
            match lookupImport(path, imports) {
                case None => TcResult.Err("missing bring '${path}'")
                case Some(source) =>
                    match parse(source) {
                        case ParseResult.Err(msg) => TcResult.Err("in bring '${path}': ${msg}")
                        case ParseResult.Ok(p, _) =>
                            match inferProgramContext(p, imports, TcContext.Context(baseEnv(), Nil, Nil)) {
                                case TcResult.Err(msg) => TcResult.Err("in bring '${path}': ${msg}")
                                case TcResult.Ok(importedCtx, _) =>
                                    collectImportContexts(rest, imports, mergeContext(importedVisibleContext(importNamespace(path), importedCtx), ctx))
                            }
                    }
            }
        case _ :: rest => collectImportContexts(rest, imports, ctx)
    }
