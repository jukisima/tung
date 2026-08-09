enum Program {
    case Decls(List[Decl])
}

enum Decl {
    case Import(String)
    case Let(String, Option[TypeExpr], Expr)
    case TypeAlias(String, TypeExpr)
    case Data(List[String], String, List[Ctor])
    case Effect(List[String], String, List[EffectOp])
    case Bone(List[String], String, List[BoneNeed], List[BoneMember])
    case Flesh(TypeExpr, String, List[Decl])
}

enum EffectOp {
    case Op(String, TypeExpr)
}

enum Ctor {
    case Case(String, List[TypeExpr])
}

enum BoneMember {
    case Spec(String, TypeExpr)
    case Default(String, Option[TypeExpr], Expr)
}

enum BoneNeed {
    case Need(List[TypeExpr], String)
}

enum TypeExpr {
    case Name(String)
    case Apply(String, List[TypeExpr])
    case Record(List[(String, TypeExpr)])
    case Arrow(List[TypeExpr], List[TypeExpr], TypeExpr)
    case Forall(List[TypeExpr], TypeExpr)
}

enum Expr {
    case Integer(Int32)
    case Float(String)
    case Char(String)
    case String(String)
    case Var(String)
    case Apply(Expr, List[Expr])
    case Record(List[(String, Expr)])
    case Update(Expr, List[RecordUpdate])
    case Try(Expr, List[HandlerCase])
    case Lambda(List[Pattern], Expr)
    case Match(List[Expr], List[MatchCase])
    case Block(List[Decl], Expr)
}

enum HandlerCase {
    case Case(String, Expr)
}

enum RecordUpdate {
    case Set(String, Expr)
    case Remove(String)
}

enum Pattern {
    case Var(String)
    case Con(String, List[Pattern])
}

enum MatchCase {
    case Case(List[Pattern], Expr)
}

enum AppPart {
    case Arg(Expr)
    case Fun(Expr)
}

enum HeaderPart {
    case Plain(List[Token])
    case Marked(List[Token])
}

enum TypePart {
    case Arg(TypeExpr)
    case Fun(TypeExpr)
}

enum Token {
    case Ident(String)
    case Integer(Int32)
    case Float(String)
    case Char(String)
    case String(String)
    case Let
    case Bring
    case Type
    case Data
    case Effect
    case Bone
    case Flesh
    case Match
    case Try
    case Func
    case LParen
    case RParen
    case LBrace
    case RBrace
    case LBracket
    case RBracket
    case Colon
    case Comma
    case MapsTo
    case Arrow
    case Forall
    case Bang
    case Equals
    case Semicolon
    case Dot
    case Dollar
}

enum ParseResult[a] {
    case Ok(a, List[Token])
    case Err(String)
}

enum LexResult {
    case Ok(List[Token])
    case Err(String)
}

pub def parse(source: String): ParseResult[Program] =
    match lex(source) {
        case LexResult.Err(msg) => ParseResult.Err(msg)
        case LexResult.Ok(ts)   => parseProgram(ts)
    }

def parseProgram(ts: List[Token]): ParseResult[Program] =
    match parseDecls(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(ds, rest) =>
            match rest {
                case Nil => ParseResult.Ok(Program.Decls(List.reverse(ds)), Nil)
                case _   => ParseResult.Err("unexpected tokens after program")
            }
    }

def parseDecls(ts: List[Token], acc: List[Decl]): ParseResult[List[Decl]] =
    match ts {
        case Nil => ParseResult.Ok(acc, Nil)
        case Token.RBrace :: _ => ParseResult.Ok(acc, ts)
        case Token.Semicolon :: rest => parseDecls(rest, acc)
        case _ =>
            match parseDecl(ts) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(d, rest) => parseDecls(dropDot(rest), d :: acc)
            }
    }

def parseDecl(ts: List[Token]): ParseResult[Decl] =
    match ts {
        case Token.Bring :: rest => parseImport(rest)
        case Token.Let :: Token.Ident(name) :: rest => parseLet(name, rest)
        case Token.Let :: Token.LParen :: rest => parseTypedArgumentLet(rest)
        case Token.Type :: Token.Ident(name) :: Token.Equals :: rest => parseTypeAlias(name, rest)
        case Token.Data :: rest => parseData(rest)
        case Token.Effect :: rest => parseEffect(rest)
        case Token.Bone :: rest => parseBone(rest)
        case Token.Flesh :: rest => parseFlesh(rest)
        case _ =>
            match parseExpr(ts) {
                case ParseResult.Err(_) => ParseResult.Err("expected declaration")
                case ParseResult.Ok(e, rest) => ParseResult.Ok(Decl.Let("_", None, e), rest)
            }
    }

def parseImport(ts: List[Token]): ParseResult[Decl] =
    match parseImportPath(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(parts, Token.Semicolon :: rest) => ParseResult.Ok(Decl.Import(joinPathParts(List.reverse(parts))), rest)
        case ParseResult.Ok(_, Nil) => ParseResult.Err("expected ';' after bring")
        case ParseResult.Ok(_, _) => ParseResult.Err("expected ';' after bring")
    }

def parseImportPath(ts: List[Token], acc: List[String]): ParseResult[List[String]] =
    match ts {
        case Token.Ident(part) :: Token.Dot :: Token.Ident(next) :: rest => parseImportPath(Token.Ident(next) :: rest, part :: acc)
        case Token.Ident(part) :: rest => ParseResult.Ok(part :: acc, rest)
        case _ => ParseResult.Err("expected bring path")
    }

def joinPathParts(parts: List[String]): String =
    match parts {
        case Nil => ""
        case x :: Nil => x
        case x :: rest => x + "." + joinPathParts(rest)
    }

def parseTypeAlias(name: String, ts: List[Token]): ParseResult[Decl] =
    match parseTypeUntilDot(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(t, rest) => ParseResult.Ok(Decl.TypeAlias(name, t), rest)
    }

def parseLet(name: String, ts: List[Token]): ParseResult[Decl] =
    match ts {
        case Token.Equals :: rest => parseUntypedLet(name, rest)
        case Token.Colon :: rest => parseColonLet(name, rest)
        case Token.Ident("_") :: rest =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, rest2) => ParseResult.Ok(Decl.Let(name, None, e), rest2)
            }
        case _ => parseLetWithOptionalType(name, ts)
    }

def parseColonLet(name: String, ts: List[Token]): ParseResult[Decl] =
    match parseTypeUntilEquals(ts, Nil) {
        case ParseResult.Ok(t, Token.Equals :: rest) =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, rest2) => ParseResult.Ok(Decl.Let(name, Some(t), e), rest2)
            }
        case _ => ParseResult.Err("expected '=' after let type")
    }

def parseLetWithOptionalType(name: String, ts: List[Token]): ParseResult[Decl] =
    match parseLetHeaderDefinition(Token.Ident(name) :: ts) {
        case Some(result) => result
        case None =>
            match parseTypeUntilExpr(ts, Nil) {
                case ParseResult.Ok(t, exprTokens) =>
                    match parseExpr(exprTokens) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(e, rest) => ParseResult.Ok(Decl.Let(name, Some(t), e), rest)
                    }
                case ParseResult.Err(_) => parseUntypedLet(name, ts)
            }
    }

def parseLetHeaderDefinition(ts: List[Token]): Option[ParseResult[Decl]] =
    match takeUntilEquals(ts, Nil) {
        case ParseResult.Ok(parts, Token.Equals :: rest) =>
            let header = List.reverse(parts);
            match splitTopLevelColon(header, Nil, 0) {
                case Some((left, right)) =>
                    match functionHeader(left) {
                        case Some((params, name)) =>
                            match parseTypeTokens(right) {
                                case ParseResult.Ok(t, Nil) => Some(parseLetHeaderTypedBody(name, params, t, rest))
                                case ParseResult.Err(msg) => Some(ParseResult.Err(msg))
                                case _ => Some(ParseResult.Err("could not parse let type annotation"))
                            }
                        case None => Some(ParseResult.Err("expected function header before ':'"))
                    }
                case None =>
                    match functionHeader(header) {
                        case Some((params, name)) => Some(parseLetHeaderBody(name, params, rest))
                        case None => None
                    }
            }
        case _ => None
    }

def parseLetHeaderBody(name: String, params: List[Pattern], ts: List[Token]): ParseResult[Decl] =
    match parseExpr(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(e, rest) => ParseResult.Ok(fleshMemberLet(name, params, e), rest)
    }

def parseLetHeaderTypedBody(name: String, params: List[Pattern], t: TypeExpr, ts: List[Token]): ParseResult[Decl] =
    match parseExpr(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(e, rest) => ParseResult.Ok(Decl.Let(name, Some(t), Expr.Lambda(params, e)), rest)
    }

def parseTypedArgumentLet(ts: List[Token]): ParseResult[Decl] =
    match parseTypedLetParams(ts, Nil, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok((params, argTypes), Token.Ident(name) :: Token.Colon :: rest) =>
            match parseTypeUntilEquals(rest, Nil) {
                case ParseResult.Ok(resultType, Token.Equals :: bodyTokens) =>
                    match parseExpr(bodyTokens) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(body, rest2) =>
                            let fullType = typedArgumentLetType(argTypes, resultType);
                            ParseResult.Ok(Decl.Let(name, Some(fullType), Expr.Lambda(params, body)), rest2)
                    }
                case _ => ParseResult.Err("expected '=' after let result type")
            }
        case ParseResult.Ok(_, _) => ParseResult.Err("expected function name and result type after typed arguments")
    }

def parseTypedLetParams(ts: List[Token], params: List[Pattern], argTypes: List[TypeExpr]): ParseResult[(List[Pattern], List[TypeExpr])] =
    match ts {
        case Token.RParen :: rest => ParseResult.Ok((List.reverse(params), List.reverse(argTypes)), rest)
        case Token.Comma :: rest => parseTypedLetParams(rest, params, argTypes)
        case _ =>
            match parsePatternUntilColon(ts, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(patternTokens, Token.Colon :: typeTokens) =>
                    match parsePattern(List.reverse(patternTokens)) {
                        case ParseResult.Ok(pattern, Nil) =>
                            match parseTypeUntilCommaOrParen(typeTokens, Nil) {
                                case ParseResult.Ok(argType, rest) => parseTypedLetParams(rest, pattern :: params, argType :: argTypes)
                                case ParseResult.Err(msg) => ParseResult.Err(msg)
                            }
                        case _ => ParseResult.Err("could not parse typed let argument pattern")
                    }
                case _ => ParseResult.Err("expected ':' in typed let argument")
            }
    }

def typedArgumentLetType(argTypes: List[TypeExpr], resultType: TypeExpr): TypeExpr =
    match resultType {
        case TypeExpr.Arrow(Nil, effects, ret) => TypeExpr.Arrow(argTypes, effects, ret)
        case _ => TypeExpr.Arrow(argTypes, Nil, resultType)
    }

def parseUntypedLet(name: String, ts: List[Token]): ParseResult[Decl] =
    match parseExpr(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(e, rest) => ParseResult.Ok(Decl.Let(name, None, e), rest)
    }

def parseData(ts: List[Token]): ParseResult[Decl] =
    match takeHeaderTokens(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(header, Token.LBrace :: rest) =>
            match headerFromTokens(List.reverse(header)) {
                case None => ParseResult.Err("data declaration requires a name")
                case Some((paramTerms, name)) =>
                    match namesFromHeaderTerms(paramTerms, Nil) {
                        case None => ParseResult.Err("data parameters must be names")
                        case Some(params) =>
                            match parseCtors(rest, Nil) {
                                case ParseResult.Err(msg) => ParseResult.Err(msg)
                                case ParseResult.Ok(ctors, rest2) => ParseResult.Ok(Decl.Data(params, name, List.reverse(ctors)), rest2)
                            }
                    }
            }
        case _ => ParseResult.Err("expected '{' after data header")
    }

def parseEffect(ts: List[Token]): ParseResult[Decl] =
    match takeHeaderTokens(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(header, Token.LBrace :: rest) =>
            match headerFromTokens(List.reverse(header)) {
                case None => ParseResult.Err("effect declaration requires a name")
                case Some((paramTerms, name)) =>
                    match namesFromHeaderTerms(paramTerms, Nil) {
                        case None => ParseResult.Err("effect parameters must be names")
                        case Some(params) =>
                            match parseEffectOps(rest, Nil) {
                                case ParseResult.Err(msg) => ParseResult.Err(msg)
                                case ParseResult.Ok(ops, rest2) => ParseResult.Ok(Decl.Effect(params, name, List.reverse(ops)), rest2)
                            }
                    }
            }
        case _ => ParseResult.Err("expected '{' after effect header")
    }

def parseEffectOps(ts: List[Token], acc: List[EffectOp]): ParseResult[List[EffectOp]] =
    match ts {
        case Token.RBrace :: rest => ParseResult.Ok(acc, rest)
        case Token.Ident(name) :: Token.Colon :: rest =>
            match parseTypeUntilCommaOrBrace(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(t, rest2) => parseEffectOps(dropComma(rest2), EffectOp.Op(name, t) :: acc)
            }
        case _ => ParseResult.Err("expected effect operation")
    }

def parseBone(ts: List[Token]): ParseResult[Decl] =
    match takeHeaderTokens(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(header, Token.LBrace :: rest) =>
            let (boneHeader, needHeader) = splitBoneHeaderNeed(List.reverse(header));
            match headerFromTokens(boneHeader) {
                case None => ParseResult.Err("bone declaration requires a name")
                case Some((paramTerms, name)) =>
                    match namesFromHeaderTerms(paramTerms, Nil) {
                        case None => ParseResult.Err("bone parameters must be names")
                        case Some(params) =>
                            match parseBoneNeeds(needHeader) {
                                case ParseResult.Err(msg) => ParseResult.Err(msg)
                                case ParseResult.Ok(needs, _) =>
                                    match parseBoneMembers(rest, Nil) {
                                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                                        case ParseResult.Ok(ms, rest2) => ParseResult.Ok(Decl.Bone(params, name, needs, List.reverse(ms)), rest2)
                                    }
                            }
                    }
            }
        case _ => ParseResult.Err("expected '{' after bone header")
    }

def splitBoneHeaderNeed(ts: List[Token]): (List[Token], List[Token]) =
    match splitTopLevel(ts, Nil, 0, t -> match t { case Token.Ident("need") => true case _ => false }) {
        case Some((left, right)) => (left, right)
        case None => (ts, Nil)
    }

def parseBoneNeeds(ts: List[Token]): ParseResult[List[BoneNeed]] =
    parseBoneNeedsLoop(ts, Nil)

def parseBoneNeedsLoop(ts: List[Token], acc: List[BoneNeed]): ParseResult[List[BoneNeed]] =
    match ts {
        case Nil => ParseResult.Ok(List.reverse(acc), Nil)
        case Token.Comma :: rest => parseBoneNeedsLoop(rest, acc)
        case _ =>
            match takeTopLevelUntilOrEnd(ts, Nil, 0, t -> match t { case Token.Comma => true case _ => false }) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(parts, rest) =>
                    match parseBoneNeed(List.reverse(parts)) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(need, _) => parseBoneNeedsLoop(dropComma(rest), need :: acc)
                    }
            }
    }

def parseBoneNeed(ts: List[Token]): ParseResult[BoneNeed] =
    match ts {
        case Nil => ParseResult.Err("empty bone need")
        case _ =>
            match headerFromTokens(ts) {
                case None => ParseResult.Err("invalid bone need")
                case Some((argTerms, name)) =>
                    match typesFromHeaderTerms(argTerms, Nil) {
                        case None => ParseResult.Err("bone need arguments must be types")
                        case Some(args) => ParseResult.Ok(BoneNeed.Need(args, name), Nil)
                    }
            }
    }

def parseFlesh(ts: List[Token]): ParseResult[Decl] =
    match takeHeaderTokens(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(header, Token.LBrace :: rest) =>
            match headerFromTokens(List.reverse(header)) {
                case None => ParseResult.Err("flesh declaration requires a bone name")
                case Some((tyTerms, boneName)) =>
                    match typeFromHeaderTerms(tyTerms) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(ty, _) =>
                            match parseFleshMembers(rest, Nil) {
                                case ParseResult.Err(msg) => ParseResult.Err(msg)
                                case ParseResult.Ok(ds, Token.RBrace :: rest2) => ParseResult.Ok(Decl.Flesh(ty, boneName, List.reverse(ds)), rest2)
                                case _ => ParseResult.Err("expected '}' after flesh body")
                            }
                    }
            }
        case _ => ParseResult.Err("expected '{' after flesh header")
    }

def parseFleshMembers(ts: List[Token], acc: List[Decl]): ParseResult[List[Decl]] =
    match ts {
        case Token.RBrace :: _ => ParseResult.Ok(acc, ts)
        case Token.Comma :: rest => parseFleshMembers(rest, acc)
        case Token.Semicolon :: rest => parseFleshMembers(rest, acc)
        case Token.Let :: Token.Ident(name) :: rest =>
            match parseLet(name, rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(d, rest2) => parseFleshMembers(rest2, d :: acc)
            }
        case Token.Ident(_) :: _ => parsePostfixFleshMember(ts, acc)
        case Token.LParen :: _ => parsePostfixFleshMember(ts, acc)
        case _ => ParseResult.Err("expected flesh member")
    }

def parsePostfixFleshMember(ts: List[Token], acc: List[Decl]): ParseResult[List[Decl]] =
    match takeUntilMatch(ts, Nil) {
        case ParseResult.Ok(parts, Token.Match :: rest) =>
            match postfixFleshMemberHeader(List.reverse(parts)) {
                case Some(_) => parsePostfixFleshMemberBody(parts, Token.Match :: rest, acc)
                case None => parsePostfixFleshMemberWithEquals(ts, acc)
            }
        case _ => parsePostfixFleshMemberWithEquals(ts, acc)
    }

def parsePostfixFleshMemberWithEquals(ts: List[Token], acc: List[Decl]): ParseResult[List[Decl]] =
    match takeUntilEquals(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(parts, Token.Equals :: rest) => parsePostfixFleshMemberBody(parts, rest, acc)
        case _ => ParseResult.Err("expected '=' in flesh member")
    }

def parsePostfixFleshMemberBody(parts: List[Token], bodyTokens: List[Token], acc: List[Decl]): ParseResult[List[Decl]] =
    match postfixFleshMemberHeader(List.reverse(parts)) {
        case None => ParseResult.Err("expected flesh member name")
        case Some((params, name)) =>
            match parseExpr(bodyTokens) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, rest) => parseFleshMembers(dropCommaOrDot(rest), fleshMemberLet(name, params, e) :: acc)
            }
    }

def fleshMemberLet(name: String, params: List[Pattern], expr: Expr): Decl =
    Decl.Let(name, None, lambdaIfParams(params, expr))

def lambdaIfParams(params: List[Pattern], expr: Expr): Expr =
    match params {
        case Nil => expr
        case _ => Expr.Lambda(params, expr)
    }

def parseCtors(ts: List[Token], acc: List[Ctor]): ParseResult[List[Ctor]] =
    match ts {
        case Token.RBrace :: rest => ParseResult.Ok(acc, rest)
        case _ =>
            match takeCtorTokens(ts, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(parts, rest) =>
                    let ctorTokens = List.reverse(parts);
                    match ctorFromTokens(ctorTokens) {
                        case None => ParseResult.Err("invalid constructor '" + showTokens(ctorTokens) + "' before " + showTokenHead(rest))
                        case Some(c) => parseCtors(dropComma(rest), c :: acc)
                    }
            }
    }

def showTokens(ts: List[Token]): String =
    match ts {
        case Nil => ""
        case x :: Nil => showToken(x)
        case x :: rest => showToken(x) + " " + showTokens(rest)
    }

def showToken(t: Token): String =
    match t {
        case Token.Ident(name) => name
        case Token.LParen => "("
        case Token.RParen => ")"
        case _ => "?"
    }

def showTokenHead(ts: List[Token]): String =
    match ts {
        case Nil => "end of input"
        case Token.Comma :: _ => "','"
        case Token.RBrace :: _ => "'}'"
        case Token.Ident(name) :: _ => "'" + name + "'"
        case _ => "token"
    }

def parseBoneMembers(ts: List[Token], acc: List[BoneMember]): ParseResult[List[BoneMember]] =
    match ts {
        case Token.RBrace :: rest => ParseResult.Ok(acc, rest)
        case Token.Comma :: rest => parseBoneMembers(rest, acc)
        case Token.Semicolon :: rest => parseBoneMembers(rest, acc)
        case Token.Let :: Token.Ident(name) :: Token.Colon :: rest =>
            match parseLetBoneMember(name, rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(member, rest2) => parseBoneMembers(rest2, member :: acc)
            }
        case Token.Let :: Token.LParen :: rest => parseTypedArgumentBoneDefault(rest, acc)
        case Token.Let :: Token.Ident(_) :: _ => parsePostfixBoneDefault(ts, acc)
        case Token.Ident(_) :: _ => parsePostfixBoneSignature(ts, acc)
        case Token.LParen :: _ => parsePostfixBoneSignature(ts, acc)
        case _ => ParseResult.Err("expected bone member")
    }

def parseLetBoneMember(name: String, ts: List[Token]): ParseResult[BoneMember] =
    match parseTypeUntilCommaOrBraceOrEquals(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(t, Token.Equals :: rest) =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, rest2) => ParseResult.Ok(BoneMember.Default(name, Some(t), e), dropCommaOrDot(rest2))
            }
        case ParseResult.Ok(t, rest) => ParseResult.Ok(BoneMember.Spec(name, t), dropCommaOrDot(rest))
    }

def parsePostfixBoneSignature(ts: List[Token], acc: List[BoneMember]): ParseResult[List[BoneMember]] =
    match takeUntilColon(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(parts, Token.Colon :: rest) =>
            match boneMemberHeader(List.reverse(parts)) {
                case None => ParseResult.Err("expected bone member name")
                case Some((argTypes, name)) =>
                    match parseBoneMemberResultType(rest) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok((ret, rest2), _) => parseBoneMembers(rest2, BoneMember.Spec(name, typedArgumentLetType(argTypes, ret)) :: acc)
                    }
            }
        case _ => ParseResult.Err("expected ':' in bone member")
    }

def boneMemberHeader(ts: List[Token]): Option[(List[TypeExpr], String)] =
    match headerFromTokens(ts) {
        case None => None
        case Some((argTerms, name)) =>
            match typesFromHeaderTerms(argTerms, Nil) {
                case None => None
                case Some(argTypes) => Some((argTypes, name))
            }
    }

def parseBoneMemberResultType(ts: List[Token]): ParseResult[(TypeExpr, List[Token])] =
    match parseTypeUntilCommaOrBraceOrEquals(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(_, Token.Equals :: _) => ParseResult.Err("bone default must start with let")
        case ParseResult.Ok(t, rest) => ParseResult.Ok((t, dropCommaOrDot(rest)), Nil)
    }

def parseTypedArgumentBoneDefault(ts: List[Token], acc: List[BoneMember]): ParseResult[List[BoneMember]] =
    match parseTypedLetParams(ts, Nil, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok((params, argTypes), Token.Ident(name) :: Token.Colon :: rest) =>
            match parseTypeUntilEquals(rest, Nil) {
                case ParseResult.Ok(resultType, Token.Equals :: bodyTokens) =>
                    match parseExpr(bodyTokens) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(body, rest2) =>
                            let fullType = typedArgumentLetType(argTypes, resultType);
                            parseBoneMembers(dropCommaOrDot(rest2), BoneMember.Default(name, Some(fullType), lambdaIfParams(params, body)) :: acc)
                    }
                case _ => ParseResult.Err("expected '=' after bone default type")
            }
        case ParseResult.Ok(_, _) => ParseResult.Err("expected default member name and result type after typed arguments")
    }

def parsePostfixBoneDefault(ts: List[Token], acc: List[BoneMember]): ParseResult[List[BoneMember]] =
    match ts {
        case Token.Let :: rest =>
            match takeUntilEquals(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(parts, Token.Equals :: rest2) =>
                    match parseBoneDefaultHeader(List.reverse(parts)) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok((params, name, ann), _) =>
                            match parseExpr(rest2) {
                                case ParseResult.Err(msg) => ParseResult.Err(msg)
                                case ParseResult.Ok(body, rest3) => parseBoneMembers(dropCommaOrDot(rest3), BoneMember.Default(name, ann, lambdaIfParams(params, body)) :: acc)
                            }
                    }
                case _ => ParseResult.Err("expected '=' in bone default")
            }
        case _ => ParseResult.Err("expected bone default")
    }

def parseBoneDefaultHeader(ts: List[Token]): ParseResult[(List[Pattern], String, Option[TypeExpr])] =
    match splitTopLevelColon(ts, Nil, 0) {
        case Some((left, right)) =>
            match functionHeader(left) {
                case None => ParseResult.Err("expected bone default member name")
                case Some((params, name)) =>
                    match parseTypeTokens(right) {
                        case ParseResult.Ok(t, Nil) => ParseResult.Ok((params, name, Some(t)), Nil)
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case _ => ParseResult.Err("could not parse bone default type annotation")
                    }
            }
        case None =>
            match functionHeader(ts) {
                case None => ParseResult.Err("expected bone default member name")
                case Some((params, name)) => ParseResult.Ok((params, name, None), Nil)
            }
    }

def parseExpr(ts: List[Token]): ParseResult[Expr] =
    parseExprNonLambda(ts)

def parseExprNonLambda(ts: List[Token]): ParseResult[Expr] =
    match parsePostfixExpr(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ok => ok
    }

def parseFunc(ts: List[Token]): ParseResult[Expr] =
    match parsePatternsUntilMapsTo(ts, Nil) {
        case ParseResult.Ok(ps, Token.MapsTo :: rest) =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(body, rest2) => ParseResult.Ok(Expr.Lambda(List.reverse(ps), body), rest2)
            }
        case _ => ParseResult.Err("not a lambda")
    }

def parsePostfixExpr(ts: List[Token]): ParseResult[Expr] =
    match parseExprParts(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(parts, rest) => applicationFromParts(List.reverse(parts), rest)
    }

def applyArgs(f: Expr, args: List[Expr]): Expr =
    match args {
        case Nil => f
        case arg :: rest => applyArgs(Expr.Apply(f, arg :: Nil), rest)
    }

def applicationFromParts(parts: List[AppPart], rest: List[Token]): ParseResult[Expr] =
    match splitMarkedFunction(parts, Nil, None) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok((Some(f), args), _) => ParseResult.Ok(applyArgs(f, args), rest)
        case ParseResult.Ok((None, _), _) => defaultApplication(parts, rest)
    }

def splitMarkedFunction(parts: List[AppPart], args: List[Expr], f: Option[Expr]): ParseResult[(Option[Expr], List[Expr])] =
    match parts {
        case Nil => ParseResult.Ok((f, List.reverse(args)), Nil)
        case AppPart.Arg(e) :: rest => splitMarkedFunction(rest, e :: args, f)
        case AppPart.Fun(e) :: rest =>
            match f {
                case None => splitMarkedFunction(rest, args, Some(e))
                case Some(_) => ParseResult.Err("application has multiple '$' function markers")
            }
    }

def defaultApplication(parts: List[AppPart], rest: List[Token]): ParseResult[Expr] =
    match partsToExprs(parts) {
        case Nil => ParseResult.Err("expected expression")
        case x :: Nil => ParseResult.Ok(x, rest)
        case arg :: f :: args => ParseResult.Ok(applyArgs(f, arg :: args), rest)
    }

def partsToExprs(parts: List[AppPart]): List[Expr] =
    match parts {
        case Nil => Nil
        case AppPart.Arg(e) :: rest => e :: partsToExprs(rest)
        case AppPart.Fun(e) :: rest => e :: partsToExprs(rest)
    }

def parseExprParts(ts: List[Token], acc: List[AppPart]): ParseResult[List[AppPart]] =
    match ts {
        case Nil => ParseResult.Ok(acc, Nil)
        case Token.Comma :: _ => ParseResult.Ok(acc, ts)
        case Token.Dot :: _ => ParseResult.Ok(acc, ts)
        case Token.Semicolon :: _ => ParseResult.Ok(acc, ts)
        case Token.MapsTo :: _ => ParseResult.Ok(acc, ts)
        case Token.RParen :: _ => ParseResult.Ok(acc, ts)
        case Token.RBrace :: _ => ParseResult.Ok(acc, ts)
        case Token.RBracket :: _ => ParseResult.Ok(acc, ts)
        case Token.Let :: _ => ParseResult.Ok(acc, ts)
        case Token.Bring :: _ => ParseResult.Ok(acc, ts)
        case Token.Type :: _ => ParseResult.Ok(acc, ts)
        case Token.Data :: _ => ParseResult.Ok(acc, ts)
        case Token.Effect :: _ => ParseResult.Ok(acc, ts)
        case Token.Bone :: _ => ParseResult.Ok(acc, ts)
        case Token.Flesh :: _ => ParseResult.Ok(acc, ts)
        case Token.Dollar :: rest =>
            match parseExprAtom(rest) {
                case ParseResult.Err(_) => ParseResult.Err("expected expression after '$'")
                case ParseResult.Ok(e, rest2) => parseExprParts(rest2, AppPart.Fun(e) :: acc)
            }
        case _ =>
            match parseExprAtom(ts) {
                case ParseResult.Err(_) =>
                    match acc {
                        case Nil => ParseResult.Err("expected expression atom")
                        case _ => ParseResult.Ok(acc, ts)
                    }
                case ParseResult.Ok(e, rest) => parseExprParts(rest, AppPart.Arg(e) :: acc)
            }
    }

def parseExprAtom(ts: List[Token]): ParseResult[Expr] =
    match ts {
        case Token.Integer(i) :: rest => ParseResult.Ok(Expr.Integer(i), rest)
        case Token.Float(s) :: rest => ParseResult.Ok(Expr.Float(s), rest)
        case Token.Char(c) :: rest => ParseResult.Ok(Expr.Char(c), rest)
        case Token.String(s) :: rest => ParseResult.Ok(Expr.String(s), rest)
        case Token.Ident(s) :: rest => ParseResult.Ok(Expr.Var(s), rest)
        case Token.Func :: rest => parseFunc(rest)
        case Token.Try :: rest => parseTry(rest)
        case Token.Match :: rest => parseMatchExpr(rest)
        case Token.LParen :: rest => parseParen(rest)
        case Token.LBracket :: rest => parseRecordExpr(rest)
        case _ => ParseResult.Err("expected expression atom")
    }

def parseMatchExpr(ts: List[Token]): ParseResult[Expr] =
    match ts {
        case Token.LBrace :: rest =>
            match parseMatchCases(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(cs, rest2) => ParseResult.Ok(Expr.Match(Nil, List.reverse(cs)), rest2)
            }
        case _ => parseMatchScrutinees(ts, Nil)
    }

def parseMatchScrutinees(ts: List[Token], acc: List[Expr]): ParseResult[Expr] =
    match parseExpr(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(scrutinee, Token.Comma :: rest) => parseMatchScrutinees(rest, scrutinee :: acc)
        case ParseResult.Ok(scrutinee, Token.LBrace :: rest) =>
            match parseMatchCases(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(cs, rest2) => ParseResult.Ok(Expr.Match(List.reverse(scrutinee :: acc), List.reverse(cs)), rest2)
            }
        case _ => ParseResult.Err("expected match scrutinee or '{'")
    }

def parseParen(ts: List[Token]): ParseResult[Expr] =
    match ts {
        case Token.RParen :: rest => ParseResult.Ok(Expr.Var("null"), rest)
        case Token.Let :: _ => parseBlock(ts)
        case _ =>
            match parseExpr(ts) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, Token.RParen :: rest) => ParseResult.Ok(e, rest)
                case _ => ParseResult.Err("expected ')' after expression")
            }
    }

def parseTry(ts: List[Token]): ParseResult[Expr] =
    match parseExpr(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(body, Token.LBrace :: rest) =>
            match parseHandlerCases(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(cases, rest2) => ParseResult.Ok(Expr.Try(body, List.reverse(cases)), rest2)
            }
        case ParseResult.Ok(body, rest) => ParseResult.Ok(body, rest)
    }

def parseHandlerCases(ts: List[Token], acc: List[HandlerCase]): ParseResult[List[HandlerCase]] =
    match ts {
        case Token.RBrace :: rest => ParseResult.Ok(acc, rest)
        case Token.Ident(effectName) :: Token.MapsTo :: rest =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, rest2) => parseHandlerCases(dropComma(rest2), HandlerCase.Case(effectName, e) :: acc)
            }
        case _ => ParseResult.Err("expected handler case")
    }

def parseBlock(ts: List[Token]): ParseResult[Expr] =
    match parseBlockLets(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(ds, rest) =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, Token.RParen :: rest2) => ParseResult.Ok(Expr.Block(List.reverse(ds), e), rest2)
                case ParseResult.Ok(e, Token.Semicolon :: Token.RParen :: rest2) => ParseResult.Ok(Expr.Block(List.reverse(ds), e), rest2)
                case _ => ParseResult.Err("expected ')' after block")
            }
    }

def parseBlockLets(ts: List[Token], acc: List[Decl]): ParseResult[List[Decl]] =
    match ts {
        case Token.Let :: _ =>
            match parseDecl(ts) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(d, Token.Semicolon :: rest) => parseBlockLets(rest, d :: acc)
                case ParseResult.Ok(_, _) => ParseResult.Err("expected ';' after block let")
            }
        case _ => ParseResult.Ok(acc, ts)
    }

def parseRecordExpr(ts: List[Token]): ParseResult[Expr] =
    match ts {
        case Token.Equals :: rest => parseRecordUpdate(rest)
        case _ =>
            match parseRecordExprFields(ts, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(fields, rest) => ParseResult.Ok(Expr.Record(List.reverse(fields)), rest)
            }
    }

def parseRecordUpdate(ts: List[Token]): ParseResult[Expr] =
    match parseExpr(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(base, Token.Comma :: rest) =>
            match parseRecordUpdates(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(updates, rest2) => ParseResult.Ok(Expr.Update(base, List.reverse(updates)), rest2)
            }
        case ParseResult.Ok(base, Token.RBracket :: rest) => ParseResult.Ok(Expr.Update(base, Nil), rest)
        case _ => ParseResult.Err("expected ',' or ']' after record update base")
    }

def parseRecordUpdates(ts: List[Token], acc: List[RecordUpdate]): ParseResult[List[RecordUpdate]] =
    match ts {
        case Token.RBracket :: rest => ParseResult.Ok(acc, rest)
        case Token.Ident(name) :: Token.Comma :: rest =>
            match removedFieldName(name) {
                case Some(fieldName) => parseRecordUpdates(rest, RecordUpdate.Remove(fieldName) :: acc)
                case None => ParseResult.Err("expected record update")
            }
        case Token.Ident(name) :: Token.RBracket :: rest =>
            match removedFieldName(name) {
                case Some(fieldName) => ParseResult.Ok(RecordUpdate.Remove(fieldName) :: acc, rest)
                case None => ParseResult.Err("expected record update")
            }
        case Token.Ident(name) :: Token.Equals :: rest =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, Token.Comma :: rest2) => parseRecordUpdates(rest2, RecordUpdate.Set(name, e) :: acc)
                case ParseResult.Ok(e, Token.RBracket :: rest2) => ParseResult.Ok(RecordUpdate.Set(name, e) :: acc, rest2)
                case _ => ParseResult.Err("expected ',' or ']' after record update field")
            }
        case _ => ParseResult.Err("expected record update")
    }

def removedFieldName(name: String): Option[String] =
    match String.toList(name) {
        case '-' :: c :: rest => Some(charsToString(c :: rest))
        case _ => None
    }

def parseRecordExprFields(ts: List[Token], acc: List[(String, Expr)]): ParseResult[List[(String, Expr)]] =
    match ts {
        case Token.RBracket :: rest => ParseResult.Ok(acc, rest)
        case Token.Ident(name) :: Token.Equals :: rest =>
            match parseExpr(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(e, Token.Comma :: rest2) => parseRecordExprFields(rest2, (name, e) :: acc)
                case ParseResult.Ok(e, Token.RBracket :: rest2) => ParseResult.Ok((name, e) :: acc, rest2)
                case _ => ParseResult.Err("expected ',' or ']' after record field")
            }
        case _ => ParseResult.Err("expected record field")
    }

def parseMatchCases(ts: List[Token], acc: List[MatchCase]): ParseResult[List[MatchCase]] =
    match ts {
        case Token.RBrace :: rest => ParseResult.Ok(acc, rest)
        case _ =>
            match parseMatchCasePatterns(ts, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(ps, Token.MapsTo :: rest) =>
                    match parseExpr(rest) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(e, rest2) => parseMatchCases(dropComma(rest2), MatchCase.Case(ps, e) :: acc)
                    }
                case _ => ParseResult.Err("expected '↦' in match case")
            }
    }

def parseMatchCasePatterns(ts: List[Token], acc: List[Pattern]): ParseResult[List[Pattern]] =
    match parsePattern(ts) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(p, Token.Comma :: rest) => parseMatchCasePatterns(rest, p :: acc)
        case ParseResult.Ok(p, Token.MapsTo :: rest) => ParseResult.Ok(List.reverse(p :: acc), Token.MapsTo :: rest)
        case _ => ParseResult.Err("expected ',' or '↦' after match pattern")
    }

def parsePatternsUntilMapsTo(ts: List[Token], acc: List[Pattern]): ParseResult[List[Pattern]] =
    match ts {
        case Token.MapsTo :: _ => ParseResult.Ok(acc, ts)
        case Token.Comma :: rest => parsePatternsUntilMapsTo(rest, acc)
        case Token.RParen :: _ => ParseResult.Err("lambda requires '↦'")
        case Token.Ident(name) :: rest if not isQualifiedName(name) => parsePatternsUntilMapsTo(rest, Pattern.Var(name) :: acc)
        case _ => ParseResult.Err("lambda parameters must be variables")
    }

def parsePattern(ts: List[Token]): ParseResult[Pattern] =
    match parsePatternParts(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(parts, rest) => patternFromParts(List.reverse(parts), rest)
    }

def parsePatternParts(ts: List[Token], acc: List[HeaderPart]): ParseResult[List[HeaderPart]] =
    match ts {
        case Nil => ParseResult.Ok(acc, Nil)
        case Token.Comma :: _ => finishPatternParts(acc, ts)
        case Token.MapsTo :: _ => finishPatternParts(acc, ts)
        case Token.RParen :: _ => finishPatternParts(acc, ts)
        case Token.RBrace :: _ => finishPatternParts(acc, ts)
        case Token.Dollar :: rest =>
            match readHeaderTerm(rest) {
                case None => ParseResult.Err("expected pattern after '$'")
                case Some((term, rest2)) => parsePatternParts(rest2, HeaderPart.Marked(term) :: acc)
            }
        case _ =>
            match readHeaderTerm(ts) {
                case None => finishPatternParts(acc, ts)
                case Some((term, rest)) => parsePatternParts(rest, HeaderPart.Plain(term) :: acc)
            }
    }

def finishPatternParts(acc: List[HeaderPart], rest: List[Token]): ParseResult[List[HeaderPart]] =
    match acc {
        case Nil => ParseResult.Err("expected pattern")
        case _ => ParseResult.Ok(acc, rest)
    }

def patternFromParts(parts: List[HeaderPart], rest: List[Token]): ParseResult[Pattern] =
    match parts {
        case HeaderPart.Plain(term) :: Nil =>
            match patternFromTerm(term) {
                case None => ParseResult.Err("expected pattern")
                case Some(Pattern.Var(name)) if isQualifiedName(name) => ParseResult.Ok(Pattern.Con(name, Nil), rest)
                case Some(p) => ParseResult.Ok(p, rest)
            }
        case _ =>
            match headerFromParts(parts) {
                case None => ParseResult.Err("constructor pattern requires a name")
                case Some((argTerms, name)) =>
                    match patternsFromHeaderTerms(argTerms, Nil) {
                        case None => ParseResult.Err("invalid constructor pattern")
                        case Some(args) => ParseResult.Ok(Pattern.Con(name, args), rest)
                    }
            }
    }

def patternFromTerm(term: List[Token]): Option[Pattern] =
    match term {
        case Token.Ident(name) :: Nil => Some(Pattern.Var(name))
        case Token.LParen :: rest =>
            match takeBalanced(rest, Nil, 1) {
                case ParseResult.Ok(inner, Nil) =>
                    match parsePattern(List.reverse(inner)) {
                        case ParseResult.Ok(p, Nil) => Some(p)
                        case _ => None
                    }
                case _ => None
            }
        case _ =>
            match parsePattern(term) {
                case ParseResult.Ok(p, Nil) => Some(p)
                case _ => None
            }
    }

def parseTypeUntilExpr(ts: List[Token], acc: List[Token]): ParseResult[TypeExpr] =
    match ts {
        case Nil => ParseResult.Err("expected expression after type")
        case Token.LParen :: rest if List.isEmpty(acc) =>
            match takeBalanced(rest, Nil, 1) {
                case ParseResult.Ok(inner, rest2) if startsExpr(rest2) =>
                    match parseTypeTokens(List.reverse(inner)) {
                        case ParseResult.Ok(t, Nil) => ParseResult.Ok(t, rest2)
                        case _ => ParseResult.Err("let has no type annotation")
                    }
                case _ => ParseResult.Err("let has no type annotation")
            }
        case x :: _ if isExprStarter(x) and List.nonEmpty(acc) =>
            match parseTypeTokens(List.reverse(acc)) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(t, Nil) => ParseResult.Ok(t, ts)
                case _ => ParseResult.Err("could not parse whole type")
            }
        case x :: _ if isExprStarter(x) => ParseResult.Err("let has no type annotation")
        case x :: xs => parseTypeUntilExpr(xs, x :: acc)
    }

def parseTypeUntilCommaOrBrace(ts: List[Token], acc: List[Token]): ParseResult[TypeExpr] =
    parseTypeUntilTopLevel(ts, acc, t -> match t { case Token.Comma => true case Token.RBrace => true case _ => false }, "unterminated type")

def parseTypeUntilCommaOrBraceOrEquals(ts: List[Token], acc: List[Token]): ParseResult[TypeExpr] =
    parseTypeUntilTopLevel(ts, acc, t -> match t { case Token.Comma => true case Token.RBrace => true case Token.Equals => true case _ => false }, "unterminated type")

def parseTypeUntilCommaOrParen(ts: List[Token], acc: List[Token]): ParseResult[TypeExpr] =
    parseTypeUntilTopLevel(ts, acc, t -> match t { case Token.Comma => true case Token.RParen => true case _ => false }, "unterminated parameter type")

def parseTypeUntilEquals(ts: List[Token], acc: List[Token]): ParseResult[TypeExpr] =
    parseTypeUntilTopLevel(ts, acc, t -> match t { case Token.Equals => true case _ => false }, "expected '=' after type")

def parseTypeUntilDot(ts: List[Token], acc: List[Token]): ParseResult[TypeExpr] =
    parseTypeUntilTopLevelOrEnd(ts, acc, t -> match t { case Token.Semicolon => true case _ => false }, "unterminated type")

def parseTypeUntilTopLevel(ts: List[Token], acc: List[Token], stop: Token -> Bool, message: String): ParseResult[TypeExpr] =
    match takeTopLevelUntil(ts, acc, 0, stop, message) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(seen, rest) => finishType(seen, rest)
    }

def parseTypeUntilTopLevelOrEnd(ts: List[Token], acc: List[Token], stop: Token -> Bool, message: String): ParseResult[TypeExpr] =
    match takeTopLevelUntilOrEnd(ts, acc, 0, stop) {
        case ParseResult.Ok(seen, rest) => finishType(seen, rest)
        case ParseResult.Err(_) => ParseResult.Err(message)
    }

def finishType(acc: List[Token], rest: List[Token]): ParseResult[TypeExpr] =
    match parseTypeTokens(List.reverse(acc)) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(t, Nil) => ParseResult.Ok(t, rest)
        case _ => ParseResult.Err("could not parse whole type")
    }

def parseTypeTokens(ts: List[Token]): ParseResult[TypeExpr] =
    parseForallType(ts)

def parseForallType(ts: List[Token]): ParseResult[TypeExpr] =
    match splitTopLevelForall(ts, Nil, 0) {
        case Some((left, right)) =>
            match parseForallParams(left, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(params, Nil) =>
                    match parseForallType(right) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(body, Nil) => ParseResult.Ok(TypeExpr.Forall(params, body), Nil)
                        case _ => ParseResult.Err("unexpected tokens in forall body")
                    }
                case _ => ParseResult.Err("unexpected tokens in forall parameters")
            }
        case None => parseArrowType(ts)
    }

def parseForallParams(ts: List[Token], acc: List[TypeExpr]): ParseResult[List[TypeExpr]] =
    match ts {
        case Nil => ParseResult.Ok(List.reverse(acc), Nil)
        case Token.Comma :: rest => parseForallParams(rest, acc)
        case Token.Ident(name) :: rest => parseForallParams(rest, TypeExpr.Name(name) :: acc)
        case _ => ParseResult.Err("forall parameters must be names")
    }

def parseArrowType(ts: List[Token]): ParseResult[TypeExpr] =
    match splitTopLevelArrow(ts, Nil, 0) {
        case Some((left, right)) =>
            match parseCommaTypes(left) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(args, Nil) =>
                    match parseArrowResult(right) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok((effects, ret), Nil) => ParseResult.Ok(TypeExpr.Arrow(args, effects, ret), Nil)
                        case _ => ParseResult.Err("unexpected tokens in arrow result")
                    }
                case _ => ParseResult.Err("unexpected tokens in arrow domain")
            }
        case None => parseEffectfulValueType(ts)
    }

def parseEffectfulValueType(ts: List[Token]): ParseResult[TypeExpr] =
    match splitTopLevelBang(ts, Nil, 0) {
        case Some((retTokens, effectTokens)) =>
            match parseTypeApplication(retTokens) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(ret, Nil) =>
                    match parseEffectList(effectTokens) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(effects, Nil) => ParseResult.Ok(TypeExpr.Arrow(Nil, effects, ret), Nil)
                        case _ => ParseResult.Err("unexpected tokens after effects")
                    }
                case _ => ParseResult.Err("unexpected tokens before effects")
            }
        case None => parseTypeApplication(ts)
    }

def parseArrowResult(ts: List[Token]): ParseResult[(List[TypeExpr], TypeExpr)] =
    match splitTopLevelBang(ts, Nil, 0) {
        case Some((retTokens, effectTokens)) =>
            match parseArrowType(retTokens) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(ret, Nil) =>
                    match parseEffectList(effectTokens) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(effects, Nil) => ParseResult.Ok((effects, ret), Nil)
                        case _ => ParseResult.Err("unexpected tokens after effects")
                    }
                case _ => ParseResult.Err("unexpected tokens before effects")
            }
        case None => parseArrowResultOld(ts)
    }

def parseArrowResultOld(ts: List[Token]): ParseResult[(List[TypeExpr], TypeExpr)] =
    match ts {
        case Token.LBrace :: rest =>
            match takeEffectTokens(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(effectTokens, rest2) =>
                    match parseArrowType(rest2) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(ret, rest3) =>
                            match parseEffectList(List.reverse(effectTokens)) {
                                case ParseResult.Err(msg) => ParseResult.Err(msg)
                                case ParseResult.Ok(effects, Nil) => ParseResult.Ok((effects, ret), rest3)
                                case _ => ParseResult.Err("unexpected tokens in effects")
                            }
                    }
            }
        case _ =>
            match parseArrowType(ts) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(ret, rest) => ParseResult.Ok((Nil, ret), rest)
            }
    }

def parseEffectList(ts: List[Token]): ParseResult[List[TypeExpr]] =
    parseCommaTypes(ts)

def takeEffectTokens(ts: List[Token], acc: List[Token]): ParseResult[List[Token]] =
    match ts {
        case Token.RBrace :: rest => ParseResult.Ok(acc, rest)
        case x :: rest => takeEffectTokens(rest, x :: acc)
        case Nil => ParseResult.Err("expected '}' after effects")
    }

def parseCommaTypes(ts: List[Token]): ParseResult[List[TypeExpr]] =
    parseCommaTypesLoop(ts, Nil)

def parseCommaTypesLoop(ts: List[Token], acc: List[TypeExpr]): ParseResult[List[TypeExpr]] =
    match splitTopLevelComma(ts, Nil, 0) {
        case Some((left, right)) =>
            match parseTypeApplication(left) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(t, Nil) => parseCommaTypesLoop(right, t :: acc)
                case _ => ParseResult.Err("unexpected tokens before comma")
            }
        case None =>
            match parseTypeApplication(ts) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(t, Nil) => ParseResult.Ok(List.reverse(t :: acc), Nil)
                case _ => ParseResult.Err("unexpected tokens in type")
            }
    }

def parseTypeApplication(ts: List[Token]): ParseResult[TypeExpr] =
    match parseTypeParts(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(parts, rest) => typeFromParts(List.reverse(parts), rest)
    }

def parseTypeParts(ts: List[Token], acc: List[TypePart]): ParseResult[List[TypePart]] =
    match ts {
        case Token.Dollar :: rest =>
            match parseTypeAtom(rest) {
                case ParseResult.Err(_) => ParseResult.Err("expected type after '$'")
                case ParseResult.Ok(t, rest2) => parseTypeParts(rest2, TypePart.Fun(t) :: acc)
            }
        case _ =>
            match parseTypeAtom(ts) {
                case ParseResult.Err(_) =>
                    match acc {
                        case Nil => ParseResult.Err("expected type")
                        case _ => ParseResult.Ok(acc, ts)
                    }
                case ParseResult.Ok(t, rest) => parseTypeParts(rest, TypePart.Arg(t) :: acc)
            }
    }

def parseTypeAtom(ts: List[Token]): ParseResult[TypeExpr] =
    match ts {
        case Token.Ident(s) :: rest => ParseResult.Ok(TypeExpr.Name(s), rest)
        case Token.LBracket :: rest =>
            match parseRecordType(rest) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(t, rest2) => ParseResult.Ok(t, rest2)
            }
        case Token.LParen :: rest =>
            match takeBalanced(rest, Nil, 1) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(inner, rest2) =>
                    match parseParenthesizedType(List.reverse(inner)) {
                        case ParseResult.Err(msg) => ParseResult.Err(msg)
                        case ParseResult.Ok(t, Nil) => ParseResult.Ok(t, rest2)
                        case _ => ParseResult.Err("could not parse parenthesized type")
                    }
            }
        case _ => ParseResult.Err("expected type")
    }

def parseParenthesizedType(ts: List[Token]): ParseResult[TypeExpr] =
    match ts {
        case Token.Ident(_) :: Token.Colon :: rest => parseTypeTokens(rest)
        case _ => parseTypeTokens(ts)
    }

def parseRecordType(ts: List[Token]): ParseResult[TypeExpr] =
    match parseRecordTypeFields(ts, Nil) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok(fields, rest) => ParseResult.Ok(TypeExpr.Record(List.reverse(fields)), rest)
    }

def parseRecordTypeFields(ts: List[Token], acc: List[(String, TypeExpr)]): ParseResult[List[(String, TypeExpr)]] =
    match ts {
        case Token.RBracket :: rest => ParseResult.Ok(acc, rest)
        case Token.Ident(name) :: Token.Colon :: rest =>
            match parseTypeUntilCommaOrBracket(rest, Nil) {
                case ParseResult.Err(msg) => ParseResult.Err(msg)
                case ParseResult.Ok(t, Token.Comma :: rest2) => parseRecordTypeFields(rest2, (name, t) :: acc)
                case ParseResult.Ok(t, Token.RBracket :: rest2) => ParseResult.Ok((name, t) :: acc, rest2)
                case _ => ParseResult.Err("expected ',' or ']' after record field type")
            }
        case _ => ParseResult.Err("expected record field type")
    }

def parseTypeUntilCommaOrBracket(ts: List[Token], acc: List[Token]): ParseResult[TypeExpr] =
    parseTypeUntilTopLevel(ts, acc, t -> match t { case Token.Comma => true case Token.RBracket => true case _ => false }, "unterminated record type")

def typeFromParts(parts: List[TypePart], rest: List[Token]): ParseResult[TypeExpr] =
    match markedType(parts, Nil, None) {
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case ParseResult.Ok((Some(name), args), _) => ParseResult.Ok(typeApplication(name, args), rest)
        case ParseResult.Ok((None, _), _) => defaultType(parts, rest)
    }

def markedType(parts: List[TypePart], args: List[TypeExpr], name: Option[String]): ParseResult[(Option[String], List[TypeExpr])] =
    match parts {
        case Nil => ParseResult.Ok((name, List.reverse(args)), Nil)
        case TypePart.Arg(t) :: rest => markedType(rest, t :: args, name)
        case TypePart.Fun(TypeExpr.Name(n)) :: rest =>
            match name {
                case None => markedType(rest, args, Some(n))
                case Some(_) => ParseResult.Err("type application has multiple '$' operator markers")
            }
        case TypePart.Fun(_) :: _ => ParseResult.Err("type operator must be a name")
    }

def defaultType(parts: List[TypePart], rest: List[Token]): ParseResult[TypeExpr] =
    match typeArgsFromParts(parts) {
        case Nil => ParseResult.Err("expected type")
        case x :: Nil => ParseResult.Ok(x, rest)
        case arg :: TypeExpr.Name(name) :: args => ParseResult.Ok(typeApplication(name, arg :: args), rest)
        case arg :: f :: args => ParseResult.Ok(TypeExpr.Apply("<type>", arg :: f :: args), rest)
    }

def typeArgsFromParts(parts: List[TypePart]): List[TypeExpr] =
    match parts {
        case Nil => Nil
        case TypePart.Arg(t) :: rest => t :: typeArgsFromParts(rest)
        case TypePart.Fun(t) :: rest => t :: typeArgsFromParts(rest)
    }

def typeApplication(name: String, args: List[TypeExpr]): TypeExpr =
    match args {
        case Nil => TypeExpr.Name(name)
        case _ => TypeExpr.Apply(name, args)
    }

def isExprStarter(t: Token): Bool =
    match t {
        case Token.Integer(_) => true
        case Token.Float(_) => true
        case Token.Char(_) => true
        case Token.String(_) => true
        case Token.LParen => true
        case Token.LBracket => true
        case Token.Try => true
        case Token.Func => true
        case Token.Match => true
        case Token.Dollar => true
        case _ => false
    }

def startsExpr(ts: List[Token]): Bool =
    match ts {
        case t :: _ => isExprStarter(t) or isIdentExprStarter(t)
        case Nil => false
    }

def isIdentExprStarter(t: Token): Bool =
    match t {
        case Token.Ident(_) => true
        case _ => false
    }

def ctorFromTokens(ts: List[Token]): Option[Ctor] =
    match headerFromTokens(ts) {
        case None => None
        case Some((args, name)) =>
            match typesFromHeaderTerms(args, Nil) {
                case None => None
                case Some(argTypes) => Some(Ctor.Case(name, argTypes))
            }
    }

def postfixMemberHeader(ts: List[Token]): Option[(List[String], String)] =
    match headerFromTokens(ts) {
        case None => None
        case Some((argTerms, name)) =>
            match namesFromHeaderTerms(argTerms, Nil) {
                case None => None
                case Some(params) => Some((params, name))
            }
    }

def postfixFleshMemberHeader(ts: List[Token]): Option[(List[Pattern], String)] =
    functionHeader(ts)

def functionHeader(ts: List[Token]): Option[(List[Pattern], String)] =
    match headerFromTokens(ts) {
        case None => None
        case Some((argTerms, name)) =>
            match patternsFromHeaderTerms(argTerms, Nil) {
                case None => None
                case Some(params) => Some((params, name))
            }
    }

def headerFromTokens(ts: List[Token]): Option[(List[List[Token]], String)] =
    match headerParts(ts, Nil) {
        case None => None
        case Some(parts) => headerFromParts(parts)
    }

def headerParts(ts: List[Token], acc: List[HeaderPart]): Option[List[HeaderPart]] =
    match ts {
        case Nil => Some(List.reverse(acc))
        case Token.Dollar :: rest =>
            match readHeaderTerm(rest) {
                case None => None
                case Some((term, rest2)) => headerParts(rest2, HeaderPart.Marked(term) :: acc)
            }
        case _ =>
            match readHeaderTerm(ts) {
                case None => None
                case Some((term, rest)) => headerParts(rest, HeaderPart.Plain(term) :: acc)
            }
    }

def readHeaderTerm(ts: List[Token]): Option[(List[Token], List[Token])] =
    match ts {
        case Token.Ident(name) :: rest => Some((Token.Ident(name) :: Nil, rest))
        case Token.LParen :: rest =>
            match takeBalanced(rest, Nil, 1) {
                case ParseResult.Ok(inner, rest2) => Some((parenthesizedTokens(List.reverse(inner)), rest2))
                case _ => None
            }
        case _ => None
    }

def parenthesizedTokens(inner: List[Token]): List[Token] =
    Token.LParen :: appendTokens(inner, Token.RParen :: Nil)

def appendTokens(xs: List[Token], ys: List[Token]): List[Token] =
    match xs {
        case Nil => ys
        case x :: rest => x :: appendTokens(rest, ys)
    }

def headerFromParts(parts: List[HeaderPart]): Option[(List[List[Token]], String)] =
    match markedHeader(parts, Nil, None) {
        case None => None
        case Some((Some(name), args)) => Some((args, name))
        case Some((None, _)) => defaultHeader(parts)
    }

def markedHeader(parts: List[HeaderPart], args: List[List[Token]], name: Option[String]): Option[(Option[String], List[List[Token]])] =
    match parts {
        case Nil => Some((name, List.reverse(args)))
        case HeaderPart.Plain(term) :: rest => markedHeader(rest, term :: args, name)
        case HeaderPart.Marked(term) :: rest =>
            match (name, headerTermName(term)) {
                case (None, Some(n)) => markedHeader(rest, args, Some(n))
                case _ => None
            }
    }

def defaultHeader(parts: List[HeaderPart]): Option[(List[List[Token]], String)] =
    match parts {
        case HeaderPart.Plain(term) :: Nil =>
            match headerTermName(term) {
                case Some(name) => Some((Nil, name))
                case None => None
            }
        case HeaderPart.Plain(arg) :: HeaderPart.Plain(fun) :: rest =>
            match (headerTermName(fun), plainHeaderTerms(rest, Nil)) {
                case (Some(name), Some(after)) => Some((arg :: after, name))
                case _ => None
            }
        case _ => None
    }

def plainHeaderTerms(parts: List[HeaderPart], acc: List[List[Token]]): Option[List[List[Token]]] =
    match parts {
        case Nil => Some(List.reverse(acc))
        case HeaderPart.Plain(term) :: rest => plainHeaderTerms(rest, term :: acc)
        case HeaderPart.Marked(_) :: _ => None
    }

def headerTermName(term: List[Token]): Option[String] =
    match term {
        case Token.Ident(name) :: Nil => Some(name)
        case _ => None
    }

def typeFromHeaderTerms(terms: List[List[Token]]): ParseResult[TypeExpr] =
    match parseTypeTokens(flatHeaderTerms(terms)) {
        case ParseResult.Ok(t, Nil) => ParseResult.Ok(t, Nil)
        case ParseResult.Err(msg) => ParseResult.Err(msg)
        case _ => ParseResult.Err("could not parse flesh type")
    }

def typesFromHeaderTerms(terms: List[List[Token]], acc: List[TypeExpr]): Option[List[TypeExpr]] =
    match terms {
        case Nil => Some(List.reverse(acc))
        case term :: rest =>
            match parseTypeTokens(term) {
                case ParseResult.Ok(t, Nil) => typesFromHeaderTerms(rest, t :: acc)
                case _ => None
            }
    }

def patternsFromHeaderTerms(terms: List[List[Token]], acc: List[Pattern]): Option[List[Pattern]] =
    match terms {
        case Nil => Some(List.reverse(acc))
        case term :: rest =>
            match patternFromTerm(term) {
                case Some(p) => patternsFromHeaderTerms(rest, p :: acc)
                case None => None
            }
    }

def namesFromHeaderTerms(terms: List[List[Token]], acc: List[String]): Option[List[String]] =
    match terms {
        case Nil => Some(List.reverse(acc))
        case term :: rest =>
            match headerTermName(term) {
                case Some(name) => namesFromHeaderTerms(rest, name :: acc)
                case None => None
            }
    }

def flatHeaderTerms(terms: List[List[Token]]): List[Token] =
    match terms {
        case Nil => Nil
        case term :: rest => appendTokens(term, flatHeaderTerms(rest))
    }

def takeUntilEquals(ts: List[Token], acc: List[Token]): ParseResult[List[Token]] =
    takeUntilToken(ts, acc, t -> match t { case Token.Equals => true case _ => false }, "expected '='")

def takeUntilMatch(ts: List[Token], acc: List[Token]): ParseResult[List[Token]] =
    takeUntilToken(ts, acc, t -> match t { case Token.Match => true case _ => false }, "expected match")

def takeUntilColon(ts: List[Token], acc: List[Token]): ParseResult[List[Token]] =
    takeUntilToken(ts, acc, t -> match t { case Token.Colon => true case _ => false }, "expected ':'")

def takeUntilToken(ts: List[Token], acc: List[Token], stop: Token -> Bool, message: String): ParseResult[List[Token]] =
    match ts {
        case Nil => ParseResult.Err(message)
        case x :: _ if stop(x) => ParseResult.Ok(acc, ts)
        case x :: rest => takeUntilToken(rest, x :: acc, stop, message)
    }

def parsePatternUntilColon(ts: List[Token], acc: List[Token]): ParseResult[List[Token]] =
    takeTopLevelUntil(ts, acc, 0, t -> match t { case Token.Colon => true case _ => false }, "expected ':'")

def dropComma(ts: List[Token]): List[Token] =
    match ts {
        case Token.Comma :: rest => rest
        case _ => ts
    }

def dropCommaOrDot(ts: List[Token]): List[Token] =
    match ts {
        case Token.Comma :: rest => rest
        case Token.Semicolon :: rest => rest
        case _ => ts
    }

def dropDot(ts: List[Token]): List[Token] =
    match ts {
        case Token.Semicolon :: rest => rest
        case _ => ts
    }

def lex(source: String): LexResult =
    lexChars(String.toList(source), Nil)

def lexChars(cs: List[Char], acc: List[Token]): LexResult =
    match cs {
        case Nil => LexResult.Ok(List.reverse(acc))
        case c :: rest if isSpace(c) => lexChars(rest, acc)
        case '#' :: rest => lexChars(dropLine(rest), acc)
        case '(' :: rest => lexChars(rest, Token.LParen :: acc)
        case ')' :: rest => lexChars(rest, Token.RParen :: acc)
        case '{' :: rest => lexChars(rest, Token.LBrace :: acc)
        case '}' :: rest => lexChars(rest, Token.RBrace :: acc)
        case '[' :: rest => lexChars(rest, Token.LBracket :: acc)
        case ']' :: rest => lexChars(rest, Token.RBracket :: acc)
        case ':' :: rest => lexChars(rest, Token.Colon :: acc)
        case ',' :: rest => lexChars(rest, Token.Comma :: acc)
        case '↦' :: rest => lexChars(rest, Token.MapsTo :: acc)
        case '!' :: rest => lexChars(rest, Token.Bang :: acc)
        case '=' :: rest => lexChars(rest, Token.Equals :: acc)
        case ';' :: rest => lexChars(rest, Token.Semicolon :: acc)
        case '.' :: '*' :: rest => lexIdent(rest, '*' :: '.' :: Nil, acc)
        case '.' :: rest => lexChars(rest, Token.Dot :: acc)
        case '$' :: rest => lexChars(rest, Token.Dollar :: acc)
        case '\\' :: '\\' :: rest => lexChars(rest, Token.Forall :: acc)
        case '→' :: rest => lexChars(rest, Token.Arrow :: acc)
        case '`' :: rest => lexCharLiteral(rest, acc)
        case '\'' :: rest => lexStringLiteral(rest, Nil, acc)
        case '-' :: d :: rest if isDigit(d) => lexNumber(d :: rest, true, acc)
        case d :: rest if isDigit(d) => lexNumber(d :: rest, false, acc)
        case c :: rest if isNameChar(c) => lexIdent(rest, c :: Nil, acc)
        case c :: _ => LexResult.Err("unexpected character: ${c}")
    }

def lexNumber(cs: List[Char], neg: Bool, acc: List[Token]): LexResult =
    let (digits, rest) = spanDigits(cs, Nil);
    match rest {
        case '.' :: d :: rest2 if isDigit(d) =>
            let (frac, rest3) = spanDigits(d :: rest2, Nil);
            let unsigned = charsToString(List.reverse(digits)) + "." + charsToString(List.reverse(frac));
            let text = if (neg) "-" + unsigned else unsigned;
            lexChars(rest3, Token.Float(text) :: acc)
        case _ =>
            let text = charsToString(List.reverse(digits));
            let value = parseInteger(text);
            let signed = if (neg) -value else value;
            lexChars(rest, Token.Integer(signed) :: acc)
    }

def lexIdent(cs: List[Char], seen: List[Char], acc: List[Token]): LexResult =
    match cs {
        case c :: rest if isNameChar(c) => lexIdent(rest, c :: seen, acc)
        case _ =>
            let word = charsToString(List.reverse(seen));
            lexChars(cs, keywordOrIdent(word) :: acc)
    }

def lexCharLiteral(cs: List[Char], acc: List[Token]): LexResult =
    match cs {
        case '{' :: rest =>
            let (digits, afterDigits) = spanDigits(rest, Nil);
            match afterDigits {
                case '}' :: rest2 => lexChars(rest2, Token.Char("{" + charsToString(List.reverse(digits)) + "}") :: acc)
                case _ => LexResult.Err("expected '}' after unicode character literal")
            }
        case c :: rest => lexChars(rest, Token.Char(charsToString(c :: Nil)) :: acc)
        case Nil => LexResult.Err("unterminated character literal")
    }

def lexStringLiteral(cs: List[Char], seen: List[Char], acc: List[Token]): LexResult =
    match cs {
        case Nil => LexResult.Err("unterminated string literal")
        case '\'' :: rest => lexChars(rest, Token.String(charsToString(List.reverse(seen))) :: acc)
        case '\\' :: '\'' :: rest => lexStringLiteral(rest, '\'' :: seen, acc)
        case '\\' :: '\\' :: rest => lexStringLiteral(rest, '\\' :: seen, acc)
        case c :: rest => lexStringLiteral(rest, c :: seen, acc)
    }

def keywordOrIdent(s: String): Token =
    match s {
        case "let" => Token.Let
        case "bring" => Token.Bring
        case "type" => Token.Type
        case "data" => Token.Data
        case "effect" => Token.Effect
        case "bone" => Token.Bone
        case "flesh" => Token.Flesh
        case "match" => Token.Match
        case "try" => Token.Try
        case "func" => Token.Func
        case _ => Token.Ident(s)
    }

def spanDigits(cs: List[Char], acc: List[Char]): (List[Char], List[Char]) =
    match cs {
        case c :: rest if isDigit(c) => spanDigits(rest, c :: acc)
        case _ => (acc, cs)
    }

def dropLine(cs: List[Char]): List[Char] =
    match cs {
        case Nil => Nil
        case '\n' :: rest => rest
        case _ :: rest => dropLine(rest)
    }

def isSpace(c: Char): Bool =
    c == ' ' or c == '\n' or c == '\r' or c == '\t'

def isDigit(c: Char): Bool =
    c >= '0' and c <= '9'

def isNameChar(c: Char): Bool =
    not isSpace(c) and not isSpecialNameChar(c)

def isSpecialNameChar(c: Char): Bool =
    c == '#' or c == '(' or c == ')' or c == '{' or c == '}' or c == '[' or c == ']' or c == ':' or c == ',' or c == '↦' or c == '!' or c == '=' or c == ';' or c == '.' or c == '$' or c == '\\' or c == '→' or c == '`' or c == '\''

def isQualifiedName(s: String): Bool =
    containsAt(String.toList(s))

def containsAt(cs: List[Char]): Bool =
    match cs {
        case Nil => false
        case '@' :: _ => true
        case _ :: rest => containsAt(rest)
    }

def charsToString(cs: List[Char]): String =
    String.unfold(nextChar, cs)

def nextChar(cs: List[Char]): Option[(Char, List[Char])] =
    match cs {
        case Nil => None
        case c :: rest => Some((c, rest))
    }

def parseInteger(s: String): Int32 =
    List.foldLeft((n, c) -> n * 10 + digitValue(c), 0, String.toList(s))

def digitValue(c: Char): Int32 =
    match c {
        case '0' => 0
        case '1' => 1
        case '2' => 2
        case '3' => 3
        case '4' => 4
        case '5' => 5
        case '6' => 6
        case '7' => 7
        case '8' => 8
        case '9' => 9
        case _ => 0
    }

def splitTopLevelForall(ts: List[Token], acc: List[Token], depth: Int32): Option[(List[Token], List[Token])] =
    splitTopLevel(ts, acc, depth, t -> match t { case Token.Forall => true case _ => false })

def splitTopLevelArrow(ts: List[Token], acc: List[Token], depth: Int32): Option[(List[Token], List[Token])] =
    splitTopLevel(ts, acc, depth, t -> match t { case Token.Arrow => true case _ => false })

def splitTopLevelBang(ts: List[Token], acc: List[Token], depth: Int32): Option[(List[Token], List[Token])] =
    splitTopLevel(ts, acc, depth, t -> match t { case Token.Bang => true case _ => false })

def splitTopLevelColon(ts: List[Token], acc: List[Token], depth: Int32): Option[(List[Token], List[Token])] =
    splitTopLevel(ts, acc, depth, t -> match t { case Token.Colon => true case _ => false })

def splitTopLevelComma(ts: List[Token], acc: List[Token], depth: Int32): Option[(List[Token], List[Token])] =
    splitTopLevel(ts, acc, depth, t -> match t { case Token.Comma => true case _ => false })

def splitTopLevel(ts: List[Token], acc: List[Token], depth: Int32, stop: Token -> Bool): Option[(List[Token], List[Token])] =
    match ts {
        case Nil => None
        case x :: rest if depth == 0 and stop(x) => Some((List.reverse(acc), rest))
        case Token.LParen :: rest => splitTopLevel(rest, Token.LParen :: acc, depth + 1, stop)
        case Token.RParen :: rest => splitTopLevel(rest, Token.RParen :: acc, depth - 1, stop)
        case Token.LBracket :: rest => splitTopLevel(rest, Token.LBracket :: acc, depth + 1, stop)
        case Token.RBracket :: rest => splitTopLevel(rest, Token.RBracket :: acc, depth - 1, stop)
        case x :: rest => splitTopLevel(rest, x :: acc, depth, stop)
    }

def takeTopLevelUntil(ts: List[Token], acc: List[Token], depth: Int32, stop: Token -> Bool, message: String): ParseResult[List[Token]] =
    match takeTopLevelUntilOrEnd(ts, acc, depth, stop) {
        case ParseResult.Ok(_, Nil) => ParseResult.Err(message)
        case other => other
    }

def takeTopLevelUntilOrEnd(ts: List[Token], acc: List[Token], depth: Int32, stop: Token -> Bool): ParseResult[List[Token]] =
    match ts {
        case Nil => ParseResult.Ok(acc, Nil)
        case x :: _ if depth == 0 and stop(x) => ParseResult.Ok(acc, ts)
        case Token.LParen :: rest => takeTopLevelUntilOrEnd(rest, Token.LParen :: acc, depth + 1, stop)
        case Token.RParen :: rest => takeTopLevelUntilOrEnd(rest, Token.RParen :: acc, depth - 1, stop)
        case Token.LBracket :: rest => takeTopLevelUntilOrEnd(rest, Token.LBracket :: acc, depth + 1, stop)
        case Token.RBracket :: rest => takeTopLevelUntilOrEnd(rest, Token.RBracket :: acc, depth - 1, stop)
        case x :: rest => takeTopLevelUntilOrEnd(rest, x :: acc, depth, stop)
    }

def takeBalanced(ts: List[Token], acc: List[Token], depth: Int32): ParseResult[List[Token]] =
    match ts {
        case Nil => ParseResult.Err("unclosed parenthesized type")
        case Token.LParen :: rest => takeBalanced(rest, Token.LParen :: acc, depth + 1)
        case Token.RParen :: rest if depth == 1 => ParseResult.Ok(acc, rest)
        case Token.RParen :: rest => takeBalanced(rest, Token.RParen :: acc, depth - 1)
        case x :: rest => takeBalanced(rest, x :: acc, depth)
    }

def takeHeaderTokens(ts: List[Token], acc: List[Token]): ParseResult[List[Token]] =
    takeTopLevelUntil(ts, acc, 0, t -> match t { case Token.LBrace => true case _ => false }, "expected '{' after flesh header")

def takeCtorTokens(ts: List[Token], acc: List[Token]): ParseResult[List[Token]] =
    match ts {
        case Nil => ParseResult.Err("unterminated data declaration")
        case Token.Comma :: _ => ParseResult.Ok(acc, ts)
        case Token.RBrace :: _ => ParseResult.Ok(acc, ts)
        case x :: xs => takeCtorTokens(xs, x :: acc)
    }
