module Jbini.Parse (
  parse,
)
where

import Control.Applicative (empty, many, some)
import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import Jbini.Syntax
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

data FunctionResult = FunctionResult
  { functionResultType :: TypeExpr
  , functionResultEffects :: [TypeExpr]
  , functionResultNeeds :: [BoneNeed]
  }
  deriving (Eq, Show)

type P a = [Token] -> Either String (a, [Token])

parse :: String -> Either String Program
parse source =
  case M.parse programParser "source" source of
    Left err -> Left (M.errorBundlePretty err)
    Right program -> Right program

programParser :: Lexer Program
programParser = do
  toks <- spaceConsumer *> many tokenParser <* M.eof
  case parseProgram toks of
    Right (program, []) -> pure program
    Right _ -> fail "unexpected tokens after program"
    Left msg -> fail msg

parseProgram :: P Program
parseProgram ts = do
  (ds, rest) <- parseDecls ts
  pure (Program ds, rest)

parseDecls :: P [Decl]
parseDecls [] = pure ([], [])
parseDecls ts@(TRBrace : _) = pure ([], ts)
parseDecls (TSemicolon : rest) = parseDecls rest
parseDecls ts = do
  (newDecls, rest) <- parseDeclGroup ts
  (ds, rest2) <- parseDecls (dropSemicolon rest)
  pure (newDecls ++ ds, rest2)

parseDeclGroup :: P [Decl]
parseDeclGroup = \case
  TIdent "show" : rest -> parseShowMany ShowDecl "show" rest
  TIdent "show-type" : rest -> parseShowMany ShowTypeDecl "show-type" rest
  ts -> do
    (d, rest) <- parseDecl ts
    pure ([d], rest)

parseDecl :: P Decl
parseDecl = \case
  TBring : rest -> parseImport rest
  TGiven : rest -> parseGivenDecl rest
  TLet : rest -> parseLetDecl [] rest
  TType : TIdent name : TEquals : rest -> parseTypeAlias name rest
  TData : rest -> parseData rest
  TEffect : rest -> parseEffect rest
  TIdent "class" : TIdent "foreign" : TLBrace : rest -> parseForeign rest
  TBone : rest -> parseBone [] rest
  TFlesh : rest -> parseFlesh [] rest
  ts -> case parseExpr ts of
    Right (e, rest) -> Right (Let "_" Nothing e, rest)
    Left _ -> Left "expected declaration"

parseGivenDecl :: P Decl
parseGivenDecl ts = do
  (needs, rest) <- parseGivenPrefix (\case TLet -> True; TBone -> True; TFlesh -> True; _ -> False) "expected 'let', 'bone' or 'flesh' after given" ts
  case rest of
    TLet : rest2 -> parseLetDecl needs rest2
    TBone : rest2 -> parseBone needs rest2
    TFlesh : rest2 -> parseFlesh needs rest2
    _ -> Left "expected 'let', 'bone' or 'flesh' after given"

parseGivenLet :: P Decl
parseGivenLet ts = do
  (needs, rest) <- parseGivenPrefix (\case TLet -> True; _ -> False) "expected 'let' after given" ts
  case rest of
    TLet : rest2 -> parseLetDecl needs rest2
    _ -> Left "expected 'let' after given"

parseGivenPrefix :: (Token -> Bool) -> String -> [Token] -> Either String ([BoneNeed], [Token])
parseGivenPrefix stop message ts = do
  (givenTokens, rest) <- takeTopLevelUntil ts stop message
  needs <- parseBoneNeedsWhole givenTokens
  pure (needs, rest)

parseLetDecl :: [BoneNeed] -> P Decl
parseLetDecl needs = \case
  TIdent name : rest -> parseLet needs name rest
  ts@(TLParen : rest) -> case parseTypedArgumentLet needs rest of
    Right parsed -> Right parsed
    Left _ -> case parseLetHeaderDefinitionWithNeeds needs ts of
      Just parsed -> parsed
      Nothing -> Left "expected let binding"
  _ -> Left "expected let binding"

parseImport :: P Decl
parseImport ts = do
  (parts, rest) <- parseImportPath ts
  case rest of
    TSemicolon : rest2 -> pure (Import (intercalate "." parts), rest2)
    _ -> Left "expected ';' after bring"

parseImportPath :: P [String]
parseImportPath (TIdent part : TDot : TIdent next : rest) = do
  (parts, rest2) <- parseImportPath (TIdent next : rest)
  pure (part : parts, rest2)
parseImportPath (TIdent part : rest) = pure ([part], rest)
parseImportPath _ = Left "expected bring path"

parseShowMany :: (String -> Decl) -> String -> P [Decl]
parseShowMany makeDecl word ts = do
  (names, rest) <- parseNameList word ts
  pure (map makeDecl names, rest)

parseNameList :: String -> [Token] -> Either String ([String], [Token])
parseNameList word = go []
 where
  go names = \case
    TIdent name : TComma : rest -> go (name : names) rest
    TIdent name : TSemicolon : rest -> pure (reverse (name : names), rest)
    TIdent _ : _ -> Left ("expected ';' after " ++ word)
    _ -> Left ("expected name after " ++ word)

parseTypeAlias :: String -> P Decl
parseTypeAlias name ts = do
  (t, rest) <- parseTypeUntilSemicolon ts
  pure (TypeAlias name t, rest)

parseLet :: [BoneNeed] -> String -> P Decl
parseLet needs name = \case
  TEquals : rest -> parseUntypedLet needs name [] rest
  TColon : rest -> parseColonLet needs name rest
  TIdent "_" : rest -> do
    (e, rest2) <- parseExpr rest
    pure (Let name (implicitLetAnn needs []) e, rest2)
  ts -> parseLetWithOptionalType needs name ts

parseColonLet :: [BoneNeed] -> String -> P Decl
parseColonLet needs name ts =
  case parseTypeAnnUntilEquals ts of
    Right (ann, TEquals : rest) -> do
      (e, rest2) <- parseExpr rest
      pure (Let name (Just (withGivenNeeds needs ann)) e, rest2)
    _ -> Left "expected '=' after let type"

parseLetWithOptionalType :: [BoneNeed] -> String -> P Decl
parseLetWithOptionalType needs name ts =
  case parseLetHeaderDefinitionWithNeeds needs (TIdent name : ts) of
    Just result -> const result ts
    Nothing -> case parseTypeUntilExpr ts of
      Right (t, exprTokens) -> do
        (e, rest) <- parseExpr exprTokens
        pure (Let name (Just (TypeAnn t needs)) e, rest)
      Left _ -> parseUntypedLet needs name [] ts

parseLetHeaderDefinitionWithNeeds :: [BoneNeed] -> [Token] -> Maybe (Either String (Decl, [Token]))
parseLetHeaderDefinitionWithNeeds needs ts =
  case takeUntilEquals ts of
    Right (parts, TEquals : rest) ->
      let header = parts
       in case splitTopLevelColon header of
            Just (left, right) -> Just $ case functionHeader left of
              Nothing -> Left "expected function header before ':'"
              Just (params, name) -> do
                result <- parseFunctionResultTokens right
                ann <- functionLetAnn needs (replicate (length params) Nothing) result
                parseLetHeaderTypedBody name params ann rest
            Nothing -> case functionHeader header of
              Just (params, name) -> Just (parseLetHeaderBody needs name params rest)
              Nothing -> Nothing
    _ -> Nothing

parseLetHeaderBody :: [BoneNeed] -> String -> [Pattern] -> P Decl
parseLetHeaderBody needs name params ts = do
  (e, rest) <- parseExpr ts
  pure (letWithParams name (replicate (length params) Nothing) needs Nothing params e, rest)

parseLetHeaderTypedBody :: String -> [Pattern] -> TypeAnn -> P Decl
parseLetHeaderTypedBody name params ann ts = do
  (e, rest) <- parseExpr ts
  pure (Let name (Just ann) (lambdaIfParams params e), rest)

parseTypedArgumentLet :: [BoneNeed] -> P Decl
parseTypedArgumentLet givenNeeds ts = do
  (parsed, rest) <- parseTypedLetParams ts
  case (parsed, rest) of
    ((params, argTypes), TIdent name : TColon : rest2) ->
      case parseFunctionResultUntilEquals rest2 of
        Right (result, TEquals : bodyTokens) -> do
          (body, rest3) <- parseExpr bodyTokens
          ann <- functionLetAnn givenNeeds argTypes result
          pure (Let name (Just ann) (lambdaIfParams params body), rest3)
        _ -> Left "expected '=' after let result type"
    ((params, argTypes), TIdent name : TEquals : bodyTokens) -> do
      (body, rest3) <- parseExpr bodyTokens
      pure (letWithParams name argTypes givenNeeds Nothing params body, rest3)
    _ -> Left "expected function name and result type after typed arguments"

parseTypedLetParams :: P ([Pattern], [Maybe TypeExpr])
parseTypedLetParams (TRParen : _) = Left "typed function argument list cannot be empty"
parseTypedLetParams ts = go [] [] ts
 where
  go params tys (TRParen : rest)
    | null params = Left "typed function argument list cannot be empty"
    | otherwise = Right ((reverse params, reverse tys), rest)
  go params tys (TComma : rest) = go params tys rest
  go params tys input = do
    (patternTokens, rest) <- parseTypedParamPatternTokens input
    pat <- parseWhole "could not parse typed let argument pattern" parsePattern patternTokens
    case rest of
      TColon : typeTokens -> do
        (argTy, rest2) <- parseTypeUntilCommaOrParen typeTokens
        go (pat : params) (Just argTy : tys) rest2
      TComma : _ -> go (pat : params) (Nothing : tys) rest
      TRParen : _ -> go (pat : params) (Nothing : tys) rest
      _ -> Left "expected ':', ',' or ')' in typed let argument"

parseTypedParamPatternTokens :: [Token] -> Either String ([Token], [Token])
parseTypedParamPatternTokens ts = takeTopLevelUntil ts (\case TColon -> True; TComma -> True; TRParen -> True; _ -> False) "unterminated typed argument"

functionLetAnn :: [BoneNeed] -> [Maybe TypeExpr] -> FunctionResult -> Either String TypeAnn
functionLetAnn needs [] FunctionResult{functionResultType = result, functionResultEffects = [], functionResultNeeds} = Right (TypeAnn result (needs ++ functionResultNeeds))
functionLetAnn _ [] _ = Left "effect annotation requires function arguments"
functionLetAnn needs argTypes FunctionResult{..} =
  TypeAnn <$> makeArrowType (fillMissingTypes argTypes) functionResultEffects functionResultType <*> pure (needs ++ functionResultNeeds)

implicitLetAnn :: [BoneNeed] -> [Maybe TypeExpr] -> Maybe TypeAnn
implicitLetAnn [] [] = Nothing
implicitLetAnn needs argTypes = Just (TypeAnn ty needs)
 where
  ty = case argTypes of
    [] -> implicitTypeAt 0
    _ -> either (const (implicitTypeAt (length argTypes))) id (makeArrowType (fillMissingTypes argTypes) [] (implicitTypeAt (length argTypes)))

letWithParams :: String -> [Maybe TypeExpr] -> [BoneNeed] -> Maybe TypeAnn -> [Pattern] -> Expr -> Decl
letWithParams name paramTypes needs resultAnn params body =
  Let name ann (lambdaIfParams params body)
 where
  ann = case resultAnn of
    Just result -> Just (withGivenNeeds needs result)
    Nothing -> implicitLetAnn needs paramTypes

fillMissingTypes :: [Maybe TypeExpr] -> [TypeExpr]
fillMissingTypes = zipWith fill [0 :: Int ..]
 where
  fill _ (Just t) = t
  fill n Nothing = implicitTypeAt n

implicitTypeAt :: Int -> TypeExpr
implicitTypeAt n = TypeName ("t" ++ show n)

withGivenNeeds :: [BoneNeed] -> TypeAnn -> TypeAnn
withGivenNeeds needs (TypeAnn t annNeeds) = TypeAnn t (needs ++ annNeeds)

parseUntypedLet :: [BoneNeed] -> String -> [Maybe TypeExpr] -> P Decl
parseUntypedLet needs name argTypes ts = do
  (e, rest) <- parseExpr ts
  pure (Let name (implicitLetAnn needs argTypes) e, rest)

parseData :: P Decl
parseData ts = do
  (params, name, body) <- parseParamHeaderBody "data" ts
  (ctors, rest) <- parseCtors body
  pure (DataDecl params name ctors, rest)

parseEffect :: P Decl
parseEffect ts = do
  (params, name, body) <- parseParamHeaderBody "effect" ts
  (ops, rest) <- parseEffectOps body
  pure (EffectDecl params name ops, rest)

parseParamHeaderBody :: String -> [Token] -> Either String ([String], String, [Token])
parseParamHeaderBody kind ts = do
  (header, body) <- parseHeaderBody kind ts
  (params, name) <- parseParamHeader kind header
  pure (params, name, body)

parseHeaderBody :: String -> [Token] -> Either String ([Token], [Token])
parseHeaderBody kind ts = do
  (header, rest) <- takeHeaderTokens kind ts
  case rest of
    TLBrace : body -> Right (header, body)
    _ -> Left ("expected '{' after " ++ kind ++ " header")

parseParamHeader :: String -> [Token] -> Either String ([String], String)
parseParamHeader kind header = do
  (paramTerms, name) <- maybeToEither (kind ++ " declaration requires a name") (headerFromTokens header)
  params <- maybeToEither (kind ++ " parameters must be names") (namesFromHeaderTerms paramTerms)
  pure (params, name)

parseEffectOps :: P [EffectOp]
parseEffectOps = parseCommaListUntil isRightBrace parseEffectOp

parseEffectOp :: P EffectOp
parseEffectOp ts =
  case takeUntilColon ts of
    Right (parts, TColon : rest) -> case boneMemberHeader parts of
      Nothing -> Left "expected effect operation name"
      Just (argTypes, name) -> do
        (result, rest2) <- parseFunctionResultUntilCommaOrBrace rest
        ann <- functionLetAnn [] (map Just argTypes) result
        case ann of
          TypeAnn t _ -> pure (EffectOp name t, rest2)
    _ -> Left "expected effect operation"

parseForeign :: P Decl
parseForeign ts = do
  (members, rest) <- parseForeignMembers ts
  pure (ForeignDecl members, rest)

parseForeignMembers :: P [ForeignMember]
parseForeignMembers (TRBrace : rest) = Right ([], rest)
parseForeignMembers (TSemicolon : rest) = parseForeignMembers rest
parseForeignMembers ts = do
  (member, rest) <- parseForeignMember ts
  (members, rest2) <- parseForeignMembers (dropSemicolon rest)
  pure (member : members, rest2)

parseForeignMember :: P ForeignMember
parseForeignMember ts =
  case takeUntilColon ts of
    Right (parts, TColon : rest) -> case boneMemberHeader parts of
      Nothing -> Left "expected foreign function name"
      Just (argTypes, name) -> do
        ((result, rest2), _) <- parseBoneMemberResultType rest
        ann <- functionLetAnn [] (map Just argTypes) result
        pure (ForeignMember name ann, rest2)
    _ -> Left "expected foreign function"

parseBone :: [BoneNeed] -> P Decl
parseBone needs ts = do
  (header, body) <- parseHeaderBody "bone" ts
  (params, name) <- parseParamHeader "bone" header
  (members, rest) <- parseBoneMembers body
  pure (BoneDecl params name needs members, rest)

parseBoneNeeds :: P [BoneNeed]
parseBoneNeeds ts = go [] ts
 where
  go acc [] = Right (reverse acc, [])
  go acc (TComma : rest) = go acc rest
  go acc input = do
    (parts, rest) <- takeTopLevelUntilOrEnd input (\case TComma -> True; _ -> False)
    (need, _) <- parseBoneNeed parts
    go (need : acc) (dropComma rest)

parseBoneNeedsWhole :: [Token] -> Either String [BoneNeed]
parseBoneNeedsWhole [] = Right []
parseBoneNeedsWhole tokens = parseWhole "unexpected tokens after given" parseBoneNeeds tokens

parseBoneNeed :: P BoneNeed
parseBoneNeed [] = Left "empty given"
parseBoneNeed ts = case headerFromTokens ts of
  Nothing -> Left "invalid given"
  Just (argTerms, name) -> case typesFromHeaderTerms argTerms of
    Nothing -> Left "given arguments must be types"
    Just args -> Right (BoneNeed args name, [])

parseFlesh :: [BoneNeed] -> P Decl
parseFlesh needs ts = do
  (header, body) <- parseHeaderBody "flesh" ts
  (tyTerms, boneName) <- maybeToEither "flesh declaration requires a bone name" (headerFromTokens header)
  (ty, _) <- typeFromHeaderTerms tyTerms
  (ds, rest) <- parseFleshMembers body
  case rest of
    TRBrace : rest2 -> pure (FleshDecl ty boneName needs ds, rest2)
    _ -> Left "expected '}' after flesh body"

parseFleshMembers :: P [Decl]
parseFleshMembers (TRBrace : rest) = Right ([], TRBrace : rest)
parseFleshMembers (TSemicolon : rest) = parseFleshMembers rest
parseFleshMembers ts@(TGiven : _) = parseFleshLetMember ts
parseFleshMembers ts@(TLet : _) = parseFleshLetMember ts
parseFleshMembers _ = Left "expected flesh member let"

parseFleshLetMember :: P [Decl]
parseFleshLetMember ts = do
  (d, rest) <- parseFleshLetDecl ts
  (ds, rest2) <- parseFleshMembers rest
  pure (d : ds, rest2)

parseFleshLetDecl :: P Decl
parseFleshLetDecl = \case
  TGiven : rest -> parseGivenLet rest
  TLet : rest -> parseLetDecl [] rest
  _ -> Left "expected flesh member let"

lambdaIfParams :: [Pattern] -> Expr -> Expr
lambdaIfParams [] expr = expr
lambdaIfParams (p : ps) expr = anonymousMatch (p :| ps) expr

anonymousMatch :: NonEmpty Pattern -> Expr -> Expr
anonymousMatch params expr = EMatch [] (MatchCase params expr :| [])

parseCtors :: P [Ctor]
parseCtors = parseCommaListUntil isRightBrace parseCtor

parseCtor :: P Ctor
parseCtor ts = do
  (parts, rest) <- takeCtorTokens ts
  c <- maybe (Left ("invalid constructor '" ++ showTokens parts ++ "' before " ++ showTokenHead rest)) Right (ctorFromTokens parts)
  pure (c, rest)

showTokens :: [Token] -> String
showTokens = unwords . map showToken

showToken :: Token -> String
showToken = \case
  TIdent name -> name
  TLParen -> "("
  TRParen -> ")"
  _ -> "?"

showTokenHead :: [Token] -> String
showTokenHead = \case
  [] -> "end of input"
  TComma : _ -> "','"
  TRBrace : _ -> "'}'"
  TIdent name : _ -> "'" ++ name ++ "'"
  _ -> "token"

parseBoneMembers :: P [BoneMember]
parseBoneMembers (TRBrace : rest) = Right ([], rest)
parseBoneMembers (TSemicolon : rest) = parseBoneMembers rest
parseBoneMembers ts@(TGiven : _) = parseBoneDefaultLet ts
parseBoneMembers ts@(TLet : _) = parseBoneDefaultLet ts
parseBoneMembers ts@(TIdent _ : _) = parsePostfixBoneSignature ts
parseBoneMembers ts@(TLParen : _) = parsePostfixBoneSignature ts
parseBoneMembers _ = Left "expected bone member"

parsePostfixBoneSignature :: P [BoneMember]
parsePostfixBoneSignature ts =
  case takeUntilColon ts of
    Right (parts, TColon : rest) -> case boneMemberHeader parts of
      Nothing -> Left "expected bone member name"
      Just (argTypes, name) -> do
        ((result, rest2), _) <- parseBoneMemberResultType rest
        (members, rest3) <- parseBoneMembers rest2
        ann <- functionLetAnn [] (map Just argTypes) result
        pure (BoneSpec name ann : members, rest3)
    _ -> Left "expected ':' in bone member"

boneMemberHeader :: [Token] -> Maybe ([TypeExpr], String)
boneMemberHeader ts = do
  (argTerms, name) <- headerFromTokens ts
  argTypes <- typesFromHeaderTerms argTerms
  pure (argTypes, name)

parseBoneMemberResultType :: P (FunctionResult, [Token])
parseBoneMemberResultType ts =
  case parseFunctionResultUntilSemicolonOrBraceOrEquals ts of
    Right (_, TEquals : _) -> Left "bone default must start with let"
    Right (t, rest) -> Right ((t, rest), [])
    Left msg -> Left msg

parseBoneDefaultLet :: P [BoneMember]
parseBoneDefaultLet ts = do
  d <- case ts of
    TGiven : rest -> parseGivenLet rest
    TLet : rest -> parseLetDecl [] rest
    _ -> Left "expected bone default"
  case d of
    (Let name ann expr, rest) -> do
      (members, rest2) <- parseBoneMembers rest
      pure (BoneDefault name ann expr : members, rest2)
    _ -> Left "bone default must be a let"

parseExpr :: P Expr
parseExpr = parseDollarExpr

parseDollarExpr :: P Expr
parseDollarExpr ts = do
  (headExpr, rest) <- parsePostfixExpr ts
  parseDollarTail headExpr rest

parseDollarTail :: Expr -> P Expr
parseDollarTail value = \case
  TDollar : rest -> do
    (terms, rest2) <- parseDollarTerms rest
    case terms of
      fn : args -> parseDollarTail (applyArgs fn (value : args)) rest2
      [] -> Left "expected expression after '$'"
  rest -> Right (value, rest)

parseDollarTerms :: P [Expr]
parseDollarTerms = go []
 where
  go acc ts = case ts of
    [] -> finish acc ts
    TDollar : _ -> finish acc ts
    t : _ | exprStop t -> finish acc ts
    _ -> case parseExprAtom ts of
      Right (arg, rest) -> go (arg : acc) rest
      Left _ -> finish acc ts
  finish acc rest = case reverse acc of
    [] -> Left "expected expression after '$'"
    terms -> Right (terms, rest)

parsePostfixExpr :: P Expr
parsePostfixExpr ts = do
  (parts, rest) <- parseExprParts ts
  applicationFromParts parts rest

applyArgs :: Expr -> [Expr] -> Expr
applyArgs = foldl' (\expr arg -> EApply expr (arg :| []))

applicationFromParts :: [Expr] -> [Token] -> Either String (Expr, [Token])
applicationFromParts = defaultApplication

defaultApplication :: [Expr] -> [Token] -> Either String (Expr, [Token])
defaultApplication parts rest = case parts of
  [] -> Left "expected expression"
  [x] -> Right (x, rest)
  arg : f : args -> Right (applyArgs f (arg : args), rest)

parseExprParts :: P [Expr]
parseExprParts = go []
 where
  go acc ts = case ts of
    [] -> Right (reverse acc, [])
    TLBrace : _ | not (null acc) -> Right (reverse acc, ts)
    t : _ | exprStop t -> Right (reverse acc, ts)
    TDollar : _ -> Right (reverse acc, ts)
    _ -> case parseExprAtom ts of
      Right (e, rest) -> go (e : acc) rest
      Left _
        | null acc -> Left "expected expression atom"
        | otherwise -> Right (reverse acc, ts)

exprStop :: Token -> Bool
exprStop = \case
  TComma -> True
  TDot -> True
  TSemicolon -> True
  TMapsTo -> True
  TRParen -> True
  TRBrace -> True
  TRBracket -> True
  TLet -> True
  TGiven -> True
  TBring -> True
  TType -> True
  TData -> True
  TEffect -> True
  TBone -> True
  TFlesh -> True
  _ -> False

parseExprAtom :: P Expr
parseExprAtom = \case
  TInteger i : rest -> Right (EInteger i, rest)
  TFloat s : rest -> Right (EFloat s, rest)
  TChar c : rest -> Right (EChar c, rest)
  TString s : rest -> Right (EString s, rest)
  TIdent s : rest -> Right (EVar s, rest)
  TTry : rest -> parseTry rest
  TMatch : rest -> parseMatchExpr rest
  TLBrace : rest -> parseAnonymousMatchExpr rest
  TLParen : rest -> parseParen rest
  TLBracket : rest -> parseRecordExpr rest
  _ -> Left "expected expression atom"

parseAnonymousMatchExpr :: P Expr
parseAnonymousMatchExpr ts = do
  (cs, rest) <- parseMatchCases ts
  pure (EMatch [] cs, rest)

parseMatchExpr :: P Expr
parseMatchExpr (TLBrace : _) = Left "match requires a scrutinee; use '{ ... }' for a function"
parseMatchExpr ts = parseMatchScrutinees [] ts

parseMatchScrutinees :: [Expr] -> P Expr
parseMatchScrutinees acc ts = do
  (scrutinee, rest) <- parseExpr ts
  case rest of
    TComma : rest2 -> parseMatchScrutinees (scrutinee : acc) rest2
    TLBrace : rest2 -> do
      (cs, rest3) <- parseMatchCases rest2
      pure (EMatch (reverse (scrutinee : acc)) cs, rest3)
    _ -> Left "expected match scrutinee or '{'"

parseParen :: P Expr
parseParen (TRParen : rest) = Right (EVar "null", rest)
parseParen ts@(TLet : _) = parseBlock ts
parseParen ts@(TGiven : _) = parseBlock ts
parseParen ts = do
  (e, rest) <- parseExpr ts
  case rest of
    TRParen : rest2 -> Right (e, rest2)
    _ -> Left "expected ')' after expression"

parseTry :: P Expr
parseTry ts = do
  (body, rest) <- parseExpr ts
  case rest of
    TLBrace : rest2 -> do
      ((returnCase, cases), rest3) <- parseHandlerCases rest2
      pure (ETry body returnCase cases, rest3)
    _ -> pure (body, rest)

data HandlerItem = HandlerReturnItem ReturnCase | HandlerCaseItem HandlerCase

parseHandlerCases :: P (Maybe ReturnCase, [HandlerCase])
parseHandlerCases ts = do
  (items, rest) <- parseCommaListUntil isRightBrace parseHandlerItem ts
  (returnCase, cases) <- foldM collect (Nothing, []) items
  pure ((returnCase, reverse cases), rest)
 where
  collect (Nothing, cases) (HandlerReturnItem returnCase) = Right (Just returnCase, cases)
  collect (Just _, _) (HandlerReturnItem _) = Left "handler has more than one return case"
  collect (returnCase, cases) (HandlerCaseItem handlerCase) = Right (returnCase, handlerCase : cases)

parseHandlerItem :: P HandlerItem
parseHandlerItem ts = do
  (header, rest) <- takeTopLevelUntil ts (\case TMapsTo -> True; _ -> False) "expected '|' in handler case"
  case rest of
    TMapsTo : bodyTokens -> case returnCaseHeader header of
      Just returnPattern -> do
        (body, rest2) <- parseExpr bodyTokens
        pure (HandlerReturnItem (ReturnCase returnPattern body), rest2)
      Nothing -> case handlerCaseHeader header of
        Nothing -> Left "expected handler case"
        Just (name, params) -> do
          (body, rest2) <- parseExpr bodyTokens
          pure (HandlerCaseItem (HandlerCase name params body), rest2)
    _ -> Left "expected handler case"

handlerCaseHeader :: [Token] -> Maybe (String, [Pattern])
handlerCaseHeader ts = do
  (argTerms, name) <- headerFromTokens ts
  params <- patternsFromHeaderTerms argTerms
  pure (name, params)

returnCaseHeader :: [Token] -> Maybe Pattern
returnCaseHeader (TIdent "return" : rest) = case parsePattern rest of
  Right (pattern, []) -> Just pattern
  _ -> Nothing
returnCaseHeader _ = Nothing

parseBlock :: P Expr
parseBlock ts = do
  (ds, rest) <- parseBlockLets ts
  (e, rest2) <- parseExpr rest
  case rest2 of
    TRParen : rest3 -> pure (EBlock ds e, rest3)
    TSemicolon : TRParen : rest3 -> pure (EBlock ds e, rest3)
    _ -> Left "expected ')' after block"

parseBlockLets :: P [Decl]
parseBlockLets ts@(TLet : _) = do
  (d, rest) <- parseDecl ts
  case rest of
    TSemicolon : rest2 -> do
      (ds, rest3) <- parseBlockLets rest2
      pure (d : ds, rest3)
    _ -> Left "expected ';' after block let"
parseBlockLets ts@(TGiven : _) = do
  (d, rest) <- parseDecl ts
  case rest of
    TSemicolon : rest2 -> do
      (ds, rest3) <- parseBlockLets rest2
      pure (d : ds, rest3)
    _ -> Left "expected ';' after block let"
parseBlockLets ts = Right ([], ts)

parseRecordExpr :: P Expr
parseRecordExpr (TEquals : rest) = parseRecordUpdate rest
parseRecordExpr ts = do
  (fields, rest) <- parseRecordExprFields ts
  pure (ERecord fields, rest)

parseRecordUpdate :: P Expr
parseRecordUpdate ts = do
  (base, rest) <- parseExpr ts
  case rest of
    TComma : rest2 -> do
      (updates, rest3) <- parseRecordUpdates rest2
      pure (EUpdate base updates, rest3)
    TRBracket : rest2 -> pure (EUpdate base [], rest2)
    _ -> Left "expected ',' or ']' after record update base"

parseRecordUpdates :: P [RecordUpdate]
parseRecordUpdates = parseCommaListUntil isRightBracket parseRecordUpdateItem

parseRecordUpdateItem :: P RecordUpdate
parseRecordUpdateItem (TIdent "-" : TIdent name : rest) = Right (RecordRemove name, rest)
parseRecordUpdateItem (TIdent name : TEquals : rest) = do
  (e, rest2) <- parseExpr rest
  pure (RecordSet name e, rest2)
parseRecordUpdateItem _ = Left "expected record update"

parseRecordExprFields :: P [(String, Expr)]
parseRecordExprFields = parseCommaListUntil isRightBracket parseRecordExprField

parseRecordExprField :: P (String, Expr)
parseRecordExprField (TIdent name : TEquals : rest) = do
  (e, rest2) <- parseExpr rest
  pure ((name, e), rest2)
parseRecordExprField _ = Left "expected record field"

parseMatchCases :: P (NonEmpty MatchCase)
parseMatchCases ts = do
  (cases, rest) <- parseCommaListUntil isRightBrace parseMatchCase ts
  case NE.nonEmpty cases of
    Just nonEmptyCases -> pure (nonEmptyCases, rest)
    Nothing -> Left "match requires at least one case"

parseMatchCase :: P MatchCase
parseMatchCase ts = do
  (ps, rest) <- parseMatchCasePatterns ts
  case rest of
    TMapsTo : bodyTokens -> do
      (e, rest2) <- parseExpr bodyTokens
      pure (MatchCase ps e, rest2)
    _ -> Left "expected '|' in match case"

parseMatchCasePatterns :: P (NonEmpty Pattern)
parseMatchCasePatterns ts = do
  (firstPattern, rest) <- parsePattern ts
  go (firstPattern :| []) rest
 where
  go acc rest = case rest of
    TComma : rest2 -> do
      (p, rest3) <- parsePattern rest2
      go (acc <> (p :| [])) rest3
    TMapsTo : _ -> Right (acc, rest)
    _ -> Left "expected ',' or '|' after match pattern"

parsePattern :: P Pattern
parsePattern ts = do
  (parts, rest) <- parsePatternParts ts
  patternFromParts parts rest

parsePatternParts :: P [[Token]]
parsePatternParts = go []
 where
  finish acc rest
    | null acc = Left "expected pattern"
    | otherwise = Right (reverse acc, rest)
  go acc ts = case ts of
    [] -> Right (reverse acc, [])
    t : _ | patternStop t -> finish acc ts
    _ -> case readHeaderTerm ts of
      Nothing -> finish acc ts
      Just (term, rest) -> go (term : acc) rest

patternStop :: Token -> Bool
patternStop = \case
  TComma -> True
  TMapsTo -> True
  TRParen -> True
  TRBrace -> True
  _ -> False

patternFromParts :: [[Token]] -> [Token] -> Either String (Pattern, [Token])
patternFromParts [term] rest = case patternFromTerm term of
  Just (PVar name) | isQualifiedName name -> Right (PCon name [], rest)
  Just p -> Right (p, rest)
  Nothing -> Left "expected pattern"
patternFromParts parts rest = case headerFromParts parts of
  Nothing -> Left "constructor pattern requires a name"
  Just (argTerms, name) -> case patternsFromHeaderTerms argTerms of
    Nothing -> Left "invalid constructor pattern"
    Just args -> Right (PCon name args, rest)

patternFromTerm :: [Token] -> Maybe Pattern
patternFromTerm [TIdent name] = Just (PVar name)
patternFromTerm (TLParen : rest) = case takeBalanced rest of
  Right (inner, []) -> parseWholeMaybe parsePattern inner
  _ -> Nothing
patternFromTerm term = parseWholeMaybe parsePattern term

parseTypeUntilExpr :: P TypeExpr
parseTypeUntilExpr = go []
 where
  go _ [] = Left "expected expression after type"
  go [] (TLParen : rest) = case takeBalanced rest of
    Right (inner, rest2) | startsExpr rest2 -> (\t -> (t, rest2)) <$> parseWhole "let has no type annotation" parseTypeTokens inner
    _ -> Left "let has no type annotation"
  go acc ts@(x : _)
    | isExprStarter x && not (null acc) = (\t -> (t, ts)) <$> parseWhole "could not parse whole type" parseTypeTokens (reverse acc)
    | isExprStarter x = Left "let has no type annotation"
  go acc (x : xs) = go (x : acc) xs

parseTypeUntilCommaOrParen, parseTypeUntilSemicolon :: P TypeExpr
parseTypeUntilCommaOrParen ts = parseTypeUntilTopLevel ts (\case TComma -> True; TRParen -> True; _ -> False) "unterminated parameter type"
parseTypeUntilSemicolon ts = parseTypeUntilTopLevelOrEnd ts (\case TSemicolon -> True; _ -> False) "unterminated type"

parseTypeAnnUntilEquals :: P TypeAnn
parseTypeAnnUntilEquals ts = parseTypeAnnUntilTopLevel ts (\case TEquals -> True; _ -> False) "expected '=' after type"

parseTypeAnnUntilTopLevel :: [Token] -> (Token -> Bool) -> String -> Either String (TypeAnn, [Token])
parseTypeAnnUntilTopLevel ts stop message = do
  (seen, rest) <- takeTopLevelUntil ts stop message
  ann <- parseTypeAnnTokens seen
  pure (ann, rest)

parseTypeAnnTokens :: [Token] -> Either String TypeAnn
parseTypeAnnTokens tokens =
  TypeAnn <$> parseWhole "could not parse whole type" parseTypeTokens tokens <*> pure []

parseFunctionResultUntilCommaOrBrace, parseFunctionResultUntilSemicolonOrBraceOrEquals, parseFunctionResultUntilEquals :: P FunctionResult
parseFunctionResultUntilCommaOrBrace ts = parseFunctionResultUntilTopLevel ts (\case TComma -> True; TRBrace -> True; _ -> False) "unterminated type"
parseFunctionResultUntilSemicolonOrBraceOrEquals ts = parseFunctionResultUntilTopLevel ts (\case TRBrace -> True; TEquals -> True; TSemicolon -> True; _ -> False) "unterminated type"
parseFunctionResultUntilEquals ts = parseFunctionResultUntilTopLevel ts (\case TEquals -> True; _ -> False) "expected '=' after type"

parseTypeUntilTopLevel :: [Token] -> (Token -> Bool) -> String -> Either String (TypeExpr, [Token])
parseTypeUntilTopLevel ts stop message = do
  (seen, rest) <- takeTopLevelUntil ts stop message
  finishType seen rest

parseTypeUntilTopLevelOrEnd :: [Token] -> (Token -> Bool) -> String -> Either String (TypeExpr, [Token])
parseTypeUntilTopLevelOrEnd ts stop message =
  case takeTopLevelUntilOrEnd ts stop of
    Right (seen, rest) -> finishType seen rest
    Left _ -> Left message

finishType :: [Token] -> [Token] -> Either String (TypeExpr, [Token])
finishType tokens rest = (\t -> (t, rest)) <$> parseWhole "could not parse whole type" parseTypeTokens tokens

parseFunctionResultUntilTopLevel :: [Token] -> (Token -> Bool) -> String -> Either String (FunctionResult, [Token])
parseFunctionResultUntilTopLevel ts stop message = do
  (seen, rest) <- takeTopLevelUntil ts stop message
  finishFunctionResult seen rest

parseFunctionResultTokens :: [Token] -> Either String FunctionResult
parseFunctionResultTokens tokens = fst <$> finishFunctionResult tokens []

finishFunctionResult :: [Token] -> [Token] -> Either String (FunctionResult, [Token])
finishFunctionResult tokens rest =
  case parseTypeTokens tokens of
    Right (t, []) -> Right (FunctionResult t [] [], rest)
    _ -> case splitTopLevelBang tokens of
      Nothing -> Left "could not parse whole function result type"
      Just (retTokens, effectTokens) -> do
        ret <- parseWhole "unexpected tokens before effects" parseTypeTokens retTokens
        effects <- parseWhole "unexpected tokens after effects" parseEffectList effectTokens
        Right (FunctionResult ret effects [], rest)

parseTypeTokens :: P TypeExpr
parseTypeTokens = parseForallType

parseForallType :: P TypeExpr
parseForallType ts = case splitTopLevelForall ts of
  Just (left, right) -> do
    params <- parseWhole "unexpected tokens in forall parameters" parseForallParams left
    body <- parseWhole "unexpected tokens in forall body" parseForallType right
    pure (TypeForall params body, [])
  Nothing -> parseArrowType ts

parseForallParams :: P [TypeExpr]
parseForallParams = go []
 where
  go acc [] = Right (reverse acc, [])
  go acc (TComma : rest) = go acc rest
  go acc (TIdent name : rest) = go (TypeName name : acc) rest
  go _ _ = Left "forall parameters must be names"

parseArrowType :: P TypeExpr
parseArrowType ts = case splitTopLevelArrow ts of
  Just (left, right) -> do
    args <- parseWhole "unexpected tokens in arrow domain" parseCommaTypes left
    (effects, ret) <- parseWhole "unexpected tokens in arrow result" parseArrowResult right
    t <- makeArrowType args effects ret
    pure (t, [])
  Nothing -> case splitTopLevelBang ts of
    Just _ -> Left "effects are only allowed on function types"
    Nothing -> parseTypeApplication ts

parseArrowResult :: P ([TypeExpr], TypeExpr)
parseArrowResult ts = case splitTopLevelArrow ts of
  Just _ -> do
    ret <- parseWhole "unexpected tokens in arrow result" parseArrowType ts
    pure (([], ret), [])
  Nothing -> parseArrowResultLeaf ts

parseArrowResultLeaf :: P ([TypeExpr], TypeExpr)
parseArrowResultLeaf ts = case splitTopLevelBang ts of
  Just (retTokens, effectTokens) -> do
    ret <- parseWhole "unexpected tokens before effects" parseArrowType retTokens
    effects <- parseWhole "unexpected tokens after effects" parseEffectList effectTokens
    pure ((effects, ret), [])
  Nothing -> parseArrowResultOld ts

makeArrowType :: [TypeExpr] -> [TypeExpr] -> TypeExpr -> Either String TypeExpr
makeArrowType args effects ret = do
  domain <- maybe (Left "function type requires at least one argument") Right (NE.nonEmpty args)
  pure $ case (effects, ret) of
    ([], TypeArrow more moreEffects finalRet) -> TypeArrow (domain <> more) moreEffects finalRet
    _ -> TypeArrow domain effects ret

parseArrowResultOld :: P ([TypeExpr], TypeExpr)
parseArrowResultOld (TLBrace : rest) = do
  (effectTokens, rest2) <- takeEffectTokens rest
  (ret, rest3) <- parseArrowType rest2
  effects <- parseWhole "unexpected tokens in effects" parseEffectList effectTokens
  pure ((effects, ret), rest3)
parseArrowResultOld ts = do
  (ret, rest) <- parseArrowType ts
  pure (([], ret), rest)

parseEffectList :: P [TypeExpr]
parseEffectList = parseCommaTypes

takeEffectTokens :: P [Token]
takeEffectTokens = go []
 where
  go acc (TRBrace : rest) = Right (reverse acc, rest)
  go acc (x : rest) = go (x : acc) rest
  go _ [] = Left "expected '}' after effects"

parseCommaTypes :: P [TypeExpr]
parseCommaTypes = go []
 where
  go acc ts = case splitTopLevelComma ts of
    Just (left, right) -> do
      t <- parseWhole "unexpected tokens before comma" parseTypeApplication left
      go (t : acc) right
    Nothing -> do
      t <- parseWhole "unexpected tokens in type" parseTypeApplication ts
      Right (reverse (t : acc), [])

parseTypeApplication :: P TypeExpr
parseTypeApplication ts = do
  (parts, rest) <- parseTypeParts ts
  typeFromParts parts rest

parseTypeParts :: P [TypeExpr]
parseTypeParts = go []
 where
  go acc ts = case parseTypeAtom ts of
    Right (t, rest) -> go (t : acc) rest
    Left _
      | null acc -> Left "expected type"
      | otherwise -> Right (reverse acc, ts)

parseTypeAtom :: P TypeExpr
parseTypeAtom = \case
  TIdent s : rest -> Right (TypeName s, rest)
  TLBracket : rest -> parseRecordType rest
  TLParen : rest -> do
    (inner, rest2) <- takeBalanced rest
    t <- parseWhole "could not parse parenthesized type" parseParenthesizedType inner
    pure (t, rest2)
  _ -> Left "expected type"

parseParenthesizedType :: P TypeExpr
parseParenthesizedType (TIdent _ : TColon : rest) = parseTypeTokens rest
parseParenthesizedType ts = parseTypeTokens ts

parseRecordType :: P TypeExpr
parseRecordType ts = do
  (fields, rest) <- parseRecordTypeFields ts
  pure (TypeRecord fields, rest)

parseRecordTypeFields :: P [(String, TypeExpr)]
parseRecordTypeFields = parseCommaListUntil isRightBracket parseRecordTypeField

parseRecordTypeField :: P (String, TypeExpr)
parseRecordTypeField (TIdent name : TColon : rest) = do
  (t, rest2) <- parseTypeUntilCommaOrBracket rest
  pure ((name, t), rest2)
parseRecordTypeField _ = Left "expected record field type"

parseTypeUntilCommaOrBracket :: P TypeExpr
parseTypeUntilCommaOrBracket ts = parseTypeUntilTopLevel ts (\case TComma -> True; TRBracket -> True; _ -> False) "unterminated record type"

typeFromParts :: [TypeExpr] -> [Token] -> Either String (TypeExpr, [Token])
typeFromParts = defaultType

defaultType :: [TypeExpr] -> [Token] -> Either String (TypeExpr, [Token])
defaultType parts rest = case parts of
  [] -> Left "expected type"
  [x] -> Right (x, rest)
  arg : TypeName name : args -> Right (typeApplication name (arg : args), rest)
  arg : f : args -> Right (TypeApply "<type>" (arg : f : args), rest)

typeApplication :: String -> [TypeExpr] -> TypeExpr
typeApplication name [] = TypeName name
typeApplication name args = TypeApply name args

isExprStarter :: Token -> Bool
isExprStarter = \case
  TInteger _ -> True
  TFloat _ -> True
  TChar _ -> True
  TString _ -> True
  TLParen -> True
  TLBrace -> True
  TLBracket -> True
  TTry -> True
  TMatch -> True
  _ -> False

startsExpr :: [Token] -> Bool
startsExpr (t : _) = isExprStarter t || isIdentExprStarter t
startsExpr [] = False

isIdentExprStarter :: Token -> Bool
isIdentExprStarter (TIdent _) = True
isIdentExprStarter _ = False

ctorFromTokens :: [Token] -> Maybe Ctor
ctorFromTokens ts = do
  (args, name) <- headerFromTokens ts
  argTypes <- typesFromHeaderTerms args
  pure (Ctor name argTypes)

functionHeader :: [Token] -> Maybe ([Pattern], String)
functionHeader ts = do
  (argTerms, name) <- headerFromTokens ts
  params <- patternsFromHeaderTerms argTerms
  pure (params, name)

headerFromTokens :: [Token] -> Maybe ([[Token]], String)
headerFromTokens ts = headerParts ts >>= headerFromParts

headerParts :: [Token] -> Maybe [[Token]]
headerParts = go []
 where
  go acc [] = Just (reverse acc)
  go acc ts = do
    (term, rest) <- readHeaderTerm ts
    go (term : acc) rest

readHeaderTerm :: [Token] -> Maybe ([Token], [Token])
readHeaderTerm (TIdent name : rest) = Just ([TIdent name], rest)
readHeaderTerm (TLParen : rest) = do
  (inner, rest2) <- either (const Nothing) Just (takeBalanced rest)
  Just (TLParen : inner ++ [TRParen], rest2)
readHeaderTerm _ = Nothing

headerFromParts :: [[Token]] -> Maybe ([[Token]], String)
headerFromParts = defaultHeader

defaultHeader :: [[Token]] -> Maybe ([[Token]], String)
defaultHeader [term] = do
  name <- headerTermName term
  pure ([], name)
defaultHeader (arg : fun : rest) = do
  name <- headerTermName fun
  pure (arg : rest, name)
defaultHeader _ = Nothing

headerTermName :: [Token] -> Maybe String
headerTermName [TIdent name] = Just name
headerTermName _ = Nothing

typeFromHeaderTerms :: [[Token]] -> Either String (TypeExpr, [Token])
typeFromHeaderTerms terms = (\t -> (t, [])) <$> parseWhole "could not parse flesh type" parseTypeTokens (concat terms)

typesFromHeaderTerms :: [[Token]] -> Maybe [TypeExpr]
typesFromHeaderTerms = traverse (parseWholeMaybe parseTypeTokens)

patternsFromHeaderTerms :: [[Token]] -> Maybe [Pattern]
patternsFromHeaderTerms = traverse patternFromTerm

namesFromHeaderTerms :: [[Token]] -> Maybe [String]
namesFromHeaderTerms = traverse headerTermName

takeUntilEquals, takeUntilColon :: [Token] -> Either String ([Token], [Token])
takeUntilEquals ts = takeUntilToken ts (\case TEquals -> True; _ -> False) "expected '='"
takeUntilColon ts = takeUntilToken ts (\case TColon -> True; _ -> False) "expected ':'"

takeUntilToken :: [Token] -> (Token -> Bool) -> String -> Either String ([Token], [Token])
takeUntilToken ts stop message =
  case break stop ts of
    (_, []) -> Left message
    found -> Right found

parseWhole :: String -> P a -> [Token] -> Either String a
parseWhole restMessage parser tokens = case parser tokens of
  Right (value, []) -> Right value
  Right _ -> Left restMessage
  Left msg -> Left msg

parseWholeMaybe :: P a -> [Token] -> Maybe a
parseWholeMaybe parser tokens = case parser tokens of
  Right (value, []) -> Just value
  _ -> Nothing

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just value) = Right value
maybeToEither msg Nothing = Left msg

parseCommaListUntil :: (Token -> Bool) -> P a -> P [a]
parseCommaListUntil stop parseItem = go
 where
  go (t : rest) | stop t = Right ([], rest)
  go ts = do
    (x, rest) <- parseItem ts
    (xs, rest2) <- go (dropComma rest)
    pure (x : xs, rest2)

dropComma, dropSemicolon :: [Token] -> [Token]
dropComma (TComma : rest) = rest
dropComma ts = ts
dropSemicolon (TSemicolon : rest) = rest
dropSemicolon ts = ts

type Lexer = Parsec Void String

tokenParser :: Lexer Token
tokenParser =
  lexeme $
    M.choice
      [ TForall <$ M.try (C.string "\\\\")
      , TInteger <$> M.try integerParser
      , TFloat <$> M.try floatParser
      , TIdent <$> M.try prependNameParser
      , charParser
      , stringParser
      , singleTokenParser
      , nameParser
      ]

singleTokenParser :: Lexer Token
singleTokenParser =
  M.choice
    [ TLParen <$ C.char '('
    , TRParen <$ C.char ')'
    , TLBrace <$ C.char '{'
    , TRBrace <$ C.char '}'
    , TLBracket <$ C.char '['
    , TRBracket <$ C.char ']'
    , TColon <$ C.char ':'
    , TComma <$ C.char ','
    , TMapsTo <$ C.char '|'
    , TBang <$ C.char '!'
    , TEquals <$ C.char '='
    , TSemicolon <$ C.char ';'
    , TDot <$ C.char '.'
    , TDollar <$ (C.char '$' <* M.notFollowedBy (M.satisfy isNameChar))
    , TArrow <$ C.char '→'
    ]

integerParser :: Lexer Int
integerParser = do
  sign <- optionalChar '-'
  digits <- some C.digitChar
  M.notFollowedBy (C.char '.' *> C.digitChar)
  pure (signed sign (read digits))

floatParser :: Lexer String
floatParser = do
  sign <- optionalChar '-'
  whole <- some C.digitChar
  _ <- C.char '.'
  frac <- some C.digitChar
  pure (maybe "" (: []) sign ++ whole ++ "." ++ frac)

prependNameParser :: Lexer String
prependNameParser = do
  _ <- C.string ".*"
  rest <- many (M.satisfy isNameChar)
  pure (".*" ++ rest)

charParser :: Lexer Token
charParser = do
  _ <- C.char '`'
  TChar <$> M.choice [unicodeCharParser, (: []) <$> M.anySingle]

unicodeCharParser :: Lexer String
unicodeCharParser = do
  _ <- C.char '{'
  digits <- many C.digitChar
  _ <- C.char '}'
  pure ("{" ++ digits ++ "}")

stringParser :: Lexer Token
stringParser = TString <$> (C.char '\'' *> many stringCharParser <* C.char '\'')

stringCharParser :: Lexer Char
stringCharParser =
  M.choice
    [ '\n' <$ C.string "\\n"
    , '\'' <$ C.string "\\'"
    , '\\' <$ C.string "\\\\"
    , M.anySingleBut '\''
    ]

nameParser :: Lexer Token
nameParser = keywordOrIdent <$> some (M.satisfy isNameChar)

spaceConsumer :: Lexer ()
spaceConsumer = L.space C.space1 (L.skipLineComment "#") empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

optionalChar :: Char -> Lexer (Maybe Char)
optionalChar c = M.optional (C.char c)

signed :: Maybe Char -> Int -> Int
signed (Just '-') n = -n
signed _ n = n

keywordOrIdent :: String -> Token
keywordOrIdent = \case
  "let" -> TLet
  "given" -> TGiven
  "bring" -> TBring
  "type" -> TType
  "data" -> TData
  "effect" -> TEffect
  "bone" -> TBone
  "flesh" -> TFlesh
  "match" -> TMatch
  "try" -> TTry
  s -> TIdent s

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c) && not (c `elem` specialNameChars)

specialNameChars :: [Char]
specialNameChars = "#(){}[]:,|!=;.$\\→`'"

isQualifiedName :: String -> Bool
isQualifiedName = elem '@'

isRightBrace, isRightBracket :: Token -> Bool
isRightBrace TRBrace = True
isRightBrace _ = False
isRightBracket TRBracket = True
isRightBracket _ = False

splitTopLevelForall, splitTopLevelArrow, splitTopLevelBang, splitTopLevelColon, splitTopLevelComma :: [Token] -> Maybe ([Token], [Token])
splitTopLevelForall ts = splitTopLevel ts (\case TForall -> True; _ -> False)
splitTopLevelArrow ts = splitTopLevel ts (\case TArrow -> True; _ -> False)
splitTopLevelBang ts = splitTopLevel ts (\case TBang -> True; _ -> False)
splitTopLevelColon ts = splitTopLevel ts (\case TColon -> True; _ -> False)
splitTopLevelComma ts = splitTopLevel ts (\case TComma -> True; _ -> False)

splitTopLevel :: [Token] -> (Token -> Bool) -> Maybe ([Token], [Token])
splitTopLevel ts stop = go [] (0 :: Int) ts
 where
  go _ _ [] = Nothing
  go acc depth (x : rest)
    | depth == 0 && stop x = Just (reverse acc, rest)
    | otherwise = case x of
        TLParen -> go (x : acc) (depth + 1) rest
        TRParen -> go (x : acc) (depth - 1) rest
        TLBracket -> go (x : acc) (depth + 1) rest
        TRBracket -> go (x : acc) (depth - 1) rest
        _ -> go (x : acc) depth rest

takeTopLevelUntil :: [Token] -> (Token -> Bool) -> String -> Either String ([Token], [Token])
takeTopLevelUntil ts stop message = case takeTopLevelUntilOrEnd ts stop of
  Right (_, []) -> Left message
  other -> other

takeTopLevelUntilOrEnd :: [Token] -> (Token -> Bool) -> Either String ([Token], [Token])
takeTopLevelUntilOrEnd ts stop = go [] (0 :: Int) ts stop
 where
  go acc _ [] _ = Right (reverse acc, [])
  go acc depth xs@(x : rest) stop
    | depth == 0 && stop x = Right (reverse acc, xs)
    | otherwise = case x of
        TLParen -> go (x : acc) (depth + 1) rest stop
        TRParen -> go (x : acc) (depth - 1) rest stop
        TLBracket -> go (x : acc) (depth + 1) rest stop
        TRBracket -> go (x : acc) (depth - 1) rest stop
        _ -> go (x : acc) depth rest stop

takeBalanced :: [Token] -> Either String ([Token], [Token])
takeBalanced = go [] (1 :: Int)
 where
  go _ _ [] = Left "unclosed parenthesized type"
  go acc depth (TLParen : rest) = go (TLParen : acc) (depth + 1) rest
  go acc 1 (TRParen : rest) = Right (reverse acc, rest)
  go acc depth (TRParen : rest) = go (TRParen : acc) (depth - 1) rest
  go acc depth (x : rest) = go (x : acc) depth rest

takeHeaderTokens :: String -> [Token] -> Either String ([Token], [Token])
takeHeaderTokens kind ts = takeTopLevelUntil ts (\case TLBrace -> True; _ -> False) ("expected '{' after " ++ kind ++ " header")

takeCtorTokens :: [Token] -> Either String ([Token], [Token])
takeCtorTokens [] = Left "unterminated data declaration"
takeCtorTokens ts@(TComma : _) = Right ([], ts)
takeCtorTokens ts@(TRBrace : _) = Right ([], ts)
takeCtorTokens (x : xs) = do
  (seen, rest) <- takeCtorTokens xs
  Right (x : seen, rest)
