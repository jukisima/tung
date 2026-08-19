{- | token parser for surface syntax. this stage lowers definition headers,
second-is-function sequences, and dollar grouping without consulting types.
-}
module Tung.Parse (
  ParsedSource (..),
  parse,
  parseLocated,
  parseTokens,
)
where

import Control.Monad (foldM)
import Data.Either (fromRight)
import Data.List (intercalate, unsnoc)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Tung.Name (isQualifiedName)
import Tung.Syntax
import Tung.Token

data FunctionResult = FunctionResult
  { functionResultType :: TypeExpr
  , functionResultEffects :: [TypeExpr]
  , functionResultNeeds :: [ShapeNeed]
  }
  deriving (Eq, Show)

-- parsers consume a token prefix and return the untouched suffix. whole-input
-- helpers are used only where the surrounding delimiter hath already been found.
type P a = [Token] -> Either String (a, [Token])

data ParsedSource = ParsedSource
  { parsedProgram :: Program
  , parsedTokens :: [LocatedToken]
  }
  deriving (Eq, Show)

parse :: String -> Either String Program
parse source = lexTokens source >>= parseTokens

parseLocated :: String -> Either String ParsedSource
parseLocated source = do
  tokens <- lexLocatedTokens source
  program <- parseTokens (map locatedToken tokens)
  pure (ParsedSource (locateProgram program tokens) tokens)

parseTokens :: [Token] -> Either String Program
parseTokens toks = case parseDecls toks of
  Right (decls, []) -> pure (Program decls)
  Right _ -> Left "unexpected tokens after program"
  Left msg -> Left msg

-- declarations are self-delimiting; semicolons are accepted between file-level
-- declarations and required only where the grammar requireth them.
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
  TShow : rest -> parseExportGroup rest
  TShowIlk : rest -> parseTypeReExportGroup rest
  ts -> do
    (d, rest) <- parseDecl ts
    pure ([d], rest)

parseExportGroup :: P [Decl]
parseExportGroup (TGraith : _) = Left "show must follow graith requirements"
parseExportGroup ts = case parseReExportNames ts of
  Right (names, rest) -> pure (map ReExport names, rest)
  Left _ -> do
    (decl, rest) <- parseDecl ts
    case decl of
      Import _ -> Left "show cannot precede bring"
      Let "_" _ _ -> Left "show must precede a declaration or name"
      _ -> pure ([Export decl], rest)

parseTypeReExportGroup :: P [Decl]
parseTypeReExportGroup ts = do
  (names, rest) <- parseReExportNames ts
  pure (map ReExportType names, rest)

parseReExportNames :: P [String]
parseReExportNames = go []
 where
  go names = \case
    TIdent name : TComma : rest -> go (name : names) rest
    TIdent name : TSemicolon : rest -> pure (reverse (name : names), rest)
    _ -> Left "expected re-exported name"

parseDecl :: P Decl
parseDecl = \case
  TBring : rest -> parseImport rest
  TGraith : rest -> parseGraithDecl rest
  TLet : rest -> parseLetDecl [] rest
  TLetIlk : rest -> parseTypeAlias rest
  TKin : rest -> parseData rest
  TDeed : rest -> parseEffect rest
  TShape : rest -> parseShape [] rest
  TFill : rest -> parseFill [] rest
  ts -> case parseExpr ts of
    Right (e, rest) -> Right (Let "_" Nothing e, rest)
    Left _ -> Left "expected declaration"

parseGraithDecl :: P Decl
parseGraithDecl ts = do
  (needs, rest) <- parseGraithPrefix isGraithEnd "expected 'let', 'shape' or 'fill' after graith" ts
  case rest of
    TLet : rest2 -> parseLetDecl needs rest2
    TShape : rest2 -> parseShape needs rest2
    TFill : rest2 -> parseFill needs rest2
    TShow : TLet : rest2 -> exportParsed (parseLetDecl needs rest2)
    TShow : TShape : rest2 -> exportParsed (parseShape needs rest2)
    TShow : TFill : rest2 -> exportParsed (parseFill needs rest2)
    _ -> Left "expected 'let', 'shape' or 'fill' after graith"

exportParsed :: Either String (Decl, [Token]) -> Either String (Decl, [Token])
exportParsed = fmap (\(decl, rest) -> (Export decl, rest))

isGraithEnd :: Token -> Bool
isGraithEnd = \case
  TLet -> True
  TShape -> True
  TFill -> True
  TShow -> True
  TShowIlk -> True
  _ -> False

parseGraithLet :: P Decl
parseGraithLet ts = do
  (needs, rest) <- parseGraithPrefix (\case TLet -> True; _ -> False) "expected 'let' after graith" ts
  case rest of
    TLet : rest2 -> parseLetDecl needs rest2
    _ -> Left "expected 'let' after graith"

parseGraithPrefix :: (Token -> Bool) -> String -> [Token] -> Either String ([ShapeNeed], [Token])
parseGraithPrefix stop message ts = do
  (graithTokens, rest) <- takeTopLevelUntil ts stop message
  needs <- parseShapeNeedsWhole graithTokens
  pure (needs, rest)

-- let parsing accepteth named and positional headers, typed parameter groups, and
-- graith prefixes, then lowers every parameter list to an anonymous match.
parseLetDecl :: [ShapeNeed] -> P Decl
parseLetDecl needs = \case
  TIdent name : rest -> parseLet needs name rest
  ts@(TInteger _ : _) -> case parseLetHeaderDefinitionWithNeeds needs ts of
    Just parsed -> parsed
    Nothing -> Left "expected let binding"
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

parseTypeAlias :: P Decl
parseTypeAlias ts = do
  (header, rest) <- takeTopLevelUntil ts (\case TEquals -> True; _ -> False) "expected '=' after let-ilk header"
  (params, name) <- parseParamHeader "let-ilk" header
  case rest of
    TEquals : body -> do
      (target, remaining) <- parseTypeUntilSemicolon body
      pure (TypeAlias params name target, remaining)
    _ -> Left "expected '=' after let-ilk header"

parseLet :: [ShapeNeed] -> String -> P Decl
parseLet needs name = \case
  TEquals : rest -> parseUntypedLet needs name [] rest
  TColon : rest -> parseColonLet needs name rest
  TIdent "_" : rest -> do
    (e, rest2) <- parseExpr rest
    pure (Let name (implicitLetAnn needs []) e, rest2)
  ts -> parseLetWithOptionalType needs name ts

parseColonLet :: [ShapeNeed] -> String -> P Decl
parseColonLet needs name ts =
  case parseTypeAnnUntilEquals ts of
    Right (ann, TEquals : rest) -> do
      (e, rest2) <- parseExpr rest
      pure (Let name (Just (withGraithNeeds needs ann)) e, rest2)
    _ -> Left "expected '=' after let type"

parseLetWithOptionalType :: [ShapeNeed] -> String -> P Decl
parseLetWithOptionalType needs name ts =
  case parseLetHeaderDefinitionWithNeeds needs (TIdent name : ts) of
    Just result -> result
    Nothing -> case parseTypeUntilExpr ts of
      Right (t, exprTokens) -> do
        (e, rest) <- parseExpr exprTokens
        pure (Let name (Just (TypeAnn t needs)) e, rest)
      Left _ -> parseUntypedLet needs name [] ts

parseLetHeaderDefinitionWithNeeds :: [ShapeNeed] -> [Token] -> Maybe (Either String (Decl, [Token]))
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

parseLetHeaderBody :: [ShapeNeed] -> String -> [Pattern] -> P Decl
parseLetHeaderBody needs name params ts = do
  (e, rest) <- parseExpr ts
  pure (letWithParams name (replicate (length params) Nothing) needs Nothing params e, rest)

parseLetHeaderTypedBody :: String -> [Pattern] -> TypeAnn -> P Decl
parseLetHeaderTypedBody name params ann ts = do
  (e, rest) <- parseExpr ts
  pure (Let name (Just ann) (lambdaIfParams params e), rest)

parseTypedArgumentLet :: [ShapeNeed] -> P Decl
parseTypedArgumentLet graithNeeds ts = do
  (parsed, rest) <- parseTypedLetParams ts
  case (parsed, rest) of
    ((params, argTypes), TIdent name : TColon : rest2) ->
      case parseFunctionResultUntilEquals rest2 of
        Right (result, TEquals : bodyTokens) -> do
          (body, rest3) <- parseExpr bodyTokens
          ann <- functionLetAnn graithNeeds argTypes result
          pure (Let name (Just ann) (lambdaIfParams params body), rest3)
        _ -> Left "expected '=' after let result type"
    ((params, argTypes), TIdent name : TEquals : bodyTokens) -> do
      (body, rest3) <- parseExpr bodyTokens
      pure (letWithParams name argTypes graithNeeds Nothing params body, rest3)
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

functionLetAnn :: [ShapeNeed] -> [Maybe TypeExpr] -> FunctionResult -> Either String TypeAnn
functionLetAnn needs [] FunctionResult{functionResultType = result, functionResultEffects = [], functionResultNeeds} = Right (TypeAnn result (needs ++ functionResultNeeds))
functionLetAnn _ [] _ = Left "effect annotation requireth function arguments"
functionLetAnn needs argTypes FunctionResult{..} =
  TypeAnn <$> makeArrowType (fillMissingTypes argTypes) functionResultEffects functionResultType <*> pure (needs ++ functionResultNeeds)

implicitLetAnn :: [ShapeNeed] -> [Maybe TypeExpr] -> Maybe TypeAnn
implicitLetAnn [] [] = Nothing
implicitLetAnn [] argTypes | all isNothing argTypes = Nothing
implicitLetAnn needs argTypes = Just (TypeAnn ty needs)
 where
  ty = case argTypes of
    [] -> implicitTypeAt 0
    _ -> fromRight (implicitTypeAt (length argTypes)) (makeArrowType (fillMissingTypes argTypes) [] (implicitTypeAt (length argTypes)))

letWithParams :: String -> [Maybe TypeExpr] -> [ShapeNeed] -> Maybe TypeAnn -> [Pattern] -> Expr -> Decl
letWithParams name paramTypes needs resultAnn params body =
  Let name ann (lambdaIfParams params body)
 where
  ann = case resultAnn of
    Just result -> Just (withGraithNeeds needs result)
    Nothing -> implicitLetAnn needs paramTypes

fillMissingTypes :: [Maybe TypeExpr] -> [TypeExpr]
fillMissingTypes = zipWith fill [0 :: Int ..]
 where
  fill _ (Just t) = t
  fill n Nothing = implicitTypeAt n

implicitTypeAt :: Int -> TypeExpr
implicitTypeAt n = TypeName ("t" ++ show n)

withGraithNeeds :: [ShapeNeed] -> TypeAnn -> TypeAnn
withGraithNeeds needs (TypeAnn t annNeeds) = TypeAnn t (needs ++ annNeeds)

parseUntypedLet :: [ShapeNeed] -> String -> [Maybe TypeExpr] -> P Decl
parseUntypedLet needs name argTypes ts = do
  (e, rest) <- parseExpr ts
  pure (Let name (implicitLetAnn needs argTypes) e, rest)

parseData :: P Decl
parseData ts = do
  (params, name, body) <- parseParamHeaderBody "kin" ts
  (ctors, rest) <- parseCtors body
  pure (DataDecl params name ctors, rest)

parseEffect :: P Decl
parseEffect ts = do
  (params, name, body) <- parseParamHeaderBody "deed" ts
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
  (paramTerms, name) <- maybeToEither (kind ++ " declaration requireth a name") (headerFromTokens header)
  params <- maybeToEither (kind ++ " parameters must be names") (namesFromHeaderTerms paramTerms)
  pure (params, name)

parseEffectOps :: P [EffectOp]
parseEffectOps = parseCommaListUntil isRightBrace parseEffectOp

parseEffectOp :: P EffectOp
parseEffectOp ts =
  case takeUntilColon ts of
    Right (parts, TColon : rest) -> case shapeMemberHeader parts of
      Nothing -> Left "expected effect operation name"
      Just (argTypes, name) -> do
        (result, rest2) <- parseFunctionResultUntilCommaOrBrace rest
        ann <- functionLetAnn [] (map Just argTypes) result
        case ann of
          TypeAnn t _ -> pure (EffectOp name t, rest2)
    _ -> Left "expected effect operation"

parseShape :: [ShapeNeed] -> P Decl
parseShape needs ts = do
  (header, body) <- parseHeaderBody "shape" ts
  (paramTerms, name) <- maybeToEither "shape declaration requireth a name" (trailingHeaderFromTokens header)
  params <- maybeToEither "shape parameters must be names" (namesFromHeaderTerms paramTerms)
  (members, rest) <- parseShapeMembers body
  pure (ShapeDecl params name needs members, rest)

parseShapeNeeds :: P [ShapeNeed]
parseShapeNeeds = go []
 where
  go acc [] = Right (reverse acc, [])
  go acc (TComma : rest) = go acc rest
  go acc input = do
    (parts, rest) <- takeTopLevelUntilOrEnd input (\case TComma -> True; _ -> False)
    (need, _) <- parseShapeNeed parts
    go (need : acc) (dropComma rest)

parseShapeNeedsWhole :: [Token] -> Either String [ShapeNeed]
parseShapeNeedsWhole [] = Right []
parseShapeNeedsWhole tokens = parseWhole "unexpected tokens after graith" parseShapeNeeds tokens

parseShapeNeed :: P ShapeNeed
parseShapeNeed [] = Left "empty graith"
parseShapeNeed ts = case trailingHeaderFromTokens ts of
  Nothing -> Left "invalid graith"
  Just (argTerms, name) -> case typesFromHeaderTerms argTerms of
    Nothing -> Left "graith arguments must be types"
    Just args -> Right (ShapeNeed args name, [])

parseFill :: [ShapeNeed] -> P Decl
parseFill needs ts = do
  (header, body) <- parseHeaderBody "fill" ts
  (tyTerms, shapeName) <- maybeToEither "fill declaration requireth a shape name" (trailingHeaderFromTokens header)
  tyArgs <- maybeToEither "fill type arguments must be types" (typesFromHeaderTerms tyTerms)
  case tyArgs of
    [] -> Left "fill declaration requireth at least one type argument"
    _ -> pure ()
  (ds, rest) <- parseFillMembers body
  case rest of
    TRBrace : rest2 -> pure (FillDecl tyArgs shapeName needs ds, rest2)
    _ -> Left "expected '}' after fill body"

parseFillMembers :: P [Decl]
parseFillMembers (TRBrace : rest) = Right ([], TRBrace : rest)
parseFillMembers (TSemicolon : rest) = parseFillMembers rest
parseFillMembers ts@(TGraith : _) = parseFillLetMember ts
parseFillMembers ts@(TLet : _) = parseFillLetMember ts
parseFillMembers _ = Left "expected fill member let"

parseFillLetMember :: P [Decl]
parseFillLetMember ts = do
  (d, rest) <- parseFillLetDecl ts
  (ds, rest2) <- parseFillMembers rest
  pure (d : ds, rest2)

parseFillLetDecl :: P Decl
parseFillLetDecl = \case
  TGraith : rest -> parseGraithLet rest
  TLet : rest -> parseLetDecl [] rest
  _ -> Left "expected fill member let"

lambdaIfParams :: [Pattern] -> Expr -> Expr
lambdaIfParams [] expr = expr
lambdaIfParams (p : ps) expr = anonymousMatch (p :| ps) expr

anonymousMatch :: NonEmpty Pattern -> Expr -> Expr
anonymousMatch params expr = EMatch [] [MatchCase params expr]

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

parseShapeMembers :: P [ShapeMember]
parseShapeMembers (TRBrace : rest) = Right ([], rest)
parseShapeMembers (TSemicolon : rest) = parseShapeMembers rest
parseShapeMembers ts@(TGraith : _)
  | startsGraithLet ts = parseShapeDefaultLet ts
  | otherwise = parseGraithShapeSignature ts
parseShapeMembers ts@(TLet : _) = parseShapeDefaultLet ts
parseShapeMembers ts@(TLaw : _) = parseShapeLaw ts
parseShapeMembers ts@(TIdent _ : _) = parsePostfixShapeSignature ts
parseShapeMembers ts@(TLParen : _) = parsePostfixShapeSignature ts
parseShapeMembers _ = Left "expected shape member"

parsePostfixShapeSignature :: P [ShapeMember]
parsePostfixShapeSignature ts = do
  (header, rest) <- takeUntilColon ts
  (argTypes, name) <- maybe (Left "expected shape member name") Right (shapeMemberHeader header)
  case rest of
    TColon : result -> parseShapeSignature [] argTypes name result
    _ -> Left "expected ':' in shape member"

parseGraithShapeSignature :: P [ShapeMember]
parseGraithShapeSignature (TGraith : ts) = do
  (header, rest) <- takeUntilColon ts
  (needs, argTypes, name) <- case mapMaybe (candidate header) [1 .. length header - 1] of
    [parsed] -> Right parsed
    [] -> Left "expected graith requirements followed by a shape member"
    _ -> Left "ambiguous graith shape member"
  case rest of
    TColon : result -> parseShapeSignature needs argTypes name result
    _ -> Left "expected ':' in shape member"
 where
  candidate header index = do
    let (needTokens, memberTokens) = splitAt index header
    needs <- either (const Nothing) Just (parseShapeNeedsWhole needTokens)
    (argTypes, name) <- shapeMemberHeader memberTokens
    pure (needs, argTypes, name)
parseGraithShapeSignature _ = Left "expected graith shape member"

parseShapeSignature :: [ShapeNeed] -> [TypeExpr] -> String -> P [ShapeMember]
parseShapeSignature needs argTypes name ts = do
  ((result, rest), _) <- parseShapeMemberResultType ts
  ann <- functionLetAnn needs (map Just argTypes) result
  (members, rest') <- parseShapeMembers rest
  pure (ShapeSpec name ann : members, rest')

startsGraithLet :: [Token] -> Bool
startsGraithLet = elem TLet . takeWhile (`notElem` [TEquals, TSemicolon, TRBrace])

shapeMemberHeader :: [Token] -> Maybe ([TypeExpr], String)
shapeMemberHeader ts = do
  (argTerms, name) <- headerFromTokens ts
  argTypes <- typesFromHeaderTerms argTerms
  pure (argTypes, name)

parseShapeMemberResultType :: P (FunctionResult, [Token])
parseShapeMemberResultType ts =
  case parseFunctionResultUntilSemicolonOrBraceOrEquals ts of
    Right (_, TEquals : _) -> Left "shape default must start with let"
    Right (t, rest) -> Right ((t, rest), [])
    Left msg -> Left msg

parseShapeDefaultLet :: P [ShapeMember]
parseShapeDefaultLet ts = do
  d <- case ts of
    TGraith : rest -> parseGraithLet rest
    TLet : rest -> parseLetDecl [] rest
    _ -> Left "expected shape default"
  case d of
    (Let name ann expr, rest) -> do
      (members, rest2) <- parseShapeMembers rest
      pure (ShapeDefault name ann expr : members, rest2)
    _ -> Left "shape default must be a let"

parseShapeLaw :: P [ShapeMember]
parseShapeLaw (TLaw : TLParen : ts) = do
  ((patterns, annotations), rest) <- parseTypedLetParams ts
  parameters <- traverse lawParameter (zip patterns annotations)
  case rest of
    TColon : body -> do
      (leftTokens, separator) <- takeTopLevelUntil body isLawSeparator "expected '~' between law sides"
      (rightTokens, rest2) <- case separator of
        TIdent "~" : right -> takeTopLevelUntilOrEnd right lawEnd
        _ -> Left "expected '~' between law sides"
      left <- parseWhole "unexpected tokens on left side of law" parseExpr leftTokens
      right <- parseWhole "unexpected tokens on right side of law" parseExpr rightTokens
      (members, rest3) <- parseShapeMembers rest2
      pure (ShapeLaw parameters left right : members, rest3)
    _ -> Left "expected ':' after law parameters"
 where
  lawParameter (PVar name, Just annotation) = Right (name, annotation)
  lawParameter (_, Nothing) = Left "law parameters require type annotations"
  lawParameter _ = Left "law parameters must be names"
  isLawSeparator (TIdent "~") = True
  isLawSeparator _ = False
  lawEnd TSemicolon = True
  lawEnd TRBrace = True
  lawEnd _ = False
parseShapeLaw _ = Left "expected parenthesised law parameters"

-- '$' is the only lower-precedence application layer. ordinary sequences are
-- parsed below it and use the second term as their function.
parseExpr :: P Expr
parseExpr = parseDollarExprAllowBrace

parseExprStopBrace :: P Expr
parseExprStopBrace = parseDollarExprStopBrace

parseDollarExprAllowBrace, parseDollarExprStopBrace :: P Expr
parseDollarExprAllowBrace = parseDollarExprWith False
parseDollarExprStopBrace = parseDollarExprWith True

parseDollarExprWith :: Bool -> P Expr
parseDollarExprWith stopBrace ts = do
  (headExpr, rest) <- parsePostfixExprWith stopBrace ts
  parseDollarTail stopBrace headExpr rest

parseDollarTail :: Bool -> Expr -> P Expr
parseDollarTail stopBrace value = \case
  TDollar : rest -> do
    (piped, rest2) <- parseDollarSegment stopBrace value rest
    parseDollarTail stopBrace piped rest2
  rest -> Right (value, rest)

parseDollarSegment :: Bool -> Expr -> P Expr
parseDollarSegment stopBrace value = \case
  TLParen : rest -> parseDollarParenSegment value rest
  rest -> parseDollarBareSegment stopBrace value rest

parseDollarBareSegment :: Bool -> Expr -> P Expr
parseDollarBareSegment stopBrace value ts = do
  (terms, rest) <- parseDollarBareTerms stopBrace ts
  case terms of
    fn : args -> Right (applyArgs fn (value : args), rest)
    [] -> Left "expected function after '$'"

parseDollarBareTerms :: Bool -> P [Expr]
parseDollarBareTerms stopBrace = go []
 where
  go acc ts = case ts of
    [] -> Right (reverse acc, [])
    TLBrace : _ | stopBrace && not (null acc) -> Right (reverse acc, ts)
    TDollar : _ -> Right (reverse acc, ts)
    t : _ | exprStop t -> Right (reverse acc, ts)
    _ -> case parseExprAtom ts of
      Right (arg, rest) -> go (arg : acc) rest
      Left _
        | null acc -> Left "expected function after '$'"
        | otherwise -> Right (reverse acc, ts)

parseDollarParenSegment :: Expr -> P Expr
parseDollarParenSegment value ts = do
  (terms, rest) <- parseFunctionFirstTerms ts
  case (terms, rest) of
    (fn : args, TRParen : rest2) -> Right (applyArgs fn (value : args), rest2)
    ([], _) -> Left "expected function after '$('"
    (_, _) -> Left "expected ')' after '$('"

parseFunctionFirstTerms :: P [Expr]
parseFunctionFirstTerms = go []
 where
  go acc ts = case ts of
    [] -> Right (reverse acc, [])
    TRParen : _ -> Right (reverse acc, ts)
    _ -> case parseExprAtom ts of
      Right (arg, rest) -> go (arg : acc) rest
      Left _ -> Right (reverse acc, ts)

parsePostfixExprWith :: Bool -> P Expr
parsePostfixExprWith stopBrace ts = do
  (parts, rest) <- parseExprParts stopBrace ts
  defaultApplication parts rest

applyArgs :: Expr -> [Expr] -> Expr
applyArgs = foldl' (\expr arg -> EApply expr (arg :| []))

-- a lone term denotes itself; otherwise @a f b c@ lowers to @f a b c@.
defaultApplication :: [Expr] -> [Token] -> Either String (Expr, [Token])
defaultApplication parts rest = case parts of
  [] -> Left "expected expression"
  [x] -> Right (x, rest)
  arg : f : args -> Right (applyArgs f (arg : args), rest)

parseExprParts :: Bool -> P [Expr]
parseExprParts stopBrace = go []
 where
  go acc ts = case ts of
    [] -> Right (reverse acc, [])
    TLBrace : _ | stopBrace && not (null acc) -> Right (reverse acc, ts)
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
  TGraith -> True
  TBring -> True
  TLetIlk -> True
  TKin -> True
  TDeed -> True
  TShape -> True
  TFill -> True
  TShow -> True
  TShowIlk -> True
  _ -> False

parseExprAtom :: P Expr
parseExprAtom = \case
  TInteger i : rest -> Right (EInteger i, rest)
  TFloat s : rest -> Right (EFloat s, rest)
  TUnicode codePoint : rest -> Right (EUnicode codePoint, rest)
  TText s : rest -> Right (EText s, rest)
  TForeign : rest -> Right (EForeign, rest)
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
parseMatchExpr (TLBrace : _) = Left "match requireth a scrutinee; use '{ ... }' for a function"
parseMatchExpr ts = parseMatchScrutinees [] ts

parseMatchScrutinees :: [Expr] -> P Expr
parseMatchScrutinees acc ts = do
  (scrutinee, rest) <- parseExprStopBrace ts
  case rest of
    TComma : rest2 -> parseMatchScrutinees (scrutinee : acc) rest2
    TLBrace : rest2 -> do
      (cs, rest3) <- parseMatchCases rest2
      pure (EMatch (reverse (scrutinee : acc)) cs, rest3)
    _ -> Left "expected match scrutinee or '{'"

parseParen :: P Expr
parseParen (TRParen : rest) = Right (EVar "null", rest)
parseParen ts@(TLet : _) = parseBlock ts
parseParen ts@(TGraith : _) = parseBlock ts
parseParen ts = do
  (e, rest) <- parseExpr ts
  case rest of
    TRParen : rest2 -> Right (e, rest2)
    _ -> Left "expected ')' after expression"

parseTry :: P Expr
parseTry ts = do
  (body, rest) <- parseExprStopBrace ts
  case rest of
    TLBrace : rest2 -> do
      ((returnCase, cases), rest3) <- parseHandlerCases rest2
      pure (ETry body returnCase cases, rest3)
    _ -> Left "expected handler cases after try body"

data HandlerItem = HandlerReturnItem ReturnCase | HandlerCaseItem HandlerCase

parseHandlerCases :: P (Maybe ReturnCase, [HandlerCase])
parseHandlerCases ts = do
  (items, rest) <- parseCommaListUntil isRightBrace parseHandlerItem ts
  (returnCase, cases) <- foldM collect (Nothing, []) items
  pure ((returnCase, reverse cases), rest)
 where
  collect (Nothing, cases) (HandlerReturnItem returnCase) = Right (Just returnCase, cases)
  collect (Just _, _) (HandlerReturnItem _) = Left "handler hath more than one return case"
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
  Right (pat, []) -> Just pat
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
parseBlockLets ts@(TGraith : _) = do
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

parseMatchCases :: P [MatchCase]
parseMatchCases = parseCommaListUntil isRightBrace parseMatchCase

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
patternFromParts parts rest = case defaultHeader parts of
  Nothing -> Left "constructor pattern requireth a name"
  Just (argTerms, name) -> case patternsFromHeaderTerms argTerms of
    Nothing -> Left "invalid constructor pattern"
    Just args -> Right (PCon name args, rest)

patternFromTerm :: [Token] -> Maybe Pattern
patternFromTerm [TIdent name] = Just (PVar name)
patternFromTerm [TInteger value] = Just (PInteger value)
patternFromTerm (TLParen : rest) = case takeBalanced rest of
  Right (inner, []) -> parseWholeMaybe parsePattern inner
  _ -> Nothing
patternFromTerm term = parseWholeMaybe parsePattern term

parseTypeUntilExpr :: P TypeExpr
parseTypeUntilExpr = go []
 where
  go _ [] = Left "expected expression after type"
  go [] (TLParen : rest) = case takeBalanced rest of
    Right (inner, rest2) | startsExpr rest2 -> (,rest2) <$> parseWhole "let hath no type annotation" parseTypeTokens inner
    _ -> Left "let hath no type annotation"
  go acc ts@(x : _)
    | isExprStarter x && not (null acc) = (,ts) <$> parseWhole "could not parse whole type" parseTypeTokens (reverse acc)
    | isExprStarter x = Left "let hath no type annotation"
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
finishType tokens rest = (,rest) <$> parseWhole "could not parse whole type" parseTypeTokens tokens

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
        effects <- parseWhole "unexpected tokens after effects" parseCommaTypes effectTokens
        Right (FunctionResult ret effects [], rest)

-- arrows are special syntax with non-empty domains. type application otherwise
-- followeth the same second-is-function rule as term application.
parseTypeTokens :: P TypeExpr
parseTypeTokens = parseArrowType

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
    effects <- parseWhole "unexpected tokens after effects" parseCommaTypes effectTokens
    pure ((effects, ret), [])
  Nothing -> parseArrowResultOld ts

makeArrowType :: [TypeExpr] -> [TypeExpr] -> TypeExpr -> Either String TypeExpr
makeArrowType args effects ret = do
  domain <- maybe (Left "function type requireth at least one argument") Right (NE.nonEmpty args)
  pure $ case (effects, ret) of
    ([], TypeArrow more moreEffects finalRet) -> TypeArrow (domain <> more) moreEffects finalRet
    _ -> TypeArrow domain effects ret

parseArrowResultOld :: P ([TypeExpr], TypeExpr)
parseArrowResultOld (TLBrace : rest) = do
  (effectTokens, rest2) <- takeEffectTokens rest
  (ret, rest3) <- parseArrowType rest2
  effects <- parseWhole "unexpected tokens in effects" parseCommaTypes effectTokens
  pure ((effects, ret), rest3)
parseArrowResultOld ts = do
  (ret, rest) <- parseArrowType ts
  pure (([], ret), rest)

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
  defaultType parts rest

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
    t <- parseWhole "could not parse parenthesised type" parseParenthesizedType inner
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

-- @a f b@ is @f<a,b>@; unresolved non-name heads are retained for the checker
-- to reject with type context rather than by guessing in the parser.
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
  TIdent _ -> True
  TInteger _ -> True
  TFloat _ -> True
  TUnicode _ -> True
  TText _ -> True
  TLParen -> True
  TLBrace -> True
  TLBracket -> True
  TTry -> True
  TMatch -> True
  _ -> False

startsExpr :: [Token] -> Bool
startsExpr (t : _) = isExprStarter t
startsExpr [] = False

ctorFromTokens :: [Token] -> Maybe Ctor
ctorFromTokens ts = do
  (args, name) <- headerFromTokens ts
  argTypes <- typesFromHeaderTerms args
  pure (Ctor name argTypes)

-- declaration headers share sequence application: the second term names the
-- declaration and all other terms are its parameters.
functionHeader :: [Token] -> Maybe ([Pattern], String)
functionHeader ts = do
  (argTerms, name) <- headerFromTokens ts
  params <- patternsFromHeaderTerms argTerms
  pure (params, name)

headerFromTokens :: [Token] -> Maybe ([[Token]], String)
headerFromTokens ts = headerParts ts >>= defaultHeader

trailingHeaderFromTokens :: [Token] -> Maybe ([[Token]], String)
trailingHeaderFromTokens ts = headerParts ts >>= trailingHeaderFromParts

headerParts :: [Token] -> Maybe [[Token]]
headerParts = go []
 where
  go acc [] = Just (reverse acc)
  go acc ts = do
    (term, rest) <- readHeaderTerm ts
    go (term : acc) rest

readHeaderTerm :: [Token] -> Maybe ([Token], [Token])
readHeaderTerm (TIdent name : rest) = Just ([TIdent name], rest)
readHeaderTerm (TInteger value : rest) = Just ([TInteger value], rest)
readHeaderTerm (TLParen : rest) = do
  (inner, rest2) <- either (const Nothing) Just (takeBalanced rest)
  Just (TLParen : inner ++ [TRParen], rest2)
readHeaderTerm _ = Nothing

defaultHeader :: [[Token]] -> Maybe ([[Token]], String)
defaultHeader [term] = do
  name <- headerTermName term
  pure ([], name)
defaultHeader (arg : fun : rest) = do
  name <- headerTermName fun
  pure (arg : rest, name)
defaultHeader _ = Nothing

trailingHeaderFromParts :: [[Token]] -> Maybe ([[Token]], String)
trailingHeaderFromParts parts = do
  (arguments, term) <- unsnoc parts
  name <- headerTermName term
  pure (arguments, name)

headerTermName :: [Token] -> Maybe String
headerTermName [TIdent name] = Just name
headerTermName _ = Nothing

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
maybeToEither msg = maybe (Left msg) Right

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

isRightBrace, isRightBracket :: Token -> Bool
isRightBrace = (== TRBrace)
isRightBracket = (== TRBracket)

splitTopLevelArrow, splitTopLevelBang, splitTopLevelColon, splitTopLevelComma :: [Token] -> Maybe ([Token], [Token])
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

-- delimiter-aware slicing keepeth declaration and type parsers small. callers
-- decide whether reaching the end is valid for their enclosing construct.
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
        TLBrace -> go (x : acc) (depth + 1) rest stop
        TRBrace -> go (x : acc) (depth - 1) rest stop
        _ -> go (x : acc) depth rest stop

takeBalanced :: [Token] -> Either String ([Token], [Token])
takeBalanced = go [] (1 :: Int)
 where
  go _ _ [] = Left "unclosed parenthesised type"
  go acc depth (TLParen : rest) = go (TLParen : acc) (depth + 1) rest
  go acc 1 (TRParen : rest) = Right (reverse acc, rest)
  go acc depth (TRParen : rest) = go (TRParen : acc) (depth - 1) rest
  go acc depth (x : rest) = go (x : acc) depth rest

takeHeaderTokens :: String -> [Token] -> Either String ([Token], [Token])
takeHeaderTokens kind ts = takeTopLevelUntil ts (\case TLBrace -> True; _ -> False) ("expected '{' after " ++ kind ++ " header")

takeCtorTokens :: [Token] -> Either String ([Token], [Token])
takeCtorTokens ts = case break (\token -> token == TComma || token == TRBrace) ts of
  (_, []) -> Left "unterminated kin declaration"
  result -> Right result

-- located parsing decorateth expression nodes after the compact parser hath
-- settled the language's unusual application order. declaration delimiters
-- align the token cursor before each body, so repeated names resolve to their
-- actual occurrence instead of the first matching token in the file.
type Locator a = [LocatedToken] -> (a, [LocatedToken])

locateProgram :: Program -> [LocatedToken] -> Program
locateProgram (Program declarations) tokens = Program (fst (locateDecls declarations tokens))

locateDecls :: [Decl] -> Locator [Decl]
locateDecls [] tokens = ([], tokens)
locateDecls (declaration : declarations) tokens =
  let (declaration2, rest) = locateDecl declaration tokens
      (declarations2, rest2) = locateDecls declarations (dropLocatedSemicolon rest)
   in (declaration2 : declarations2, rest2)

locateDecl :: Decl -> Locator Decl
locateDecl declaration tokens = case declaration of
  Import path -> (declaration, afterToken TSemicolon (afterToken (TIdent path) tokens))
  Export nested ->
    let (nested2, rest) = locateDecl nested tokens
     in (Export nested2, rest)
  ReExport name -> (declaration, afterToken TSemicolon (afterToken (TIdent name) tokens))
  ReExportType name -> (declaration, afterToken TSemicolon (afterToken (TIdent name) tokens))
  Let name annotation body ->
    let bodyTokens = if startsLetDeclaration tokens then afterToken TEquals tokens else tokens
        (body2, rest) = locateExpr body bodyTokens
     in (Let name annotation body2, rest)
  TypeAlias{} -> (declaration, afterToken TSemicolon tokens)
  DataDecl{} -> (declaration, afterBraceBlock tokens)
  EffectDecl{} -> (declaration, afterBraceBlock tokens)
  ShapeDecl parameters name needs members ->
    let (members2, _) = locateShapeMembers members (afterToken TLBrace tokens)
     in (ShapeDecl parameters name needs members2, afterBraceBlock tokens)
  FillDecl types name needs members ->
    let (members2, _) = locateDecls members (afterToken TLBrace tokens)
     in (FillDecl types name needs members2, afterBraceBlock tokens)
  ElaboratedFill key types name needs members ->
    let (members2, _) = locateDecls members (afterToken TLBrace tokens)
     in (ElaboratedFill key types name needs members2, afterBraceBlock tokens)

locateShapeMembers :: [ShapeMember] -> Locator [ShapeMember]
locateShapeMembers [] tokens = ([], tokens)
locateShapeMembers (member : members) tokens =
  let (member2, rest) = locateShapeMember member tokens
      (members2, rest2) = locateShapeMembers members (dropLocatedSemicolon rest)
   in (member2 : members2, rest2)

locateShapeMember :: ShapeMember -> Locator ShapeMember
locateShapeMember member tokens = case member of
  ShapeSpec{} -> (member, afterToken TSemicolon tokens)
  ShapeDefault name annotation body ->
    let (body2, rest) = locateExpr body (afterToken TEquals tokens)
     in (ShapeDefault name annotation body2, rest)
  ShapeLaw parameters left right ->
    let (left2, rest) = locateExpr left (afterToken TColon tokens)
        (right2, rest2) = locateExpr right (afterToken (TIdent "~") rest)
     in (ShapeLaw parameters left2 right2, rest2)

locateExpr :: Expr -> Locator Expr
locateExpr expression tokens = case expression of
  ELocated _ inner -> locateExpr inner tokens
  EInteger value -> locateLeaf (TInteger value) expression tokens
  EFloat value -> locateLeaf (TFloat value) expression tokens
  EUnicode value -> locateLeaf (TUnicode value) expression tokens
  EText value -> locateLeaf (TText value) expression tokens
  EForeign -> locateLeaf TForeign expression tokens
  EVar name -> locateLeaf (TIdent name) expression tokens
  EApply function arguments ->
    let firstArgument :| remainingArguments = arguments
        (first2, rest) = locateExpr firstArgument tokens
        (function2, rest2) = locateExpr function rest
        (remaining2, rest3) = locateExprs remainingArguments rest2
        node = EApply function2 (first2 :| remaining2)
     in (locateAround (first2 : function2 : remaining2) node, rest3)
  ERecord fields ->
    let (fields2, rest) = locateFields fields tokens
     in (locateAround (map snd fields2) (ERecord fields2), rest)
  EField base name ->
    let (base2, rest) = locateExpr base tokens
     in (locateAround [base2] (EField base2 name), rest)
  EUpdate base updates ->
    let (base2, rest) = locateExpr base (afterToken TEquals tokens)
        (updates2, rest2) = locateUpdates updates rest
        children = base2 : [value | RecordSet _ value <- updates2]
     in (locateAround children (EUpdate base2 updates2), rest2)
  ETry body returned handlers ->
    let (body2, rest) = locateExpr body (afterToken TTry tokens)
        (returned2, rest2) = locateReturn returned rest
        (handlers2, rest3) = locateHandlers handlers rest2
        children = body2 : [result | ReturnCase _ result <- maybeToList returned2] ++ [result | HandlerCase _ _ result <- handlers2]
     in (locateAround children (ETry body2 returned2 handlers2), rest3)
  EMatch scrutinees cases ->
    let aligned = if null scrutinees then tokens else afterToken TMatch tokens
        (scrutinees2, rest) = locateExprs scrutinees aligned
        (cases2, rest2) = locateCases cases rest
        children = scrutinees2 ++ [result | MatchCase _ result <- cases2]
     in (locateAround children (EMatch scrutinees2 cases2), rest2)
  EBlock declarations body ->
    let (declarations2, rest) = locateDecls declarations tokens
        (body2, rest2) = locateExpr body rest
     in (locateAround [body2] (EBlock declarations2 body2), rest2)
  EWithEvidence function evidence ->
    let (function2, rest) = locateExpr function tokens
     in (locateAround [function2] (EWithEvidence function2 evidence), rest)

locateExprs :: [Expr] -> Locator [Expr]
locateExprs [] tokens = ([], tokens)
locateExprs (expression : expressions) tokens =
  let (expression2, rest) = locateExpr expression tokens
      (expressions2, rest2) = locateExprs expressions rest
   in (expression2 : expressions2, rest2)

locateFields :: [(String, Expr)] -> Locator [(String, Expr)]
locateFields [] tokens = ([], tokens)
locateFields ((name, value) : fields) tokens =
  let (value2, rest) = locateExpr value (afterToken TEquals tokens)
      (fields2, rest2) = locateFields fields rest
   in ((name, value2) : fields2, rest2)

locateUpdates :: [RecordUpdate] -> Locator [RecordUpdate]
locateUpdates [] tokens = ([], tokens)
locateUpdates (update : updates) tokens = case update of
  RecordSet name value ->
    let (value2, rest) = locateExpr value (afterToken TEquals tokens)
        (updates2, rest2) = locateUpdates updates rest
     in (RecordSet name value2 : updates2, rest2)
  RecordRemove name ->
    let (updates2, rest) = locateUpdates updates (afterToken (TIdent name) tokens)
     in (RecordRemove name : updates2, rest)

locateReturn :: Maybe ReturnCase -> Locator (Maybe ReturnCase)
locateReturn Nothing tokens = (Nothing, tokens)
locateReturn (Just (ReturnCase pattern body)) tokens =
  let (body2, rest) = locateExpr body (afterToken TMapsTo tokens)
   in (Just (ReturnCase pattern body2), rest)

locateHandlers :: [HandlerCase] -> Locator [HandlerCase]
locateHandlers [] tokens = ([], tokens)
locateHandlers (HandlerCase name patterns body : handlers) tokens =
  let (body2, rest) = locateExpr body (afterToken TMapsTo tokens)
      (handlers2, rest2) = locateHandlers handlers rest
   in (HandlerCase name patterns body2 : handlers2, rest2)

locateCases :: [MatchCase] -> Locator [MatchCase]
locateCases [] tokens = ([], tokens)
locateCases (MatchCase patterns body : cases) tokens =
  let (body2, rest) = locateExpr body (afterToken TMapsTo tokens)
      (cases2, rest2) = locateCases cases rest
   in (MatchCase patterns body2 : cases2, rest2)

locateLeaf :: Token -> Expr -> Locator Expr
locateLeaf wanted expression tokens = case break ((== wanted) . locatedToken) tokens of
  (_, LocatedToken span _ : rest) -> (ELocated span expression, rest)
  _ -> (expression, tokens)

locateAround :: [Expr] -> Expr -> Expr
locateAround children expression = maybe expression (`ELocated` expression) (coverSpans (mapMaybe exprSpan children))

exprSpan :: Expr -> Maybe SourceSpan
exprSpan (ELocated span _) = Just span
exprSpan _ = Nothing

coverSpans :: [SourceSpan] -> Maybe SourceSpan
coverSpans [] = Nothing
coverSpans spans = Just (SourceSpan (minimum (map spanStart spans)) (maximum (map spanEnd spans)))

seekToken :: Token -> [LocatedToken] -> (SourceSpan, [LocatedToken])
seekToken wanted tokens = case break ((== wanted) . locatedToken) tokens of
  (_, LocatedToken span _ : rest) -> (span, rest)
  _ -> (SourceSpan 0 0, tokens)

afterToken :: Token -> [LocatedToken] -> [LocatedToken]
afterToken token = snd . seekToken token

dropLocatedSemicolon :: [LocatedToken] -> [LocatedToken]
dropLocatedSemicolon (LocatedToken _ TSemicolon : rest) = rest
dropLocatedSemicolon tokens = tokens

startsLetDeclaration :: [LocatedToken] -> Bool
startsLetDeclaration (LocatedToken _ token : _) = token `elem` [TLet, TShow, TGraith]
startsLetDeclaration [] = False

afterBraceBlock :: [LocatedToken] -> [LocatedToken]
afterBraceBlock tokens = case dropWhile ((/= TLBrace) . locatedToken) tokens of
  [] -> tokens
  _ : body -> go 1 body
 where
  go :: Int -> [LocatedToken] -> [LocatedToken]
  go _ [] = []
  go depth (token : rest) = case locatedToken token of
    TLBrace -> go (depth + 1) rest
    TRBrace
      | depth == 1 -> rest
      | otherwise -> go (depth - 1) rest
    _ -> go depth rest
