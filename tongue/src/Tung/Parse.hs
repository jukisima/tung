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
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isNothing, mapMaybe)
import Tung.Name (checkImportAlias, isQualifiedName)
import Tung.Syntax
import Tung.Token

data FunctionResult = FunctionResult
  { functionResultType :: TypeExpr
  , functionResultEffects :: [TypeExpr]
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
  pure (ParsedSource program tokens)

parseTokens :: [Token] -> Either String Program
parseTokens toks = do
  (decls, rest) <- parseDecls toks
  case rest of
    [] -> pure (Program decls)
    TYield : expressionTokens -> do
      expression <- parseWhole "unexpected tokens after final expression" parseExpr expressionTokens
      pure (Program (decls ++ [Let "_" Nothing expression]))
    _ -> Left "unexpected tokens after program"

-- file declarations are delimited by their declaration heads or 'yield'.
parseDecls :: P [Decl]
parseDecls [] = pure ([], [])
parseDecls ts@(TRBrace : _) = pure ([], ts)
parseDecls ts@(TYield : _) = pure ([], ts)
parseDecls ts = do
  (newDecls, rest) <- parseDeclGroup ts
  (ds, rest2) <- parseDecls rest
  pure (newDecls ++ ds, rest2)

parseDeclGroup :: P [Decl]
parseDeclGroup = \case
  TShow : rest -> parseExportGroup rest
  TShowIlk : rest -> parseTypeReExportGroup rest
  ts -> do
    (d, rest) <- parseDecl ts
    pure ([d], rest)

parseExportGroup :: P [Decl]
parseExportGroup (TGraith : _) = Left "show must follow graiþ requirements"
parseExportGroup ts = case parseReExportNames ts of
  Right (names, rest) -> pure (map ReExport names, rest)
  Left _ -> do
    (decl, rest) <- parseDecl ts
    case decl of
      Import _ _ -> Left "show cannot precede bring"
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
    TIdent name : rest -> pure (reverse (name : names), rest)
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
  _ -> Left "expected declaration"

parseGraithDecl :: P Decl
parseGraithDecl ts = do
  (needs, rest) <- parseGraithPrefix isGraithEnd "expected 'let', 'shape' or 'fill' after graiþ" ts
  case rest of
    TLet : rest2 -> parseLetDecl needs rest2
    TShape : rest2 -> parseShape needs rest2
    TFill : rest2 -> parseFill needs rest2
    TShow : TLet : rest2 -> exportParsed (parseLetDecl needs rest2)
    TShow : TShape : rest2 -> exportParsed (parseShape needs rest2)
    TShow : TFill : rest2 -> exportParsed (parseFill needs rest2)
    _ -> Left "expected 'let', 'shape' or 'fill' after graiþ"

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
  (needs, rest) <- parseGraithPrefix (\case TLet -> True; _ -> False) "expected 'let' after graiþ" ts
  case rest of
    TLet : rest2 -> parseLetDecl needs rest2
    _ -> Left "expected 'let' after graiþ"

parseGraithPrefix :: (Token -> Bool) -> String -> [Token] -> Either String ([ShapeNeed], [Token])
parseGraithPrefix stop message ts = do
  (graithTokens, rest) <- takeTopLevelUntil ts stop message
  needs <- parseShapeNeedsWhole graithTokens
  pure (needs, rest)

-- let parsing accepteþ named and positional headers, typed parameter groups, and
-- graiþ prefixes, then lowers every parameter list to an anonymous match.
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
    TIdent alias : remaining -> do
      checkImportAlias alias
      pure (Import (intercalate "." parts) (Just alias), remaining)
    _ -> pure (Import (intercalate "." parts) Nothing, rest)

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
      (target, remaining) <- parseTypeUntilFileBoundary body
      pure (TypeAlias params name target, remaining)
    _ -> Left "expected '=' after let-ilk header"

parseTypeUntilFileBoundary :: P TypeExpr
parseTypeUntilFileBoundary ts =
  parseTypeUntilTopLevelOrEnd ts isBoundary "unterminated type"
 where
  isBoundary TYield = True
  isBoundary token = startsFileDeclaration token

startsFileDeclaration :: Token -> Bool
startsFileDeclaration = \case
  TBring -> True
  TGraith -> True
  TLet -> True
  TLetIlk -> True
  TKin -> True
  TDeed -> True
  TShape -> True
  TFill -> True
  TShow -> True
  TShowIlk -> True
  _ -> False

parseLet :: [ShapeNeed] -> String -> P Decl
parseLet needs name = \case
  TEquals : rest -> parseUntypedLet needs name [] rest
  TColon : rest -> parseColonLet needs name rest
  ts -> parseLetHeader needs name ts

parseColonLet :: [ShapeNeed] -> String -> P Decl
parseColonLet needs name ts =
  case parseTypeAnnUntilEquals ts of
    Right (ann, TEquals : rest) -> do
      (e, rest2) <- parseExpr rest
      pure (Let name (Just (withGraithNeeds needs ann)) e, rest2)
    _ -> Left "expected '=' after let type"

parseLetHeader :: [ShapeNeed] -> String -> P Decl
parseLetHeader needs name ts =
  case parseLetHeaderDefinitionWithNeeds needs (TIdent name : ts) of
    Just result -> result
    Nothing -> Left "expected '=' in let declaration"

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
        (argTy, rest2) <- parseTypeUntilCommaOrParen "unterminated parameter type" typeTokens
        go (pat : params) (Just argTy : tys) rest2
      TComma : _ -> go (pat : params) (Nothing : tys) rest
      TRParen : _ -> go (pat : params) (Nothing : tys) rest
      _ -> Left "expected ':', ',' or ')' in typed let argument"

parseTypedParamPatternTokens :: [Token] -> Either String ([Token], [Token])
parseTypedParamPatternTokens ts = takeTopLevelUntil ts (\case TColon -> True; TComma -> True; TRParen -> True; _ -> False) "unterminated typed argument"

functionLetAnn :: [ShapeNeed] -> [Maybe TypeExpr] -> FunctionResult -> Either String TypeAnn
functionLetAnn needs [] FunctionResult{functionResultType = result, functionResultEffects = []} = Right (TypeAnn result needs)
functionLetAnn _ [] _ = Left "effect annotation requireþ function arguments"
functionLetAnn needs argTypes FunctionResult{..} =
  TypeAnn <$> makeArrowType (fillMissingTypes argTypes) functionResultEffects functionResultType <*> pure needs

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
  (paramTerms, name) <- maybeToEither (kind ++ " declaration requireþ a name") (headerFromTokens header)
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
  (paramTerms, name) <- maybeToEither "shape declaration requireþ a name" (headerFromTokens header)
  params <- maybeToEither "shape parameters must be names" (namesFromHeaderTerms paramTerms)
  (members, rest) <- parseShapeMembers body
  case rest of
    TRBrace : following -> pure (ShapeDecl params name needs members, following)
    _ -> Left "expected '}' after shape body"

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
parseShapeNeedsWhole tokens = parseWhole "unexpected tokens after graiþ" parseShapeNeeds tokens

parseShapeNeed :: P ShapeNeed
parseShapeNeed [] = Left "empty graiþ"
parseShapeNeed ts = case headerFromTokens ts of
  Nothing -> Left "invalid graiþ"
  Just (argTerms, name) -> case typesFromHeaderTerms argTerms of
    Nothing -> Left "graiþ arguments must be types"
    Just args -> Right (ShapeNeed args name, [])

parseFill :: [ShapeNeed] -> P Decl
parseFill needs ts = do
  (header, body) <- parseHeaderBody "fill" ts
  (tyTerms, shapeName) <- maybeToEither "fill declaration requireþ a shape name" (headerFromTokens header)
  tyArgs <- maybeToEither "fill type arguments must be types" (typesFromHeaderTerms tyTerms)
  case tyArgs of
    [] -> Left "fill declaration requireþ at least one type argument"
    _ -> pure ()
  (ds, rest) <- parseLocalDecls body
  case rest of
    TRBrace : rest2 -> pure (FillDecl tyArgs shapeName needs ds, rest2)
    _ -> Left "expected '}' after fill body"

parseLocalDecl :: P Decl
parseLocalDecl = \case
  TGraith : rest -> parseGraithLet rest
  TLet : rest -> parseLetDecl [] rest
  _ -> Left "expected local let"

parseLocalDecls :: P [Decl]
parseLocalDecls ts@(TLet : _) = parseNextLocalDecl ts
parseLocalDecls ts@(TGraith : _) = parseNextLocalDecl ts
parseLocalDecls ts = Right ([], ts)

parseNextLocalDecl :: P [Decl]
parseNextLocalDecl ts = do
  (declaration, rest) <- parseLocalDecl ts
  (declarations, remaining) <- parseLocalDecls rest
  pure (declaration : declarations, remaining)

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
parseShapeMembers ts@(TRBrace : _) = Right ([], ts)
parseShapeMembers ts = do
  (member, rest) <- parseShapeMember ts
  (members, remaining) <- parseShapeMembers rest
  pure (member : members, remaining)

parseShapeMember :: P ShapeMember
parseShapeMember (TGraith : ts) = do
  (needs, rest) <- parseGraithPrefix (\case TLet -> True; _ -> False) "expected 'let' after shape member graiþ" ts
  case rest of
    TLet : _ -> parseShapeLet needs rest
    _ -> Left "expected 'let' after shape member graiþ"
parseShapeMember ts@(TLet : _) = parseShapeLet [] ts
parseShapeMember ts@(TLaw : _) = parseShapeLaw ts
parseShapeMember _ = Left "expected shape member"

parseShapeLet :: [ShapeNeed] -> P ShapeMember
parseShapeLet needs (TLet : ts) = do
  (memberTokens, rest) <- takeTopLevelUntilOrEnd ts isShapeMemberBoundary
  member <- parseShapeSignature needs memberTokens
  pure (member, rest)
parseShapeLet _ _ = Left "expected 'let' before shape member"

parseShapeSignature :: [ShapeNeed] -> [Token] -> Either String ShapeMember
parseShapeSignature needs ts = case splitTopLevelColon ts of
  Nothing -> Left "expected ':' in required shape member"
  Just (header, resultTokens) -> do
    (argTypes, name) <- maybe (Left "expected shape member name") Right (shapeMemberHeader header)
    result <- parseFunctionResultTokens resultTokens
    ann <- functionLetAnn needs (map Just argTypes) result
    pure (ShapeSpec name ann)

isShapeMemberBoundary :: Token -> Bool
isShapeMemberBoundary = \case
  TLet -> True
  TGraith -> True
  TLaw -> True
  TRBrace -> True
  _ -> False

shapeMemberHeader :: [Token] -> Maybe ([TypeExpr], String)
shapeMemberHeader ts = do
  (argTerms, name) <- headerFromTokens ts
  argTypes <- typesFromHeaderTerms argTerms
  pure (argTypes, name)

parseShapeLaw :: P ShapeMember
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
      pure (ShapeLaw parameters left right, rest2)
    _ -> Left "expected ':' after law parameters"
 where
  lawParameter (PVar name, Just annotation) = Right (name, annotation)
  lawParameter (_, Nothing) = Left "law parameters require type annotations"
  lawParameter _ = Left "law parameters must be names"
  isLawSeparator (TIdent "~") = True
  isLawSeparator _ = False
  lawEnd = isShapeMemberBoundary
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
applyArgs = foldl' (\expr arg -> locateAround [expr, arg] (EApply expr (arg :| [])))

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
exprStop token = startsFileDeclaration token || token `elem` [TComma, TDot, TMapsTo, TRParen, TRBrace, TYield, TLaw]

parseExprAtom :: P Expr
parseExprAtom = locateParsed parseExprAtomRaw

parseExprAtomRaw :: P Expr
parseExprAtomRaw = \case
  TInteger i : rest -> Right (EInteger i, rest)
  TFloat s : rest -> Right (EFloat s, rest)
  TUnicode codePoint : rest -> Right (EUnicode codePoint, rest)
  TText key : TForeign : rest -> Right (EForeign key, rest)
  TText s : rest -> Right (EText s, rest)
  TForeign : _ -> Left "fremmed requireþ a preceding text key"
  TParenKeyword marker : rest -> parseParenKeywordExpr marker rest
  TIdent "r" : TLParen : _ -> Left "record opener must be written 'r(' without whitespace"
  TIdent s : rest -> Right (EVar s, rest)
  TTry : rest -> parseTry rest
  TMatch : rest -> parseMatchExpr rest
  TLBrace : rest -> parseAnonymousMatchExpr rest
  TLParen : rest -> parseParen rest
  _ -> Left "expected expression atom"

parseAnonymousMatchExpr :: P Expr
parseAnonymousMatchExpr ts = do
  (cs, rest) <- parseMatchCases ts
  pure (EMatch [] cs, rest)

parseMatchExpr :: P Expr
parseMatchExpr (TLBrace : _) = Left "match requireþ a scrutinee; use '{ ... }' for a function"
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
parseParen ts@(TLet : _) = parseBlock ts
parseParen ts@(TGraith : _) = parseBlock ts
parseParen ts = do
  (e, rest) <- parseExpr ts
  case rest of
    TRParen : rest2 -> Right (e, rest2)
    TColon : rest2 -> do
      (typeTokens, rest3) <- takeTopLevelUntil rest2 (\case TRParen -> True; _ -> False) "expected ')' after type ascription"
      annotation <- parseWhole "could not parse type ascription" parseTypeTokens typeTokens
      case rest3 of
        TRParen : rest4 -> Right (EAscribe e annotation, rest4)
        _ -> Left "expected ')' after type ascription"
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
  collect (Just _, _) (HandlerReturnItem _) = Left "handler hath more than one yield case"
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
returnCaseHeader (TYield : rest) = case parsePattern rest of
  Right (pat, []) -> Just pat
  _ -> Nothing
returnCaseHeader _ = Nothing

parseBlock :: P Expr
parseBlock ts = do
  (ds, rest) <- parseLocalDecls ts
  case rest of
    TYield : expressionTokens -> do
      (e, rest2) <- parseExpr expressionTokens
      case rest2 of
        TRParen : rest3 -> pure (EBlock ds e, rest3)
        _ -> Left "expected ')' after block result"
    _ -> Left "expected 'yield' before block result"

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
    TRParen : rest2 -> pure (EUpdate base [], rest2)
    _ -> Left "expected ',' or ')' after record update base"

parseRecordUpdates :: P [RecordUpdate]
parseRecordUpdates = parseCommaListUntil isRightParen parseRecordUpdateItem

parseRecordUpdateItem :: P RecordUpdate
parseRecordUpdateItem (TIdent "-" : TIdent name : rest) = Right (RecordRemove name, rest)
parseRecordUpdateItem (TIdent name : TEquals : rest) = do
  (e, rest2) <- parseExpr rest
  pure (RecordSet name e, rest2)
parseRecordUpdateItem _ = Left "expected record update"

parseRecordExprFields :: P [(String, Expr)]
parseRecordExprFields = parseCommaListUntil isRightParen parseRecordExprField

parseRecordExprField :: P (String, Expr)
parseRecordExprField (TIdent name : TEquals : rest) = do
  (e, rest2) <- parseExpr rest
  pure ((name, e), rest2)
parseRecordExprField _ = Left "expected record field"

parseParenKeywordExpr :: String -> P Expr
parseParenKeywordExpr "r" = parseRecordExpr
parseParenKeywordExpr marker = parseAssociativeExpr marker

parseAssociativeExpr :: String -> P Expr
parseAssociativeExpr marker ts = do
  (expressions, rest) <- parseCommaListUntil isRightParen parseExpr ts
  case expressions of
    [] -> Left (marker ++ "(...) requireþ a combining function")
    function : values@(_ : _ : _) -> pure (associate function values, rest)
    _ -> Left (marker ++ "(...) requireþ at least two values")
 where
  associate function = direction (applyBinary function)
  direction = if marker == "<" then foldl1 else foldr1

applyBinary :: Expr -> Expr -> Expr -> Expr
applyBinary function left right = applyArgs function [left, right]

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
  Nothing -> Left "constructor pattern requireþ a name"
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

parseTypeUntilCommaOrParen :: String -> P TypeExpr
parseTypeUntilCommaOrParen message ts = parseTypeUntilTopLevel ts (\case TComma -> True; TRParen -> True; _ -> False) message

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

parseFunctionResultUntilCommaOrBrace, parseFunctionResultUntilEquals :: P FunctionResult
parseFunctionResultUntilCommaOrBrace = collect []
 where
  collect seen ts = do
    (part, rest) <- takeTopLevelUntil ts (\case TComma -> True; TRBrace -> True; _ -> False) "unterminated type"
    let resultTokens = seen ++ part
    case rest of
      TRBrace : _ -> finishFunctionResult resultTokens rest
      TComma : following
        | startsEffectOperation following || startsRightBrace following -> finishFunctionResult resultTokens rest
        | otherwise -> collect (resultTokens ++ [TComma]) following
      _ -> Left "unterminated type"

  startsEffectOperation ts = case takeTopLevelUntil ts (\case TColon -> True; TComma -> True; TRBrace -> True; _ -> False) "expected effect operation" of
    Right (header, TColon : _) -> case shapeMemberHeader header of
      Just _ -> True
      Nothing -> False
    _ -> False

  startsRightBrace (TRBrace : _) = True
  startsRightBrace _ = False
parseFunctionResultUntilEquals ts = do
  (seen, rest) <- takeTopLevelUntil ts (\case TEquals -> True; _ -> False) "expected '=' after type"
  finishFunctionResult seen rest

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

parseFunctionResultTokens :: [Token] -> Either String FunctionResult
parseFunctionResultTokens tokens = fst <$> finishFunctionResult tokens []

finishFunctionResult :: [Token] -> [Token] -> Either String (FunctionResult, [Token])
finishFunctionResult tokens rest =
  case parseTypeTokens tokens of
    Right (t, []) -> Right (FunctionResult t [], rest)
    _ -> case splitTopLevelBang tokens of
      Nothing -> Left "could not parse whole function result type"
      Just (retTokens, effectTokens) -> do
        ret <- parseWhole "unexpected tokens before effects" parseTypeTokens retTokens
        effects <- parseWhole "unexpected tokens after effects" parseCommaTypes effectTokens
        Right (FunctionResult ret effects, rest)

-- arrows are special syntax with non-empty domains. type application otherwise
-- followeþ the same second-is-function rule as term application.
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
  Nothing -> do
    ret <- parseWhole "unexpected tokens in arrow result" parseArrowType ts
    pure (([], ret), [])

makeArrowType :: [TypeExpr] -> [TypeExpr] -> TypeExpr -> Either String TypeExpr
makeArrowType args effects ret = do
  domain <- maybe (Left "function type requireþ at least one argument") Right (NE.nonEmpty args)
  pure $ case (effects, ret) of
    ([], TypeArrow more moreEffects finalRet) -> TypeArrow (domain <> more) moreEffects finalRet
    _ -> TypeArrow domain effects ret

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
  TParenKeyword "r" : rest -> parseRecordType rest
  TIdent "r" : TLParen : _ -> Left "record opener must be written 'r(' without whitespace"
  TIdent s : rest -> Right (TypeName s, rest)
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
parseRecordTypeFields = parseCommaListUntil isRightParen parseRecordTypeField

parseRecordTypeField :: P (String, TypeExpr)
parseRecordTypeField (TIdent name : TColon : rest) = do
  (t, rest2) <- parseTypeUntilCommaOrParen "unterminated record type" rest
  pure ((name, t), rest2)
parseRecordTypeField _ = Left "expected record field type"

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

dropComma :: [Token] -> [Token]
dropComma (TComma : rest) = rest
dropComma ts = ts

isRightBrace, isRightParen :: Token -> Bool
isRightBrace = (== TRBrace)
isRightParen = (== TRParen)

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
        _ | isParenthesisOpen x -> go (x : acc) (depth + 1) rest
        TRParen -> go (x : acc) (depth - 1) rest
        _ -> go (x : acc) depth rest

-- delimiter-aware slicing keepeþ declaration and type parsers small. callers
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
        _ | isParenthesisOpen x -> go (x : acc) (depth + 1) rest stop
        TRParen -> go (x : acc) (depth - 1) rest stop
        TLBrace -> go (x : acc) (depth + 1) rest stop
        TRBrace -> go (x : acc) (depth - 1) rest stop
        _ -> go (x : acc) depth rest stop

takeBalanced :: [Token] -> Either String ([Token], [Token])
takeBalanced = go [] (1 :: Int)
 where
  go _ _ [] = Left "unclosed parenthesised type"
  go acc depth (x : rest)
    | isParenthesisOpen x = go (x : acc) (depth + 1) rest
  go acc 1 (TRParen : rest) = Right (reverse acc, rest)
  go acc depth (TRParen : rest) = go (TRParen : acc) (depth - 1) rest
  go acc depth (x : rest) = go (x : acc) depth rest

isParenthesisOpen :: Token -> Bool
isParenthesisOpen = \case
  TLParen -> True
  TParenKeyword _ -> True
  _ -> False

takeHeaderTokens :: String -> [Token] -> Either String ([Token], [Token])
takeHeaderTokens kind ts = takeTopLevelUntil ts (\case TLBrace -> True; _ -> False) ("expected '{' after " ++ kind ++ " header")

takeCtorTokens :: [Token] -> Either String ([Token], [Token])
takeCtorTokens ts = case break (\token -> token == TComma || token == TRBrace) ts of
  (_, []) -> Left "unterminated kin declaration"
  result -> Right result

-- located parser tokens decorate atoms as they are consumed; application spans
-- then grow from their already located children.
locateParsed :: P Expr -> P Expr
locateParsed parser tokens = do
  (expression, rest) <- parser tokens
  case expression of
    -- fremmed is a declaration marker rather than an ordinary expression; its
    -- direct-body shape must remain visible to validation and elaboration.
    EForeign _ -> pure (expression, rest)
    _ -> do
      let consumed = take (length tokens - length rest) tokens
          spans = mapMaybe tokenSpan consumed
      pure (maybe expression (`ELocated` expression) (coverSpans spans), rest)

locateAround :: [Expr] -> Expr -> Expr
locateAround children expression = maybe expression (`ELocated` expression) (coverSpans (mapMaybe exprSpan children))

exprSpan :: Expr -> Maybe SourceSpan
exprSpan (ELocated span _) = Just span
exprSpan _ = Nothing

coverSpans :: [SourceSpan] -> Maybe SourceSpan
coverSpans [] = Nothing
coverSpans spans = Just (SourceSpan (minimum (map spanStart spans)) (maximum (map spanEnd spans)))
