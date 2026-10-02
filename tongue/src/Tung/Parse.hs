{-# LANGUAGE PatternSynonyms #-}

-- | token parser for surface syntax. this stage lowers definition headers,
-- second-is-function sequences, and dollar grouping without consulting types.
module Tung.Parse
  ( ParsedSource (..),
    SourceUnit (..),
    ParsedBundle (..),
    prepareSource,
    prepareBundle,
    parse,
    parseDetailed,
    parseTokens,
    SourceRole (..),
    parseSourceRoles,
  )
where

import Control.Monad (foldM, unless, when)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Tung.Name (checkImportAlias, isQualifiedName)
import Tung.Primitive (primitiveTypeNames)
import Tung.Syntax
import Tung.Token

data FunctionResult = FunctionResult
  { functionResultType :: TypeExpr,
    functionResultEffects :: [TypeExpr]
  }
  deriving (Eq, Show)

data HeaderSlot = BoundSlot Pattern TypeExpr | BareSlot TypeExpr

-- parsing emitteþ source roles alongside its value. diagnostics remain strict;
-- editor clients can retain the roles preceding an incomplete expression.
data SourceRole = SourceRole SourceSpan String [String] deriving (Eq, Show)

data Result a = Result (Either SourceFailure a) (Seq SourceRole)

instance Functor Result where
  fmap f (Result value roles) = Result (fmap f value) roles

instance Applicative Result where
  pure value = Result (Right value) Seq.empty
  function <*> value = do
    f <- function
    x <- value
    pure (f x)

instance Monad Result where
  Result (Left failure) roles >>= _ = Result (Left failure) roles
  Result (Right value) roles >>= next = retain roles (next value)

retain :: Seq SourceRole -> Result a -> Result a
retain roles (Result value more) = Result value (roles <> more)

resultEither :: Result a -> Either SourceFailure a
resultEither (Result value _) = value

pattern Parsed :: a -> Result a
pattern Parsed value <- Result (Right value) _
  where
    Parsed value = pure value

pattern Failed :: String -> Result a
pattern Failed message <- Result (Left (SourceFailure _ message)) _
  where
    Failed message = Result (Left (SourceFailure Nothing message)) Seq.empty

{-# COMPLETE Failed, Parsed #-}

-- parsers consume a token prefix and return the untouched suffix. whole-input
-- helpers are used only where the surrounding delimiter hath already been found.
type P a = [Token] -> Result (a, [Token])

data ParsedSource = ParsedSource
  { parsedProgram :: Program,
    parsedTokens :: [LocatedToken]
  }
  deriving (Eq, Show)

-- a source unit shareþ its syntax and its failure across all compiler clients.
data SourceUnit = SourceUnit
  { unitSource :: String,
    unitParsed :: Either SourceFailure ParsedSource
  }
  deriving (Eq, Show)

data ParsedBundle = ParsedBundle
  { bundleRoot :: SourceUnit,
    bundleImports :: Map.Map String SourceUnit
  }
  deriving (Eq, Show)

prepareSource :: String -> SourceUnit
prepareSource source = SourceUnit source (parseDetailed source)

prepareBundle :: String -> Map.Map String String -> ParsedBundle
prepareBundle source imports = ParsedBundle (prepareSource source) (Map.map prepareSource imports)

-- public text APIs retain their messages; structured clients retain spans.
parse :: String -> Either String Program
parse source = lexTokens source >>= parseTokens

parseDetailed :: String -> Either SourceFailure ParsedSource
parseDetailed source = do
  tokens <- lexLocatedTokensDetailed source
  program <- resultEither (parseTokenStream (map locatedToken tokens))
  pure (ParsedSource program tokens)

parseTokens :: [Token] -> Either String Program
parseTokens = either (Left . failureMessage) Right . resultEither . parseTokenStream

withPosition :: P a -> P a
withPosition parser tokens = locateFailure tokens (parser tokens)

locateFailure :: [Token] -> Result a -> Result a
locateFailure tokens (Result value roles) =
  Result
    ( case value of
        Left failure@SourceFailure {failureSpan = Nothing} -> Left failure {failureSpan = case tokens of token : _ -> tokenSpan token; [] -> Nothing}
        other -> other
    )
    roles

failedAt :: [Token] -> String -> Result a
failedAt tokens message = locateFailure tokens (Failed message)

parseTokenStream :: [Token] -> Result Program
parseTokenStream toks = do
  (decls, rest) <- parseDecls toks
  case rest of
    [] -> pure (Program decls)
    TYield : expressionTokens -> do
      expression <- parseWhole "unexpected tokens after final expression" parseExpr expressionTokens
      pure (Program (decls ++ [Let "_" Nothing expression]))
    _ -> failedAt rest "unexpected tokens after program"

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
parseDeclGroup = withPosition \case
  TExport : rest -> parseExportGroup rest
  TExportType : rest -> parseTypeReExportGroup rest
  ts -> do
    (d, rest) <- parseDecl ts
    pure ([d], rest)

parseExportGroup :: P [Decl]
parseExportGroup ts = case parseReExportNames ts of
  Parsed (names, rest) -> pure (map ReExport names, rest)
  Failed _ -> do
    (decl, rest) <- parseDecl ts
    case decl of
      Import _ _ -> Failed "show cannot precede use"
      Let "_" _ _ -> Failed "show must precede a declaration or name"
      _ -> pure ([Export decl], rest)

parseTypeReExportGroup :: P [Decl]
parseTypeReExportGroup ts = do
  (names, rest) <- parseReExportNames ts
  markNames "type" [] (consumedTokens ts rest)
  pure (map ReExportType names, rest)

parseReExportNames :: P [String]
parseReExportNames = go []
  where
    go names = \case
      TIdent name : TComma : rest -> go (name : names) rest
      TIdent name : rest -> pure (reverse (name : names), rest)
      _ -> Failed "expected re-exported name"

parseDecl :: P Decl
parseDecl = \case
  TImport : rest -> parseImport rest
  TLet : rest -> parseLetDecl rest
  TType : rest -> parseData rest
  TEffect : rest -> parseEffect rest
  TClass : rest -> parseClass rest
  TInstance : rest -> parseInstance rest
  _ -> Failed "expected declaration"

-- let parsing accepteþ named and positional bracketed headers, then
-- lowers every parameter list to an anonymous match.
parseLetDecl :: P Decl
parseLetDecl ts = do
  (header, rest) <- takeTopLevelUntil ts (== TLBracket) "let requireþ a bracketed header; use '[]' to infer its type"
  (patterns, name) <- functionHeader header
  case rest of
    TLBracket : after -> parseBracketLet name patterns after
    _ -> Failed "expected '[' after let header"

parseImport :: P Decl
parseImport ts = do
  (path, rest) <- parseImportPath ts
  traverse_ (markRole "import" []) (take 1 ts)
  case rest of
    token@(TIdent alias) : remaining -> do
      markRole "namespace" ["declaration"] token
      either (failedAt rest) pure (checkImportAlias alias)
      pure (Import path (Just alias), remaining)
    _ -> pure (Import path Nothing, rest)

parseImportPath :: P String
parseImportPath (TIdent first : rest) = go first rest
  where
    go path (TDot : TIdent segment : remaining) = go (path ++ "." ++ segment) remaining
    go path remaining = Parsed (path, remaining)
parseImportPath _ = Failed "expected use path"

parseTypeUntilFileBoundary :: P TypeExpr
parseTypeUntilFileBoundary ts =
  parseTypeUntilTopLevelOrEnd ts isBoundary "unterminated type"
  where
    isBoundary TYield = True
    isBoundary token = startsFileDeclaration token

startsFileDeclaration :: Token -> Bool
startsFileDeclaration = \case
  TImport -> True
  TLet -> True
  TType -> True
  TEffect -> True
  TClass -> True
  TInstance -> True
  TExport -> True
  TExportType -> True
  _ -> False

parseBracketLet :: Token -> [Pattern] -> P Decl
parseBracketLet name positional ts = do
  (header, closing) <- takeTopLevelUntil ts (== TRBracket) "expected ']' after let header"
  items <- headerItems (fst (splitBracketEffects header))
  case closing of
    TRBracket : TEquals : _ -> Failed "bracketed let header doth not use '='"
    TRBracket : body | not (null items) && last items == [TKindStar] -> parseAliasLet name positional header body
    TRBracket : body -> do
      (entries, slots, result, effects) <- parseLetBracket header
      let arguments = replicate (length positional) Nothing ++ map slotType slots
          patterns = positional ++ [pat | BoundSlot pat _ <- slots]
      annotation <- case result of
        Just ty -> Just <$> functionLetAnn entries arguments (FunctionResult ty effects)
        Nothing
          | null effects -> Parsed (implicitLetAnn entries arguments)
          | otherwise -> Failed "effect row requireþ a result type"
      unless (null entries || isJust annotation) (Failed "type parameters requireþ a type annotation")
      parseLetBody name annotation patterns body
    _ -> Failed "expected ']' after let header"
  where
    slotType (BoundSlot _ ty) = Just ty
    slotType (BareSlot ty) = Just ty

-- a star result selecteþ a type-level declaration before value-header parsing.
-- its named inputs are kinds, so no runtime arguments or effect row are created.
parseAliasLet :: Token -> [Pattern] -> [Token] -> P Decl
parseAliasLet token@(TIdent name) positional header body = do
  unless (null positional) (Failed "type alias requireþ a named header without value arguments or constraints")
  let (parameterTokens, effects) = splitBracketEffects header
  unless (isNothing effects) (Failed "type alias header cannot have effects")
  items <- splitBracketItems parameterTokens
  params <- traverse (parseKindedParameter "type alias" . dropOptionalAt) (init items)
  markRole "type" (["declaration"] ++ ["typeFunction" | not (null params)]) token
  traverse_ (markRole "keyword" []) (last items)
  (target, remaining) <- parseTypeUntilFileBoundary body
  pure (TypeAlias params name target, remaining)
  where
    dropOptionalAt (TAt : rest) = rest
    dropOptionalAt rest = rest
parseAliasLet _ _ _ _ = Failed "expected type alias name"

parseLetBracket :: [Token] -> Result ([TypeHeaderEntry], [HeaderSlot], Maybe TypeExpr, [TypeExpr])
parseLetBracket tokens = do
  let (slotTokens, effectTokens) = splitBracketEffects tokens
  items <- headerItems slotTokens
  (entries, argumentItems) <- parseHeaderPrefix items
  slots <- traverse parseHeaderSlot argumentItems
  effects <- maybe (Parsed []) (withEffect . parseBracketTypes) effectTokens
  let (arguments, result) = case reverse slots of
        BareSlot ty : rest -> (reverse rest, Just ty)
        _ -> (slots, Nothing)
  if hasBoundAfterBare arguments
    then Failed "named arguments must precede bare argument types"
    else Parsed (entries, arguments, result, effects)
  where
    hasBoundAfterBare = go False
    go _ [] = False
    go _ (BareSlot _ : rest) = go True rest
    go seenBare (BoundSlot _ _ : rest) = seenBare || go False rest

-- every header putteþ requirements before type parameters, then value slots.
-- kind checking later resolveþ requirements against imported class signatures.
parseHeaderPrefix :: [[Token]] -> Result ([TypeHeaderEntry], [[Token]])
parseHeaderPrefix items = do
  (requirements, rest) <- parseRequirementPrefix items
  let (prefix, remaining) = span startsTypeParameter rest
  parameters <- traverse (fmap Parameter . parseKindedParameter "polymorphic" . drop 1) prefix
  let names = parameterNames (headerParameters parameters)
  when (any startsTypeParameter remaining) (Failed "type parameters must precede value arguments")
  when (length names /= Set.size (Set.fromList names)) (Failed "type parameters must have distinct names")
  pure (requirements ++ parameters, remaining)

parseRequirementPrefix :: [[Token]] -> Result ([TypeHeaderEntry], [[Token]])
parseRequirementPrefix items = do
  let (prefix, remaining) = span startsRequirement items
  when (any startsRequirement remaining) (Failed "byzen constraints must precede type parameters and value arguments")
  requirements <- traverse requirement prefix
  pure (requirements, remaining)
  where
    startsRequirement (TConstraint : _) = True
    startsRequirement _ = False
    requirement (TConstraint : tokens) = Requirement <$> parseWhole "invalid class constraint" parseClassConstraint tokens
    requirement _ = Failed "expected byzen constraint"

headerItems :: [Token] -> Result [[Token]]
headerItems [] = pure []
headerItems tokens = splitBracketItems tokens

parseHeaderSlot :: [Token] -> Result HeaderSlot
parseHeaderSlot (TAt : _) = Failed "type parameters and constraints must precede arguments"
parseHeaderSlot tokens = case splitTopLevelColon tokens of
  Just (patternTokens, typeTokens) -> do
    pat <- plainBinders (parseHeaderPattern patternTokens)
    ty <- parseWhole "argument type is required after ':'" parseTypeTokens typeTokens
    pure (BoundSlot pat ty)
  Nothing -> BareSlot <$> parseWhole "invalid unnamed argument type" parseTypeTokens tokens

parseHeaderPattern :: [Token] -> Result Pattern
parseHeaderPattern tokens
  | Just [_, [TIdent constructor], _] <- headerParts tokens,
    all (not . isAlphaNum) constructor =
      parseWhole "invalid infix pattern" parsePattern tokens
parseHeaderPattern tokens = case tokens of
  [token@(TIdent name)] -> markRole "parameter" ["declaration"] token >> pure (PVar name)
  [TInteger value] -> Parsed (PInteger value)
  [TText value] -> Parsed (PText value)
  token@(TIdent constructor) : rest -> markRole "enumMember" [] token >> (PCon constructor <$> parseHeaderPatternArguments rest)
  TLParen : rest -> do
    (inside, following) <- takeBalanced rest
    if null following
      then parseWhole "invalid grouped pattern" parsePattern inside
      else Failed "unexpected tokens after grouped pattern"
  _ -> Failed "invalid argument pattern"

parseHeaderPatternArguments :: [Token] -> Result [Pattern]
parseHeaderPatternArguments [] = Failed "constructor pattern requireþ an argument"
parseHeaderPatternArguments tokens = do
  (term, rest) <- maybe (Failed "invalid constructor pattern") Parsed (readHeaderTerm tokens)
  argument <- case term of
    TLParen : inside -> case reverse inside of
      TRParen : reversed -> parseHeaderPattern (reverse reversed)
      _ -> Failed "invalid grouped pattern"
    _ -> parseHeaderPattern term
  if null rest then Parsed [argument] else (argument :) <$> parseHeaderPatternArguments rest

parseLetBody :: Token -> Maybe TypeAnn -> [Pattern] -> P Decl
parseLetBody token@(TIdent name) ann params ts = do
  let callable = not (null params) || maybe False (functionType . (\(TypeAnn ty _) -> ty)) ann
  markRole (if callable then "function" else "variable") (["declaration"] ++ ["applied" | callable]) token
  (e, rest) <- parseExpr ts
  when (not callable && isAnonymousMatchExpr e) (markRole "function" ["declaration", "applied"] token)
  pure (Let name ann (lambdaIfParams params e), rest)
parseLetBody _ _ _ _ = Failed "expected let name"

functionType :: TypeExpr -> Bool
functionType TypeArrow {} = True
functionType (TypeForall _ body) = functionType body
functionType _ = False

functionLetAnn :: [TypeHeaderEntry] -> [Maybe TypeExpr] -> FunctionResult -> Result TypeAnn
functionLetAnn constraints [] FunctionResult {functionResultType = result, functionResultEffects = []} = Parsed (TypeAnn result constraints)
functionLetAnn _ [] _ = Failed "effect annotation requireþ function arguments"
functionLetAnn constraints argTypes FunctionResult {..} =
  TypeAnn <$> makeArrowType (instanceMissingTypes argTypes) functionResultEffects functionResultType <*> pure constraints

implicitLetAnn :: [TypeHeaderEntry] -> [Maybe TypeExpr] -> Maybe TypeAnn
implicitLetAnn [] [] = Nothing
implicitLetAnn [] argTypes | all isNothing argTypes = Nothing
implicitLetAnn constraints argTypes = Just (TypeAnn ty constraints)
  where
    ty = case argTypes of
      [] -> implicitTypeAt 0
      _ -> fromRight (implicitTypeAt (length argTypes)) (resultEither (makeArrowType (instanceMissingTypes argTypes) [] (implicitTypeAt (length argTypes))))

instanceMissingTypes :: [Maybe TypeExpr] -> [TypeExpr]
instanceMissingTypes = zipWith instance_ [0 :: Int ..]
  where
    instance_ _ (Just t) = t
    instance_ n Nothing = implicitTypeAt n

implicitTypeAt :: Int -> TypeExpr
implicitTypeAt = TypeHole

parseData :: P Decl
parseData ts = do
  (header, rest) <- takeTopLevelUntil ts (== TLBracket) "expected bracketed ilk header"
  (contents, closing) <- case rest of
    TLBracket : after -> takeTopLevelUntil after (== TRBracket) "expected ']' after ilk header"
    _ -> Failed "expected bracketed ilk header"
  body <- case closing of
    TRBracket : TLBrace : after -> pure after
    _ -> Failed "expected '{' after ilk header"
  let fullHeader = header ++ [TLBracket] ++ contents ++ [TRBracket]
  (params, name) <- parseDataHeader fullHeader
  markHeaderName "type" (["declaration"] ++ ["typeFunction" | not (null params)]) header
  (ctors, rest) <- parseCtors body
  pure (DataDecl params name ctors, rest)

parseDataHeader :: [Token] -> Result ([TypeParameter], String)
parseDataHeader (TIdent name : TLBracket : rest) = do
  (contents, closing) <- takeTopLevelUntil rest (== TRBracket) "expected ']' after ilk parameters"
  items <- splitBracketItems contents
  unless (last items == [TKindStar]) (Failed "ilk header requireþ a final *")
  traverse_ (markRole "keyword" []) (last items)
  params <- traverse (parseKindedParameter "ilk") (init items)
  case closing of
    [TRBracket] -> Parsed (params, name)
    _ -> Failed "unexpected tokens after ilk parameters"
parseDataHeader _ = Failed "ilk requireþ a name and kinded bracketed header"

parseKindedParameter :: String -> [Token] -> Result (String, KindExpr)
parseKindedParameter _ (token@(TIdent name) : TColon : kind)
  | name /= "_" && not (isQualifiedName name) && name `notElem` primitiveTypeNames = do
      markRole "type" ["declaration"] token
      parsed <- parseKind kind
      pure (name, parsed)
parseKindedParameter declaration _ = Failed (declaration ++ " parameters require a name and kind")

parseKind :: [Token] -> Result KindExpr
parseKind [token@TKindStar] = markRole "keyword" [] token >> pure KindType
parseKind (TLBracket : rest) = do
  (contents, closing) <- takeTopLevelUntil rest (== TRBracket) "expected ']' after kind"
  case closing of
    [TRBracket] -> do
      kinds <- splitBracketItems contents >>= traverse parseKind
      case reverse kinds of
        result : argument : others -> Parsed (foldr KindArrow result (reverse (argument : others)))
        _ -> Failed "kind arrow requireþ an argument and result"
    _ -> Failed "unexpected tokens after kind"
parseKind _ = Failed "expected * or bracketed kind arrow"

parseEffect :: P Decl
parseEffect ts = do
  (params, name, body) <- parseParamHeaderBody "deed" ts
  (ops, rest) <- parseEffectOps body
  pure (EffectDecl params name ops, rest)

parseParamHeaderBody :: String -> [Token] -> Result ([TypeParameter], String, [Token])
parseParamHeaderBody kind ts = do
  (header, body) <- parseHeaderBody kind ts
  (params, name) <- parseParamHeader kind header
  pure (unknownParameters params, name, body)

parseHeaderBody :: String -> [Token] -> Result ([Token], [Token])
parseHeaderBody kind ts = do
  (header, rest) <- takeHeaderTokens kind ts
  case rest of
    TLBrace : body -> Parsed (header, body)
    _ -> Failed ("expected '{' after " ++ kind ++ " header")

parseParamHeader :: String -> [Token] -> Result ([String], String)
parseParamHeader kind header = do
  (paramTerms, name) <- maybeToEither (kind ++ " declaration requireþ a name") (headerFromTokens header)
  params <- maybeToEither (kind ++ " parameters must be names") (namesFromHeaderTerms paramTerms)
  traverse_ (markNames "type" ["declaration"]) paramTerms
  markHeaderName "type" (["declaration"] ++ ["typeFunction" | not (null params)] ++ ["effect" | kind == "deed"]) header
  pure (params, name)

parseEffectOps :: P [EffectOp]
parseEffectOps (TRBrace : rest) = Parsed ([], rest)
parseEffectOps ts = do
  (operation, rest) <- parseEffectOp ts
  case rest of
    TComma : following -> do
      (operations, remaining) <- parseEffectOps following
      pure (operation : operations, remaining)
    TRBrace : following -> Parsed ([operation], following)
    _ -> Failed "deed members use commas"

parseEffectOp :: P EffectOp
parseEffectOp (token@(TIdent name) : TLBracket : ts) = do
  markRole "method" ["declaration", "effect"] token
  (header, closing) <- takeTopLevelUntil ts (== TRBracket) "expected ']' after effect operation type"
  annotation <- parseBareSignature header
  let TypeAnn ty entries = annotation
  unless (null (headerRequirements entries)) (Failed "effect operation constraints are not supported")
  let operationType = quantifiedType (headerParameters entries) ty
  case closing of
    TRBracket : rest -> Parsed (EffectOp name operationType, rest)
    _ -> Failed "expected ']' after effect operation type"
parseEffectOp _ = Failed "expected bracketed effect operation"

parseClass :: P Decl
parseClass (token@(TIdent name) : header) = do
  markRole "class" (["declaration"] ++ ["typeFunction" | case header of TLBracket : _ -> True; _ -> False]) token
  (entries, body) <- case header of
    TLBrace : rest -> Parsed ([], rest)
    TLBracket : rest -> do
      (contents, closing) <- takeTopLevelUntil rest (== TRBracket) "expected ']' after flock parameters"
      items <- headerItems contents
      (requirements, remaining) <- parseRequirementPrefix items
      parameters <- traverse (fmap Parameter . parseKindedParameter "flock") remaining
      let entries = requirements ++ parameters
      case closing of
        TRBracket : TLBrace : members -> Parsed (entries, members)
        _ -> Failed "expected '{' after flock parameters"
    _ -> Failed "expected '{' or kinded parameters after flock name"
  (members, rest) <- parseClassMembers body
  case rest of
    TRBrace : following -> pure (ClassDecl entries name members, following)
    _ -> Failed "expected '}' after flock body"
parseClass _ = Failed "flock declaration requireþ a name and braced body"

parseClassConstraint :: P ClassConstraint
parseClassConstraint [] = Failed "empty class constraint"
parseClassConstraint ts = do
  (argTerms, name) <- maybeToEither "invalid class constraint" (headerFromTokens ts)
  args <- typesFromHeaderTerms argTerms
  markHeaderName "class" ["applied"] ts
  pure (ClassConstraint args name, [])

parseInstance :: P Decl
parseInstance ts = do
  (entries, headTokens) <- case ts of
    TColon : after -> pure ([], after)
    TLBracket : after -> do
      (contents, closing) <- takeTopLevelUntil after (== TRBracket) "expected ']' after bizen parameters"
      items <- headerItems contents
      (entries, remaining) <- parseHeaderPrefix items
      unless (null remaining) (Failed "bizen header accepteþ only @ parameters and byzen constraints")
      case closing of
        TRBracket : TColon : headTokens -> pure (entries, headTokens)
        _ -> Failed "expected ':' after bizen parameters"
    _ -> Failed "bizen requireþ ':' or bracketed parameters"
  (header, rest) <- takeHeaderTokens "bizen" headTokens
  (tyTerms, className) <- maybeToEither "bizen declaration requireþ a flock name" (headerFromTokens header)
  tyArgs <- typesFromHeaderTerms tyTerms
  markHeaderName "class" ["applied"] header
  when (null tyArgs) (Failed "bizen declaration requireþ at least one type argument")
  body <- case rest of
    TLBrace : after -> pure after
    _ -> Failed "expected '{' after bizen head"
  (ds, following) <- parseLocalDeclsWith (memberRole . parseLocalDecl) body
  case following of
    TRBrace : after -> pure (InstanceDecl entries tyArgs className ds, after)
    _ -> Failed "expected '}' after bizen body"

parseLocalDecl :: P Decl
parseLocalDecl = \case
  TLet : rest -> parseLetDecl rest
  _ -> Failed "expected local let"

parseLocalDecls :: P [Decl]
parseLocalDecls = parseLocalDeclsWith parseLocalDecl

parseLocalDeclsWith :: P Decl -> P [Decl]
parseLocalDeclsWith parser ts = case ts of
  TLet : _ -> next
  _ -> pure ([], ts)
  where
    next = do
      (declaration, rest) <- parser ts
      (declarations, remaining) <- parseLocalDeclsWith parser rest
      pure (declaration : declarations, remaining)

lambdaIfParams :: [Pattern] -> Expr -> Expr
lambdaIfParams [] expr = expr
lambdaIfParams (p : ps) expr = anonymousMatch (p :| ps) expr

anonymousMatch :: NonEmpty Pattern -> Expr -> Expr
anonymousMatch params expr = EMatch [] [MatchCase params expr]

parseCtors :: P [Ctor]
parseCtors = parseCommaListUntil isRightBrace parseCtor

parseCtor :: P Ctor
parseCtor (token@(TIdent name) : TLBracket : ts) = do
  (contents, closing) <- takeTopLevelUntil ts (== TRBracket) "expected ']' after constructor type"
  items <- splitBracketItems contents
  (entries, remaining) <- parseHeaderPrefix items
  unless (null (headerRequirements entries)) (Failed "constructor constraints are not supported")
  types <- traverse (parseWhole "unexpected tokens in constructor type" parseTypeTokens) remaining
  let parameters = headerParameters entries
  case (reverse types, closing) of
    (result : reversedFields, TRBracket : rest) -> do
      markRole (if null reversedFields then "enumMember" else "function") ["declaration", "constructor"] token
      pure (Ctor name parameters (reverse reversedFields) (Just result), rest)
    _ -> Failed "constructor signature requireþ a result type"
parseCtor ts = do
  (parts, rest) <- takeCtorTokens ts
  (argTerms, name) <- maybeToEither ("invalid constructor '" ++ showTokens parts ++ "' before " ++ showTokenHead rest) (headerFromTokens parts)
  fields <- typesFromHeaderTerms argTerms
  markHeaderName (if null fields then "enumMember" else "function") ["declaration", "constructor"] parts
  pure (Ctor name [] fields Nothing, rest)

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

parseClassMembers :: P [ClassMember]
parseClassMembers ts@(TRBrace : _) = Parsed ([], ts)
parseClassMembers ts = do
  (member, rest) <- parseClassMember ts
  (members, remaining) <- parseClassMembers rest
  pure (member : members, remaining)

parseClassMember :: P ClassMember
parseClassMember ts@(TLet : _) = parseClassLet ts
parseClassMember ts@(TLaw : _) = parseClassLaw ts
parseClassMember _ = Failed "expected flock member"

parseClassLet :: P ClassMember
parseClassLet (TLet : ts) = do
  (memberTokens, rest) <- takeTopLevelUntilOrEnd ts isClassMemberBoundary
  member <- parseClassSignature memberTokens
  pure (member, rest)
parseClassLet _ = Failed "expected 'let' before flock member"

parseClassSignature :: [Token] -> Result ClassMember
parseClassSignature ts = case ts of
  token@(TIdent name) : TLBracket : typeTokens -> do
    markRole "call" ["declaration", "applied"] token
    (header, rest) <- takeTopLevelUntil typeTokens (== TRBracket) "expected ']' after flock member type"
    case rest of
      [TRBracket] -> do
        ty <- parseBareSignature header
        pure (ClassSignature name ty)
      _ -> Failed "unexpected tokens after flock member type"
  _ -> Failed "required flock member type requireþ brackets"

parseBareSignature :: [Token] -> Result TypeAnn
parseBareSignature header = do
  (entries, slots, result, effects) <- parseLetBracket header
  arguments <- traverse bareType slots
  resultType <- maybe (Failed "signature requireþ a result type") Parsed result
  TypeAnn ty requirements <- functionLetAnn entries (map Just arguments) (FunctionResult resultType effects)
  pure (TypeAnn ty requirements)
  where
    bareType (BareSlot ty) = Parsed ty
    bareType (BoundSlot _ _) = Failed "signature cannot bind arguments"

isClassMemberBoundary :: Token -> Bool
isClassMemberBoundary = \case
  TLet -> True
  TLaw -> True
  TRBrace -> True
  _ -> False

parseClassLaw :: P ClassMember
parseClassLaw (TLaw : TLBracket : ts) = do
  (header, closing) <- takeTopLevelUntil ts (== TRBracket) "expected ']' after law parameters"
  items <- splitBracketItems header
  (entries, remaining) <- parseHeaderPrefix items
  unless (null (headerRequirements entries)) (Failed "law requirements belong to the enclosing flock")
  parameters <- traverse parseLawBracketParameter remaining
  let names = headerParameters entries
  case closing of
    TRBracket : body -> parseLawBody names parameters body
    _ -> Failed "expected ']' after law parameters"
parseClassLaw _ = Failed "expected bracketed law parameters"

parseLawBracketParameter :: [Token] -> Result (Pattern, TypeExpr)
parseLawBracketParameter tokens = do
  slot <- parseHeaderSlot tokens
  case slot of
    BoundSlot pat ty -> pure (pat, ty)
    _ -> Failed "law parameters require typed patterns"

parseLawBody :: [TypeParameter] -> [(Pattern, TypeExpr)] -> P ClassMember
parseLawBody names parameters body = do
  (leftTokens, separator) <- takeTopLevelUntil body (== TEquals) "expected '=' between law sides"
  (rightTokens, rest) <- case separator of
    TEquals : right -> takeTopLevelUntilOrEnd right isClassMemberBoundary
    _ -> Failed "expected '=' between law sides"
  left <- parseWhole "unexpected tokens on left side of law" parseExpr leftTokens
  right <- parseWhole "unexpected tokens on right side of law" parseExpr rightTokens
  pure (ClassLaw names parameters left right, rest)

-- '$' is the only lower-precedence application layer. ordinary sequences are
-- parsed below it and use the second term as their function.
parseExpr :: P Expr
parseExpr = withPosition parseDollarExprAllowBrace

parseExprStopBrace :: P Expr
parseExprStopBrace = withPosition parseDollarExprStopBrace

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
  rest -> Parsed (value, rest)

parseDollarSegment :: Bool -> Expr -> P Expr
parseDollarSegment stopBrace value = \case
  TLParen : rest -> parseDollarParenSegment value rest
  rest -> parseDollarBareSegment stopBrace value rest

parseDollarBareSegment :: Bool -> Expr -> P Expr
parseDollarBareSegment stopBrace value ts = do
  (terms, rest) <- parseExprParts "expected function after '$'" stopBrace ts
  case terms of
    ValueArgument fn : args -> (,rest) <$> applyParsed fn (ValueArgument value : args)
    TypeArgument _ : _ -> Failed "expected a function before type arguments"
    [] -> Failed "expected function after '$'"

parseDollarParenSegment :: Expr -> P Expr
parseDollarParenSegment value ts = do
  (terms, rest) <- parseFunctionFirstTerms ts
  case (terms, rest) of
    (ValueArgument fn : args, TRParen : rest2) -> (,rest2) <$> applyParsed fn (ValueArgument value : args)
    ([], _) -> Failed "expected function after '$('"
    (_, _) -> Failed "expected ')' after '$('"

data ApplicationArgument = ValueArgument Expr | TypeArgument TypeExpr

parseApplicationArgument :: P ApplicationArgument
parseApplicationArgument (TAt : rest) = do
  (ty, remaining) <- parseTypeAtom rest
  pure (TypeArgument ty, remaining)
parseApplicationArgument tokens = first ValueArgument <$> parseExprAtom tokens

parseFunctionFirstTerms :: P [ApplicationArgument]
parseFunctionFirstTerms = go []
  where
    go acc ts = case ts of
      [] -> Parsed (reverse acc, [])
      TRParen : _ -> Parsed (reverse acc, ts)
      _ ->
        branch
          (parseApplicationArgument ts)
          (\(arg, rest) -> go (arg : acc) rest)
          (pure (reverse acc, ts))

parsePostfixExprWith :: Bool -> P Expr
parsePostfixExprWith stopBrace ts = do
  (parts, rest) <- parseExprParts "expected expression atom" stopBrace ts
  defaultApplication parts rest

applyArgs :: Expr -> [Expr] -> Expr
applyArgs = foldl' (\expr arg -> locateAround [expr, arg] (EApply expr (arg :| [])))

-- a lone term denotes itself; otherwise @a f b c@ lowers to @f a b c@.
defaultApplication :: [ApplicationArgument] -> [Token] -> Result (Expr, [Token])
defaultApplication parts rest = case parts of
  [] -> Failed "expected expression"
  [ValueArgument x] -> Parsed (x, rest)
  ValueArgument f : args@(TypeArgument _ : _) -> (,rest) <$> applyParsed f args
  arg : ValueArgument f : args -> (,rest) <$> applyParsed f (arg : args)
  _ -> Failed "type arguments requireþ a function"

parseExprParts :: String -> Bool -> P [ApplicationArgument]
parseExprParts emptyMessage stopBrace = go []
  where
    go acc ts = case ts of
      [] -> Parsed (reverse acc, [])
      TLBrace : _ | stopBrace && not (null acc) -> Parsed (reverse acc, ts)
      t : _ | exprStop t -> Parsed (reverse acc, ts)
      TDollar : _ -> Parsed (reverse acc, ts)
      _ ->
        branch
          (parseApplicationArgument ts)
          (\(e, rest) -> go (e : acc) rest)
          (if null acc then failedAt ts emptyMessage else pure (reverse acc, ts))

exprStop :: Token -> Bool
exprStop token = startsFileDeclaration token || token `elem` [TComma, TMapsTo, TRParen, TRBrace, TYield, TLaw]

parseExprAtom :: P Expr
parseExprAtom = withPosition (locateParsed parseExprAtomWithFields)

parseExprAtomWithFields :: P Expr
parseExprAtomWithFields ts = do
  (base, rest) <- parseExprAtomRaw ts
  parseRecordFieldSuffixes base rest

parseRecordFieldSuffixes :: Expr -> P Expr
parseRecordFieldSuffixes base = \case
  TDot : token@(TIdent field) : rest
    | not (isQualifiedName field) -> markRole "property" [] token >> parseRecordFieldSuffixes (EField base field) rest
  TDot : TIdent _ : _ -> Failed "record field name must be unqualified"
  TDot : _ -> Failed "expected record field name after '.'"
  rest -> Parsed (base, rest)

parseExprAtomRaw :: P Expr
parseExprAtomRaw = \case
  TInteger i : rest -> Parsed (EInteger i, rest)
  TFloat s : rest -> Parsed (EFloat s, rest)
  TUnicode codePoint : rest -> Parsed (EUnicode codePoint, rest)
  TText key : TForeign : rest -> Parsed (EForeign key, rest)
  TText s : rest -> Parsed (EText s, rest)
  TForeign : _ -> Failed "fremmed requireþ a preceding text key"
  TBraceKeyword marker : rest -> parseBraceKeywordExpr marker rest
  TIdent "r" : TLBrace : _ -> Failed "record opener must be written 'r{' without whitespace"
  token@(TIdent name) : rest -> markRole "term" [] token >> pure (EVar name, rest)
  TTry : rest -> parseTry rest
  TMatch : rest -> parseMatchExpr rest
  TLParen : rest -> parseParen rest
  TLBrace : rest -> parseAnonymousMatchExpr rest
  _ -> Failed "expected expression atom"

parseAnonymousMatchExpr :: P Expr
parseAnonymousMatchExpr ts = do
  (cs, rest) <- parseMatchCases ts
  pure (EMatch [] cs, rest)

parseMatchExpr :: P Expr
parseMatchExpr (TLBrace : _) = Failed "match requireþ a scrutinee; use '{...}' for a function"
parseMatchExpr ts = parseMatchScrutinees [] ts

parseMatchScrutinees :: [Expr] -> P Expr
parseMatchScrutinees acc ts = do
  (scrutinee, rest) <- parseExprStopBrace ts
  case rest of
    TComma : rest2 -> parseMatchScrutinees (scrutinee : acc) rest2
    TLBrace : rest2 -> do
      (cs, rest3) <- parseMatchCases rest2
      pure (EMatch (reverse (scrutinee : acc)) cs, rest3)
    _ -> Failed "expected match scrutinee or '{'"

parseParen :: P Expr
parseParen ts@(TLet : _) = parseBlock ts
parseParen ts = do
  (e, rest) <- parseExpr ts
  case rest of
    TRParen : rest2 -> Parsed (e, rest2)
    TColon : rest2 -> do
      (typeTokens, rest3) <- takeTopLevelUntil rest2 (\case TRParen -> True; _ -> False) "expected ')' after type ascription"
      annotation <- parseWhole "could not parse type ascription" parseTypeTokens typeTokens
      case rest3 of
        TRParen : rest4 -> Parsed (EAscribe e annotation, rest4)
        _ -> Failed "expected ')' after type ascription"
    _ -> Failed "expected ')' after expression"

parseTry :: P Expr
parseTry ts = do
  (body, rest) <- parseExprStopBrace ts
  case rest of
    TLBrace : rest2 -> do
      ((returnCase, cases), rest3) <- parseHandlerCases rest2
      pure (ETry body returnCase cases, rest3)
    _ -> Failed "expected handler cases after try body"

data HandlerItem = HandlerReturnItem ReturnCase | HandlerCaseItem HandlerCase

parseHandlerCases :: P (Maybe ReturnCase, [HandlerCase])
parseHandlerCases ts = do
  (items, rest) <- parseCommaListUntil isRightBrace parseHandlerItem ts
  (returnCase, cases) <- foldM collect (Nothing, []) items
  pure ((returnCase, reverse cases), rest)
  where
    collect (Nothing, cases) (HandlerReturnItem returnCase) = Parsed (Just returnCase, cases)
    collect (Just _, _) (HandlerReturnItem _) = Failed "handler hath more than one yield case"
    collect (returnCase, cases) (HandlerCaseItem handlerCase) = Parsed (returnCase, handlerCase : cases)

parseHandlerItem :: P HandlerItem
parseHandlerItem ts = do
  (header, rest) <- takeTopLevelUntil ts (\case TMapsTo -> True; _ -> False) "expected '^' in handler case"
  item <- case header of
    TYield : returnTokens -> do
      pat <- parseWhole "invalid yield pattern" parsePattern returnTokens
      pure (HandlerReturnItem . ReturnCase pat)
    _ -> do
      (argTerms, name) <- maybeToEither "expected handler case" (headerFromTokens header)
      params <- patternsFromHeaderTerms argTerms
      markHeaderName "call" ["effect", "applied"] header
      pure (HandlerCaseItem . HandlerCase name params)
  case rest of
    TMapsTo : bodyTokens -> do
      (body, following) <- parseExpr bodyTokens
      pure (item body, following)
    _ -> Failed "expected handler case"

parseBlock :: P Expr
parseBlock ts = do
  (ds, rest) <- parseLocalDecls ts
  case rest of
    TYield : expressionTokens -> do
      (e, rest2) <- parseExpr expressionTokens
      case rest2 of
        TRParen : rest3 -> pure (EBlock ds e, rest3)
        _ -> Failed "expected ')' after block result"
    _ -> Failed "expected 'yield' before block result"

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
    TRBrace : rest2 -> pure (EUpdate base [], rest2)
    _ -> Failed "expected ',' or '}' after record update base"

parseRecordUpdates :: P [RecordUpdate]
parseRecordUpdates = parseCommaListUntil isRightBrace parseRecordUpdateItem

parseRecordUpdateItem :: P RecordUpdate
parseRecordUpdateItem (TIdent "-" : token@(TIdent name) : rest) = markRole "property" [] token >> pure (RecordRemove name, rest)
parseRecordUpdateItem (token@(TIdent name) : TEquals : rest) = do
  markRole "property" [] token
  (e, rest2) <- parseExpr rest
  pure (RecordSet name e, rest2)
parseRecordUpdateItem _ = Failed "expected record update"

parseRecordExprFields :: P [(String, Expr)]
parseRecordExprFields = parseCommaListUntil isRightBrace parseRecordExprField

parseRecordExprField :: P (String, Expr)
parseRecordExprField (token@(TIdent name) : TEquals : rest) = do
  markRole "property" [] token
  (e, rest2) <- parseExpr rest
  pure ((name, e), rest2)
parseRecordExprField _ = Failed "expected record field"

parseBraceKeywordExpr :: String -> P Expr
parseBraceKeywordExpr "r" = parseRecordExpr
parseBraceKeywordExpr marker = parseAssociativeExpr marker

parseAssociativeExpr :: String -> P Expr
parseAssociativeExpr marker ts = do
  (expressions, rest) <- parseCommaListUntil isRightBrace parseExpr ts
  case expressions of
    [] -> Failed (marker ++ "{...} requireþ a combining function")
    function : values@(_ : _ : _) -> markFunction function >> pure (associate function values, rest)
    _ -> Failed (marker ++ "{...} requireþ at least two values")
  where
    associate function = direction (applyBinary function)
    direction = if marker == "<" then foldl1 else foldr1

applyBinary :: Expr -> Expr -> Expr -> Expr
applyBinary function left right = applyArgs function [left, right]

parseMatchCases :: P [MatchCase]
parseMatchCases ts = do
  (grouped, rest) <- parseCommaListUntil isRightBrace parseMatchCase ts
  pure (concat grouped, rest)

parseMatchCase :: P [MatchCase]
parseMatchCase ts = do
  (rows, rest) <- parseMatchCaseAlternatives ts
  case rest of
    TMapsTo : bodyTokens -> do
      (e, rest2) <- parseExpr bodyTokens
      pure (map (`MatchCase` e) rows, rest2)
    _ -> Failed "expected '^' in match case"

parseMatchCaseAlternatives :: P [NonEmpty Pattern]
parseMatchCaseAlternatives ts = do
  (first, rest) <- parseMatchCasePatterns ts
  go [first] rest
  where
    go acc (TIdent "|" : rest) = do
      (row, remaining) <- parseMatchCasePatterns rest
      go (row : acc) remaining
    go acc rest@(TMapsTo : _) = Parsed (reverse acc, rest)
    go _ _ = Failed "expected '|' or '^' after match pattern row"

parseMatchCasePatterns :: P (NonEmpty Pattern)
parseMatchCasePatterns ts = do
  (firstPattern, rest) <- parsePattern ts
  go firstPattern [] rest
  where
    go first acc rest = case rest of
      TComma : rest2 -> do
        (p, rest3) <- parsePattern rest2
        go first (p : acc) rest3
      TIdent "|" : _ -> Parsed (first :| reverse acc, rest)
      TMapsTo : _ -> Parsed (first :| reverse acc, rest)
      _ -> Failed "expected ',', '|', or '^' after match pattern"

parsePattern :: P Pattern
parsePattern ts = do
  (parts, rest) <- parsePatternParts ts
  patternFromParts parts rest

parsePatternParts :: P [[Token]]
parsePatternParts = go []
  where
    finish acc rest
      | null acc = Failed "expected pattern"
      | otherwise = Parsed (reverse acc, rest)
    go acc ts = case ts of
      [] -> Parsed (reverse acc, [])
      t : _ | patternStop t -> finish acc ts
      _ -> case readHeaderTerm ts of
        Nothing -> finish acc ts
        Just (term, rest) -> go (term : acc) rest

patternStop :: Token -> Bool
patternStop = \case
  TComma -> True
  TIdent "|" -> True
  TMapsTo -> True
  TRParen -> True
  TRBrace -> True
  _ -> False

patternFromParts :: [[Token]] -> [Token] -> Result (Pattern, [Token])
patternFromParts [term] rest = do
  pat <- patternFromTerm term
  case pat of
    PVar name | isQualifiedName name -> do
      markNames "enumMember" [] term
      pure (PCon name [], rest)
    _ -> pure (pat, rest)
patternFromParts parts rest = do
  (argTerms, name) <- maybeToEither "constructor pattern requireþ a name" (defaultHeader parts)
  args <- patternsFromHeaderTerms argTerms
  markHeaderName "enumMember" [] (concat parts)
  pure (PCon name args, rest)

patternFromTerm :: [Token] -> Result Pattern
patternFromTerm [token@(TIdent name)] = do
  markRole "parameter" ["declaration"] token
  pure (PVar name)
patternFromTerm [TInteger value] = pure (PInteger value)
patternFromTerm [TText value] = pure (PText value)
patternFromTerm (TLParen : rest) = do
  (inner, following) <- takeBalanced rest
  if null following then parseWhole "invalid grouped pattern" parsePattern inner else Failed "unexpected tokens after pattern"
patternFromTerm term = parseWhole "invalid pattern" parsePattern term

parseTypeUntilCommaOrBrace :: String -> P TypeExpr
parseTypeUntilCommaOrBrace message ts = parseTypeUntilTopLevel ts (\case TComma -> True; TRBrace -> True; _ -> False) message

parseTypeUntilTopLevel :: [Token] -> (Token -> Bool) -> String -> Result (TypeExpr, [Token])
parseTypeUntilTopLevel ts stop message = do
  (seen, rest) <- takeTopLevelUntil ts stop message
  finishType seen rest

parseTypeUntilTopLevelOrEnd :: [Token] -> (Token -> Bool) -> String -> Result (TypeExpr, [Token])
parseTypeUntilTopLevelOrEnd ts stop message =
  case takeTopLevelUntilOrEnd ts stop of
    Parsed (seen, rest) -> finishType seen rest
    Failed _ -> Failed message

finishType :: [Token] -> [Token] -> Result (TypeExpr, [Token])
finishType tokens rest = (,rest) <$> parseWhole "could not parse whole type" parseTypeTokens tokens

-- type application followeþ the same second-is-function rule as term application.
parseTypeTokens :: P TypeExpr
parseTypeTokens = withPosition \ts -> case splitTopLevelBang ts of
  Just _ -> Failed "effects are only allowed on function types"
  Nothing -> case splitTopLevel ts (== TArrow) of
    Just (before, after) | not (null before) && not (null after) -> do
      argument <- parseWhole "invalid function argument type" parseTypeApplication before
      result <- parseWhole "invalid function result type" parseTypeTokens after
      ty <- makeArrowType [argument] [] result
      pure (ty, [])
    _ -> parseTypeApplication ts

makeArrowType :: [TypeExpr] -> [TypeExpr] -> TypeExpr -> Result TypeExpr
makeArrowType args effects ret = do
  domain <- maybe (Failed "function type requireþ at least one argument") Parsed (NE.nonEmpty args)
  pure $ case (effects, ret) of
    ([], TypeArrow more moreEffects finalRet) -> TypeArrow (domain <> more) moreEffects finalRet
    _ -> TypeArrow domain effects ret

parseTypeApplication :: P TypeExpr
parseTypeApplication ts = do
  (parts, rest) <- parseTypeParts ts
  defaultType parts rest

parseTypeParts :: P [TypeExpr]
parseTypeParts = go []
  where
    go acc ts =
      branch
        (parseTypeAtom ts)
        ( \(ty, rest) -> do
            when (case acc of [_] -> True; _ -> False) (markTypeFunction ty (consumedTokens ts rest))
            go (ty : acc) rest
        )
        (if null acc then Failed "expected type" else pure (reverse acc, ts))

parseTypeAtom :: P TypeExpr
parseTypeAtom = \case
  TBraceKeyword "r" : rest -> parseRecordType rest
  TIdent "r" : TLBrace : _ -> Failed "record opener must be written 'r{' without whitespace"
  token@(TIdent name) : rest -> markRole "type" [] token >> pure (TypeName name, rest)
  token@TArrow : rest -> markRole "type" [] token >> pure (TypeName "→", rest)
  TLBracket : rest -> do
    (contents, closing) <- takeTopLevelUntil rest (== TRBracket) "expected ']' after function type"
    case closing of
      TRBracket : remaining -> (,remaining) <$> parseBracketType contents
      _ -> Failed "expected ']' after function type"
  TLParen : rest -> do
    (inner, rest2) <- takeBalanced rest
    t <- parseWhole "could not parse parenthesised type" parseParenthesizedType inner
    pure (t, rest2)
  _ -> Failed "expected type"

parseBracketType :: [Token] -> Result TypeExpr
parseBracketType tokens = do
  let (typeTokens, effectTokens) = splitBracketEffects tokens
  items <- splitBracketItems typeTokens
  (entries, remaining) <- parseHeaderPrefix items
  unless (null (headerRequirements entries)) (Failed "nested class constraints are not supported")
  let names = headerParameters entries
  types <- traverse (parseWhole "unexpected tokens in function type" parseTypeTokens) remaining
  effects <- maybe (Parsed []) (withEffect . parseBracketTypes) effectTokens
  body <- case reverse types of
    result : argument : remaining -> makeArrowType (reverse (argument : remaining)) effects result
    _ -> Failed "function type requireþ an argument and result"
  pure (if null names then body else TypeForall names body)

startsTypeParameter :: [Token] -> Bool
startsTypeParameter (TAt : _) = True
startsTypeParameter _ = False

quantifiedType :: [TypeParameter] -> TypeExpr -> TypeExpr
quantifiedType [] body = body
quantifiedType names body = TypeForall names body

parseBracketTypes :: [Token] -> Result [TypeExpr]
parseBracketTypes tokens =
  splitBracketItems tokens >>= traverse (parseWhole "unexpected tokens in function type" parseTypeTokens)

splitBracketEffects :: [Token] -> ([Token], Maybe [Token])
splitBracketEffects tokens = case splitTopLevel tokens (== TSemicolon) of
  Nothing -> (tokens, Nothing)
  Just (before, after) -> (before, Just after)

splitBracketItems :: [Token] -> Result [[Token]]
splitBracketItems [] = Failed "expected item in function brackets"
splitBracketItems tokens = case splitTopLevelComma tokens of
  Just (before, after)
    | null before -> Failed "empty item in function brackets"
    | otherwise -> (before :) <$> splitBracketItems after
  Nothing -> Parsed [tokens]

parseParenthesizedType :: P TypeExpr
parseParenthesizedType (TIdent _ : TColon : rest) = parseTypeTokens rest
parseParenthesizedType ts = parseTypeTokens ts

parseRecordType :: P TypeExpr
parseRecordType ts = do
  (fields, rest) <- parseRecordTypeFields ts
  pure (TypeRecord fields, rest)

parseRecordTypeFields :: P [(String, TypeExpr)]
parseRecordTypeFields = parseCommaListUntil isRightBrace parseRecordTypeField

parseRecordTypeField :: P (String, TypeExpr)
parseRecordTypeField (token@(TIdent name) : TColon : rest) = do
  markRole "property" [] token
  (t, rest2) <- parseTypeUntilCommaOrBrace "unterminated record type" rest
  pure ((name, t), rest2)
parseRecordTypeField _ = Failed "expected record field type"

-- @a f b@ is @f<a,b>@; unresolved non-name heads are retained for the checker
-- to reject with type context rather than by guessing in the parser.
defaultType :: [TypeExpr] -> [Token] -> Result (TypeExpr, [Token])
defaultType parts rest = case parts of
  [] -> Failed "expected type"
  [x] -> Parsed (x, rest)
  arg : TypeName name : args -> Parsed (typeApplication name (arg : args), rest)
  arg : f : args -> Parsed (TypeApply "<type>" (arg : f : args), rest)

typeApplication :: String -> [TypeExpr] -> TypeExpr
typeApplication name [] = TypeName name
typeApplication name args = TypeApply name args

functionHeader :: [Token] -> Result ([Pattern], Token)
functionHeader ts = do
  (argTerms, _) <- maybeToEither "expected positional function header" (headerFromTokens ts)
  params <- patternsFromHeaderTerms argTerms
  name <- maybeToEither "expected function name" (headerNameToken ts)
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
readHeaderTerm (token@(TIdent _) : rest) = Just ([token], rest)
readHeaderTerm (token@(TArrow) : rest) = Just ([token], rest)
readHeaderTerm (token@(TInteger _) : rest) = Just ([token], rest)
readHeaderTerm (token@(TText _) : rest) = Just ([token], rest)
readHeaderTerm (TLParen : rest) = do
  (inner, rest2) <- either (const Nothing) Just (resultEither (takeBalanced rest))
  Just (TLParen : inner ++ [TRParen], rest2)
readHeaderTerm (TLBracket : rest) = do
  (inner, closing) <- either (const Nothing) Just (resultEither (takeTopLevelUntil rest (== TRBracket) "expected ']' after header type"))
  case closing of
    TRBracket : rest2 -> Just (TLBracket : inner ++ [TRBracket], rest2)
    _ -> Nothing
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

typesFromHeaderTerms :: [[Token]] -> Result [TypeExpr]
typesFromHeaderTerms = traverse (parseWhole "invalid header type" parseTypeTokens)

patternsFromHeaderTerms :: [[Token]] -> Result [Pattern]
patternsFromHeaderTerms = traverse patternFromTerm

namesFromHeaderTerms :: [[Token]] -> Maybe [String]
namesFromHeaderTerms = traverse headerTermName

parseWhole :: String -> P a -> [Token] -> Result a
parseWhole restMessage parser tokens = do
  (value, rest) <- parser tokens
  if null rest then pure value else failedAt rest restMessage

maybeToEither :: String -> Maybe a -> Result a
maybeToEither msg = maybe (Failed msg) Parsed

parseCommaListUntil :: (Token -> Bool) -> P a -> P [a]
parseCommaListUntil stop parseItem = go
  where
    go (t : rest) | stop t = Parsed ([], rest)
    go ts = do
      (x, rest) <- parseItem ts
      (xs, rest2) <- go (dropComma rest)
      pure (x : xs, rest2)

dropComma :: [Token] -> [Token]
dropComma (TComma : rest) = rest
dropComma ts = ts

isRightBrace :: Token -> Bool
isRightBrace = (== TRBrace)

splitTopLevelBang, splitTopLevelColon, splitTopLevelComma :: [Token] -> Maybe ([Token], [Token])
splitTopLevelBang ts = splitTopLevel ts (\case TBang -> True; _ -> False)
splitTopLevelColon ts = splitTopLevel ts (\case TColon -> True; _ -> False)
splitTopLevelComma ts = splitTopLevel ts (\case TComma -> True; _ -> False)

splitTopLevel :: [Token] -> (Token -> Bool) -> Maybe ([Token], [Token])
splitTopLevel ts stop = case spanTopLevel ts stop of
  (_, []) -> Nothing
  (prefix, _ : rest) -> Just (prefix, rest)

-- delimiter-aware slicing keepeþ declaration and type parsers small. callers
-- decide whether reaching the end is valid for their enclosing construct.
takeTopLevelUntil :: [Token] -> (Token -> Bool) -> String -> Result ([Token], [Token])
takeTopLevelUntil ts stop message = case takeTopLevelUntilOrEnd ts stop of
  Parsed (_, []) -> Failed message
  other -> other

takeTopLevelUntilOrEnd :: [Token] -> (Token -> Bool) -> Result ([Token], [Token])
takeTopLevelUntilOrEnd ts stop = Parsed (spanTopLevel ts stop)

spanTopLevel :: [Token] -> (Token -> Bool) -> ([Token], [Token])
spanTopLevel ts stop = go [] (0 :: Int) ts
  where
    go acc _ [] = (reverse acc, [])
    go acc depth xs@(x : rest)
      | depth == 0 && stop x = (reverse acc, xs)
      | otherwise = go (x : acc) (depth + change x) rest
    change token
      | isDelimiterOpen token = 1
      | token `elem` [TRParen, TRBracket, TRBrace] = -1
      | otherwise = 0

takeBalanced :: [Token] -> Result ([Token], [Token])
takeBalanced tokens = do
  (prefix, rest) <- takeTopLevelUntil tokens (== TRParen) "unclosed parenthesised type"
  pure (prefix, drop 1 rest)

isDelimiterOpen :: Token -> Bool
isDelimiterOpen = \case
  TLParen -> True
  TLBracket -> True
  TLBrace -> True
  TBraceKeyword _ -> True
  _ -> False

takeHeaderTokens :: String -> [Token] -> Result ([Token], [Token])
takeHeaderTokens kind ts = takeTopLevelUntil ts (\case TLBrace -> True; _ -> False) ("expected '{' after " ++ kind ++ " header")

takeCtorTokens :: [Token] -> Result ([Token], [Token])
takeCtorTokens ts = takeTopLevelUntil ts (\token -> token == TComma || token == TRBrace) "unterminated ilk declaration"

-- located parser tokens decorate atoms as they are consumed; application spans
-- then grow from their already located children.
locateParsed :: P Expr -> P Expr
locateParsed parser tokens = do
  (expression, rest) <- parser tokens
  case expression of
    -- fremmed is a declaration marker rather than an ordinary expression; its
    -- direct-body class must remain visible to validation and elaboration.
    EForeign _ -> pure (expression, rest)
    _ -> do
      pure (maybe expression (`ELocated` expression) (consumedSpan tokens rest), rest)

locateAround :: [Expr] -> Expr -> Expr
locateAround children expression = maybe expression (`ELocated` expression) (coverSpans (mapMaybe exprSpan children))

exprSpan :: Expr -> Maybe SourceSpan
exprSpan (ELocated span _) = Just span
exprSpan _ = Nothing

coverSpans :: [SourceSpan] -> Maybe SourceSpan
coverSpans [] = Nothing
coverSpans spans = Just (SourceSpan (minimum (map spanStart spans)) (maximum (map spanEnd spans)))

-- speculative parsing retainþ events from the selected branch. failed prefixes
-- can still contribute useful roles, without accepting their syntax.
branch :: Result a -> (a -> Result b) -> Result b -> Result b
branch (Result value roles) success failure = retain roles (either (const failure) success value)

markRole :: String -> [String] -> Token -> Result ()
markRole _ _ (TIdent "_") = pure ()
markRole role modifiers token = case tokenSpan token of
  Nothing -> pure ()
  Just span -> Result (Right ()) (Seq.singleton (SourceRole span role modifiers))

markNames :: String -> [String] -> [Token] -> Result ()
markNames role modifiers = traverse_ \case
  token@TIdent {} -> markRole role modifiers token
  token@TType -> markRole role modifiers token
  _ -> pure ()

headerNameToken :: [Token] -> Maybe Token
headerNameToken (token@TIdent {} : TLBracket : _) = Just token
headerNameToken tokens = do
  parts <- headerParts tokens
  case parts of
    [[token@TIdent {}]] -> Just token
    _ : [token@TIdent {}] : _ -> Just token
    _ -> Nothing

markHeaderName :: String -> [String] -> [Token] -> Result ()
markHeaderName role modifiers = maybe (pure ()) (markRole role modifiers) . headerNameToken

plainBinders :: Result a -> Result a
plainBinders (Result value roles) = Result value (fmap plain roles)
  where
    plain (SourceRole span role _) | role `elem` ["parameter", "enumMember"] = SourceRole span "headerPattern" []
    plain role = role

memberRole :: Result a -> Result a
memberRole (Result value roles) =
  Result
    value
    ( case Seq.findIndexL isBinding roles of
        Just index -> Seq.adjust' member index roles
        Nothing -> roles
    )
  where
    isBinding (SourceRole _ role modifiers) = role `elem` ["function", "variable"] && "declaration" `elem` modifiers
    member (SourceRole span "function" modifiers) = SourceRole span "call" modifiers
    member role = role

withEffect :: Result a -> Result a
withEffect (Result value roles) = Result value (fmap effect roles)
  where
    effect (SourceRole span "type" modifiers) = SourceRole span "type" ("effect" : modifiers)
    effect role = role

consumedTokens :: [Token] -> [Token] -> [Token]
consumedTokens tokens [] = tokens
consumedTokens tokens rest@(next : _) = case tokenSpan next of
  Just span -> takeWhile (maybe True ((< spanStart span) . spanStart) . tokenSpan) tokens
  Nothing -> take (length tokens - length rest) tokens

markTypeFunction :: TypeExpr -> [Token] -> Result ()
markTypeFunction (TypeName name) tokens = traverse_ (markRole "type" ["applied"]) (filter named tokens)
  where
    named (TIdent value) = value == name
    named TArrow = name == "→"
    named _ = False
markTypeFunction _ _ = pure ()

applyParsed :: Expr -> [ApplicationArgument] -> Result Expr
applyParsed function arguments = do
  markFunction function
  let specialised = maybe function (ETypeApply function) (NE.nonEmpty [ty | TypeArgument ty <- arguments])
  pure (applyArgs specialised [value | ValueArgument value <- arguments])

markFunction :: Expr -> Result ()
markFunction (ELocated span (EVar "eftgin")) = Result (Right ()) (Seq.singleton (SourceRole span "keyword" []))
markFunction (ELocated span (EVar name))
  | name /= "_" = Result (Right ()) (Seq.singleton (SourceRole span "call" ["applied"]))
markFunction (ELocated _ body) = markFunction body
markFunction (EApply function _) = markFunction function
markFunction (ETypeApply function _) = markFunction function
markFunction _ = pure ()

-- highlighting useþ the same lexer and parser, without imports or type checking.
-- after failure, unindented declaration heads bound each recovery parse. a series
-- of unfinished brackets must not repeatedly scan the remaining file.
parseSourceRoles :: String -> [SourceRole]
parseSourceRoles source =
  lexical ++ case parseTokenStream tokens of
    Result (Right _) events -> toList events
    Result (Left _) events -> toList events ++ foldMap roles (chunks tokens)
  where
    tokens = map locatedToken (lexLocatedPrefix source)
    lexical = [SourceRole span "keyword" [] | token <- tokens, token `elem` [TAt, TKindStar, TConstraint], Just span <- [tokenSpan token]]
    roles (Result _ events) = toList events
    chunks [] = []
    chunks (token : rest) =
      let (body, remaining) = break boundary rest
       in parseTokenStream (token : body) : chunks remaining
    boundary token =
      (startsFileDeclaration token || token == TYield)
        && maybe False ((`Set.member` starts) . spanStart) (tokenSpan token)
    starts = Set.fromList (0 : lineStarts 0 source)
    lineStarts _ [] = []
    lineStarts offset (character : rest)
      | character == '\n' || character == '\r' = next : lineStarts next rest
      | otherwise = lineStarts next rest
      where
        next = offset + if fromEnum character > 0xffff then 2 else 1
