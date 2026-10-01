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

import Control.Monad (foldM, when)
import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map
import Data.Maybe (isNothing, mapMaybe, maybeToList)
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

data KindExpr = KindType | KindArrow KindExpr KindExpr | KindUnknown deriving (Eq)

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
parseExportGroup (TConstraints : _) = Failed "show must follow graiþ requirements"
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
  TConstraints : rest -> parseConstraintDecl rest
  TLet : rest -> parseLetDecl [] rest
  TTypeAlias : rest -> parseTypeAlias rest
  TType : rest -> parseData rest
  TEffect : rest -> parseEffect rest
  TClass : rest -> parseClass [] rest
  TInstance : rest -> parseInstance [] rest
  _ -> Failed "expected declaration"

parseConstraintDecl :: P Decl
parseConstraintDecl ts = do
  (constraints, rest) <- parseConstraintPrefix isConstraintEnd expected ts
  let (exported, declaration) = case rest of
        TExport : after -> (True, after)
        _ -> (False, rest)
  (parsed, remaining) <- case declaration of
    TLet : rest2 -> parseLetDecl constraints rest2
    TClass : rest2 -> parseClass constraints rest2
    TInstance : rest2 -> parseInstance constraints rest2
    _ -> Failed expected
  pure (if exported then Export parsed else parsed, remaining)
  where
    expected = "expected 'let', 'flock' or 'bizen' after graiþ"

isConstraintEnd :: Token -> Bool
isConstraintEnd = \case
  TLet -> True
  TClass -> True
  TInstance -> True
  TExport -> True
  TExportType -> True
  _ -> False

parseConstraintLet :: P Decl
parseConstraintLet ts = do
  (constraints, rest) <- parseConstraintPrefix (\case TLet -> True; _ -> False) "expected 'let' after graiþ" ts
  case rest of
    TLet : rest2 -> parseLetDecl constraints rest2
    _ -> Failed "expected 'let' after graiþ"

parseConstraintPrefix :: (Token -> Bool) -> String -> [Token] -> Result ([ClassConstraint], [Token])
parseConstraintPrefix stop message ts = do
  (constraintTokens, rest) <- takeTopLevelUntil ts stop message
  constraints <- parseClassConstraintsWhole constraintTokens
  pure (constraints, rest)

-- let parsing accepteþ named and positional headers and constraint prefixes, then
-- lowers every parameter list to an anonymous match.
parseLetDecl :: [ClassConstraint] -> P Decl
parseLetDecl constraints = \case
  token@(TIdent _) : rest -> parseLet constraints token rest
  ts@(TInteger _ : _) -> parsePositionalLet constraints ts
  ts@(TLParen : _) -> parsePositionalLet constraints ts
  _ -> Failed "expected let binding"

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

parseTypeAlias :: P Decl
parseTypeAlias ts = do
  (header, rest) <- takeTopLevelUntil ts (\case TEquals -> True; _ -> False) "expected '=' after let-ilk header"
  (params, name) <- parseParamHeader "let-ilk" header
  case rest of
    TEquals : body -> do
      (target, remaining) <- parseTypeUntilFileBoundary body
      pure (TypeAlias params name target, remaining)
    _ -> Failed "expected '=' after let-ilk header"

parseTypeUntilFileBoundary :: P TypeExpr
parseTypeUntilFileBoundary ts =
  parseTypeUntilTopLevelOrEnd ts isBoundary "unterminated type"
  where
    isBoundary TYield = True
    isBoundary token = startsFileDeclaration token

startsFileDeclaration :: Token -> Bool
startsFileDeclaration = \case
  TImport -> True
  TConstraints -> True
  TLet -> True
  TTypeAlias -> True
  TType -> True
  TEffect -> True
  TClass -> True
  TInstance -> True
  TExport -> True
  TExportType -> True
  _ -> False

parseLet :: [ClassConstraint] -> Token -> P Decl
parseLet constraints name = \case
  TDefine : rest -> parseLetBody name (implicitLetAnn constraints []) [] rest
  TEquals : _ -> Failed "let definition useþ '≔'"
  TLBracket : rest -> parseBracketLet constraints name [] rest
  ts -> parsePositionalLet constraints (name : ts)

parsePositionalLet :: [ClassConstraint] -> P Decl
parsePositionalLet constraints ts = do
  (header, rest) <- takeTopLevelUntil ts isHeaderBoundary "expected '≔' after let header"
  (patterns, name) <- functionHeader header
  case rest of
    TLBracket : after -> parseBracketLet constraints name patterns after
    TDefine : body -> parseLetBody name (implicitLetAnn constraints (replicate (length patterns) Nothing)) patterns body
    TEquals : _ -> Failed "let definition useþ '≔'"
    _ -> Failed "expected '≔' after let header"
  where
    isHeaderBoundary = \case
      TLBracket -> True
      TEquals -> True
      TDefine -> True
      _ -> False

parseBracketLet :: [ClassConstraint] -> Token -> [Pattern] -> P Decl
parseBracketLet constraints name positional ts = do
  (header, closing) <- takeTopLevelUntil ts (== TRBracket) "expected ']' after let header"
  (slots, result, effects) <- parseLetBracket (not (null positional)) header
  case closing of
    TRBracket : TEquals : _ -> Failed "bracketed let header doth not use '='"
    TRBracket : TDefine : _ -> Failed "bracketed let header doth not use '≔'"
    TRBracket : body -> do
      let arguments = replicate (length positional) Nothing ++ map slotType slots
          patterns = positional ++ [pat | BoundSlot pat _ <- slots]
      annotation <- case result of
        Just ty -> Just <$> functionLetAnn constraints arguments (FunctionResult ty effects)
        Nothing
          | null effects -> Parsed (implicitLetAnn constraints arguments)
          | otherwise -> Failed "effect row requireþ a result type"
      parseLetBody name annotation patterns body
    _ -> Failed "expected ']' after let header"
  where
    slotType (BoundSlot _ ty) = Just ty
    slotType (BareSlot ty) = Just ty

parseLetBracket :: Bool -> [Token] -> Result ([HeaderSlot], Maybe TypeExpr, [TypeExpr])
parseLetBracket hasPositional tokens = do
  let (slotTokens, effectTokens) = splitBracketEffects tokens
  items <- splitBracketItems slotTokens
  -- type parameters are declarations, not runtime arguments or result slots.
  let (parameterItems, argumentItems) = span startsTypeParameter items
  names <- traverse parseTypeParameter parameterItems
  if any startsTypeParameter argumentItems
    then Failed "type parameters must precede value arguments"
    else pure ()
  if length names /= Set.size (Set.fromList names)
    then Failed "type parameters must have distinct names"
    else pure ()
  slots <- traverse parseHeaderSlot argumentItems
  effects <- maybe (Parsed []) (withEffect . parseBracketTypes) effectTokens
  let (arguments, result) = case reverse slots of
        BareSlot ty : rest -> (reverse rest, Just ty)
        _ -> (slots, Nothing)
  if null arguments && not hasPositional && isNothing result
    then Failed "function header requireþ at least one argument"
    else
      if hasBoundAfterBare arguments
        then Failed "named arguments must precede bare argument types"
        else Parsed (arguments, result, effects)
  where
    hasBoundAfterBare = go False
    go _ [] = False
    go _ (BareSlot _ : rest) = go True rest
    go seenBare (BoundSlot _ _ : rest) = seenBare || go False rest
    startsTypeParameter (TAt : _) = True
    startsTypeParameter _ = False

parseTypeParameter :: [Token] -> Result String
parseTypeParameter [TAt, token@(TIdent name), TColon, kind@TType]
  | name /= "_" && not (isQualifiedName name) && name `notElem` primitiveTypeNames = do
      markRole "type" ["declaration"] token
      markRole "type" [] kind
      pure name
parseTypeParameter _ = Failed "type parameter must have the form '@name:ilk'"

parseHeaderSlot :: [Token] -> Result HeaderSlot
parseHeaderSlot (TAt : _) = Failed "type parameter must have the form '@name:ilk'"
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
  let callable = not (null params) || case ann of Just (TypeAnn TypeArrow {} _) -> True; _ -> False
  markRole (if callable then "function" else "variable") (["declaration"] ++ ["applied" | callable]) token
  (e, rest) <- parseExpr ts
  when (not callable && isAnonymousMatchExpr e) (markRole "function" ["declaration", "applied"] token)
  pure (Let name ann (lambdaIfParams params e), rest)
parseLetBody _ _ _ _ = Failed "expected let name"

functionLetAnn :: [ClassConstraint] -> [Maybe TypeExpr] -> FunctionResult -> Result TypeAnn
functionLetAnn constraints [] FunctionResult {functionResultType = result, functionResultEffects = []} = Parsed (TypeAnn result constraints)
functionLetAnn _ [] _ = Failed "effect annotation requireþ function arguments"
functionLetAnn constraints argTypes FunctionResult {..} =
  TypeAnn <$> makeArrowType (instanceMissingTypes argTypes) functionResultEffects functionResultType <*> pure constraints

implicitLetAnn :: [ClassConstraint] -> [Maybe TypeExpr] -> Maybe TypeAnn
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
implicitTypeAt n = TypeName ("t" ++ show n)

parseData :: P Decl
parseData ts = do
  (header, body) <- parseHeaderBody "ilk" ts
  (params, name, kinds) <- parseDataHeader header
  markHeaderName "type" (["declaration"] ++ ["typeFunction" | not (null params)]) header
  (ctors, rest) <- parseCtors body
  if null kinds && not (null params)
    then pure ()
    else traverse_ (checkConstructorKinds name kinds) ctors
  pure (DataDecl params name ctors, rest)

parseDataHeader :: [Token] -> Result ([String], String, [(String, KindExpr)])
parseDataHeader (TIdent name : TLBracket : rest) = do
  (contents, closing) <- takeTopLevelUntil rest (== TRBracket) "expected ']' after ilk parameters"
  params <- splitBracketItems contents >>= traverse (parseKindedParameter "ilk")
  case closing of
    [TRBracket] -> Parsed (map fst params, name, params)
    _ -> Failed "unexpected tokens after ilk parameters"
parseDataHeader header = do
  (params, name) <- parseParamHeader "ilk" header
  pure (params, name, [])

parseKindedParameter :: String -> [Token] -> Result (String, KindExpr)
parseKindedParameter _ (token@(TIdent name) : TColon : kind) = do
  markRole "type" ["declaration"] token
  parsed <- parseKind kind
  pure (name, parsed)
parseKindedParameter declaration _ = Failed (declaration ++ " parameters require a name and kind")

parseKind :: [Token] -> Result KindExpr
parseKind [token@TType] = markRole "type" [] token >> pure KindType
parseKind (TLBracket : rest) = do
  (contents, closing) <- takeTopLevelUntil rest (== TRBracket) "expected ']' after kind"
  case closing of
    [TRBracket] -> do
      kinds <- splitBracketItems contents >>= traverse parseKind
      case reverse kinds of
        result : argument : others -> Parsed (foldr KindArrow result (reverse (argument : others)))
        _ -> Failed "kind arrow requireþ an argument and result"
    _ -> Failed "unexpected tokens after kind"
parseKind _ = Failed "expected ilk or bracketed kind arrow"

checkConstructorKinds :: String -> [(String, KindExpr)] -> Ctor -> Result ()
checkConstructorKinds dataName declaredParameters (Ctor _ fields result) =
  checkSignatureKinds "constructor" (Just dataName) declaredParameters (fields ++ maybeToList result)

-- check local kinds while the declared parameters are still available. other
-- type constructors are resolved later, so their kinds remain unknown here.
checkSignatureKinds :: String -> Maybe String -> [(String, KindExpr)] -> [TypeExpr] -> Result ()
checkSignatureKinds owner self declaredParameters = traverse_ (checkTypeKind KindType)
  where
    parameters = Map.fromList declaredParameters
    selfKind = foldr KindArrow KindType (map snd declaredParameters)
    kindOf (TypeName name) = Parsed (headKind name)
    kindOf (TypeApply name args) = do
      argKinds <- traverse kindOf args
      foldM apply (headKind name) argKinds
    kindOf (TypeRecord fields) = traverse_ (checkTypeKind KindType . snd) fields >> pure KindType
    kindOf (TypeArrow arguments effects resultType) = do
      traverse_ (checkTypeKind KindType) (NE.toList arguments ++ effects ++ [resultType])
      pure KindType
    checkTypeKind expected ty = do
      actual <- kindOf ty
      if actual == expected || actual == KindUnknown then pure () else Failed (owner ++ " type hath the wrong kind")
    apply (KindArrow expected resultKind) actual
      | expected == actual || actual == KindUnknown = Parsed resultKind
      | otherwise = Failed (owner ++ " type argument hath the wrong kind")
    apply KindUnknown _ = Parsed KindUnknown
    apply KindType _ = Failed ("too many type arguments in " ++ owner ++ " signature")
    headKind name
      | Just name == self = selfKind
      | name == "→" = KindArrow KindType (KindArrow KindType KindType)
      | name `elem` primitiveTypeNames = KindType
      | otherwise = Map.findWithDefault KindUnknown name parameters

parseEffect :: P Decl
parseEffect ts = do
  (params, name, body) <- parseParamHeaderBody "deed" ts
  (ops, rest) <- parseEffectOps body
  pure (EffectDecl params name ops, rest)

parseParamHeaderBody :: String -> [Token] -> Result ([String], String, [Token])
parseParamHeaderBody kind ts = do
  (header, body) <- parseHeaderBody kind ts
  (params, name) <- parseParamHeader kind header
  pure (params, name, body)

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
  annotation <- parseBareSignature [] header
  let TypeAnn ty _ = annotation
  case closing of
    TRBracket : rest -> Parsed (EffectOp name ty, rest)
    _ -> Failed "expected ']' after effect operation type"
parseEffectOp _ = Failed "expected bracketed effect operation"

parseClass :: [ClassConstraint] -> P Decl
parseClass constraints (token@(TIdent name) : header) = do
  markRole "class" (["declaration"] ++ ["typeFunction" | case header of TLBracket : _ -> True; _ -> False]) token
  (params, body) <- case header of
    TLParen : rest -> Parsed ([], rest)
    TLBracket : rest -> do
      (contents, closing) <- takeTopLevelUntil rest (== TRBracket) "expected ']' after flock parameters"
      parameters <- splitBracketItems contents >>= traverse (parseKindedParameter "flock")
      case closing of
        TRBracket : TLParen : members -> Parsed (parameters, members)
        _ -> Failed "expected '(' after flock parameters"
    _ -> Failed "expected '(' or kinded parameters after flock name"
  (members, rest) <- parseClassMembers body
  traverse_ (checkClassMemberKinds params) members
  case rest of
    TRParen : following -> pure (ClassDecl (map fst params) name constraints members, following)
    _ -> Failed "expected ')' after flock body"
parseClass _ _ = Failed "flock declaration requireþ a name and parenthesised body"

checkClassMemberKinds :: [(String, KindExpr)] -> ClassMember -> Result ()
checkClassMemberKinds params (ClassSignature _ (TypeAnn signature _)) =
  checkSignatureKinds "flock member" Nothing params [signature]
checkClassMemberKinds params (ClassLaw patterns _ _) =
  checkSignatureKinds "flock law" Nothing params (map snd patterns)

parseClassConstraints :: P [ClassConstraint]
parseClassConstraints = go []
  where
    go acc [] = Parsed (reverse acc, [])
    go acc (TComma : rest) = go acc rest
    go acc input = do
      (parts, rest) <- takeTopLevelUntilOrEnd input (\case TComma -> True; _ -> False)
      (constraint, _) <- parseClassConstraint parts
      go (constraint : acc) (dropComma rest)

parseClassConstraintsWhole :: [Token] -> Result [ClassConstraint]
parseClassConstraintsWhole [] = Parsed []
parseClassConstraintsWhole tokens = parseWhole "unexpected tokens after graiþ" parseClassConstraints tokens

parseClassConstraint :: P ClassConstraint
parseClassConstraint [] = Failed "empty graiþ"
parseClassConstraint ts = do
  (argTerms, name) <- maybeToEither "invalid graiþ" (headerFromTokens ts)
  args <- typesFromHeaderTerms argTerms
  markHeaderName "class" ["applied"] ts
  pure (ClassConstraint args name, [])

parseInstance :: [ClassConstraint] -> P Decl
parseInstance constraints ts = do
  (header, body) <- parseHeaderBody "bizen" ts
  (tyTerms, className) <- maybeToEither "bizen declaration requireþ a flock name" (headerFromTokens header)
  tyArgs <- typesFromHeaderTerms tyTerms
  markHeaderName "class" ["applied"] header
  case tyArgs of
    [] -> Failed "bizen declaration requireþ at least one type argument"
    _ -> pure ()
  (ds, rest) <- parseLocalDeclsWith (memberRole . parseLocalDecl) body
  case rest of
    TRBrace : rest2 -> pure (InstanceDecl tyArgs className constraints ds, rest2)
    _ -> Failed "expected '}' after bizen body"

parseLocalDecl :: P Decl
parseLocalDecl = \case
  TConstraints : rest -> parseConstraintLet rest
  TLet : rest -> parseLetDecl [] rest
  _ -> Failed "expected local let"

parseLocalDecls :: P [Decl]
parseLocalDecls = parseLocalDeclsWith parseLocalDecl

parseLocalDeclsWith :: P Decl -> P [Decl]
parseLocalDeclsWith parser ts = case ts of
  TLet : _ -> next
  TConstraints : _ -> next
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
  types <- parseBracketTypes contents
  case (reverse types, closing) of
    (result : reversedFields, TRBracket : rest) -> do
      markRole (if null reversedFields then "enumMember" else "function") ["declaration", "constructor"] token
      pure (Ctor name (reverse reversedFields) (Just result), rest)
    _ -> Failed "constructor signature requireþ a result type"
parseCtor ts = do
  (parts, rest) <- takeCtorTokens ts
  (argTerms, name) <- maybeToEither ("invalid constructor '" ++ showTokens parts ++ "' before " ++ showTokenHead rest) (headerFromTokens parts)
  fields <- typesFromHeaderTerms argTerms
  markHeaderName (if null fields then "enumMember" else "function") ["declaration", "constructor"] parts
  pure (Ctor name fields Nothing, rest)

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
parseClassMembers ts@(TRParen : _) = Parsed ([], ts)
parseClassMembers ts = do
  (member, rest) <- parseClassMember ts
  (members, remaining) <- parseClassMembers rest
  pure (member : members, remaining)

parseClassMember :: P ClassMember
parseClassMember (TConstraints : ts) = do
  (constraints, rest) <- parseConstraintPrefix (\case TLet -> True; _ -> False) "expected 'let' after flock member graiþ" ts
  case rest of
    TLet : _ -> parseClassLet constraints rest
    _ -> Failed "expected 'let' after flock member graiþ"
parseClassMember ts@(TLet : _) = parseClassLet [] ts
parseClassMember ts@(TLaw : _) = parseClassLaw ts
parseClassMember _ = Failed "expected flock member"

parseClassLet :: [ClassConstraint] -> P ClassMember
parseClassLet constraints (TLet : ts) = do
  (memberTokens, rest) <- takeTopLevelUntilOrEnd ts isClassMemberBoundary
  member <- parseClassSignature constraints memberTokens
  pure (member, rest)
parseClassLet _ _ = Failed "expected 'let' before flock member"

parseClassSignature :: [ClassConstraint] -> [Token] -> Result ClassMember
parseClassSignature constraints ts = case ts of
  token@(TIdent name) : TLBracket : typeTokens -> do
    markRole "call" ["declaration", "applied"] token
    (header, rest) <- takeTopLevelUntil typeTokens (== TRBracket) "expected ']' after flock member type"
    case rest of
      [TRBracket] -> do
        ty <- parseBareSignature constraints header
        pure (ClassSignature name ty)
      _ -> Failed "unexpected tokens after flock member type"
  _ -> Failed "required flock member type requireþ brackets"

parseBareSignature :: [ClassConstraint] -> [Token] -> Result TypeAnn
parseBareSignature constraints header = do
  -- member signatures have no binders, including type-parameter binders.
  if TAt `elem` header
    then Failed "type parameters must precede signature arguments"
    else pure ()
  (slots, result, effects) <- parseLetBracket False header
  arguments <- traverse bareType slots
  resultType <- maybe (Failed "signature requireþ a result type") Parsed result
  functionLetAnn constraints (map Just arguments) (FunctionResult resultType effects)
  where
    bareType (BareSlot ty) = Parsed ty
    bareType (BoundSlot _ _) = Failed "signature cannot bind arguments"

isClassMemberBoundary :: Token -> Bool
isClassMemberBoundary = \case
  TLet -> True
  TConstraints -> True
  TLaw -> True
  TRParen -> True
  _ -> False

parseClassLaw :: P ClassMember
parseClassLaw (TLaw : TLBracket : ts) = do
  (header, closing) <- takeTopLevelUntil ts (== TRBracket) "expected ']' after law parameters"
  parameters <- splitBracketItems header >>= traverse parseLawBracketParameter
  case closing of
    TRBracket : body -> parseLawBody parameters body
    _ -> Failed "expected ']' after law parameters"
parseClassLaw _ = Failed "expected bracketed law parameters"

parseLawBracketParameter :: [Token] -> Result (Pattern, TypeExpr)
parseLawBracketParameter tokens = do
  slot <- parseHeaderSlot tokens
  case slot of
    BoundSlot pat ty -> pure (pat, ty)
    _ -> Failed "law parameters require typed patterns"

parseLawBody :: [(Pattern, TypeExpr)] -> P ClassMember
parseLawBody parameters body = do
  (leftTokens, separator) <- takeTopLevelUntil body (== TEquals) "expected '=' between law sides"
  (rightTokens, rest) <- case separator of
    TEquals : right -> takeTopLevelUntilOrEnd right isClassMemberBoundary
    _ -> Failed "expected '=' between law sides"
  left <- parseWhole "unexpected tokens on left side of law" parseExpr leftTokens
  right <- parseWhole "unexpected tokens on right side of law" parseExpr rightTokens
  pure (ClassLaw parameters left right, rest)

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
    fn : args -> (,rest) <$> applyParsed fn (value : args)
    [] -> Failed "expected function after '$'"

parseDollarParenSegment :: Expr -> P Expr
parseDollarParenSegment value ts = do
  (terms, rest) <- parseFunctionFirstTerms ts
  case (terms, rest) of
    (fn : args, TRParen : rest2) -> (,rest2) <$> applyParsed fn (value : args)
    ([], _) -> Failed "expected function after '$('"
    (_, _) -> Failed "expected ')' after '$('"

parseFunctionFirstTerms :: P [Expr]
parseFunctionFirstTerms = go []
  where
    go acc ts = case ts of
      [] -> Parsed (reverse acc, [])
      TRParen : _ -> Parsed (reverse acc, ts)
      _ ->
        branch
          (parseExprAtom ts)
          (\(arg, rest) -> go (arg : acc) rest)
          (pure (reverse acc, ts))

parsePostfixExprWith :: Bool -> P Expr
parsePostfixExprWith stopBrace ts = do
  (parts, rest) <- parseExprParts "expected expression atom" stopBrace ts
  defaultApplication parts rest

applyArgs :: Expr -> [Expr] -> Expr
applyArgs = foldl' (\expr arg -> locateAround [expr, arg] (EApply expr (arg :| [])))

-- a lone term denotes itself; otherwise @a f b c@ lowers to @f a b c@.
defaultApplication :: [Expr] -> [Token] -> Result (Expr, [Token])
defaultApplication parts rest = case parts of
  [] -> Failed "expected expression"
  [x] -> Parsed (x, rest)
  arg : f : args -> (,rest) <$> applyParsed f (arg : args)

parseExprParts :: String -> Bool -> P [Expr]
parseExprParts emptyMessage stopBrace = go []
  where
    go acc ts = case ts of
      [] -> Parsed (reverse acc, [])
      TLBrace : _ | stopBrace && not (null acc) -> Parsed (reverse acc, ts)
      t : _ | exprStop t -> Parsed (reverse acc, ts)
      TDollar : _ -> Parsed (reverse acc, ts)
      _ ->
        branch
          (parseExprAtom ts)
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
  TParenKeyword marker : rest -> parseParenKeywordExpr marker rest
  TIdent "r" : TLParen : _ -> Failed "record opener must be written 'r(' without whitespace"
  token@(TIdent name) : rest -> markRole "term" [] token >> pure (EVar name, rest)
  TTry : rest -> parseTry rest
  TMatch : rest -> parseMatchExpr rest
  TLBrace : rest -> parseAnonymousMatchExpr rest
  TLParen : rest -> parseParen rest
  _ -> Failed "expected expression atom"

parseAnonymousMatchExpr :: P Expr
parseAnonymousMatchExpr ts = do
  (cs, rest) <- parseMatchCases ts
  pure (EMatch [] cs, rest)

parseMatchExpr :: P Expr
parseMatchExpr (TLBrace : _) = Failed "match requireþ a scrutinee; use '{ ... }' for a function"
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
parseParen ts@(TConstraints : _) = parseBlock ts
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
    TRParen : rest2 -> pure (EUpdate base [], rest2)
    _ -> Failed "expected ',' or ')' after record update base"

parseRecordUpdates :: P [RecordUpdate]
parseRecordUpdates = parseCommaListUntil isRightParen parseRecordUpdateItem

parseRecordUpdateItem :: P RecordUpdate
parseRecordUpdateItem (TIdent "-" : token@(TIdent name) : rest) = markRole "property" [] token >> pure (RecordRemove name, rest)
parseRecordUpdateItem (token@(TIdent name) : TEquals : rest) = do
  markRole "property" [] token
  (e, rest2) <- parseExpr rest
  pure (RecordSet name e, rest2)
parseRecordUpdateItem _ = Failed "expected record update"

parseRecordExprFields :: P [(String, Expr)]
parseRecordExprFields = parseCommaListUntil isRightParen parseRecordExprField

parseRecordExprField :: P (String, Expr)
parseRecordExprField (token@(TIdent name) : TEquals : rest) = do
  markRole "property" [] token
  (e, rest2) <- parseExpr rest
  pure ((name, e), rest2)
parseRecordExprField _ = Failed "expected record field"

parseParenKeywordExpr :: String -> P Expr
parseParenKeywordExpr "r" = parseRecordExpr
parseParenKeywordExpr marker = parseAssociativeExpr marker

parseAssociativeExpr :: String -> P Expr
parseAssociativeExpr marker ts = do
  (expressions, rest) <- parseCommaListUntil isRightParen parseExpr ts
  case expressions of
    [] -> Failed (marker ++ "(...) requireþ a combining function")
    function : values@(_ : _ : _) -> markFunction function >> pure (associate function values, rest)
    _ -> Failed (marker ++ "(...) requireþ at least two values")
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

parseTypeUntilCommaOrParen :: String -> P TypeExpr
parseTypeUntilCommaOrParen message ts = parseTypeUntilTopLevel ts (\case TComma -> True; TRParen -> True; _ -> False) message

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
  TParenKeyword "r" : rest -> parseRecordType rest
  TIdent "r" : TLParen : _ -> Failed "record opener must be written 'r(' without whitespace"
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
  types <- parseBracketTypes typeTokens
  effects <- maybe (Parsed []) (withEffect . parseBracketTypes) effectTokens
  case reverse types of
    result : argument : remaining -> makeArrowType (reverse (argument : remaining)) effects result
    _ -> Failed "function type requireþ an argument and result"

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
parseRecordTypeFields = parseCommaListUntil isRightParen parseRecordTypeField

parseRecordTypeField :: P (String, TypeExpr)
parseRecordTypeField (token@(TIdent name) : TColon : rest) = do
  markRole "property" [] token
  (t, rest2) <- parseTypeUntilCommaOrParen "unterminated record type" rest
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

isRightBrace, isRightParen :: Token -> Bool
isRightBrace = (== TRBrace)
isRightParen = (== TRParen)

splitTopLevelBang, splitTopLevelColon, splitTopLevelComma :: [Token] -> Maybe ([Token], [Token])
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
          TRBracket -> go (x : acc) (depth - 1) rest
          _ -> go (x : acc) depth rest

-- delimiter-aware slicing keepeþ declaration and type parsers small. callers
-- decide whether reaching the end is valid for their enclosing construct.
takeTopLevelUntil :: [Token] -> (Token -> Bool) -> String -> Result ([Token], [Token])
takeTopLevelUntil ts stop message = case takeTopLevelUntilOrEnd ts stop of
  Parsed (_, []) -> Failed message
  other -> other

takeTopLevelUntilOrEnd :: [Token] -> (Token -> Bool) -> Result ([Token], [Token])
takeTopLevelUntilOrEnd ts stop = go [] (0 :: Int) ts stop
  where
    go acc _ [] _ = Parsed (reverse acc, [])
    go acc depth xs@(x : rest) stop
      | depth == 0 && stop x = Parsed (reverse acc, xs)
      | otherwise = case x of
          _ | isParenthesisOpen x -> go (x : acc) (depth + 1) rest stop
          TRParen -> go (x : acc) (depth - 1) rest stop
          TRBracket -> go (x : acc) (depth - 1) rest stop
          TLBrace -> go (x : acc) (depth + 1) rest stop
          TRBrace -> go (x : acc) (depth - 1) rest stop
          _ -> go (x : acc) depth rest stop

takeBalanced :: [Token] -> Result ([Token], [Token])
takeBalanced = go [] (1 :: Int)
  where
    go _ _ [] = Failed "unclosed parenthesised type"
    go acc depth (x : rest)
      | isParenthesisOpen x = go (x : acc) (depth + 1) rest
    go acc 1 (TRParen : rest) = Parsed (reverse acc, rest)
    go acc depth (TRParen : rest) = go (TRParen : acc) (depth - 1) rest
    go acc depth (TRBracket : rest) = go (TRBracket : acc) (depth - 1) rest
    go acc depth (x : rest) = go (x : acc) depth rest

isParenthesisOpen :: Token -> Bool
isParenthesisOpen = \case
  TLParen -> True
  TLBracket -> True
  TParenKeyword _ -> True
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

applyParsed :: Expr -> [Expr] -> Result Expr
applyParsed function arguments = markFunction function >> pure (applyArgs function arguments)

markFunction :: Expr -> Result ()
markFunction (ELocated span (EVar "eftgin")) = Result (Right ()) (Seq.singleton (SourceRole span "keyword" []))
markFunction (ELocated span (EVar name))
  | name /= "_" = Result (Right ()) (Seq.singleton (SourceRole span "call" ["applied"]))
markFunction (ELocated _ body) = markFunction body
markFunction (EApply function _) = markFunction function
markFunction _ = pure ()

-- highlighting useþ the same lexer and parser, without imports or type checking.
-- after failure, unindented declaration heads bound each recovery parse. a series
-- of unfinished brackets must not repeatedly scan the remaining file.
parseSourceRoles :: String -> [SourceRole]
parseSourceRoles source = case parseTokenStream tokens of
  Result (Right _) events -> toList events
  Result (Left _) events -> toList events ++ foldMap roles (chunks tokens)
  where
    tokens = map locatedToken (lexLocatedPrefix source)
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
