-- shared assertions preserve the boundary between static evaluator rejection and
-- deliberate runtime failure from an unhandled deed.
module Test.Harness
  ( Group,
    Test,
    group,
    runGroups,
    expect,
    expectEq,
    expectPrefix,
    propertyTest,
    parseOk,
    parseErr,
    typeOk,
    typeErr,
    typeErrContaining,
    typeOkWith,
    typeErrWith,
    runnableOk,
    runnableErr,
    evalOk,
    evalOkWith,
    evalTypeErr,
    evalTypeErrWith,
  )
where

import Control.Monad (foldM, forM_, when)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict qualified as Map
import System.Environment qualified as Environment
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (hFlush, stdout)
import System.Process (createProcess, env, proc, waitForProcess)
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Random qualified as QuickCheck
import Tung

type Test = IO (Maybe String)

data Group = Group String [Test]

group :: String -> [Test] -> IO Group
group name tests = pure (Group name tests)

runGroups :: [(String, IO Group)] -> IO ()
runGroups groups = do
  progress <- (== Just "1") <$> Environment.lookupEnv "TUNG_TEST_PROGRESS"
  selected <- Environment.lookupEnv "TUNG_TEST_GROUP"
  case selected of
    Nothing -> do
      executable <- Environment.getExecutablePath
      environment <- Environment.getEnvironment
      forM_ (map fst groups) $ \name -> do
        let childEnvironment = ("TUNG_TEST_GROUP", name) : filter ((/= "TUNG_TEST_GROUP") . fst) environment
        (_, _, _, child) <- createProcess (proc executable []) {env = Just childEnvironment}
        waitForProcess child >>= \case
          ExitSuccess -> pure ()
          failure -> exitWith failure
    Just names -> go progress 0 [] [action | (name, action) <- groups, name `elem` words names]
  where
    go _ count failures [] =
      if null failures
        then putStrLn ("ok (" ++ show count ++ " tests)")
        else mapM_ putStrLn (reverse failures) >> exitFailure
    go progress count failures (action : rest) = do
      Group name tests <- action
      when progress (putStrLn ("running " ++ name) >> hFlush stdout)
      found <- foldM (runTest progress name) failures (zip [1 :: Int ..] tests)
      let nextCount = count + length tests
      nextCount `seq` go progress nextCount found rest
    runTest progress name failures (index, test) = do
      when progress (putStrLn (name ++ "/" ++ show index) >> hFlush stdout)
      result <- test
      pure $ case result of
        Nothing -> failures
        Just failure -> (name ++ "/" ++ failure) : failures

expect :: String -> Bool -> Test
expect name passed = pure $ if passed then Nothing else Just name

expectEq :: (Eq a, Show a) => String -> a -> a -> Test
expectEq name expected actual =
  expectMessage name (expected == actual) ("expected " ++ show expected ++ ", got " ++ show actual)

expectPrefix :: String -> String -> String -> Test
expectPrefix name prefix actual =
  expectMessage name (prefix `isPrefixOf` actual) ("expected prefix " ++ show prefix ++ ", got " ++ show actual)

propertyTest :: (QuickCheck.Testable property) => String -> property -> Test
propertyTest name property = do
  result <-
    QuickCheck.quickCheckWithResult
      QuickCheck.stdArgs
        { QuickCheck.chatty = False,
          QuickCheck.maxSuccess = 50,
          QuickCheck.replay = Just (QuickCheck.mkQCGen 20260820, 0)
        }
      property
  pure case result of
    QuickCheck.Success {} -> Nothing
    _ -> Just (name ++ ": " ++ QuickCheck.output result)

parseOk, parseErr, typeOk, typeErr, runnableOk, runnableErr :: String -> String -> Test
parseOk name = expectEither name True . parse
parseErr name = expectEither name False . parse
typeOk name = expectEq name "type ok" . check
typeErr name = expectPrefix name "type error:" . check

typeErrContaining :: String -> String -> String -> Test
typeErrContaining name expected source =
  let actual = check source
   in expectMessage name ("type error:" `isPrefixOf` actual && expected `isInfixOf` actual) ("expected error containing " ++ show expected ++ ", got " ++ show actual)

runnableOk name = expectEq name "type ok" . (`checkRunnableWithImports` Map.empty)

runnableErr name = expectPrefix name "type error:" . (`checkRunnableWithImports` Map.empty)

typeOkWith, typeErrWith :: String -> String -> Map.Map String String -> Test
typeOkWith name source = expectEq name "type ok" . checkWithImports source
typeErrWith name source = expectPrefix name "type error:" . checkWithImports source

evalOk :: String -> String -> String -> Test
evalOk name source expected = evaluate source >>= expectEq name expected

evalOkWith :: String -> String -> Map.Map String String -> String -> Test
evalOkWith name source imports expected = evaluateWithImports source imports >>= expectEq name expected

evalTypeErr :: String -> String -> Test
evalTypeErr name source = evaluate source >>= expectPrefix name "eval error: type error:"

evalTypeErrWith :: String -> String -> Map.Map String String -> Test
evalTypeErrWith name source imports = evaluateWithImports source imports >>= expectPrefix name "eval error: type error:"

expectEither :: String -> Bool -> Either String a -> Test
expectEither name wantRight result =
  expectMessage name (wantRight == either (const False) (const True) result) $ case result of
    Left message -> "unexpected rejection: " ++ message
    Right _ -> "unexpected acceptance"

expectMessage :: String -> Bool -> String -> Test
expectMessage name passed detail = pure $ if passed then Nothing else Just (name ++ ": " ++ detail)
