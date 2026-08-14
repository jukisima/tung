module Test.Harness (
  Group,
  Test,
  group,
  runGroups,
  expect,
  expectEq,
  expectPrefix,
  parseOk,
  parseErr,
  typeOk,
  typeErr,
  typeOkWith,
  typeErrWith,
  runnableOk,
  runnableErr,
  evalOk,
  evalOkWith,
  evalErr,
  evalErrWith,
)
where

import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Jbini
import System.Exit (exitFailure)

type Test = IO (Maybe String)

data Group = Group String [Test]

group :: String -> [Test] -> IO Group
group name tests = pure (Group name tests)

runGroups :: [Group] -> IO ()
runGroups groups = do
  failures <- concat <$> traverse runGroup groups
  if null failures
    then putStrLn ("ok (" ++ show (sum [length tests | Group _ tests <- groups]) ++ " tests)")
    else mapM_ putStrLn failures >> exitFailure
 where
  runGroup (Group name tests) = do
    results <- sequence tests
    pure [name ++ "/" ++ failure | Just failure <- results]

expect :: String -> Bool -> Test
expect name passed = pure $ if passed then Nothing else Just name

expectEq :: (Eq a, Show a) => String -> a -> a -> Test
expectEq name expected actual =
  expectMessage name (expected == actual) ("expected " ++ show expected ++ ", got " ++ show actual)

expectPrefix :: String -> String -> String -> Test
expectPrefix name prefix actual =
  expectMessage name (prefix `isPrefixOf` actual) ("expected prefix " ++ show prefix ++ ", got " ++ show actual)

parseOk, parseErr, typeOk, typeErr, runnableOk, runnableErr :: String -> String -> Test
parseOk name = expectEither name True . parse
parseErr name = expectEither name False . parse
typeOk name = expectEq name "type ok" . check
typeErr name = expectPrefix name "type error:" . check
runnableOk name = expectEq name "type ok" . (`checkRunnableWithImports` Map.empty)
runnableErr name = expectPrefix name "type error:" . (`checkRunnableWithImports` Map.empty)

typeOkWith, typeErrWith :: String -> String -> Map.Map String String -> Test
typeOkWith name source = expectEq name "type ok" . checkWithImports source
typeErrWith name source = expectPrefix name "type error:" . checkWithImports source

evalOk :: String -> String -> String -> Test
evalOk name source expected = evaluate source >>= expectEq name expected

evalOkWith :: String -> String -> Map.Map String String -> String -> Test
evalOkWith name source imports expected = evaluateWithImports source imports >>= expectEq name expected

evalErr :: String -> String -> Test
evalErr name source = evaluate source >>= expectPrefix name "eval error:"

evalErrWith :: String -> String -> Map.Map String String -> Test
evalErrWith name source imports = evaluateWithImports source imports >>= expectPrefix name "eval error:"

expectEither :: String -> Bool -> Either String a -> Test
expectEither name wantRight result =
  expectMessage name (wantRight == either (const False) (const True) result) $ case result of
    Left message -> "unexpected rejection: " ++ message
    Right _ -> "unexpected acceptance"

expectMessage :: String -> Bool -> String -> Test
expectMessage name passed detail = pure $ if passed then Nothing else Just (name ++ ": " ++ detail)
