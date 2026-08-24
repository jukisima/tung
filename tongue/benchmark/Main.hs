module Main (main) where

import Control.Exception qualified as Exception
import Control.Monad (forM, replicateM, unless)
import Data.List (find, intercalate, sort)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (allocated_bytes, max_mem_in_use_bytes), getRTSStats, getRTSStatsEnabled)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), die)
import System.FilePath (takeDirectory, (</>))
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Tung

data Config = Config
  { configRuns :: Int
  , configSize :: Int
  }

data BenchmarkCase
  = GeneratedCompile
  | GeneratedEvaluate
  | FibonacciCompile
  | FizzbuzzCompile
  | FizzbuzzEvaluate
  | MultishotCompile
  | MultishotEvaluate
  deriving stock (Bounded, Enum, Eq, Show)

data Sample = Sample
  { sampleElapsedNs :: Word64
  , sampleAllocatedBytes :: Word64
  , samplePeakMemoryBytes :: Word64
  }

data SourceBundle = SourceBundle
  { bundleSource :: String
  , bundleImports :: Map.Map String String
  }

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--sample", name, "--size", sizeText] -> do
      benchmarkCase <- parseCase name
      size <- positiveInteger "size" sizeText
      runChild benchmarkCase size
    _ -> do
      Config{configRuns, configSize} <- parseConfig arguments
      runParent configRuns configSize

parseConfig :: [String] -> IO Config
parseConfig = go (Config 3 1000)
 where
  go config [] = pure config
  go config ("--runs" : value : rest) = do
    runs <- positiveInteger "runs" value
    go config{configRuns = runs} rest
  go config ("--size" : value : rest) = do
    size <- positiveInteger "size" value
    go config{configSize = size} rest
  go _ _ = die "usage: tung-benchmark [--runs positive-integer] [--size positive-integer]"

positiveInteger :: String -> String -> IO Int
positiveInteger label value = case readMaybe value of
  Just number | number > 0 -> pure number
  _ -> die (label ++ " must be a positive integer")

runParent :: Int -> Int -> IO ()
runParent runs size = do
  executable <- getExecutablePath
  putStrLn ("tung benchmark: " ++ show runs ++ " runs, " ++ show size ++ " generated declarations")
  putStrLn "case                       runs    median ms       min ms  median allocated mib   median peak memory mib"
  rows <- forM allCases \benchmarkCase -> do
    samples <- replicateM runs (readChild executable benchmarkCase size)
    pure (benchmarkCase, samples)
  mapM_ printRow rows

allCases :: [BenchmarkCase]
allCases = [minBound .. maxBound]

readChild :: FilePath -> BenchmarkCase -> Int -> IO Sample
readChild executable benchmarkCase size = do
  (exitCode, output, errors) <-
    readProcessWithExitCode executable ["--sample", caseName benchmarkCase, "--size", show size] ""
  case exitCode of
    ExitSuccess -> case findSample benchmarkCase output of
      Just sample -> pure sample
      Nothing -> die ("benchmark child returned no sample for " ++ caseName benchmarkCase ++ ":\n" ++ output)
    ExitFailure code ->
      die
        ( "benchmark child failed for "
            ++ caseName benchmarkCase
            ++ " with exit "
            ++ show code
            ++ ":\n"
            ++ errors
            ++ output
        )

findSample :: BenchmarkCase -> String -> Maybe Sample
findSample benchmarkCase output = findParsed (lines output)
 where
  findParsed [] = Nothing
  findParsed (line : rest) = case parseSample benchmarkCase line of
    Just sample -> Just sample
    Nothing -> findParsed rest

parseSample :: BenchmarkCase -> String -> Maybe Sample
parseSample benchmarkCase line = case splitTabs line of
  ["tung-benchmark-sample", name, elapsed, allocated, peak]
    | name == caseName benchmarkCase ->
        Sample <$> readMaybe elapsed <*> readMaybe allocated <*> readMaybe peak
  _ -> Nothing

splitTabs :: String -> [String]
splitTabs value = case break (== '\t') value of
  (field, []) -> [field]
  (field, _ : rest) -> field : splitTabs rest

printRow :: (BenchmarkCase, [Sample]) -> IO ()
printRow (benchmarkCase, samples) =
  printf
    "%-26s %4d %12.3f %12.3f %21.3f %21.3f\n"
    (caseName benchmarkCase)
    (length samples)
    (nanosecondsToMilliseconds (median (sampleElapsedNs <$> samples)))
    (nanosecondsToMilliseconds (minimum (sampleElapsedNs <$> samples)))
    (bytesToMebibytes (median (sampleAllocatedBytes <$> samples)))
    (bytesToMebibytes (median (samplePeakMemoryBytes <$> samples)))

median :: [Word64] -> Word64
median values = sort values !! (length values `div` 2)

nanosecondsToMilliseconds :: Word64 -> Double
nanosecondsToMilliseconds value = fromIntegral value / 1000000

bytesToMebibytes :: Word64 -> Double
bytesToMebibytes value = fromIntegral value / (1024 * 1024)

runChild :: BenchmarkCase -> Int -> IO ()
runChild benchmarkCase size = do
  enabled <- getRTSStatsEnabled
  unless enabled (die "rts statistics are disabled; run the benchmark with +RTS -T")
  action <- prepareAction benchmarkCase size
  performMajorGC
  before <- getRTSStats
  started <- getMonotonicTimeNSec
  action
  finished <- getMonotonicTimeNSec
  performMajorGC
  after <- getRTSStats
  putStrLn
    ( intercalate
        "\t"
        [ "tung-benchmark-sample"
        , caseName benchmarkCase
        , show (finished - started)
        , show (allocated_bytes after - allocated_bytes before)
        , show (max_mem_in_use_bytes after)
        ]
    )

prepareAction :: BenchmarkCase -> Int -> IO (IO ())
prepareAction benchmarkCase size = case benchmarkCase of
  GeneratedCompile -> pure (compileAndForce False (generatedBundle size))
  GeneratedEvaluate -> evaluateAction False (generatedBundle size)
  FibonacciCompile -> loadByspel "benchmark/fibonacci.tung" >>= pure . compileAndForce True
  FizzbuzzCompile -> loadByspel "byspel/fizzbuzz.tung" >>= pure . compileAndForce True
  FizzbuzzEvaluate -> loadByspel "byspel/fizzbuzz.tung" >>= evaluateAction True
  MultishotCompile -> loadByspel "byspel/multishot.tung" >>= pure . compileAndForce True
  MultishotEvaluate -> loadByspel "byspel/multishot.tung" >>= evaluateAction True

generatedBundle :: Int -> SourceBundle
generatedBundle size =
  SourceBundle
    { bundleSource =
        unlines
          ( ["let value0: integer = 0"]
              ++ ["let value" ++ show index ++ ": integer = " ++ show index | index <- [1 .. size - 1]]
              ++ ["yield value" ++ show (size - 1)]
          )
    , bundleImports = Map.empty
    }

loadByspel :: FilePath -> IO SourceBundle
loadByspel relativePath = do
  root <- findRepositoryRoot
  bookhoard <- readBookhoardImports
  loaded <- loadProjectFile bookhoard (root </> relativePath)
  case loaded of
    Left message -> die ("could not load " ++ relativePath ++ ": " ++ message)
    Right Project{projectSource, projectImports} -> pure (SourceBundle projectSource projectImports)

findRepositoryRoot :: IO FilePath
findRepositoryRoot = getCurrentDirectory >>= search
 where
  search directory = do
    hathBenchmark <- doesFileExist (directory </> "benchmark/fibonacci.tung")
    hathCabal <- doesFileExist (directory </> "tongue/tung.cabal")
    if hathBenchmark && hathCabal
      then pure directory
      else
        let parent = takeDirectory directory
         in if parent == directory
              then die "could not find the tung repository root"
              else search parent

compileAndForce :: Bool -> SourceBundle -> IO ()
compileAndForce runnable bundle = compileBundle runnable bundle >>= forceCore

evaluateAction :: Bool -> SourceBundle -> IO (IO ())
evaluateAction runnable bundle = do
  program <- compileBundle runnable bundle
  forceCore program
  pure do
    result <-
      if runnable
        then evaluateMainCoreProgram program
        else evaluateCoreProgram program
    Exception.evaluate (length result)
    unless ("eval ok: " `prefixOf` result) (die result)

compileBundle :: Bool -> SourceBundle -> IO CoreProgram
compileBundle runnable SourceBundle{bundleSource, bundleImports} =
  case parse bundleSource >>= elaborate of
    Left message -> die ("benchmark compilation failed: " ++ message)
    Right program -> pure program
 where
  elaborate program
    | runnable = elaborateProgramWithImports program bundleImports True
    | otherwise = elaborateInteractiveProgramWithImports program bundleImports

forceCore :: CoreProgram -> IO ()
forceCore program = Exception.evaluate (length (show program)) >> pure ()

prefixOf :: String -> String -> Bool
prefixOf prefix value = take (length prefix) value == prefix

caseName :: BenchmarkCase -> String
caseName benchmarkCase = case benchmarkCase of
  GeneratedCompile -> "generated/compile"
  GeneratedEvaluate -> "generated/evaluate"
  FibonacciCompile -> "fibonacci/compile"
  FizzbuzzCompile -> "fizzbuzz/compile"
  FizzbuzzEvaluate -> "fizzbuzz/evaluate"
  MultishotCompile -> "multishot/compile"
  MultishotEvaluate -> "multishot/evaluate"

parseCase :: String -> IO BenchmarkCase
parseCase name = case find ((== name) . caseName) allCases of
  Just benchmarkCase -> pure benchmarkCase
  Nothing -> die ("unknown benchmark case '" ++ name ++ "'")
