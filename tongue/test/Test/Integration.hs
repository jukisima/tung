-- runnable byspels and host-backed effects cross the full compiler boundary here.
module Test.Integration (group) where

import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryReadMVar)
import Control.Exception (IOException, SomeException, bracket, finally, try)
import Control.Monad (filterM, when)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath (takeExtension)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcess)
import System.Timeout qualified as Timeout
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  imports <- readLibraryImports "." >>= either fail pure
  byspels <- byspelFiles
  benchmarks <- benchmarkFiles
  Harness.group "integration" $
    map (runnableFileCase imports) (byspels ++ benchmarks)
      ++ [mainEntry imports, rejectedMain imports, fibonacciSampleResult imports, fileRoundTrip imports, webServerRoundTrip imports]

byspelFiles :: IO [FilePath]
byspelFiles = tungFiles "../byspel"

benchmarkFiles :: IO [FilePath]
benchmarkFiles = tungFiles "../benchmark"

tungFiles :: FilePath -> IO [FilePath]
tungFiles directory = do
  -- the working tree may contain drafts without a runnable main entry.
  entries <- lines <$> readProcess "git" ["ls-files", "--cached", "--", directory] ""
  sort <$> filterM doesFileExist [entry | entry <- entries, takeExtension entry == ".tung"]

runnableFileCase :: Imports -> FilePath -> Test
runnableFileCase imports path = do
  source <- readFile path
  pure $ case checkRunnableWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("runnable file " ++ path ++ ": " ++ actual)

mainEntry :: Imports -> Test
mainEntry imports = do
  actual <- evaluateMainWithImports "use ground.tung let main [_: 𝟙, 𝟙] only yield 42" imports
  pure $ if actual == "eval ok: only" then Nothing else Just ("main entry: " ++ actual)

rejectedMain :: Imports -> Test
rejectedMain imports = do
  actual <- evaluateMainWithImports "use ground.tung let main [_: 𝟙, 𝟙] missing" imports
  pure $ if "type error:" `isPrefixOf` actual then Nothing else Just ("rejected main reached evaluation: " ++ actual)

fibonacciSampleResult :: Imports -> Test
fibonacciSampleResult imports = do
  source <- readFile "../benchmark/fibonacci.tung"
  actual <- evaluateWithImports (source ++ "\nyield 20 fibonacci") imports
  pure $ if actual == "eval ok: 6765" then Nothing else Just ("fibonacci 20: " ++ actual)

fileRoundTrip :: Imports -> Test
fileRoundTrip imports = do
  directory <- getTemporaryDirectory
  let path = directory ++ "/tung-file-effect-test~txt"
      source = "use ground.tung let _ = '" ++ path ++ "' write-file 'hello' let _ = '" ++ path ++ "' append-file ' world' yield '" ++ path ++ "' read-file"
  removeIfPresent path
  actual <- evaluateWithImports source imports
  removeIfPresent path
  pure $ if actual == "eval ok: 'hello world'" then Nothing else Just ("file round trip: " ++ actual)

webServerRoundTrip :: Imports -> Test
webServerRoundTrip imports = do
  port <- unusedPort
  let source =
        """
        use ground.tung
        use deed/clock.tung
        use ilk/option.tung
        use ilk/table.tung
        use web/server.tung

        let route [incoming: request, response; clock] (
          let stamp = only unix-time
          yield match ((incoming request-meþod) ≡ 'POST') ∧ ((incoming request-target) ≡ '/echo') {
            yea ^ match ((incoming request-headers) table~lookup 'Host') {
              host option~some ^ match host ≡ 'localhost' {
                yea ^ ((((incoming request-body) ok) wiþ-header 'Transfer-Encoding' 'chunked') wiþ-header 'X-Tung' 'old') wiþ-header 'X-Tung' 'yea',
                nay ^ 'not found\\n' not-found
              },
              _ ^ 'not found\\n' not-found
            },
            nay ^ 'not found\\n' not-found
          }
        )

        let main [_: 𝟙, 𝟙; clock, web] try
        """
          ++ " "
          ++ show port
          ++ " serve route { _ fail ^ only }"
  completed <- newEmptyMVar
  server <- forkFinally (evaluateMainWithImports source imports) (putMVar completed)
  -- type checking and evaluator startup share this budget with the round trip.
  response <- Timeout.timeout 30000000 (requestEventually completed port) `finally` killThread server
  pure case response of
    Nothing -> Just "web server round trip timed out"
    Just (Left message) -> Just message
    Just (Right bytes)
      | all ((`ByteString.isInfixOf` bytes) . Char8.pack) ["HTTP/1.1 200 OK", "Content-Length: 16", "X-Tung: yea", "\r\n\r\nhello from tung\n"]
          && not (any ((`ByteString.isInfixOf` bytes) . Char8.pack) ["Transfer-Encoding:", "X-Tung: old"]) ->
          Nothing
      | otherwise -> Just ("web server returned " ++ show bytes)

unusedPort :: IO Integer
unusedPort = Socket.withSocketsDo $ bracket open Socket.close $ \listener -> do
  Socket.bind listener (Socket.SockAddrInet 0 loopback)
  Socket.getSocketName listener >>= \case
    Socket.SockAddrInet port _ -> pure (toInteger port)
    _ -> fail "expected an ipv4 test socket"
  where
    open = Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol

requestEventually :: MVar (Either SomeException String) -> Integer -> IO (Either String ByteString.ByteString)
requestEventually completed port =
  tryIOException (requestOnce port) >>= \case
    Right response -> pure (Right response)
    Left exception
      | isDoesNotExistError exception ->
          tryReadMVar completed >>= \case
            Just (Left failure) -> pure (Left ("web server crashed before accepting a connection: " ++ show failure))
            Just (Right result) -> pure (Left ("web server exited before accepting a connection: " ++ result))
            Nothing -> threadDelay 20000 >> requestEventually completed port
      | otherwise -> pure (Left ("web server request failed: " ++ show exception))

requestOnce :: Integer -> IO ByteString.ByteString
requestOnce port = Socket.withSocketsDo $ bracket open Socket.close $ \connection -> do
  Socket.connect connection (Socket.SockAddrInet (fromInteger port) loopback)
  SocketBytes.sendAll connection (Char8.pack "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 16\r\n\r\nhello from tung\n")
  receiveAll connection ByteString.empty
  where
    open = Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol

receiveAll :: Socket.Socket -> ByteString.ByteString -> IO ByteString.ByteString
receiveAll connection received =
  SocketBytes.recv connection 4096 >>= \chunk ->
    if ByteString.null chunk
      then pure received
      else receiveAll connection (received <> chunk)

loopback :: Socket.HostAddress
loopback = Socket.tupleToHostAddress (127, 0, 0, 1)

tryIOException :: IO a -> IO (Either IOException a)
tryIOException = try

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = doesFileExist path >>= \present -> when present (removeFile path)

type Imports = Map.Map String String
