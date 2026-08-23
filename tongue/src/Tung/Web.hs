{-# LANGUAGE OverloadedStrings #-}

-- | a bounded, one-request-per-connection http/1.1 transport.
module Tung.Web (
  Request (..),
  Response (..),
  serve,
  validHeaderName,
  validHeaderValue,
) where

import Control.Exception (IOException, bracket, finally, onException, try)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Text.Read (readMaybe)

data Request = Request
  { requestMethod :: Text.Text
  , requestTarget :: Text.Text
  , requestHeaders :: [(Text.Text, Text.Text)]
  , requestBody :: Text.Text
  }

data Response = Response
  { responseStatus :: Integer
  , responseHeaders :: [(Text.Text, Text.Text)]
  , responseBody :: Text.Text
  }

serve :: Integer -> (Request -> IO (Either stop Response)) -> IO stop
serve port respond = Socket.withSocketsDo (bracket (openSocket port) Socket.close loop)
 where
  loop listener = do
    (connection, _) <- Socket.accept listener
    outcome <- tryIOException (handleConnection respond connection `finally` Socket.close connection)
    case outcome of
      Left _ -> loop listener
      Right Nothing -> loop listener
      Right (Just stop) -> pure stop

openSocket :: Integer -> IO Socket.Socket
openSocket port = do
  listener <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  ( do
      Socket.setSocketOption listener Socket.ReuseAddr 1
      Socket.bind listener (Socket.SockAddrInet (fromInteger port) 0)
      Socket.listen listener 128
    )
    `onException` Socket.close listener
  pure listener

handleConnection :: (Request -> IO (Either stop Response)) -> Socket.Socket -> IO (Maybe stop)
handleConnection respond connection = do
  readRequest connection >>= \case
    Left _ -> sendResponse connection (Response 400 [] "bad request\n") >> pure Nothing
    Right request ->
      respond request >>= \case
        Left stop -> pure (Just stop)
        Right response -> sendResponse connection response >> pure Nothing

readRequest :: Socket.Socket -> IO (Either String Request)
readRequest connection =
  receiveHead connection ByteString.empty >>= \case
    Left message -> pure (Left message)
    Right (headBytes, initialBody) -> case parseHead headBytes of
      Left message -> pure (Left message)
      Right (method, target, headers, contentLength) -> do
        body <- receiveBody connection contentLength initialBody
        pure do
          bodyBytes <- body
          bodyText <- decodeUtf8 "body" bodyBytes
          pure Request{requestMethod = method, requestTarget = target, requestHeaders = headers, requestBody = bodyText}

receiveHead :: Socket.Socket -> ByteString.ByteString -> IO (Either String (ByteString.ByteString, ByteString.ByteString))
receiveHead connection buffered
  | ByteString.length buffered > headLimit = pure (Left "http request headers are too large")
  | otherwise = case ByteString.breakSubstring headEnd buffered of
      (headBytes, rest)
        | not (ByteString.null rest) ->
            if ByteString.length headBytes > headLimit
              then pure (Left "http request headers are too large")
              else pure (Right (headBytes, ByteString.drop (ByteString.length headEnd) rest))
      _ -> do
        chunk <- SocketBytes.recv connection 4096
        if ByteString.null chunk
          then pure (Left "http request ended before its headers")
          else receiveHead connection (buffered <> chunk)

receiveBody :: Socket.Socket -> Int -> ByteString.ByteString -> IO (Either String ByteString.ByteString)
receiveBody connection wanted initial = collect (ByteString.take wanted initial)
 where
  collect body
    | ByteString.length body == wanted = pure (Right body)
    | otherwise = do
        chunk <- SocketBytes.recv connection (min 4096 (wanted - ByteString.length body))
        if ByteString.null chunk
          then pure (Left "http request ended before its body")
          else collect (body <> chunk)

parseHead :: ByteString.ByteString -> Either String (Text.Text, Text.Text, [(Text.Text, Text.Text)], Int)
parseHead bytes = case linesCRLF bytes of
  [] -> Left "http request hath no request line"
  requestLine : headerLines -> do
    (method, target) <- parseRequestLine requestLine
    headers <- traverse parseHeader headerLines
    rejectTransferEncoding headers
    contentLength <- requestContentLength headers
    pure (method, target, headers, contentLength)

parseRequestLine :: ByteString.ByteString -> Either String (Text.Text, Text.Text)
parseRequestLine line = case Char8.words line of
  [method, target, version]
    | version `elem` ["HTTP/1.0", "HTTP/1.1"] ->
        (,) <$> decodeUtf8 "method" method <*> decodeUtf8 "target" target
    | otherwise -> Left "unsupported http version"
  _ -> Left "malformed http request line"

parseHeader :: ByteString.ByteString -> Either String (Text.Text, Text.Text)
parseHeader line = case ByteString.break (== 58) line of
  (name, rest)
    | ByteString.null name || ByteString.null rest -> Left "malformed http header"
    | otherwise ->
        let nameText = TextEncoding.decodeLatin1 name
            valueText = TextEncoding.decodeLatin1 (trimWhitespace (ByteString.drop 1 rest))
         in if validHeaderName nameText && validHeaderValue valueText
              then Right (nameText, valueText)
              else Left "invalid http header"

rejectTransferEncoding :: [(Text.Text, Text.Text)] -> Either String ()
rejectTransferEncoding headers = case headerValues "transfer-encoding" headers of
  [] -> Right ()
  [value]
    | Text.toCaseFold value == "identity" -> Right ()
  _ -> Left "transfer-encoded requests are unsupported"

requestContentLength :: [(Text.Text, Text.Text)] -> Either String Int
requestContentLength headers = case headerValues "content-length" headers of
  [] -> Right 0
  [value] -> case readMaybe (Text.unpack value) :: Maybe Integer of
    Just size
      | size >= 0 && size <= toInteger bodyLimit -> Right (fromInteger size)
    _ -> Left "invalid or excessive http content length"
  _ -> Left "duplicate http content length"

headerValues :: Text.Text -> [(Text.Text, Text.Text)] -> [Text.Text]
headerValues wanted headers = [value | (name, value) <- headers, Text.toCaseFold name == wanted]

sendResponse :: Socket.Socket -> Response -> IO ()
sendResponse connection Response{responseStatus, responseHeaders, responseBody} =
  SocketBytes.sendAll connection (ByteString.concat (statusLine : map renderHeader headers ++ ["\r\n", bodyBytes]))
 where
  bodyBytes = TextEncoding.encodeUtf8 responseBody
  statusLine = Char8.pack ("HTTP/1.1 " ++ show responseStatus ++ " " ++ statusReason responseStatus ++ "\r\n")
  kept = filter (not . reserved . fst) responseHeaders
  typed
    | any ((== "content-type") . Text.toCaseFold . fst) kept = kept
    | otherwise = ("Content-Type", "text/plain; charset=utf-8") : kept
  headers =
    ("Content-Length", Text.pack (show (ByteString.length bodyBytes)))
      : ("Connection", "close")
      : typed
  reserved name = Text.toCaseFold name `elem` ["content-length", "connection", "transfer-encoding"]
  renderHeader (name, value) = TextEncoding.encodeUtf8 name <> ": " <> TextEncoding.encodeUtf8 value <> "\r\n"

statusReason :: Integer -> String
statusReason = \case
  200 -> "OK"
  201 -> "Created"
  202 -> "Accepted"
  204 -> "No Content"
  301 -> "Moved Permanently"
  302 -> "Found"
  304 -> "Not Modified"
  400 -> "Bad Request"
  401 -> "Unauthorized"
  403 -> "Forbidden"
  404 -> "Not Found"
  405 -> "Method Not Allowed"
  409 -> "Conflict"
  413 -> "Content Too Large"
  415 -> "Unsupported Media Type"
  429 -> "Too Many Requests"
  500 -> "Internal Server Error"
  501 -> "Not Implemented"
  502 -> "Bad Gateway"
  503 -> "Service Unavailable"
  _ -> "Status"

decodeUtf8 :: String -> ByteString.ByteString -> Either String Text.Text
decodeUtf8 label bytes = case TextEncoding.decodeUtf8' bytes of
  Left _ -> Left ("http " ++ label ++ " is not valid utf-8")
  Right value -> Right value

linesCRLF :: ByteString.ByteString -> [ByteString.ByteString]
linesCRLF = map (ByteString.dropWhileEnd (== 13)) . Char8.split '\n'

trimWhitespace :: ByteString.ByteString -> ByteString.ByteString
trimWhitespace = ByteString.dropWhileEnd horizontal . ByteString.dropWhile horizontal
 where
  horizontal byte = byte == 32 || byte == 9

validHeaderName :: Text.Text -> Bool
validHeaderName name = not (Text.null name) && Text.all valid name
 where
  valid character = character > ' ' && character < '\DEL' && character /= ':'

validHeaderValue :: Text.Text -> Bool
validHeaderValue = not . Text.any (`elem` ['\r', '\n'])

headEnd :: ByteString.ByteString
headEnd = "\r\n\r\n"

headLimit, bodyLimit :: Int
headLimit = 64 * 1024
bodyLimit = 8 * 1024 * 1024

tryIOException :: IO a -> IO (Either IOException a)
tryIOException = try
