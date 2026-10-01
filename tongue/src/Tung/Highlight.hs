-- | source ranges classified by the compiler parser. the editor transports
-- these roles and can refine constructor names through its workspace index.
module Tung.Highlight (highlightSource) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Map.Strict qualified as Map
import Tung.Parse (SourceRole (..), parseSourceRoles)
import Tung.Token (SourceSpan (..))

highlightSource :: String -> String
highlightSource source =
  "tung-highlight\n" ++ ByteString.unpack (encode [(start, end, role, modifiers) | (start, (end, role, modifiers)) <- Map.toAscList roles])
  where
    roles = Map.fromList [(spanStart span, (spanEnd span, role, modifiers)) | SourceRole span role modifiers <- parseSourceRoles source]
