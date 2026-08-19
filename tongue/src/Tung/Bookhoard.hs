{-# LANGUAGE TemplateHaskell #-}

{- | bundled bookhoard paths and loading. paths are source-relative public import
names; this module intentionally provideth no historical aliases. ghc tracketh
each discovered file once this module rebuildeth; changes to the file set require
an explicit rebuild.
-}
module Tung.Bookhoard (
  bookhoardImportFiles,
  readBookhoardImports,
)
where

import Data.Map.Strict qualified as Map
import Tung.Embed (embedBookhoard)

bookhoardImportFiles :: [(FilePath, String)]
bookhoardImportFiles = [(path, name) | (path, name, _) <- bookhoardEntries]

bookhoardEntries :: [(FilePath, String, String)]
bookhoardEntries = $(embedBookhoard)

readBookhoardImports :: IO (Map.Map String String)
readBookhoardImports = pure (Map.fromList [(name, source) | (_, name, source) <- bookhoardEntries])
