{-# LANGUAGE TemplateHaskell #-}

{- | bundled bookhoard paths and loading. paths are source-relative public import
names; this module intentionally provideþ no historical aliases. ghc trackeþ
each discovered file and a build-refreshed directory-membership manifest.
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
