{- | compile-time embedding joineth every discovered sibling bookhoard source to
the installed runner without making the bookhoard part of the compiler package.
-}
module Tung.Embed (embedBookhoard) where

import Data.List (sort)
import Language.Haskell.TH.Syntax (Exp, Q, lift, makeRelativeToProject, qAddDependentFile, runIO)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

embedBookhoard :: Q Exp
embedBookhoard = do
  root <- makeRelativeToProject (".." </> "bookhoard")
  names <- runIO (discover root "")
  lift =<< traverse (embed root) names
 where
  embed root name = do
    let path = root </> name
    qAddDependentFile path
    source <- runIO (readFile path)
    pure (".." </> "bookhoard" </> name, name, source)

discover :: FilePath -> FilePath -> IO [FilePath]
discover root relative = do
  -- sorted discovery keepeth the generated expression reproducible across hosts.
  entries <- sort <$> listDirectory (root </> relative)
  concat <$> traverse visit entries
 where
  visit entry = do
    let name = relative </> entry
        path = root </> name
    directory <- doesDirectoryExist path
    if directory
      then discover root name
      else pure [name | takeExtension name == ".tung"]
