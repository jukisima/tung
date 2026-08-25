{- | compile-time embedding joineþ every discovered sibling bookhoard source to
the installed runner without making the bookhoard part of the compiler package.
-}
module Tung.Embed (embedBookhoard) where

import Control.Monad (unless)
import Language.Haskell.TH.Syntax (Exp, Q, lift, makeRelativeToProject, qAddDependentFile, runIO)
import System.FilePath ((</>))
import Tung.Bookhoard.Files (bookhoardMembership, discoverBookhoard)

embedBookhoard :: Q Exp
embedBookhoard = do
  root <- makeRelativeToProject (".." </> "bookhoard")
  membership <- makeRelativeToProject ".bookhoard-membership"
  names <- runIO (discoverBookhoard root)
  qAddDependentFile membership
  registered <- runIO (readFile membership)
  expected <- runIO (bookhoardMembership root)
  unless (registered == expected) (fail "bookhoard sources changed; run 'make bookhoard-membership'")
  lift =<< traverse (embed root) names
 where
  embed root name = do
    let path = root </> name
    qAddDependentFile path
    source <- runIO (readFile path)
    pure (".." </> "bookhoard" </> name, name, source)
