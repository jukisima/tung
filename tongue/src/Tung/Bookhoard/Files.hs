-- | deterministic discovery and membership tracking for bundled tung sources.
module Tung.Bookhoard.Files (
  discoverBookhoard,
  bookhoardMembership,
  updateBookhoardMembership,
)
where

import Control.Monad (unless)
import Data.List (sort)
import GHC.Fingerprint (getFileHash)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

discoverBookhoard :: FilePath -> IO [FilePath]
discoverBookhoard root = discover ""
 where
  discover relative = do
    entries <- sort <$> listDirectory (root </> relative)
    concat <$> traverse (visit relative) entries
  visit relative entry = do
    let name = relative </> entry
    directory <- doesDirectoryExist (root </> name)
    if directory
      then discover name
      else pure [name | takeExtension name == ".tung"]

bookhoardMembership :: FilePath -> IO String
bookhoardMembership root = do
  names <- discoverBookhoard root
  unlines <$> traverse entry names
 where
  entry name = do
    fingerprint <- getFileHash (root </> name)
    pure (name ++ "\t" ++ show fingerprint)

updateBookhoardMembership :: FilePath -> FilePath -> IO ()
updateBookhoardMembership root membership = do
  content <- bookhoardMembership root
  exists <- doesFileExist membership
  previous <- if exists then readFile membership else pure ""
  let unchanged = length previous `seq` (exists && previous == content)
  unless unchanged (writeFile membership content)
