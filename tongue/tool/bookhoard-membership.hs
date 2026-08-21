module Main (main) where

import Tung.Bookhoard.Files (updateBookhoardMembership)

main :: IO ()
main = updateBookhoardMembership "../bookhoard" ".bookhoard-membership"
