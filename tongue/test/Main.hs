module Main where

import Test.Evaluate qualified as Evaluate
import Test.Harness (runGroups)
import Test.Import qualified as Import
import Test.Integration qualified as Integration
import Test.Library qualified as Library
import Test.Name qualified as Name
import Test.Parse qualified as Parse
import Test.Token qualified as Token
import Test.Type qualified as Type
import Test.Validate qualified as Validate

main :: IO ()
main = sequence [Token.group, Parse.group, Validate.group, Import.group, Type.group, Name.group, Evaluate.group, Library.group, Integration.group] >>= runGroups
