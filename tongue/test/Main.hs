module Main where

import Test.Bookhoard qualified as Bookhoard
import Test.Core qualified as Core
import Test.Diagnostic qualified as Diagnostic
import Test.Evaluate qualified as Evaluate
import Test.Harness (runGroups)
import Test.Import qualified as Import
import Test.Integration qualified as Integration
import Test.Name qualified as Name
import Test.Parse qualified as Parse
import Test.Project qualified as Project
import Test.Token qualified as Token
import Test.Type qualified as Type
import Test.Validate qualified as Validate

main :: IO ()
main = sequence [Token.group, Parse.group, Validate.group, Import.group, Project.group, Type.group, Name.group, Evaluate.group, Bookhoard.group, Core.group, Diagnostic.group, Integration.group] >>= runGroups
