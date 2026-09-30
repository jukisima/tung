module Main where

import Test.Bookhoard qualified as Bookhoard
import Test.Core qualified as Core
import Test.Diagnostic qualified as Diagnostic
import Test.Evaluate qualified as Evaluate
import Test.Format qualified as Format
import Test.Harness (runGroups)
import Test.Import qualified as Import
import Test.Integration qualified as Integration
import Test.Library qualified as Library
import Test.Name qualified as Name
import Test.Parse qualified as Parse
import Test.Project qualified as Project
import Test.Token qualified as Token
import Test.Type qualified as Type
import Test.Validate qualified as Validate

main :: IO ()
main =
  runGroups
    [ ("token", Token.group),
      ("parse", Parse.group),
      ("format", Format.group),
      ("validate", Validate.group),
      ("import", Import.group),
      ("project", Project.group),
      ("library", Library.group),
      ("type", Type.group),
      ("name", Name.group),
      ("evaluate", Evaluate.group),
      ("bookhoard", Bookhoard.group),
      ("core", Core.group),
      ("diagnostic", Diagnostic.group),
      ("integration", Integration.group)
    ]
