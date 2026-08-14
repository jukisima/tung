module Test.Import (group) where

import Jbini (enterImport)
import Test.Harness (Group, expectEq)
import Test.Harness qualified as Harness

group :: IO Group
group =
  Harness.group
    "import"
    [ expectEq "empty stack accepts a path" (Right ["a.tung"]) (enterImport [] "a.tung")
    , expectEq "nested path is pushed" (Right ["b.tung", "a.tung"]) (enterImport ["a.tung"] "b.tung")
    , expectEq "self cycle is rejected" (Left "bring cycle: a.tung -> a.tung") (enterImport ["a.tung"] "a.tung")
    , expectEq "long cycle keeps its route" (Left "bring cycle: a.tung -> b.tung -> a.tung") (enterImport ["b.tung", "a.tung"] "a.tung")
    ]
