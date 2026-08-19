-- import-stack growth, self cycles, and longer cycle routes.
module Test.Import (group) where

import Test.Harness (Group, expectEq)
import Test.Harness qualified as Harness
import Tung (enterImport)

group :: IO Group
group =
  Harness.group
    "import"
    [ expectEq "empty stack accepteth a path" (Right ["a.tung"]) (enterImport [] "a.tung")
    , expectEq "nested path is pushed" (Right ["b.tung", "a.tung"]) (enterImport ["a.tung"] "b.tung")
    , expectEq "self cycle is rejected" (Left "bring cycle: a.tung -> a.tung") (enterImport ["a.tung"] "a.tung")
    , expectEq "long cycle keepeth its route" (Left "bring cycle: a.tung -> b.tung -> a.tung") (enterImport ["b.tung", "a.tung"] "a.tung")
    ]
