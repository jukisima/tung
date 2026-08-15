{- | bundled bookhoard paths and loading. paths are source-relative public import
names; this module intentionally provides no historical aliases. keep this
registry equal to the source tree and list only direct, bottom-up dependencies.
-}
module Tung.Bookhoard (
  bookhoardImportFiles,
  readBookhoardImports,
)
where

import Data.Map.Strict qualified as Map

bookhoardImportFiles :: [(FilePath, String)]
bookhoardImportFiles =
  map
    moduleFile
    [ "ground.tung"
    , "_foreign.tung"
    , "system.tung"
    , "clock.tung"
    , "control.tung"
    , "data/zero.tung"
    , "data/one.tung"
    , "data/two.tung"
    , "data/product.tung"
    , "data/sum.tung"
    , "string.tung"
    , "string/to-string.tung"
    , "string/from-string.tung"
    , "equal.tung"
    , "order.tung"
    , "algebra/partial/semigroupoid.tung"
    , "algebra/partial/category.tung"
    , "algebra/partial/groupoid.tung"
    , "algebra/total/magma.tung"
    , "algebra/total/semigroup.tung"
    , "algebra/total/semigroup/additive.tung"
    , "algebra/total/semigroup/multiplicative.tung"
    , "algebra/total/monoid.tung"
    , "algebra/total/monoid/conjunctive.tung"
    , "algebra/total/monoid/disjunctive.tung"
    , "algebra/total/group.tung"
    , "algebra/arithmetic/semiring.tung"
    , "algebra/arithmetic/ring.tung"
    , "algebra/arithmetic/field.tung"
    , "collection/applicative.tung"
    , "collection/catamorphism.tung"
    , "collection/filter.tung"
    , "collection/functor.tung"
    , "collection/monad.tung"
    , "collection/size.tung"
    , "collection/traverse.tung"
    , "combinator.tung"
    , "data/list.tung"
    , "data/natural.tung"
    , "data/option.tung"
    , "numeric/absolute.tung"
    , "numeric/complex.tung"
    , "numeric/constant.tung"
    , "numeric/elementary.tung"
    , "numeric/integer.tung"
    ]

moduleFile :: FilePath -> (FilePath, String)
moduleFile path = ("bookhoard/" ++ path, path)

readBookhoardImports :: IO (Map.Map String String)
readBookhoardImports =
  Map.fromList <$> traverse readOne bookhoardImportFiles
 where
  readOne (path, name) = (name,) <$> readFile path
