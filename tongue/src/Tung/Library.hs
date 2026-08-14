module Tung.Library (
  libraryImportFiles,
  readLibraryImports,
)
where

import Data.Map.Strict qualified as Map

libraryImportFiles :: [(FilePath, String)]
libraryImportFiles =
  concat
    [ moduleFile "ground.tung" []
    , moduleFile "_foreign.tung" ["foreign.tung"]
    , moduleFile "data/zero.tung" ["zero.tung"]
    , moduleFile "data/one.tung" ["one.tung"]
    , moduleFile "data/two.tung" ["two.tung"]
    , moduleFile "data/product.tung" ["product.tung"]
    , moduleFile "data/sum.tung" ["sum.tung"]
    , moduleFile "string.tung" []
    , moduleFile "string/to-string.tung" []
    , moduleFile "string/from-string.tung" []
    , moduleFile "equal.tung" []
    , moduleFile "order.tung" []
    , moduleFile "algebra/category.tung" []
    , moduleFile "algebra/field.tung" []
    , moduleFile "algebra/group.tung" []
    , moduleFile "algebra/monoid.tung" []
    , moduleFile "algebra/monoid/conjunctive.tung" []
    , moduleFile "algebra/monoid/disjunctive.tung" []
    , moduleFile "algebra/ring.tung" []
    , moduleFile "algebra/semigroup.tung" []
    , moduleFile "algebra/semigroupoid.tung" []
    , moduleFile "algebra/semiring.tung" []
    , moduleFile "collection/applicative.tung" []
    , moduleFile "collection/catamorphism.tung" []
    , moduleFile "collection/filter.tung" []
    , moduleFile "collection/functor.tung" []
    , moduleFile "collection/list.tung" ["list.tung"]
    , moduleFile "collection/monad.tung" []
    , moduleFile "collection/size.tung" []
    , moduleFile "collection/traverse.tung" []
    , moduleFile "combinator.tung" []
    , moduleFile "data/natural.tung" ["natural.tung"]
    , moduleFile "data/option.tung" ["option.tung"]
    , moduleFile "numeric/absolute.tung" []
    , moduleFile "numeric/complex.tung" ["complex.tung"]
    , moduleFile "numeric/constant.tung" ["constant.tung"]
    , moduleFile "numeric/elementary.tung" []
    , moduleFile "numeric/integer.tung" []
    ]

moduleFile :: FilePath -> [String] -> [(FilePath, String)]
moduleFile path aliases = [("library/" ++ path, name) | name <- path : aliases]

readLibraryImports :: IO (Map.Map String String)
readLibraryImports =
  Map.fromList <$> traverse readOne libraryImportFiles
 where
  readOne (path, name) = (name,) <$> readFile path
