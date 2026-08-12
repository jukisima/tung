module Jbini.Library (
  libraryImportFiles,
  readLibraryImports,
)
where

import qualified Data.Map.Strict as Map

libraryImportFiles :: [(FilePath, String)]
libraryImportFiles =
  [ ("library/ground.jbini", "ground.jbini")
  , ("library/_foreign.jbini", "foreign.jbini")
  , ("library/data/zero.jbini", "data/zero.jbini")
  , ("library/data/zero.jbini", "zero.jbini")
  , ("library/data/one.jbini", "data/one.jbini")
  , ("library/data/one.jbini", "one.jbini")
  , ("library/data/two.jbini", "data/two.jbini")
  , ("library/data/two.jbini", "two.jbini")
  , ("library/data/product.jbini", "data/product.jbini")
  , ("library/data/product.jbini", "product.jbini")
  , ("library/data/sum.jbini", "data/sum.jbini")
  , ("library/data/sum.jbini", "sum.jbini")
  , ("library/string.jbini", "string.jbini")
  , ("library/string/to-string.jbini", "string/to-string.jbini")
  , ("library/string/from-string.jbini", "string/from-string.jbini")
  , ("library/equal.jbini", "equal.jbini")
  , ("library/order.jbini", "order.jbini")
  , ("library/algebra/category.jbini", "algebra/category.jbini")
  , ("library/algebra/field.jbini", "algebra/field.jbini")
  , ("library/algebra/group.jbini", "algebra/group.jbini")
  , ("library/algebra/monoid.jbini", "algebra/monoid.jbini")
  , ("library/algebra/ring.jbini", "algebra/ring.jbini")
  , ("library/algebra/semigroup.jbini", "algebra/semigroup.jbini")
  , ("library/algebra/semigroupoid.jbini", "algebra/semigroupoid.jbini")
  , ("library/algebra/semiring.jbini", "algebra/semiring.jbini")
  , ("library/collection/applicative.jbini", "collection/applicative.jbini")
  , ("library/collection/catamorphism.jbini", "collection/catamorphism.jbini")
  , ("library/collection/filter.jbini", "collection/filter.jbini")
  , ("library/collection/functor.jbini", "collection/functor.jbini")
  , ("library/collection/list.jbini", "collection/list.jbini")
  , ("library/collection/list.jbini", "list.jbini")
  , ("library/collection/monad.jbini", "collection/monad.jbini")
  , ("library/collection/size.jbini", "collection/size.jbini")
  , ("library/collection/traverse.jbini", "collection/traverse.jbini")
  , ("library/combinator.jbini", "combinator.jbini")
  , ("library/data/natural.jbini", "data/natural.jbini")
  , ("library/data/natural.jbini", "natural.jbini")
  , ("library/data/option.jbini", "data/option.jbini")
  , ("library/data/option.jbini", "option.jbini")
  , ("library/numeric/absolute.jbini", "numeric/absolute.jbini")
  , ("library/numeric/complex.jbini", "numeric/complex.jbini")
  , ("library/numeric/complex.jbini", "complex.jbini")
  , ("library/numeric/constant.jbini", "numeric/constant.jbini")
  , ("library/numeric/constant.jbini", "constant.jbini")
  , ("library/numeric/elementary.jbini", "numeric/elementary.jbini")
  , ("library/numeric/integer.jbini", "numeric/integer.jbini")
  ]

readLibraryImports :: IO (Map.Map String String)
readLibraryImports =
  Map.fromList <$> traverse readOne libraryImportFiles
 where
  readOne (path, name) = do
    source <- readFile path
    pure (name, source)
