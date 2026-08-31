-- structural reuse across scopes and duplicate labels, binders, operations, and members.
module Test.Validate (group) where

import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group =
  Harness.group "validate" $
    map (uncurry validationOk) accepted
      ++ map (uncurry validationErr) rejected
      ++ astCases

accepted :: [(String, String)]
accepted =
  [ ("distinct declarations", "ilk pair { pair } deed pulse { integer pulse: integer } frame a same { let a same: a }")
  , ("explicit aliases override colliding defaults", "use data/list.tung data-list use syntax/list.tung syntax-list")
  , ("explicit aliases distinguish extension-sharing paths", "use a.tung one use a.extra.tung two")
  , ("explicit aliases distinguish dotted directories", "use pkg.one/query.tung one use pkg.two/query.tung two")
  , ("labels may repeat in separate records", "let first = r(x = 1) let second = r(x = 2)")
  , ("handler names may repeat in separate handlers", "yield try (try 1 { fail | 2 }) { fail | 3 }")
  , ("annotated fremmed let", "let plus: integer → integer → integer = 'add-integer' fremmed")
  ]

rejected :: [(String, String)]
rejected =
  [ ("colliding default use aliases", "use data/list.tung use syntax/list.tung")
  , ("extension-sharing paths retain colliding default aliases", "use a.tung use a.extra.tung")
  , ("extensionless use path", "use ground")
  , ("qualified type alias name", "let-ilk left@right = integer")
  , ("qualified data name", "ilk left@right { value }")
  , ("qualified constructor name", "ilk value { left@right }")
  , ("duplicate data parameter", "ilk a bad a { bad }")
  , ("duplicate type alias parameter", "let-ilk a bad a = a")
  , ("duplicate constructor", "ilk bad { same, same }")
  , ("qualified effect name", "deed left@right { integer op: integer }")
  , ("qualified effect operation name", "deed left { integer right@op: integer }")
  , ("duplicate effect parameter", "deed a bad a { a op: a }")
  , ("duplicate effect operation", "deed bad { integer op: integer, integer op: integer }")
  , ("unannotated fremmed let", "let plus = 'add-integer' fremmed")
  , ("nested fremmed marker", "let bad: r(value: integer) = r(value = 'add-integer' fremmed)")
  , ("qualified frame name", "frame a left@right { let a op: a }")
  , ("qualified frame member name", "frame a left { let a right@op: a }")
  , ("duplicate frame parameter", "frame a bad a { let a op: a }")
  , ("duplicate frame member", "frame a bad { let a op: a let a op: a }")
  , ("duplicate frame law parameter", "frame a bad { law (x: a, x: a): x ~ x }")
  , ("duplicate fill member", "fill integer bad { let x op = x let y op = y }")
  , ("duplicate record type field", "let bad: r(x: integer, x: text) = r(x = 1)")
  , ("duplicate record field", "let bad = r(x = 1, x = 2)")
  , ("duplicate handler case", "yield try 1 { fail | 2, fail | 3 }")
  ]

validationOk, validationErr :: String -> String -> Test
validationOk name = validationCase name True
validationErr name = validationCase name False

validationCase :: String -> Bool -> String -> Test
validationCase name shouldPass source = pure $ case parse source of
  Left message -> Just (name ++ ": parse failed: " ++ message)
  Right program -> case (shouldPass, validateProgram program) of
    (True, Left message) -> Just (name ++ ": unexpected rejection: " ++ message)
    (False, Right ()) -> Just (name ++ ": unexpected acceptance")
    _ -> Nothing

astCases :: [Test]
astCases =
  [ pure $ case validateProgram (Program [Export (Import "ground.tung" Nothing)]) of
      Left _ -> Nothing
      Right () -> Just "show use ast: unexpected acceptance"
  ]
