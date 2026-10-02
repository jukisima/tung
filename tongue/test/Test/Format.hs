module Test.Format (group) where

import Data.List (intercalate)
import Test.Harness (Group, Test, expect, expectEq)
import Test.Harness qualified as Harness
import Tung (formatSource)

group :: IO Group
group = Harness.group "format" (map formatCase cases ++ [idempotence, generatedIdempotence])

formatCase :: (String, String, String) -> Test
formatCase (name, source, expected) = expectEq name expected (formatSource source)

idempotence :: Test
idempotence = expect "formatter is idempotent" (all (\(_, source, _) -> isIdempotent source) cases)

generatedIdempotence :: Test
generatedIdempotence = expect "formatter is idempotent for generated structural sources" (all isIdempotent generatedSources)
  where
    generatedSources =
      [ intercalate "\n" (map (prefix ++) declaration) ++ ending
      | declaration <- declarations,
        prefix <- ["", " ", "  ", "\t"],
        ending <- ["", "\n"]
      ]
    declarations =
      [ ["show ilk box [a:*, *] {", "a box", "}"],
        ["let result []", "match value {", "yea | nay ^ 1", "}"],
        ["let handled []", "try risky {", "message fail ^ 0", "}"],
        ["flock identity [a:*] {", "let identity [@a:*, a, a]", "law [@a:*, x:a]", "x identity = x", "}"]
      ]

isIdempotent :: String -> Bool
isIdempotent source = formatSource formatted == formatted
  where
    formatted = formatSource source

cases :: [(String, String, String)]
cases =
  [ ( "braced class body uses one indentation level",
      "show flock add [a:*] {\nlet + [a, a, a]\n}\n",
      "show flock add [a:*] {\n  let + [a, a, a]\n}\n"
    ),
    ( "instance colon beginneþ a declaration after a continued let",
      "let value []\n1\nbizen : ℤ ring {\nlet f - g [] f\n}\n",
      "let value []\n  1\nbizen:ℤ ring {\n  let f - g [] f\n}\n"
    ),
    ( "instance header brackets use one indentation level",
      """
      bizen [
      byzen b ring,
      @a : *
      ] : [a, b] ring {
      let f - g x [] (x f) - (x g)
      }

      """,
      """
      bizen [
        byzen b ring,
        @a:*
      ]:[a, b] ring {
        let f - g x [] (x f) - (x g)
      }

      """
    ),
    ( "type colons have no surrounding spaces",
      """
      let add [x : ℤ, y:  ℤ, ℤ] (x + y: ℤ)
      flock identity [a:*] { law [x : a] x = x }
      let record [r{field : ℤ}] r{field: 1}
      let text [] 'x: y' # note: untouched
      let letter [] `:
      /* note: untouched */

      """,
      """
      let add [x:ℤ, y:ℤ, ℤ] (x + y:ℤ)
      flock identity [a:*] { law [x:a] x = x }
      let record [r{field:ℤ}] r{field:1}
      let text [] 'x: y' # note: untouched
      let letter [] `:
      /* note: untouched */

      """
    ),
    ( "sort use declarations",
      """
      use zebra.tung zebra
      use alpha.tung

      ## a separate use block.
      use delta.tung
      use beta.tung beta

      """,
      """
      use alpha.tung
      use zebra.tung zebra

      ## a separate use block.
      use beta.tung beta
      use delta.tung

      """
    ),
    ( "nested declarations",
      """
      show ilk box [a:*, *] {
      a box
      }
      let text [] '{not a block}' # }

      """,
      """
      show ilk box [a:*, *] {
        a box
      }
      let text [] '{not a block}' # }

      """
    ),
    ( "current application syntax",
      """
      show let filter [x:a option, f:[a, 𝟚], a option] x match {
      a some ^ a f $ if (a some) none
      none ^ none
      }

      """,
      """
      show let filter [x:a option, f:[a, 𝟚], a option] x match {
        a some ^ a f $ if (a some) none
        none ^ none
      }

      """
    ),
    ( "expression continuations",
      """
      let picked []
      match value {
      yea ^ 1,
      nay ^ 0
      }
      let handled []
      try risky {
      yield value ^ value,
      message fail ^ 0
      }
      let identity []
      { x ^ x }

      """,
      """
      let picked []
        match value {
          yea ^ 1,
          nay ^ 0
        }
      let handled []
        try risky {
          yield value ^ value,
          message fail ^ 0
        }
      let identity []
        { x ^ x }

      """
    ),
    ( "local result",
      """
      let main [] (
      let first []
      1 + 2
      let second [] 3
      yield first + second
      )

      """,
      """
      let main [] (
        let first []
          1 + 2
        let second [] 3
        yield first + second
      )

      """
    ),
    ( "nested continuation",
      """
      let fixtures []
      first _*
      (second _*
      (third _* empty))

      let next []
      value

      """,
      """
      let fixtures []
        first _*
          (second _*
            (third _* empty))

      let next []
        value

      """
    ),
    ( "split right side",
      """
      bizen:(float complex) elementary {
      let (a complex b) sine []
      (((0.0 subtract-float b) complex a) exponent)
      sine-from-exponents ((b complex (0.0 subtract-float a)) exponent)
      }

      """,
      """
      bizen:(float complex) elementary {
        let (a complex b) sine []
          (((0.0 subtract-float b) complex a) exponent)
            sine-from-exponents ((b complex (0.0 subtract-float a)) exponent)
      }

      """
    ),
    ( "split annotation and definition",
      """
      show let compose [f0:[a, b; e0], f1:[b, c; e1], [a, c]; e0, e1] { a ^ a f0 $ f1 }

      """,
      """
      show let compose [f0:[a, b; e0], f1:[b, c; e1], [a, c]; e0, e1] { a ^ a f0 $ f1 }

      """
    ),
    ( "body after a multiline header",
      """
      show let identity [value:a, a]
      value

      """,
      """
      show let identity [value:a, a]
        value

      """
    ),
    ( "multiline bracket header keepeþ one indentation level",
      """
      show let lift₃ [
      a₀:a₀ f,
      a₁:a₁ f,
      a₂:a₂ f,
      f:[a₀, a₁, a₂, b; e],
      b f; e]
      >{apply, a₂, a₁, a₀, f pure}

      """,
      """
      show let lift₃ [
        a₀:a₀ f,
        a₁:a₁ f,
        a₂:a₂ f,
        f:[a₀, a₁, a₂, b; e],
        b f; e]
        >{apply, a₂, a₁, a₀, f pure}

      """
    ),
    ( "term type ascription",
      """
      let answer [] (1 + 2:ℤ)

      """,
      """
      let answer [] (1 + 2:ℤ)

      """
    ),
    ( "multiline record",
      """
      let person [r{
      name:text,
      age:ℤ
      }] r{
      name = 'naoki',
      age = 35
      }

      """,
      """
      let person [r{
        name:text,
        age:ℤ
      }] r{
        name = 'naoki',
        age = 35
      }

      """
    ),
    ( "associative sequence",
      """
      let list [] >{
      _*,
      a,
      b,
      empty
      }

      """,
      """
      let list [] >{
        _*,
        a,
        b,
        empty
      }

      """
    ),
    ( "multiline law header",
      """
      flock applicative [f:[*, *]] {
      law [a:a f]
      a apply (identity pure) = a
      law [a:a] a pure = a pure
      }

      """,
      """
      flock applicative [f:[*, *]] {
        law [a:a f]
          a apply (identity pure) = a
        law [a:a] a pure = a pure
      }

      """
    ),
    ( "equals signs in strings and comments",
      """
      let text [] 'a = b' # =
      let next [] 1

      """,
      """
      let text [] 'a = b' # =
      let next [] 1

      """
    ),
    ( "decimal unicode and block comments",
      """
      let result []
      match character {
      `\\32; ^ 1,
      _ ^ 0
      }
      /*
        {
      */
      let next [] 1

      """,
      """
      let result []
        match character {
          `\\32; ^ 1,
          _ ^ 0
        }
      /*
        {
      */
      let next [] 1

      """
    ),
    ( "delimiters in block comments",
      """
      let x [] 1 /* { = */
      let y [] 2
      /*
        {
      */
      let z [] 3

      """,
      """
      let x [] 1 /* { = */
      let y [] 2
      /*
        {
      */
      let z [] 3

      """
    ),
    ("windows newlines", "ilk box [*] {\r\nbox\r\n}\r\n", "ilk box [*] {\n  box\n}\n"),
    ( "blank line endeþ continuation",
      """
      show let value []
      1

      ## the next declaration.
      show let next [] 2

      """,
      """
      show let value []
        1

      ## the next declaration.
      show let next [] 2

      """
    ),
    ( "comment useþ structural indentation",
      """
      flock identity [a:*] {
      law [x:a]
      # the equation.
      x identity = x
      # the next law.
      law [x:a] x identity = x
      }

      """,
      """
      flock identity [a:*] {
        law [x:a]
        # the equation.
          x identity = x
        # the next law.
        law [x:a] x identity = x
      }

      """
    )
  ]
