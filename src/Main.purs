module Main where

import Prelude
import Data.Either (Either(..))
import Data.String as Data.String
import Effect (Effect)
import Effect.Class.Console as Effect.Class.Console
import Lox.Lexer as Lox.Lexer

main :: Effect Unit
main = do
  Effect.Class.Console.log $ lex "6-3/1"
  Effect.Class.Console.log $ lex " ( 6 - 3 ) / 1 "
  Effect.Class.Console.log
    $ lex
        """
{- test comment 1 -}
add x y = x + y

value = " test string "
"""

lex :: String -> String
lex input =
  "\n" <> show input <> ":\n\n"
    <> case Lox.Lexer.lex input of
        Left parseError -> show parseError
        Right xs -> Data.String.joinWith "\n" <<< map show $ xs
