module Lox.Token
  ( Token(..)
  , prettyPrint
  ) where

import Prelude
import Data.Either (Either(..))

data Token
  -- Single-character tokens.
  = {- ( -} LeftParen
  | {- ) -} RightParen
  | {- { -} LeftBrace
  | {- } -} RightBrace
  | {- , -} Comma
  | {- . -} Dot
  | {- - -} Minus
  | {- + -} Plus
  | {- ; -} Semicolon
  | {- / -} Slash
  | {- * -} Star
  -- one or two character tokens.
  | {- !  -} Bang
  | {- != -} BangEqual
  | {- =  -} Equal
  | {- == -} EqualEqual
  | {- >  -} Greater
  | {- >= -} GreaterEqual
  | {- <  -} Less
  | {- <= -} LessEqual
  -- literals.
  | Identifier String -- UName
  -- | Quantifier String, type variable name
  -- | Symbol String, user defined operator
  -- | CharLiteral Char
  | String String -- StringLiteral
  | Number (Either Int Number) -- NumberToken (Either Int Number)
  -- keywords. LName
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  -- end of file.
  | EOF

prettyPrint :: Token -> String
prettyPrint = case _ of
  LeftParen -> "("
  RightParen -> ")"
  LeftBrace -> "{"
  RightBrace -> "}"
  Comma -> ","
  Dot -> "."
  Minus -> "-"
  Plus -> "+"
  Semicolon -> ";"
  Slash -> "/"
  Star -> "*"
  Bang -> "!"
  BangEqual -> "!="
  Equal -> "="
  EqualEqual -> "=="
  Greater -> ">"
  GreaterEqual -> ">="
  Less -> "<"
  LessEqual -> "<="
  Identifier x -> show x
  String x -> "String " <> show x
  Number x -> case x of
    Left y -> "Int " <> show y
    Right y -> "Number " <> show y
  And -> "and"
  Class -> "class"
  Else -> "else"
  False -> "false"
  Fun -> "fun"
  For -> "for"
  If -> "if"
  Nil -> "nil"
  Or -> "or"
  Print -> "print"
  Return -> "return"
  Super -> "super"
  This -> "this"
  True -> "true"
  Var -> "var"
  While -> "while"
  EOF -> "EOF"
