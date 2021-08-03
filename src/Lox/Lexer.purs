module Lox.Lexer where

import Prelude
import Control.Alt ((<|>))
import Control.Plus as Control.Plus
import Data.Array as Data.Array
import Data.CodePoint.Unicode as Data.CodePoint.Unicode
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List as Data.List
import Data.String.CodePoints as Data.String.CodePoints
import Data.String.CodeUnits as Data.String.CodeUnits
import Lox.Token as Lox.Token
import Text.Parsing.Parser as Text.Parsing.Parser
import Text.Parsing.Parser.Combinators as Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Pos as Text.Parsing.Parser.Pos
import Text.Parsing.Parser.String as Text.Parsing.Parser.String
import Text.Parsing.Parser.Token as Text.Parsing.Parser.Token

newtype ParseStateWrapper
  = ParseStateWrapper
  { parseState :: Text.Parsing.Parser.ParseState String
  , tokens :: Data.List.List PositionedToken
  }

newtype PositionedToken
  = PositionedToken
  { sourcePos :: Text.Parsing.Parser.Pos.Position
  , token :: Lox.Token.Token
  , comments :: Array Comment
  }

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken posTok) =
    "PositionedToken { ptSourcePos = " <> show posTok.sourcePos
      <> " , ptToken = "
      <> Lox.Token.prettyPrint posTok.token
      <> " , ptComments = "
      <> show posTok.comments
      <> " }"

data Comment
  = LineComment String
  | BlockComment String

instance showComment :: Show Comment where
  show = case _ of
    LineComment x -> "LineComment " <> show x
    BlockComment x -> "BlockComment " <> show x

lex :: String -> Either Text.Parsing.Parser.ParseError (Array PositionedToken)
lex s = Text.Parsing.Parser.runParser s parseTokens

parseTokens :: Text.Parsing.Parser.Parser String (Array PositionedToken)
parseTokens =
  whitespace
    *> Data.Array.many parsePositionedToken
    <* Text.Parsing.Parser.Combinators.skipMany parseComment
    <* Text.Parsing.Parser.String.eof

parsePositionedToken :: Text.Parsing.Parser.Parser String PositionedToken
parsePositionedToken = do
  comments <- Data.Array.many parseComment
  pos <- Text.Parsing.Parser.position
  token <- parseToken
  pure (PositionedToken { comments, sourcePos: pos, token })

parseComment :: Text.Parsing.Parser.Parser String Comment
parseComment =
  ( Text.Parsing.Parser.Combinators.choice
      [ BlockComment <$> blockComment
      , LineComment <$> lineComment
      ]
  )
    <* whitespace
  where
  blockComment :: Text.Parsing.Parser.Parser String String
  blockComment =
    Text.Parsing.Parser.Combinators.try
      $ Text.Parsing.Parser.String.string "{-"
      *> manyTillString
          ( Text.Parsing.Parser.Combinators.try
              (Text.Parsing.Parser.String.string "-}")
          )

  lineComment :: Text.Parsing.Parser.Parser String String
  lineComment =
    Text.Parsing.Parser.Combinators.try
      $ Text.Parsing.Parser.String.string "--"
      *> manyTillString
          ( Text.Parsing.Parser.Combinators.try
              ( Text.Parsing.Parser.Combinators.choice
                  [ void (Text.Parsing.Parser.String.char '\n')
                  , Text.Parsing.Parser.String.eof
                  ]
              )
          )

parseToken :: Text.Parsing.Parser.Parser String Lox.Token.Token
parseToken =
  Text.Parsing.Parser.Combinators.choice
    [ Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "!=" Lox.Token.BangEqual
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "==" Lox.Token.EqualEqual
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy ">=" Lox.Token.GreaterEqual
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "<=" Lox.Token.LessEqual
    , Text.Parsing.Parser.Combinators.try
        $ parseString "(" Lox.Token.LeftParen
    , Text.Parsing.Parser.Combinators.try
        $ parseString ")" Lox.Token.RightParen
    , Text.Parsing.Parser.Combinators.try
        $ parseString "{" Lox.Token.LeftBrace
    , Text.Parsing.Parser.Combinators.try
        $ parseString "}" Lox.Token.RightBrace
    , Text.Parsing.Parser.Combinators.try
        $ parseString "," Lox.Token.Comma
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "." Lox.Token.Dot
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "-" Lox.Token.Minus
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "+" Lox.Token.Plus
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy ";" Lox.Token.Semicolon
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "/" Lox.Token.Slash
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "*" Lox.Token.Star
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "!" Lox.Token.Bang
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "=" Lox.Token.Equal
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy ">" Lox.Token.Greater
    , Text.Parsing.Parser.Combinators.try
        $ parseStringNotFollowedBy "<" Lox.Token.Less
    , Lox.Token.Identifier <$> parseLName
    , Lox.Token.String <$> parseStringLiteral
    , Lox.Token.Number <$> parseNumber
    ]
    <* whitespace
  where
  parseLName :: Text.Parsing.Parser.Parser String String
  parseLName =
    map Data.String.CodeUnits.fromCharArray
      $ Data.Array.cons
      <$> identStart
      <*> Data.Array.many identLetter

  identStart :: Text.Parsing.Parser.Parser String Char
  identStart = lower <|> Text.Parsing.Parser.String.char '_'

  identLetter :: Text.Parsing.Parser.Parser String Char
  identLetter = alphaNum <|> Text.Parsing.Parser.String.oneOf [ '_', '\'' ]

  parseString ::
    String ->
    Lox.Token.Token ->
    Text.Parsing.Parser.Parser String Lox.Token.Token
  parseString x token = Text.Parsing.Parser.String.string x $> token

  parseStringNotFollowedBy ::
    String ->
    Lox.Token.Token ->
    Text.Parsing.Parser.Parser String Lox.Token.Token
  parseStringNotFollowedBy x token =
    Text.Parsing.Parser.String.string x
      *> Text.Parsing.Parser.Combinators.notFollowedBy symbolChar
      $> token

  parseStringLiteral :: Text.Parsing.Parser.Parser String String
  parseStringLiteral =
    Text.Parsing.Parser.Combinators.choice
      [ blockString
      , tokenParser.stringLiteral
      ]
    where
    blockString :: Text.Parsing.Parser.Parser String String
    blockString = delimiter *> manyTillString delimiter

    delimiter :: Text.Parsing.Parser.Parser String String
    delimiter =
      Text.Parsing.Parser.Combinators.try
        $ Text.Parsing.Parser.String.string "\"\"\""

  parseNumber :: Text.Parsing.Parser.Parser String (Either Int Number)
  parseNumber =
    Text.Parsing.Parser.Combinators.choice
      [ consumeLeadingZero *> Control.Plus.empty
      , Text.Parsing.Parser.Combinators.choice
          [ Right <$> Text.Parsing.Parser.Combinators.try tokenParser.float
          , Left <$> Text.Parsing.Parser.Combinators.try tokenParser.natural
          ]
          Text.Parsing.Parser.Combinators.<?> "number"
      ]
    where
    -- lookAhead doesn't consume any input if its parser succeeds
    -- if notFollowedBy fails though, the consumed '0' will break the choice chain
    consumeLeadingZero =
      Text.Parsing.Parser.Combinators.lookAhead
        ( Text.Parsing.Parser.String.char '0'
            *> ( Text.Parsing.Parser.Combinators.notFollowedBy Text.Parsing.Parser.Token.digit
                  Text.Parsing.Parser.Combinators.<?> "no leading zero in number literal"
              )
        )

langDef :: Text.Parsing.Parser.Token.GenLanguageDef String Identity
langDef =
  Text.Parsing.Parser.Token.LanguageDef
    { reservedNames: []
    , reservedOpNames: []
    , commentStart: ""
    , commentEnd: ""
    , commentLine: ""
    , nestedComments: true
    , identStart: Text.Parsing.Parser.fail "Identifiers not supported"
    , identLetter: Text.Parsing.Parser.fail "Identifiers not supported"
    , opStart: Text.Parsing.Parser.fail "Operators not supported"
    , opLetter: Text.Parsing.Parser.fail "Operator not supported"
    , caseSensitive: true
    }

tokenParser :: Text.Parsing.Parser.Token.GenTokenParser String Identity
tokenParser = Text.Parsing.Parser.Token.makeTokenParser langDef

alphaNum :: Text.Parsing.Parser.Parser String Char
alphaNum = satisfyCP Data.CodePoint.Unicode.isAlphaNum

manyTillString ::
  forall a.
  Text.Parsing.Parser.Parser String a ->
  Text.Parsing.Parser.Parser String String
manyTillString p =
  Data.String.CodeUnits.fromCharArray <<< Data.List.toUnfoldable
    <$> Text.Parsing.Parser.Combinators.manyTill
        Text.Parsing.Parser.String.anyChar
        p

symbolChar :: Text.Parsing.Parser.Parser String Char
symbolChar = satisfyCP isSymbolChar

isSymbolChar :: Data.String.CodePoints.CodePoint -> Boolean
isSymbolChar c =
  Data.Array.elem c chars
    || (not Data.CodePoint.Unicode.isAscii c && Data.CodePoint.Unicode.isSymbol c)
  where
  chars :: Array Data.String.CodePoints.CodePoint
  chars =
    [ ':'
    , '!'
    , '#'
    , '$'
    , '%'
    , '&'
    , '*'
    , '+'
    , '.'
    , '/'
    , '<'
    , '='
    , '>'
    , '?'
    , '@'
    , '\\'
    , '^'
    , '|'
    , '-'
    , '~'
    ]
      <#> Data.String.CodePoints.codePointFromChar

lower :: Text.Parsing.Parser.Parser String Char
lower = satisfyCP Data.CodePoint.Unicode.isLower

upper :: Text.Parsing.Parser.Parser String Char
upper = satisfyCP Data.CodePoint.Unicode.isUpper

satisfyCP ::
  forall m.
  Monad m =>
  (Data.String.CodePoints.CodePoint -> Boolean) ->
  Text.Parsing.Parser.ParserT String m Char
satisfyCP p =
  Text.Parsing.Parser.String.satisfy
    (p <<< Data.String.CodePoints.codePointFromChar)

whitespace :: Text.Parsing.Parser.Parser String Unit
whitespace = Text.Parsing.Parser.Combinators.skipMany (satisfyCP Data.CodePoint.Unicode.isSpace)
