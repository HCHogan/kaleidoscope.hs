module KaleidoscopeHs.Scanner.Combinator where

import Control.Monad (void)
import Data.Char
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt = L.skipLineComment "//"
  blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquotes :: Parser a -> Parser a
dquotes = between (single '"') (single '"')

squotes :: Parser a -> Parser a
squotes = between (single '\'') (single '\'')

semi :: Parser ()
semi = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

star :: Parser ()
star = void $ symbol "*"

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text]
rws =
  [ "if"
  , "then"
  , "else"
  , "while"
  , "true"
  , "false"
  , "for"
  , "int"
  , "bool"
  , "char"
  , "float"
  , "void"
  , "return"
  , "struct"
  , "NULL"
  , "sizeof"
  ]

strlit :: Parser Text
strlit = do
  content <- dquotes $ takeWhileP Nothing (/= '"')
  pure $ T.pack (read ('"' : cs content ++ "\""))

charlit :: Parser Int
charlit =
  squotes $
    (ord <$> satisfy (`notElem` ['\\', '\'']))
      <|> (single '\\' >> int)

int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
 where
  p = fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
  check x =
    if x `elem` rws
      then fail $ "keyword " <> show x <> " cannot be an identifier"
      else return x
