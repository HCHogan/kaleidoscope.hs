{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module KaleidoscopeHs.TypeChecker.CoreLanguage where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

-- expressions:
{-
 - abc                    -- Variable
 - if a then b else c     -- if expression
 - \x -> y : a -> b       -- Abstraction
 - f x                    -- Application
 - let a = b in c         -- let binding
 - 123456789 : Int        -- Integer
 - True, False : Bool     -- Boolean
 - x + y                  -- Integer addition
 - x - y                  -- Integer subtraction
-}

type Name = Text

data Expr
  = EVar Name
  | EIf Expr Expr Expr
  | EAbs Name Expr
  | EApp Expr Expr
  | ELet Name Expr Expr
  | EInt Integer
  | EBool Bool
  | EBin Oper Expr Expr
  deriving (Show)

data Oper = Add | Sub deriving (Show)

newtype TVar = TV Name deriving (Eq, Show)

data Type
  = TCon Name [Type]
  | TVar TVar
  deriving (Eq, Show)

-- some builtin types
pattern TInt :: Type
pattern TInt = TCon "Int" []

pattern TBool :: Type
pattern TBool = TCon "Bool" []

pattern (:->) :: Type -> Type -> Type
pattern a :-> b = TCon "->" [a, b]

------------
-- Parser --
------------

type Parser = Parsec Void Text

-- type Parsec e s a = ParsecT e s Identity a
-- where s must be Stream

-- Lexer --

reservedWords :: [Text]
reservedWords = ["if", "then", "else", "let", "in", "True", "False"]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

llexeme :: Parser a -> Parser a
llexeme = L.lexeme sc

lsymbol :: Text -> Parser Text
lsymbol = L.symbol sc

linteger :: Parser Expr
linteger = EInt <$> llexeme L.decimal

lidentifier :: Parser Text
lidentifier = llexeme $ try $ do
  first <- letterChar
  rest <- many alphaNumChar
  let name = T.pack (first : rest)
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name

operTable :: [[Operator Parser Expr]]
operTable =
  [
    [ InfixL (llexeme (char '+') $> EBin Add)
    , InfixL (llexeme (char '-') $> EBin Sub)
    ]
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pApp operTable

pFactor :: Parser Expr
pFactor =
  choice
    [ try pAbs
    , pIf
    , pLet
    , EVar <$> pVar
    , pLiteral
    , between (lsymbol "(") (lsymbol ")") pExpr
    ]

pBool :: Parser Expr
pBool = EBool <$> (True <$ lsymbol "True" <|> False <$ lsymbol "False")

pVar :: Parser Text
pVar = llexeme $ do
  first <- letterChar
  rest <- many alphaNumChar
  let name = T.pack (first : rest)
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name

pLiteral :: Parser Expr
pLiteral = linteger <|> pBool

pIf :: Parser Expr
pIf = do
  void $ lsymbol "if"
  condExpr <- pExpr
  void $ lsymbol "then"
  thenExpr <- pExpr
  void $ lsymbol "else"
  EIf condExpr thenExpr <$> pExpr

pAbs :: Parser Expr
pAbs = do
  void $ lsymbol "\\"
  varName <- pVar
  void $ lsymbol "->"
  EAbs varName <$> pExpr

pApp :: Parser Expr
pApp = do
  f <- pFactor
  args <- many pFactor
  return $ foldl EApp f args

pLet :: Parser Expr
pLet = do
  void $ lsymbol "let"
  var <- pVar
  void $ lsymbol "="
  expr <- pExpr
  void $ lsymbol "in"
  ELet var expr <$> pExpr

{-
 - The hindley milner type system is defined by a set of inference rules:
 - x : σ ∈ Γ => Γ ⊢ x : σ                                    -- Var
 - Γ ⊢ e1 : τ1 → τ2 Γ ⊢ e2 : τ1 =>  Γ ⊢ e1 e2 : τ2           -- App
 - Γ , x : τ1 ⊢ e : τ2 => Γ ⊢ λx.e : τ1 → τ2                 -- Abs
 - Γ ⊢ e1 : σ Γ , x : σ ⊢ e2 : τ => Γ ⊢ let x = e1 in e2 : τ -- Let
 - Γ ⊢ e : σ
 -}
