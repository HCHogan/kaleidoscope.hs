{-# LANGUAGE PatternSynonyms #-}

module KaleidoscopeHs.TypeChecker.CoreLanguage where

import Control.Monad.Combinators.Expr
import Data.Functor
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
 - x + y                  -- Integer addiction
 - x - y                  -- Integer subtraction
-}

type Name = String

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

type Parser = Parsec Void String
-- type Parsec e s a = ParsecT e s Identity a
-- where s must be Stream

-- Lexer --

sc :: Parser ()
sc = L.space space1 empty empty

clexeme :: Parser a -> Parser a
clexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

operTable :: [[Operator Parser Expr]]
operTable =
  [
    [ InfixL (clexeme (char '+') $> EBin Add)
    , InfixL (clexeme (char '-') $> EBin Sub)
    ]
  ]

{-
 - The hindley milner type system is defined by a set of inference rules:
 - x : σ ∈ Γ => Γ ⊢ x : σ                              -- Var
 - Γ ⊢ e1 : τ1 → τ2 Γ ⊢ e2 : τ1 =>  Γ ⊢ e1 e2 : τ2     -- App
 -
 -}
