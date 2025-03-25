{-# LANGUAGE PatternSynonyms #-}

module KaleidoscopeHs.TypeChecker.CoreLanguage where

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
