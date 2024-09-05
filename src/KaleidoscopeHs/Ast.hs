module KaleidoscopeHs.Ast where

import Data.Char (chr)
import Data.Text (Text)

-- Binary operator
data Bop
  = Add
  | Mult
  | Div
  | Power 
  | Equal
  | Sub
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  deriving (Show, Eq)

-- Unary operator
data Uop
  = Neg
  | Not
  deriving (Show, Eq)

data Expr
  = Literal Int -- 0 | 42
  | StrLit Text -- "hello
  | CharLit Int -- 'a' as int
  | Fliteral Double -- 0.1 | 4.2
  | BoolLit Bool -- true | false
  | Null -- null
  | Id Text -- x
  | Binop Bop Expr Expr -- 1 + 2
  | Unop Uop Expr -- !true
  | Call Text [Expr]
  | Cast Type Expr -- (int *)malloc(100)
  | Access Expr Expr -- struct.field
  | Deref Expr
  | -- \*p
    Addr Expr -- &x
  | Assign Expr Expr -- x = y
  | Sizeof Type -- sizeof(int)
  | Noexpr -- used for dangling if
  deriving (Show, Eq)

data Statement
  = Expr Expr
  | Block [Statement]
  | Return Expr
  | If Expr Statement Statement
  | For Expr Expr Expr Statement
  | While Expr Statement
  deriving (Show, Eq)

data Type
  = Pointer Type
  | TyInt -- i32
  | TyBool -- true | false
  | TyChar -- u8
  | TyFloat
  | TyVoid -- TODO: add tuple and remove this in future
  | TyStruct Text -- struct name
  deriving (Show, Eq)

data Bind = Bind {bindType :: Type, bindName :: Text}
  deriving (Show, Eq)

data Struct = Struct {structname :: Text, structField :: [Bind]}
  deriving (Show, Eq)

data Function = Function {typ :: Type, name :: Text, formals :: [Bind], locals :: [Bind], body :: [Statement]}
  deriving (Show, Eq)

data Program = Program [Struct] [Bind] [Function] deriving (Eq, Show)


