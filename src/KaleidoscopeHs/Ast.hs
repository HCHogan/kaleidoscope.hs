module KaleidoscopeHs.Ast () where

import Data.Char (chr)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

-- Binary operator
data Bop
  = Add
  | Mult
  | Div
  | -- | Power ** for fun
    Equal
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
  | Filteral Double -- 0.1 | 4.2
  | BoolLit Bool -- true | false
  | Null -- null
  | Id Text -- x
  | Binop Bop Expr Expr -- 1 + 2
  | Unop Uop Expr -- !true
  | Call Text
