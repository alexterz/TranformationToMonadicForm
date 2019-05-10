module Syntax where

type Name = String

data Expr
  = Lam Name Expr
  | App Expr Expr
  | Let Dclr Expr
  | Var Name
  | Lit Lit
  | Op Binop Expr Expr
  deriving (Eq,Show)
 

-- data Dclrs 

data Dclr
  = Assign String Expr
  deriving (Eq,Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)
