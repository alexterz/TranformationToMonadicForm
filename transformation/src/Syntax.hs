module Syntax where

type Name = String


data Expr
  = Lam [Name] Expr
--  | Dclr Dclr
  | App Expr Expr
  | Let [Dclr] Expr --uses Parsing sequences
  | Var Name
  | Lit Lit
  | Op Binop Expr Expr
  deriving (Eq,Show)
   

data Dclr 
  = Assign Name Expr
--  | FuncDclr Name [Name] Expr--Rhs
  deriving (Eq,Show)

type Dclrs = [Dclr]


data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)
