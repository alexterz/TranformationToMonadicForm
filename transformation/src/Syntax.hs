module Syntax where

type Name = String

type Dclrs = [Dclr]

data Expr
  = Lam [Apats] Expr-- Apat
  | App Expr Expr
  | Let [Dclr] Expr --uses Parsing sequences
  | Apat Apats
  | Op Binop Expr Expr
  deriving (Eq,Show)
   

data Dclr 
  = Assign Name [Apats] Expr
--  | FuncDclr Name [Name] Expr--Rhs
  deriving (Eq,Show)


data Apats  
    = Var Name
    | Lit Lit 
    deriving (Eq,Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)
