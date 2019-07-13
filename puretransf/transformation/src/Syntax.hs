module Syntax where

import Control.Monad
import Data.Function
import qualified Data.Map as Map

type Name = String

type Dclrs = [Dclr]

type AllDclrs =[AllDclr]

data AllDclr
  = Dclr Dclr   
  | WithSign TypeSignature [Dclr]
  deriving (Eq,Show)

data Dclr 
  = Assign Name [Apats] Expr 
  deriving (Eq,Show)

data TypeSignature
  = Signature Name Type
  | ContSignature Name [Context]  Type
  deriving (Eq,Show)

data Type 
  = Literal Name
  | TFunc Type Type
  | Container Name [Type]
  | TList Type
  | Void    
  deriving (Eq,Show)

data Context
  = Constraint Name Name
   deriving (Eq,Show) 

data Expr
  = Lam [Apats] Expr
  | App Expr Expr
  | Let [Dclr] Expr --uses Parsing sequences
  | Apat Apats
  | Op Binop Expr Expr
  | Cons Expr [Expr] 
  | List [Expr]
  | Bind Expr Expr 
  | Monadic Expr
  deriving (Eq,Show)

data KMonad 
  = Mname Name 
   
data Apats  
    = Var Name
    | Lit Lit 
    | ListArgs [Apats]
    deriving (Eq,Show,Ord)

type TypedApats =Map.Map Apats Type

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)
