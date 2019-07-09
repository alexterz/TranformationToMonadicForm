module Transformation (
  runTransformation 
) where

import Syntax

import Control.Monad.Except
import Control.Monad.Extra
import Control.Eff
import Control.Monad
import Data.Function
import qualified Data.Map as Map



runTransformation:: [AllDclr]->[AllDclr]
runTransformation [] = []
runTransformation (d:ds) = (transformAllDclr d): (runTransformation ds)

transformAllDclr:: AllDclr->AllDclr
transformAllDclr (Dclr d) = Dclr (transformDclr d)
transformAllDclr (WithSign typesign d) = (WithSign (transformTypeSign typesign) (transformDclr d))

transformTypeSign :: TypeSignature -> TypeSignature
transformTypeSign (Signature name t) = Signature name (transformType t)
transformTypeSign (ContSignature name cont  t) =ContSignature name cont (transformType t)

transformType :: Type -> Type
transformType (Literal name) = Literal name
transformType (TFunc t1 t2) =TFunc (transformType t1)  (Container "Eff" [Literal "r" ,transformType t2])  
transformType (Container name t) = Container name t 
transformType (TList t) = TList t

-------------------------------------------------------------------------------------------------------------
returnExpr:: Expr -> Expr
returnExpr expr = App (Apat (Var "return")) expr

toMonad:: Expr -> Integer -> Expr
toMonad (Apat apats) _ = returnExpr (Apat apats)
toMonad (List exprs) _ = returnExpr (List exprs)
toMonad (Cons expr exprs) _ = returnExpr (Cons expr exprs)
toMonad (Let ds expr) i = transformLet ds expr [] i
toMonad (App e1 e2) _ = App e1 e2 --it's a monad itself
toMonad (Lam apats expr) _ = undefined
toMonad (Op binop e1 e2) _ = Op binop e1 e2


transformDclr:: Dclr -> Dclr
transformDclr (Assign name apats expr) = 
  Assign name apats expr'
    where expr' = (transformExpr expr 0) --(Lam [Var "x"] (App (Apat (Var "return")) (Apat (Var "x"))))


transformExpr:: Expr-> Integer-> Expr
transformExpr (Apat apats) _= returnExpr (Apat apats) -- οκ
transformExpr (List exprs) _= returnExpr (List exprs) -- οκ
transformExpr (Cons expr exprs) i= 
  Bind (toMonad expr i) (Lam [Var ("h"++show i)] 
  (Bind (transformExprs exprs (i+1)) (Lam [Var ("t"++ show i)] 
  (returnExpr (Cons (Apat (Var ("h"++show i))) [Apat (Var ("t"++show i))]))))) --οκ
transformExpr (Let ds expr) i =  transformLet ds expr [] i -- ok
transformExpr (App e1 e2) i = 
  Bind (toMonad e2 i) (Lam [Var ("x"++show i)]
  (Bind (transformExpr e1 (i+1)) (Lam [Var ("g"++ show i)]
  (App (Apat (Var ("g"++ show i))) (Apat (Var ("x"++show i))))))) -- ok  
transformExpr (Lam apats expr) i = undefined
transformExpr (Op binop e1 e2) i = returnExpr (Op binop e1 e2) 

transformExprs:: [Expr] -> Integer -> Expr
transformExprs [Cons expr exprs] i= transformExpr (Cons expr exprs) i
transformExprs [exprs] i = toMonad exprs i 

transformLet::[Dclr] -> Expr -> [Dclr] -> Integer -> Expr
transformLet [] expr dclrs i = Let dclrs (transformExpr expr i)
transformLet ((Assign name [] e):ds) expr dclrs i=
  Bind (toMonad e i) (Lam [Var name] (transformLet ds expr dclrs i))
transformLet (d:ds) expr dclrs i= -- case d is for a function
  transformLet ds expr dclrs' i
    where dclrs' = (transformDclr d):dclrs  
