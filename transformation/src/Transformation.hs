module Transformation (
  runTransformation,
) where

import Syntax

import Control.Monad.Except
import Control.Monad.Extra
import Control.Eff
import Control.Monad
import Data.Function
import qualified Data.Map as Map



runTransformation :: [AllDclr]->[AllDclr]
runTransformation [] = []
runTransformation (d:ds) = (transformAllDclr d): (runTransformation ds)


transformAllDclr :: AllDclr->AllDclr
transformAllDclr (WithSign typesign d) = (WithSign signToMonad (transformDclrs d times))
  where (signToMonad,times) = (transformTypeSign typesign)

transformTypeSign :: TypeSignature -> (TypeSignature,Integer)
transformTypeSign (Signature name t) = 
  (Signature name (fst $transformType t 0),times )
    where times =
            case (snd $ transformType t 0) of
              0-> 0
              x-> x-1
transformTypeSign (ContSignature name cont  t) =(ContSignature name cont (fst $ transformType t 0), ((snd $ transformType t 0)-1))

transformType :: Type -> Integer ->(Type,Integer)
transformType (Literal name) i= ((Literal name),i)
transformType (TFunc t1 t2) i=
  ((TFunc t1'  (Container "Eff" [Literal "r" ,t2'])), k)
    where (t1',j) = transformType t1 i
          (t2',k) = transformType t2 (i+1)    
transformType (Container name t) i= ((Container name t),i) 
transformType (TList t) i= ((TList t) ,i)

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
toMonad (Op binop e1 e2) _ = Op binop e1 e2 --App (App (Apat (Var ("("++show binop++")"))) e1) e2--

transformDclrs:: Dclrs-> Integer -> Dclrs
transformDclrs ((Assign name apats expr):ds) times = 
  [(Assign name [] expr' )]
  where
    expr' = Let (transformlocalDclrs ((Assign name apats expr):ds)) (App (Apat (Var ("mConvert" ++ (show times)))) (Apat(Var name)))
 

transformlocalDclrs [] = []
transformlocalDclrs (d:ds) = (transformDclr d):(transformlocalDclrs ds)

transformDclr:: Dclr -> Dclr
transformDclr (Assign name apats expr) = 
  Assign (name) apats expr'
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
transformExpr (Op binop e1 e2) i = returnExpr (Op binop e1 e2) --transformExpr (App (App (Apat (Var ("("++show binop++")"))) e1) e2) i-- 

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
