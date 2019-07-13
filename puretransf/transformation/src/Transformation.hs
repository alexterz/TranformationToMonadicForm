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
transformAllDclr (WithSign typesign@(Signature name t) d) = 
  (WithSign signToMonad ((transformDclrs d times t)))
    where 
      (signToMonad,times) = (transformTypeSign typesign)
transformAllDclr (WithSign typesign@(ContSignature name cont t) d) =
  (WithSign signToMonad ((transformDclrs d times t)))
    where 
      (signToMonad,times) = (transformTypeSign typesign)  


transformTypeSign :: TypeSignature -> (TypeSignature,Integer)
transformTypeSign (Signature name t) = 
  (Signature name t',times)
    where (t',times) =transformType t 0
transformTypeSign (ContSignature name cont t) =
  (ContSignature name cont t', times)
    where (t',times) =transformType t 0



transformType :: Type -> Integer ->(Type,Integer)     -------------- diorthsiiiiiiiiiiiiiiiiiiii
transformType (Literal name) 0= ((Container "Eff" [Literal "r", (Literal name)]), 0)
transformType (TList t) 0= ((Container "Eff" [Literal "r", (TList t)]),0)
transformType (Literal name) i= (Literal name, i)
transformType (TFunc t1 t2@(TFunc _ _)) i=
  ((Container "Eff" [Literal "r" ,(TFunc t1'  t2')]), k)
    where (t1',j) = transformType t1 (i+1)
          (t2',k) = transformType t2 (i+1)  
transformType (TFunc t1 t2) i=
  ((Container "Eff" [Literal "r" ,(TFunc t1'  t2'')]), k)
    where (t1',j) = transformType t1 (i+1)
          (t2',k) = transformType t2 (i+1) 
          t2'' = Container "Eff" [Literal "r", t2']   
transformType (Container name t) i= ((Container name t),i)      -------------- diorthsiiiiiiiiiiiiiiiiiiii
transformType (TList t) i= ((TList t) ,i)

-------------------------------------------------------------------------------------------------------------
returnExpr:: Expr -> Expr
returnExpr expr = App (Apat (Var "return")) expr


toMonad:: Expr -> TypedApats ->Integer -> Expr
toMonad e@(Apat (Lit _)) tApats _= 
  returnExpr e
toMonad e@(Apat (Var name)) tApats _= 
  case (Map.lookup (Var name) tApats) of -- for literals οκ
    Just (Literal n) -> returnExpr e
    Just (TList t) -> returnExpr e
    otherwise -> e {--      Just (TFunc t1 t2)-> e Just (Container n t) -> e--}
toMonad e@(Apat (ListArgs apats)) tApats _ = 
  undefined
{--   
toMonad (List exprs) _ = Monadic (returnExpr (List exprs))
toMonad (Cons expr exprs) _ = Monadic (returnExpr (Cons expr exprs))
toMonad (Let ds expr) i = Monadic (transformLet ds expr [] i)
toMonad (App e1 e2) _ = Monadic (App e1 e2) --it's a monad itself
toMonad (Lam apats expr) _ = undefined
toMonad (Op binop e1 e2) _ = Monadic (Op binop e1 e2) --App (App (Apat (Var ("("++show binop++")"))) e1) e2--
toMonad (Monadic e) _= Monadic e
--}

transformDclrs:: Dclrs-> Integer ->Type -> Dclrs
transformDclrs ((Assign name apats expr):ds) times t = 
  [(Assign name [] expr' )]
  where
    expr' = case times of 
        0 -> Let (transformlocalDclrs ((Assign name apats expr):ds) t) (Apat(Var name))
        1 -> App (Apat (Var "return")) (Let (transformlocalDclrs ((Assign name apats expr):ds) t) (Apat(Var name))) 
        x-> App (Apat (Var "return")) (Let (transformlocalDclrs ((Assign name apats expr):ds) t) (App (Apat (Var ("mConvert" ++ (show (x-1))))) (Apat(Var name))))
 

transformlocalDclrs:: Dclrs -> Type-> Dclrs
transformlocalDclrs [] _= []
transformlocalDclrs (d:ds) t = (transformDclr d t):(transformlocalDclrs ds t)

transformDclr:: Dclr -> Type -> Dclr
transformDclr (Assign name apats expr) t = 
  Assign (name) apats expr'
    where expr' = (transformExpr expr typedApats name 0) --(Lam [Var "x"] (App (Apat (Var "return")) (Apat (Var "x"))))
          typedApats = transformApats apats t Map.empty

transformApats:: [Apats]-> Type-> TypedApats -> TypedApats
transformApats [] typesign tApats= tApats
transformApats (l:ls) (TFunc t1 t2) tApats= transformApats ls t2 (Map.insert l t2 tApats)

transformExpr:: Expr-> TypedApats -> Name -> Integer-> Expr
transformExpr e@(Apat apats) tApats _ i= 
  toMonad e tApats i -- for literals οκ 
transformExpr (App e1 e2) tApats name i = 
  case e1 of 
    (Apat(Var x)) -> 
      if x == name then (returnExpr (App e1 e2)) else apply 
    otherwise ->
      apply
  where
    apply =                                     -- Na dw ti ginetai an to e2 otan einai monad kai oxi func
      Bind (transformExpr e1 tApats name (i+1)) (Lam [Var ("g"++ show i)]
      (App (Apat (Var ("g"++ show i))) e2)) -- ok
transformExpr (Op binop e1 e2) tApats _ i = returnExpr (Op binop e1 e2)      

{--
transformApp:: Expr -> Expr -> TypeApats -> Integer
transformApp (e1 e2) = 
  case e2 of 
    (Apat (Var name)) ->
        case (Map.lookup (Var name) tApats) of 
          Just (Literal n) -> applyVar
          Just (TList t) -> applyVar
          --Just (Container)
          otherwise -> applyFunc
    otherwise -> applyVar

  where
    applyVar = 
      Bind (toMonad e2 tApats i) (Lam [Var ("x"++show i)]
      (Bind (transformExpr e1 tApats (i+1)) (Lam [Var ("g"++ show i)] 
      (App (Apat (Var ("g"++ show i))) (Apat (Var ("x"++show i))))))) -- ok  
    applyFunc =
      Bind (transformExpr e1 tApats (i+1)) (Lam [Var ("g"++ show i)]
      (App (Apat (Var ("g"++ show i))) e2)) 
--}


{--
transformExpr:: Expr-> Integer-> Expr
transformExpr (Apat apats) _= returnExpr (Apat apats) -- for literals οκ
transformExpr (List exprs) _= returnExpr (List exprs) -- nooooooo
transformExpr (Cons expr exprs) i= 
  Bind (toMonad expr i) (Lam [Var ("h"++show i)] 
  (Bind (transformExprs exprs (i+1)) (Lam [Var ("t"++ show i)] 
  (returnExpr (Cons (Apat (Var ("h"++show i))) [Apat (Var ("t"++show i))]))))) --οκ
transformExpr (Let ds expr) i =  transformLet ds expr [] i -- ok
transformExpr (App e1 e2) i = 
  Bind (toMonad e2 i) (Lam [Var ("x"++show i)]
  (Bind (transformExpr e1 (i+1)) (Lam [Var ("g"++ show i)]
  (Bind (Apat(Var ("g"++ show i))) (Lam [Var ("g'"++ show i)]  
  (App (Apat (Var ("g'"++ show i))) (Apat (Var ("x"++show i))))))))) -- ok  
transformExpr (Lam apats expr) i = undefined
transformExpr (Op binop e1 e2) i = returnExpr (Op binop e1 e2) --transformExpr (App (App (Apat (Var ("("++show binop++")"))) e1) e2) i-- 
transformExpr (Monadic e) _= Monadic e


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


--}