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



runTransformation :: [AllDclr]->TypedApats->[AllDclr]
runTransformation [] _= []
runTransformation (d:ds) tApats= (transformAllDclr d tApats): (runTransformation ds tApats)


transformAllDclr :: AllDclr-> TypedApats -> AllDclr
transformAllDclr (WithSign typesign@(Signature name t) d) tApats= 
  (WithSign signToMonad ((transformDclrs d times t' tApats)))
    where 
      (signToMonad,times) = (transformTypeSign typesign)
      t' = case t of
            (ForAll names t1)-> t1
            (Type t1) -> t1
transformAllDclr (WithSign typesign@(ContSignature name cont t) d) tApats=
  (WithSign signToMonad ((transformDclrs d times t' tApats)))
    where 
      (signToMonad,times) = (transformTypeSign typesign)
      t' = case t of
            (ForAll names t1)-> t1
            (Type t1) -> t1  


transformTypeSign :: TypeSignature -> (TypeSignature,Integer)
transformTypeSign (Signature name t) = 
  (Signature name t'',times)
    where (t',times) =transformTypeScope t
          t'' = case t' of
             (ForAll names t1)-> (ForAll names (effType t1))
             (Type t1) -> (Type (effType t1))
          effType t = (Container "Eff" [Literal "r", t])
transformTypeSign (ContSignature name cont t) =
  (ContSignature name cont t'', times)
    where (t',times) =transformTypeScope t 
          t'' = case t' of
             (ForAll names t1)-> (ForAll names (effType t1))
             (Type t1) -> (Type (effType t1))
          effType t = (Container "Eff" [Literal "r", t])


transformTypeScope :: TypeScope -> (TypeScope,Integer)
transformTypeScope (ForAll names t) = ((ForAll (("r"):names) t'),times)
  where (t',times) =transformType t 0
transformTypeScope (Type t) = ((Type t'),times)
 where (t',times) =transformType t 0  

transformType :: Type -> Integer -> (Type,Integer)
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


toMonad:: Expr -> TypedApats ->Integer -> Expr
toMonad e@(Apat (Lit _)) tApats _= 
  returnExpr e
toMonad e@(Apat (Var name)) tApats _= 
  case (Map.lookup (Var name) tApats) of -- for literals οκ
    Just (Literal n) -> returnExpr e
    Just (TList t) -> returnExpr e
    Just (TFunc t1 t2)-> returnExpr e --this case is for arguments that are functions (a->Eff r (...))
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

transformDclrs:: Dclrs-> Integer ->Type ->TypedApats -> Dclrs
transformDclrs ((Assign name apats expr):ds) times t tApats= 
  [(Assign name [] expr' )]
  where
    expr' = case times of 
        0 -> Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats)] (Apat(Var (name++"'")))
        1 -> returnExpr (Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats)] (Apat(Var (name++"'")))) 
        x-> returnExpr (Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats)] (App (Apat (Var ("mConvert" ++ (show (x-1))))) (Apat(Var (name++"'")))))
 

transformlocalDclrs:: Dclrs -> Type-> TypedApats-> Dclrs
transformlocalDclrs [] _ tApats= []
transformlocalDclrs (d:ds) t tApats= (transformDclr d t tApats):(transformlocalDclrs ds t tApats)

transformDclr:: Dclr -> Type -> TypedApats -> Dclr
transformDclr (Assign name apats expr) t tApats= 
  Assign (name++"'") apats expr'
    where expr' = (transformExpr expr typedApats 0) --(Lam [Var "x"] (App (Apat (Var "return")) (Apat (Var "x"))))
          typedApats = transformApats apats t tApats

transformApats:: [Apats]-> Type-> TypedApats -> TypedApats
transformApats [] typesign tApats = tApats 
transformApats (l:ls) (TFunc t1 t2) tApats= 
  transformApats ls t2 tApats'
  where
     tApats' = insertApat l t1 tApats

insertApat:: Apats -> Type -> TypedApats ->TypedApats
insertApat a@(ListArgs []) t tApats = tApats
insertApat (ListArgs [Var xs]) (TList t) tApats = 
  Map.insert (Var xs) (TList t) tApats 
insertApat (ListArgs (x:xs)) (TList t) tApats = 
  insertApats xs (TList t) tApats'
  where 
    tApats' = Map.insert x t tApats 
insertApat a t tApats =  
  Map.insert a t tApats             
 

insertApats:: [Apats] -> Type -> TypedApats ->TypedApats
insertApats [] t tApats = tApats
insertApats (l:ls) t tApats =
  insertApat l t tApats'
  where 
    tApats' = insertApats ls t tApats



transformExpr:: Expr-> TypedApats -> Integer-> Expr
transformExpr e@(Apat apats) tApats  i= --  οκ 
  toMonad e tApats i 
transformExpr (List es) tApats i=  App (Apat(Var "sequence")) (List (transformExprs es tApats i))---ok
transformExpr (App e1 e2) tApats i = --ok 
  Bind e2' (Lam [Var ("x"++show i)]                                    -- Na dw ti ginetai an to e2 otan einai monad kai oxi func
  (Bind e1' (Lam [Var ("g"++ show i)]
  (App (Apat (Var ("g"++ show i))) (Apat(Var ("x"++show i))))))) 
  where
    e1' = transformExpr e1 tApats (i+1)  
    e2' = transformExpr e2 tApats (i+1) 
transformExpr (Cons e1 e2) tApats  i= --οκ
  transformExpr (App (App (Apat(Var "cons")) e1) e2) tApats (i+1)
transformExpr (Op binop e1 e2) tApats i = --ok
  transformExpr (App (App (Apat(Var x)) e1) e2) tApats (i+1)
  where
    x= case binop of
         Add -> "plus"
         Sub -> "sub"
         Mul -> "multiple"
transformExpr (Let ds expr) tApats i =  
  Let (runTransformation ds tApats) (transformExpr expr tApats (i+1))        


transformExprs:: [Expr] -> TypedApats -> Integer -> [Expr]
transformExprs [] tApats i = []
transformExprs (e:es) tApats i = (transformExpr e tApats i):(transformExprs es tApats i)


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