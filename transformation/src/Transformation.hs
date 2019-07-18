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
import Data.List




runTransformation :: [AllDclr]->TypedApats->[AllDclr]
runTransformation [] _= []
runTransformation (d:ds) tApats= (transformAllDclr d tApats): (runTransformation ds tApats)


transformAllDclr :: AllDclr-> TypedApats -> AllDclr
transformAllDclr (WithSign typesign@(ContSignature name cont t) d) tApats=
  (WithSign signToMonad ((transformDclrs d t' tApats)))
    where 
      signToMonad = (transformTypeSign typesign)
      t' = case t of
            (ForAll names t1)-> t1
            (Type t1) -> t1  

------------------------------------------------------------------------------------------------------------------------------------------------------
transformTypeSign :: TypeSignature -> TypeSignature
transformTypeSign (ContSignature name cont t) =
  (ContSignature name cont' t'')
    where (t',cont') =transformTypeScope t cont
          t'' = case t' of
             (ForAll names t1)-> (ForAll names (effType t1))
             (Type t1) -> (Type (effType t1))
          effType t = case t of
                    Container _ _ -> t
                    otherwise -> (Container "Eff" [Literal "r", t])


transformTypeScope :: TypeScope -> [Context]-> (TypeScope,[Context])
transformTypeScope (ForAll names t) cont= 
  ((ForAll (("r"):names) t'),uniq cont'')
  where (t',cont',delcont) =transformType t cont []
        cont'' = filter (\x -> x `notElem` delcont) cont'
transformTypeScope (Type t) cont = 
  ((Type t'),cont'')
  where (t',cont',delcont) =transformType t cont []
        cont'' = filter (\x -> x `notElem` delcont) cont' 

transformType :: Type -> [Context] -> [Context]-> (Type, [Context], [Context])
transformType (Literal name) cont delcont = ((Literal name),cont, delcont)
transformType (TFunc t1 t2) cont delcont =
  ((TFunc t1'  (effType t2')), cont, c1++c2)
    where (t1',j,c1) = transformType t1 cont  []
          (t2',k,c2) = transformType t2 cont delcont
          effType t = case t of
                    Container _ _ -> t
                    otherwise -> (Container "Eff" [Literal "r", t])
{--transformType (Container "IO" [t]) cont delcont=
    ((Container "Eff" ((Literal "r"):[t'])), cont', delcont)  --for a specific monad like State s 
       where cont'= (Constraint (SetMember Lift (Lift IO) t)):cont  
             (t',c,c1) = transformType t cont' delcont --}
transformType (Container name types) cont delcont= 
  case types of 
    [t] -> ((Container "Eff" (Literal "r" :[t'])), c, c1) --for an arbitrary monad m type   
           where
              (t',c,c1) = transformType t cont delcont'
              delcont' =  if (elem (Constraint  (Class "Monad") name) delcont) then delcont else ((Constraint  (Class "Monad") name):delcont)
    ((Literal s):[ts]) -> 
       ((Container "Eff" ((Literal "r"):[ts'])), cont', delcont)  --for a specific monad like State s 
       where cont'= (Constraint (Member name s) "r"):cont  
             (ts',c,c1) = transformType ts cont' delcont         
transformType (TList t) cont delcont= ((TList t) , cont , delcont)


-------------------------------------------------------------------------------------------------------------


transformDclrs:: Dclrs ->Type ->TypedApats -> Dclrs
transformDclrs ((Assign name apats expr):ds) t tApats= -- to times den xreiazetai pia, vriskw periptwsi mConvert apo length apats
  [(Assign name [] expr' )]
  where
    lenApats = toInteger(length apats)
    expr' = 
        case lenApats of
          0 -> Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats)] (Apat(Var (name++"'")))
          1 -> returnExpr (Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats)] (Apat(Var (name++"'")))) 
          otherwise -> returnExpr (Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats)] (App (Apat (Var ("mConvert" ++ (show (lenApats-1))))) (Apat(Var (name++"'")))))
      
transformlocalDclrs:: Dclrs -> Type-> TypedApats-> Dclrs
transformlocalDclrs [] _ tApats= []
transformlocalDclrs (d:ds) t tApats= (transformDclr d t tApats):(transformlocalDclrs ds t tApats)

transformDclr:: Dclr -> Type -> TypedApats -> Dclr
transformDclr (Assign name apats expr) t tApats= 
  Assign (name++"'") apats expr'
    where expr' = (transformExprMonad expr typedApats 0) --(Lam [Var "x"] (App (Apat (Var "return")) (Apat (Var "x"))))
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
------------------------------------------------------------------------------------------------------------------
transformExprMonad::  Expr-> TypedApats -> Integer-> Expr
transformExprMonad e@(Apat (Var name)) tApats  i= --  οκ 
  case (Map.lookup (Var name) tApats) of
    Just (Container _ _ ) -> e
    otherwise -> toMonad e tApats i
transformExprMonad e tApats i = 
  transformExpr e tApats i    


transformExpr:: Expr-> TypedApats -> Integer-> Expr
transformExpr e@(Apat apats) tApats  i= --  οκ 
  toMonad e tApats i 
transformExpr (List es) tApats i= 
  App (Apat(Var "sequence")) (List (transformExprs es tApats i))---ok
transformExpr (Cons e1 e2) tApats  i= --οκ
  transformExpr (App (App (Apat(Var "cons")) e1) e2) tApats (i+1)
transformExpr (Let ds expr) tApats i =  
  Let (runTransformation ds tApats) (transformExpr expr tApats (i+1)) 
transformExpr (App e1 e2) tApats i = --ok 
  Bind (transformExpr e2 tApats (i+1)) (Lam [Var ("x"++show i)]                                    -- Na dw ti ginetai an to e2 otan einai monad kai oxi func
  (Bind (transformExpr e1 tApats (i+1)) (Lam [Var ("g"++ show i)]
  (App (Apat (Var ("g"++ show i))) (Apat(Var ("x"++show i)))))))  
transformExpr (Op binop e1 e2) tApats i = --ok
  transformExpr (App (App (Apat(Var x)) e1) e2) tApats (i+1)
  where
    x= case binop of
         Add -> "plus"
         Sub -> "sub"
         Mul -> "multiple"
transformExpr (Lam apats expr) tApats i = undefined 
transformExpr (Bind e1 e2) tApats i = 
  Bind (transformExpr e2 tApats (i+1)) (Lam [Var ("f"++show i)]                                    -- Na dw ti ginetai an to e2 otan einai monad kai oxi func
  (Bind (transformExpr e1 tApats (i+1)) (Lam [Var ("y"++ show i)]
  (Bind (Apat (Var ("y"++ show i))) (Apat(Var ("f"++show i))))))) 
transformExpr (Monadic e) _ _= Monadic e      


transformExprs:: [Expr] -> TypedApats -> Integer -> [Expr]
transformExprs [] tApats i = []
transformExprs (e:es) tApats i = (transformExprMonad e tApats i):(transformExprs es tApats i)

toMonad:: Expr -> TypedApats ->Integer -> Expr
toMonad e@(Apat (Lit _)) tApats _ = 
  returnExpr e
toMonad e@(Apat (Var "return")) tApats _ = returnExpr e
toMonad e@(Apat (Var name)) tApats _ = 
  case (Map.lookup (Var name) tApats) of -- for literals οκ
    Just (Literal n) -> returnExpr e
    Just (TList t) -> returnExpr e
    Just (TFunc t1 t2)-> returnExpr e --this case is for arguments that are functions (a->Eff r (...))
    Just (Container _ _ ) -> returnExpr e
    otherwise -> e
toMonad e@(Apat (ListArgs apats)) tApats _ = 
  undefined

returnExpr:: Expr -> Expr
returnExpr expr = App (Apat (Var "return")) expr


uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)
