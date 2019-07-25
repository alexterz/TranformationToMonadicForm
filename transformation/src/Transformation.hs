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




runTransformation :: [AllDclr]->TypedFuncs->TypedApats->([AllDclr],TypedFuncs)
runTransformation [] tFuncs _= ([],tFuncs)
runTransformation ((d@(WithSign (ContSignature name _ t) _)):ds) tFuncs tApats=
  case t of
    ForAll _ t' -> 
      (((transformAllDclr d alltFuncs tApats ): list), alltFuncs)
      where typedFuncs =  Map.insert name t' tFuncs --check if it works-- tFuncs --it works but it's not lazy! fix it!
            (list, alltFuncs) = (runTransformation ds typedFuncs tApats)
    Type t' ->   
      (((transformAllDclr d alltFuncs tApats ): list), alltFuncs)
      where typedFuncs =  Map.insert name t' tFuncs--alltFuncs --check if it works--  --it works but it's not lazy! fix it!
            (list, alltFuncs) = (runTransformation ds typedFuncs tApats)    

transformAllDclr :: AllDclr-> TypedFuncs-> TypedApats-> AllDclr
transformAllDclr (WithSign typesign@(ContSignature name cont t) d) tFuncs tApats=
  (WithSign signToMonad ((transformDclrs d t' tApats tFuncs )))
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
  ((TFunc t1'  (effType t2')), uniq (j++k), c1++c2)
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
transformType (TTuple ts) cont delcont = ((TTuple ts) , cont , delcont)


-------------------------------------------------------------------------------------------------------------


transformDclrs:: Dclrs ->Type ->TypedApats ->TypedFuncs -> Dclrs
transformDclrs ((Assign name apats expr):ds) t tApats tFuncs= -- to times den xreiazetai pia, vriskw periptwsi mConvert apo length apats
  [(Assign name [] expr' )]
  where
    lenApats = toInteger(length apats)
    expr' = 
        case lenApats of
          0 -> Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats tFuncs)] (Apat(Var (name++"'")))
          1 -> returnExpr (Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats tFuncs)] (Apat(Var (name++"'")))) 
          otherwise -> returnExpr (Let [Dclrs (transformlocalDclrs ((Assign name apats expr):ds) t tApats tFuncs)] (App (Apat (Var ("mConvert" ++ (show (lenApats-1))))) (Apat(Var (name++"'")))))
      
transformlocalDclrs:: Dclrs -> Type-> TypedApats-> TypedFuncs ->Dclrs
transformlocalDclrs [] _ _ _= []
transformlocalDclrs (d:ds) t tApats tFuncs= (transformDclr d t tApats tFuncs):(transformlocalDclrs ds t tApats tFuncs)

transformDclr:: Dclr -> Type -> TypedApats ->TypedFuncs -> Dclr
transformDclr (Assign name apats expr) t tApats tFuncs= 
  Assign (name++"'") apats expr'
    where expr' = fst (transformExprMonad (expr,Void) typedApats tFuncs 0) --(Lam [Var "x"] (App (Apat (Var "return")) (Apat (Var "x"))))
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
transformExprMonad::  (Expr,Type)-> TypedApats ->TypedFuncs -> Integer-> (Expr,Type)--(Expr,Type)
transformExprMonad (e@(Apat (Var name)),t) tApats tFuncs i= --  οκ 
  case (Map.lookup (Var name) tApats) of
    Just (Container _ _ ) -> (e,Void)
    otherwise -> toMonad (e,t) tApats tFuncs i
transformExprMonad (e,t) tApats tFuncs i = 
  transformExpr (e,t) tApats tFuncs i   


transformExpr:: (Expr,Type)-> TypedApats ->TypedFuncs -> Integer-> (Expr,Type) --Expr --
transformExpr (e@(Apat apats),t) tApats tFuncs i= --  οκ 
  toMonad (e,t) tApats tFuncs i 
transformExpr ((List es),t) tApats tFuncs i= 
  (App (Apat(Var "sequence")) (List (transformExprs es t tApats tFuncs i)),t) ---ok
transformExpr ((Tuple es),t) tApats tFuncs i= 
  ((returnExpr (Tuple es)),t) -- (App (Apat(Var "sequenceT")) (Tuple (transformExprs es t tApats tFuncs i)),t) ---ok  
transformExpr ((Cons e1 e2),t) tApats tFuncs i= --οκ
  transformExpr ((App (App (Apat(Var "cons")) e1) e2),t) tApats tFuncs (i+1)
transformExpr ((Let ds expr),t) tApats tFuncs i =  
  (Let (fst (runTransformation ds tFuncs tApats)) (fst (transformExpr (expr,t) tApats tFuncs (i+1))),t) 
transformExpr ((App (App (Apat(Var "runState")) e2) e3),t) tApats tFuncs i = --ok
  ((Bind e3' (Lam [Var ("s"++show i)] (App (App (Apat(Var "runState")) (Apat(Var ("s"++show i)))) e2'))),t) 
  where
    (e2',_) = transformExpr (e2,t) tApats tFuncs (i+1)
    (e3',_) = transformExpr (e3,t) tApats tFuncs (i+1)
transformExpr ((App e1 e2),t) tApats tFuncs i = --ok
  case t1 of
    (Container _ _) -> 
      (Bind e1' (Lam [Var ("g"++ show i)]
      (App (Apat (Var ("g"++ show i))) e2')), t2)
    otherwise ->
      (Bind e2' (Lam [Var ("x"++show i)]                                    -- Na dw ti ginetai an to e2 otan einai monad kai oxi func
      (Bind e1' (Lam [Var ("g"++ show i)]
      (App (Apat (Var ("g"++ show i))) (Apat(Var ("x"++show i))))))),t2)
  where
    (e2',_) = transformExpr (e2,t) tApats tFuncs (i+1)
    (e1',TFunc t1 t2) = transformExpr (e1,t) tApats tFuncs (i+1)      
transformExpr ((Op binop e1 e2),t) tApats tFuncs i = --ok
  transformExpr ((App (App (Apat(Var x)) e1) e2),t) tApats tFuncs (i+1)
  where
    x= case binop of
         Add -> "plus"
         Sub -> "sub"
         Mul -> "multiple"
transformExpr ((Lam apats expr),t) tApats tFuncs i = undefined 
transformExpr ((Bind e1 e2),t) tApats tFuncs i = 
  (Bind e2' (Lam [Var ("f"++show i)]                                    -- Na dw ti ginetai an to e2 otan einai monad kai oxi func
  (Bind e1' (Lam [Var ("y"++ show i)]
  (Bind (Apat (Var ("y"++ show i))) (Apat(Var ("f"++show i))))))),t2) 
  where
    (e2',TFunc t1 t2) = transformExpr (e2,t) tApats tFuncs (i+1)
    (e1',_) = transformExpr (e1,t) tApats tFuncs (i+1)
transformExpr ((Monadic e),t) _ _ _= ((Monadic e),t)      


transformExprs:: [Expr] ->Type -> TypedApats ->TypedFuncs -> Integer -> [Expr]
transformExprs [] t tApats tFuncs i = []
transformExprs (e:es) t tApats tFuncs i = (fst (transformExprMonad (e,t) tApats tFuncs i)):(transformExprs es t tApats tFuncs i)

toMonad:: (Expr,Type) -> TypedApats -> TypedFuncs -> Integer -> (Expr,Type) --Expr
toMonad (e@(Apat (Lit _)),t) tApats tFuncs _ = 
  (returnExpr e,Void)
toMonad (e@(Apat (Var "get")),t) tApats tFuncs _ = 
  (returnExpr e, Void)  
toMonad (e@(Apat (Var "return")),t) tApats tFuncs _ = 
  (returnExpr e, TFunc (Literal "a") (Container "m" [Literal "a"]))
--toMonad (e@(Apat (Var "runState")),t) tApats tFuncs _ = 
--  (returnExpr e, TFunc (Container "State" [Literal "s", Literal "a"]) (TFunc (Literal "s") (TTuple [Literal "a",Literal "s"])))
toMonad (e@(Apat (Var name)),t) tApats tFuncs _ = 
  case (Map.lookup (Var name) tApats) of -- for literals οκ
    Just (Literal n) -> (returnExpr e,Void)
    Just (TList t) -> (returnExpr e,Void)
    Just (TFunc t1 t2)-> (returnExpr e,(TFunc t1 t2)) --this case is for arguments that are functions (a->Eff r (...))
    Just (Container n ts ) -> (returnExpr e, Void)
    otherwise -> 
      case (Map.lookup name tFuncs) of
         Just (TFunc t1 t2) -> (e, TFunc t1 t2)
         otherwise -> (e,Void) 
toMonad (e@(Apat (ListArgs apats)),t) tApats tFuncs _ = 
  undefined

returnExpr:: Expr -> Expr
returnExpr expr = App (Apat (Var "return")) expr


uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

