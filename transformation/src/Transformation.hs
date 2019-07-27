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
      ((dclr: list), alltFuncs)
      where (dclr,cont) = transformAllDclr d alltFuncs tApats
            typedFuncs =  Map.insert name t' tFuncs --check if it works-- tFuncs --it works but it's not lazy! fix it! 
           -- contexts = Map.insert name cont allCont  
            (list, alltFuncs) = (runTransformation ds typedFuncs tApats)
    Type t' ->   
      ((dclr: list), alltFuncs)
      where (dclr,cont) = transformAllDclr d alltFuncs tApats
            typedFuncs =  Map.insert name t' tFuncs --check if it works-- tFuncs --it works but it's not lazy! fix it!
            (list, alltFuncs) = (runTransformation ds typedFuncs tApats)  

transformAllDclr :: AllDclr-> TypedFuncs-> TypedApats-> (AllDclr,[Context])
transformAllDclr (WithSign typesign@(ContSignature name cont t) d) tFuncs tApats=
  ((WithSign signToMonad dclr),cont)
    where 
      (dclr) = transformDclrs d t' tApats tFuncs
      (signToMonad@(ContSignature name cont t))= transformTypeSign typesign
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
transformType (Container "Either" [t1,t2]) cont delcont=
  ((Container "Eff" [Literal "r", Container "Either" [t1',t2']]), uniq (j++k), c1++c2)  
      where (t1',j,c1) = transformType t1 cont  []
            (t2',k,c2) = transformType t2 cont delcont             
transformType (Container "Except" types) cont delcont=
  transformType (Container "Exc" types) cont delcont          
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
    where expr' = fst (transformExpr (expr,Void) typedApats tFuncs 0) --(Lam [Var "x"] (App (Apat (Var "return")) (Apat (Var "x"))))
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
transformExpr ((App e1 e2),t) tApats tFuncs i = 
  transformSpecialExpr ((App e1 e2),t) tApats tFuncs i        
transformExpr ((Op binop e1 e2),t) tApats tFuncs i = --ok
  transformExpr ((App (App (Apat(Var x)) e1) e2),t) tApats tFuncs (i+1)
  where
    x= case binop of
         Add -> "plus"
         Sub -> "sub"
         Mul -> "multiple"
transformExpr ((Lam apats expr),t) tApats tFuncs i = undefined 
transformExpr ((Bind e1 e2),t) tApats tFuncs i = 
  case e2 of
    (Lam [(Var name)] expr) -> 
      ((Bind e1' (Lam [(Var name)] expr')),t')
      where 
        tApats' = Map.insert (Var name) Void tApats
        (expr',t') = transformExpr (expr,t) tApats' tFuncs (i+1) 
    otherwise -> 
      ((Bind e2' (Lam [Var ("f"++show i)] 
      (Bind e1' (Apat(Var ("f"++show i)))))),t2)                                 
  where
    (e2',TFunc t1 t2) = transformExpr (e2,t) tApats tFuncs (i+1)
    (e1',_) = transformExpr (e1,t) tApats tFuncs (i+1)
transformExpr ((Str e),t) _ _ _= (returnExpr(Str e),t)      


transformExprs:: [Expr] ->Type -> TypedApats ->TypedFuncs -> Integer -> [Expr]
transformExprs [] t tApats tFuncs i = []
transformExprs (e:es) t tApats tFuncs i = (fst (transformExpr(e,t) tApats tFuncs i)):(transformExprs es t tApats tFuncs i)


transformSpecialExpr:: (Expr,Type)-> TypedApats ->TypedFuncs -> Integer-> (Expr,Type)
-----------------------Functions for STate Monad ---------------------------------------------------------------
transformSpecialExpr ((App (App (Apat(Var "runState")) e2) e3),t) tApats tFuncs i = 
  ((Bind e3' (Lam [Var ("s"++show i)] (App (App (Apat(Var "runState")) (Apat(Var ("s"++show i)))) e2'))),t) 
  where
    (e2',_) = transformExpr (e2,t) tApats tFuncs (i+1)
    (e3',_) = transformExpr (e3,t) tApats tFuncs (i+1)
transformSpecialExpr ((App (Apat(Var "put")) e2),t) tApats tFuncs i =
  ((Bind e2' (Lam [Var ("s"++show i)] (App (Apat(Var "put")) (Apat(Var ("s"++show i)))))),t) 
  where
    (e2',_) = transformExpr (e2,t) tApats tFuncs (i+1)
---------------------Functions for Exception Monad -------------------------------------------------------------
transformSpecialExpr ((App (Apat(Var "throwError")) e2),t) tApats tFuncs i =
  ((Bind e2' (Lam [Var ("e"++show i)] (App (Apat(Var "throwError")) (Apat(Var ("e"++show i)))))),Void) 
  where
    (e2',_) = transformExpr (e2,t) tApats tFuncs (i+1) 
transformSpecialExpr ((App (Apat(Var "runExcept")) e2),t) tApats tFuncs i = 
  (App (Apat(Var "runError")) e2',Void) 
  where
    (e2',_) = transformExpr (e2,t) tApats tFuncs (i+1)      
----------------------------------------------------------------------------------------------------------------  
transformSpecialExpr ((App e1 e2),t) tApats tFuncs i = --ok
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



toMonad:: (Expr,Type) -> TypedApats -> TypedFuncs -> Integer -> (Expr,Type) --Expr
toMonad (e@(Apat (Lit _)),t) tApats tFuncs _ = 
  (returnExpr e,Void)
toMonad (e@(Apat (Var "get")),t) tApats tFuncs _ = 
  (e, Void)  
toMonad (e@(Apat (Var "return")),t) tApats tFuncs _ = 
  (returnExpr e, TFunc (Literal "a") (Container "m" [Literal "a"]))
toMonad (e@(Apat (Var name)),t) tApats tFuncs _ = 
  case (Map.lookup (Var name) tApats) of -- for literals οκ
    Just (Void) -> (returnExpr e,Void) 
    Just (Literal n) -> (returnExpr e,Void)
    Just (TList t) -> (returnExpr e,Void)
    Just (TFunc t1 t2)-> (returnExpr e,(TFunc t1 t2)) --this case is for arguments that are functions (a->Eff r (...))
    Just (Container n ts ) -> (e, Void)
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

