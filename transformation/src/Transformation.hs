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




runTransformation :: [AllDclr]->TypedFuncs->TypedApats-> ContextFuncs ->([AllDclr],TypedFuncs,ContextFuncs)
runTransformation [] tFuncs _ cFuncs = ([],tFuncs,cFuncs)
runTransformation ((d@(WithSign (ContSignature name _ t) _)):ds) tFuncs tApats cFuncs =
  ((dclr: list), alltFuncs, allcFuncs)
  where --dclr = transformAllDclr d alltFuncs tApats 
        ((WithSign (ContSignature name cont monadicT) transfDclr),ns) = transformAllDclr d alltFuncs tApats 
        finalCont = uniq ((concat (giveCont name ns allcFuncs)) ++ cont) --thanks lazyness!!
        updatedcFuncs = Map.insert name finalCont cFuncs
        typedFuncs =  Map.insert name t' tFuncs --thanks lazyness!!
        dclr = (WithSign (ContSignature name finalCont monadicT) transfDclr)  
        (list, alltFuncs, allcFuncs) = (runTransformation ds typedFuncs tApats updatedcFuncs)        
        t' = case t of
              (ForAll names t1)-> t1
              (Type t1) -> t1  

transformAllDclr :: AllDclr-> TypedFuncs-> TypedApats -> (AllDclr,[Name])
transformAllDclr (WithSign typesign@(ContSignature name _ t) d) tFuncs tApats =
  ((WithSign signToMonad dclr),ns)
    where 
      (dclr,ns) = transformDclrs d t' tApats tFuncs
      signToMonad@(ContSignature name intermedCont monadicT)= transformTypeSign typesign
      t' = case t of
            (ForAll names t1)-> t1
            (Type t1) -> t1  

giveCont:: Name -> [Name] -> ContextFuncs -> [[Context]]
giveCont name [] _  = []
giveCont name (n:ns) cFuncs = 
  if (n==name) 
  then (giveCont name ns cFuncs)
  else       
      case Map.lookup n cFuncs of
        Just context -> ((monadicCont context):(giveCont name ns cFuncs))
        otherwise -> (giveCont name ns cFuncs)


monadicCont:: [Context] -> [Context]
monadicCont [] = []
monadicCont (c:cs) = 
  case c of 
      Constraint (Class cl) name -> monadicCont cs
      otherwise -> c:(monadicCont cs)
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
transformDclrs:: Dclrs ->Type ->TypedApats ->TypedFuncs -> (Dclrs,[Name])
transformDclrs ((Assign name apats expr):ds) t tApats tFuncs= 
  ([(Assign name [] expr' )],names)
  where
    lenApats = toInteger(length apats)
    expr' = 
        case lenApats of
          0 -> Let [Dclrs dclrs] (Apat(Var (name++"'")))
          1 -> returnExpr (Let [Dclrs dclrs] (Apat(Var (name++"'")))) 
          otherwise -> returnExpr (Let [Dclrs dclrs] (App (Apat (Var ("mConvert" ++ (show (lenApats-1))))) (Apat(Var (name++"'")))))
    (dclrs,names) = (transformlocalDclrs ((Assign name apats expr):ds) t tApats tFuncs)

transformlocalDclrs:: Dclrs -> Type-> TypedApats-> TypedFuncs ->(Dclrs, [Name])
transformlocalDclrs [] _ _ _ = ([],[])
transformlocalDclrs (d:ds) t tApats tFuncs = 
  ((dclr:dclrs),ns++ns')
  where (dclr,ns) = (transformDclr d t tApats tFuncs)
        (dclrs,ns') =(transformlocalDclrs ds t tApats tFuncs) 

transformDclr:: Dclr -> Type -> TypedApats ->TypedFuncs -> (Dclr, [Name])
transformDclr (Assign name apats expr) t tApats tFuncs= 
  ((Assign (name++"'") apats expr'), (uniq ns))
    where (expr', _ , ns) = (transformExpr (expr,Void) typedApats tFuncs [] 0) --(Lam [Var "x"] (App (Apat (Var "return")) (Apat (Var "x"))))
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
transformExpr:: (Expr,Type)-> TypedApats ->TypedFuncs ->[Name] -> Integer-> (Expr,Type,[Name]) --Expr --
transformExpr (e@(Apat apats),t) tApats tFuncs ns i = --  οκ 
  toMonad (e,t) tApats tFuncs ns
transformExpr ((List es),t) tApats tFuncs ns i = 
  (App (Apat(Var "sequence")) (List trexprs), t, ns'')
  where (trexprs,ns') = (transformExprs es t tApats tFuncs ns i) ---ok
        ns'' = case ns' of 
          [] -> ns
          otherwise -> ns'  
transformExpr ((Tuple es),t) tApats tFuncs ns i =  
  ((returnExpr (Tuple es)),t ,ns) -- (App (Apat(Var "sequenceT")) (Tuple (transformExprs es t tApats tFuncs i)),t) ---ok  
transformExpr ((Cons e1 e2),t) tApats tFuncs ns i = --οκ
  transformExpr ((App (App (Apat(Var "cons")) e1) e2), t) tApats tFuncs ns (i+1)
transformExpr ((Let ds expr),t) tApats tFuncs ns i =  
  ((Let dclrs' expr'), t, ns')
  where (dclrs',_,contFuncs) = runTransformation ds tFuncs tApats Map.empty
        (expr',_,ns') = transformExpr (expr,t) tApats tFuncs ns (i+1)
transformExpr ((App e1 e2),t) tApats tFuncs ns i = 
  transformSpecialExpr ((App e1 e2),t) tApats tFuncs ns i        
transformExpr ((Op binop e1 e2),t) tApats tFuncs ns i = --ok
  transformExpr ((App (App (Apat(Var x)) e1) e2),t) tApats tFuncs ns (i+1)
  where
    x= case binop of
         Add -> "plus"
         Sub -> "sub"
         Mul -> "multiple"
transformExpr ((Lam apats expr),t) tApats tFuncs ns i = undefined 
transformExpr ((Bind e1 e2),t) tApats tFuncs ns i = 
  case e2 of
    (Lam [(Var name)] expr) -> 
      ((Bind e1' (Lam [(Var name)] expr')),t',ns'++names)
      where 
        tApats' = Map.insert (Var name) Void tApats
        (expr',t',names) = transformExpr (expr,t) tApats' tFuncs ns (i+1) 
    otherwise -> 
      ((Bind e2' (Lam [Var ("f"++show i)] 
      (Bind e1' (Apat(Var ("f"++show i)))))),t2, ns'++ns'')                                 
  where
    (e2',TFunc t1 t2,ns'') = transformExpr (e2,t) tApats tFuncs ns (i+1)
    (e1',_,ns') = transformExpr (e1,t) tApats tFuncs [] (i+1)
transformExpr ((Str e),t) _ _ ns _= (returnExpr(Str e), t, ns)      


transformExprs:: [Expr] ->Type -> TypedApats ->TypedFuncs ->[Name] ->Integer -> ([Expr], [Name])
transformExprs [] t tApats tFuncs ns i = ([],[])
transformExprs (e:es) t tApats tFuncs ns i = ((trexpr:trexprs), (ns'++ns'')) 
  where (trexpr ,_ ,ns') = (transformExpr(e,t) tApats tFuncs ns i)
        (trexprs, ns'') = (transformExprs es t tApats tFuncs ns i)


transformSpecialExpr:: (Expr,Type)-> TypedApats ->TypedFuncs -> [Name] -> Integer-> (Expr,Type,[Name])
-----------------------Functions for STate Monad ---------------------------------------------------------------
transformSpecialExpr ((App (App (Apat(Var "runState")) e2) e3),t) tApats tFuncs ns i = 
  ((Bind e3' (Lam [Var ("s"++show i)] (App (App (Apat(Var "runState")) (Apat(Var ("s"++show i)))) e2'))),t, (ns'++ns'')) 
  where
    (e2',_,ns') = transformExpr (e2,t) tApats tFuncs ns (i+1)
    (e3',_,ns'') = transformExpr (e3,t) tApats tFuncs ns (i+1)
transformSpecialExpr ((App (Apat(Var "put")) e2),t) tApats tFuncs ns i =
  ((Bind e2' (Lam [Var ("s"++show i)] (App (Apat(Var "put")) (Apat(Var ("s"++show i)))))),t,ns') 
  where
    (e2',_,ns') = transformExpr (e2,t) tApats tFuncs ns (i+1)
---------------------Functions for Exception Monad -------------------------------------------------------------
transformSpecialExpr ((App (Apat(Var "throwError")) e2),t) tApats tFuncs ns i =
  ((Bind e2' (Lam [Var ("e"++show i)] (App (Apat(Var "throwError")) (Apat(Var ("e"++show i)))))),Void,ns') 
  where
    (e2',_,ns') = transformExpr (e2,t) tApats tFuncs ns (i+1) 
transformSpecialExpr ((App (Apat(Var "runExcept")) e2),t) tApats tFuncs ns i = 
  (App (Apat(Var "runError")) e2',Void, ns') 
  where
    (e2',_,ns') = transformExpr (e2,t) tApats tFuncs ns (i+1)      
----------------------------------------------------------------------------------------------------------------  
transformSpecialExpr ((App e1 e2),t) tApats tFuncs ns i = --ok
  case t1 of
    (Container _ _) -> 
      (Bind e1' (Lam [Var ("g"++ show i)]
      (App (Apat (Var ("g"++ show i))) e2')), t2, ns'++ns'')
    otherwise ->
      (Bind e2' (Lam [Var ("x"++show i)]                                    -- Na dw ti ginetai an to e2 otan einai monad kai oxi func
      (Bind e1' (Lam [Var ("g"++ show i)]
      (App (Apat (Var ("g"++ show i))) (Apat(Var ("x"++show i))))))),t2, ns'++ns'')
  where
    (e2',_ ,ns'') = transformExpr (e2,t) tApats tFuncs ns (i+1)
    (e1',TFunc t1 t2 , ns') = transformExpr (e1,t) tApats tFuncs [] (i+1)  



toMonad:: (Expr,Type) -> TypedApats -> TypedFuncs -> [Name] -> (Expr,Type,[Name]) --Expr
toMonad (e@(Apat (Lit _)),t) tApats tFuncs ns = 
  (returnExpr e,Void, ns)
toMonad (e@(Apat (Var "get")),t) tApats tFuncs ns = 
  (e, Void, ns)  
toMonad (e@(Apat (Var "return")),t) tApats tFuncs ns = 
  (returnExpr e, TFunc (Literal "a") (Container "m" [Literal "a"]), ns)
toMonad (e@(Apat (Var name)),t) tApats tFuncs ns = 
  case (Map.lookup (Var name) tApats) of -- for literals οκ
    Just (Void) -> (returnExpr e, Void , ns) 
    Just (Literal n) -> (returnExpr e, Void, ns)
    Just (TList t) -> (returnExpr e, Void, ns)
    Just (TFunc t1 t2)-> (returnExpr e, (TFunc t1 t2) , ns) --this case is for arguments that are functions (a->Eff r (...))
    Just (Container n ts ) -> (e, Void, ns)
    Just (TTuple ts) -> (returnExpr e, Void, ns)
    otherwise -> 
      case (Map.lookup name tFuncs) of
         Just (TFunc t1 t2) -> (e, TFunc t1 t2, name:ns)
         otherwise -> (e, Void, (name:ns)) 
toMonad (e@(Apat (ListArgs apats)),t) tApats tFuncs ns = 
  undefined

returnExpr:: Expr -> Expr
returnExpr expr = App (Apat (Var "return")) expr



uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

