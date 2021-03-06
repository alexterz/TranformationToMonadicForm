module Print (
  runPrint 
) where

import Syntax

import Control.Monad.Except
import Control.Monad.Extra
import Control.Eff
import Control.Monad
import Data.Function
import qualified Data.Map as Map


runPrint :: [AllDclr] -> String ->  String
runPrint [] s = ""
runPrint (d:ds) s = (printAllDclr d s ++ runPrint ds s)

printAllDclr :: AllDclr -> String -> String
printAllDclr (Dclrs d) s = printDclrs d ++";"--undefined 
printAllDclr (WithSign typesign ds) s= (printTypeSign typesign) ++s++ printDclrs ds ++s1
 where s1 = (if (s =="\n") then "\n" else "")


printTypeSign :: TypeSignature -> String
printTypeSign (Signature name t) = name ++ " :: " ++ printTypeScope t
printTypeSign (ContSignature name cont  t) =name ++ " :: " ++ " (" ++ printContext cont ++ ") =>"++ printTypeScope t

printTypeScope:: TypeScope -> String
printTypeScope (ForAll names t) = "forall "++ printNames names ++"." ++ printType t 
printTypeScope (Type t) = printType t 

printNames:: [Name] -> String
printNames [] = ""
printNames (n:ns) = n ++ " " ++ printNames ns 

printType :: Type -> String
printType (Literal name) = name
printType (TFunc t1 t2) = "(" ++ printType t1  ++"-> "  ++ printType t2 ++ ")" 
--printType (Container name []) = "(" ++ name ++" () )" 
printType (Container name t) = "(" ++ name ++" " ++ printTypeList t ++")" 
printType (TList t) = "["++ printType t ++"]"
printType (Void) = "()"
 
printTypeList :: [Type] -> String
printTypeList [] = ""
printTypeList (t:ts) = printType t ++ " " ++ printTypeList ts

printContext :: [Context] -> String
printContext [] = ""
printContext [Constraint c n] = c ++ " " ++ n 
printContext ((Constraint c n):ls) = c ++ " " ++ n ++", " ++ printContext ls 


printDclr (Assign name apats expr)= name ++" " ++ printApats apats ++"= " ++ printExpr expr ++ ";" -- VAR Apats '=' Expr

printDclrs:: [Dclr] -> String
printDclrs [] = ""
printDclrs (d:ds) = printDclr d ++ printDclrs ds 
-----------------------------------------------------------------------------------------------------------------
printApats:: [Apats]-> String
printApats [] = ""
printApats (l:ls) = printApat l ++ " " ++ printApats ls


printApat:: Apats -> String
printApat (Var name) = name
printApat (Lit (LInt num )) = show num
printApat (Lit (LBool bool )) = show bool  
printApat (ListArgs apats)= printListArgs apats

printListArgs ::[Apats] -> String
printListArgs [] = "[]"
printListArgs [l] = "[" ++ printApat l ++ "]"
printListArgs (l:ls) = 
  case (last ls) of
    ListArgs [Var n] -> "(" ++ printConsElem (init (l:ls)) ++ " : " ++  n ++ ")" 
 --   ListArgs apats -> "(" ++ printConsElem (init (l:ls)) ++ " : " ++  printListArgs apats ++ ")" 
    otherwise -> "[" ++ printListElem (l:ls) ++ "]"

printConsElem:: [Apats] -> String
printConsElem [x] = printApat x
printConsElem (x:xs) = printApat x ++ " : " ++ printConsElem xs

printListElem:: [Apats]->String
printListElem [] = ""
printListElem [l] = printApat l
printListElem (l:ls) = printApat l ++ "," ++ printListElem ls

-------------------------------------------------------------------------------------------------------------------

printExpr:: Expr -> String
printExpr (Let ds expr) = 
 "("++ "let " ++  (runPrint ds ";") ++ "in " ++ printExpr expr ++")" 
printExpr (Lam apats expr) =
  "(" ++ " \\ " ++ printApats apats ++ " -> " ++  printExpr expr ++")" 
printExpr (Op binop e1 e2) = 
  "("++ printExpr e1++  printBiOp binop ++ printExpr e2  ++")"
printExpr (Apat apats) = 
  printApat apats 
printExpr (List exprs) = 
  "[" ++ printListExpr exprs ++"]"  
printExpr (Cons e1 e2) =
     "(" ++ printExpr e1 ++ ":" ++ printExpr e2 ++")"
    -- Cons e es -> "(" ++ printExpr expr ++":" ++ printExpr (Cons e es) ++")"    
   --  otherwise -> "(" ++ printExpr expr ++ ":" ++ printListExpr exprs ++ ")" 
printExpr (App e1 e2) = 
  "("++ printExpr e1 ++ " " ++ printExpr e2 ++")" 
printExpr (Bind e1 e2) =
  "("++ printExpr e1 ++ ">>=" ++ printExpr e2 ++")"
--printExpr (Monadic expr) =   
printExpr (Monadic e) = "(" ++ printExpr e ++ ")"

printListExpr:: [Expr]->String
printListExpr [] = ""
printListExpr [l] = printExpr l
printListExpr (l:ls) = printExpr l ++ "," ++ printListExpr ls

printBiOp:: Binop -> String
printBiOp (Add) = "+"
printBiOp (Sub) = "-"
printBiOp (Mul) = "*"
--printBiOp (Eql) = "=="


