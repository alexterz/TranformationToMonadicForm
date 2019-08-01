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


printTypeSign :: TypeSignature  -> String
printTypeSign (ContSignature name []  t) = name ++ " :: " ++ (printTypeScope t [])
printTypeSign (ContSignature name cont  t) =name ++ " :: " ++ " (" ++ printContext cont ++ ") =>"++ printTypeScope t cont

printTypeScope:: TypeScope -> [Context] -> String
printTypeScope (ForAll names t) cont = "forall "++ printNames names ++"." ++ printType t cont 
printTypeScope (Type t) cont = printType t cont 

printNames:: [Name] -> String
printNames [] = ""
printNames (n:ns) = n ++ " " ++ printNames ns 

printType :: Type -> [Context] -> String
printType (Literal name) _ = name
printType (TFunc t1 t2) cont = "(" ++ printType t1 cont  ++"-> "  ++ printType t2 cont++ ")" 
--printType (Container name []) = "(" ++ name ++" () )" 
printType (Container name t) cont = "(" ++ name ++" " ++ printTypeList t cont ++")"
printType effUnion@(EffUnionContainer name t1 t2) cont = printEffUnionContainer effUnion (delClassCont cont) (delClassCont cont)
printType (TList t) cont= "["++ printType t cont ++"]"
printType (TTuple ts) cont= "("++ printTuples ts cont ++")"
printType (Void) _= "()"

delClassCont:: [Context] -> [Context]
delClassCont [] =[]
delClassCont ((Constraint (Class _) _):cs) = delClassCont cs
delClassCont (c:cs) = c:(delClassCont cs)  

-- if Monad Constraint exists in the Context, dont print it inside type, otherwise print it 
printEffUnionContainer:: Type -> [Context] -> [Context] -> String
printEffUnionContainer (EffUnionContainer "empty" t _) _ cont = "(Eff r " ++ printType t cont ++ ")"
printEffUnionContainer (EffUnionContainer name t1 t2) [] cont = 
 "(Eff (" ++ name ++" "++ printType t2 cont ++ " ': r) "++ printType t2 cont ++ ")"
printEffUnionContainer (EffUnionContainer name t1 t2) ((Constraint (Member n _) _):cs) cont=
 if name == n then "(Eff r " ++ printType t2 cont ++ ")" else  printEffUnionContainer (EffUnionContainer name t1 t2) cs cont

printTuples:: [Type] -> [Context] -> String
printTuples [] _ = ""
printTuples [l] cont = printType l cont
printTuples (l:ls) cont = printType l cont++ "," ++ printTuples ls cont
 
printTypeList :: [Type] -> [Context] -> String
printTypeList [] _= ""
printTypeList (t:ts) cont = printType t cont ++ " " ++ printTypeList ts cont

printContext :: [Context] -> String
printContext [] = ""
printContext [Constraint c n] = printConstraint c ++ " " ++ n 
printContext ((Constraint c n):ls) = printConstraint c ++ " " ++ n ++", " ++ printContext ls


printConstraint:: Constraint-> String
printConstraint (Member n1 n2) = "Member ("++ n1 ++ " "++ n2 ++")" 
printConstraint (Class n) = n 


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
printApat (ListArgs apats) = printListArgs apats
printApat (Constructor n apats) = "(" ++ n ++ " " ++ printApat apats ++ ")"

printListArgs ::[Apats] -> String
printListArgs [] = "[]"
printListArgs [l] = "[" ++ printApat l ++ "]"
printListArgs (l:ls) = 
  case (last ls) of
    ListArgs [Var n] -> "(" ++ printConsElem (init (l:ls)) ++ " : " ++  n ++ ")" 
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
printExpr (Tuple exprs) =
   "(" ++ printListExpr exprs ++")"  
printExpr (Cons e1 e2) =
     "(" ++ printExpr e1 ++ ":" ++ printExpr e2 ++")"
printExpr (App e1 e2) = 
  "("++ printExpr e1 ++ " " ++ printExpr e2 ++")" 
printExpr (Bind e1 e2) =
  "("++ printExpr e1 ++ ">>=" ++ printExpr e2 ++")"  
printExpr (Str e) =
 ('"') :(strs e ++ ['"'])
printExpr (Case expr cases) = 
 "("++ "case " ++ (printExpr expr) ++ " of " ++  printCases cases ++")"

strs [] = []
strs [s] = s
strs (s:ss) = s++" "++strs ss

printCases:: [Case] -> String
printCases [] = ""
printCases (c:cs) = printCase c ++"; "++printCases cs

printCase:: Case -> String
printCase (Condition apats expr) = printApat apats ++ "->" ++ printExpr expr 

printListExpr:: [Expr]->String
printListExpr [] = ""
printListExpr [l] = printExpr l
printListExpr (l:ls) = printExpr l ++ "," ++ printListExpr ls

printBiOp:: Binop -> String
printBiOp (Add) = "+"
printBiOp (Sub) = "-"
printBiOp (Mul) = "*"
printBiOp (Div) = "`div`"
--printBiOp (Eql) = "=="


