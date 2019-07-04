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


runPrint :: [AllDclr] -> String
runPrint [] = ""
runPrint (d:ds) = (printAllDclr d ++ runPrint ds)

printAllDclr :: AllDclr -> String
printAllDclr (Dclr d) = printDclr d--undefined 
printAllDclr (WithSign typesign d) = (printTypeSign typesign) ++ printDclr d ++"\nok"

printTypeSign :: TypeSignature -> String
printTypeSign (Signature name t) = name ++ " :: " ++ printType t
printTypeSign (ContSignature name cont  t) =name ++ " :: " ++ " (" ++ printContext cont ++ ") =>"++ printType t

printType :: Type -> String
printType (Literal name) = name
printType (TFunc t1 t2) = "(" ++ printType t1  ++ ") -> " ++ "(" ++ printType t2 ++")" 
printType (Container name t) = " (" ++ name ++ " (" ++ printType t ++")" ++ " )"
 

printContext :: [Context] -> String
printContext [] = ""
printContext [Constraint c n] = c ++ " " ++ n 
printContext ((Constraint c n):ls) = c ++ " " ++ n ++", " ++ printContext ls 

printDclr:: Dclr -> String
printDclr (Assign name apats expr) = name ++" " ++ printApats apats ++" = " ++ "ok \n"--printExpr -- VAR Apats '=' Expr

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

