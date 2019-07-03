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
runPrint (d:ds) = (printDclr d ++ runPrint ds)

printDclr :: AllDclr -> String
printDclr (Dclr d) = "ok"--undefined 
printDclr (WithSign typesign d) = (printTypeSign typesign) ++ " ok"

printTypeSign :: TypeSignature -> String
printTypeSign (Signature name t) = name ++ " :: " ++ printType t
printTypeSign (ContSignature name cont  t) =name ++ " :: " ++ " (" ++ printContext cont ++ ") =>"++ printType t

printType :: Type -> String
printType (Literal name) = name
printType (TFunc t1 t2) = "(" ++ printType t1  ++ ") -> " ++ "(" ++ printType t2 ++")" 
printType (Container name t) = name ++ " (" ++ printType t ++")"
 

printContext :: [Context] -> String
printContext [] = ""
printContext [Constraint c n] = c ++ " " ++ n 
printContext ((Constraint c n):ls) = c ++ " " ++ n ++", "  
