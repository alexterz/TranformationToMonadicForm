module Transformation (
  runTransformation 
) where

import Syntax

import Control.Monad.Except
import Control.Monad.Extra
import Control.Eff
import Control.Monad
import Data.Function
import qualified Data.Map as Map



runTransformation:: [AllDclr]->[AllDclr]
runTransformation [] = []
runTransformation (d:ds) = (transformAllDclr d): (runTransformation ds)

transformAllDclr:: AllDclr->AllDclr
transformAllDclr (Dclr d) = Dclr d
transformAllDclr (WithSign typesign d) = (WithSign (transformTypeSign typesign) (transformDclr d))

transformTypeSign :: TypeSignature -> TypeSignature
transformTypeSign (Signature name t) = Signature name (transformType t)
--transformTypeSign (ContSignature name cont  t) =ContSignature name (transformContext cont) (transformType t)

transformType :: Type -> Type
transformType (Literal name) = Literal name
transformType (TFunc t1 t2) =TFunc (transformType t1)  (Container "Eff" [Literal "r" ,transformType t2])  
transformType (Container name t) = Container name t 
transformType (TList t) = TList t


transformDclr (Assign name apats expr) = (Assign name apats expr)
