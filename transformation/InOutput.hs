{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-#LANGUAGE FlexibleContexts, TypeOperators, DataKinds #-}

import Control.Monad.State.Lazy
import Control.Monad.Except

{--Syntax Dclr: [WithSign (ContSignature "plusOne" [] (Type (TFunc (Literal "Integer") (Literal "Integer")))) [Assign "plusOne" [Var "x"] (Op Add (Apat (Var "x")) (Apat (Lit (LInt 1))))],WithSign (ContSignature "result" [] (Type (Literal "Integer"))) [Assign "result" [] (App (Apat (Var "plusOne")) (Apat (Lit (LInt 2))))]]--}
plusOne :: (Integer-> Integer)
plusOne x = (x+1);
result :: Integer
result = (plusOne 2);


main::IO ()
main= putStrLn $show $result