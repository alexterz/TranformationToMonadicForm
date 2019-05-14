module Eval (
  runEval,
) where

import Syntax

import Control.Monad.Except
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr (Eval.Scope)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"

type Eval t = Except String t

type Scope = Map.Map String Value

eval :: Eval.Scope -> Expr -> Eval Value
eval env expr = case expr of
  Lit (LInt x) -> return $ VInt (fromIntegral x)
  Lit (LBool x) -> return $ VBool x
  Var x -> return $ env Map.! x
  Lam x body -> return (VClosure x body env)
  App a b -> do
    x <- eval env a
    y <- eval env b
    apply x y
  Op op a b -> do
    x <- eval env a
    y <- eval env b
    binop op x y
  Let dclrs expr -> do 
                    env' <- assign dclrs env
                    eval env' expr    
    

-- takes a list of declarations and an env and returns an updated env'
assign:: [Dclr] -> Scope ->Eval Scope 
assign [] env = return env
assign ((Assign name expr'):xs) env = 
                     let  value =  runExcept $ eval env expr'
                     in   case value of 
                              Left err -> throwError "Error on the assigned expression in declaration"
                              Right val -> assign xs env'
                                       where 
                                       env' = extend env name val  --mhpws gia anadromh thelei kai sto right hand env'


binop :: Binop -> Value -> Value -> Eval Value
binop Add (VInt a) (VInt b) = return $ VInt (a+b)
binop Sub (VInt a) (VInt b) = return $ VInt (a-b)
binop Mul (VInt a) (VInt b) = return $ VInt (a*b)
binop Eql (VInt a) (VInt b) = return $ VBool (a==b)
binop _ _ _ = throwError "Tried to do arithmetic operation over non-number"

-- updates the env
extend :: Scope -> String -> Value -> Scope 
extend env v t = Map.insert v t env

--apply the function to the new env
apply :: Value -> Value -> Eval Value
apply (VClosure v t0 e) t1 = eval (extend e v t1) t0
apply _ _  = throwError "Tried to apply closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> Either String Value
runEval x = runExcept (eval emptyScope x)
