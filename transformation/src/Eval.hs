module Eval (
  runEval,
  runMain 
) where

import Syntax

import Control.Monad.Except
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure [Apats] Expr (Eval.Scope)
-- | Values [Value]



instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"
--  show (Values {}) = "<<>values>"

type Eval t = Except String t

type Scope = Map.Map String [Definition] -- me case [args] of tha kanw to pattern matching

type Definition = ([Apats],Value)

runMain :: [Dclr] -> Either String Value
runMain x = runExcept $ evalMain emptyScope x

evalMain :: Eval.Scope -> [Dclr] -> Eval Value
--given some declarations, evaluates the last one (e.g main)
evalMain env [(Assign name args expr)] = eval env expr -- edw mallon tha kanw to pattern matching, pros to paron asto, me noiazei mono o orismos
evalMain env (x:xs) = do 
                      env' <- assign [x] env
                      evalMain env' xs 

eval :: Eval.Scope -> Expr -> Eval Value
eval env expr = case expr of
  Apat (Lit (LInt x)) -> return $ VInt (fromIntegral x)
  Apat (Lit (LBool x)) -> return $ VBool x
  Apat (Var x) -> return $ let 
                            def = case env Map.! x of
                                    (y:ys) -> y
                           in  snd $ def
 
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
    



checkArgs:: [Apats] -> [Definition] -> Eval Value
checkArgs args [] = throwError "Pattern Matching failed"
checkArgs args (x:xs)= if (args == (fst $ x)) then return $ snd $ x
                       else checkArgs args xs 


-- takes a list of declarations and an env and returns an updated env'
assign:: [Dclr] -> Scope ->Eval Scope 
assign [] env = return env
assign ((Assign name args expr'):xs) env = 
                     let  value =  runExcept $ eval env expr'
                     in   case value of 
                              Left err -> throwError "Error on the assigned expression in declaration"
                              Right val -> assign xs env'
                                       where 
                                       env' = extend env name (args,val)  --mhpws gia anadromh thelei kai sto right hand env'


binop :: Binop -> Value -> Value -> Eval Value
binop Add (VInt a) (VInt b) = return $ VInt (a+b)
binop Sub (VInt a) (VInt b) = return $ VInt (a-b)
binop Mul (VInt a) (VInt b) = return $ VInt (a*b)
binop Eql (VInt a) (VInt b) = return $ VBool (a==b)
binop _ _ _ = throwError "Tried to do arithmetic operation over non-number"

-- updates the env
extend :: Scope -> String -> Definition -> Scope 
extend env v t = case (Map.lookup v env) of
                   Just l-> Map.insert v list env -- list contains all the definitions of the form ([args],Value) for a function with name v
                            where list = l++ [t]
                   Nothing ->  Map.insert v [t] env



--apply the function to the new env
--edw prepei na metatrepsw to Apats se String
apply :: Value -> Value -> Eval Value
apply (VClosure [v] t0 e) t1 = case v of
                                  Var name -> eval (extend e name ([Var name],t1)) t0
                                  otherwise -> eval e t0  
apply (VClosure (v:vs) t0 e) t1 =  case v of
                                  Var name -> eval (extend e name ([Var name],t1)) (Lam vs t0) 
                                  otherwise -> eval e t0 

--apply (VClosure '_' t0 e) t1 = eval env t0
apply _ _  = throwError "Tried to apply closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> Either String Value
runEval x = runExcept (eval emptyScope x)
