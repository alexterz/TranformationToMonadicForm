module Eval (
  runMain 
) where

import Syntax

import Control.Monad.Except
import Control.Monad.Extra
import Data.Function
import qualified Data.Map as Map

data Value = Value [Apats] Expr Env

instance Show Value where
  show (Value l expr env) = 
    case l of
     []-> case expr of
            Apat (Lit (LInt num)) -> show num
            Apat (Lit (LBool b))  -> show b
     otherwise->
           "<<value>>: /"++show l ++"->" ++ show expr 

type Env = Map.Map String [Value]

emptyEnv :: Env
emptyEnv = Map.empty

type Eval t = Except String t


runMain :: [Dclr] -> Either String [Value]
runMain ds = runExcept $ eval env $ Apat (Var name)
  where env = fixEnv ds emptyEnv
        name = case last ds of
                 Assign name args expr -> name

fixEnv :: [Dclr] -> Env -> Env
fixEnv ds env = env'
  where envDclr = assign ds env'
        env' = extendEnv env envDclr

extendEnv envOuter envInner = Map.foldrWithKey Map.insert envOuter envInner

assign :: [Dclr] -> Env -> Env
assign [] env = emptyEnv
assign (Assign name args expr : ds) env = extendDef envRest name thisValue
  where thisValue = Value args expr env
        envRest = assign ds env

extendDef :: Env -> String -> Value -> Env 
extendDef env name val =
  case Map.lookup name env of
    Just l -> Map.insert name (val : l) env 
    Nothing -> Map.insert name [val] env

eval :: Env -> Expr -> Eval [Value]
eval env e@(Apat (Lit _)) = return [Value [] e env]
eval env (Apat (Var x)) =
  case Map.lookup x env of
    Just values -> force values
    Nothing -> throwError ("Can't find Variable "++ show x)
eval env (Lam xs body) = return [Value xs body env]
eval env (App a b) = do
  values <- eval env a
  apply values $ Value [] b env
eval env (Op op a b) = do
  x <- evalInt env a
  y <- evalInt env b
  return [Value [] (Apat (Lit (binop op x y))) env]
eval env (Let ds expr) = eval (fixEnv ds env) expr 

force :: [Value] -> Eval [Value]
force (Value [] expr env : _) = eval env expr
force values = return values

evalInt :: Env -> Expr -> Eval Int
evalInt env expr = do
  values <- eval env expr
  valuesInt values

valuesInt :: [Value] -> Eval Int 
valuesInt values =
  case values of
    [Value [] (Apat (Lit (LInt n))) _] -> return n
    _ -> throwError $ "Tried to do arithmetic operation over non-number: " ++ show values

binop :: Binop -> Int -> Int -> Lit
binop Add x y = LInt $ x + y
binop Sub x y = LInt $ x - y
binop Mul x y = LInt $ x * y
binop Eql x y = LBool $ x == y

apply :: [Value] -> Value -> Eval [Value]
apply values expr = walk values expr >>= force
  where walk [] a = return []
        walk (Value [] _ _ : _) _ = throwError $ "Can't apply non-function " ++ show values
        walk (Value (x : xs) expr env : vs) a = do
          (matched, a', env') <- match x a env
          if matched
            then do values' <- walk vs a'
                    return $ Value xs expr env' : values'
            else walk vs a'

match :: Apats -> Value -> Env -> Eval (Bool, Value, Env)
match (Var x) value env = return (True, value, Map.insert x [value] env)
match (Lit (LInt n)) value env = do
  num <- force [value] >>= valuesInt
  if num == n then return (True, Value [] (Apat (Lit (LInt n))) emptyEnv, env)
              else return (False, Value [] (Apat (Lit (LInt num))) emptyEnv, env)

