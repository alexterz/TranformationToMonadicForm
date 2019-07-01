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
  show (Value args expr env) = 
    case args of
     []-> case expr of
            Apat (Lit (LInt num)) -> show num
            Apat (Lit (LBool b))  -> show b
            List l -> show l
            otherwise -> show expr
     otherwise->
           "<<value>>: /"++show args ++"->" ++ show expr  

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

--for local envs
extendEnv:: Env -> Env -> Env
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
eval env (List l) = 
  return [Value [] (List (forceList l env)) env] 
eval env (Apat (Var x)) =
  case Map.lookup x env of
    Just values -> force values
    Nothing -> throwError ("Can't find Variable "++ show x)
eval env (Lam xs body) = return [Value xs body env]
eval env (Cons expr [List listExpr]) = do 
  x <- force [Value [] expr env]
  case x of 
    [Value _ e _] -> return [Value [] (List (e:(forceList listExpr env))) env]   
eval env (Cons expr listExpr) =
  eval env (Cons expr (forceList listExpr env))
eval env (App a b) = do
  values <- eval env a
  apply values $ Value [] b env
eval env (Op op a b) = do
  x <- evalInt env a
  y <- evalInt env b
  return [Value [] (Apat (Lit (binop op x y))) env]
eval env (Let ds expr) = eval (fixEnv ds env) expr 

force :: [Value] -> Eval [Value]
force (Value [] expr env : _) =  eval env expr
force values = return values

--takes a list of expression and returns a list of evaluated expr
forceList :: [Expr] -> Env -> [Expr]
forceList [] _= []
forceList (e:es) env = e': (forceList es env)
    where v = runExcept $ eval env e
          e' = case v of   
                 Right [Value args expr env] -> expr 

evalInt :: Env -> Expr -> Eval Int
evalInt env expr = do
  values <- eval env expr
  valuesInt values

valuesInt :: [Value] -> Eval Int 
valuesInt values =
  case values of
    [Value [] (Apat (Lit (LInt n))) _] -> return n
    _ -> throwError $ "Tried to do arithmetic operation over non-number: " ++ show values

valuesList :: [Value] -> Eval Value
valuesList values = do
  val <- force values
  case val of
    ((Value [] (List a) env):xs)-> return (Value [] (List a) env)
    _ -> throwError $ "Tried to match list, but evaluated expr is not a list"


binop :: Binop -> Int -> Int -> Lit
binop Add x y = LInt $ x + y
binop Sub x y = LInt $ x - y
binop Mul x y = LInt $ x * y
binop Eql x y = LBool $ x == y

apply :: [Value] -> Value -> Eval [Value]
apply values expr = walk values expr >>= force
  where walk [] a = return []
        walk (Value [] _ _ : _) _ = throwError $ "Can't apply non-function " ++ show values
        walk (Value (x : xs) expr' env : vs) a = do
          (matched, a', env') <- match x a env
          if matched
            then do 
                    values' <- walk vs a' -- for the other definitions
                    return $ Value xs expr' env' : values'
            else walk vs a'

match :: Apats -> Value -> Env -> Eval (Bool, Value, Env)
match (Var x) value env = return (True, value, Map.insert x [value] env)
match (Lit (LInt n)) value env = do
  num <- force [value] >>= valuesInt
  --in any case, the evaluated value (num) won't be evaluated twice
  if num == n then return (True, Value [] (Apat (Lit (LInt n))) emptyEnv, env)
              else return (False, Value [] (Apat (Lit (LInt num))) emptyEnv, env) 
match (ListArgs l) (Value [] (List expr) envExp ) env = 
  matchList l expr [] envExp env
--case value is an unevaluated variable
match (ListArgs l) value env = do 
  list <- force [value]>>=valuesList
  match (ListArgs l) list env   
match _ value env = return (False, value, env)  
   
--(a:as)::listOfArguments, (e:es):: list of unevaluated expressions, expr:: list of evaluated expressions
matchList:: [Apats] -> [Expr] -> [Expr] -> Env -> Env -> Eval (Bool, Value, Env)
matchList [] [] expr envExpr env = return (True, Value [] (List expr) emptyEnv, env)
-- case  f [1,x] = x ; g = f [1,2] -> 2 
matchList [Var x] [e] expr envExpr env = do
  return (True, value, Map.insert x [val] env)
         where val = Value [] e emptyEnv
               value = Value [] (List (expr ++ [e])) envExpr 
-- case f (1:x) = x ; g = f [1,2] --> [2]
matchList [ListArgs [Var x]] e expr envExpr env =
  return (True, value, Map.insert x [val] env)
         where val = Value [] (List e) envExpr
               value = Value [] (List (expr ++ e)) envExpr   
matchList (a:as) (e:es) expr envExpr env = do
  (matched, a', env') <- match a (Value [] e envExpr) env
  case a' of 
     Value _ expr' envExpr' -> 
       if matched
          then matchList as es (expr ++ [expr']) envExpr env'
       else   
          return (False, Value [] (List ((expr ++ [expr']) ++ es)) envExpr, env)
matchList a e expr envExpr env = return (False, Value [] (List (expr++e)) envExpr, env) 

                           
  
