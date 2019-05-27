module Eval (
  runEval,
  runMain 
) where

import Syntax

import Control.Monad.Except
import qualified Data.Map as Map

data Value
  = VInt Int
  | VBool Bool
  | VClosure [Apats] Expr (Eval.Scope)
-- | Values [Value]



instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"
--  show (Values {}) = "<<>values>"



type Eval t = Except String t

type Scope = Map.Map String [Value] -- me case [args] of tha kanw to pattern matching

--type Definition = Value

runMain :: [Dclr] -> Either String [Value]
runMain x = runExcept $ evalMain emptyScope x

evalMain :: Eval.Scope -> [Dclr] -> Eval [Value]
--given some declarations, evaluates the last one (e.g main)
evalMain env [(Assign name args expr)] = eval env expr -- edw mallon tha kanw to pattern matching, pros to paron asto, me noiazei mono o orismos
evalMain env (x:xs) = do 
                      env' <- assign [x] env
                      evalMain env' xs 

eval :: Eval.Scope -> Expr -> Eval [Value]
eval env expr = case expr of
  Apat (Lit (LInt x)) -> return $ [VInt (fromIntegral x)]
  Apat (Lit (LBool x)) -> return $ [VBool x]
  Apat (Var x) -> case (Map.lookup x env) of
                          Just (l:ls) ->  return $ l:ls
                          Nothing -> throwError ("Can't find Variable"++ show x)
  Lam x body -> return [VClosure x body env]
  App a b -> do
    y <- eval env b   
    x <- eval env a--tryMatch a y env
    apply x y
    --x <- eval env a
    --check a b x y env         
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
assign ((Assign name args expr'):xs) env = 
                     let  value =  runExcept $ eval env expr'
                     in   case value of 
                              Left err -> throwError "Error on the assigned expression in declaration"
                              Right val -> assign xs env'
                                       where 
                                       env' = extendDef env name val  --mhpws gia anadromh thelei kai sto right hand env'


binop :: Binop -> [Value] -> [Value] -> Eval [Value]
binop Add [VInt a] [VInt b] = return $ [VInt (a+b)]
binop Sub [VInt a] [VInt b] = return $ [VInt (a-b)]
binop Mul [VInt a] [VInt b] = return $ [VInt (a*b)]
binop Eql [VInt a] [VInt b] = return $ [VBool (a==b)]
binop _ _ _ = throwError "Tried to do arithmetic operation over non-number"

-- updates function's Definitions
extendDef :: Scope -> String -> [Value] -> Scope 
extendDef env v t = case (Map.lookup v env) of
                      Just l-> Map.insert v list env -- list contains all the definitions of the form ([args],Value) for a function with name v
                              where list = l ++ t
                      Nothing ->  Map.insert v t env


-- updates the local env
extendLocal :: Scope -> String -> [Value] -> Scope 
extendLocal env v t = Map.insert v t env


--apply the function to the new env
--edw prepei na metatrepsw to Apats se String
apply :: [Value] -> [Value] -> Eval [Value]
apply [VClosure [v] t0 e] [t1] = case v of
                                   Var name -> eval (extendLocal e name [t1]) t0
                                   Lit (LInt literal) -> case t1 of 
                                                           (VInt x) -> if (x==literal) 
                                                                       then eval e t0 
                                                                       else return []--throwError"Error on pattern matching 1" 
                                                           otherwise -> throwError("Cant match "++ show t1 ++ " with Int")
                             --  otherwise -> eval e t0  
apply [VClosure (v:vs) t0 e] [t1] =  case v of
                                       Var name -> eval (extendLocal e name [t1]) (Lam vs t0) 
                                       Lit (LInt literal) -> case t1 of 
                                                              (VInt x) -> if (x==literal) 
                                                                          then eval e (Lam vs t0) 
                                                                          else return [] --throwError"Error on pattern matching" 
                                                              otherwise -> throwError ("Cant match "++ show t1 ++ " with Int")
                             --   otherwise -> eval e (Lam vs t0) 
--apply (VClosure '_' t0 e) t1 = eval env t0
apply (l:ls) t = case (runExcept $ apply [l] t) of 
                    Right values -> do x<- apply ls t 
                                       return $ values ++ x
                    otherwise -> apply ls t
apply _ _  = throwError "Tried to apply closure"



emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> Either String [Value]
runEval x = runExcept (eval emptyScope x)

{--
--an to y den einai Value alla string apo error???
tryMatch:: Expr -> Value -> Scope -> Eval Value
tryMatch a y env = case a of
                      (Apat (Var x)) -> case (Map.lookup x env) of
                              Just (l:ls) -> let 
                                                newDefs = checkMatch y (l:ls)
                                                env' = extendLocal env x newDefs    
                                              in  
                                                case newDefs of
                                                 [] -> throwError ("Pattern Matching failed for function"++ show x ++ "and variable "++ show y) 
                                                 otherwise -> eval env' a  
                              otherwise   -> throwError ("can't find function"++ show x) 
                      otherwise     ->  eval env a


check:: Expr -> Expr -> Value -> Value -> Scope -> Eval Value
check a b x y env =let 
                      checkPattern =  apply x y 
                      check = runExcept checkPattern
                   in
                     case check of
                       Right pat -> checkPattern
                       Left  err -> case (deleteDef env a) of --throwError "errooooorrrrrrr"
                                       ([], _)  -> throwError "Not exaustive patterns"
                                       (_, env')-> eval env' (App a b) --throwError "Try other patterns"

deleteDef:: Scope -> Expr -> ([Definition], Scope)
deleteDef env a =  case a of
                      (Apat (Var x)) -> case (Map.lookup x env) of
                              Just (l:ls) -> (ls, extendLocal env x ls)
                              otherwise   -> ([], extendLocal env x [])  
                      App c b    -> deleteDef env c -- na to tsekarw giati doyleyeiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
                                             

--takes a value and a list of function's definitions and tries to find the right match (returns all the definitions that matches the first arg, but without its first argument) with the following way:
--takes the first arg of the head definition and if its a number checks if its equal with the given value, 
--else if its a variable it matches either way, otherwise doesn't match for sure
checkMatch:: Value -> [Definition] -> [Definition]
checkMatch _ [] = []--throwError "Pattern Matching failed"
checkMatch (VInt int) (x:xs)= let listArgs = fst $ x
                                  value = snd $ x
                              in case listArgs of
                                ((Lit(LInt l)):ls) -> if (int==l) 
                                                      then ((ls,value):checkMatch (VInt int) xs ) 
                                                      else checkMatch (VInt int) xs
                                ((Var l):ls)       -> ((ls,value):checkMatch (VInt int) xs ) 
                                otherwise          -> checkMatch (VInt int) xs 
                             
--}
