module Eval (
  runEval,
  runMain 
) where

import Syntax

import Control.Monad.Except
import Data.Function
import qualified Data.Map as Map

data Value
  = VInt Int
  | VBool Bool
  | VClosure [Apats] Expr Scope




instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VClosure l expr env) =  "<<closure>>: /"++show l ++"->" ++ show expr



type Eval t = Except String t


type Scope = Map.Map String [Value] -- me case [args] of tha kanw to pattern matching




runMain :: [Dclr] -> Either String [Value]
runMain x = runExcept $ evalMain emptyScope x


evalMain :: Scope -> [Dclr] -> Eval [Value]
--given some declarations, evaluates the last one (e.g main)
evalMain env [(Assign name args expr)] = eval env expr 
evalMain env (x:xs) = do 
                      env' <- assign [x] env
                      evalMain env' xs 


eval :: Scope -> Expr -> Eval [Value]
eval env expr = case expr of
  Apat (Lit (LInt x)) -> return $ [VInt x]
  Apat (Lit (LBool x)) -> return $ [VBool x]
  Apat (Var x) -> case x of
                     ("combY")-> return [VClosure [Var "f"] (App (Apat (Var "f")) (Lam [Var "x"] (App (App (Apat (Var "combY")) (Apat (Var "f"))) (Apat (Var "x"))))) env] 
                     otherwise ->  case (Map.lookup x env) of
                                        Just (l:ls) -> return $ (l:ls) -- fixapply x (l:ls)
                                        Nothing ->  throwError ("Can't find Variable "++ show x ++ " in the env " ++ show env)
  Lam x body -> return [VClosure x body env]
  App a b -> do
    y  <- eval env b  
    x  <- eval env a
 {--   x' <- case x of 
             [VClosure ((Var a):args) expr env']-> if ((eval env (Var a)) == [VClosure ((Var a):args) expr env']) then (applyCombY x env)
                                                   else x
             otherwise -> x    
 --}x' <- (applyCombY x env)
    apply x' y       
  Op op a b -> do
    x <- eval env a
    y <- eval env b
    binop op x y
  Let dclrs expr -> do 
                    env' <- assign dclrs env
                    eval env' expr    

eval1 :: Eval.Scope -> Expr -> Eval [Value]
eval1 env expr = case expr of
  Apat (Lit (LInt x)) -> return $ [VInt x]
  Apat (Lit (LBool x)) -> return $ [VBool x]
  Apat (Var x) -> case x of
                     ("combY")-> return [VClosure [Var "f"] (App (Apat (Var "f")) (Lam [Var "x"] (App (App (Apat (Var "combY")) (Apat (Var "f"))) (Apat (Var "x"))))) env] 
                     otherwise ->  case (Map.lookup x env) of
                                        Just (l:ls) -> return $ (l:ls) -- fixapply x (l:ls)
                                        Nothing ->  throwError ("Can't find Variable "++ show x ++ " in the env " ++ show env)
  Lam x body -> return [VClosure x body env]
  App a b -> do
    y  <- eval1 env b  
    x  <- eval1 env a
  --  comb <- eval env (Apat (Var "combY"))
 --   x' <- (applyCombY x)
 --   case x' of 
 --     [VClosure args expr env']-> throwError ("Applying "++ show x' ++ "to "++ show y++ " with env " ++ show env'++"\n") 
    apply x y        
  Op op a b -> do
    x <- eval1 env a
    y <- eval1 env b
    binop op x y
  Let dclrs expr -> do 
                    env' <- assign dclrs env
                    eval1 env' expr 



-- takes a list of declarations and an env and returns an updated env'
assign:: [Dclr] -> Scope ->Eval Scope 
assign [] env = return env
assign ((Assign name args expr'):xs) env = 
                     let  value =  runExcept $ eval env expr'
                     in   case value of 
                              Left err -> throwError (err ++ " Error on the assigned expression in declaration")              
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

applyCombY:: [Value]->Scope-> Eval [Value]
applyCombY l env = let 
                     env' = (extendLocal env ("f") l)
                   in 
                     do                               --throwError ("in ApplyCombY env :" ++ show env'')
                       x<- eval env' (Apat(Var "f")) 
                       y<- eval env' (Lam [Var "x"] (App (App (Apat (Var "combY")) (Apat (Var "f"))) (Apat (Var "x")))) 
                       apply x y      


--apply the function to the new env
--edw prepei na kanw elegxo gia typo kai plhthos orismatwn giati mexri stigmhs mporei na oristei h f x =x ; f 1 0 = 10
apply :: [Value] -> [Value] -> Eval [Value]
--for one argument (v)
apply [VClosure [v] t0 e] [t1] = case v of
                                   Var name -> eval1 (extendLocal e name [t1]) t0 --do throwError ("eval closure:"++show [t0]++ "\n"++ "old env:" ++ show e ++ "\n new env: "++ show(extendLocal e name [t1]) )-- 
                                   Lit (LInt literal) -> case t1 of 
                                                           (VInt x) -> if (x==literal) 
                                                                       then eval1 e t0 
                                                                       else return []--throwError"Error on pattern matching 1" 
                                                           otherwise -> throwError("Cant match "++ show t1 ++ " with Int")
                             --  otherwise -> eval e t0
--for more than one args (v:vs)  
apply [VClosure (v:vs) t0 e] [t1] =  case v of
                                       Var name -> eval1 (extendLocal e name [t1]) (Lam vs t0) --prin than vs kai e
                                       Lit (LInt literal) -> case t1 of 
                                                              (VInt x) -> if (x==literal) 
                                                                          then eval e' (Lam ((Var "Name"):vs) t0) --prin htan vs 
                                                                          else return [] --throwError"Error on pattern matching" 
                                                              otherwise -> throwError ("Cant match "++ show t1 ++ " with Intttt ")
                                     where e' = extendLocal e "Name" [VClosure ((Var "Name"):vs) t0 e]
                             --   otherwise -> eval e (Lam vs t0) 
--apply (VClosure '_' t0 e) t1 = eval env t0
apply [VClosure v t0 e] (t:ts) = applyCombY (t:ts) e --throwError "f Listtttttttttttttttttttttttttttttttttt"
apply (l:ls) [VClosure v t0 e] = case (runExcept $ apply [l] t) of
                                     Right values -> do x<- apply ls t 
                                                        return $ values ++ x 
                                     otherwise -> throwError "oooo"
                                 where t = [VClosure v t0 e] 
apply (l:ls) t = case (runExcept $ apply [l] t) of
                    Right [] -> apply ls t 
                    Right values -> return values--do x<- apply ls t 
                                     --  return $ values ++ x 
            --        Left err -> throwError err
            --        otherwise -> apply ls t
apply _ _  = throwError "Tried to apply closure"



emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> Either String [Value]
runEval x = runExcept (eval emptyScope x)


{--
newtype Mu a = Roll (Mu a -> (a -> a))
unroll (Roll x) = x

fix :: (a -> a) -> a -> a
fix = \f ->       (\x z -> f ((unroll x) x z))
            (Roll (\x z -> f ((unroll x) x z)))

fixClosure:: Scope -> [Value]
fixClosure env= [VClosure [Var "f"] (App (Lam [Var "x",Var "z"] (App (Apat (Var "f")) (App (App (App (Apat (Var "unroll")) (Apat (Var "x"))) (Apat (Var "x"))) (Apat (Var "z"))))) (App (Apat (Var "Roll")) (Lam [Var "x",Var "z"] (App (Apat (Var "f")) (App (App (App (Apat (Var "unroll")) (Apat (Var "x"))) (Apat (Var "x"))) (Apat (Var "z"))))))) env]
--(Apat (Lit (LInt 0))) emptyScope
--}
data Paradox a = Self (Paradox a -> a)
fixpoint'' = \f -> let half (Self twin) = f (twin (Self twin))
                   in half (Self half)


{--
tryfix:: Expr
tryfix = let 
             f = \ff x -> return $ [VInt 1] --x =return $[VInt 1]
         in
             fix f   --}

fixClosure:: Scope -> [Value]
fixClosure env= [VClosure [Var "f"] (App (Lam [Var "x"] (App (Apat (Var "f")) (App (Apat (Var "x")) (Apat (Var "x"))))) (Lam [Var "x"] (App (Apat (Var "f")) (App (Apat (Var "x")) (Apat (Var "x"))))))  env]

--fixpoint = \f -> ((\x -> f (x x)) (\x -> f (x x)))

--fixpoint f x = f (fixpoint f) x

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
