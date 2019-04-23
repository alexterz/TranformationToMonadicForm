{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module ForTesting where
import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Exception
import Control.Eff.Trace
import Control.Monad
import MConvert


--auxiliary functions

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

incr :: Member (State Int) r => Eff r ()
incr = get >>= put.(+ (1:: Int))

--giving a x, returns the (x + Env + (s+1)) sum, and increases the state 
--func:: (Member (State Int) r, Member (Reader Int) r) => Int -> Eff r Int
totalAdd x = do 
           incr --incr2: increases the s2
           let sum1 = ask `add` return x
           sum1 `add` get 

--giving  x, y , returns the (x+y+env1+env2+s1+1+s2+1) sum, and increases both states (s1,s2)
combTotalAdd:: (Member (State Int) r, Member (Reader Int) r) => Int -> Eff r (Int -> Eff r Int)
combTotalAdd x = do -- The outer Eff r has a different State (s1) from those of the inner one Eff (s2).
                   incr -- incr1 :increases the s1. 
                   h <- (ask `add` return x) `add` get   
                   let 
                     f y = totalAdd y `add` return h
                   return f

--giving x, returns (x+env) sum
--addEnv::  (Member (Reader Int) r) => Int -> Eff r Int
addEnv x = do 
           ask `add` return x
          -- sum1 `add` get 

-- giving x, returns the (x+env+s1+1) sum and increases the state, using addEnv
--addEnvState:: (Member (State Int) r1, Member (Reader Int) r2) => Int -> Eff r1 (Int -> Eff r2 Int)
addEnvState x = do 
                  incr -- incr1 :increases the s1. 
                  h <- (return x) `add` get   
                  let 
                      f y = addEnv y `add` return h
                  return f


