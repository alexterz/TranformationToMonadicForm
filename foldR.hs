{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}


import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Exception
import Control.Eff.Trace
import Control.Monad
import MConvert


{--
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)
--}


--foldr' ::Monad m => (a-> m (b -> m b)) -> b -> [a] -> m b
foldr' ::(a-> Eff r (b -> Eff r b)) -> b -> [a] -> Eff r b
foldr' f z [] = return z
foldr' f z (x:xs) = do
                       y  <- foldr' f z xs
                       g  <- f x 
                       h  <- g y
                       return h

--foldrEff:: (Monad m, Monad m1, Monad m2) => (a-> m (b -> m b)) -> m1 (b -> m2 ([a] -> m b))
foldrEff ::(a-> Eff r (b -> Eff r b)) -> Eff r1 (b -> Eff r2 ([a] -> Eff r b))-- or r1=r2=r 
foldrEff = mConvert2 foldr'


--auxiliary functions

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

incr :: Member (State Int) r => Eff r ()
incr = get >>= put.(+ (1:: Int))


--func::  (Member (Reader Int) r) => Int -> Eff r Int
func x = do 
           ask `add` return x
          -- sum1 `add` get 

--func1:: (Member (State Int) r1, Member (Reader Int) r2) => Int -> Eff r1 (Int -> Eff r2 Int)
func1 x = do 
           incr -- incr1 :increases the s1. 
           h <- (return x) `add` get   
           let 
               f y = func y `add` return h
           return f

-- examples

--r is the union of r1-r2. It needs type annotation
t1:: (Member (State Int) r, Member (Reader Int) r) => Eff r Int
t1  = (foldr' func1 0 [0,5])  -- func1 0 (func1 0)

--here you can run t1'', without type annotation, because its type is inferred by the run functions that you use
--t1'' = run $ runState (0::Int) $ runReader (1::Int) $ foldr' func1 0 [0,0]

t1' = run $ runState (0::Int) $ runReader (1::Int) $ t1  -- func1 0 (1+1+0) = 2+1+5 +0 (+1+1) 
--(5,2)

-- Every r, is inferred by the run functions that you use.
--here, for example, foldrEff 0 [0,5] has type ::(Member (State Int)r1, Member (State Int) r, Member (Reader Int) r)) =>  Eff r1 (b -> Eff r2 ([a] -> Eff r b)), while r2 = [] (or Void)
t2 = run $ runState (5::Int) $ runReader (0::Int) $ ( run $ (fst (run $ runState (0::Int) $ (foldrEff func1))) 0) [0,5]
--(18,7)

                                 
