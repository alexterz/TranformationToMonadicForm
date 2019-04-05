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
zipWith          :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)
                 =  z a b : zipWith z as bs
zipWith _ _ _    =  []
--}

zipWith':: (a->Eff r ( b-> Eff r c)) -> [a] -> [b] -> Eff r [c]
zipWith' z (a:as) (b:bs) = do
                              f <- z a
                              h <- f b
                              t <- zipWith' z as bs  
                              return (h:t) 
                         {--   
                              do
                                 h <- f b
                                 t <- zipWith' z as bs  
                                 return (h:t) 
                          --} 
                          
zipWith' _ _ _  = return []


--zipWithEff::(Monad m1, Monad m)=> (a->Eff r ( b-> Eff r c)) -> m ([a] -> m1 ([b] -> Eff r [c]))
zipWithEff:: (a->Eff r ( b-> Eff r c)) -> Eff r1 ([a] -> Eff r2 ([b] -> Eff r [c])) -- or r=r1=r2
zipWithEff = mConvert2 zipWith'
   

--auxiliary functions

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

incr :: Member (State Int) r => Eff r ()
incr = get >>= put.(+ (1:: Int))


--func:: (Member (State Int) r, Member (Reader Int) r) => Int -> Eff r Int
func x = do 
           incr --incr2: increases the s2
           let sum1 = ask `add` return x
           sum1 `add` get 

func1:: (Member (State Int) r, Member (Reader Int) r) => Int -> Eff r (Int -> Eff r Int)
func1 x = do -- The outer Eff r has a different State (s1) from those of the inner one Eff (s2).
           incr -- incr1 :increases the s1. 
           h <- (ask `add` return x) `add` get   
           let 
               f y = func y `add` return h
           return f


--examples
-- When we unwrap the monad, the innermost runstate (15::Int) affects the outermost monad Eff (s1), which is increased by the incr1 of func1.
t1 = run $ runState (0::Int) $ runReader (1::Int) $  (fst (run $ runState (15::Int) $ runReader (100::Int) (func1 5))) 6 
-- (129,1)

t1' = run $ runState (0::Int) $ runReader (1::Int) $ (zipWith' func1 [0,0] [0,0]) 
-- ([5,9],4)

-- the runstate that affects the result is the outer one (5::Int), which is matched with the inner state of zipWithEff 
t2 = run $ runState (5::Int) $ runReader (0::Int) $ (fst $ run $ runState (0::Int) $ runReader (0::Int) $ (fst (run $ runState (0::Int) $ runReader (0::Int) $ (zipWithEff func1))) [0]) [0]

--([13],7)













