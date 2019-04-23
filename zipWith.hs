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
import ForTesting


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


--examples

--combTotalAdd imported from ForTesting, is the function that giving  x, y , returns the (x+y+env1+env2+s1+1+s2+1), and increases both states (s1,s2)

-- When we unwrap the monad, the innermost runstate (15::Int) affects the outermost monad Eff (s1), which is increased by the incr1 of combTotalAdd.
t1 = run $ runState (0::Int) $ runReader (1::Int) $  (fst (run $ runState (15::Int) $ runReader (100::Int) (combTotalAdd 5))) 6 
-- (129,1)

t1' = run $ runState (0::Int) $ runReader (1::Int) $ (zipWith' combTotalAdd [0,0] [0,0]) 
-- ([5,9],4)

-- the runstate that affects the result is the outer one (5::Int), which is matched with the inner state of zipWithEff 
t2 = run $ runState (5::Int) $ runReader (0::Int) $ (fst $ run $ runState (0::Int) $ runReader (0::Int) $ (fst (run $ runState (0::Int) $ runReader (0::Int) $ (zipWithEff combTotalAdd))) [0]) [0]

--([13],7)













