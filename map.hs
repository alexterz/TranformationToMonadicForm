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
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
--}
map'':: (a-> Eff r b) -> [a] -> Eff r [b]
map'' f [] = return []
map'' f (h:t) = map'' f t >>= \t' -> (f h >>= \h' -> return (h':t') )


map':: (a-> Eff r b) -> [a] -> Eff r [b]
map' f [] = return []
map' f (h:t) = do
                 h'<- f h 
                 t'<- map' f t
                 return (h':t')

--mapNew:: (a-> Eff r b) -> Eff r ([a] -> Eff r [b])
mapEff:: Monad m => (a-> Eff r b) -> m ([a] -> Eff r [b])
mapEff = mConvert1 map'



--examples with map'

t1 = run $ runReader (10::Int) (map' f [1..5])
     where f x = ask `add` return x
--[11,12,13,14,15]

-- totalAdd imported from ForTesting, i sthe function that giving a x, returns the (x + Env + (s+1)), and increases the state 

t2 = run $ runState (5::Int) $ runReader (10::Int) $ map'  totalAdd [1..5]
--([17,19,21,23,25],10)

--examples with mapNew

--t3 :: ([Int] -> Eff' [Reader Int, State Int] [Int], Int)

t3 = fst $ run $ runState (5::Int) $ runReader (10::Int) $ mapEff  totalAdd 

t3'=  run $ runState (5::Int) $ runReader (10::Int) $ t3 [1..5]
--([17,19,21,23,25],10)

{-- for IO monad
map'':: (Monad m,SetMember Lift (Lift m) r)=> (a-> m b) -> [a] -> Eff r [b]
map'' f [] = return []
map'' f (h:t) = do
                 h'<- lift $ f h 
                 t'<- map'' f t
                 return (h':t')
--}



{--
mapMdebug:: (Show a, Member Trace r) => (a -> Eff r b) -> [ a] -> Eff r [ b]
mapMdebug f [] = return []
mapMdebug f (h:t) = do
                      trace $ "mapMdebug: " ++ show h
                      h' <- f h
                      t' <- mapMdebug f t
                      return (h': t')



tMd = runTrace $ runReader (10:: Int) (mapMdebug f [1..5]) 
      where f x = ask `add` return x

--}
