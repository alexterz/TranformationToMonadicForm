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






maplet:: (a-> b) -> [a]->  [b]
maplet f [] = []
maplet f (x:xs) =
  let
    h= f x
    t = maplet f xs 
  in
    h:t

mapletK::  (Eff r ( (a->Eff r b) -> Eff r ([a]-> Eff r [b])))
mapletK = 
  let
     mapletK':: (a->Eff r b)-> [a]->Eff r [b]
     mapletK' f [] = return []
     mapletK' f (x:xs) = 
      let
         h = f x
         t = (mapletK >>= (\g-> (g f)))>>=(\g1-> (g1 xs))
      in 
         h>>=(\h1->(t>>=(\t1-> return (h1:t1))))  
  in 
    return(mConvert1 mapletK')   


cons:: Eff r (a-> Eff r ([a]->Eff r [a]))
cons =
  let
     cons' x xs = return (x:xs)
  in 
     return (mConvert1 cons')   

plus:: forall a b r.(Num a ) => Eff r (a->Eff r (a-> Eff r a))
plus = 
  let
     plus x y = return (x+y)
  in
     return (mConvert1 plus)   

mapE:: Eff r (Eff r (a->Eff r b) -> Eff r ([a]-> Eff r [b]))
mapE = 
  let
    -- mapE':: (Eff r (a->Eff r b)) -> ([a]-> Eff r [b])
     mapE f [] = return []
     mapE f (h:t) = f>>= (\f'-> ((f' h) >>= (\h' -> (mapE f t >>= \t' -> return (h':t')))))
  in
    return (mConvert1 mapE)



mapA:: Eff r ((a-> Eff r b) -> Eff r ([a] -> Eff r [b]))
mapA = 
  let
    mapA f [] = return []
    mapA f (h:t) = (f h) >>= (\h' -> (mapA f t >>= \t' -> return (h':t') ))
  in
    return (mConvert1 mapA)  

mapK f [] = []
mapK f (x:xs) = f x : mapK f xs 


--map'':: (a-> Eff r b) -> [a] -> Eff r [b]
map'' f [] = return []
map'' f (h:t) = (f h) >>= (\h' -> (map'' f t >>= \t' -> return (h':t') ))


map':: (a-> Eff r b) -> [a] -> Eff r [b]
map' f [] = return []
map' f (h:t) = do
                 h'<- f h 
                 t'<- map' f t
                 return (h':t')

--mapNew:: (a-> Eff r b) -> Eff r ([a] -> Eff r [b])
mapEff:: (a-> Eff r b) -> Eff r ([a] -> Eff r [b])
mapEff = mConvert1 map''


--examples with map'

t1 = run $ runReader (10::Int) (map'' f [1..5])
     where f x = ask `add` return x
--[11,12,13,14,15]

-- totalAdd imported from ForTesting, i sthe function that giving a x, returns the (x + Env + (s+1)), and increases the state 

t2 = run $ runState (5::Int) $ runReader (10::Int) $ map''  totalAdd [1..5]
--([17,19,21,23,25],10)

--examples with mapNew

--t3 :: ([Int] -> Eff' [Reader Int, State Int] [Int], Int)

t3 = fst $ run $ runState (5::Int) $ runReader (10::Int) $ mapEff  totalAdd 

t4 = run((mapA >>= (\f -> f addOne)) >>= \g -> g [1,2]) 
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
