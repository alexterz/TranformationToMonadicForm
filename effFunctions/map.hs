{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds, AllowAmbiguousTypes #-}


import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Exception
import Control.Eff.Trace
import Control.Monad
import MConvert



map':: (a-> Eff r b) -> [a] -> Eff r [b]
map' f [] = return []
map' f (h:t) = do
                 h'<- f h 
                 t'<- map' f t
                 return (h':t')

mapNew:: (a-> Eff r b) -> Eff r ([a] -> Eff r [b])
mapNew = mConvert1 map'

--auxiliary functions 

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

incr :: Member (State Int) r => Eff r ()
incr = get >>= put.(+ (1:: Int))

--func:: (Member (State Int) r, Member (Reader Int) r) => Int -> Eff r Int
func x = do 
           incr
           let sum1 = ask `add` return x
           sum1 `add` get 

--examples with map'

t1 = run $ runReader (10::Int) (map' f [1..5])
     where f x = ask `add` return x


t2 = run $ runState (5::Int) $ runReader (10::Int) $ map'  func [1..5]


--examples with mapNew

--t3 :: ([Int] -> Eff' [Reader Int, State Int] [Int], Int)

t3 = fst $ run $ runState (5::Int) $ runReader (10::Int) $ mapNew  func 

t3'=  run $ runState (5::Int) $ runReader (10::Int) $ t3 [1..5]


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
