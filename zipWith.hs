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


--zipWithEff::(Monad m1, Monad m)=> (a->Eff r ( b-> Eff r c)) -> m ([a] -> m1 ([b] -> Eff r [c]))
--zipWithEff:: (a->Eff r ( b-> Eff r c)) -> Eff r ([a] -> Eff r ([b] -> Eff r [c]))
zipWithEff = mConvert2 zipWith'
   
