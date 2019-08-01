module InputMConvert where 

import Control.Eff
import Control.Monad
import Control.Eff.State.Lazy
import Control.Eff.Exception
import Data.Tuple.Sequence

mConvert0 :: a -> Eff r a 
mConvert0 =return 

mConvert1 :: (t -> a) -> (t -> Eff r a) 
mConvert1 f x = return(f x) 

mConvert2:: (t2 -> t1 -> a) -> t2 -> Eff r (t1 -> Eff r a) 
mConvert2 f x = return $ mConvert1 $ f x 

mConvert3 :: (t -> t2 -> t1 -> a) -> t -> Eff r (t2 -> Eff r (t1 -> Eff r a)) 
mConvert3 f x =return $ mConvert2 $ f x 


plus::(Num a)=> Eff r (a->Eff r (a-> Eff r a))
plus = let plus' x y = return (x+y) in return (mConvert1 plus')


sub::(Num a)=> Eff r (a->Eff r (a-> Eff r a))
sub = let sub' x y = return (x-y) in return (mConvert1 sub')


multiple::(Num a)=> Eff r (a->Eff r (a-> Eff r a))
multiple = let multiple' x y = return (x*y) in return (mConvert1 multiple')


division:: (Integral a, Num a) => Eff r (a->Eff r (a-> Eff r a))
division = let division' x y = return (x `div` y) in return (mConvert1 division')

cons:: Eff r (a-> Eff r ([a]->Eff r [a]))
cons = let cons' x xs = return (x:xs) in return (mConvert1 cons')

