module InputMConvert where 

import Control.Eff

mConvert0 :: a -> Eff r a 
mConvert0 =return 

mConvert1 :: (t -> a) -> (t -> Eff r a) 
mConvert1 f x = return(f x) 

mConvert2:: (t2 -> t1 -> a) -> t2 -> Eff r (t1 -> Eff r a) 
mConvert2 f x = return $ mConvert1 $ f x 

mConvert3 :: (t -> t2 -> t1 -> a) -> t -> Eff r (t2 -> Eff r (t1 -> Eff r a)) 
mConvert3 f x =return $ mConvert2 $ f x 

