{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-#LANGUAGE FlexibleContexts, TypeOperators, DataKinds #-}

import InMConvert 
import Control.Eff
import Control.Monad
import Control.Eff.State.Lazy
import Control.Eff.Exception
import Data.Tuple.Sequence

plusOne :: (Eff r (Integer-> (Eff r Integer)))
plusOne = (return (let plusOne' x = ((return 1)>>=( \ x1  -> (((return x)>>=( \ x2  -> (plus>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))));;in plusOne'));
result :: (Eff r Integer)
result = (let result' = ((return 2)>>=( \ x0  -> (plusOne>>=( \ g0  -> (g0 x0)))));;in result');

main::IO ()
main= putStrLn $show $run $result