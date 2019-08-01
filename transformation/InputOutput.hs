{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-#LANGUAGE FlexibleContexts, TypeOperators, DataKinds #-}

import Control.Monad.State.Lazy
import Control.Monad.Except

addStateToValue :: (Integer-> (State Integer Integer ))
addStateToValue a = (get>>=( \ s  -> (return (a-s))));
divExc :: (Integer-> (Integer-> (Except String Integer )))
divExc i j = (case j of 0->(throwError "Divide with zero"); x->(return (i`div`x)); );
res ::  (Monad m) =>(m Integer )
res = (fst ((runState ((totalTest 5) 1)) 1));
result :: (Either String Integer )
result = (runExcept res);
totalTest ::  (Monad m) =>(Integer-> (Integer-> (m Integer )))
totalTest i j = ((addStateToValue j)>>=( \ x  -> ((divExc i) x)));

main::IO ()
main= putStrLn $show $result