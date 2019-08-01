{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-#LANGUAGE FlexibleContexts, TypeOperators, DataKinds #-}

import InputMConvert 
import Control.Eff
import Control.Monad
import Control.Eff.State.Lazy
import Control.Eff.Exception
import Data.Tuple.Sequence

addStateToValue ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r Integer)))
addStateToValue = (return (let addStateToValue' a = (get>>=( \ s  -> (((return s)>>=( \ x3  -> (((return a)>>=( \ x4  -> (sub>>=( \ g4  -> (g4 x4)))))>>=( \ g3  -> (g3 x3)))))>>=( \ x1  -> ((return return)>>=( \ g1  -> (g1 x1)))))));;in addStateToValue'));
divExc ::  (Member (Exc String) r) =>(Eff r (Integer-> (Eff r (Integer-> (Eff r Integer)))))
divExc = (return (let divExc' i j = ((return j)>>=( \ c0  -> (case c0 of 0->((return "Divide with zero")>>=( \ e2  -> (throwError e2))); x->(((return x)>>=( \ x4  -> (((return i)>>=( \ x5  -> (division>>=( \ g5  -> (g5 x5)))))>>=( \ g4  -> (g4 x4)))))>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2))))); )));;in (mConvert1 divExc')));
res ::  (Member (Exc String) r) =>(Eff r Integer)
res = (let res' = (((return 1)>>=( \ s1  -> ((runState s1) ((return 1)>>=( \ x2  -> (((return 5)>>=( \ x3  -> (totalTest>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2))))))))>>=( \ t0  -> (return (fst t0))));;in res');
result :: (Eff r (Either String Integer ))
result = (let result' = (runError res);;in result');
totalTest ::  (Member (State Integer) r, Member (Exc String) r) =>(Eff r (Integer-> (Eff r (Integer-> (Eff r Integer)))))
totalTest = (return (let totalTest' i j = (((return j)>>=( \ x1  -> (addStateToValue>>=( \ g1  -> (g1 x1)))))>>=( \ x  -> ((return x)>>=( \ x1  -> (((return i)>>=( \ x2  -> (divExc>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))))));;in (mConvert1 totalTest')));

main::IO ()
main= putStrLn $show $run $result