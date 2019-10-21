{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-#LANGUAGE FlexibleContexts, TypeOperators, DataKinds #-}

import InputMConvert 
import Control.Eff
import Control.Monad
import Control.Eff.State.Lazy
import Control.Eff.Exception
import Data.Tuple.Sequence

checkNum ::  (Member (Exc String) r) =>(Eff r (Integer-> (Eff r Integer)))
checkNum = (return (let checkNum' num = ((return num)>>=( \ c0  -> (case c0 of 0->((return "The list contains at least one zero element")>>=( \ e2  -> (throwError e2))); x->(return x); )));;in checkNum'));
checkSum ::  (Member (Exc String) r) =>(Eff r (Integer-> (Eff r Integer)))
checkSum = (return (let checkSum' sum = ((return sum)>>=( \ c0  -> (case c0 of 10->((return "Total sum is exactly ten")>>=( \ e2  -> (throwError e2))); x->(return x); )));;in checkSum'));
map' :: (Eff r ((a-> (Eff r b))-> (Eff r ([a]-> (Eff r [b])))))
map' = (return (let map'' f [] = (sequence []);map'' f (x : xs) = (((return xs)>>=( \ x2  -> (((return f)>>=( \ x3  -> (map'>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ x1  -> ((((return x)>>=( \ x3  -> ((return f)>>=( \ g3  -> (g3 x3)))))>>=( \ x2  -> (cons>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))));;in (mConvert1 map'')));
res2 ::  (Member (Exc String) r) =>(Eff r Integer)
res2 = (let res2' = (((return 0)>>=( \ s1  -> ((runState s1) ((((sequence [(return 0),(return 5),(return 4)])>>=( \ x4  -> ((checkNum>>=( \ x5  -> (map'>>=( \ g5  -> (g5 x5)))))>>=( \ g4  -> (g4 x4)))))>>=( \ x3  -> (sumOfAList>>=( \ g3  -> (g3 x3)))))>>=( \ x2  -> (checkSum>>=( \ g2  -> (g2 x2))))))))>>=( \ t0  -> (return (fst t0))));;in res2');
result :: (Eff r (Either String Integer ))
result = (let result' = (runError res2);;in result');
sumOfAList ::  (Member (State Integer) r) =>(Eff r ([Integer]-> (Eff r Integer)))
sumOfAList = (return (let sumOfAList' [] = get;sumOfAList' (x : xs) = (((get>>=( \ x3  -> (((return x)>>=( \ x4  -> (plus>>=( \ g4  -> (g4 x4)))))>>=( \ g3  -> (g3 x3)))))>>=( \ s1  -> (put s1)))>>=( \ z  -> ((return xs)>>=( \ x1  -> (sumOfAList>>=( \ g1  -> (g1 x1)))))));;in sumOfAList'));

main::IO ()
main= putStrLn $show $run $result