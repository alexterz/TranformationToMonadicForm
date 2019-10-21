{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-#LANGUAGE FlexibleContexts, TypeOperators, DataKinds #-}

import Control.Monad.State.Lazy
import Control.Monad.Except

checkNum :: (Integer-> (Except String Integer ))
checkNum num = (case num of 0->(throwError "The list contains at least one zero element"); x->x; );
checkSum :: (Integer-> (Except String Integer ))
checkSum sum = (case sum of 10->(throwError "Total sum is exactly ten"); x->x; );
map' :: ((a-> b)-> ([a]-> [b]))
map' f [] = [];map' f (x : xs) = ((f x):((map' f) xs));
ok :: ((State Integer a )-> ((Except e a )-> (m a )))
ok ms me = (return 1);
res2 ::  (Monad m) =>(m Integer )
res2 = (fst ((runState (checkSum (sumOfAList ((map' checkNum) [0,5,4])))) 0));
result :: (Either String Integer )
result = (runExcept res2);
sumOfAList :: ([Integer]-> (State Integer Integer ))
sumOfAList [] = get;sumOfAList (x : xs) = ((put (x+get))>>=( \ z  -> (sumOfAList xs)));

main::IO ()
main= putStrLn $show $result