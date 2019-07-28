{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State.Lazy
import Control.Monad.Except

foldrK :: ((a-> (b-> b))-> (b-> ([a]-> b)))
foldrK f z [] = z;foldrK f z (x : xs) = ((f x) (((foldrK f) z) xs));
mapK :: ((a-> b)-> ([a]-> [b]))
mapK f [] = [];mapK f (x : xs) = ((f x):((mapK f) xs));
plus1 :: (Integer-> Integer)
plus1 x = (x+1);
plus2 :: (Integer-> (Integer-> Integer))
plus2 x y = (x+y);
alex :: (Integer-> Integer)
alex x = (x-1);
m1 :: (State Integer Integer )
m1 = (return 1);
one :: Integer
one = ((alex' 5) m1);
sub1 :: (Integer-> (State Integer Integer ))
sub1 x = (return (x-1));
inter :: ((Integer-> (Integer-> Integer))-> (Integer-> ([Integer]-> Integer)))
inter f = (foldrK f);
alex' :: (Integer-> ((State Integer Integer )-> Integer))
alex' x y = (1+x);
plusMonads ::  (Monad m) =>((m Integer )-> ((m Integer )-> (m Integer )))
plusMonads x y = (x>>=( \ z  -> (y>>=( \ v  -> (return v)))));
mid :: (Integer,Integer)
mid = ((runState (((example plusMonads) 1) (return 2))) 7);
example ::  (Monad m) =>(((m Integer )-> ((m Integer )-> (m Integer )))-> (Integer-> ((m Integer )-> (m Integer ))))
example f a b = ((plusMonads (return a)) b);
addState :: ((State Integer Integer )-> (State Integer Integer ))
addState x = (x>>=f);
f :: (Integer-> (State Integer Integer ))
f p = (get>>=(g p));
g :: (Integer-> (Integer-> (State Integer Integer )))
g a s = (return (a+s));
addState' :: ((State Integer Integer )-> (State Integer Integer ))
addState' x = (x>>=( \ q  -> (get>>=( \ y  -> ((put (y+1))>>=( \ z  -> (return (y+q))))))));
createStMonad :: (Integer-> (Integer,Integer))
createStMonad s = (1,s);
sum1 :: (Integer-> (State Integer Integer ))
sum1 x = (addState' (return x));
tryBind :: (Integer-> (Integer-> (State Integer Integer )))
tryBind x y = ((return (x+y))>>=( \ z  -> ((g 1) z)));
tryExc :: (Integer-> (Except String Integer ))
tryExc x = (return x);
tryBoth ::  (Monad m) =>((Except String Integer )-> ((State Integer Integer )-> (m Integer )))
tryBoth exc st = (exc>>=( \ x  -> (st>>=( \ y  -> (return (x+y))))));
tryRunExc :: (Either String Integer )
tryRunExc = (runExcept (tryExc 1));
result :: Integer
result = ((tryBoth (return 1)) (return 2));

main::IO ()
main= putStrLn $show $result