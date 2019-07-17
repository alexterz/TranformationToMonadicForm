{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State.Lazy

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
intermediate :: ([Integer]-> [Integer])
intermediate = (maplet alex);
inter :: ((Integer-> (Integer-> Integer))-> (Integer-> ([Integer]-> Integer)))
inter f = (foldrK f);
one ::  (Num s) =>(State s Integer )
one = (return 0);
maplet :: forall a b .((a-> b)-> ([a]-> [b]))
maplet f [] = [];maplet f (x : xs) = (let h :: b;h = (f x);t :: [b];t = ((maplet f) xs);z :: Integer;z = (alex 1);in (h:t));
result :: [Integer]
result = (intermediate (2:(3:[1,2])));

main::IO ()
main= putStrLn $show $result