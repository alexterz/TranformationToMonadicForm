mapK :: ((a-> b)-> ([a]-> [b]))
mapK f [] = [];mapK f (x : xs) = ((f x):((mapK f) xs));
foldrK :: ((a-> (b-> b))-> (b-> ([a]-> b)))
foldrK f z [] = z;foldrK f z (x : xs) = ((f x) (((foldrK f) z) xs));
plus1 :: (Integer-> Integer)
plus1 x = (x+1);
id' :: (a-> a)
id' a = a;
intermed1 :: ([Integer]-> [Integer])
intermed1 [] = [];intermed1 (x : xs) = ((id' x):(intermed1 xs));
monFunc :: (a-> (IO (a-> (IO a )) ))
monFunc x = (return return);
monadic :: (a-> (IO a ))
monadic a = (return a);
lit :: (IO Integer )
lit = (return 1);
one :: Integer
one = 1;
result :: [Integer]
result = ((mapK plus1) [1,2]);

main::IO ()
main= putStrLn $show $result