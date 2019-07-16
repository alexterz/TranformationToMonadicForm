foldrK :: ((a-> (b-> b))-> (b-> ([a]-> b)))
foldrK f z [] = z;foldrK f z (x : xs) = ((f x) (((foldrK f) z) xs));
mapK :: ((a-> b)-> ([a]-> [b]))
mapK f [] = [];mapK f (x : xs) = ((f x):((mapK f) xs));
listCons :: (a-> ([a]-> [a]))
listCons x xs = (x:xs);
plus1 :: (Integer-> Integer)
plus1 x = (x+1);
alex :: (Integer-> Integer)
alex x = (x-1);
result :: [Integer]
result = ((mapK plus1) (1:[2,3]));

main::IO ()
main= putStrLn $show $result