a = 5 + 3;

map :: (a -> b) -> [a] -> [b];
map f [] = [];
map f (x:xs) = f x : map f xs;
map f (x:xs) = f x : [1,2];
map f (x:y:xs) = f x : (f y : map f xs);
foldr'' :: (a -> b -> b) -> b -> [a] -> b;
foldr'' f z []     =  z;
foldr'' f z (x:xs) =  f x (foldr'' f z xs);

mapM' f (x:xs)= let h = f x; t = mapM' f xs in ((f 1):(h:t)) 

