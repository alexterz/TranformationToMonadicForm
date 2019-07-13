
mapK:: (a->b)-> [a]->[b] ;
mapK f [] = [];
mapK f (x:xs) = f x : mapK f xs \n

foldrK :: (a -> b -> b) -> b -> [a] -> b ;
foldrK f z []     =  z ;
foldrK f z (x:xs) =  f x (foldrK f z xs) \n

plus1 :: (Integer->Integer);
plus1 x = x+1 \n

id':: a -> a;
id' a = a \n

intermed1:: [Integer] -> [Integer];
intermed1 [] = [];
intermed1 (x:xs) = (id' x):(intermed1 xs)\n



monFunc:: a-> IO (a -> IO a);
monFunc x = return (return ) \n

monadic:: a-> IO a; 
monadic a = return a \n

lit:: IO Integer;
lit = return 1 \n



one:: Integer;
one = 1 \n

result:: [Integer];
result = mapK plus1 [1,2] 




