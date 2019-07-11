mapK:: (a->b)-> [a]->[b] ;
mapK f [] = [];
mapK f (x:xs) = f x : mapK f xs \n

foldrK :: (a -> b -> b) -> b -> [a] -> b ;
foldrK f z []     =  z ;
foldrK f z (x:xs) =  f x (foldrK f z xs) \n

plus1 :: (Integer->Integer);
plus1 x = x+1 \n

result:: [Integer];
result = mapK plus1 [1,2] 




