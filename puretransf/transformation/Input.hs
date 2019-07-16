foldrK            :: (a -> b -> b) -> b -> [a] -> b;
foldrK f z []     =  z;
foldrK f z (x:xs) =  f x (foldrK f z xs)\n


mapK:: (a->b)-> [a]->[b];
mapK f [] = [];
mapK f (x:xs) = f x : mapK f xs \n


plus1:: Integer->Integer;
plus1 x= x+1\n

plus2::Integer->Integer->Integer;
plus2 x y = x+y\n

alex::Integer->Integer;
alex x = x-1\n

maplet:: forall a b .((a-> b) -> [a]->  [b]);
maplet f [] = [];
maplet f (x:xs) =
  let
    h:: b;
    h= f x\n
    t:: [b];
    t = maplet f xs  
  in
    h:t \n

result:: [Integer];
result = maplet alex (2:3:[1,2]) 



