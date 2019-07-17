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

intermediate:: [Integer]->[Integer];
intermediate = maplet alex\n

inter:: (Integer->Integer->Integer)-> (Integer->[Integer]->Integer);
inter f =foldrK f\n

one::Monad m=> m Integer -> m Integer;
one x = x\n

maplet:: forall a b .((a-> b) -> [a]->  [b]);
maplet f [] = [];
maplet f (x:xs) =
  let
    h:: b;
    h= f x\n
    t:: [b];
    t = maplet f xs\n
    z:: Integer;
    z= alex 1
  in
    h:t \n

result:: [Integer];
result = intermediate (2:3:[1,2]) 



