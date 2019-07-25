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

sub1::Integer->State Integer Integer;
sub1 x = return (x-1)\n

intermediate:: [Integer]->[Integer];
intermediate = maplet alex\n

inter:: (Integer->Integer->Integer)-> (Integer->[Integer]->Integer);
inter f =foldrK f\n



alex':: Integer -> State Integer Integer -> Integer;
alex' x y = 1+x \n

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


mid::(Integer,Integer);
mid = runState (example plusMonads 1 (return 2)) 7  \n


example::Monad m => (m Integer -> m Integer -> m Integer)->  Integer -> m Integer -> m Integer;
example f a b = plusMonads (return a)  b\n

plusMonads::Monad m => m Integer -> m Integer -> m Integer;
plusMonads x y = x>>=(\z->(y>>=\v->(return(v+z)))) \n


addState::State Integer Integer -> State Integer Integer ;
addState x = x>>= f \n 

f:: Integer -> State Integer Integer ; 
f p = get >>= (g p) \n

g:: Integer -> Integer -> State Integer Integer ;
g a s = return (a+s) \n

addState':: State Integer Integer -> State Integer Integer ;
addState' x = (x>>=(\q-> get>>= (\y-> (put (y+1))>>=(\z-> return (y+q))))) \n 


createStMonad:: Integer -> (Integer , Integer) ;
createStMonad s = (1 , s) \n

sum1:: State Integer Integer;
sum1 = (addState' (return 1))\n    

tryBind:: Integer ->Integer -> State Integer Integer;
tryBind x y = return (x+y) >>= \z -> return z\n

result:: (Integer, Integer);
result = runState (addState' (tryBind 4 5)) 2


