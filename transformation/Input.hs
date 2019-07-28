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

m1:: State Integer Integer;
m1= return 1\n

one::Integer ;
one = alex' 5 ( m1) \n

sub1::Integer->State Integer Integer;
sub1 x = return (x-1)\n



inter:: (Integer->Integer->Integer)-> (Integer->[Integer]->Integer);
inter f =foldrK f\n



alex':: Integer -> State Integer Integer -> Integer;
alex' x y = 1+x \n




plusMonads::Monad m => m Integer -> m Integer -> m Integer;
plusMonads x y = x>>=(\z->(y>>=\v->(return(v)))) \n

mid::(Integer,Integer);
mid = runState (example plusMonads 1 (return 2)) 7  \n


example::Monad m => (m Integer -> m Integer -> m Integer)->  Integer -> m Integer -> m Integer;
example f a b = plusMonads (return a)  b\n



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

sum1:: Integer-> State Integer Integer;
sum1 x= (addState' (return x))\n    

tryBind:: Integer ->Integer -> State Integer Integer;
tryBind x y = return (x+y) >>= \z -> (g 1 z)\n

tryExc:: Integer -> Except String Integer;
tryExc x = return x \n

tryBoth::Monad m => Except String Integer-> State Integer Integer ->m Integer;
tryBoth exc st = exc>>=\x-> (st>>=\y-> return (x+y))\n

tryRunExc:: Either String Integer;
tryRunExc = runExcept (tryExc 1)\n

res1:: Monad m => m Integer ;
res1 = (fst (runState (tryBoth (return 1) (return 2)) 5))\n


result:: Either String Integer;
result = runExcept res1
