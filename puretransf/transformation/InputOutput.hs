trial :: ((Integer-> Integer)-> (Integer-> Integer))
trial f 0 = 0;trial f x = ((trial f) ((f x)-1));
ex :: Integer
ex = ((trial alex) 2);
func :: ((Integer-> Integer)-> (Integer-> Integer))
func f x = (f x);
alex :: (Integer-> Integer)
alex x = (x-1);
result :: Integer
result = ((func alex) 1);

main::IO ()
main= putStrLn $show $result