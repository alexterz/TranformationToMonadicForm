import InputMConvert 
import Control.Eff

foldrK :: (Eff r ((a-> (Eff r (b-> (Eff r b )) ))-> (Eff r (b-> (Eff r ([a]-> (Eff r b )) )) )) )
foldrK = (return (let foldrK' f z [] = (return z);foldrK' f z (x : xs) = (((return xs)>>=( \ x1  -> (((return z)>>=( \ x2  -> (((return f)>>=( \ x3  -> (foldrK>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))))>>=( \ x0  -> (((return x)>>=( \ x1  -> ((return f)>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));in (mConvert2 foldrK')));
mapK :: (Eff r ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) )) )
mapK = (return (let mapK' f [] = (return []);mapK' f (x : xs) = (((return x)>>=( \ x1  -> ((return f)>>=( \ g1  -> (g1 x1)))))>>=( \ h0  -> (((return xs)>>=( \ x1  -> (((return f)>>=( \ x2  -> (mapK>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))))>>=( \ t0  -> (return (h0:t0))))));in (mConvert1 mapK')));
listCons :: (Eff r (a-> (Eff r ([a]-> (Eff r [a] )) )) )
listCons = (return (let listCons' x xs = ((return x)>>=( \ h0  -> ((return xs)>>=( \ t0  -> (return (h0:t0))))));in (mConvert1 listCons')));
plus1 :: (Eff r (Integer-> (Eff r Integer )) )
plus1 = (return (let plus1' x = (return (x+1));in plus1'));
alex :: (Eff r (Integer-> (Eff r Integer )) )
alex = (return (let alex' x = (return (x-1));in alex'));
result :: (Eff r [Integer] )
result = (let result' = (((return 1)>>=( \ h1  -> ((return [2,3])>>=( \ t1  -> (return (h1:t1))))))>>=( \ x0  -> ((plus1>>=( \ x1  -> (mapK>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));in result');

plus::(Num a)=> Eff r (a->Eff r (a-> Eff r a))
plus = let plus x y = return (x+y) in return (mConvert1 plus)


main::IO ()
main= putStrLn $show $run $result