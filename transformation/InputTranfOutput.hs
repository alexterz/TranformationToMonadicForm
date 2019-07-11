import InputMConvert 
import Control.Eff

mapK :: ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) ))
mapK = (let mapK f [] = (return []);mapK f (x : xs) = ((f x)>>=( \ h0  -> (((mapK f) xs)>>=( \ t0  -> (return (h0:t0))))));in (mConvert1 mapK));
foldrK :: ((a-> (Eff r (b-> (Eff r b )) ))-> (Eff r (b-> (Eff r ([a]-> (Eff r b )) )) ))
foldrK = (let foldrK f z [] = (return z);foldrK f z (x : xs) = ((((foldrK f) z) xs)>>=( \ x0  -> (((return x)>>=( \ x1  -> ((return f)>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));in (mConvert2 foldrK));
plus1 :: (Integer-> (Eff r Integer ))
plus1 = (let plus1 x = (return (x+1));in plus1);
result :: [Integer]
result = (run (let result = ((return [1,2])>>=( \ x0  -> (((return plus1)>>=( \ x1  -> ((return mapK)>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));in result));

main::IO ()
main= putStrLn $show $result