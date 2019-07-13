import InputMConvert 
import Control.Eff

mapK :: ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) ))
mapK = (let mapK f [] = (return []);mapK f (x : xs) = (((f x))>>=( \ h0  -> ((((mapK f) xs))>>=( \ t0  -> (return (h0:t0))))));in (mConvert1 mapK));
foldrK :: ((a-> (Eff r (b-> (Eff r b )) ))-> (Eff r (b-> (Eff r ([a]-> (Eff r b )) )) ))
foldrK = (let foldrK f z [] = (return z);foldrK f z (x : xs) = (((((foldrK f) z) xs))>>=( \ x0  -> (((((return x))>>=( \ x1  -> (((return f))>>=( \ g1  -> (g1 x1))))))>>=( \ g0  -> (g0 x0)))));in (mConvert2 foldrK));
plus1 :: (Integer-> (Eff r Integer ))
plus1 = (let plus1 x = (return (x+1));in plus1);
id' :: (a-> (Eff r a ))
id' = (let id' a = (return a);in id');
intermed1 :: ([Integer]-> (Eff r [Integer] ))
intermed1 = (let intermed1 [] = (return []);intermed1 (x : xs) = (((id' x))>>=( \ h0  -> (((intermed1 xs))>>=( \ t0  -> (return (h0:t0))))));in intermed1);
monFunc :: (a-> (Eff r (a-> (Eff r a )) ))
monFunc = (let monFunc x = ((return return));in monFunc);
monadic :: (a-> (Eff r a ))
monadic = (let monadic a = ((return a));in monadic);
lit :: Integer
lit = (run (let lit = ((return 1));in lit));
one :: Integer
one = (run (let one = (return 1);in one));
result :: [Integer]
result = (run (let result = (((return [1,2]))>>=( \ x0  -> (((((return plus1))>>=( \ x1  -> (((return mapK))>>=( \ g1  -> (g1 x1))))))>>=( \ g0  -> (g0 x0)))));in result));

main::IO ()
main= putStrLn $show $result