import InMConvert 
import Control.Eff

mapK :: ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) ))
mapK = (let mapK f [] = (return []);mapK f (x : xs) = ((f x)>>=( \ h0  -> (((mapK f) xs)>>=( \ t0  -> (return (h0:t0))))));in (mConvert1 mapK));

main::IO ()
main= putStrLn $show $result