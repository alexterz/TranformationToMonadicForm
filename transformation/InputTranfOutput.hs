{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}

import InputMConvert 
import Control.Eff
import Control.Monad
import Control.Eff.State.Strict

foldrK :: (Eff r ((a-> (Eff r (b-> (Eff r b )) ))-> (Eff r (b-> (Eff r ([a]-> (Eff r b )) )) )) )
foldrK = (return (let foldrK' f z [] = (return z);foldrK' f z (x : xs) = (((return xs)>>=( \ x1  -> (((return z)>>=( \ x2  -> (((return f)>>=( \ x3  -> (foldrK>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))))>>=( \ x0  -> (((return x)>>=( \ x1  -> ((return f)>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));;in (mConvert2 foldrK')));
mapK :: (Eff r ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) )) )
mapK = (return (let mapK' f [] = (sequence []);mapK' f (x : xs) = (((return xs)>>=( \ x2  -> (((return f)>>=( \ x3  -> (mapK>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ x1  -> ((((return x)>>=( \ x3  -> ((return f)>>=( \ g3  -> (g3 x3)))))>>=( \ x2  -> (cons>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))));;in (mConvert1 mapK')));
plus1 :: (Eff r (Integer-> (Eff r Integer )) )
plus1 = (return (let plus1' x = ((return 1)>>=( \ x1  -> (((return x)>>=( \ x2  -> (plus>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))));;in plus1'));
plus2 :: (Eff r (Integer-> (Eff r (Integer-> (Eff r Integer )) )) )
plus2 = (return (let plus2' x y = ((return y)>>=( \ x1  -> (((return x)>>=( \ x2  -> (plus>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))));;in (mConvert1 plus2')));
alex :: (Eff r (Integer-> (Eff r Integer )) )
alex = (return (let alex' x = ((return 1)>>=( \ x1  -> (((return x)>>=( \ x2  -> (sub>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))));;in alex'));
intermediate :: (Eff r ([Integer]-> (Eff r [Integer] )) )
intermediate = (let intermediate' = (alex>>=( \ x0  -> (maplet>>=( \ g0  -> (g0 x0)))));;in intermediate');
inter :: (Eff r ((Integer-> (Eff r (Integer-> (Eff r Integer )) ))-> (Eff r (Integer-> (Eff r ([Integer]-> (Eff r Integer )) )) )) )
inter = (return (let inter' f = ((return f)>>=( \ x0  -> (foldrK>>=( \ g0  -> (g0 x0)))));;in inter'));
one ::  (Member (State s) r, Num s) =>(Eff r Integer )
one = (let one' = ((return 0)>>=( \ x0  -> ((return return)>>=( \ g0  -> (g0 x0)))));;in one');
maplet :: forall r a b .(Eff r ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) )) )
maplet = (return (let maplet' f [] = (sequence []);maplet' f (x : xs) = (let h :: (Eff r b );h = (let h' = ((return x)>>=( \ x0  -> ((return f)>>=( \ g0  -> (g0 x0)))));;in h');t :: (Eff r [b] );t = (let t' = ((return xs)>>=( \ x0  -> (((return f)>>=( \ x1  -> (maplet>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));;in t');z :: (Eff r Integer );z = (let z' = ((return 1)>>=( \ x0  -> (alex>>=( \ g0  -> (g0 x0)))));;in z');in (t>>=( \ x2  -> ((h>>=( \ x3  -> (cons>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2))))));;in (mConvert1 maplet')));
result :: (Eff r [Integer] )
result = (let result' = ((((sequence [(return 1),(return 2)])>>=( \ x4  -> (((return 3)>>=( \ x5  -> (cons>>=( \ g5  -> (g5 x5)))))>>=( \ g4  -> (g4 x4)))))>>=( \ x2  -> (((return 2)>>=( \ x3  -> (cons>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ x0  -> (intermediate>>=( \ g0  -> (g0 x0)))));;in result');

main::IO ()
main= putStrLn $show $run $result