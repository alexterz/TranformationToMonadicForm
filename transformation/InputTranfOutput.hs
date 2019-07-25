{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import InputMConvert 
import Control.Eff
import Control.Monad
import Control.Eff.State.Strict
import Data.Tuple.Sequence

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
sub1 ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r Integer )) )
sub1 = (return (let sub1' x = (((return 1)>>=( \ x2  -> (((return x)>>=( \ x3  -> (sub>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ x0  -> ((return return)>>=( \ g0  -> (g0 x0)))));;in sub1'));
intermediate :: (Eff r ([Integer]-> (Eff r [Integer] )) )
intermediate = (let intermediate' = (alex>>=( \ x0  -> (maplet>>=( \ g0  -> (g0 x0)))));;in intermediate');
inter :: (Eff r ((Integer-> (Eff r (Integer-> (Eff r Integer )) ))-> (Eff r (Integer-> (Eff r ([Integer]-> (Eff r Integer )) )) )) )
inter = (return (let inter' f = ((return f)>>=( \ x0  -> (foldrK>>=( \ g0  -> (g0 x0)))));;in inter'));
alex' ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r ((Eff r Integer )-> (Eff r Integer )) )) )
alex' = (return (let alex'' x y = ((return x)>>=( \ x1  -> (((return 1)>>=( \ x2  -> (plus>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))));;in (mConvert1 alex'')));
maplet :: forall r a b .(Eff r ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) )) )
maplet = (return (let maplet' f [] = (sequence []);maplet' f (x : xs) = (let h :: (Eff r b );h = (let h' = ((return x)>>=( \ x0  -> ((return f)>>=( \ g0  -> (g0 x0)))));;in h');t :: (Eff r [b] );t = (let t' = ((return xs)>>=( \ x0  -> (((return f)>>=( \ x1  -> (maplet>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));;in t');z :: (Eff r Integer );z = (let z' = ((return 1)>>=( \ x0  -> (alex>>=( \ g0  -> (g0 x0)))));;in z');in (t>>=( \ x2  -> ((h>>=( \ x3  -> (cons>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2))))));;in (mConvert1 maplet')));
mid :: (Eff r Integer )
mid = (let mid' = (((plusMonads>>=( \ x2  -> (example>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 ((return 1)>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2))))))))>>=( \ g0  -> (g0 ((return 2)>>=( \ x1  -> ((return return)>>=( \ g1  -> (g1 x1))))))));;in mid');
example :: (Eff r (((Eff r Integer )-> (Eff r ((Eff r Integer )-> (Eff r Integer )) ))-> (Eff r ((Eff r Integer )-> (Eff r ((Eff r Integer )-> (Eff r Integer )) )) )) )
example = (return (let example' f a b = ((plusMonads>>=( \ g1  -> (g1 a)))>>=( \ g0  -> (g0 b)));;in (mConvert2 example')));
plusMonads :: (Eff r ((Eff r Integer )-> (Eff r ((Eff r Integer )-> (Eff r Integer )) )) )
plusMonads = (return (let plusMonads' x y = (x>>=( \ z  -> (y>>=( \ v  -> (((return z)>>=( \ x4  -> (((return v)>>=( \ x5  -> (plus>>=( \ g5  -> (g5 x5)))))>>=( \ g4  -> (g4 x4)))))>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2)))))))));;in (mConvert1 plusMonads')));
addState ::  (Member (State Integer) r) =>(Eff r ((Eff r Integer )-> (Eff r Integer )) )
addState = (return (let addState' x = (f>>=( \ f0  -> (x>>=f0)));;in addState'));
f ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r Integer )) )
f = (return (let f' p = (((return p)>>=( \ x1  -> (g>>=( \ g1  -> (g1 x1)))))>>=( \ f0  -> (get>>=f0)));;in f'));
g ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r (Integer-> (Eff r Integer )) )) )
g = (return (let g' a s = (((return s)>>=( \ x2  -> (((return a)>>=( \ x3  -> (plus>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ x0  -> ((return return)>>=( \ g0  -> (g0 x0)))));;in (mConvert1 g')));
createStMonad :: (Eff r (Integer-> (Eff r (Integer,Integer) )) )
createStMonad = (return (let createStMonad' s = (return (1,s));;in createStMonad'));
sum1 ::  (Member (State Integer) r) =>(Eff r Integer )
sum1 = (let sum1' = (addState>>=( \ g0  -> (g0 ((return 1)>>=( \ x1  -> ((return return)>>=( \ g1  -> (g1 x1))))))));;in sum1');
result :: (Eff r (Integer,Integer) )
result = (let result' = ((return 5)>>=( \ s0  -> ((runState s0) sum1)));;in result');

main::IO ()
main= putStrLn $show $run $result