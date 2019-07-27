{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}

import InputMConvert 
import Control.Eff
import Control.Monad
import Control.Eff.State.Lazy
import Control.Eff.Exception
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
m1 ::  (Member (State Integer) r) =>(Eff r Integer )
m1 = (let m1' = ((return 1)>>=( \ x0  -> ((return return)>>=( \ g0  -> (g0 x0)))));;in m1');
one :: (Eff r Integer )
one = (let one' = (((return 5)>>=( \ x1  -> (alex'>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 m1)));;in one');
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
plusMonads :: (Eff r ((Eff r Integer )-> (Eff r ((Eff r Integer )-> (Eff r Integer )) )) )
plusMonads = (return (let plusMonads' x y = (x>>=( \ z  -> (y>>=( \ v  -> ((return v)>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2)))))))));;in (mConvert1 plusMonads')));
mid :: (Eff r (Integer,Integer) )
mid = (let mid' = ((return 7)>>=( \ s0  -> ((runState s0) (((return 1)>>=( \ x2  -> ((plusMonads>>=( \ x3  -> (example>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 ((return 2)>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2)))))))))));;in mid');
example :: (Eff r (((Eff r Integer )-> (Eff r ((Eff r Integer )-> (Eff r Integer )) ))-> (Eff r (Integer-> (Eff r ((Eff r Integer )-> (Eff r Integer )) )) )) )
example = (return (let example' f a b = ((plusMonads>>=( \ g1  -> (g1 ((return a)>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2))))))))>>=( \ g0  -> (g0 b)));;in (mConvert2 example')));
addState ::  (Member (State Integer) r) =>(Eff r ((Eff r Integer )-> (Eff r Integer )) )
addState = (return (let addState' x = (f>>=( \ f0  -> (x>>=f0)));;in addState'));
f ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r Integer )) )
f = (return (let f' p = (((return p)>>=( \ x1  -> (g>>=( \ g1  -> (g1 x1)))))>>=( \ f0  -> (get>>=f0)));;in f'));
g ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r (Integer-> (Eff r Integer )) )) )
g = (return (let g' a s = (((return s)>>=( \ x2  -> (((return a)>>=( \ x3  -> (plus>>=( \ g3  -> (g3 x3)))))>>=( \ g2  -> (g2 x2)))))>>=( \ x0  -> ((return return)>>=( \ g0  -> (g0 x0)))));;in (mConvert1 g')));
addState' ::  (Member (State Integer) r) =>(Eff r ((Eff r Integer )-> (Eff r Integer )) )
addState' = (return (let addState'' x = (x>>=( \ q  -> (get>>=( \ y  -> ((((return 1)>>=( \ x5  -> (((return y)>>=( \ x6  -> (plus>>=( \ g6  -> (g6 x6)))))>>=( \ g5  -> (g5 x5)))))>>=( \ s3  -> (put s3)))>>=( \ z  -> (((return q)>>=( \ x5  -> (((return y)>>=( \ x6  -> (plus>>=( \ g6  -> (g6 x6)))))>>=( \ g5  -> (g5 x5)))))>>=( \ x3  -> ((return return)>>=( \ g3  -> (g3 x3)))))))))));;in addState''));
createStMonad :: (Eff r (Integer-> (Eff r (Integer,Integer) )) )
createStMonad = (return (let createStMonad' s = (return (1,s));;in createStMonad'));
sum1 ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r Integer )) )
sum1 = (return (let sum1' x = (addState'>>=( \ g0  -> (g0 ((return x)>>=( \ x1  -> ((return return)>>=( \ g1  -> (g1 x1))))))));;in sum1'));
tryBind ::  (Member (State Integer) r) =>(Eff r (Integer-> (Eff r (Integer-> (Eff r Integer )) )) )
tryBind = (return (let tryBind' x y = ((((return y)>>=( \ x3  -> (((return x)>>=( \ x4  -> (plus>>=( \ g4  -> (g4 x4)))))>>=( \ g3  -> (g3 x3)))))>>=( \ x1  -> ((return return)>>=( \ g1  -> (g1 x1)))))>>=( \ z  -> ((return z)>>=( \ x1  -> (((return 1)>>=( \ x2  -> (g>>=( \ g2  -> (g2 x2)))))>>=( \ g1  -> (g1 x1)))))));;in (mConvert1 tryBind')));
tryExc ::  (Member (Exc String) r) =>(Eff r (Integer-> (Eff r Integer )) )
tryExc = (return (let tryExc' x = ((return x)>>=( \ x0  -> ((return return)>>=( \ g0  -> (g0 x0)))));;in tryExc'));
tryBoth ::  (Member (Exc String) r, Member (State Integer) r) =>(Eff r ((Eff r Integer )-> (Eff r ((Eff r Integer )-> (Eff r Integer )) )) )
tryBoth = (return (let tryBoth' exc st = (exc>>=( \ x  -> (st>>=( \ y  -> (((return y)>>=( \ x4  -> (((return x)>>=( \ x5  -> (plus>>=( \ g5  -> (g5 x5)))))>>=( \ g4  -> (g4 x4)))))>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2)))))))));;in (mConvert1 tryBoth')));
tryRunExc :: (Eff r (Either String Integer ) )
tryRunExc = (let tryRunExc' = (runError ((return 1)>>=( \ x1  -> (tryExc>>=( \ g1  -> (g1 x1))))));;in tryRunExc');
result :: (Eff r Integer )
result = (let result' = ((tryBoth>>=( \ g1  -> (g1 ((return 1)>>=( \ x2  -> ((return return)>>=( \ g2  -> (g2 x2))))))))>>=( \ g0  -> (g0 ((return 2)>>=( \ x1  -> ((return return)>>=( \ g1  -> (g1 x1))))))));;in result');

main::IO ()
main= putStrLn $show $run $result