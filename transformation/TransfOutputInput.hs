map :: ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) ))
map f [] = [];
map f (x : xs) = ((f x):((map f) xs));
f = ((map' ( \ x  -> (x+1))) [1,2]);
