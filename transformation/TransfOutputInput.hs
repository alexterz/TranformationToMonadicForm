a = (return (5+3));
map :: ((a-> (Eff r b ))-> (Eff r ([a]-> (Eff r [b] )) ))
map f [] = (return []);
map f (x : xs) = ((f x)>>=( \ h0  -> (((map f) xs)>>=( \ t0  -> (return (h0:t0))))));
map f (x : xs) = ((f x)>>=( \ h0  -> ((return [1,2])>>=( \ t0  -> (return (h0:t0))))));
map f (x : y : xs) = ((f x)>>=( \ h0  -> (((f y)>>=( \ h1  -> (((map f) xs)>>=( \ t1  -> (return (h1:t1))))))>>=( \ t0  -> (return (h0:t0))))));
foldr'' :: ((a-> (Eff r (b-> (Eff r b )) ))-> (Eff r (b-> (Eff r ([a]-> (Eff r b )) )) ))
foldr'' f z [] = (return z);
foldr'' f z (x : xs) = ((((foldr'' f) z) xs)>>=( \ x0  -> (((return x)>>=( \ x1  -> ((return f)>>=( \ g1  -> (g1 x1)))))>>=( \ g0  -> (g0 x0)))));
mapM' f (x : xs) = ((f x)>>=( \ h  -> (((mapM' f) xs)>>=( \ t  -> (let in ((f 1)>>=( \ h0  -> (((return h)>>=( \ h1  -> ((return t)>>=( \ t1  -> (return (h1:t1))))))>>=( \ t0  -> (return (h0:t0)))))))))));
