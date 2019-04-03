module MConvert where

--παίρνει μία συνάρτηση τύπου (t->a) και τη μετατρέπει σε μία συνάρτηση τύπου (t-> m a), 
--αν στην mConvert κάνουμε partial application των arguments, δηλαδή καλέσουμε mConvert f, θα μας δωθεί η f σε monadic form


-- Η mConvert1 διαφέρει απτην return (:: Monad m => a -> m a) στο ότι της δίνουμε μία συνάρτηση και μας την επιστρέφει σε Monadic form, δηλαδή την καλούμε με ένα μόνο όρισμα (partial application), την συνάρτηση που θέλουμε να μετατρέψουμε σε monadic form

--mConvert::Monad m => (Int->Int)-> (Int-> m Int) 
mConvert1 :: Monad m => (t -> a) -> (t -> m a)
mConvert1 f x = return(f x) 

-- (Int->Int->Int)->(Int->m (Int->m1 Int))
--mConvert2:: (Int -> (Int -> Int)) -> (forall m1. Monad m1 => Int-> m1(forall m2. Monad m2 => Int -> m2 Int))
--δίνω μόνο το πρώτο όρισμα της συνάρτησης γιατί αυτό θέλω μόνο να είναι εκτός monad
mConvert2:: (Monad m, Monad m1) => (t2 -> t1 -> a) -> t2 -> m (t1 -> m1 a)
mConvert2 f x = return $ mConvert1 $ f x

--για first ordered function με πολλά arguments όπου υπάρχει curring δηλαδή partial application of a function 
--και ού το κάθε εξής.. άρα τώρα πρέπει να φτιάξω μια αναδρομική συνάρτηση που να καλεί την mConvert(i-1) για τον υπολογισμός της mConvert (i)
mConvert3 :: (Monad m, Monad m1, Monad m2) =>(t -> t2 -> t1 -> a) -> t -> m (t2 -> m1 (t1 -> m2 a))
mConvert3 f x =return $ mConvert2 $ f x
