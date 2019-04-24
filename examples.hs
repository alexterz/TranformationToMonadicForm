

--isws provlima an t teleutaio argument einai literal, qvar, gcon 


add':: (Monad m, Num t1, Num t2) => (t1 -> m (t2 -> m (m b))) -> m b

add' add = add 5 >>= \g2 -> g2 6 >>= \g1 -> g1 >>= \h -> return h




add'' :: (Monad m, Num t1, Num t2) => (t1 -> m (t2 -> m b)) -> m b
add'' add = add 5 >>= \g2 -> g2 6 >>= \h -> return h


-- προβλημα αν expr = literal

--t:: Monad m => m Int
--t = 5 >>=  \h -> return h 
