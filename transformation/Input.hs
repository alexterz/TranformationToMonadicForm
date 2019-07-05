map ::   (a -> b) -> [a] -> [b] \n
map f [] = [];
map f (x:xs) = f x : map f xs;

f =  map' \x->x+1  [1,2]
