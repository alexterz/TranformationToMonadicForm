trial:: (Integer->Integer)-> Integer->Integer;
trial f 0 = 0;
trial f x = trial f ((f x)-1)  \n

ex::Integer;
ex = trial alex 2\n


func:: (Integer->Integer)->Integer->Integer;
func f x = f x\n
alex::Integer->Integer;
alex x = x-1\n

result:: Integer;
result = func alex 1 



