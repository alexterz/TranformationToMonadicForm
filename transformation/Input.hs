
divExc:: Integer -> Integer -> Except String Integer;
divExc i j= 
  case j of
    0 -> throwError "Divide with zero" ;
    x -> return (i `div` x) \n


addStateToValue:: Integer ->State Integer Integer ;
addStateToValue a = get >>= (\s-> return (a+s))\n


totalTest::Monad m =>Integer -> Integer -> m Integer ;
totalTest i j = (addStateToValue j) >>= (\x->divExc i x) \n



res::Monad m => m Integer;
res = (fst (runState (totalTest 5 0) 0))\n


result:: Either String Integer ;
result = runExcept res 


