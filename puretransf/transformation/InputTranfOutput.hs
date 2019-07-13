import InputMConvert 
import Control.Eff

trial :: (Eff r ((Eff r (Integer-> (Eff r Integer )) )-> (Eff r (Integer-> (Eff r Integer )) )) )
trial = (return (let trial f 0 = (return 0);trial f x = ((return (trial f))>>=( \ g0  -> (g0 ((f x)-1))));in (mConvert1 trial)));
ex :: (Eff r Integer )
ex = (let ex = ((trial>>=( \ g1  -> (g1 alex)))>>=( \ g0  -> (g0 2)));in ex);
func :: (Eff r ((Eff r (Integer-> (Eff r Integer )) )-> (Eff r (Integer-> (Eff r Integer )) )) )
func = (return (let func f x = (f>>=( \ g0  -> (g0 x)));in (mConvert1 func)));
alex :: (Eff r (Integer-> (Eff r Integer )) )
alex = (return (let alex x = (return (x-1));in alex));
result :: (Eff r Integer )
result = (let result = ((func>>=( \ g1  -> (g1 alex)))>>=( \ g0  -> (g0 1)));in result);

main::IO ()
main= putStrLn $show $run $result