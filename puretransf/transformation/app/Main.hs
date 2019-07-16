{-# LANGUAGE QuasiQuotes #-} 

import Syntax (Expr,AllDclr)
--import Eval (runMain)
import Parser (parseExpr,parseDclr,parseTokens)
import Print (runPrint)
import Transformation ( runTransformation)


import System.ShQQ
import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO (String, String)
process input = do
  let tokens = parseTokens input
--  return $ ("Tokens: " ++ show tokens)
 -- let ast = parseExpr input
--  return $("Syntax Expr: " ++ show ast)
  return (p1,p2) 
  where
      mainAst = parseDclr input
      (p1,p2) = 
        case mainAst of
          Left err -> (err, err)
       --     return $ "Parse Error:"
          Right ast ->  ((execPrint ast),(execTransformation ast)) --show ast ++"\n"++ 
 
execPrint :: [AllDclr]-> String
execPrint ast = runPrint ast--("{--Syntax Dclr: " ++ show ast ++ "--}\n") ++(runPrint ast)++ "\n" 

execTransformation :: [AllDclr]-> (String)
execTransformation ast = runPrint transformed --("{--"++show (runTransformation ast)) ++"--}\n" ++(runPrint transformed) --
  where transformed = runTransformation ast 

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just ((':'):('l'):(' '):inputFile) -> 
                 liftIO 
                 (((readInput $ inputFile++".hs")>>= process) >>= 
                 (\(str1,str2) -> (writeOutput inputFile str1 str2 ))>>=
                 (\_-> [sh| ghc $file1 |])>>=
                 (\comp1-> [sh| ghc $file2 |]>>= 
                 (\comp2-> [sh| ./$exec1 |]>>= 
                 (\exec1->([sh| ./$exec2 |])>>=
                 (\exec2-> putStrLn (comp1++"Output:\n"++exec1 ++"\n"++comp2++"Transformated Output:\n"++exec2)))))>>=
                 (\_->[sh| rm $out1 $out2 $exec1 $exec2|])) >> loop
                 where
                    file1 = (inputFile ++"Output.hs")
                    file2 = (inputFile ++ "TranfOutput.hs")
                    out1 = (inputFile ++"Output.o")
                    out2 = (inputFile ++ "TranfOutput.o") 
                    exec1 = (inputFile ++"Output")
                    exec2 = (inputFile ++ "TranfOutput")     
        --       ((liftIO  ((readInput inputFile)>>= process)) >>= outputStrLn)>> loop 
      Just input -> (liftIO $ process input) >> loop


writeOutput :: FilePath -> String -> String ->IO()
writeOutput i1 str1 str2 = do 
  writeFile (i1++"Output.hs") (str1++forMain)
  writeFile (i1++ "TranfOutput.hs") ("import "++ i1 ++ "MConvert \n" ++ imports ++ str2++forTranfMain)
  writeFile (i1++"MConvert.hs") ("module "++ i1 ++ "MConvert" ++mConvert)

readInput:: FilePath ->IO String
readInput fileName = do 
                       contents <- readFile fileName
                       return contents 


mConvert:: String
mConvert = 
  " where \n\n" ++ imports
  ++"mConvert0 :: a -> Eff r a \nmConvert0 =return \n\n"
  ++ "mConvert1 :: (t -> a) -> (t -> Eff r a) \nmConvert1 f x = return(f x) \n\n"
  ++ "mConvert2:: (t2 -> t1 -> a) -> t2 -> Eff r (t1 -> Eff r a) \nmConvert2 f x = return $ mConvert1 $ f x \n\n"
  ++ "mConvert3 :: (t -> t2 -> t1 -> a) -> t -> Eff r (t2 -> Eff r (t1 -> Eff r a)) \nmConvert3 f x =return $ mConvert2 $ f x \n\n"  


forMain:: String
forMain = 
 "\nmain::IO ()\nmain= putStrLn $show $result"

forTranfMain:: String
forTranfMain = plus ++ "\nmain::IO ()\nmain= putStrLn $show $run $result"                     

plus:: String 
plus = 
 "\nplus::(Num a)=> Eff r (a->Eff r (a-> Eff r a))\n"++
 "plus = let plus x y = return (x+y) in return (mConvert1 plus)\n\n"

imports::String
imports =
  "import Control.Eff\n\n" 
