import Syntax (Expr,AllDclr)
import Eval (runMain)
import Parser (parseExpr,parseDclr,parseTokens)
import Print (runPrint)
import Transformation (runTransformation)

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO (String, String)
process input = do
  let tokens = parseTokens input
--  return $ ("Tokens: " ++ show tokens)
 -- let ast = parseExpr input
--  return $("Syntax Expr: " ++ show ast)
  return (p1,transfp1) 
  where
      mainAst = parseDclr input
      (p1,transfp1) = 
        case mainAst of
          Left err -> (err, err)
       --     return $ "Parse Error:"
          Right ast ->  ((execPrint ast),(execTransformation ast))
   
    
     -- putStrLn "OK!"-- 

execPrint :: [AllDclr]-> String
execPrint ast = (runPrint ast) -- $ ("Syntax Dclr: " ++ show ast ++ "\n") ++

execTransformation :: [AllDclr]-> String
execTransformation ast = (runPrint (runTransformation ast))

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just ((':'):('l'):(' '):inputFile) -> 
                 liftIO (((readInput inputFile)>>= process) >>= (\(str1,str2) -> (writeOutput inputFile str1 str2)))>> loop    
        --       ((liftIO  ((readInput inputFile)>>= process)) >>= outputStrLn)>> loop 
      Just input -> (liftIO $ process input) >> loop


writeOutput :: FilePath -> String -> String ->IO()
writeOutput i1 str1 str2 = do 
  writeFile ("Output"++ i1) str1
  writeFile ("TransfOutput"++ i1) str2
 
readInput:: String ->IO String
readInput fileName = do 
                       contents <- readFile fileName
                       return contents 
                         
