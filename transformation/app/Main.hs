import Syntax (Expr,AllDclr)
--import Eval (runMain)
import Parser (parseExpr,parseDclr,parseTokens)
import Print (runPrint)
import Transformation (convert, runTransformation)

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO (String, (String,String))
process input = do
  let tokens = parseTokens input
--  return $ ("Tokens: " ++ show tokens)
 -- let ast = parseExpr input
--  return $("Syntax Expr: " ++ show ast)
  return (p1,(p2,p3)) 
  where
      mainAst = parseDclr input
      (p1,(p2,p3)) = 
        case mainAst of
          Left err -> (err, (err,err))
       --     return $ "Parse Error:"
          Right ast ->  ((execPrint ast),(execTransformation ast)) --show ast ++"\n"++ 
 
execPrint :: [AllDclr]-> String
execPrint ast = (runPrint ast) -- $ ("Syntax Dclr: " ++ show ast ++ "\n") ++

execTransformation :: [AllDclr]-> (String,String)
execTransformation ast = (runPrint transformed, ("Import MConvert\n\n"++runPrint (convert transformed))) --show (runTransformation ast) ++"\n" ++
  where transformed = runTransformation ast 

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just ((':'):('l'):(' '):inputFile) -> 
                 liftIO (((readInput inputFile)>>= process) >>= (\(str1,(str2,str3)) -> (writeOutput inputFile str1 str2 str3)))>> loop    
        --       ((liftIO  ((readInput inputFile)>>= process)) >>= outputStrLn)>> loop 
      Just input -> (liftIO $ process input) >> loop


writeOutput :: FilePath -> String -> String->String ->IO()
writeOutput i1 str1 str2 str3= do 
  writeFile ("Output"++ i1) str1
  writeFile ("TransfOutput"++ i1) str2
  writeFile ("EndOutput"++ i1) str3
readInput:: FilePath ->IO String
readInput fileName = do 
                       contents <- readFile fileName
                       return contents 
                         
