import Syntax (Expr,AllDclr)
import Eval (runMain)
import Parser (parseExpr,parseDclr,parseTokens)
import Print (runPrint)

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO String
process input = do
  let tokens = parseTokens input
--  return $ ("Tokens: " ++ show tokens)
 -- let ast = parseExpr input
--  return $("Syntax Expr: " ++ show ast)
  let mainAst = parseDclr input
--  return $("Syntax Dclr: " ++ show mainAst)
  case mainAst of
    Left err -> do
      return $ "Parse Error:"
      return $ err
    Right ast ->  (execPrint ast) -- putStrLn "OK!"-- 

execPrint:: [AllDclr]-> IO String
execPrint ast = return (runPrint ast) -- $ ("Syntax Dclr: " ++ show ast ++ "\n") ++

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just ((':'):('l'):(' '):inputFile) ->  
               ((liftIO  ((readInput inputFile)>>= process)) >>= outputStrLn)>> loop
      Just input -> (liftIO $ process input) >> loop



readInput:: String ->IO String
readInput fileName = do 
                       contents <- readFile fileName
                       return contents 
                         
