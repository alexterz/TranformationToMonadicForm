import Syntax (Expr,AllDclr)
import Eval (runMain)
import Parser (parseExpr,parseDclr,parseTokens)
import Print (runPrint)

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process input = do
  let tokens = parseTokens input
  putStrLn ("Tokens: " ++ show tokens)
  let ast = parseExpr input
  putStrLn ("Syntax Expr: " ++ show ast)
  let mainAst = parseDclr input
  putStrLn ("Syntax Dclr: " ++ show mainAst)
  case mainAst of
    Left err -> do
      putStrLn "Parse Error:"
      print err
    Right ast -> execPrint ast -- putStrLn "OK!"-- 

execPrint:: [AllDclr]-> IO ()
execPrint ast = do 
  putStrLn $ runPrint ast
{--
execMain :: [Dclr] -> IO ()
execMain ast = do
  let result = runMain ast
  case result of
    Left err -> do
      putStrLn "Runtime Error:"
      putStrLn err
    Right (l:ls) -> print l
    otherwise -> do
                  putStrLn "Runtime Error: Non-exhaustive patterns"
--}
main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
