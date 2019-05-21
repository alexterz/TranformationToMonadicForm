import Syntax (Expr,Dclr)
import Eval (runEval,runMain)
import Parser (parseExpr,parseDclr,parseTokens)

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
    Right ast -> execMain ast --putStrLn "OK!"-- 

execMain :: [Dclr] -> IO ()
execMain ast = do
  let result = runMain ast
  case result of
    Left err -> do
      putStrLn "Runtime Error:"
      putStrLn err
    Right res -> print res

exec :: Expr -> IO ()
exec ast = do
  let result = runEval ast
  case result of
    Left err -> do
      putStrLn "Runtime Error:"
      putStrLn err
    Right res -> print res

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
