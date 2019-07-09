{
{-#LANGUAGE FlexibleContexts #-}

module Lexer (
  Token(..),
  scanTokens
) where

import Syntax

import Control.Monad.Except

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  \\n                           { \s -> TokenNewLine }
  \::                           { \s -> TokenHasType }
  "=>"                          { \s -> TokenContext }
--  return                        { \s -> TokenReturn } 
  let                           { \s -> TokenLet }
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }
  in                            { \s -> TokenIn }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \s -> TokenArrow }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
  \;                            { \s -> TokenSemicolon }
  \{                            { \s -> TokenLBracket }
  \}                            { \s -> TokenRBracket }
  \[                            { \s -> TokenLListOp }
  \]                            { \s -> TokenRListOp }
  \,                            { \s -> TokenComma }
  \:                            { \s -> TokenCons }
  \_                            { \s -> TokenUnderScore }
  \>>=                          { \s -> TokenBind }
   

{

data Token
  = TokenNewLine
  | TokenHasType
  | TokenContext 
  | TokenLet
--  | TokenReturn
  | TokenTrue
  | TokenFalse
  | TokenIn
  | TokenLambda
  | TokenNum Int
  | TokenSym String
  | TokenArrow
  | TokenEq
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenLParen
  | TokenRParen
  | TokenSemicolon
  | TokenLBracket
  | TokenRBracket
  | TokenLListOp
  | TokenRListOp
  | TokenComma
  | TokenCons  
  | TokenEOF
  | TokenUnderScore
  | TokenBind 

  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)

}
