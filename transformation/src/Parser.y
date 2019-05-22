{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
  parseDclr,
  parseTokens,
) where

import Lexer
import Syntax

import Control.Monad.Except

}



-- Entry point
%name expr
%name dclr Dclrs

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { TokenLet }
    true  { TokenTrue }
    false { TokenFalse }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    '='   { TokenEq }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '('   { TokenLParen }
    ')'   { TokenRParen }
    ';'   { TokenSemicolon}
    '{'   { TokenLBracket}
    '}'   { TokenRBracket}
    '_'   { TokenUnderScore }

-- Osperators
%left '+' '-'
%left '*'
%%

--let VAR '=' Expr in Expr    { App (Lam $2 $6) $4 }
Expr : let Dclrs in Expr            { Let $2 $4} 
     | '\\' Apats '->' Expr         { Lam $2 $4 }
     | Form                         { $1 }



Form : Form '+' Form               { Op Add $1 $3 }
     | Form '-' Form               { Op Sub $1 $3 }
     | Form '*' Form               { Op Mul $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom :'(' Expr ')'                 {  $2 }
     | NUM                         { Apat ( Lit (LInt $1)) }
     | VAR                         { Apat (Var $1) }
     | true                        { Apat (Lit (LBool True)) }
     | false                       { Apat (Lit (LBool False)) }

--Declarations are of the form Dclr;...;Dclr
Dclrs :  Dclr ';' Dclrs            { $1 : $3 } -- that's way we can declare something first that we will use later, but not the opposite
      |  Dclr                      { [$1] }    -- , so order matters... :(
--      |  Dclrs ';' Dclr            { $3 : $1 }


Dclr : VAR '=' Expr                { Assign $1 [] $3 } --den xreiazetai
     | VAR Apats '=' Expr          { Assign $1 $2 (Lam $2 $4)}


Apats: Apat Apats                   { $1 : $2 }
     | {-empty-}                    {[]} 


Apat : VAR                          { Var $1}
     | NUM                          { Lit (LInt $1) }
--     | '_'                          { } -- kapws prepei na to ftiaxw gia underscore
 

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseDclr ::String -> Either String Dclrs
parseDclr input = runExcept $ do
  tokenStream <- scanTokens input
  dclr tokenStream 

parseExpr :: String -> Either String Expr
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
    
}
