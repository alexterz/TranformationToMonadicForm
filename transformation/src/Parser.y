{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
  parseTokens,
) where

import Lexer
import Syntax

import Control.Monad.Except

}



-- Entry point
%name expr

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

-- Osperators
%left '+' '-'
%left '*'
%%


Expr : let Dclrs in Expr           { Let $2 $4} 
     | '\\' Vars '->' Expr         { Lam $2 $4 }
     | Form                        { $1 }
--     | Dclr


Form : Form '+' Form               { Op Add $1 $3 }
     | Form '-' Form               { Op Sub $1 $3 }
     | Form '*' Form               { Op Mul $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Lit (LInt $1) }
     | VAR                         { Var $1 }
     | true                        { Lit (LBool True) }
     | false                       { Lit (LBool False) }

--Declarations are of the form Dclr;...;Dclr
Dclrs :  Dclrs ';' Dclr            { $3 : $1 }
      |  Dclr                      { [$1] }


Dclr : VAR '=' Expr                { Assign $1 $3 }
--     | VAR Apats '=' Expr          { FunDclr $2 $4}

--Apats: Apat Apats                  {$1: $2}
--     | {-empty-}                   {[]} -- edw den eimai sigourh an thelw panta lista

--Apat : VAR                         {Var $1} ---allooooooooooooooooooooooooooooooooooooooo

Vars: VAR Vars                     {$1: $2}
    | VAR                          {[$1]} 


{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
    
}
