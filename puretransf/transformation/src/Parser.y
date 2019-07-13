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
%name expr Expr
%name dclr AllDclrs

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    '\n'   { TokenNewLine}
    '::'   { TokenHasType }
    '=>'   { TokenContext } 
--    return { TokenReturn }
    let    { TokenLet }
    true   { TokenTrue }
    false  { TokenFalse }
    in     { TokenIn }
    NUM    { TokenNum $$ }
    VAR    { TokenSym $$ }
    '\\'   { TokenLambda }
    '->'   { TokenArrow }
    '='    { TokenEq }
    '+'    { TokenAdd }
    '-'    { TokenSub }
    '*'    { TokenMul }
    '('    { TokenLParen }
    ')'    { TokenRParen }
    '{'    { TokenLBracket }
    '}'    { TokenRBracket }
    '['    { TokenLListOp }
    ']'    { TokenRListOp }
    ','    { TokenComma }
    ':'    { TokenCons }
    '_'    { TokenUnderScore }
    '>>='  { TokenBind }
    ';'    { TokenSemicolon }
    

-- Osperators
%right '->'
%right in
%left '>>='
%right ':'
%left '+' '-'
%left '*'
%%


AllDclrs :  AllDclr '\n' AllDclrs  { $1 : $3 } 
         |  AllDclr                { [$1] }
                

AllDclr: TypeSignature ';' Dclrs   { WithSign $1 $3 } 

--Declarations are of the form Dclr;...;Dclr
Dclrs :  Dclr ';' Dclrs            { $1 : $3 } 
      |  Dclr                      { [$1] }

Dclr : VAR Apats '=' Expr       { Assign $1 $2 $4} 

--------------------------------------------------------------------------------

--let VAR '=' Expr in Expr    { App (Lam $2 $6) $4 }
Expr : let Dclrs in Expr            { Let $2 $4} 
     | '\\' Apats '->' Expr         { Lam $2 $4 }
     | Expr ':' Expr                { Cons $1 [$3]} 
     --| return Expr                  { Monadic $2 }
     --| Expr '>>=' Expr              { Bind $1 $3 } 
     | Form                         { $1 }



Form : Form '+' Form               { Op Add $1 $3 }
     | Form '-' Form               { Op Sub $1 $3 }
     | Form '*' Form               { Op Mul $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { App $1 $2 }
     | Atom                        { $1 }


Atom :'(' Expr ')'                 {  $2 } 
     | List                        { List $1} 
     | NUM                         { Apat (Lit (LInt $1)) }
     | VAR                         { Apat (Var $1) }
     | true                        { Apat (Lit (LBool True)) }
     | false                       { Apat (Lit (LBool False)) }


-- List for expressions
List : '[' ListExpr ']'             { $2 }
     | '[' {-empty-} ']'            { [] }

    

ListExpr : Expr ',' ListExpr      { $1 : $3 }
         | Expr                   { $1:[] }

--------------------------------------------------------------------------------


TypeSignature: VAR '::' Type               {Signature $1 $3} 
             | VAR '::' Contexts '=>' Type {ContSignature $1 $3 $5}

Contexts: '('Context')'             { $2 }
        | VAR VAR                   { [Constraint $1 $2]}

Context: VAR VAR ',' Context        { (Constraint $1 $2):$4} 
       | VAR VAR                    { [Constraint $1 $2]}
             
Type: Container '->' Type           { TFunc $1 $3}  
    | Container                     { $1 } 

Container: VAR TypeList             { Container $1 $2 }
         | SimpleType               { $1 }

TypeList: SimpleType TypeList       { $1 : $2 } 
        | SimpleType                { [$1] }

SimpleType: '('Type')'              { $2 } 
          | '['Type']'              { TList $2}
          | VAR                     { Literal $1 } 
          | '('{-empty-}')'         { Void }



---------------------------------------------------------------------------
Apats: Apat Apats                   { $1 : $2 }
     | {-empty-}                    { [] } 

Apat : ListArgs                      { ListArgs $1 }
     | SimpleApat                    { $1 }    
--     | '_'                          { } -- kapws prepei na to ftiaxw gia underscore

SimpleApat :'('Apat')'                   { $2 }
           | VAR                         { Var $1 }
           | NUM                         { Lit (LInt $1) }

ListArgs : '[' ListApats ']'        { $2 }
         | '[' {-empty-} ']'        { [] } 
         | SimpleApat ':' TailArgs  { $1 : $3 } -- gia na exw lista apo listes prepei na exw parentheseis :/ ([x]):xs


ListApats : Apat ',' ListApats      { $1 : $3 }
          | Apat                    { [$1] }

TailArgs : ListArgs                 { $1 }
         |'(' ListArgs ')'          { $2 } --edw ginetai ena shift/reduce conflict alla to thelw gia (x:(y:ys)) pws alliws??
         | VAR                      { [ListArgs [Var $1]] } --{[Var $1]}


 

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseDclr ::String -> Either String AllDclrs
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

