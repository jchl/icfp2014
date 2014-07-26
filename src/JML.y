{
module JML (parse) where

import JMLTokens
import Types
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  '('     { TokenOpenParen }
  ')'     { TokenCloseParen }
  ','     { TokenComma }
  '='     { TokenEquals }
  '=>'    { TokenArrow }
  fn      { TokenFn }
  fun     { TokenFun }
  val     { TokenVal }
  if      { TokenIf }
  then    { TokenThen }
  else    { TokenElse }
  let     { TokenLet }
  in      { TokenIn }
  end     { TokenEnd }
  op1     { TokenOperator1 $$ }
  op2     { TokenOperator2 $$ }
  num     { TokenNumber $$ }
  bool    { TokenBool $$ }
  id      { TokenIdentifier $$ }

%%

Program : Declaration Program      { $1 : $2 }
        | Declaration              { [$1] }

Declaration : FunDecl              { $1 }
            | ValDecl              { $1 }

FunDecl : fun id Pats '=>' Exp     { FunDecl $2 $3 $5 }
ValDecl : val Pat '=' Exp          { ValDecl $2 $4 }

Pats : Pat Pats                    { $1 : $2 }
     | Pat                         { [$1] }
Pat : id                           { $1 }

Exp : num                          { Number $1 }
    | bool                         { Boolean $1 }
    | id                           { Identifier (makeIdentifier $1) }
    | '(' Exp ')'                  { $2 }
    | '(' Exp ',' Exp ')'          { Pair $2 $4 }
    | op1 Exp                      { Operator1 (stringToOp1 $1) $2 }
    | Exp op2 Exp                  { Operator2 (stringToOp2 $2) $1 $3 }
    | if Exp then Exp else Exp     { If $2 $4 $6 }
    | fn Pats '=>' Exp             { Fn $2 $4 }
    | Exp Exps                     { App $1 $2 }
    | let Pat '=' Exp in Exp end   { Let $2 $4 $6 }

Exps : Exp Exps                    { $1 : $2 }
     | Exp                         { [$1] }

{

makeIdentifier :: String -> Identifier
makeIdentifier name = name

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
