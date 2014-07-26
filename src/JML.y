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
  '['     { TokenOpenBracket }
  ']'     { TokenCloseBracket }
  ','     { TokenComma }
  '='     { TokenEquals }
  '=>'    { TokenArrow }
  fn      { TokenFn }
  fun     { TokenFun }
  val     { TokenVal }
  const   { TokenConst }
  main    { TokenMain }
  if      { TokenIf }
  then    { TokenThen }
  else    { TokenElse }
  let     { TokenLet }
  in      { TokenIn }
  end     { TokenEnd }
  trace   { TokenTrace }
  op1     { TokenOperator1 $$ }
  op2     { TokenOperator2 $$ }
  num     { TokenNumber $$ }
  bool    { TokenBool $$ }
  id      { TokenIdentifier $$ }

%%

Program : ConstDecls Declarations MainDecl     { ($1, $2, $3) }

ConstDecl : const id '=' Exp       { ($2, $4) }
ConstDecls : ConstDecl ConstDecls  { $1 : $2 }
           |                       { [] }

Declarations : Declaration Declarations { $1 : $2 }
             |                          { [] }
Declaration : FunDecl              { $1 }
            | ValDecl              { $1 }

FunDecl : fun id Pats '=' Exp      { FunDecl $2 $3 $5 }
ValDecl : val Pat '=' Exp          { ValDecl $2 $4 }

MainDecl : main Pats '=' Exp       { MainDecl $2 $4 }

Pats : Pat Pats                    { $1 : $2 }
     |                             { [] }
Pat : id                           { $1 }

Exp : num                          { Number $1 }
    | bool                         { Boolean $1 }
    | '[' ExpsWithCommas ']'       { List $2 }
    | id                           { Identifier (makeIdentifier $1) }
    | '(' Exp ')'                  { $2 }
    | '(' Exp ',' Exp ')'          { Pair $2 $4 }
    | op1 Exp                      { Operator1 (stringToOp1 $1) $2 }
    | trace Exp in Exp end         { Trace $2 $4 }
    | Exp op2 Exp                  { Operator2 (stringToOp2 $2) $1 $3 }
    | if Exp then Exp else Exp     { If $2 $4 $6 }
    | fn Pats '=>' Exp             { Fn $2 $4 }
    | Exp Exps                     { App $1 $2 }
    | let Pat '=' Exp in Exp end   { Let $2 $4 $6 }

Exps : Exp Exps                    { $1 : $2 }
     | Exp                         { [$1] }

ExpsWithCommas : Exp ',' ExpsWithCommas { $1 : $3 }
               | Exp                    { [$1] }
               |                        { [] }

{

makeIdentifier :: String -> Identifier
makeIdentifier name = name

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
