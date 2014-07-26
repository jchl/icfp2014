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

AtExp : num                                { Number $1 }
      | bool                               { Boolean $1 }
      | '(' Exp ',' ExpsWithCommasPlus ')' { Tuple ($2 : $4) }
      | '[' ExpsWithCommas ']'             { List $2 }
      | id                                 { Identifier (makeIdentifier $1) }
      | '(' Exp ')'                        { $2 }
      | let LetPat '=' Exp in Exp end      { Let $2 $4 $6 }
      | trace Exp in Exp end               { Trace $2 $4 }

OpExp : AtExp                             { $1 }
      | op1 AtExp                         { Operator1 (stringToOp1 $1) $2 }
      | AtExp op2 OpExp                   { Operator2 (stringToOp2 $2) $1 $3 }
      | AtExp AtExps                      { App $1 $2 }

Exp : OpExp                        { $1 }
    | fn Pats '=>' Exp             { Fn $2 $4 }
    | if Exp then Exp else Exp     { If $2 $4 $6 }

LetPat : id                                 { IdPat $1 }
       | '(' id ',' IdsWithCommasPlus ')'   { TuplePat ($2 : $4) }

IdsWithCommasPlus : id ',' IdsWithCommasPlus { $1 : $3 }
                  | id                       { [$1] }

AtExps : AtExp AtExps                  { $1 : $2 }
       | AtExp                         { [$1] }

ExpsWithCommas : Exp ',' ExpsWithCommas { $1 : $3 }
               | Exp                    { [$1] }
               |                        { [] }

ExpsWithCommasPlus : Exp ',' ExpsWithCommasPlus { $1 : $3 }
                   | Exp ','                    { [$1] }
                   | Exp                        { [$1] }

{

makeIdentifier :: String -> Identifier
makeIdentifier name = name

parseError :: [Token] -> a
parseError tokens = error $ "Parse error at " ++ show (take 20 tokens)

}
