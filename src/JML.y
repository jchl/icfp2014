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

Program : ConstDecls FunDecls MainDecl          { ($1, $2, $3) }

ConstDecl : const id '=' Exp                    { ($2, $4) }
ConstDecls : ConstDecl ConstDecls               { $1 : $2 }
           |                                    { [] }

FunDecl : fun id Args '=' Exp                   { ($2, $3, $5) }
FunDecls : FunDecl FunDecls                     { $1 : $2 }
         |                                      { [] }

MainDecl : main Args '=' Exp                    { ($2, $4) }

Args : id Args                                  { $1 : $2 }
     |                                          { [] }

AtExp : num                                     { Number $1 }
      | bool                                    { Boolean $1 }
      | '(' Exp ',' ExpsWithCommasPlus ')'      { Tuple ($2 : $4) }
      | '[' ExpsWithCommas ']'                  { List $2 }
      | id                                      { Identifier $1 }
      | let LetPat '=' Exp in Exp end           { Let $2 $4 $6 }
      | trace Exp in Exp end                    { Trace $2 $4 }
      | '(' Exp ')'                             { $2 }

AppExp : AtExp                                  { $1 }
       | AtExp AtExps                           { App $1 $2 }

OpExp : AppExp                                  { $1 }
      | op1 AppExp                              { Operator1 (stringToOp1 $1) $2 }
      | AppExp op2 OpExp                        { Operator2 (stringToOp2 $2) $1 $3 }

Exp : OpExp                                     { $1 }
    | fn Args '=>' Exp                          { Fn $2 $4 }
    | if Exp then Exp else Exp                  { If $2 $4 $6 }

LetPat : id                                     { IdPat $1 }
       | '(' id ',' IdsWithCommasPlus ')'       { TuplePat ($2 : $4) }

IdsWithCommasPlus : id ',' IdsWithCommasPlus    { $1 : $3 }
                  | id                          { [$1] }

AtExps : AtExp AtExps                           { $1 : $2 }
       | AtExp                                  { [$1] }

ExpsWithCommas : Exp ',' ExpsWithCommas         { $1 : $3 }
               | Exp                            { [$1] }
               |                                { [] }

ExpsWithCommasPlus : Exp ',' ExpsWithCommasPlus { $1 : $3 }
                   | Exp ','                    { [$1] }
                   | Exp                        { [$1] }

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error at " ++ show (take 20 tokens)

}
