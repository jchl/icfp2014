{
module JMLTokens (Token(..), lex) where
import Prelude hiding (lex)
}

%wrapper "posn"

@op1 =
   fst
   |snd
   |not
   |break
   |null
   |head
   |tail

@op2 =
   and
   |or
   |[\+ \- \* \/ \< \>]
   |[\= \! \< \>]=
   |::

tokens :-

   $white+                               ;
   \; .*                                 ;
   \{ ([\x00-\x10ffff] # \})* \}         ;

   \(                                    { \p -> \s -> TokenOpenParen }
   \)                                    { \p -> \s -> TokenCloseParen }
   \[                                    { \p -> \s -> TokenOpenBracket }
   \]                                    { \p -> \s -> TokenCloseBracket }
   \,                                    { \p -> \s -> TokenComma }
   =                                     { \p -> \s -> TokenEquals }
   =>                                    { \p -> \s -> TokenArrow }

   fn                                    { \p -> \s -> TokenFn }
   fun                                   { \p -> \s -> TokenFun }
   const                                 { \p -> \s -> TokenConst }
   main                                  { \p -> \s -> TokenMain }
   if                                    { \p -> \s -> TokenIf }
   then                                  { \p -> \s -> TokenThen }
   else                                  { \p -> \s -> TokenElse }
   let                                   { \p -> \s -> TokenLet }
   in                                    { \p -> \s -> TokenIn }
   end                                   { \p -> \s -> TokenEnd }
   trace                                 { \p -> \s -> TokenTrace }
   @op1                                  { \p -> \s -> TokenOperator1 s }
   @op2                                  { \p -> \s -> TokenOperator2 s }

   [\-]?[0-9]+                           { \p -> \s -> TokenNumber (read s) }
   true                                  { \p -> \s -> TokenBool True }
   false                                 { \p -> \s -> TokenBool False }
   [a-zA-Z][a-zA-Z_\'0-9]*               { \p -> \s -> TokenIdentifier s }

{

data Token = TokenOpenParen
           | TokenCloseParen
           | TokenOpenBracket
           | TokenCloseBracket
           | TokenComma
           | TokenEquals
           | TokenArrow
           | TokenFn
           | TokenFun
           | TokenConst
           | TokenMain
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenLet
           | TokenIn
           | TokenEnd
           | TokenTrace
           | TokenOperator1 String
           | TokenOperator2 String
           | TokenNumber Int
           | TokenBool Bool
           | TokenIdentifier String
           deriving (Show)

lex = alexScanTokens

}
