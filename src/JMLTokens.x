{
module JMLTokens (Token(..), jmlLex) where
}

%wrapper "posn"

@op1 =
   fst
   |snd
   |not
   |break
   |trace

@op2 =
   and
   |or
   |[\+ \- \* \/ \< \>]
   |==
   |!=

tokens :-

   $white+                               ;
   \; .*                                 ;

   \(                                    { \p -> \s -> TokenOpenParen }
   \)                                    { \p -> \s -> TokenCloseParen }
   \,                                    { \p -> \s -> TokenComma }
   =                                     { \p -> \s -> TokenEquals }
   =>                                    { \p -> \s -> TokenArrow }

   fn                                    { \p -> \s -> TokenFn }
   fun                                   { \p -> \s -> TokenFun }
   val                                   { \p -> \s -> TokenVal }
   if                                    { \p -> \s -> TokenIf }
   then                                  { \p -> \s -> TokenThen }
   else                                  { \p -> \s -> TokenElse }
   let                                   { \p -> \s -> TokenLet }
   in                                    { \p -> \s -> TokenIn }
   end                                   { \p -> \s -> TokenEnd }
   @op1                                  { \p -> \s -> TokenOperator1 s }
   @op2                                  { \p -> \s -> TokenOperator2 s }

   [0-9]+                                { \p -> \s -> TokenNumber (read s) }
   true                                  { \p -> \s -> TokenBool True }
   false                                 { \p -> \s -> TokenBool False }
   [a-z][a-z_0-9]*                       { \p -> \s -> TokenIdentifier s }

{

tokenError :: (String -> String) -> AlexPosn -> String -> a
tokenError f (AlexPn char line col) s =
   error $ f s ++ " at line " ++ show line ++ ", column " ++ show col

data Token = TokenOpenParen
           | TokenCloseParen
           | TokenComma
           | TokenEquals
           | TokenArrow
           | TokenFn
           | TokenFun
           | TokenVal
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenLet
           | TokenIn
           | TokenEnd
           | TokenOperator1 String
           | TokenOperator2 String
           | TokenNumber Int
           | TokenBool Bool
           | TokenIdentifier String
           deriving (Show)

jmlLex = alexScanTokens

}
