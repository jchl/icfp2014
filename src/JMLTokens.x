{
module JMLTokens (Token(..), jmlLex) where
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
   |==
   |!=
   |::

tokens :-

   $white+                               ;
   \; .*                                 ;

   \(                                    { \p -> \s -> TokenOpenParen }
   \)                                    { \p -> \s -> TokenCloseParen }
   \[                                    { \p -> \s -> TokenOpenBracket }
   \]                                    { \p -> \s -> TokenCloseBracket }
   \,                                    { \p -> \s -> TokenComma }
   =                                     { \p -> \s -> TokenEquals }
   =>                                    { \p -> \s -> TokenArrow }

   fn                                    { \p -> \s -> TokenFn }
   fun                                   { \p -> \s -> TokenFun }
   val                                   { \p -> \s -> TokenVal }
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

   [0-9]+                                { \p -> \s -> TokenNumber (read s) }
   true                                  { \p -> \s -> TokenBool True }
   false                                 { \p -> \s -> TokenBool False }
   [a-zA-Z][a-zA-Z_\'0-9]*               { \p -> \s -> TokenIdentifier s }

{

tokenError :: (String -> String) -> AlexPosn -> String -> a
tokenError f (AlexPn char line col) s =
   error $ f s ++ " at line " ++ show line ++ ", column " ++ show col

data Token = TokenOpenParen
           | TokenCloseParen
           | TokenOpenBracket
           | TokenCloseBracket
           | TokenComma
           | TokenEquals
           | TokenArrow
           | TokenFn
           | TokenFun
           | TokenVal
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

jmlLex = alexScanTokens

}
