module Types where

import Data.Maybe

type Identifier = String
data Op2 = Plus |
           Minus |
           Times |
           Divide |
           Equals |
           NotEquals |
           LessThan |
           GreaterThan |
           LessThanOrEquals |
           GreaterThanOrEquals |
           And |
           Or

data Op1 = Not |
           Fst |
           Snd |
           Break |
           Trace

type Pat = Identifier

data Expr = Number Int |
            Boolean Bool |
            Identifier Identifier |
            Pair Expr Expr |
            Operator1 Op1 Expr |
            Operator2 Op2 Expr Expr |
            If Expr Expr Expr |
            Fn [Pat] Expr |
            App Expr [Expr] |
            Let [Pat] Expr Expr

data Declaration = FunDecl Identifier [Pat] Expr |
                   ValDecl [Pat] Expr

type JmlProgram = [Declaration]

ops2Map = [
  ( "+", Plus ),
  ( "-", Minus ),
  ( "*", Times ),
  ( "/", Divide ),
  ( "==", Equals ),
  ( "!=", NotEquals ),
  ( "<", LessThan ),
  ( ">", GreaterThan ),
  ( "<=", LessThanOrEquals ),
  ( ">=", GreaterThanOrEquals ),
  ( "and", And ),
  ( "or", Or ) ]

ops1Map = [
  ( "not", Not ),
  ( "fst", Fst ),
  ( "snd", Snd ),
  ( "break", Break ),
  ( "trace", Trace ) ]

stringToOp1 :: String -> Op1
stringToOp1 s = fromJust $ lookup s ops1Map

stringToOp2 :: String -> Op2
stringToOp2 s = fromJust $ lookup s ops2Map
