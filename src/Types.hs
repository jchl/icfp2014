module Types where

import Data.Maybe
import Data.List

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

instance Show Op2 where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show Equals = "=="
  show NotEquals = "!="
  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanOrEquals = "<="
  show GreaterThanOrEquals = ">="
  show And = "and"
  show Or = "or"

data Op1 = Not |
           Fst |
           Snd |
           Break

instance Show Op1 where
  show Not = "not"
  show Fst = "fst"
  show Snd = "snd"
  show Break = "break"

type Pat = Identifier

data Expr = Number Int |
            Boolean Bool |
            List [Expr] |
            Identifier Identifier |
            Pair Expr Expr |
            Operator1 Op1 Expr |
            Operator2 Op2 Expr Expr |
            Trace Expr Expr |
            If Expr Expr Expr |
            Fn [Pat] Expr |
            App Expr [Expr] |
            Let Pat Expr Expr

instance Show Expr where
  show (Number n) = show n
  show (Boolean b) = show b
  show (List es) = show es
  show (Identifier id) = id
  show (Pair e1 e2) = show (e1, e2)
  show (Operator1 op e) = show op ++ " (" ++ show e ++ ")"
  show (Operator2 op e1 e2) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (Trace e1 e2) = "trace (" ++ show e1 ++ ") in (" ++ show e2 ++ ")"
  show (If e1 e2 e3) = "if (" ++ show e1 ++ ") then (" ++ show e2 ++ ") else (" ++ show e3 ++ ")"
  show (Fn pats e) = "fn " ++ intercalate " " pats ++ " => (" ++ show e ++ ")"
  show (App e es) = "(" ++ show e ++ ") " ++ intercalate " " (map (\e -> "(" ++ show e ++ ")") es)
  show (Let pat e1 e2) = "let " ++ pat ++ " = " ++ show e1 ++ " in (" ++ show e2 ++ ")"

data Declaration = FunDecl Identifier [Pat] Expr |
                   ValDecl Pat Expr
                   deriving Show

data MainDecl = MainDecl [Pat] Expr
                deriving Show

type JmlProgram = ([Declaration], MainDecl)

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
  ( "break", Break ) ]

stringToOp1 :: String -> Op1
stringToOp1 s = fromJust $ lookup s ops1Map

stringToOp2 :: String -> Op2
stringToOp2 s = fromJust $ lookup s ops2Map
