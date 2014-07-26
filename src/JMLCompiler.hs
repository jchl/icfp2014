module JMLCompiler where

import Types
import GCC

compileJml :: JmlProgram -> ProgramWithLabels
compileJml = undefined

elimBools :: JmlProgram -> JmlProgram
elimBools = undefined

elimBoolsExpr :: Expr -> Expr
elimBoolsExpr e = case e of
  Number n -> Number n
  Boolean True -> Number 1
  Boolean False -> Number 0
  Identifier id -> Identifier id
  Pair e1 e2 -> Pair (elimBoolsExpr e1) (elimBoolsExpr e2)
  Operator1 Not e -> Operator2 Minus (Number 1) (elimBoolsExpr e)
  Operator1 op e -> Operator1 op (elimBoolsExpr e)
  Operator2 And e1 e2 -> If (elimBoolsExpr e1) (elimBoolsExpr e2) (Boolean False)
  Operator2 Or e1 e2 -> If (elimBoolsExpr e1) (Boolean True) (elimBoolsExpr e2)
  Operator2 NotEquals e1 e2 -> elimBoolsExpr (Operator1 Not (Operator2 Equals e1 e2))
  Operator2 op e1 e2 -> Operator2 op (elimBoolsExpr e1) (elimBoolsExpr e2)
  If e1 e2 e3 -> If (elimBoolsExpr e1) (elimBoolsExpr e2) (elimBoolsExpr e3)
  Fn pats e -> Fn pats (elimBoolsExpr e)
  Let pats e1 e2 -> Let pats (elimBoolsExpr e1) (elimBoolsExpr e2)

compileExpr :: Expr -> ProgramWithLabels
compileExpr e =
  case e of
    Number n -> [Instruction $ LDC n]
    Boolean _ -> undefined -- There shouldn't be any booleans left at this point
    Identifier id -> [Instruction $ LD 0 0] -- XXX
    Pair e1 e2 -> compileExpr e1 ++ compileExpr e2 ++ [Instruction $ CONS]
    Operator1 Not e -> undefined -- There shouldn't be any booleans left at this point
    Operator1 Fst e -> compileExpr e ++ [Instruction $ CAR]
    Operator1 Snd e -> compileExpr e ++ [Instruction $ CDR]
    Operator1 Break e -> compileExpr e ++ [Instruction $ BRK]
    Operator1 Trace e -> compileExpr e ++ [Instruction $ DBUG]
    Operator2 op e1 e2 ->
      case op of
        Plus -> stack2Op ADD
        Minus -> stack2Op SUB
        Times -> stack2Op MUL
        Divide -> stack2Op DIV
        Equals -> stack2Op CEQ
        LessThan -> stack2OpRev CGT
        GreaterThan -> stack2Op CGT
        LessThanOrEquals -> stack2OpRev CGTE
        GreaterThanOrEquals -> stack2Op CGTE
        And -> undefined -- There shouldn't be any booleans left at this point
        Or -> undefined -- There shouldn't be any booleans left at this point
      where
        stack2Op insn = compileExpr e1 ++ compileExpr e2 ++ [Instruction $ insn]
        stack2OpRev insn = compileExpr e2 ++ compileExpr e1 ++ [Instruction $ insn]
    If e1 e2 e3 ->
      let lt = "true" in
      let lf = "false" in
      let le = "end" in
      compileExpr e1 ++ [Instruction $ SEL (RefAddr lt) (RefAddr lf)] ++
        [Instruction $ LDC 0, Instruction $ SEL (RefAddr le) (RefAddr le)] ++
        [Label lt] ++ compileExpr e2 ++ [Instruction $ JOIN] ++
        [Label lf] ++ compileExpr e3 ++ [Instruction $ JOIN] ++
        [Label le]
    Fn pats e -> undefined
    Let pats e1 e2 -> undefined
