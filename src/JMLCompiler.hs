module JMLCompiler where

import Data.List
import Types
import GCC

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
  Trace e1 e2 -> Trace (elimBoolsExpr e1) (elimBoolsExpr e2)
  If e1 e2 e3 -> If (elimBoolsExpr e1) (elimBoolsExpr e2) (elimBoolsExpr e3)
  Fn pats e -> Fn pats (elimBoolsExpr e)
  App e es -> App (elimBoolsExpr e) (map elimBoolsExpr es)
  Let pat e1 e2 -> Let pat (elimBoolsExpr e1) (elimBoolsExpr e2)

data BExpr = BNumber Int |
             BIdentifier Int Int |
             BPair BExpr BExpr |
             BOperator1 Op1 BExpr |
             BOperator2 Op2 BExpr BExpr |
             BTrace BExpr BExpr |
             BIf BExpr BExpr BExpr |
             BFn BExpr |
             BApp BExpr [BExpr]

toDeBruin :: Expr -> [[Identifier]] -> BExpr
toDeBruin e bindings =
  case e of
    Number n -> BNumber n
    Boolean b -> undefined -- There shouldn't be any booleans left at this point
    Identifier id ->
      let (i, j) = lookupIdent bindings id in
      BIdentifier i j
    Pair e1 e2 ->
      let be1 = toDeBruin e1 bindings in
      let be2 = toDeBruin e2 bindings in
      BPair be1 be2
    Operator1 op e1 ->
      let be1 = toDeBruin e1 bindings in
      BOperator1 op be1
    Operator2 op e1 e2 ->
      let be1 = toDeBruin e1 bindings in
      let be2 = toDeBruin e2 bindings in
      BOperator2 op be1 be2
    Trace e1 e2 ->
      let be1 = toDeBruin e1 bindings in
      let be2 = toDeBruin e2 bindings in
      BTrace be1 be2
    If e1 e2 e3 ->
      let be1 = toDeBruin e1 bindings in
      let be2 = toDeBruin e2 bindings in
      let be3 = toDeBruin e3 bindings in
      BIf be1 be2 be3
    Fn pats e1 ->
      let be1 = toDeBruin e1 (pats:bindings) in
      BFn be1
    App e1 es ->
      let be1 = toDeBruin e1 bindings in
      let bes = map (flip toDeBruin bindings) es in
      BApp be1 bes
    Let pat e1 e2 -> toDeBruin (App (Fn [pat] e2) [e1]) bindings
  where
    lookupIdent :: [[Identifier]] -> Identifier -> (Int, Int)
    lookupIdent = lookupIdent' 0
      where
        lookupIdent' i bindings id =
          case bindings of
            [] -> undefined
            (b:bs) ->
              case elemIndex id b of
                Nothing -> lookupIdent' (i + 1) bs id
                Just j -> (i, j)

data IExpr = INumber Int |
             IIdentifier Int Int |
             IPair IExpr IExpr |
             IOperator1 Op1 IExpr |
             IOperator2 Op2 IExpr IExpr |
             ITrace IExpr IExpr |
             IIf IExpr IExpr IExpr |
             IFn Int |
             IApp IExpr [IExpr]

extractFuncs :: BExpr -> [(Int, IExpr)] -> (IExpr, [(Int, IExpr)])
extractFuncs e ctx =
  case e of
    BNumber n -> (INumber n, ctx)
    BIdentifier i j -> (IIdentifier i j, ctx)
    BPair e1 e2 ->
      let (ie1, ctx1) = extractFuncs e1 ctx in
      let (ie2, ctx2) = extractFuncs e2 ctx1 in
      (IPair ie1 ie2, ctx2)
    BOperator1 op e1 ->
      let (ie1, ctx1) = extractFuncs e1 ctx in
      (IOperator1 op ie1, ctx1)
    BOperator2 op e1 e2 ->
      let (ie1, ctx1) = extractFuncs e1 ctx in
      let (ie2, ctx2) = extractFuncs e2 ctx1 in
      (IOperator2 op ie1 ie2, ctx2)
    BTrace e1 e2 ->
      let (ie1, ctx1) = extractFuncs e1 ctx in
      let (ie2, ctx2) = extractFuncs e2 ctx1 in
      (ITrace ie1 ie2, ctx2)
    BIf e1 e2 e3 ->
      let (ie1, ctx1) = extractFuncs e1 ctx in
      let (ie2, ctx2) = extractFuncs e2 ctx1 in
      let (ie3, ctx3) = extractFuncs e3 ctx2 in
      (IIf ie1 ie2 ie3, ctx3)
    BFn e1 ->
      let (ie1, ctx1) = extractFuncs e1 ctx in
      let label = allocLabel ctx1 in
      (IFn label, (label, ie1):ctx1)
    BApp e1 es ->
      let (ie1, ctx1) = extractFuncs e1 ctx in
      let (ies, ctx2) =
            foldr (\e (ies, ctx) -> let (ie', ctx') = extractFuncs e ctx in (ie':ies, ctx'))
                  ([], ctx1) es in
      (IApp ie1 ies, ctx2)
  where
    allocLabel :: [(Int, a)] -> Int
    allocLabel ctx = maximum (0 : map fst ctx) + 1

compileExpr :: String -> IExpr -> ProgramWithLabels
compileExpr fnname e =
  case e of
    INumber n -> [Instruction $ LDC n]
    IIdentifier i j -> [Instruction $ LD i j]
    IPair e1 e2 -> compileExpr fnname e1 ++ compileExpr fnname e2 ++ [Instruction $ CONS]
    IOperator1 Not e -> undefined -- There shouldn't be any booleans left at this point
    IOperator1 Fst e -> compileExpr fnname e ++ [Instruction $ CAR]
    IOperator1 Snd e -> compileExpr fnname e ++ [Instruction $ CDR]
    IOperator1 Break e -> compileExpr fnname e ++ [Instruction $ BRK]
    IOperator2 op e1 e2 ->
      case op of
        Plus -> stack2Op ADD
        Minus -> stack2Op SUB
        Times -> stack2Op MUL
        Divide -> stack2Op DIV
        Equals -> stack2Op CEQ
        NotEquals -> undefined -- There shouldn't be any != left at this point
        LessThan -> stack2OpRev CGT
        GreaterThan -> stack2Op CGT
        LessThanOrEquals -> stack2OpRev CGTE
        GreaterThanOrEquals -> stack2Op CGTE
        And -> undefined -- There shouldn't be any booleans left at this point
        Or -> undefined -- There shouldn't be any booleans left at this point
      where
        stack2Op insn = compileExpr fnname e1 ++ compileExpr fnname e2 ++ [Instruction $ insn]
        stack2OpRev insn = compileExpr fnname e2 ++ compileExpr fnname e1 ++ [Instruction $ insn]
    ITrace e1 e2 -> compileExpr fnname e1 ++ [Instruction $ DBUG] ++ compileExpr fnname e2
    IIf e1 e2 e3 ->
      let lt = "true" in -- XXX allocate fresh labels
      let lf = "false" in
      let le = "end" in
      compileExpr fnname e1 ++ [Instruction $ SEL (RefAddr lt) (RefAddr lf)] ++
        [Instruction $ LDC 0, Instruction $ TSEL (RefAddr le) (RefAddr le)] ++
        [Label lt] ++ compileExpr fnname e2 ++ [Instruction $ JOIN] ++
        [Label lf] ++ compileExpr fnname e3 ++ [Instruction $ JOIN] ++
        [Label le]
    IFn label ->
      let lf = fnname ++ "_fn" ++ show label in
      [Instruction $ LDF (RefAddr lf)]
    IApp e es -> concat (map (compileExpr fnname) es) ++ compileExpr fnname e ++ [Instruction $ AP (length es)]

compileFunctionBody :: String -> String -> IExpr -> ProgramWithLabels
compileFunctionBody fnname label ie = [Label $ label] ++ compileExpr fnname ie ++ [Instruction $ RTN]

compileFunDecl :: [[Identifier]] -> Identifier -> Expr -> ProgramWithLabels
compileFunDecl bindings id e =
  let (ie, labeledIes) = (flip extractFuncs [] . flip toDeBruin bindings . elimBoolsExpr) e in
  compileFunctionBody id id ie ++ concat (map (\(label, ie) -> compileFunctionBody id (id ++ "_fn" ++ show label) ie) labeledIes)

compileJml :: JmlProgram -> ProgramWithLabels
compileJml (ds, MainDecl mainPats e) = [Instruction $ DUM (length recFuncs)] ++
                                       map (\lf -> Instruction $ LDF (RefAddr lf)) recFuncs ++
                                       [Instruction $ LDF (RefAddr "main"),
                                        Instruction $ RAP (length recFuncs),
                                        Instruction $ RTN] ++
                                       compileFunDecl [recFuncs, mainPats] "main" e ++
                                       concat (map compileDecl ds)
  where
    recFuncs = gatherRecFuncs [] ds

    gatherRecFuncs fs [] = reverse fs
    gatherRecFuncs fs ((FunDecl id pats e):ds') = gatherRecFuncs (id:fs) ds'
    gatherRecFuncs fs (_:ds') = gatherRecFuncs fs ds'

    compileDecl (FunDecl id pats e) = compileFunDecl [pats, recFuncs, mainPats] id e
