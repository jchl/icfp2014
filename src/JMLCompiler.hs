module JMLCompiler where

import Data.List
import Types
import GCC
import Control.Monad.State

listToPair :: [Expr] -> Expr
listToPair [] = Number 0
listToPair (e:es) = Pair e (listToPair es)

desugarLists :: Expr -> Expr
desugarLists e = case e of
  Number n -> Number n
  Boolean b -> Boolean b
  List es -> listToPair es
  Identifier id -> Identifier id
  Pair e1 e2 -> Pair (desugarLists e1) (desugarLists e2)
  Operator1 Null e -> Operator1 Atom (desugarLists e)
  Operator1 Head e -> Operator1 Fst (desugarLists e)
  Operator1 Tail e -> Operator1 Snd (desugarLists e)
  Operator2 ListCons e1 e2 -> Pair (desugarLists e1) (desugarLists e2)
  Operator2 op e1 e2 -> Operator2 op (desugarLists e1) (desugarLists e2)
  Trace e1 e2 -> Trace (desugarLists e1) (desugarLists e2)
  If e1 e2 e3 -> If (desugarLists e1) (desugarLists e2) (desugarLists e3)
  Fn pats e -> Fn pats (desugarLists e)
  App e es -> App (desugarLists e) (map desugarLists es)
  Let pat e1 e2 -> Let pat (desugarLists e1) (desugarLists e2)

elimBoolsExpr :: Expr -> Expr
elimBoolsExpr e = case e of
  Number n -> Number n
  Boolean True -> Number 1
  Boolean False -> Number 0
  List _ -> undefined -- There shouldn't be any lists left at this point
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

compileExpr :: String -> IExpr -> State Int ProgramWithLabels
compileExpr fnname e =
  case e of
    INumber n -> return [Instruction $ LDC n]
    IIdentifier i j -> return [Instruction $ LD i j]
    IPair e1 e2 ->
      do p1 <- compileExpr fnname e1
         p2 <- compileExpr fnname e2
         return $ p1 ++ p2 ++ [Instruction $ CONS]
    IOperator1 Not e -> undefined -- There shouldn't be any booleans left at this point
    IOperator1 Fst e ->
      do p <- compileExpr fnname e
         return $ p ++ [Instruction $ CAR]
    IOperator1 Snd e ->
      do p <- compileExpr fnname e
         return $ p ++ [Instruction $ CDR]
    IOperator1 Atom e ->
      do p <- compileExpr fnname e
         return $ p ++ [Instruction $ ATOM]
    IOperator1 Break e ->
      do p <- compileExpr fnname e
         return $ p ++ [Instruction $ BRK]
    IOperator1 Null e -> undefined -- There shouldn't be any lists left at this point
    IOperator1 Head e -> undefined -- There shouldn't be any lists left at this point
    IOperator1 Tail e -> undefined -- There shouldn't be any lists left at this point
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
        ListCons -> undefined -- There shouldn't be any lists left at this point
      where
        stack2Op insn =
          do p1 <- compileExpr fnname e1
             p2 <- compileExpr fnname e2
             return $ p1 ++ p2 ++ [Instruction $ insn]
        stack2OpRev insn =
          do p1 <- compileExpr fnname e2
             p2 <- compileExpr fnname e1
             return $ p1 ++ p2 ++ [Instruction $ insn]
    ITrace e1 e2 ->
      do p1 <- compileExpr fnname e1
         p2 <- compileExpr fnname e2
         return $ p1 ++ [Instruction $ DBUG] ++ p2
    IIf e1 e2 e3 ->
      do n <- get
         let lt = fnname ++ "_true" ++ show n
         let lf = fnname ++ "_false" ++ show n
         let le = fnname ++ "_end" ++ show n
         put (n + 1)
         p1 <- compileExpr fnname e1
         p2 <- compileExpr fnname e2
         p3 <- compileExpr fnname e3
         return $ p1 ++ [Instruction $ SEL (RefAddr lt) (RefAddr lf)] ++
           [Instruction $ LDC 0, Instruction $ TSEL (RefAddr le) (RefAddr le)] ++
           [Label lt] ++ p2 ++ [Instruction $ JOIN] ++
           [Label lf] ++ p3 ++ [Instruction $ JOIN] ++
           [Label le]
    IFn label ->
      let lf = fnname ++ "_fn" ++ show label in
      return [Instruction $ LDF (RefAddr lf)]
    IApp e es ->
      do ps <- liftM concat $ mapM (compileExpr fnname) es
         p <- compileExpr fnname e
         return $ ps ++ p ++ [Instruction $ AP (length es)]

compileFunctionBody :: String -> String -> IExpr -> ProgramWithLabels
compileFunctionBody fnname label ie =
  let (p, _) = runState (compileExpr fnname ie) 0 in
  [Label $ label] ++ p ++ [Instruction $ RTN]

compileFunDecl :: [[Identifier]] -> Identifier -> Expr -> ProgramWithLabels
compileFunDecl bindings id e =
  let (ie, labeledIes) = (flip extractFuncs [] . flip toDeBruin bindings . elimBoolsExpr . desugarLists) e in
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
