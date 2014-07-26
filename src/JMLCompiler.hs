module JMLCompiler where

import Data.List
import Types
import GCC
import Control.Monad.State

substConsts :: [ConstDecl] -> Expr -> Expr
substConsts consts e = case e of
  Number n -> Number n
  Boolean b -> Boolean b
  Tuple es -> Tuple (map (substConsts consts) es)
  List es -> List (map (substConsts consts) es)
  Identifier id ->
    case lookup id consts of
      Nothing -> Identifier id
      Just e -> e
  Pair e1 e2 -> Pair (substConsts consts e1) (substConsts consts e2)
  Operator1 op e -> Operator1 op (substConsts consts e)
  Operator2 op e1 e2 -> Operator2 op (substConsts consts e1) (substConsts consts e2)
  Trace e1 e2 -> Trace (substConsts consts e1) (substConsts consts e2)
  If e1 e2 e3 -> If (substConsts consts e1) (substConsts consts e2) (substConsts consts e3)
  Fn pats e -> Fn pats (substConsts consts e)
  App e es -> App (substConsts consts e) (map (substConsts consts) es)
  Let pat e1 e2 -> Let pat (substConsts consts e1) (substConsts consts e2)

listToPair :: [Expr] -> Expr
listToPair [] = Number 0
listToPair (e:es) = Pair e (listToPair es)

tupleToPair :: [Expr] -> Expr
tupleToPair [e1] = e1
tupleToPair (e:es) = Pair e (tupleToPair es)

desugarListsAndTuples :: Expr -> Expr
desugarListsAndTuples e = case e of
  Number n -> Number n
  Boolean b -> Boolean b
  Tuple es -> tupleToPair (map desugarListsAndTuples es)
  List es -> listToPair (map desugarListsAndTuples es)
  Identifier id -> Identifier id
  Pair e1 e2 -> Pair (desugarListsAndTuples e1) (desugarListsAndTuples e2)
  Operator1 Null e -> Operator1 Atom (desugarListsAndTuples e)
  Operator1 Head e -> Operator1 Fst (desugarListsAndTuples e)
  Operator1 Tail e -> Operator1 Snd (desugarListsAndTuples e)
  Operator1 op e -> Operator1 op (desugarListsAndTuples e)
  Operator2 ListCons e1 e2 -> Pair (desugarListsAndTuples e1) (desugarListsAndTuples e2)
  Operator2 op e1 e2 -> Operator2 op (desugarListsAndTuples e1) (desugarListsAndTuples e2)
  Trace e1 e2 -> Trace (desugarListsAndTuples e1) (desugarListsAndTuples e2)
  If e1 e2 e3 -> If (desugarListsAndTuples e1) (desugarListsAndTuples e2) (desugarListsAndTuples e3)
  Fn pats e -> Fn pats (desugarListsAndTuples e)
  App e es -> App (desugarListsAndTuples e) (map desugarListsAndTuples es)
  Let pat e1 e2 -> Let pat (desugarListsAndTuples e1) (desugarListsAndTuples e2)

elimBoolsExpr :: Expr -> Expr
elimBoolsExpr e = case e of
  Number n -> Number n
  Boolean True -> Number 1
  Boolean False -> Number 0
  Tuple _ -> error "unexpected tuple"
  List _ -> error "unexpected list"
  Identifier id -> Identifier id
  Pair e1 e2 -> Pair (elimBoolsExpr e1) (elimBoolsExpr e2)
  Operator1 Not e -> Operator2 Minus (Number 1) (elimBoolsExpr e)
  Operator1 op e -> Operator1 op (elimBoolsExpr e)
  Operator2 And e1 e2 -> If (elimBoolsExpr e1) (elimBoolsExpr e2) (Number 0)
  Operator2 Or e1 e2 -> If (elimBoolsExpr e1) (Number 1) (elimBoolsExpr e2)
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
    Boolean b -> error "unexpected boolean"
    Identifier id ->
      let (i, j) = lookupIdent bindings id in
      BIdentifier i j
    Tuple _ -> error "unexpected tuple"
    List _ -> error "unexpected list"
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
    Let (IdPat id) e1 e2 -> toDeBruin (App (Fn [id] e2) [e1]) bindings
    Let (TuplePat ids) e1 e2 -> toDeBruin (App (Fn ids e2) (splitTuple (length ids) e1)) bindings
  where
    lookupIdent :: [[Identifier]] -> Identifier -> (Int, Int)
    lookupIdent = lookupIdent' 0
      where
        lookupIdent' i bindings id =
          case bindings of
            [] -> error $  "unbound identifier: " ++ id
            (b:bs) ->
              case elemIndex id b of
                Nothing -> lookupIdent' (i + 1) bs id
                Just j -> (i, j)

    splitTuple n e = map (flip tupleNth e) [0..(n - 2)] ++ [tupleNth' (n - 1) e]
    tupleNth n e = if n == 0 then Operator1 Fst e else tupleNth (n - 1) (Operator1 Snd e)
    tupleNth' n e = if n == 0 then e else tupleNth' (n - 1) (Operator1 Snd e)

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

compileExpr :: String -> String -> IExpr -> State Int ProgramWithLabels
compileExpr fnname innerFnname e =
  case e of
    INumber n -> return [Instruction $ LDC n]
    IIdentifier i j -> return [Instruction $ LD i j]
    IPair e1 e2 ->
      do p1 <- compileExpr fnname innerFnname e1
         p2 <- compileExpr fnname innerFnname e2
         return $ p1 ++ p2 ++ [Instruction $ CONS]
    IOperator1 Not e -> error "unexpected boolean (not)"
    IOperator1 Fst e ->
      do p <- compileExpr fnname innerFnname e
         return $ p ++ [Instruction $ CAR]
    IOperator1 Snd e ->
      do p <- compileExpr fnname innerFnname e
         return $ p ++ [Instruction $ CDR]
    IOperator1 Atom e ->
      do p <- compileExpr fnname innerFnname e
         return $ p ++ [Instruction $ ATOM]
    IOperator1 Break e ->
      do p <- compileExpr fnname innerFnname e
         return $ p ++ [Instruction $ BRK]
    IOperator1 Null e -> error "unexpected boolean (null)"
    IOperator1 Head e -> error "unexpected boolean (head)"
    IOperator1 Tail e -> error "unexpected boolean (tail)"
    IOperator2 op e1 e2 ->
      case op of
        Plus -> stack2Op ADD
        Minus -> stack2Op SUB
        Times -> stack2Op MUL
        Divide -> stack2Op DIV
        Equals -> stack2Op CEQ
        NotEquals -> error "unexpected boolean (!=)"
        LessThan -> stack2OpRev CGT
        GreaterThan -> stack2Op CGT
        LessThanOrEquals -> stack2OpRev CGTE
        GreaterThanOrEquals -> stack2Op CGTE
        And -> error "unexpected boolean (and)"
        Or -> error "unexpected boolean (or)"
        ListCons -> error "unexpected list (::)"
      where
        stack2Op insn =
          do p1 <- compileExpr fnname innerFnname e1
             p2 <- compileExpr fnname innerFnname e2
             return $ p1 ++ p2 ++ [Instruction $ insn]
        stack2OpRev insn =
          do p1 <- compileExpr fnname innerFnname e2
             p2 <- compileExpr fnname innerFnname e1
             return $ p1 ++ p2 ++ [Instruction $ insn]
    ITrace e1 e2 ->
      do p1 <- compileExpr fnname innerFnname e1
         p2 <- compileExpr fnname innerFnname e2
         return $ p1 ++ [Instruction $ DBUG] ++ p2
    IIf e1 e2 e3 ->
      do n <- get
         let lt = innerFnname ++ "_true" ++ show n
         let lf = innerFnname ++ "_false" ++ show n
         let le = innerFnname ++ "_end" ++ show n
         put (n + 1)
         p1 <- compileExpr fnname innerFnname e1
         p2 <- compileExpr fnname innerFnname e2
         p3 <- compileExpr fnname innerFnname e3
         return $ p1 ++ [Instruction $ SEL (RefAddr lt) (RefAddr lf)] ++
           [Instruction $ LDC 0, Instruction $ TSEL (RefAddr le) (RefAddr le)] ++
           [Label lt] ++ p2 ++ [Instruction $ JOIN] ++
           [Label lf] ++ p3 ++ [Instruction $ JOIN] ++
           [Label le]
    IFn label ->
      let lf = fnname ++ "_fn" ++ show label in
      return [Instruction $ LDF (RefAddr lf)]
    IApp e es ->
      do ps <- liftM concat $ mapM (compileExpr fnname innerFnname) es
         p <- compileExpr fnname innerFnname e
         return $ ps ++ p ++ [Instruction $ AP (length es)]

compileFunctionBody :: String -> String -> IExpr -> ProgramWithLabels
compileFunctionBody fnname label ie =
  let (p, _) = runState (compileExpr fnname label ie) 0 in
  let n = 0 in -- XXX
  [Label $ label] ++
--  [Instruction $ LDC n, Instruction $ DBUG] ++ -- XXX
  p ++ [Instruction $ RTN]

compileFunDecl :: [ConstDecl] -> [[Identifier]] -> Identifier -> Expr -> ProgramWithLabels
compileFunDecl consts bindings id e =
  let (ie, labeledIes) = (flip extractFuncs [] . flip toDeBruin bindings . elimBoolsExpr . desugarListsAndTuples . substConsts consts) e in
  compileFunctionBody id id ie ++ concat (map (\(label, ie) -> compileFunctionBody id (id ++ "_fn" ++ show label) ie) labeledIes)

compileJml :: JmlProgram -> ProgramWithLabels
compileJml (consts, ds, MainDecl mainPats e) = [Instruction $ DUM (length recFuncs)] ++
                                               map (\lf -> Instruction $ LDF (RefAddr lf)) recFuncs ++
                                               [Instruction $ LDF (RefAddr "main"),
                                                Instruction $ RAP (length recFuncs),
                                                Instruction $ RTN] ++
                                               compileFunDecl consts [recFuncs, mainPats] "main" e ++
                                               concat (map compileDecl ds)
  where
    recFuncs = gatherRecFuncs [] ds

    gatherRecFuncs fs [] = reverse fs
    gatherRecFuncs fs ((FunDecl id pats e):ds') = gatherRecFuncs (id:fs) ds'
    gatherRecFuncs fs (_:ds') = gatherRecFuncs fs ds'

    compileDecl (FunDecl id pats e) = compileFunDecl consts [pats, recFuncs, mainPats] id e
