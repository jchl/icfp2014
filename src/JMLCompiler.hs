module JMLCompiler (compile) where

import Data.List
import Types
import GCC
import Control.Monad.State

substConsts :: [ConstDecl] -> Expr -> Expr
substConsts consts e =
  case e of
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

desugarListsAndTuples :: Expr -> Expr
desugarListsAndTuples e =
  case e of
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
  where
    listToPair [] = Number 0
    listToPair (e:es) = Pair e (listToPair es)

    tupleToPair [e1] = e1
    tupleToPair (e:es) = Pair e (tupleToPair es)

desugarBools :: Expr -> Expr
desugarBools e =
  case e of
    Number n -> Number n
    Boolean True -> Number 1
    Boolean False -> Number 0
    Tuple _ -> error "unexpected tuple"
    List _ -> error "unexpected list"
    Identifier id -> Identifier id
    Pair e1 e2 -> Pair (desugarBools e1) (desugarBools e2)
    Operator1 Not e -> Operator2 Minus (Number 1) (desugarBools e)
    Operator1 op e -> Operator1 op (desugarBools e)
    Operator2 And e1 e2 -> If (desugarBools e1) (desugarBools e2) (Number 0)
    Operator2 Or e1 e2 -> If (desugarBools e1) (Number 1) (desugarBools e2)
    Operator2 NotEquals e1 e2 -> desugarBools (Operator1 Not (Operator2 Equals e1 e2))
    Operator2 op e1 e2 -> Operator2 op (desugarBools e1) (desugarBools e2)
    Trace e1 e2 -> Trace (desugarBools e1) (desugarBools e2)
    If e1 e2 e3 -> If (desugarBools e1) (desugarBools e2) (desugarBools e3)
    Fn pats e -> Fn pats (desugarBools e)
    App e es -> App (desugarBools e) (map desugarBools es)
    Let pat e1 e2 -> Let pat (desugarBools e1) (desugarBools e2)

data BExpr = BNumber Int |
             BIdentifier Int Int |
             BPair BExpr BExpr |
             BOperator1 Op1 BExpr |
             BOperator2 Op2 BExpr BExpr |
             BTrace BExpr BExpr |
             BIf BExpr BExpr BExpr |
             BFn BExpr |
             BApp BExpr [BExpr]

toDeBruin :: [[Identifier]] -> Expr -> BExpr
toDeBruin bindings e =
  case e of
    Number n -> BNumber n
    Boolean b -> error "unexpected boolean"
    Identifier id ->
      let (i, j) = lookupIdent bindings id in
      BIdentifier i j
    Tuple _ -> error "unexpected tuple"
    List _ -> error "unexpected list"
    Pair e1 e2 -> BPair (toDeBruin bindings e1) (toDeBruin bindings e2)
    Operator1 op e1 -> BOperator1 op (toDeBruin bindings e1)
    Operator2 op e1 e2 -> BOperator2 op (toDeBruin bindings e1) (toDeBruin bindings e2)
    Trace e1 e2 -> BTrace (toDeBruin bindings e1) (toDeBruin bindings e2)
    If e1 e2 e3 -> BIf (toDeBruin bindings e1) (toDeBruin bindings e2) (toDeBruin bindings e3)
    Fn pats e1 -> BFn (toDeBruin (pats:bindings) e1)
    App e1 es -> BApp (toDeBruin bindings e1) (map (toDeBruin bindings) es)
    Let (IdPat id) e1 e2 -> toDeBruin bindings (App (Fn [id] e2) [e1])
    Let (TuplePat ids) e1 e2 -> toDeBruin bindings (App (Fn ids e2) (splitTuple (length ids) e1))
  where
    lookupIdent :: [[Identifier]] -> Identifier -> (Int, Int)
    lookupIdent = lookupIdent' 0
      where
        lookupIdent' i bindings id =
          case bindings of
            [] -> error $ "unbound identifier: " ++ id
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

extractFuncs :: BExpr -> State [(Int, IExpr)] IExpr
extractFuncs e =
  case e of
    BNumber n -> return $ INumber n
    BIdentifier i j -> return $ IIdentifier i j
    BPair e1 e2 ->
      do ie1 <- extractFuncs e1
         ie2 <- extractFuncs e2
         return $ IPair ie1 ie2
    BOperator1 op e1 ->
      do ie1 <- extractFuncs e1
         return $ IOperator1 op ie1
    BOperator2 op e1 e2 ->
      do ie1 <- extractFuncs e1
         ie2 <- extractFuncs e2
         return $ IOperator2 op ie1 ie2
    BTrace e1 e2 ->
      do ie1 <- extractFuncs e1
         ie2 <- extractFuncs e2
         return $ ITrace ie1 ie2
    BIf e1 e2 e3 ->
      do ie1 <- extractFuncs e1
         ie2 <- extractFuncs e2
         ie3 <- extractFuncs e3
         return $ IIf ie1 ie2 ie3
    BFn e1 ->
      do ie1 <- extractFuncs e1
         label <- allocLabel ie1
         return $ IFn label
    BApp e1 es ->
      do ie1 <- extractFuncs e1
         ies <- mapM extractFuncs es
         return $ IApp ie1 ies
  where
    allocLabel :: IExpr -> State [(Int, IExpr)] Int
    allocLabel ie =
      do ctx <- get
         let label = maximum (0 : map fst ctx) + 1
         put $ (label, ie):ctx
         return label

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
         put (n + 1)
         let lt = innerFnname ++ "_true" ++ show n
         let lf = innerFnname ++ "_false" ++ show n
         let le = innerFnname ++ "_end" ++ show n
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
  [Label $ label] ++ p ++ [Instruction $ RTN]

compileDecl :: [ConstDecl] -> [[Identifier]] -> Identifier -> Expr -> ProgramWithLabels
compileDecl consts bindings id e =
  let be = (toDeBruin bindings . desugarBools . desugarListsAndTuples . substConsts consts) e in
  let (ie, labeledIes) = runState (extractFuncs be) [] in
  compileFunctionBody id id ie ++
    concat (map (\(label, ie) -> compileFunctionBody id (id ++ "_fn" ++ show label) ie) labeledIes)

compile :: JmlProgram -> ProgramWithLabels
compile (consts, funs, (mainPats, e)) = [Instruction $ DUM (length funNames)] ++
                                          map (\lf -> Instruction $ LDF (RefAddr lf)) funNames ++
                                          [Instruction $ LDF (RefAddr "main"),
                                           Instruction $ RAP (length funNames),
                                           Instruction $ RTN] ++
                                          compileDecl consts [funNames, mainPats] "main" e ++
                                          concat (map compileFunDecl funs)
  where
    funNames = map (\(id, _, _) -> id) funs
    compileFunDecl (id, pats, e) = compileDecl consts [pats, funNames, mainPats] id e
