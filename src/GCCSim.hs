module GCCSim where

import GCC

data DataValue = VInt Int32 |
                 VCons DataValue DataValue |
                 VClosure Addr EnvPtr
                 deriving Show

data ControlValue = VJoin Addr |
                    VRet Addr |
                    VEnv EnvPtr
                    deriving Show

isInt :: DataValue -> Bool
isInt (VInt _) = True
isInt _ = False

fromInt :: DataValue -> Int32
fromInt (VInt n) = n
fromInt _ = error $ "fromInt"

fromJoin :: ControlValue -> Addr
fromJoin (VJoin a) = a
fromJoin _ = error $ "fromJoin"

type EnvPtr = [Env]
data Env = Env [DataValue] |
           Dummy
           deriving Show

type CpuState = (Addr, [DataValue], [ControlValue], EnvPtr, Maybe (Maybe [DataValue]))

loadEnv :: Env -> Maybe (Maybe [DataValue]) -> Int -> DataValue
loadEnv (Env vs) edummy i = vs !! i
loadEnv Dummy (Just (Just vs)) i = vs !! i

sim :: Instruction -> CpuState -> Maybe CpuState
sim i =
  case i of
    LDC n -> \(c, s, d, e, ed) -> Just (c + 1, VInt n : s, d, e, ed)
    LD n i -> \(c, s, d, e, ed) -> Just (c + 1, loadEnv (e !! n) ed i: s, d, e, ed)
    ST n i -> \(c, s, d, e, ed) -> error $ "ST not yet implemented"
    ADD -> liftOp2 $ intop (+)
    SUB -> liftOp2 $ intop (-)
    MUL -> liftOp2 $ intop (*)
    DIV -> liftOp2 $ intop quot -- XXX check division
    CEQ -> liftOp2 $ intop (\x y -> if x == y then 1 else 0)
    CGT -> liftOp2 $ intop (\x y -> if x > y then 1 else 0)
    CGTE -> liftOp2 $ intop (\x y -> if x >= y then 1 else 0)
    ATOM -> liftOp1 (\x -> if isInt x then VInt 1 else VInt 0)
    CONS -> liftOp2 (\x y -> VCons x y)
    CAR -> liftOp1 (\(VCons x _) -> x)
    CDR -> liftOp1 (\(VCons _ y) -> y)
    SEL (AbsAddr a1) (AbsAddr a2) -> \(c, cond:s, d, e, ed) -> Just (if fromInt cond /= 0 then a1 else a2, s, VJoin (c + 1) : d, e, ed)
    JOIN -> \(c, s, (VJoin c'):d, e, ed) -> Just (c', s, d, e, ed)
    LDF (AbsAddr a) -> \(c, s, d, e, ed) -> Just (c + 1, VClosure a e : s, d, e, ed)
    AP n -> \(c, (VClosure f e'):s, d, e, ed) -> Just (f, drop n s, (VRet (c + 1)):(VEnv e):d, (Env (reverse $ take n s)):e', ed)
    RTN -> doReturn
      where
        doReturn (c, s, (VRet c'):(VEnv e'):d, e, ed) = Just (c', s, d, e', ed)
        doReturn (c, s, [], e, ed) = Nothing -- XXX too general!
    DUM n -> \(c, s, d, e, Nothing) -> Just (c + 1, s, d, Dummy:e, Just Nothing)
    RAP n -> \(c, (VClosure f (Dummy:e')):s, d, Dummy:e, Just Nothing) -> Just (f, drop n s, (VRet (c + 1)):(VEnv e):d, Dummy:e', Just $ Just (reverse $ take n s))
    STOP -> \(c, s, d, e, ed) -> error $ "RAP not yet implemented"
    TSEL (AbsAddr a1) (AbsAddr a2) -> \(c, cond:s, d, e, ed) -> Just (if fromInt cond /= 0 then a1 else a2, s, d, e, ed)
    TAP n -> \(c, s, d, e, ed) -> error $ "TAP not yet implemented"
    TRAP n -> \(c, s, d, e, ed) -> error $ "TRAP not yet implemented"
    DBUG -> \(c, v:s, d, e, ed) -> Just (c + 1, s, d, e, ed) -- XXX also print the value of v
    BRK -> \(c, s, d, e, ed) -> Just (c + 1, s, d, e, ed) -- XXX also drop into the debugger
  where
    liftOp1 :: (DataValue -> DataValue) -> CpuState -> Maybe CpuState
    liftOp1 op (c, x:s, d, e, ed) = Just (c + 1, op x : s, d, e, ed)

    liftOp2 :: (DataValue -> DataValue -> DataValue) -> CpuState -> Maybe CpuState
    liftOp2 op (c, y:x:s, d, e, ed) = Just (c + 1, op x y : s, d, e, ed)

    intop :: (Int32 -> Int32 -> Int32) -> (DataValue -> DataValue -> DataValue)
    intop op (VInt x) (VInt y) = VInt (op x y)

simulator :: [Instruction] -> CpuState -> IO CpuState
simulator code state@(c, _, _, _, _) =
  let insn = code !! c in
  do putStrLn $ show state
     putStrLn $ "  " ++ show insn
     case sim insn state of
       Nothing -> return state
       Just state' -> simulator code state'
