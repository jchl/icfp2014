module GCCSim where

import GCC
import Data.List
import System.IO

data Value = VInt Int32 |
             VCons Value Value |
             VClosure Addr EnvPtr |
             VJoin Addr |
             VRet Addr |
             VEnv EnvPtr
             deriving Show

-- XXX I believe: first 3 are on data stack, second 3 are on control stack
-- XXX better typing would be nice

isInt :: Value -> Bool
isInt (VInt _) = True
isInt _ = False

fromInt :: Value -> Int32
fromInt (VInt n) = n

fromJoin :: Value -> Addr
fromJoin (VJoin a) = a

type EnvPtr = [Env]
data Env = Env [Value] |
           Dummy
           deriving Show

type CpuState = (Addr, [Value], [Value], EnvPtr, Maybe (Maybe [Value]))

liftOp1 :: (Value -> Value) -> CpuState -> Maybe CpuState
liftOp1 op (c, x:s, d, e, ed) = Just (c + 1, op x : s, d, e, ed)

liftOp2 :: (Value -> Value -> Value) -> CpuState -> Maybe CpuState
liftOp2 op (c, y:x:s, d, e, ed) = Just (c + 1, op x y : s, d, e, ed)

intop :: (Int32 -> Int32 -> Int32) -> (Value -> Value -> Value)
intop op (VInt x) (VInt y) = VInt (op x y)

loadEnv :: Env -> Maybe (Maybe [Value]) -> Int -> Value
loadEnv (Env vs) edummy i = vs !! i
loadEnv Dummy (Just (Just vs)) i = vs !! i

sim :: Instruction -> CpuState -> Maybe CpuState
sim (LDC n) = \(c, s, d, e, ed) -> Just (c + 1, VInt n : s, d, e, ed)
sim (LD n i) = \(c, s, d, e, ed) -> Just (c + 1, loadEnv (e !! n) ed i: s, d, e, ed)
sim (ST n 1) = \(c, s, d, e, ed) -> undefined
sim (ADD) = liftOp2 $ intop (+)
sim (SUB) = liftOp2 $ intop (-)
sim (MUL) = liftOp2 $ intop (*)
sim (DIV) = liftOp2 $ intop quot -- XXX check division
sim (CEQ) = liftOp2 $ intop (\x y -> if x == y then 1 else 0)
sim (CGT) = liftOp2 $ intop (\x y -> if x > y then 1 else 0)
sim (CGTE) = liftOp2 $ intop (\x y -> if x >= y then 1 else 0)
sim (ATOM) = liftOp1 (\x -> if isInt x then VInt 1 else VInt 2)
sim (CONS) = liftOp2 (\x y -> VCons x y)
sim (CAR) = liftOp1 (\(VCons x _) -> x)
sim (CDR) = liftOp1 (\(VCons _ y) -> y)
sim (SEL (AbsAddr a1) (AbsAddr a2)) = \(c, cond:s, d, e, ed) -> Just (if fromInt cond /= 0 then a1 else a2, s, VJoin (c + 1) : d, e, ed)
sim (JOIN) = \(c, s, (VJoin c'):d, e, ed) -> Just (c', s, d, e, ed)
sim (LDF (AbsAddr a)) = \(c, s, d, e, ed) -> Just (c + 1, VClosure a e : s, d, e, ed)
sim (AP n) = \(c, (VClosure f e'):s, d, e, ed) -> Just (f, drop n s, (VRet (c + 1)):(VEnv e):d, (Env (reverse $ take n s)):e', ed)
sim (RTN) = doReturn
  where
    doReturn (c, s, (VRet c'):(VEnv e'):d, e, ed) = Just (c', s, d, e', ed)
    doReturn (c, s, [], e, ed) = Nothing -- XXX too general!
sim (DUM n) = \(c, s, d, e, Nothing) -> Just (c + 1, s, d, Dummy:e, Just Nothing)
sim (RAP n) = \(c, (VClosure f (Dummy:e')):s, d, Dummy:e, Just Nothing) -> Just (f, drop n s, (VRet (c + 1)):(VEnv e):d, Dummy:e', Just $ Just (reverse $ take n s))
sim (STOP) = \(c, s, d, e, ed) -> undefined
sim (TSEL (AbsAddr a1) (AbsAddr a2)) = \(c, cond:s, d, e, ed) -> Just (if fromInt cond /= 0 then a1 else a2, s, d, e, ed)
sim (TAP n) = \(c, s, d, e, ed) -> undefined
sim (TRAP n) = \(c, s, d, e, ed) -> undefined
sim (DBUG) = \(c, v:s, d, e, ed) -> Just (c + 1, s, d, e, ed) -- XXX also print the value of v
sim (BRK) = \(c, s, d, e, ed) -> Just (c + 1, s, d, e, ed) -- XXX also drop into the debugger

simulator :: [Instruction] -> CpuState -> IO CpuState
simulator code state@(c, _, _, _, _) =
  let insn = code !! c in
  do print state -- XXX debugging trace
     print insn
     hFlush stdout
     case sim insn state of
       Nothing -> return state
       Just state' -> simulator code state'

simpleSim :: [Instruction] -> IO CpuState
simpleSim code =
  do state' <- simulator code (0, [], [], [], Nothing)
     return $ state'
