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

--data EnvPtr = ENull | EPtr EnvPtr [Value]
type EnvPtr = [[Value]]

type CpuState = (Addr, [Value], [Value], EnvPtr)

liftOp1 :: (Value -> Value) -> CpuState -> Maybe CpuState
liftOp1 op (c, x:s, d, e) = Just (c + 1, op x : s, d, e)

liftOp2 :: (Value -> Value -> Value) -> CpuState -> Maybe CpuState
liftOp2 op (c, y:x:s, d, e) = Just (c + 1, op x y : s, d, e)

intop :: (Int32 -> Int32 -> Int32) -> (Value -> Value -> Value)
intop op (VInt x) (VInt y) = VInt (op x y)

sim :: Instruction -> CpuState -> Maybe CpuState
sim (LDC n) = \(c, s, d, e) -> Just (c + 1, VInt n : s, d, e)
sim (LD n i) = \(c, s, d, e) -> Just (c + 1, (e !! n) !! i: s, d, e)
sim (ST n 1) = \(c, s, d, e) -> undefined
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
sim (SEL (AbsAddr a1) (AbsAddr a2)) = \(c, s, d, e) -> Just (if fromInt (head s) /= 0 then a1 else a2, s, VJoin (c + 1) : d, e)
sim (JOIN) = \(c, s, (VJoin c'):d, e) -> Just (c', s, d, e)
sim (LDF (AbsAddr a)) = \(c, s, d, e) -> Just (c + 1, VClosure a e : s, d, e)
sim (AP n) = \(c, (VClosure f e'):s, d, e) -> Just (f, drop n s, (VRet (c + 1)):(VEnv e):d, (reverse $ take n s):e)
sim (RTN) = doReturn
  where
    doReturn (c, s, (VRet c'):(VEnv e'):d, e) = Just (c', s, d, e')
    doReturn (c, s, [], e) = Nothing -- XXX too general!
sim (DUM n) = \(c, s, d, e) -> Just (c + 1, s, d, (replicate n undefined):e)
sim (RAP n) = \(c, s, d, e) -> undefined
sim (STOP) = \(c, s, d, e) -> undefined
sim (TSEL (AbsAddr a1) (AbsAddr a2)) = \(c, s, d, e) -> Just (if fromInt (head s) /= 0 then a1 else a2, s, d, e)
sim (TAP n) = \(c, s, d, e) -> undefined
sim (TRAP n) = \(c, s, d, e) -> undefined
sim (DBUG) = \(c, v:s, d, e) -> Just (c + 1, s, d, e) -- XXX also print the value of v
sim (BRK) = \(c, s, d, e) -> Just (c + 1, s, d, e) -- XXX also drop into the debugger

simulator :: [Instruction] -> CpuState -> IO CpuState
simulator code state@(c, _, _, _) =
  let insn = code !! c in
  do print state -- XXX debugging trace
     print insn
     hFlush stdout
     case sim insn state of
       Nothing -> return state
       Just state' -> simulator code state'

simpleSim :: [Instruction] -> IO CpuState
simpleSim code =
  do state' <- simulator code (0, [], [], [])
     return $ state'
