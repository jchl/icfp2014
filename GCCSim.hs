module GCCSim where

import GCC
import Data.List

data Value = VInt Int32 |
             VCons Value Value |
             VClosure Addr EnvPtr |
             VJoin Addr

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

liftOp1 :: (Value -> Value) -> CpuState -> CpuState
liftOp1 op (c, x:s, d, e) = (c + 1, op x : s, d, e)

liftOp2 :: (Value -> Value -> Value) -> CpuState -> CpuState
liftOp2 op (c, y:x:s, d, e) = (c + 1, op x y : s, d, e)

intop :: (Int32 -> Int32 -> Int32) -> (Value -> Value -> Value)
intop op (VInt x) (VInt y) = VInt (op x y)

sim :: Instruction -> CpuState -> CpuState
sim (LDC n) = \(c, s, d, e) -> (c + 1, VInt n : s, d, e)
sim (LD n i) = \(c, s, d, e) -> (c + 1, (e !! n) !! i: s, d, e)
sim (ST n 1) = \(c, s, d, e) -> undefined
sim (ADD) = liftOp2 $ intop (+)
sim (SUB) = liftOp2 $ intop (-)
sim (MUL) = liftOp2 $ intop (*)
sim (DIV) = liftOp2 $ intop quot
sim (CEQ) = liftOp2 $ intop (\x y -> if x == y then 1 else 0)
sim (CGT) = liftOp2 $ intop (\x y -> if x > y then 1 else 0)
sim (CGTE) = liftOp2 $ intop (\x y -> if x >= y then 1 else 0)
sim (ATOM) = liftOp1 (\x -> if isInt x then VInt 1 else VInt 2)
sim (CONS) = liftOp2 (\x y -> VCons x y)
sim (CAR) = liftOp1 (\(VCons x _) -> x)
sim (CDR) = liftOp1 (\(VCons _ y) -> y)
sim (SEL (AbsAddr a1) (AbsAddr a2)) = \(c, s, d, e) -> (if fromInt (head s) /= 0 then a1 else a2, s, VJoin (c + 1) : d, e)
sim (JOIN) = \(c, s, d, e) -> (fromJoin $ head d, s, tail d, e)
sim (LDF (AbsAddr a)) = \(c, s, d, e) -> (c + 1, VClosure a e : s, d, e)
sim (AP n) = \(c, s, d, e) -> undefined
sim (RTN) = \(c, s, d, e) -> undefined
sim (DUM n) = \(c, s, d, e) -> undefined
sim (RAP n) = \(c, s, d, e) -> undefined
sim (STOP) = \(c, s, d, e) -> undefined
sim (TSEL (AbsAddr a1) (AbsAddr a2)) = \(c, s, d, e) -> undefined
sim (TAP n) = \(c, s, d, e) -> undefined
sim (TRAP n) = \(c, s, d, e) -> undefined
sim (DBUG) = \(c, s, d, e) -> undefined
sim (BRK) = \(c, s, d, e) -> undefined
