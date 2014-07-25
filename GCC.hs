module GCC where

import Text.Regex.Posix
import Data.Maybe
import Data.Char
import Control.Monad

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

type Int32 = Int
data Addr = AbsAddr Int | RefAddr String

instance Show Addr where
  show (AbsAddr n) = show n
  show (RefAddr label) = label

instance Read Addr where
  readsPrec _ value =
    let v = dropWhile isSpace value in
    let ds = takeWhile isDigit v in
    if null ds then
      [(RefAddr (takeWhile (not . isSpace) v), dropWhile (not . isSpace) v)]
    else
      [(AbsAddr (read ds), dropWhile isDigit v)]

data Instruction = LDC Int32 |
                   LD Int Int |
                   ST Int Int |
                   ADD | SUB | MUL | DIV | CEQ | CGT | CGTE |
                   ATOM |
                   CONS | CAR | CDR |
                   SEL Addr Addr |
                   JOIN |
                   LDF Addr |
                   AP Int |
                   RTN |
                   DUM Int |
                   RAP Int |
                   STOP |
                   TSEL Addr Addr |
                   TAP Int |
                   TRAP Int |
                   DBUG |
                   BRK

instance Show Instruction where
  show (LDC i) = "LDC  " ++ show i
  show (LD i j) = "LD   " ++ show i ++ " " ++ show j
  show (ST i j) = "ST   " ++ show i ++ " " ++ show j
  show (ADD) = "ADD"
  show (SUB) = "SUB"
  show (MUL) = "MUL"
  show (DIV) = "DIV"
  show (CEQ) = "CEQ"
  show (CGT) = "CGT"
  show (CGTE) = "CGTE"
  show (ATOM) = "ATOM"
  show (CONS) = "CONS"
  show (CAR) = "CAR"
  show (CDR) = "CDR"
  show (SEL a1 a2) = "SEL  " ++ show a1 ++ " " ++ show a2
  show (JOIN) = "JOIN"
  show (LDF a) = "LDF  " ++ show a
  show (AP n) = "AP   " ++ show n
  show (RTN) = "RTN"
  show (DUM n) = "DUM  " ++ show n
  show (RAP n) = "RAP  " ++ show n
  show (STOP) = "STOP"
  show (TSEL a1 a2) = "TSEL " ++ show a1 ++ " " ++ show a2
  show (TAP n) = "TAP  " ++ show n
  show (TRAP n) = "TRAP " ++ show n
  show (DBUG) = "DBUG"
  show (BRK) = "BRK"

parseInstruction :: String -> [String] -> Instruction
parseInstruction "LDC" [i] = LDC (read i)
parseInstruction "LD" [i, j] = LD (read i) (read j)
parseInstruction "ST" [i, j] = ST (read i) (read j)
parseInstruction "ADD" [] = ADD
parseInstruction "SUB" [] = SUB
parseInstruction "MUL" [] = MUL
parseInstruction "DIV" [] = DIV
parseInstruction "CEQ" [] = CEQ
parseInstruction "CGT" [] = CGT
parseInstruction "CGTE" [] = CGTE
parseInstruction "ATOM" [] = ATOM
parseInstruction "CONS" [] = CONS
parseInstruction "CAR" [] = CAR
parseInstruction "CDR" [] = CDR
parseInstruction "SEL" [a1, a2] = SEL (read a1) (read a2)
parseInstruction "JOIN" [] = JOIN
parseInstruction "LDF" [a] = LDF (read a)
parseInstruction "AP" [n] = AP (read n)
parseInstruction "RTN" [] = RTN
parseInstruction "DUM" [n] = DUM (read n)
parseInstruction "RAP" [n] = RAP (read n)
parseInstruction "STOP" [] = STOP
parseInstruction "TSEL" [a1, a2] = TSEL (read a1) (read a2)
parseInstruction "TAP" [n] = TAP (read n)
parseInstruction "TRAP" [n] = TRAP (read n)
parseInstruction "DBUG" [] = DBUG
parseInstruction "BRK" [] = BRK

data InstructionOrLabel = Instruction Instruction | Label String

instance Show InstructionOrLabel where
  show (Instruction i) = "  " ++ show i
  show (Label label) = label ++ ":"

type ProgramWithLabels = [InstructionOrLabel]
type Program = [Instruction]

lineToMaybeInstructionOrLabel :: String -> Maybe InstructionOrLabel
lineToMaybeInstructionOrLabel s =
  let ws = words s in
  case ws of
    [] -> Nothing
    w:ws ->
      if (last w == ':') && (ws == []) then
        Just $ Label $ init w
      else
        Just $ Instruction $ parseInstruction w ws

stripComment :: String -> String
stripComment s = s =~ "^[^;]*"

readAsmWithLabels :: FilePath -> IO ProgramWithLabels
readAsmWithLabels filename = liftM (catMaybes . map lineToMaybeInstructionOrLabel . map stripComment . lines) $ readFile filename

writeAsmWithLabels :: FilePath -> ProgramWithLabels -> IO ()
writeAsmWithLabels filename = writeFile filename . unlines . map show