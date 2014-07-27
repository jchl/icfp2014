module GCC where

import Data.Maybe
import Data.List
import Data.Char

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

type Int32 = Int
type Addr = Int
data AddrOrLabel = AbsAddr Addr | RefAddr String

instance Show AddrOrLabel where
  show (AbsAddr n) = show n
  show (RefAddr label) = label

instance Read AddrOrLabel where
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
                   SEL AddrOrLabel AddrOrLabel |
                   JOIN |
                   LDF AddrOrLabel |
                   AP Int |
                   RTN |
                   DUM Int |
                   RAP Int |
                   STOP |
                   TSEL AddrOrLabel AddrOrLabel |
                   TAP Int |
                   TRAP Int |
                   DBUG |
                   BRK

instance Show Instruction where
  show (LDC n) = "LDC  " ++ show n
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
parseInstruction "LDC" [n] = LDC (read n)
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
parseInstruction s args = error $ "bad instruction: " ++ s ++ " " ++ intercalate " " args

data InstructionOrLabel = Instruction Instruction | Label String

type Program = [Instruction]
type ProgramWithLabels = [InstructionOrLabel]

writeAsm :: FilePath -> Program -> IO ()
writeAsm filename = writeFile filename . unlines . map show

assemble :: ProgramWithLabels -> Program
assemble p = assemble' (buildSymtab 0 p) p
  where
    buildSymtab n p =
      case p of
        [] -> []
        (Instruction _):p' -> buildSymtab (n + 1) p'
        (Label label):p' -> (label, n) : buildSymtab n p'

    assemble' symtab p =
      case p of
        [] -> []
        (Instruction i):p' -> assembleInstruction symtab i : assemble' symtab p'
        (Label _):p' -> assemble' symtab p'

    assembleInstruction symtab i =
      case i of
        SEL a1 a2 -> SEL (lookupLabel symtab a1) (lookupLabel symtab a2)
        LDF a -> LDF (lookupLabel symtab a)
        TSEL a1 a2 -> TSEL (lookupLabel symtab a1) (lookupLabel symtab a2)
        _ -> i

    lookupLabel symtab a =
      case a of
        RefAddr label -> AbsAddr $ fromJust $ lookup label symtab
        AbsAddr _ -> a

-- The following are not currently used.
instance Show InstructionOrLabel where
  show (Instruction i) = "  " ++ show i
  show (Label label) = label ++ ":"

readAsmWithLabels :: FilePath -> IO ProgramWithLabels
readAsmWithLabels filename =
  do s <- readFile filename
     return $ (catMaybes . map lineToMaybeInstructionOrLabel . map stripComment . lines) s
  where
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
    stripComment = takeWhile (/= ';')

writeAsmWithLabels :: FilePath -> ProgramWithLabels -> IO ()
writeAsmWithLabels filename = writeFile filename . unlines . map show
