module Main where

import GCC
import GCCSim
import System.Environment

main :: IO ()
main =
  do [filename] <- getArgs
     asm <- readAsmWithLabels filename
     let code = assemble asm
     state <- simpleSim code
     print state
