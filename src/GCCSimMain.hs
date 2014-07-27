module Main where

import System.Environment
import GCC
import GCCSim

main :: IO ()
main =
  do [filename] <- getArgs
     asm <- readAsmWithLabels filename
     state <- simulator (assemble asm) (0, [], [], [], Nothing)
     print state
