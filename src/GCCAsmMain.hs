module Main where

import GCC
import System.Environment

main :: IO ()
main =
  do [inFilename, outFilename] <- getArgs
     asm <- readAsmWithLabels inFilename
     let code = assemble asm
     writeAsm outFilename code
