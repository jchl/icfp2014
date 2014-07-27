module Main where

import System.Environment
import GCC

main :: IO ()
main =
  do [inFilename, outFilename] <- getArgs
     asm <- readAsmWithLabels inFilename
     writeAsm outFilename (assemble asm)
