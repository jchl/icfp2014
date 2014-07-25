module Main where

import GCC

main :: IO ()
main =
  do p <- readAsmWithLabels "test.gcc.in"
     print p
     writeAsmWithLabels "test.gcc.out" p
