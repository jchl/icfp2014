module Main where

import JML
import JMLTokens
import JMLCompiler
import GCC
import Types
import Control.Monad
import System.Environment

main :: IO ()
main =
  do [inFilename, outFilename] <- getArgs
     s <- readFile inFilename
     let p = (parse . jmlLex) s
     print p
     writeAsmWithLabels outFilename (({- assemble . -} compileJml) p)
