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
     writeAsmWithLabels outFilename (({- assemble . -} compileJml . parse . jmlLex) s)
