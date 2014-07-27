module Main where

import Prelude hiding (lex)
import System.Environment (getArgs)
import JMLTokens (lex)
import JML (parse)
import JMLCompiler (compile)
import GCC (assemble, writeAsm)

main :: IO ()
main =
  do [inFilename, outFilename] <- getArgs
     s <- readFile inFilename
     writeAsm outFilename ((assemble . compile . parse . lex) s)
