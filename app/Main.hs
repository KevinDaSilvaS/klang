module Main where

import System.Environment ( getArgs )
import Compiler.StartCompiler ( startCompilation )
        
main :: IO ()
main = do
    args <- getArgs
        
    startCompilation args