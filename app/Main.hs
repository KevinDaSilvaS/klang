module Main where

import System.Environment ( getArgs )
import Compiler.StartCompiler ( startCompilation )
import Compiler.RunProject
        
main :: IO ()
main = do
    getAllTokens
    {- args <- getArgs
        
    startCompilation args -}