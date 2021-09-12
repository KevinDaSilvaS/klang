module Compiler.KlangSets where

identifierSet :: [Char]
identifierSet = ['A'..'z'] ++ ['_']
    
spaces :: [Char]
spaces = [' ']
    
lineBreaks :: [Char]
lineBreaks = ['\n', '\r']
    
endInput :: [Char]
endInput = spaces ++ lineBreaks
    
integers :: [Char]
integers = ['0'..'9']
    
floats = '.':integers
    
arithmeticOperators = ["+", "-", "/", "*"]
    
comparativeOperators = [">", ">=", "<", "<=", "!=", "=="]