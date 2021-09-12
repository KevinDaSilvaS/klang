module Compiler.SymbolTableKlang where

import Compiler.TokensKlang
    ( TokensKlang(IdentifierToken, IntegerToken, StringToken) )
import Data.Maybe ( fromJust, isJust ) 

startSymbolTable :: [([Char], (TokensKlang, [Char]))]
startSymbolTable = 
    [("_GET_CURR_INDEX", (IntegerToken, "1")), 
    ("_readNum",(IntegerToken, "1")), 
    ("_readLine",(StringToken, ""))]

dropScope :: [([Char], b)] -> [([Char], b)]
dropScope (("$end_scope", _):xs) = xs
dropScope (_:xs)                 = dropScope xs
dropScope []                     = []
    
identifierNotAlreadyDefined :: [([Char], a)] -> (TokensKlang, [Char]) -> Bool
identifierNotAlreadyDefined st (IdentifierToken, value)
    | isJust existentIdentifier =
        error ("Identifier " ++ value ++ " already defined.")
    | otherwise = True
        where
            existentIdentifier = lookup value st
identifierNotAlreadyDefined _ _ = error "Identifier expected"
getIdentifier :: [([Char], p)] -> (TokensKlang, [Char]) -> p
    
getIdentifier st (IdentifierToken, value)
    | isJust existentIdentifier = fromJust existentIdentifier
    | otherwise = error ("Identifier '" ++ value ++ "' not defined")
        where
            existentIdentifier = lookup value st
getIdentifier _ _ = error "Identifier expected"