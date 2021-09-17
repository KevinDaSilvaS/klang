module Compiler.RunProject where

import Compiler.LexicalAnalyser ( startAutomaton )
import Compiler.TokensKlang
import Compiler.SintaticAnalyser
import Compiler.ParseTree

import Compiler.SemanticAnalyser
import Compiler.CodeGenerationKlang
import Compiler.SymbolTableKlang

getAllTokens = do
    --let t = parseResult (startAutomaton "#let $a := 12 + * - / \"oi\"" 1 0 [])
    let t = getToken "fn _safeDiv v d | d == 0 \\-> d | _ \\-> v / d let a := 12 + * - / \"oi\" : ; >= if > < != == <= routine if_ t5" 1 0
    print t

getToken [] l c = []
getToken xs l c = (token, value):getToken program line col
    where
        ((token, value), line, col, program) =  startAutomaton xs line col []

sintaticAnalysis = do
    let program = "let a := 12 / 2 - readLine if a <= 12-2 : routine 3*2 : show 42 ; ;"
        --"let a := 12 + * - / \"oi\" : ; >= if > < != == <= routine if_ t5"
    let r = startSintaticAnalysis program 1 0 0
    print r

parseTree = do
    let program = "let a := 12 / 2 - readLine * 3 + 2 routine v : ; let r := \"oi\" if a+2 > c*2 : show a + 23 ;"
    let r = startSintaticAnalysis program 1 0 0
    let tree = createParseTree r
    print tree
semantic' = do
    let program = "let a := 12 / 2 if a+2 > 2 : ; routine 2*2 : let b := \"ab\" show a + 23 show _GET_CURR_INDEX + readNum ;"
    let r = startSintaticAnalysis program 1 0 0
    let tree = createParseTree r
    let sa = startSemanticAnalysis startSymbolTable tree
    print tree
    print sa
    makeFile tree ""
    