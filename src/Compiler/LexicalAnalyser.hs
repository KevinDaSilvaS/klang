module Compiler.LexicalAnalyser where

import Compiler.TokensKlang
import Compiler.KlangSets ( identifierSet, integers, lineBreaks )

startAutomaton ('l':xs) line col []   =
    letAutomaton xs line (col+1) "l"
startAutomaton ('s':xs) line col []   =
    showAutomaton xs line (col+1) "s"
startAutomaton ('i':xs) line col []   =
    ifAutomaton xs line (col+1) "i"
startAutomaton ('r':xs) line col []   =
    routineAutomaton xs line (col+1) "r"
startAutomaton ('+':xs) line col []   =
    ((PlusToken, "+"), line, col, xs)
startAutomaton ('-':xs) line col []   =
    ((MinusToken, "-"), line, col, xs)
startAutomaton ('*':xs) line col []   =
    ((MultToken, "*"), line, col, xs)
startAutomaton ('/':xs) line col []   =
    ((DivisionToken, "/"), line, col, xs)
startAutomaton (':':xs) line col []   =
    assignAutomaton xs line col ":"
startAutomaton (';':xs) line col []   =
    ((CloseBlockToken, ";"), line, col, xs)
startAutomaton ('\"':xs) line col []  =
    stringAutomaton xs line col "\""
startAutomaton ('!':xs) line col []   =
    notEqualityAutomaton xs line col "!"
startAutomaton ('=':xs) line col []   =
    equalityAutomaton xs line col "="
startAutomaton ('>':xs) line col []   =
    greaterThanAutomaton xs line col ">"
startAutomaton ('<':xs) line col []   =
    lessThanAutomaton xs line col "<"
startAutomaton ('_':xs) line col []   =
    identifierAutomaton xs line col "_"
startAutomaton [] line col []         =
    ((EmptyToken, ""), line, col, [])
startAutomaton (x:xs) line col []
    | x `elem` lineBreaks || x == ' ' =
        startAutomaton xs (line+1) col []
    | x `elem` integers               =
        numberAutomaton xs line (col+1) [x]
startAutomaton _ line col reading     =
    error ("[startAutomaton] Unexpected token in line:"
            ++ show line ++ " col:"
            ++ show col ++ " while reading: " ++ reading)

letAutomaton ('e':xs) line col "l"  = letAutomaton xs line (col+1) "le"
letAutomaton ('t':xs) line col "le" = letAutomaton xs line (col+1) "let"
letAutomaton [] line col "let" =
    ((LetToken, "let"), line, col, [])
letAutomaton (' ':xs) line col "let" =
    ((LetToken, "let"), line, (col+1), xs)
letAutomaton xs line col reading = 
    error ("[letAutomaton] Unexpected token in line:"
        ++ show line ++ " col:" ++ show col)

identifierAutomaton (x:xs) line col reading
    | x `elem` identifierSet =
        identifierAutomaton xs line (col+1) (reading++[x])
identifierAutomaton (x:xs) line col [] =
    error ("[identifierAutomaton] Unexpected token "
        ++ [x] ++ " in line:"
        ++ show line ++ " col:" ++ show col)
identifierAutomaton xs line col reading =
    ((IdentifierToken, reading), line, col, xs)

assignAutomaton ('=':xs) line col ":" =
    ((AssignToken, ":="), line, col+1, xs)
assignAutomaton xs line col ":"       =
    ((OpenBlockToken, ":"), line, col+1, xs)
assignAutomaton xs line col reading   =
    error ("[assignAutomaton] Unexpected token in line:"
        ++ show line ++ " col:"
        ++ show col ++ " while reading: " ++ reading)

stringAutomaton ('\"':xs) line col reading =
    ((StringToken, reading ++ "\""), line, col+1, xs)
stringAutomaton (x:xs) line col reading    =
    stringAutomaton xs line (col+1) (reading++[x])
stringAutomaton xs line col reading    =
    error ("[stringAutomaton] Unexpected eof in line:"
    ++ show line ++ " col:"
    ++ show col ++ " while reading: " ++ reading)
numberAutomaton (x:xs) line col reading
    | x `elem` integers = numberAutomaton xs line (col+1) (reading++[x])
numberAutomaton xs line col reading     =
    ((IntegerToken, reading), line, col, xs)

ifAutomaton ('f':xs) line col "i"  = ifAutomaton xs line (col+1) "if"
ifAutomaton [] line col "if" =
    ((IfToken, "if"), line, col, [])
ifAutomaton (' ':xs) line col "if" =
    ((IfToken, "if"), line, col+1, xs)
ifAutomaton xs line col reading =
    error ("[ifAutomaton] Unexpected token in line:"
        ++ show line ++ " col:" ++ show col)

routineAutomaton ('o':xs) line col "r"       = 
    routineAutomaton xs line (col+1) "ro"
routineAutomaton ('u':xs) line col "ro"      = 
    routineAutomaton xs line (col+1) "rou"
routineAutomaton ('t':xs) line col "rou"     = 
    routineAutomaton xs line (col+1) "rout"
routineAutomaton ('i':xs) line col "rout"    = 
        routineAutomaton xs line (col+1) "routi"
routineAutomaton ('n':xs) line col "routi"   = 
    routineAutomaton xs line (col+1) "routin"
routineAutomaton ('e':xs) line col "routin"  = 
    routineAutomaton xs line (col+1) "routine"
routineAutomaton [] line col "routine"       =
    ((RoutineToken, "routine"), line, col, [])
routineAutomaton (' ':xs) line col "routine" =
    ((RoutineToken, "routine"), line, col+1, xs)
routineAutomaton [] line col "routine"       =
    ((RoutineToken, "routine"), line, col, [])
routineAutomaton (' ':xs) line col "routine" =
    ((RoutineToken, "routine"), line, col+1, xs)
routineAutomaton xs line col reading =
    error ("[routineAutomaton] Unexpected token " ++ xs ++ " in line:"
        ++ show line ++ " col:" ++ show col)

notEqualityAutomaton ('=':xs) line col "!" =
    ((NotEqlToken, "!="), line, col+1, xs)
notEqualityAutomaton _ line col reading =
    error ("[notEqualityAutomaton] Unexpected token in line:"
    ++ show line ++ " col:"
    ++ show col ++ " while reading: " ++ reading)

equalityAutomaton ('=':xs) line col "=" =
    ((EqualityToken, "=="), line, col+1, xs)
equalityAutomaton _ line col reading =
    error ("[equalityAutomaton] Unexpected token in line:"
    ++ show line ++ " col:"
    ++ show col ++ " while reading: " ++ reading)

lessThanAutomaton ('=':xs) line col "<" =
    ((LessEqlToken, "<="), line, col+1, xs)
lessThanAutomaton xs line col "<" =
    ((LessToken, "<"), line, col+1, xs)
lessThanAutomaton _ line col reading =
    error ("[lessThanAutomaton] Unexpected token in line:"
    ++ show line ++ " col:"
    ++ show col ++ " while reading: " ++ reading)

greaterThanAutomaton ('=':xs) line col ">" =
    ((GreaterEqlToken, ">="), line, col+1, xs)
greaterThanAutomaton xs line col ">" =
    ((GreaterToken, ">"), line, col+1, xs)
greaterThanAutomaton _ line col reading =
    error ("[greaterThanAutomaton] Unexpected token in line:"
    ++ show line ++ " col:"
    ++ show col ++ " while reading: " ++ reading)

showAutomaton ('h':xs) line col "s"   = showAutomaton xs line (col+1) "sh"
showAutomaton ('o':xs) line col "sh"  = showAutomaton xs line (col+1) "sho"
showAutomaton ('w':xs) line col "sho" = showAutomaton xs line (col+1) "show"
showAutomaton [] line col "show" =
    ((ShowToken, "show"), line, col, [])
showAutomaton (' ':xs) line col "show" =
    ((ShowToken, "show"), line, col, xs)
showAutomaton xs line col reading =
    error ("[showAutomaton] Unexpected token in line:"
        ++ show line ++ " col:" ++ show col)