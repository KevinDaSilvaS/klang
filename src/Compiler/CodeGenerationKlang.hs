module Compiler.CodeGenerationKlang where

import Compiler.ParseTree
import Compiler.TokensKlang ( TokensKlang(IdentifierToken) )
import System.IO

makeFile ir path = do
    writeFile (path++".js") (startCodeGen ir)
    return ()

startCodeGen ((Assign (_, identifier) expr):xs) = const ++ startCodeGen xs
    where
        const = "const " ++ identifier ++ " = " ++ generateCodeFromExpr expr ++ " \n"
startCodeGen ((Show _ expr):xs) = consoleLog ++ startCodeGen xs
    where
        consoleLog = "console.log( " ++ generateCodeFromExpr expr ++ ") \n"
startCodeGen ((If expr _):xs) = _if ++ startCodeGen xs
    where
        _if = "if (" ++ generateCodeFromExpr expr ++ ") { \n"
startCodeGen ((Routine expr _):xs) = loop ++ startCodeGen xs
    where
        loop = "for (let _GET_CURR_INDEX = 0; _GET_CURR_INDEX < ( "
            ++ generateCodeFromExpr expr ++ "); _GET_CURR_INDEX++) {\n"
startCodeGen ((CloseBlock _):xs) = " \n}\n" ++ startCodeGen xs
startCodeGen [EndNode] = ""
startCodeGen _ = error "Malformed parseTree in code generation"   

generateCodeFromExpr (Value (IdentifierToken, "_readLine") expr) = valueVariable
    where
        valueVariable = " prompt(\"Please enter a valid string\") " 
            ++ generateCodeFromExpr expr
generateCodeFromExpr (Value (IdentifierToken, "_readNum") expr) = valueVariable
    where
        valueVariable = " parseInt(prompt(\"Please enter a valid integer\")) " 
            ++ generateCodeFromExpr expr
generateCodeFromExpr (Value (_, value) expr) = valueVariable
    where
        valueVariable = value ++ " " ++ generateCodeFromExpr expr
generateCodeFromExpr (Arithmetic (_, operator) expr) = arithmeticOperator
    where
        arithmeticOperator = operator ++ " " ++ generateCodeFromExpr expr
generateCodeFromExpr (Comparative expr (_, operator) expr') = 
    comparativeOperation
    where
        comparativeOperation = "( " ++ generateCodeFromExpr expr ++ ") " 
            ++ operator ++ " ( " ++ generateCodeFromExpr expr' ++ ")"
generateCodeFromExpr EndExpr = ""