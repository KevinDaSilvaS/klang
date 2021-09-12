module Compiler.SemanticAnalyser where

import Compiler.ParseTree
    ( Expr(Value, Arithmetic, EndExpr, Comparative),
      ParsingTree(EndNode, Assign, If, Routine, Show, CloseBlock) )
import Compiler.TokensKlang
    ( TokensKlang(IntegerToken, EmptyToken, IdentifierToken,
                  StringToken) )
import Compiler.SymbolTableKlang
    ( dropScope, identifierNotAlreadyDefined, getIdentifier )

startSemanticAnalysis symbolTable ((Assign identifier value):xs)
    | identifierNotAlreadyDefined symbolTable identifier == True =
        startSemanticAnalysis nSymbolTable xs
    where
        nSymbolTable =
            (snd identifier, resolveExpr symbolTable value)
            :symbolTable
startSemanticAnalysis symbolTable ((If expr _):xs) 
    | fst (resolveExpr symbolTable expr) /= EmptyToken = 
        startSemanticAnalysis nSymbolTable xs
    where
        nSymbolTable =
            ("$end_scope", (EmptyToken, ""))
            :symbolTable
startSemanticAnalysis symbolTable ((Routine expr _):xs) 
    | fst (resolveExpr symbolTable expr) == IntegerToken = 
        startSemanticAnalysis nSymbolTable xs
    | otherwise = error "routine only accepts an integer as iterator value"
    where
        nSymbolTable =
            ("$end_scope", (EmptyToken, ""))
            :symbolTable
startSemanticAnalysis symbolTable ((Show _ expr):xs) 
    | fst (resolveExpr symbolTable expr) /= EmptyToken = 
        startSemanticAnalysis symbolTable xs
startSemanticAnalysis symbolTable ((CloseBlock _):xs) = 
    startSemanticAnalysis nSymbolTable xs
    where
        nSymbolTable = dropScope symbolTable
startSemanticAnalysis symbolTable [EndNode] = symbolTable
startSemanticAnalysis st xs = error $ "Unexpected tree node in: " ++ show xs

resolveExpr st (Comparative expr _ expr') 
    | fst (resolveExpr st expr) == fst (resolveExpr st expr') = (StringToken, "value")
    | otherwise = error "Comparative expression must contain two values of same type"
resolveExpr st (Value (IdentifierToken, value) expr)
    | token == IntegerToken = (token, identifierValue ++ 
        value ++ parseArithmeticExpr st expr)
    | token == StringToken  = (token, identifierValue)
    where
        (token, identifierValue) = getIdentifier st (IdentifierToken, value)
resolveExpr st (Value (IntegerToken, value) EndExpr) = (IntegerToken, value)
resolveExpr st (Value (IntegerToken, value) expr)    = (IntegerToken, 
    value ++ parseArithmeticExpr st expr)
resolveExpr st (Value (StringToken, value) EndExpr)  = (StringToken, value)
resolveExpr st (Value (StringToken, value) expr) =
    error "String cant have additional expressions"
resolveExpr _ expr =
    error ("Valid value expected. Expression given: " ++ show expr)

parseArithmeticExpr st (Arithmetic (_, "/") (Value value expr))
    | parsedToken == IntegerToken && parsedValueToFloat /= 0 = 
        " / " ++ snd (resolveExpr st (Value value expr))
    | otherwise = error 
    ("Division arithmetic expression only accepts integer values different than 0. instead received: " 
    ++ parsedValue)
    where
        (parsedToken, parsedValue) = resolveExpr st (Value value EndExpr)
        parsedValueToFloat = read parsedValue :: Float
parseArithmeticExpr st (Arithmetic operator (Value value expr))
    | parsedToken == IntegerToken = 
        snd operator ++ snd (resolveExpr st (Value value expr))
    | otherwise = error "Only integers can be used in arithmetic expressions"
    where
        (parsedToken, parsedValue) = resolveExpr st (Value value EndExpr)
parseArithmeticExpr st (Comparative (Value value' expr') operator (Value value expr)) 
    | parsedTokenValue == parsedTokenValue' = ""
    | otherwise = 
        error "Can only compare expressions with same type. Int == Int or String == String"
    where
        (parsedTokenValue, _)  = resolveExpr st (Value value expr)
        (parsedTokenValue', _) = resolveExpr st (Value value' expr')
parseArithmeticExpr _ EndExpr = ""
parseArithmeticExpr _ expr = error "Only arithmetic expression, integer or string expected. Received: " 
    ++ show expr
