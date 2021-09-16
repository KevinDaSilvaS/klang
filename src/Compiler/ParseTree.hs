module Compiler.ParseTree where

import Compiler.TokensKlang
    ( TokensKlang(CloseBlockToken, LetToken, ShowToken, IfToken,
                  RoutineToken) )
import Compiler.KlangSets
    ( arithmeticOperators, comparativeOperators )

data Expr = Value (TokensKlang, String) Expr |
            Arithmetic  (TokensKlang, String) Expr |
            Comparative Expr (TokensKlang, String) Expr |
            EndExpr deriving(Show, Eq)

data ParsingTree = Assign (TokensKlang, String) Expr |
                   If Expr (TokensKlang, String)     |
                   CloseBlock (TokensKlang, String)  |
                   Routine Expr
                        (TokensKlang, String)        |
                   Show (TokensKlang, String) Expr   |
                   EndNode deriving(Show, Eq)


createParseTree (x:xs)
     | fst x == LetToken  =
          Assign identifierName expr     : createParseTree remain
     | fst x == ShowToken = Show x expr' : createParseTree remain'
     | fst x == IfToken   = If expr' openBlock : createParseTree (tail remain')
     | fst x == RoutineToken = 
          Routine expr' openBlock : createParseTree (tail remain')
     | fst x == CloseBlockToken = CloseBlock x : createParseTree xs
     | otherwise = createParseTree xs
     where
          identifierName   = head xs
          (expr, remain)   = parseExpr (tail xs)
          (expr', remain') = parseExpr xs
          openBlock  = head remain'
createParseTree [] = [EndNode]

parseExpr []  = (EndExpr, [])
parseExpr [x] = (Value x EndExpr, [])
parseExpr (x:xs)
     | snd x `elem` arithmeticOperators =
          (Arithmetic x parsed, remain')
     | snd nextToken `elem` arithmeticOperators =
          (Value x parsed, remain') 
     | snd nextToken `elem` comparativeOperators =
          (Comparative (Value x EndExpr) nextToken expr, remain)
     | otherwise = (Value x EndExpr, xs)
     where
          nextToken = head xs
          (expr, remain)    = parseExpr (tail xs)
          (parsed, remain') = parseExpr xs
