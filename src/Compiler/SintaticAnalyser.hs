module Compiler.SintaticAnalyser where

import Compiler.LexicalAnalyser ( startAutomaton )
import Compiler.TokensKlang
    ( TokensKlang(OpenBlockToken, LetToken, IfToken, RoutineToken,
                  CloseBlockToken, ShowToken, AssignToken, IdentifierToken,
                  IntegerToken, StringToken, PlusToken, MinusToken, MultToken,
                  DivisionToken, EqualityToken, NotEqlToken, LessToken, GreaterToken,
                  LessEqlToken, GreaterEqlToken, EmptyToken) )

startSintaticAnalysis [] _ _ 0 = []
startSintaticAnalysis program line col closingBlocks 
    | token == LetToken = 
        (token, value):_identifier remain nline ncol closingBlocks
    | token == IfToken = 
        (token, value):_value remain nline ncol closingBlocks _comparative
    | token == RoutineToken = 
        (token, value):_value remain nline ncol closingBlocks _openblock
    | token == CloseBlockToken = 
        (token, value):startSintaticAnalysis remain nline ncol (closingBlocks-1)
    | token == ShowToken = 
        (token, value):_value remain nline ncol closingBlocks startSintaticAnalysis
    | token == EmptyToken = startSintaticAnalysis [] nline ncol closingBlocks
    | otherwise = error ("[startSintaticAnalysis] Unexpected " 
        ++ value ++ ". line:" ++ show line ++ " col:" ++ show col)
    where
        ((token, value), nline, ncol, remain) = 
            startAutomaton program line col []

_identifier program line col closingBlocks 
    | token == IdentifierToken = 
        (token, value):_assign remain nline ncol closingBlocks
    | otherwise = error ("[_identifier] Unexpected " 
        ++ value ++ " expecting identifier. line:" 
        ++ show line ++ " col:" ++ show col)
    where
        ((token, value), nline, ncol, remain) = 
            startAutomaton program line col []

_assign program line col closingBlocks 
    | token == AssignToken = 
        _value remain nline ncol closingBlocks startSintaticAnalysis
    | otherwise = error ("[_assign] Unexpected " 
    ++ value ++ " expecting assign symbol ':=' " 
    ++ show line ++ " col:" ++ show col)
    where
        ((token, value), nline, ncol, remain) = 
            startAutomaton program line col []

_value program line col closingBlocks fn  
    | token == IdentifierToken ||
      token == IntegerToken    ||
      token == StringToken     = 
        (token, value):_arithmetic remain nline ncol closingBlocks fn
    | otherwise = error ("[_value] Unexpected " 
    ++ value ++ " expecting valid type value [integer, string, identifier] " 
    ++ show line ++ " col:" ++ show col)
    where
        ((token, value), nline, ncol, remain) = 
            startAutomaton program line col []

_arithmetic program line col closingBlocks fn 
    | token == PlusToken  ||
      token == MinusToken ||
      token == MultToken  ||
      token == DivisionToken = 
          (token, value):_value remain nline ncol closingBlocks fn
    | otherwise = fn program line col closingBlocks
    where
        ((token, value), nline, ncol, remain) = 
            startAutomaton program line col []

_comparative program line col closingBlocks 
    | token == EqualityToken ||
      token == NotEqlToken   ||
      token == LessToken     ||
      token == GreaterToken  ||
      token == LessEqlToken  ||
      token == GreaterEqlToken = 
        (token, value):_value remain nline ncol closingBlocks _openblock
    | otherwise = error ("[_comparative] Unexpected " 
        ++ value ++ " expecting comparative symbol " 
        ++ show line ++ " col:" ++ show col)
    where
        ((token, value), nline, ncol, remain) = 
            startAutomaton program line col []

_openblock program line col closingBlocks 
    | token == OpenBlockToken = 
        (token, value):startSintaticAnalysis remain nline ncol (closingBlocks+1)
    | otherwise = error ("[_openblock] Unexpected " 
        ++ value ++ " expecting openBlock symbol ':' " 
        ++ show line ++ " col:" ++ show col)
    where
        ((token, value), nline, ncol, remain) = 
            startAutomaton program line col []
