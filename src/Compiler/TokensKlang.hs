module Compiler.TokensKlang where

data TokensKlang = LetToken        |
                   AssignToken     |
                   IntegerToken    |
                   StringToken     |
                   IdentifierToken |
                   IfToken         |
                   RoutineToken    |
                   OpenBlockToken  |
                   CloseBlockToken |
                   PlusToken       |
                   MinusToken      |
                   MultToken       |
                   DivisionToken   |
                   GreaterToken    |
                   LessToken       |
                   GreaterEqlToken |
                   LessEqlToken    |
                   NotEqlToken     |
                   EqualityToken   |
                   ShowToken       |
                   FnToken         |
                   GuardToken      |
                   ReturnToken     |
                   EmptyToken      deriving (Show, Eq)
