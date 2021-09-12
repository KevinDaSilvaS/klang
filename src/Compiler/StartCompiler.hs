module Compiler.StartCompiler where

import Control.Exception ( try, SomeException ) 

import Compiler.LexicalAnalyser ( startAutomaton )
import Compiler.SintaticAnalyser ( startSintaticAnalysis )
import Compiler.ParseTree ( createParseTree )
import Compiler.SymbolTableKlang ( startSymbolTable )
import Compiler.SemanticAnalyser ( startSemanticAnalysis )
import Compiler.CodeGenerationKlang ( makeFile )

import OutputFormatter.FormatMessage ( _error, _success )

startCompilation :: [FilePath] -> IO ()
startCompilation [path] = do
    let pathExtension = reverse $ take 6 (reverse path)
    if pathExtension /= ".klang" then do
        putStrLn $ 
            _error "Expected a valid path containing a file with .klang extension"
    else do
        contents <- try (readFile path) :: IO (Either SomeException [Char])
        case contents of
            Right file -> do
                parsingResult <- try (compile file path)
                    :: IO (Either SomeException ())
                case parsingResult of
                    Right file -> do return ()
                    Left e -> do 
                        putStrLn (_error $ show e)
            Left e -> do
                putStrLn $ _error "[File not found] Error file not found"
startCompilation _ = do
        putStrLn $ _error
            "[CLI] Error expecting a valid filepath"

compile program path = do
    let sintaticallyCorrectProgram = startSintaticAnalysis program 1 0 0
    let tree = createParseTree sintaticallyCorrectProgram
    putStrLn $ _success (startSemanticAnalysis startSymbolTable tree)
    let sanitizedPath = reverse (drop 6 (reverse path))
    makeFile tree sanitizedPath
    putStrLn $ _success "Klang compiled"