module Main where

import Text.ParserCombinators.Parsec
  
import Parser
import TypeChecker
import CodeGenerator

import System

main = do 
  args <- getArgs
  when (length args < 1) do
    putStrLn "no input file"
    exitFailure
  
  r <- getContents
  case parse cFile "<stdin>" r of
    Left err -> putStrLn .show $ err
    Right res -> case typeCheckTranslationUnit res of
      Left err -> putStrLn . show $ err
      Right (ir, env) -> do
        let (as, structs) = generateAssembly "Test" ir env
        putStrLn . showAssembly $ as