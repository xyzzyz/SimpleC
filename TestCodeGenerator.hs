module Main where

import Text.ParserCombinators.Parsec
  
import Parser
import TypeChecker
import CodeGenerator

main = do 
  r <- getContents
  case parse cFile "<stdin>" r of
    Left err -> putStrLn .show $ err
    Right res -> case typeCheckTranslationUnit res of
      Left err -> putStrLn . show $ err
      Right (ir, env) -> do
        putStrLn "OK" 
        let as = generateAssembly ir env
        putStrLn . show $ as