module Main where

import Text.ParserCombinators.Parsec
import Parser
import TypeChecker

main = getContents >>= 
       (\r -> 
         case parse cFile "<stdin>" r of
           Left err -> putStrLn .show $ err
           Right res -> case typeCheckTranslationUnit res of
             Left err -> putStrLn . show $ err
             Right res -> putStrLn "OK" >> putStrLn (show res))