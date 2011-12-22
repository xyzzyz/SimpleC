module Main where

import Text.ParserCombinators.Parsec
import Parser
main = getContents >>= 
       (\r -> 
         case parse cFile "<stdin>" r of
           Left err -> putStrLn .show $ err
           Right res -> mapM_ (putStrLn . show) $ res)