module Main where

import Text.ParserCombinators.Parsec
  
import Parser
import TypeChecker
import CodeGenerator

import System
import System.Process
import System.IO
import System.FilePath
import Control.Monad

main = do 
  args <- getArgs
  
  when (length args < 1) $ do
    putStrLn "no input file"
    exitFailure
    
  let fileName = args !! 0
      baseName = takeBaseName fileName
      outName  = replaceExtension fileName ".j"
      outDir   = dropFileName fileName
  putStrLn outName
  --exitWith ExitSuccess
  withFile fileName ReadMode $ \h -> do
    r <- hGetContents h
    case parse cFile fileName r of
      Left err -> putStrLn .show $ err
      Right res -> case typeCheckTranslationUnit res of
        Left err -> putStrLn . show $ err
        Right (ir, env) -> do
          let (as, structs) = generateAssembly baseName ir env
          writeFile outName $ showAssembly as
          compileFile outName
          mapM_ (compileStruct outDir) structs
          
compileFile filePath = system $ "jasmin " ++ filePath
      
structFilePath outDir name = joinPath [outDir, replaceExtension name ".j"]

compileStruct outDir (name, as) = do
  let structPath = structFilePath outDir name
  writeFile structPath $ showAssembly as
  compileFile structPath
          