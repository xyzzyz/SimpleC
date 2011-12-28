module CodeGenerator(AssemblyInstruction(..), 
                     Assembly, 
                     generateAssembly,
                     showAssembly) where
import Control.Monad
import Control.Monad.State

import Data.Maybe
import Data.Foldable(foldMap)
import qualified Data.Map as Map

import IR
import TypeChecker

data AssemblyType = AInt | AFloat | AVoid | AChar | AReference String

instance Show AssemblyType where
  show AInt = "LCInt;"
  show AFloat = "LCFloat;"
  show AVoid = "V"
  show AChar = "LCInt;"
  show (AReference s) = "L" ++ s ++ ";"

cTypeToAType CInt = AInt
cTypeToAType CFloat = AFloat
cTypeToAType CVoid = AVoid
cTypeToAType CChar = AChar
cTypeToAType (CTypedefType _ t) = cTypeToAType t
cTypeToAType (CPointerType t) = AReference "CPointer"

aTypeToRef AInt = "CInt"
aTypeToRef AFloat = "CFloat"
aTypeToRef AChar = "CChar"
aTypeToRef (AReference _) = "CPointer"

aTypeToInstrPrefix AInt = "i"
aTypeToInstrPrefix AFloat = "f"
aTypeToInstrPrefix AChar = "i"
aTypeToInstrPrefix (AReference _) = "a"

data AssemblyInstruction = AProgram String 
                         | AFunction AssemblyType String [AssemblyType]
                         | AEndFunction
                         | ALimitStack Int
                         | ALimitLocals Int
                         | ANew AssemblyType
                         | AIntPush Integer
                         | ADup
                         | AALoad Int
                         | AGetField AssemblyType String
                         | APutField AssemblyType String
                         | AAdd AssemblyType
                         | AReturn AssemblyType
                         | ALOL

instance Show AssemblyInstruction where
  show (AProgram c) = ".class public " ++ c ++ "\n"
                      ++ ".super java/lang/Object\n" 
                      ++ ".method public <init>()V\n"
                      ++ "aload_0\n"
                      ++ "invokenonvirtual java/lang/Object/<init>()V\n"
                      ++ "return\n"
                      ++ ".end method\n"

  show (AFunction t n as) = ".method public static " ++ n 
                         ++ "(" ++ concatMap show as ++ ")" ++ show t
  show AEndFunction = ".end method"
  show (ALimitStack n) = ".limit stack " ++ show n
  show (ALimitLocals n) = ".limit locals " ++ show n
  show (ANew t) = "new " ++ aTypeToRef t
  show (AIntPush n) = "ldc " ++ show n 
  show ADup = "dup"
  show (AALoad n) = "aload " ++ show n
  show (AGetField t n) = "getfield " ++ aTypeToRef t ++ "/" ++ n ++ " " ++ show t
  show (APutField t n) = "putfield " ++ aTypeToRef t ++ "/" ++ n ++ " " ++ show t
  show (AAdd t) = aTypeToInstrPrefix t ++ "add"
  show (AReturn AVoid) = "return"
  show (AReturn _) = "areturn"
  show ALOL = "LOL"

type Assembly = [AssemblyInstruction]

data CGEnv = CGEnv { assembly :: Assembly,
                     env :: Env,
                     nextLocalVar :: Int,
                     localVars :: Map.Map String Int}
             
getVarStore n = get >>= return . fromJust .  Map.lookup n . localVars

emit a = do
  e <- get
  put (e { assembly = a:(assembly e)})


countLocals (IRBlock ss) = foldr (+) 0 $ map countLocals ss
countLocals (IRLet l s) = (length l) + countLocals s
countLocals (IRIfElse _ t Nothing) = countLocals t
countLocals (IRIfElse _ t (Just e)) = countLocals t + countLocals e
countLocals (IRWhile _ b) = countLocals b
countLocals _ = 0

countStackExpr (IRStringLiteral _) = 1
countStackExpr (IRCharLiteral _) = 1
countStackExpr (IRIntLiteral _) = 1
countStackExpr (IRVariable _ _) = 1
countStackExpr (IRAssign _ lhs rhs) = countStackExpr rhs
countStackExpr (IRPostIncrement _ e) = 1 + countStackExpr e
countStackExpr (IRBinDot _ _ _) = undefined
countStackExpr (IRBinPlus _ e1 e2) = max (countStackExpr e1) (1 + countStackExpr e2)
countStackExpr _ = 666

countStackStmt (IRBlock ss) = foldr max 0 $ map countStackStmt ss
countStackStmt (IRExpressionStatement e) = countStackExpr e
countStackStmt (IRIfElse cond thn Nothing) = max (countStackExpr cond) (countStackStmt thn)
countStackStmt (IRIfElse cond thn (Just els)) = max (countStackExpr cond) (max (countStackStmt thn) (countStackStmt els))
countStackStmt (IRWhile cond body) = max (countStackExpr cond) (countStackStmt body)
countStackStmt (IRLet _ body) = countStackStmt body
countStackStmt IRSkip = 0
countStackStmt (IRReturn e) = countStackExpr e

generateExpr (IRIntLiteral n) = emit $ AIntPush n
generateExpr (IRVariable t name) = do
  loc <- getVarStore name
  emit $ AALoad loc
  emit $ AGetField (cTypeToAType t) "c"
  
generateExpr (IRAssign t e1 e2) = do
  let (IRVariable _ name) = e1
  loc <- getVarStore name
  emit $ AALoad loc
  generateExpr e2
  emit $ APutField (cTypeToAType t) "c"

generateExpr (IRBinPlus t e1 e2) = do
  generateExpr e1
  generateExpr e2
  emit $ AAdd (cTypeToAType t)
  
  
generateExpr _ = return ()

generateStmt (IRBlock ss) = mapM_ generateStmt ss
generateStmt (IRExpressionStatement e) = generateExpr e
generateStmt (IRReturn e) = do
  let t = cTypeOf e
      a = cTypeToAType t
  emit $ ANew a
  emit $ ADup
  generateExpr e
  emit $ APutField a "c"
  emit $ AReturn (cTypeToAType t)
  
generateStmt _ = return ()


pushArgsDef args = do
  e <- get
  let CGEnv { nextLocalVar = n, localVars = vs } = e
  put $ e { nextLocalVar = n + length args,
            localVars = foldr (uncurry Map.insert) vs (zip (map snd args) [n..]) }
  return e

popArgsDef oldE = do
  e <- get
  put $ e { nextLocalVar = nextLocalVar oldE, localVars = localVars oldE }

generateDef (IRFunctionDefinition t name args body) = do
  emit $ AFunction (cTypeToAType t) name (map (cTypeToAType . fst) args)
  emit $ ALimitLocals (countLocals (IRBlock body))
  emit $ ALimitStack (countStackStmt (IRBlock body))
  oldE <- pushArgsDef args
  mapM_ generateStmt body
  popArgsDef oldE
  emit $ AEndFunction


generateAssembly :: String -> IRTrlanslationUnit -> Env -> Assembly
generateAssembly name ir env = 
  let (_, s) = runState (mapM_ generateDef ir) (CGEnv { assembly = [AProgram name], env = env, nextLocalVar = 0, localVars = Map.empty})
  in reverse $ assembly s

showAssembly :: Assembly -> String
showAssembly = foldMap $ (++ "\n") . show 
            