{-# LANGUAGE FlexibleContexts #-}
module CodeGenerator(AssemblyInstruction(..), 
                     Assembly, 
                     generateAssembly,
                     showAssembly) where
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Maybe
import Data.Foldable(foldMap)
import qualified Data.Map as Map

import IR
import TypeChecker

data AssemblyType = AInt 
                  | AFloat 
                  | AVoid 
                  | AChar 
                  | AReference String 

instance Show AssemblyType where
  show AInt           = "LCInt;"
  show AFloat         = "LCFloat;"
  show AVoid          = "V"
  show AChar          = "LCInt;"
  show (AReference s) = "L" ++ s ++ ";"


cTypeToAType CInt               = AInt
cTypeToAType CFloat             = AFloat
cTypeToAType CVoid              = AVoid
cTypeToAType CChar              = AChar
cTypeToAType (CTypedefType _ t) = cTypeToAType t
cTypeToAType (CPointerType t)   = AReference "CPointer"


aTypeToRef AInt           = "CInt"
aTypeToRef AFloat         = "CFloat"
aTypeToRef AChar          = "CChar"
aTypeToRef (AReference _) = "CPointer"

aTypeToUnboxedType AInt   = "I"
aTypeToUnboxedType AFloat = "F"


aTypeToInstrPrefix AInt           = "i"
aTypeToInstrPrefix AFloat         = "f"
aTypeToInstrPrefix AChar          = "i"
aTypeToInstrPrefix (AReference _) = "a"

data BinRel = LT | GT | LE | GE | EQ | NE
            deriving Show

data AssemblyInstruction = AProgram String 
                         | AFunction AssemblyType String [AssemblyType]
                         | AEndFunction
                         | ALimitStack Int
                         | ALimitLocals Int
                         | ANew AssemblyType
                         | AIntPush Integer
                         | AConst AssemblyType Int
                         | ADup
                         | ASwap
                         | APop
                         | ALabel String
                         | AGoto String
                         | AALoad Int
                         | AAStore Int
                         | AGetField AssemblyType String
                         | APutField AssemblyType String
                         | AAdd AssemblyType
                         | ASub AssemblyType
                         | AMul AssemblyType
                         | ADiv AssemblyType
                         | AIfICmp BinRel String
                         | AIfCmp BinRel String
                         | AFcmpl 
                         | AInvoke AssemblyType String String [AssemblyType]
                         | AReturn AssemblyType
                         | ALOL

instance Show AssemblyInstruction where
  show (AProgram c)                      = ".class public " ++ c ++ "\n"
                                           ++ ".super java/lang/Object\n" 
                                           ++ ".method public <init>()V\n"
                                           ++ "aload_0\n"
                                           ++ "invokenonvirtual java/lang/Object/<init>()V\n"
                                           ++ "return\n"
                                           ++ ".end method\n"

  show (AFunction t n as)                = ".method public static " ++ n 
                                           ++ "(" ++ concatMap show as ++ ")" ++ show t
  show AEndFunction                      = ".end method"
  show (ALimitStack n)                   = ".limit stack " ++ show n
  show (ALimitLocals n)                  = ".limit locals " ++ show n
  show (ANew t)                          = "new " ++ aTypeToRef t ++ "\n"
                                           ++ "dup\n"
                                           ++ "invokenonvirtual " ++ aTypeToRef t ++ "/<init>()V"
  show (AIntPush n)                      = "ldc " ++ show n 
  show (AConst t n)                      = aTypeToInstrPrefix t ++ "const_" ++ show n
  show ADup                              = "dup"
  show ASwap                             = "swap"
  show APop                              = "pop"
  show (ALabel s)                        = s ++ ":"
  show (AGoto s)                         = "goto " ++ s
  show (AALoad n)                        = "aload " ++ show n
  show (AAStore n)                       = "astore " ++ show n
  show (AGetField t n)                   = "getfield " ++ aTypeToRef t ++ "/" ++ n ++ " " ++ aTypeToUnboxedType t
  show (APutField t n)                   = "putfield " ++ aTypeToRef t ++ "/" ++ n ++ " " ++ aTypeToUnboxedType t
  show (AAdd t)                          = aTypeToInstrPrefix t ++ "add"
  show (ASub t)                          = aTypeToInstrPrefix t ++ "sub"
  show (AMul t)                          = aTypeToInstrPrefix t ++ "mul"
  show (ADiv t)                          = aTypeToInstrPrefix t ++ "div"
  show (AIfICmp rel label)               = "if_icmp" ++ map toLower (show rel) ++ " " ++ label 
  show (AIfCmp rel label)                = "if" ++ map toLower (show rel) ++ " " ++ label 
  show (AInvoke ret progName fName args) = "invokestatic " ++ progName ++ "/" ++ fName 
                                           ++ "(" ++ concatMap show args ++ ")"
                                           ++ show ret
  show (AReturn AVoid)                   = "return"
  show (AReturn _)                       = "areturn"
  show ALOL                              = "LOL"

type Assembly = [AssemblyInstruction]

data CGEnv = CGEnv { progName     :: String,
                     assembly     :: Assembly,
                     env          :: Env,
                     nextLocalVar :: Int,
                     nextLabel    :: Int,
                     localVars    :: Map.Map String Int}

getVarStore :: MonadState CGEnv m => String -> m Int
getVarStore n = get >>= return . fromJust .  Map.lookup n . localVars
  
getNextLabel :: MonadState CGEnv m => String -> m String
getNextLabel name = do
  env <- get 
  let n = nextLabel env
  put $ env { nextLabel = n+1}
  return $ name ++ (show n)

emit a = do
  e <- get
  put (e { assembly = a:(assembly e)})


countLocals (IRBlock ss)            = foldr (+) 0 $ map countLocals ss
countLocals (IRLet l s)             = (length l) + countLocals s
countLocals (IRIfElse _ t Nothing)  = countLocals t
countLocals (IRIfElse _ t (Just e)) = countLocals t + countLocals e
countLocals (IRWhile _ b)           = countLocals b
countLocals _                       = 0

countStackExpr (IRStringLiteral _)   = 1
countStackExpr (IRCharLiteral _)     = 1
countStackExpr (IRIntLiteral _)      = 1
countStackExpr (IRVariable _ _)      = 1
countStackExpr (IRAssign _ lhs rhs)  = max (countStackExpr rhs) 3
--countStackExpr (IRPostIncrement _ e) = 1 + countStackExpr e
countStackExpr (IRBinDot _ _ _)      = undefined
countStackExpr (IRBinPlus _ e1 e2)   = max (countStackExpr e1) (1 + countStackExpr e2)
countStackExpr (IRBinMinus _ e1 e2)  = max (countStackExpr e1) (1 + countStackExpr e2)
countStackExpr (IRBinMul _ e1 e2)    = max (countStackExpr e1) (1 + countStackExpr e2)
countStackExpr (IRBinDiv _ e1 e2)    = max (countStackExpr e1) (1 + countStackExpr e2)
countStackExpr (IRCall _ _ es)       = foldr max 0 $ zipWith countAndAdd [0..] es
  where countAndAdd n e = n + countStackExpr e

countStackExpr _                     = 666

countStackStmt (IRBlock ss)                   = foldr max 0 $ map countStackStmt ss
countStackStmt (IRExpressionStatement e)      = countStackExpr e
countStackStmt (IRIfElse cond thn Nothing)    = max (countStackExpr cond) (countStackStmt thn)
countStackStmt (IRIfElse cond thn (Just els)) = max (countStackExpr cond) (max (countStackStmt thn) (countStackStmt els))
countStackStmt (IRWhile cond body)            = max (countStackExpr cond) (countStackStmt body)
countStackStmt (IRLet _ body)                 = countStackStmt body
countStackStmt IRSkip                         = 0
countStackStmt (IRReturn e)                   = 2 + countStackExpr e

generateExpr (IRIntLiteral n) = do 
  emit $ AIntPush n
  
generateExpr (IRVariable t name) = do
  loc <- getVarStore name
  emit $ AALoad loc
  emit $ AGetField (cTypeToAType t) "c"
  
generateExpr (IRAssign t e1 e2) = do
  let (IRVariable _ name) = e1
  loc <- getVarStore name
  emit $ AALoad loc
  emit $ ADup
  generateExpr e2
  emit $ APutField (cTypeToAType t) "c"
  emit $ AAStore loc

  
generateExpr (IRBinPlus  t e1 e2)   = generateBinOp AAdd t e1 e2
generateExpr (IRBinMinus t e1 e2)   = generateBinOp ASub t e1 e2
generateExpr (IRBinMul   t e1 e2)   = generateBinOp AMul t e1 e2
generateExpr (IRBinDiv   t e1 e2)   = generateBinOp ADiv t e1 e2                                  

generateExpr (IRUnPlus t e) = generateExpr e
generateExpr (IRUnMinus t e) = do
  let t' = cTypeToAType t
  emit $ AConst t' 0
  generateExpr e
  emit $ ASub t'

generateExpr (IRBinLessThan e1 e2)      = generateBinRel CodeGenerator.LT e1 e2
generateExpr (IRBinGreaterThan e1 e2)   = generateBinRel CodeGenerator.GT e1 e2
generateExpr (IRBinLessEquals e1 e2)    = generateBinRel CodeGenerator.LE e1 e2
generateExpr (IRBinGreaterEquals e1 e2) = generateBinRel CodeGenerator.GE e1 e2

generateExpr (IREquals e1 e2)    = generateBinRel CodeGenerator.EQ e1 e2
generateExpr (IRNotEquals e1 e2) = generateBinRel CodeGenerator.NE e1 e2


generateExpr (IRCall t n args) = do
  mapM_ generateBoxedExpr args
  env <- get
  let pName = progName env
  emit $ AInvoke (cTypeToAType t) pName n (map (cTypeToAType . cTypeOf) args)
  emit $ AGetField (cTypeToAType t) "c" 

generateExpr _ = return ()

generateBoxedExpr expr = do
  let t = cTypeToAType . cTypeOf $ expr
  emit $ ANew t
  emit $ ADup
  generateExpr expr
  emit $ APutField t "c"

generateBinRel rel e1 e2 = do
  generateExpr e1
  generateExpr e2
  generateRelOp rel (cTypeToAType . cTypeOf $ e1)
  where generateRelOp rel AInt = do
          thn <- getNextLabel "then"
          els <- getNextLabel "else"
          emit $ AIfICmp rel thn
          emit $ AIntPush 0
          emit $ AGoto els
          emit $ ALabel thn
          emit $ AIntPush 1
          emit $ ALabel els
        generateRelOp rel AFloat = do
          thn <- getNextLabel "then"
          els <- getNextLabel "else"
          emit $ AFcmpl
          emit $ AIfCmp rel thn 
          emit $ AIntPush 0
          emit $ AGoto els
          emit $ ALabel thn
          emit $ AIntPush 1
          emit $ ALabel els

generateBinOp ret t e1 e2 = do
  let t' = cTypeToAType t
  generateExpr e1
  generateExpr e2
  emit $ ret t'

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
  
generateStmt (IRIfElse cond thn els) = do
  generateExpr cond
  emit $ AIntPush 0
  elsL <- getNextLabel "else"
  emit $ AIfICmp CodeGenerator.EQ elsL
  generateStmt thn
  emit $ ALabel elsL
  maybeEmit els
  where maybeEmit Nothing = return ()
        maybeEmit (Just s) = generateStmt s

generateStmt (IRWhile cond body) = do
  exit <- getNextLabel "exit"
  loop <- getNextLabel "loop"
  emit $ ALabel loop 
  generateExpr cond
  emit $ AIntPush 0
  emit $ AIfICmp CodeGenerator.EQ exit
  generateStmt body
  emit $ AGoto loop
  emit $ ALabel exit
  

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
  emit $ ALimitLocals (length args + countLocals (IRBlock body))
  emit $ ALimitStack (countStackStmt (IRBlock body))
  oldE <- pushArgsDef args
  mapM_ generateStmt body
  popArgsDef oldE
  emit $ AEndFunction


generateAssembly :: String -> IRTrlanslationUnit -> Env -> Assembly
generateAssembly name ir env = 
  let (_, s) = runState (mapM_ generateDef ir) (CGEnv { progName = name, assembly = [AProgram name], env = env, nextLocalVar = 0, nextLabel = 0, localVars = Map.empty})
  in reverse $ assembly s

showAssembly :: Assembly -> String
showAssembly = foldMap $ (++ "\n") . show 
            