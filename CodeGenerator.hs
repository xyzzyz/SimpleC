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
                  | AReference String 
                  | AJavaClass String
                  | AArray AssemblyType
                  deriving (Eq)

instance Show AssemblyType where
  show AInt           = "LCInt;"
  show AFloat         = "LCFloat;"
  show AVoid          = "V"
  show (AReference s) = "L" ++ s ++ ";"
  show (AJavaClass s) = "L" ++ s ++ ";"
  show (AArray t)     = "[" ++ show t


cTypeToAType CInt               = AInt
cTypeToAType CFloat             = AFloat
cTypeToAType CVoid              = AVoid
cTypeToAType CChar              = AInt
cTypeToAType (CTypedefType _ t) = cTypeToAType t
cTypeToAType (CPointerType t)   = AReference "CPointer"


aTypeToRef AInt           = "CInt"
aTypeToRef AFloat         = "CFloat"
aTypeToRef AVoid          = "CVoid"
aTypeToRef (AReference _) = "CPointer"

aTypeToUnboxedType AInt   = "I"
aTypeToUnboxedType AFloat = "F"
aTypeToUnboxedType (AReference _) = "[Ljava/lang/Object;"

aTypeToInstrPrefix AInt           = "i"
aTypeToInstrPrefix AFloat         = "f"
aTypeToInstrPrefix (AReference _) = "a"

data BinRel = LT | GT | LE | GE | EQ | NE
            deriving Show

data AssemblyInstruction = AProgram String 
                         | AConstructor
                         | AMain String
                         | AFunction AssemblyType String [AssemblyType]
                         | AEndFunction
                         | ALimitStack Int
                         | ALimitLocals Int
                         | ANew AssemblyType
                         | ANewArray AssemblyType
                         | AIntPush Integer
                         | AStringPush String
                         | AConst AssemblyType Int
                         | ADup
                         | ADupX1
                         | ASwap
                         | APop
                         | ANop
                         | ACheckCast AssemblyType
                         | ALabel String
                         | AGoto String
                         | AALoad Int
                         | AAStore Int
                         | AILoad Int
                         | AIStore Int
                         | AField AssemblyType String
                         | AGetField AssemblyType String
                         | APutField AssemblyType String
                         | AAALoad 
                         | AAAStore
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
  show AConstructor                      = ".method public <init>()V\n"
                                           ++ "aload_0\n"
                                           ++ "invokenonvirtual java/lang/Object/<init>()V\n"
  show (AMain progName)                  = ".method public static main([Ljava/lang/String;)V\n"
                                           ++ ".limit locals 1\n"
                                           ++ show (AInvoke AVoid progName "$main" []) ++ "\n"
                                           ++ "return\n"
                                           ++ ".end method\n\n"

  show (AFunction t n as)                = ".method public static " ++ n 
                                           ++ "(" ++ concatMap show as ++ ")" ++ show t
  show AEndFunction                      = ".end method"
  show (ALimitStack n)                   = ".limit stack " ++ show n
  show (ALimitLocals n)                  = ".limit locals " ++ show n
  show (ANew t)                          = "new " ++ aTypeToRef t ++ "\n"
                                           ++ "dup\n"
                                           ++ "invokenonvirtual " ++ aTypeToRef t ++ "/<init>()V"
  show (ANewArray t)                     = "anewarray " ++ aTypeToRef t
  show (AIntPush n)                      = "ldc " ++ show n 
  show (AStringPush str)                 = "ldc \"" ++ str ++ "\""
  show (AConst t n)                      = aTypeToInstrPrefix t ++ "const_" ++ show n
  show ADup                              = "dup"
  show ADupX1                            = "dup_x1"
  show ASwap                             = "swap"
  show APop                              = "pop"
  show ANop                              = "nop"
  show (ACheckCast t)                    = "checkcast " ++ aTypeToRef t
  show (ALabel s)                        = s ++ ":"
  show (AGoto s)                         = "goto " ++ s
  show (AALoad n)                        = "aload " ++ show n
  show (AAStore n)                       = "astore " ++ show n
  show (AILoad n)                        = "iload " ++ show n
  show (AIStore n)                       = "istore " ++ show n
  show (AField t n)                      = ".field public " ++ n ++ " " ++ show t
  show (AGetField t n)                   = "getfield " ++ aTypeToRef t ++ "/" ++ n ++ " " ++ aTypeToUnboxedType t
  show (APutField t n)                   = "putfield " ++ aTypeToRef t ++ "/" ++ n ++ " " ++ aTypeToUnboxedType t
  show AAALoad                           = "aaload"
  show AAAStore                          = "aastore"
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

type StructAssembly = (String, Assembly)

data CGEnv = CGEnv { progName     :: String,
                     assembly     :: Assembly,
                     structs      :: [StructAssembly],
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
countStackExpr (IRAssign _ lhs rhs)  = max (countStackExpr rhs) (max 5 (1 + countStackExpr lhs))
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
countStackStmt (IRAllocate _ _ e)             = max (2 + countStackExpr e) 3
countStackStmt (IRExpressionStatement e)      = countStackExpr e
countStackStmt (IRIfElse cond thn Nothing)    = max (countStackExpr cond) (countStackStmt thn)
countStackStmt (IRIfElse cond thn (Just els)) = max (countStackExpr cond) (max (countStackStmt thn) (countStackStmt els))
countStackStmt (IRWhile cond body)            = max (countStackExpr cond) (countStackStmt body)
countStackStmt (IRLet defs body)              = max (foldr max 0 $ zipWith countDefAdding [0..] defs) (countStackStmt body)
  where countDefAdding n (IRVariableDefinition _ _ Nothing) = n + 1
        countDefAdding n (IRVariableDefinition _ _ (Just e)) = n + 2 + countStackExpr e
countStackStmt IRSkip                         = 0
countStackStmt (IRReturn Nothing)             = 0
countStackStmt (IRReturn (Just e))            = 2 + countStackExpr e

generateExpr (IRIntLiteral n) = do 
  emit $ AIntPush n

generateExpr (IRCharLiteral c) = do
  emit $ AIntPush (fromIntegral . ord $ c)

generateExpr (IRStringLiteral str) = do
  emit $ AStringPush str
  emit $ AInvoke (AArray AInt) "Runtime" "string_to_null_array" [AJavaClass "java/lang/String"]
  
generateExpr (IRVariable t name) = do
  loc <- getVarStore name
  emit $ AALoad loc
  emit $ AGetField (cTypeToAType t) "c"
  
generateExpr (IRAssign t e1 e2) = do
  generateExpr e2
  emit $ ADup
  generateAssignment e1
  where generateAssignment (IRVariable _ name) = do
          loc <- getVarStore name
          emit $ AALoad loc
          emit $ ADupX1
          emit $ ASwap
          emit $ APutField (cTypeToAType t) "c"
          emit $ AAStore loc
        generateAssignment (IRDereference t e) = do
          generateExpr e
          emit $ AConst AInt 0
          emit $ AAALoad 
          emit $ ACheckCast (cTypeToAType t)
          emit $ ASwap
          emit $ APutField (cTypeToAType t) "c"
        generateAssignment (IRArrayRef t arr ref) = do
          generateExpr arr
          generateExpr ref
          emit $ AAALoad
          emit $ ACheckCast (cTypeToAType t)
          emit $ ASwap
          emit $ APutField (cTypeToAType t) "c"

generateExpr (IRDereference t e) = do
  generateExpr e
  emit $ AConst AInt 0
  emit $ AAALoad
  emit $ ACheckCast (cTypeToAType t)
  emit $ AGetField (cTypeToAType t) "c"


generateExpr (IRAddressOf t e) = do
  emit $ AConst AInt 1
  emit $ ANewArray (cTypeToAType t)
  emit $ ADup
  emit $ AConst AInt 0
  generateAddress e 
  where generateAddress (IRVariable _ name) = do
          loc <- getVarStore name
          emit $ AALoad loc
          emit $ AAAStore
        generateAddress (IRArrayRef t arr ref) = do
          generateExpr arr
          generateExpr ref
          emit $ AAALoad
          emit $ ACheckCast (cTypeToAType t)
          emit $ AAAStore

generateExpr (IRArrayRef t arr ref) = do
  generateExpr arr
  generateExpr ref
  emit $ AAALoad
  emit $ ACheckCast (cTypeToAType t)
  emit $ AGetField (cTypeToAType t) "c"

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

generateExpr (IRCall t "main" args) = 
    generateExpr (IRCall t "$main" args)

generateExpr (IRCall t n args) = do
  mapM_ generateBoxedExpr args
  env <- get
  let pName = progName env
  emit $ AInvoke (cTypeToAType t) pName n (map (cTypeToAType . cTypeOf) args)
  when (cTypeToAType t /= AVoid) $
    emit $ AGetField (cTypeToAType t) "c" 

generateExpr (IRExternCall t m n args) = do
  mapM_ generateBoxedExpr args
  emit $ AInvoke (cTypeToAType t) m n (map (cTypeToAType . cTypeOf) args)
  when (cTypeToAType t /= AVoid) $
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

generateStmt (IRAllocate t n s) = do
  loc <- getVarStore n
  loop <- getNextLabel "loop"
  -- take a pointer
  emit $ AALoad loc
  emit $ ADup
  -- compute array size
  generateExpr s
  -- save it in register
  emit $ ADup 
  emit $ AIStore loc
  
  -- allocate array
  emit $ ANewArray (cTypeToAType t)
  emit $ ALabel loop
  -- copy array reference
  emit $ ADup
  -- decrease array index
  emit $ AILoad loc
  emit $ AConst AInt 1
  emit $ ASub AInt
  -- save it in register
  emit $ ADup
  emit $ AIStore loc
  -- create new object
  emit $ ANew (cTypeToAType t)
  -- store it in array
  emit $ AAAStore
  -- compare index to 0
  emit $ AILoad loc
  emit $ AConst AInt 0
  -- if not 0, loop
  emit $ AIfICmp CodeGenerator.NE loop
  -- else put array into pointer
  emit $ APutField (cTypeToAType (CPointerType t)) "c"
  -- and store it in register
  emit $ AAStore loc

generateStmt (IRExpressionStatement e) = do
  generateExpr e
  when (cTypeToAType (cTypeOf e) /= AVoid) $
    emit $ APop

generateStmt (IRReturn Nothing) = do
  emit $ AReturn AVoid

generateStmt (IRReturn (Just e)) = do
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
  fi  <- getNextLabel "fi"
  emit $ AIfICmp CodeGenerator.EQ elsL
  generateStmt thn
  emit $ AGoto fi
  emit $ ALabel elsL
  maybeEmit els
  emit $ ALabel fi
  emit $ ANop
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
  
generateStmt (IRLet defs body) = do
  oldE <- pushDefs defs
  generateStmt body
  popDefs oldE  

generateStmt _ = return ()

pushDefs defs = do
  e <- get 
  let CGEnv { nextLocalVar = n, localVars = vs } = e
  put $ e { nextLocalVar = n + length defs,
            localVars = foldr insertIndexedDef vs (zip defs [n..]) }
  zipWithM_ generateVarDef [n..] defs
  return e
  where insertIndexedDef ((IRVariableDefinition _ name _), i) vs = Map.insert name i vs
        generateVarDef i (IRVariableDefinition t name e) = do
          let t' = cTypeToAType t
          emit $ ANew t'
          maybeGenerateExpr t' e
          emit $ AAStore i
        maybeGenerateExpr _ Nothing = return ()
        maybeGenerateExpr t' (Just e) = do 
          emit $ ADup
          generateExpr e
          emit $ APutField t' "c"
        

pushArgsDef args = do
  e <- get
  let CGEnv { nextLocalVar = n, localVars = vs } = e
  put $ e { nextLocalVar = n + length args,
            localVars = foldr (uncurry Map.insert) vs (zip (map snd args) [n..]) }
  return e

popDefs oldE = do
  e <- get
  put $ e { nextLocalVar = nextLocalVar oldE, localVars = localVars oldE }

generateDef (IRFunctionDefinition t "main" args body) = do
    generateDef (IRFunctionDefinition t "$main" args body)
    env <- get
    let name = progName env
    emit $ AMain name

generateDef (IRFunctionDefinition t name args body) = do
  emit $ AFunction (cTypeToAType t) name (map (cTypeToAType . fst) args)
  emit $ ALimitLocals (length args + countLocals (IRBlock body))
  emit $ ALimitStack (countStackStmt (IRBlock body))
  oldE <- pushArgsDef args
  mapM_ generateStmt body
  popDefs oldE
  emit $ AEndFunction

generateDef (IRStructDefinition name fields) = do
  e <- get
  put e { structs = newStruct : (structs e) }
    where newStruct = (name, [AProgram name] ++ map makeField fields ++ [AConstructor, AReturn AVoid, AEndFunction] )
          makeField (t, fName) = AField (cTypeToAType t) fName

generateAssembly :: String -> IRTrlanslationUnit -> Env -> (Assembly, [StructAssembly])
generateAssembly name ir env = 
  let (_, s) = runState (mapM_ generateDef ir) (CGEnv { progName = name, 
                                                        assembly = [AEndFunction, AReturn AVoid, AConstructor, AProgram name], 
                                                        structs = [],
                                                        env = env, 
                                                        nextLocalVar = 0, 
                                                        nextLabel = 0, 
                                                        localVars = Map.empty})
  in (reverse $ assembly s, structs s)

showAssembly :: Assembly -> String
showAssembly = foldMap $ (++ "\n") . show 
            