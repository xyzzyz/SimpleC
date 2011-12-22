{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeChecker(typeCheckTranslationUnit) where
import AST

import Control.Monad.State
import Control.Monad.Error

import Data.Maybe
import qualified Data.Map as Map
                             
data CType = CInt | CFloat | CBool | CVoid | CChar
           | CSelf -- used in recursive structures
           | CTypedefType String CType
           | CStructType String [(CType, String)]
           | CPointerType CType
           deriving Show
                    
instance Eq CType where
  CInt == CInt = True
  CFloat == CFloat = True
  CBool == CBool = True
  CChar == CChar = True
  CSelf == CSelf = True
  (CTypedefType _ t1) == (CTypedefType _ t2) = t1 == t2
  (CStructType _ t1) == (CStructType _ t2) = t1 == t2
  (CPointerType t1) == (CPointerType t2) = t1 == t2
  _ == _ = False

type CFunType = (CType, [CType])

type TypeEnv = Map.Map String CType

emptyTypeEnv = Map.fromList [("int", CInt), ("float", CFloat),
                             ("bool", CBool), ("void", CVoid),
                             ("char", CChar)]

type VarEnv = [Map.Map String CType]

type FunEnv = Map.Map String CFunType

data Env = Env {
  typeEnv :: TypeEnv,
  varEnv :: VarEnv,
  funEnv :: FunEnv, 
  curRetType :: CType }
         deriving Show

emptyEnv = Env {typeEnv = emptyTypeEnv, varEnv = [Map.empty], funEnv = Map.empty, curRetType = CVoid} 

data TypeError = TypeExistsError String CType
               | UnknownTypeError String
               | UnboundSymbolError String
               | TypedefingCompositeType String CType
               | TypedefedStructIsNotStruct String CType
               | TypeMismatch CType CType
               | FunctionExistsError String CFunType
               | UnexpectedNonvariableDefinition
               | WrongArgumentCount String
               | OtherTypeError String
               deriving Show
                            
instance Error TypeError where
  noMsg = OtherTypeError "type error occured"
  strMsg = OtherTypeError
  
newtype TypeChecker a = Checker {
  runChecker :: ErrorT TypeError (State Env) a
  } deriving (Monad, MonadError TypeError)

liftChecker m = Checker (lift m)
  
getEnv = liftChecker get
putEnv e = liftChecker $ put e

getType name = do
  e <- getEnv 
  case Map.lookup name (typeEnv e) of
    Nothing -> throwError (UnknownTypeError name)
    Just t -> return t


ensureTypeDoesNotExist name = do
  e <- getEnv
  case Map.lookup name (typeEnv e) of
    Nothing -> return ()
    Just t -> throwError $ TypeExistsError name t
  

putType name t = do
  env <- getEnv
  let tEnv = typeEnv env
  putEnv $ env {typeEnv = Map.insert name t tEnv}

getVarType name = do
  e <- getEnv
  findVarType (varEnv e)
  where findVarType [] = throwError $ UnboundSymbolError name
        findVarType (x:xs) = case Map.lookup name x of
          Nothing -> findVarType xs
          Just t -> return t

putVarIntoCurrentVarEnv t name = do
  env <- getEnv
  let (ve:ves) = varEnv env 
  putEnv $ env { varEnv = (Map.insert name t ve):ves }
  
pushVarEnv ve = do
  env <- getEnv
  putEnv $ env { varEnv = ve : (varEnv env) }
  
popVarEnv = do
  env <- getEnv
  putEnv $ env { varEnv = tail (varEnv env) }

makeNewVarEnv names types = 
  foldr (uncurry Map.insert) Map.empty $ zipWith (,) names types

getFunction name = do
  e <- getEnv
  case Map.lookup name (funEnv e) of
    Nothing -> throwError $ UnboundSymbolError name
    Just t -> return t

ensureFunctionDoesNotExist name = do
  e <- getEnv
  case Map.lookup name (funEnv e) of
    Nothing -> return ()
    Just t -> throwError $ FunctionExistsError name t

putFunction name t = do
  env <- getEnv
  putEnv $ env {funEnv = Map.insert name t (funEnv env) }

getCurRetType = getEnv >>= return . curRetType
setCurRetType t = do
  e <- getEnv
  putEnv $ e { curRetType = t }


declToType (CPrimitiveTypeDeclaration "int") = return CInt
declToType (CPrimitiveTypeDeclaration "float") = return CFloat
declToType (CPrimitiveTypeDeclaration "bool") = return CBool
declToType (CPrimitiveTypeDeclaration "void") = return CVoid
declToType (CPrimitiveTypeDeclaration "char") = return CChar

declToType (CTypedefTypeDeclaration name) = do
  t <- getType name
  case t of
    CTypedefType _ t -> return t
    CSelf -> return CSelf
    t -> throwError (TypedefingCompositeType name t)
    
declToType (CStructDeclaration name) = do
  t <- getType name
  case t of
    s@(CStructType _ _) -> return s
    CSelf -> return CSelf
    t -> throwError (TypedefedStructIsNotStruct name t)

declToType (CPointerDeclaration decl) = do
  t <- declToType decl
  return $ CPointerType t

structDeftoType name fields = do
  putType name CSelf
  (mapM fieldDeclToDef fields) >>= (return . CStructType name )
  where fieldDeclToDef (decl, varName) = do
          t <- declToType decl
          return (t, varName)

typeCheck (CTypedefDefinition decl name) = do
  ensureTypeDoesNotExist name
  declToType decl >>= putType name . CTypedefType name
    
typeCheck (CStructDefinition name fields) = do
  ensureTypeDoesNotExist name
  structDeftoType name fields >>= putType name
  
typeCheck (CVariableDefinition decl name (Just expr)) = do
  tt <- declToType decl
  et <- typeCheckExpr expr
  if tt /= et 
    then throwError $ TypeMismatch tt et
    else putVarIntoCurrentVarEnv tt name


typeCheck (CVariableDefinition decl name Nothing) = do
  tt <- declToType decl
  putVarIntoCurrentVarEnv tt name
  
typeCheck (CFunctionDefinition decl name args body) = do
  rt <- declToType decl
  ensureFunctionDoesNotExist name
  argTypes <- mapM (declToType . fst) args
  putFunction name (rt, argTypes)
  let newVarEnv = makeNewVarEnv (map snd args) argTypes
  pushVarEnv newVarEnv
  setCurRetType rt
  mapM_ typeCheckStatement body
  popVarEnv
          

ensureVariableDef (CVariableDefinition decl name expr) = do
  dt <- declToType decl
  case expr of
    Nothing -> return ()
    Just expr -> do 
      et <- typeCheckExpr expr
      when (dt /= et) 
        (throwError $ TypeMismatch et dt) 
  return (name, dt)

ensureVariableDef _ = throwError $ UnexpectedNonvariableDefinition

typeCheckStatement (CBlock stmts) = mapM_ typeCheckStatement stmts
typeCheckStatement (CExpressionStatement e) = do
  typeCheckExpr e
  return ()
  
typeCheckStatement (CIfElse cond thn els) = do
  t <- typeCheckExpr cond
  when (t /= CBool)
    (throwError $ TypeMismatch t CBool)
  typeCheckStatement thn
  case els of
    Nothing -> return ()
    Just els -> typeCheckStatement els

typeCheckStatement (CWhile cond body) = do
  t <- typeCheckExpr cond
  when (t /= CBool)
    (throwError $ TypeMismatch t CBool)
  typeCheckStatement body

typeCheckStatement (CFor (init, cond, after) body) = do
  case init of
    Nothing -> return ()
    Just init -> typeCheckExpr init >> return ()
  case after of 
    Nothing -> return ()
    Just after -> typeCheckExpr after >> return ()
  case cond of
    Nothing -> return ()
    Just cond -> do 
      t <- typeCheckExpr cond
      when (t /= CBool)
        (throwError $ TypeMismatch t CBool)
      typeCheckStatement body

typeCheckStatement (CLet defs body) = do
  defs <- mapM ensureVariableDef defs
  let newVarEnv = uncurry makeNewVarEnv $ unzip defs
  pushVarEnv newVarEnv
  typeCheckStatement body
  popVarEnv
  
typeCheckStatement (CReturn e) = do
  t <- typeCheckExpr e
  curRet <- getCurRetType
  when (t /= curRet)
    (throwError $ TypeMismatch t curRet)

checkLValue (CSymbol _) = return True

typeCheckExpr (CStringLiteral _) = return $ CPointerType CChar
typeCheckExpr (CCharLiteral _) = return CChar
typeCheckExpr (CSymbol name) = getVarType name
typeCheckExpr (CIntLiteral _) = return CInt
typeCheckExpr (CAssign lhs rhs) = do
  lt <- typeCheckExpr lhs
  rt <- typeCheckExpr rhs
  checkLValue lhs
  if lt /= rt
    then throwError $ TypeMismatch lt rt
    else return rt

typeCheckExpr (CPostIncrement t) = typeCheckExpr t
typeCheckExpr (CBinDot s f) = undefined -- TODO
typeCheckExpr (CBinLessThan e1 e2) = typeCheckBinRelExpr e1 e2 
typeCheckExpr (CEquals e1 e2) = typeCheckBinRelExpr e1 e2 
typeCheckExpr (CBinPlus e1 e2) = typeCheckBinOpExpr e1 e2 
typeCheckExpr (CBinMinus e1 e2) = typeCheckBinOpExpr e1 e2 
typeCheckExpr (CBinMul e1 e2) = typeCheckBinOpExpr e1 e2 
typeCheckExpr (CBinDiv e1 e2) = typeCheckBinOpExpr e1 e2
typeCheckExpr (CUnPlus e) = typeCheckUnaryNum e
typeCheckExpr (CUnMinus e) = typeCheckUnaryNum e
typeCheckExpr (CCall name args) = do
  (retType, argTypes) <- getFunction name
  ts <- mapM typeCheckExpr args
  checkArgTypes argTypes ts
  return retType
  where checkArgTypes [] [] = return ()
        checkArgTypes [] (_:_) = throwError $ WrongArgumentCount name
        checkArgTypes (_:_) [] = throwError $ WrongArgumentCount name
        checkArgTypes (a:as) (t:ts) = do
          if a /= t 
            then throwError $ TypeMismatch t a
            else checkArgTypes as ts

typeCheckUnaryNum e = do
  t <- typeCheckExpr e
  if t /= CInt && t /= CFloat 
    then throwError $ TypeMismatch t CInt
    else return t

typeCheckBinRelExpr e1 e2 = do   
  lt <- typeCheckExpr e1
  rt <- typeCheckExpr e2
  if lt /= CInt && lt /= CFloat 
    then throwError $ TypeMismatch lt CInt
    else if lt /= rt
         then throwError $ TypeMismatch rt lt
         else return CBool

typeCheckBinOpExpr e1 e2 = do   
  lt <- typeCheckExpr e1
  rt <- typeCheckExpr e2
  if lt /= CInt && lt /= CFloat 
    then throwError $ TypeMismatch lt CInt
    else if lt /= rt
         then throwError $ TypeMismatch rt lt
         else return lt


typeChecker :: CTranslationUnit -> TypeChecker ()
typeChecker = mapM_ typeCheck

runTypeChecker p s = case runState (runErrorT (runChecker p)) s of
  (Left err, _) -> Left err
  (Right r, bs) -> Right bs
  
typeCheckTranslationUnit p = runTypeChecker (typeChecker p) emptyEnv 
