{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeChecker(typeCheckTranslationUnit) where
import AST

import Control.Monad.State
import Control.Monad.Error

import Data.Maybe
import qualified Data.Map as Map
                             
data CType = CInt | CFloat | CBool | CVoid
           | CSelf -- used in recursive structures
           | CTypedefType String CType
           | CStructType String [(CType, String)]
           | CPointerType CType
           deriving Show
                    
type CFunType = (CType, [CType])

type TypeEnv = Map.Map String CType

emptyTypeEnv = Map.fromList [("int", CInt), ("float", CFloat),
                             ("bool", CBool), ("void", CVoid)]

type VarEnv = [Map.Map String CType]

type FunEnv = Map.Map String CFunType

data Env = Env {
  typeEnv :: TypeEnv,
  varEnv :: VarEnv,
  funEnv :: FunEnv }
         deriving Show

emptyEnv = Env {typeEnv = emptyTypeEnv, varEnv = [], funEnv = Map.empty} 

data TypeError = TypeExistsError String CType
               | UnknownTypeError String
               | TypedefingCompositeType String CType
               | TypedefedStructIsNotStruct String CType
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

getType name = getEnv >>= return . typeEnv >>= return . Map.lookup name

putEnv e = liftChecker $ put e

putType name t = do
  env <- getEnv
  let tEnv = typeEnv env
  putEnv $ env {typeEnv = Map.insert name t tEnv}

declToType (CPrimitiveTypeDeclaration "int") = return CInt
declToType (CPrimitiveTypeDeclaration "float") = return CFloat
declToType (CPrimitiveTypeDeclaration "bool") = return CBool
declToType (CPrimitiveTypeDeclaration "void") = return CVoid

declToType (CTypedefTypeDeclaration name) = do
  t <- getType name
  case t of
    Just t -> case t of
      CTypedefType _ t -> return t
      CSelf -> return CSelf
      t -> throwError (TypedefingCompositeType name t)
    Nothing -> throwError (UnknownTypeError name)
    
declToType (CStructDeclaration name) = do
  t <- getType name
  case t of
    Just t -> case t of
      s@(CStructType _ _) -> return s
      CSelf -> return CSelf
      t -> throwError (TypedefedStructIsNotStruct name t)
    Nothing -> throwError (UnknownTypeError name)

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
  t <- getType name
  case t of
    Just t -> throwError (TypeExistsError name t)
    Nothing -> declToType decl >>= putType name . CTypedefType name
    
typeCheck (CStructDefinition name fields) = do
  t <- getType name
  case t of
    Just t -> throwError (TypeExistsError name t)
    Nothing -> structDeftoType name fields >>= putType name
  
typeChecker :: CTranslationUnit -> TypeChecker ()
typeChecker = mapM_ typeCheck

runTypeChecker p s = case runState (runErrorT (runChecker p)) s of
  (Left err, _) -> Left err
  (Right r, bs) -> Right bs
  
typeCheckTranslationUnit p = runTypeChecker (typeChecker p) emptyEnv 