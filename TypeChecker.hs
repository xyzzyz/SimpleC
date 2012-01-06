{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeChecker(CFunType, 
                   TypeEnv,
                   VarEnv,
                   FunEnv,
                   Env,
                   typeCheckTranslationUnit) where
import AST
import IR
import Control.Monad.State
import Control.Monad.Error

import Data.Maybe
import qualified Data.Map as Map

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
  return Nothing
    
typeCheck (CStructDefinition name fields) = do
  ensureTypeDoesNotExist name
  structDeftoType name fields >>= putType name
  return Nothing
  
typeCheck (CVariableDefinition decl name (Just expr)) = do
  tt <- declToType decl
  et <- typeCheckExpr expr
  if tt /= (cTypeOf et)
    then throwError $ TypeMismatch tt (cTypeOf et)
    else putVarIntoCurrentVarEnv tt name
  return $ Just (IRVariableDefinition tt name (Just et))


typeCheck (CVariableDefinition decl name Nothing) = do
  tt <- declToType decl
  putVarIntoCurrentVarEnv tt name
  return $ Just (IRVariableDefinition tt name Nothing)
  
typeCheck (CFunctionDefinition decl name args (CBlock body)) = do
  rt <- declToType decl
  ensureFunctionDoesNotExist name
  argTypes <- mapM (declToType . fst) args
  putFunction name (rt, argTypes)
  let newVarEnv = makeNewVarEnv (map snd args) argTypes
  pushVarEnv newVarEnv
  setCurRetType rt
  stmts <- mapM typeCheckStatement body
  popVarEnv
  return $ Just (IRFunctionDefinition rt name (zipWith makeArg argTypes args) stmts)
  where makeArg t (_, n) = (t, n)
          

ensureVariableDef (CVariableDefinition decl name expr) = do
  dt <- declToType decl
  expr' <- case expr of
    Nothing -> return Nothing
    Just expr -> do 
      et <- typeCheckExpr expr
      when (dt /= cTypeOf et) 
        (throwError $ TypeMismatch (cTypeOf et) dt) 
      return $ Just et
  return $ IRVariableDefinition dt name expr'

ensureVariableDef _ = throwError $ UnexpectedNonvariableDefinition

typeCheckStatement (CBlock stmts) = mapM typeCheckStatement stmts >>= return . IRBlock 

typeCheckStatement (CExpressionStatement e) = do
  e' <- typeCheckExpr e
  return $ IRExpressionStatement e'
  
typeCheckStatement (CIfElse cond thn els) = do
  t <- typeCheckExpr cond
  when (cTypeOf t /= CBool)
    (throwError $ TypeMismatch (cTypeOf t) CBool)
  thn' <- typeCheckStatement thn
  case els of
    Nothing -> return $ IRIfElse t thn' Nothing
    Just els -> typeCheckStatement els >>= return . IRIfElse t thn' . Just

typeCheckStatement (CWhile cond body) = do
  t <- typeCheckExpr cond
  when (cTypeOf t /= CBool)
    (throwError $ TypeMismatch (cTypeOf t) CBool)
  body' <- typeCheckStatement body
  return $ IRWhile t body'

typeCheckStatement (CFor (init, cond, after) body) = do
  init' <- case init of
    Nothing -> return IRSkip
    Just init -> typeCheckExpr init >>= return . IRExpressionStatement
  after' <- case after of 
    Nothing -> return IRSkip
    Just after -> typeCheckExpr after >>= return . IRExpressionStatement
  cond' <- case cond of
    Nothing -> return $ IREquals (IRIntLiteral 42) (IRIntLiteral 42)
    Just cond -> typeCheckExpr cond
  let t = cTypeOf cond'
  when (t /= CBool)
    (throwError $ TypeMismatch t CBool)
  body' <- typeCheckStatement body
  return $ IRBlock [init', IRWhile cond' (IRBlock [body', after'])]

typeCheckStatement (CLet defs body) = do
  defs <- mapM ensureVariableDef defs
  let newVarEnv = uncurry makeNewVarEnv $ unzip . map defToVar $ defs
  pushVarEnv newVarEnv
  body' <- typeCheckStatement body
  popVarEnv
  return $ IRLet defs body'
  where defToVar (IRVariableDefinition t name _) = (name, t)
  
typeCheckStatement (CReturn e) = do
  t <- typeCheckExpr e
  curRet <- getCurRetType
  when (cTypeOf t /=  curRet)
    (throwError $ TypeMismatch (cTypeOf t) curRet)
  return $ IRReturn t

checkLValue (CSymbol _) = return True
--checkLValue (CPointer t) = checkLValue t

typeCheckExpr (CStringLiteral s) = return $ IRStringLiteral s
typeCheckExpr (CCharLiteral c) = return $ IRCharLiteral c
typeCheckExpr (CSymbol name) = getVarType name >>= return . ((flip IRVariable) name)
typeCheckExpr (CIntLiteral i) = return $ IRIntLiteral i
typeCheckExpr (CAssign lhs rhs) = do
  lt <- typeCheckExpr lhs
  rt <- typeCheckExpr rhs
  checkLValue lhs
  if cTypeOf lt /= cTypeOf rt
    then throwError $ TypeMismatch (cTypeOf lt) (cTypeOf rt)
    else return $ IRAssign (cTypeOf rt) lt rt

typeCheckExpr (CPostIncrement t) = do 
  checkLValue t 
  t' <- typeCheckExpr t
  return $ IRAssign (cTypeOf t') t' (IRBinPlus (cTypeOf t') t' (IRIntLiteral 1))

typeCheckExpr (CBinDot s f) = undefined -- TODO
typeCheckExpr (CBinLessThan e1 e2) = typeCheckBinRelExpr e1 e2 IRBinLessThan
typeCheckExpr (CBinGreaterThan e1 e2) = typeCheckBinRelExpr e1 e2 IRBinGreaterThan
typeCheckExpr (CBinLessEqual e1 e2) = typeCheckBinRelExpr e1 e2 IRBinLessEquals
typeCheckExpr (CBinGreaterEqual e1 e2) = typeCheckBinRelExpr e1 e2 IRBinGreaterEquals
typeCheckExpr (CEquals e1 e2) = typeCheckBinRelExpr e1 e2 IREquals
typeCheckExpr (CNotEquals e1 e2) = typeCheckBinRelExpr e1 e2 IRNotEquals
typeCheckExpr (CBinPlus e1 e2) = typeCheckBinOpExpr e1 e2 IRBinPlus
typeCheckExpr (CBinMinus e1 e2) = typeCheckBinOpExpr e1 e2 IRBinMinus
typeCheckExpr (CBinMul e1 e2) = typeCheckBinOpExpr e1 e2 IRBinMul
typeCheckExpr (CBinDiv e1 e2) = typeCheckBinOpExpr e1 e2 IRBinDiv
typeCheckExpr (CUnPlus e) = do  
  t <- typeCheckExpr e
  if cTypeOf t /= CInt && cTypeOf t /= CFloat 
    then throwError $ TypeMismatch (cTypeOf t) CInt
    else return t

typeCheckExpr (CUnMinus e) = do 
  t <- typeCheckExpr e
  if cTypeOf t /= CInt && cTypeOf t /= CFloat 
    then throwError $ TypeMismatch (cTypeOf t) CInt
    else return $ IRUnMinus (cTypeOf t) t

typeCheckExpr (CCall name args) = do
  (retType, argTypes) <- getFunction name
  ts <- mapM typeCheckExpr args
  checkArgTypes argTypes (map cTypeOf ts)
  return $ IRCall retType name ts
  where checkArgTypes [] [] = return ()
        checkArgTypes [] (_:_) = throwError $ WrongArgumentCount name
        checkArgTypes (_:_) [] = throwError $ WrongArgumentCount name
        checkArgTypes (a:as) (t:ts) = do
          if a /= t 
            then throwError $ TypeMismatch t a
            else checkArgTypes as ts


typeCheckBinRelExpr e1 e2 cons = do   
  lt <- typeCheckExpr e1
  rt <- typeCheckExpr e2
  if cTypeOf lt /= CInt && cTypeOf lt /= CFloat 
    then throwError $ TypeMismatch (cTypeOf lt) CInt
    else if cTypeOf lt /= cTypeOf rt
         then throwError $ TypeMismatch (cTypeOf rt) (cTypeOf lt)
         else return $ cons lt rt

typeCheckBinOpExpr e1 e2 cons = do   
  lt <- typeCheckExpr e1
  rt <- typeCheckExpr e2
  if cTypeOf lt /= CInt && cTypeOf lt /= CFloat 
    then throwError $ TypeMismatch (cTypeOf lt) CInt
    else if cTypeOf lt /= cTypeOf rt
         then throwError $ TypeMismatch (cTypeOf rt) (cTypeOf lt)
         else return $ cons (cTypeOf lt) lt rt

typeChecker :: CTranslationUnit -> TypeChecker IRTrlanslationUnit
typeChecker p = mapM typeCheck p >>= return . catMaybes

runTypeChecker p s = case runState (runErrorT (runChecker p)) s of
  (Left err, _) -> Left err
  (Right r, bs) -> Right (r, bs)
  
typeCheckTranslationUnit p = runTypeChecker (typeChecker p) emptyEnv 
