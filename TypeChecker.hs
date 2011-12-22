{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeChecker where
import AST

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
                             
data CType = CInt | CFloat | CBool | CVoid
           | CSelf -- used in recursive structures
           | CTypedefType String CType
           | CStructType String [(CType, String)]
           | CPointerType CType
           deriving Show
                    
type TypeEnv = Map.Map String CType

emptyTypeEnv = Map.fromList [("int", CInt), ("float", CFloat),
                             ("bool", CBool), ("void", CVoid)]

data TypeError = TypeExistsError String CType
                   | UnknownTypeError String
                   | OtherTypeError String
                   deriving Show
                            
instance Error TypeError where
  noMsg = OtherTypeError "type error occured"
  strMsg = OtherTypeError
  
newtype TypeChecker a = Checker {
  runChecker :: ErrorT TypeError (State TypeEnv) a
  } deriving (Monad, MonadError TypeError)

liftChecker m = Checker (lift m)
  
typeChecker :: CTranslationUnit -> TypeChecker ()
typeChecker unit = do
  return ()

runParser p s = case runState (runErrorT (runChecker p)) s of
  (Left err, _) -> Left err
  (Right r, bs) -> Right (r, bs)