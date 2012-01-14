module IR(CType(..),
          IRExpression(..),
          IRDefinition(..),
          IRStatement(..),
          IRTrlanslationUnit,
          cTypeOf) where

data CType = CInt | CFloat | CBool | CVoid | CChar
           | CSelf -- used in recursive structures
           | CTypedefType String CType
           | CStructType String [(CType, String)]
           | CPointerType CType
           deriving Show
                    
instance Eq CType where
  CInt == CInt                                = True
  CFloat == CFloat                            = True
  CBool == CBool                              = True
  CChar == CChar                              = True
  CSelf == CSelf                              = True
  (CTypedefType _ t1) == (CTypedefType _ t2)  = t1 == t2
  (CStructType _ t1) == (CStructType _ t2)    = t1 == t2
  (CPointerType t1) == (CPointerType t2)      = t1 == t2
  _ == _                                      = False

data IRExpression = IRStringLiteral String
                  | IRCharLiteral Char   
                  | IRIntLiteral Integer
                  | IRVariable CType String 
                  | IRAssign CType IRExpression IRExpression
                  | IRDereference CType IRExpression
                  | IRAddressOf CType IRExpression
                  | IRArrayRef CType IRExpression IRExpression
                  | IRBinDot CType IRExpression IRExpression
                  | IRBinLessThan IRExpression IRExpression
                  | IRBinGreaterThan IRExpression IRExpression
                  | IRBinLessEquals IRExpression IRExpression
                  | IRBinGreaterEquals IRExpression IRExpression
                  | IREquals IRExpression IRExpression
                  | IRNotEquals IRExpression IRExpression
                  | IRBinPlus CType IRExpression IRExpression
                  | IRBinMinus CType IRExpression IRExpression
                  | IRBinMul CType IRExpression IRExpression
                  | IRBinDiv CType IRExpression IRExpression
                  | IRUnPlus CType IRExpression
                  | IRUnMinus CType IRExpression
                  | IRCall CType String [IRExpression]
                  | IRExternCall CType String String [IRExpression]
                  deriving (Show)

cTypeOf (IRStringLiteral _)      = CPointerType CChar
cTypeOf (IRCharLiteral _)        = CChar
cTypeOf (IRIntLiteral _)         = CInt
cTypeOf (IRVariable t _)         = t
cTypeOf (IRAssign t _ _)         = t
cTypeOf (IRDereference t _)      = t
cTypeOf (IRAddressOf t _)        = t
cTypeOf (IRArrayRef t _ _)       = t
cTypeOf (IRBinDot t _ _)         = t
cTypeOf (IRBinLessThan _ _)      = CBool
cTypeOf (IRBinGreaterThan _ _)   = CBool
cTypeOf (IRBinLessEquals _ _)    = CBool
cTypeOf (IRBinGreaterEquals _ _) = CBool
cTypeOf (IREquals _ _)           = CBool
cTypeOf (IRNotEquals _ _)        = CBool
cTypeOf (IRBinPlus t _ _)        = t
cTypeOf (IRBinMinus t _ _)       = t
cTypeOf (IRBinMul t _ _)         = t
cTypeOf (IRBinDiv t _ _)         = t
cTypeOf (IRUnPlus t _)           = t
cTypeOf (IRUnMinus t _)          = t
cTypeOf (IRCall t _ _)           = t
cTypeOf (IRExternCall t _ _ _)   = t

data IRDefinition = IRVariableDefinition CType String (Maybe IRExpression)
                  | IRFunctionDefinition CType String [(CType, String)] [IRStatement]
                  deriving Show
                           
data IRStatement = IRBlock [IRStatement]
                 | IRAllocate CType String IRExpression
                 | IRExpressionStatement IRExpression
                 | IRIfElse IRExpression IRStatement (Maybe IRStatement)
                 | IRWhile IRExpression IRStatement
                 | IRLet [IRDefinition] IRStatement
                 | IRSkip
                 | IRReturn IRExpression
                 deriving Show

type IRTrlanslationUnit = [IRDefinition]