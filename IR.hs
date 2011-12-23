module IR(CType(..),
          IRExpression(..),
          IRDefinition(..),
          IRStatement(..),
          IRProgram,
          cTypeOf,
          countLocals) where

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

data IRExpression = IRStringLiteral String
                  | IRCharLiteral Char   
                  | IRIntLiteral Integer
                  | IRVariable CType String 
                  | IRAssign CType IRExpression IRExpression
                  | IRPostIncrement CType IRExpression
                  | IRBinDot CType IRExpression IRExpression
                  | IRBinLessThan IRExpression IRExpression
                  | IREquals IRExpression IRExpression
                  | IRBinPlus CType IRExpression IRExpression
                  | IRBinMinus CType IRExpression IRExpression
                  | IRBinMul CType IRExpression IRExpression
                  | IRBinDiv CType IRExpression IRExpression
                  | IRUnPlus CType IRExpression
                  | IRUnMinus CType IRExpression
                  | IRCall CType String [IRExpression]
                  deriving (Show)

cTypeOf (IRStringLiteral _) = CPointerType CChar
cTypeOf (IRCharLiteral _) = CChar
cTypeOf (IRIntLiteral _) = CInt
cTypeOf (IRVariable t _) = t
cTypeOf (IRAssign t _ _) = t
cTypeOf (IRPostIncrement t _) = t
cTypeOf (IRBinDot t _ _) = t
cTypeOf (IRBinLessThan _ _) = CBool
cTypeOf (IREquals _ _) = CBool
cTypeOf (IRBinPlus t _ _) = t
cTypeOf (IRBinMinus t _ _) = t
cTypeOf (IRBinMul t _ _) = t
cTypeOf (IRBinDiv t _ _) = t
cTypeOf (IRUnPlus t _) = t
cTypeOf (IRUnMinus t _) = t
cTypeOf (IRCall t _ _) = t

data IRDefinition = IRVariableDefinition CType String (Maybe IRExpression)
                  | IRFunctionDefinition CType String [(CType, String)] Int [IRStatement] 
                  deriving Show
                           
data IRStatement = IRBlock [IRStatement]
                 | IRExpressionStatement IRExpression
                 | IRIfElse IRExpression IRStatement (Maybe IRStatement)
                 | IRWhile IRExpression IRStatement
                 | IRLet [IRDefinition] IRStatement
                 | IRSkip
                 | IRReturn IRExpression
                 deriving Show


countLocals (IRBlock ss) = foldr (+) 0 $ map countLocals ss
countLocals (IRLet l s) = (length l) + countLocals s
countLocals (IRIfElse _ t Nothing) = countLocals t
countLocals (IRIfElse _ t (Just e)) = countLocals t + countLocals e
countLocals (IRWhile _ b) = countLocals b
countLocals _ = 0

type IRProgram = [IRDefinition]