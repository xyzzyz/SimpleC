module AST(CType(CPrimitiveType, CTypedefType, CStruct, CPointer), 
           CTypeDeclaration(CTypedefDeclaration, CStructDeclaration), 
           CDeclaration(CType, CVariable, CFunction), 
           CExpression(CString, CChar, CSymbol, CInteger, CAssign, CPostIncrement, 
                       CBinDot, CBinLessThan, CEquals, CBinPlus, CBinMinus, CBinMul, CBinDiv,
                       CUnPlus, CUnMinus, CCall),
           CStatement(CBlock, CExpressionStatement, CIfElse, CWhile, CFor, CLet, CReturn)) 
       where

data CType = CPrimitiveType String
           | CTypedefType String 
           | CStruct String
           | CPointer CType
           deriving Show

data CTypeDeclaration = CTypedefDeclaration CType String
                      | CStructDeclaration String [(CType, String)]
                      deriving Show

data CDeclaration = CType CTypeDeclaration
                  | CVariable CType String (Maybe CExpression)
                  | CFunction CType String [(CType, String)] [CStatement]
                  deriving Show



data CExpression = CString String
                 | CChar Char
                 | CSymbol String
                 | CInteger Integer
                 | CAssign CExpression CExpression
                 | CPostIncrement CExpression
                 | CBinDot CExpression CExpression
                 | CBinLessThan CExpression CExpression
                 | CEquals CExpression CExpression
                 | CBinPlus CExpression CExpression
                 | CBinMinus CExpression CExpression
                 | CBinMul CExpression CExpression
                 | CBinDiv CExpression CExpression
                 | CUnPlus CExpression
                 | CUnMinus CExpression
                 | CCall String [CExpression]
                 deriving Show
                  
data CStatement = CBlock [CStatement]
                | CExpressionStatement CExpression
                | CIfElse CExpression CStatement (Maybe CStatement)
                | CWhile CExpression CStatement
                | CFor (Maybe CExpression, Maybe CExpression, Maybe CExpression) CStatement
                | CLet [CDeclaration] CStatement
                | CReturn CExpression
                deriving Show
