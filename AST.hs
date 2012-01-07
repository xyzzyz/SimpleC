module AST(CTypeDeclaration(..), 
           CDefinition(..), 
           CExpression(..),
           CStatement(..),
           CTranslationUnit) 
       where

data CTypeDeclaration = CPrimitiveTypeDeclaration String
                      | CTypedefTypeDeclaration String 
                      | CStructDeclaration String
                      | CPointerDeclaration CTypeDeclaration
                      deriving Show

data CDefinition = CTypedefDefinition CTypeDeclaration String
                 | CStructDefinition String [(CTypeDeclaration, String)]
                 | CVariableDefinition CTypeDeclaration String (Maybe CExpression)
                 | CFunctionDefinition CTypeDeclaration String [(CTypeDeclaration, String)] CStatement
                  deriving Show

data CExpression = CStringLiteral String
                 | CCharLiteral Char
                 | CIntLiteral Integer
                 | CSymbol String
                 | CAssign CExpression CExpression
                 | CDereference CExpression
                 | CAddressOf CExpression
                 | CPostIncrement CExpression
                 | CBinDot CExpression CExpression
                 | CBinLessThan CExpression CExpression
                 | CBinGreaterThan CExpression CExpression
                 | CBinLessEqual CExpression CExpression
                 | CBinGreaterEqual CExpression CExpression
                 | CEquals CExpression CExpression
                 | CNotEquals CExpression CExpression
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
                | CLet [CDefinition] CStatement
                | CReturn CExpression
                deriving Show

type CTranslationUnit = [CDefinition]