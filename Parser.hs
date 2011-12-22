module Parser(cFile) where 

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr

import AST
                         
primitiveTypes = ["int", "float", "bool"]
keywords = ["if", "while", "for", "else", "while", "for", "return", "struct", "let", "typedef"]

cDef = javaStyle { reservedNames = keywords ++ primitiveTypes}

cLexer = P.makeTokenParser cDef

reserved = P.reserved cLexer
reservedOp = P.reservedOp cLexer


whitespace = P.whiteSpace cLexer
stringLiteral = P.stringLiteral cLexer
charLiteral = P.charLiteral cLexer
integer = P.integer cLexer
operator = P.operator cLexer
identifier = P.identifier cLexer
symbol = P.symbol cLexer
parens = P.parens cLexer
braces = P.braces cLexer
commaSep = P.commaSep cLexer
semi = P.semi cLexer

functionCall = do 
  n <- identifier
  args <- parens (commaSep expr)
  return $ CCall n args

term = (fmap CString stringLiteral)
       <|> (fmap CChar charLiteral)
       <|> try functionCall
       <|> (fmap CSymbol identifier)
       <|> (fmap CInteger integer)
       <|> (parens expr)
       

table   = [ [binary "." (CBinDot) AssocLeft, postfix "++" CPostIncrement]
          , [prefix "-" CUnMinus, prefix "+" CUnPlus ]
          , [binary "*" (CBinMul) AssocLeft, binary "/" (CBinDiv) AssocLeft ]
          , [binary "+" (CBinPlus) AssocLeft, binary "-" (CBinMinus)   AssocLeft ]
          , [binary "<" (CBinLessThan) AssocLeft]
          , [binary "==" (CEquals) AssocLeft]
          , [binary "=" (CAssign) AssocRight]
          ]
            
  
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })      


expr = buildExpressionParser table term
       <?> "expression"
       
reservedKeyword name = do
  reserved name
  return name
  
simpleType = (fmap CPrimitiveType (choice $ map reservedKeyword primitiveTypes))
           <|> (fmap CTypedefType identifier)
       
pointerType = do
  reservedOp "*"
  t <- cType
  return $ CPointer t
  
  
structType = do
  reserved "struct"
  name <- identifier
  return $ CStruct name 
  
cType = pointerType
        <|> simpleType 
        <|> structType
  
variableDecl = do
  t <- cType 
  n <- identifier
  e <- optionMaybe init
  return $ CVariable t n e
  where init = do
          reservedOp "="
          expr

letStatement = do
  reserved "let"
  decls <- parens (endBy1 variableDecl semi)
  body <- statement
  return $ CLet decls body

blockStatement = fmap CBlock (braces (many statement))

expressionStatement = do 
  e <- expr 
  semi
  return (CExpressionStatement e)


ifStatement = do
  reserved "if"
  e <- parens expr
  thn <- statement
  els <- optionMaybe elseStatement 
  return $ CIfElse e thn els
  where elseStatement = do
          reserved "else"
          statement

whileStatement = do
  reserved "while"
  cond <- parens expr
  body <- statement
  return $ CWhile cond body

forStatement = do
  reserved "for"
  args <- parens forArgs
  body <- statement
  return $ CFor args body
  where forArgs = do
          init <- optionMaybe expr
          semi 
          cond <- optionMaybe expr
          semi
          iter <- optionMaybe expr
          return (init, cond, iter)

returnStatement = do
  reserved "return"
  e <- expr
  semi
  return (CReturn e)

statement = blockStatement
            <|> expressionStatement
            <|> ifStatement
            <|> whileStatement
            <|> forStatement
            <|> letStatement
            <|> returnStatement

typedefDecl = do
  reserved "typedef"
  t <- cType
  name <- identifier
  return $ CTypedefDeclaration t name
  
structElementsDecl = braces $ structElement `endBy` semi
  where structElement = do
          t <- cType 
          n <- identifier
          return (t, n)

structDecl = do
  reserved "struct"
  n <- identifier
  elems <- structElementsDecl
  return $ CStructDeclaration n elems

  
typeDecl = structDecl
           <|> typedefDecl
           <?> "typeDecl"
           
functionDecl = do
  t <- cType 
  n <- identifier
  args <- parens (sepBy arg semi)
  body <- braces (sepBy statement semi)
  return $ CFunction t n args body
  where arg = do
          t <- cType
          n <- identifier
          return (t, n)
          
globalDecl = (fmap CType typeDecl)
             <|> try functionDecl
             <|> variableDecl
             <?> "globalDecl"

cFile = do
  whitespace
  decls <- endBy globalDecl semi
  eof
  return decls
  

