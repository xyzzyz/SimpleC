module Parser(cFile) where 

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr

import AST
                         
primitiveTypes = ["void", "int", "float", "bool", "char"]
keywords = ["if", "while", "for", "else", "while", "for", "return", "struct", "let", "typedef", "extern"]

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
brackets = P.brackets cLexer
commaSep = P.commaSep cLexer
semi = P.semi cLexer

externFunctionCall = do 
  m <- identifier
  string "::"
  n <- identifier
  args <- parens (commaSep expr)
  return $ CExternCall m n args

functionCall = do 
  n <- identifier
  args <- parens (commaSep expr)
  return $ CCall n args

term = (fmap CStringLiteral stringLiteral)
       <|> (fmap CCharLiteral charLiteral)
       <|> try externFunctionCall
       <|> try functionCall
       <|> (fmap CSymbol identifier)
       <|> (fmap CIntLiteral integer)
       <|> (parens expr)
       

table   = [ [binary "." (CBinDot) AssocLeft]
          , [prefix "-" CUnMinus, prefix "+" CUnPlus,
             prefix "*" CDereference, prefix "&" CAddressOf,
             Postfix (do { ref <- brackets expr; return $ (flip CArrayRef) ref }) ]
          , [binary "*" (CBinMul) AssocLeft, binary "/" (CBinDiv) AssocLeft ]
          , [binary "+" (CBinPlus) AssocLeft, binary "-" (CBinMinus)   AssocLeft ]
          , [binary "<" (CBinLessThan) AssocLeft, binary ">" (CBinGreaterThan) AssocLeft,
             binary "<=" (CBinLessEqual) AssocLeft, binary ">=" (CBinGreaterEqual) AssocLeft]
          , [binary "==" (CEquals) AssocLeft, binary "!=" (CNotEquals) AssocLeft]
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
  
simpleType = (fmap CPrimitiveTypeDeclaration (choice $ map reservedKeyword primitiveTypes))
           <|> (fmap CTypedefTypeDeclaration identifier)
       
pointerType = do
  string "*"
  t <- cType
  return $ CPointerDeclaration t
  
  
structType = do
  reserved "struct"
  name <- identifier
  return $ CStructDeclaration name 
  
cType = pointerType
        <|> simpleType 
        <|> structType
  
variableDef = do
  t <- cType 
  n <- identifier
  e <- optionMaybe init
  return $ CVariableDefinition t n e
  where init = do
          reservedOp "="
          expr

letStatement = do
  reserved "let"
  decls <- parens (endBy1 variableDef semi)
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

typedefDef = do
  reserved "typedef"
  t <- cType
  name <- identifier
  return $ CTypedefDefinition t name
  
structElementsDecl = braces $ structElement `endBy` semi
  where structElement = do
          t <- cType 
          n <- identifier
          return (t, n)

structDef = do
  reserved "struct"
  n <- identifier
  elems <- structElementsDecl
  return $ CStructDefinition n elems

  
typeDef = structDef
          <|> typedefDef
          <?> "typeDef"
           
functionDef = do
  t <- cType 
  n <- identifier
  args <- parens (sepBy arg semi)
  body <- blockStatement
  return $ CFunctionDefinition t n args body
  where arg = do
          t <- cType
          n <- identifier
          return (t, n)
          
externDef = do
  reserved "extern"
  t <- cType
  m <- identifier
  string "::"
  n <- identifier
  args <- parens (sepBy arg semi)
  return $ CExternDefinition t m n args
  where arg = do
          t <- cType
          n <- identifier
          return (t, n)

globalDef = typeDef
            <|> externDef
            <|> try functionDef
            <|> variableDef
            <?> "globalDef"

cFile = do
  whitespace
  decls <- endBy globalDef semi
  eof
  return decls
  

