module Parser where

import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))

{- Megaparsec helpers -}

type Parser = Parsec Void Text

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

{- Token parsers -}

identifier :: Parser Text
identifier = label "identifier" $
  lexeme $ do
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let value = T.pack (firstChar : rest)
    pure value

trueToken :: Parser Bool
trueToken = lexeme (string "true" $> True) <?> "true"

falseToken :: Parser Bool
falseToken = lexeme (string "false" $> False) <?> "false"

nullToken :: Parser Text
nullToken = lexeme (string "null") <?> "null"

undefinedToken :: Parser Text
undefinedToken = lexeme (string "undefined") <?> "undefined"

functionToken :: Parser Text
functionToken = lexeme (string "function") <?> "function"

ifToken :: Parser Text
ifToken = lexeme (string "if") <?> "if"

elseToken :: Parser Text
elseToken = lexeme (string "else") <?> "else"

returnToken :: Parser Text
returnToken = lexeme (string "return") <?> "return"

varToken :: Parser Text
varToken = lexeme (string "var") <?> "var"

whileToken :: Parser Text
whileToken = lexeme (string "while") <?> "while"

lengthToken :: Parser Text
lengthToken = lexeme (string "length") <?> "length"

comma :: Parser Text
comma = lexeme (string ",") <?> "','"

assign :: Parser Text
assign = lexeme (string "=") <?> "'='"

semicolon :: Parser Text
semicolon = lexeme (string ";") <?> "';'"

leftBracket :: Parser Text
leftBracket = lexeme (string "[") <?> "'['"

rightBracket :: Parser Text
rightBracket = lexeme (string "]") <?> "']'"

leftParen :: Parser Text
leftParen = lexeme (string "(") <?> "'('"

rightParen :: Parser Text
rightParen = lexeme (string ")") <?> "')'"

leftBrace :: Parser Text
leftBrace = lexeme (string "{") <?> "'{'"

rightBrace :: Parser Text
rightBrace = lexeme (string "}") <?> "'}'"

notToken :: Parser (Expr -> Expr)
notToken = label "not" $
  lexeme $ do
    _ <- string "!"
    pure Not

equal :: Parser (Expr -> Expr -> Expr)
equal = label "'=='" $
  lexeme $ do
    _ <- string "=="
    pure Equal

notEqual :: Parser (Expr -> Expr -> Expr)
notEqual = label "'!='" $
  lexeme $ do
    _ <- string "!="
    pure NotEqual

plus :: Parser (Expr -> Expr -> Expr)
plus = label "'+'" $
  lexeme $ do
    _ <- string "+"
    pure Add

minus :: Parser (Expr -> Expr -> Expr)
minus = label "'-'" $
  lexeme $ do
    _ <- string "-"
    pure Subtract

star :: Parser (Expr -> Expr -> Expr)
star = label "'*'" $
  lexeme $ do
    _ <- string "*"
    pure Multiply

slash :: Parser (Expr -> Expr -> Expr)
slash = label "'/'" $
  lexeme $ do
    _ <- string "/"
    pure Divide

parenthesized :: Parser a -> Parser a
parenthesized p = do
  _ <- leftParen
  expr <- p
  _ <- rightParen
  pure expr

{- Expression Parser Grammar:

  length <- LENGTH LEFT_PAREN expression RIGHT_PAREN
  call <- ID LEFT_PAREN args RIGHT_PAREN
  scalar <- boolean | null | undefined | ID | NUMBER
  arrayLiteral <- LEFT_BRACKET args RIGHT_BRACKET
  arrayLookup <- ID LEFT_BRACKET expression RIGHT_BRACKET
  atom <- call | arrayLiteral | arrayLookup | scalar | LEFT_PAREN expression RIGHT_PAREN
  unary <- NOT? atom
  product <- unary ((STAR | SLASH) unary)*
  sum <- product ((PLUS | MINUS) product)*
  comparison <- sum ((EQUAL | NOT_EQUAL) sum)*
  expression <- comparison

-}

-- length <- LENGTH LEFT_PAREN expression RIGHT_PAREN
lengthExpr :: Parser Expr
lengthExpr = do
  _ <- lengthToken
  array <- parenthesized expr
  pure (Length array)

-- call <- ID LEFT_PAREN args RIGHT_PAREN
callExpr :: Parser Expr
callExpr = label "call" $ do
  callee <- identifier
  args <- parenthesized (expr `sepBy` comma)
  pure (Call callee args)

-- scalar <- boolean | null | undefined | ID | NUMBER
scalarExpr :: Parser Expr
scalarExpr = 
  booleanExpr
    <|> nullExpr
    <|> undefinedExpr
    <|> identifierExpr
    <|> numberExpr
    <?> "scalar"

-- arrayLiteral <- LEFT_BRACKET args RIGHT_BRACKET
arrayLiteralExpr :: Parser Expr
arrayLiteralExpr = do
  _ <- leftBracket
  args <- expr `sepBy` comma
  _ <- rightBracket
  pure (ArrayLiteral args)

-- arrayLookup <- ID LEFT_BRACKET expression RIGHT_BRACKET
arrayLookupExpr :: Parser Expr
arrayLookupExpr = do
  array <- identifierExpr <|> arrayLiteralExpr
  index <- between leftBracket rightBracket expr
  pure (ArrayLookup array index)

numberExpr :: Parser Expr
numberExpr = label "number" $
  lexeme $ do
    digits <- some numberChar
    let value = read digits :: Integer
    pure (Number value)

booleanExpr :: Parser Expr
booleanExpr = label "boolean" $ 
  Boolean <$> (trueToken <|> falseToken)

nullExpr :: Parser Expr
nullExpr = nullToken $> Null

undefinedExpr :: Parser Expr
undefinedExpr = undefinedToken $> Undefined

identifierExpr :: Parser Expr
identifierExpr = Identifier <$> identifier

-- atom <- length | call | arrayLiteral | arrayLookup | scalar | LEFT_PAREN expression RIGHT_PAREN
atomExpr :: Parser Expr
atomExpr =
  lengthExpr
    <|> try callExpr
    <|> try arrayLookupExpr
    <|> arrayLiteralExpr
    <|> scalarExpr
    <|> parenthesized expr
    <?> "atom"

-- unary <- NOT? atom
unaryExpr :: Parser Expr
unaryExpr = label "unary" $ do
  not <- optional notToken
  atom <- atomExpr
  case not of
    Nothing -> pure atom
    Just _ -> pure (Not atom)

-- parses a left-associative infix operator
parseInfix :: Parser (a -> a -> a) -> Parser a -> Parser a
parseInfix operatorParser termParser = do
  first <- termParser
  operatorTerms <- many $ do
    operator <- operatorParser
    term <- termParser
    pure (operator, term)
  let expr = foldl' (\left (op, term) -> op left term) first operatorTerms
  pure expr

-- product <- unary ((STAR / SLASH) unary)*
productExpr :: Parser Expr
productExpr = (parseInfix (star <|> slash) unaryExpr <?> "product")

-- sum <- product ((PLUS / MINUS) product)*
sumExpr :: Parser Expr
sumExpr = (parseInfix (plus <|> minus) productExpr <?> "sum")

-- comparison <- sum ((EQUAL / NOT_EQUAL) sum)*
comparisonExpr :: Parser Expr
comparisonExpr = (parseInfix (equal <|> notEqual) sumExpr <?> "comparison")

expr :: Parser Expr
expr = comparisonExpr <?> "expression"

{- Statement Parser Grammar:

  returnStatement <- RETURN expression SEMICOLON
  expressionStatement <- expression SEMICOLON
  ifStatement <-
    IF LEFT_PAREN expression RIGHT_PAREN
    statement
    ELSE statement
  whileStatement <- WHILE LEFT_PAREN expression RIGHT_PAREN statement
  varStatement <- VAR ID ASSIGN expression SEMICOLON
  assignmentStatement <- ID ASSIGN EXPRESSION SEMICOLON
  blockStatement <- LEFT_BRACE statement* RIGHT_BRACE
  parameters <- (ID (COMMA ID)*)?
  functionStatement <-
    FUNCTION ID LEFT_PAREN parameters RIGHT_PAREN
    blockStatement
  statement <-
    returnStatement
    | ifStatement
    | whileStatement
    | varStatement
    | assignmentStatemnt
    | blockStatement
    | functionStatement
    | expressionStatement

 -}

returnStatement :: Parser Expr
returnStatement = label "return statement" $ do
  _ <- returnToken
  term <- expr
  _ <- semicolon
  pure (Return term)

expressionStatement :: Parser Expr
expressionStatement = label "expression statement" $ do
  term <- expr
  _ <- semicolon
  pure term

ifStatement :: Parser Expr
ifStatement = label "if statement" $ do
  _ <- ifToken
  condition <- parenthesized expr
  consequent <- statement
  _ <- elseToken
  alternate <- statement
  pure (If condition consequent alternate)

whileStatement :: Parser Expr
whileStatement = label "while statement" $ do
  _ <- whileToken
  condition <- parenthesized expr
  body <- statement
  pure (While condition body)

varStatement :: Parser Expr
varStatement = label "var statement" $ do
  _ <- varToken
  name <- identifier
  _ <- assign
  value <- expr
  _ <- semicolon
  pure (Var name value)

assignmentStatement :: Parser Expr
assignmentStatement = label "assignment statement" $ do
  name <- identifier
  _ <- assign
  value <- expr
  _ <- semicolon
  pure (Assign name value)

blockStatement :: Parser Expr
blockStatement = label "block statement" $ do
  _ <- leftBrace
  statements <- many statement
  _ <- rightBrace
  pure (Block statements)

functionStatement :: Parser Expr
functionStatement = label "function statement" $ do
  _ <- functionToken
  name <- identifier
  params <- parenthesized (identifier `sepBy` comma)
  block <- blockStatement
  pure (Function name params block)

statement :: Parser Expr
statement =
  returnStatement
    <|> functionStatement
    <|> ifStatement
    <|> whileStatement
    <|> varStatement
    <|> try assignmentStatement
    <|> blockStatement
    <|> expressionStatement

statements :: Parser Expr
statements = do
  skipSpace
  stmts <- many statement
  eof
  pure (Block stmts)

parseProgram :: Text -> FilePath -> Either String Expr
parseProgram input path =
  let outputE = parse statements path input
   in case outputE of
        Left err -> Left (errorBundlePretty err)
        Right output -> Right output

parseProgramFile :: FilePath -> IO (Either String Expr)
parseProgramFile path = do
  source <- TIO.readFile path
  pure (parseProgram source path)
