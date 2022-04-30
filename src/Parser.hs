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
    rest <- many (letterChar <|> char '_' <|> digitChar)
    let value = T.pack (firstChar : rest)
    pure value

functionToken :: Parser Text
functionToken = lexeme (string "function")

ifToken :: Parser Text
ifToken = lexeme (string "if")

elseToken :: Parser Text
elseToken = lexeme (string "else")

returnToken :: Parser Text
returnToken = lexeme (string "return")

varToken :: Parser Text
varToken = lexeme (string "var")

whileToken :: Parser Text
whileToken = lexeme (string "while")

comma :: Parser Text
comma = lexeme (string ",")

assign :: Parser Text
assign = lexeme (string "=")

semicolon :: Parser Text
semicolon = lexeme (string ";")

leftParen :: Parser Text
leftParen = lexeme (string "(")

rightParen :: Parser Text
rightParen = lexeme (string ")")

leftBrace :: Parser Text
leftBrace = lexeme (string "{")

rightBrace :: Parser Text
rightBrace = lexeme (string "}")

notSymbol :: Parser (Expr -> Expr)
notSymbol = lexeme $ do
  _ <- string "!"
  pure Not

equal :: Parser (Expr -> Expr -> Expr)
equal = lexeme $ do
  _ <- string "=="
  pure Equal

notEqual :: Parser (Expr -> Expr -> Expr)
notEqual = lexeme $ do
  _ <- string "!="
  pure NotEqual

plus :: Parser (Expr -> Expr -> Expr)
plus = lexeme $ do
  _ <- string "+"
  pure Add

minus :: Parser (Expr -> Expr -> Expr)
minus = lexeme $ do
  _ <- string "-"
  pure Subtract

star :: Parser (Expr -> Expr -> Expr)
star = lexeme $ do
  _ <- string "*"
  pure Multiply

slash :: Parser (Expr -> Expr -> Expr)
slash = lexeme $ do
  _ <- string "/"
  pure Divide

parenthesized :: Parser a -> Parser a
parenthesized p = do
  _ <- leftParen
  expr <- p
  _ <- rightParen
  pure expr

{- Expression Parser Grammar:

  call <- ID LEFT_PAREN args RIGHT_PAREN
  atom <- call | ID | NUMBER | LEFT_PAREN expression RIGHT_PAREN
  unary <- NOT? atom
  product <- unary ((STAR / SLASH) unary)*
  sum <- product ((PLUS / MINUS) product)*
  comparison <- sum ((EQUAL / NOT_EQUAL) sum)*
  expression <- comparison

-}

-- call <- ID LEFT_PAREN args RIGHT_PAREN
callExpr :: Parser Expr
callExpr = label "call" $ do
  callee <- identifier
  args <- parenthesized (expr `sepBy` comma)
  pure (Call callee args)

numberExpr :: Parser Expr
numberExpr = label "number" $
  lexeme $ do
    digits <- some numberChar
    let value = read digits :: Double
    pure (Number value)

identifierExpr :: Parser Expr
identifierExpr = Identifier <$> identifier

-- atom <- call | ID | NUMBER | LEFT_PAREN expression RIGHT_PAREN
atomExpr :: Parser Expr
atomExpr = callExpr <|> identifierExpr <|> numberExpr <|> parenthesized expr

-- unary <- NOT? atom
unaryExpr :: Parser Expr
unaryExpr = do
  not <- optional notSymbol
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
productExpr = parseInfix (star <|> slash) unaryExpr

-- sum <- product ((PLUS / MINUS) product)*
sumExpr :: Parser Expr
sumExpr = parseInfix (plus <|> minus) productExpr

-- comparison <- sum ((EQUAL / NOT_EQUAL) sum)*
comparisonExpr :: Parser Expr
comparisonExpr = parseInfix (equal <|> notEqual) sumExpr

expr :: Parser Expr
expr = comparisonExpr

{- Statement Parser Grammar:

  returnStatement <- RETURN expression SEMICOLON
  expressionStatement <- expression SEMICOLON
  ifStatement <- IF LEFT_PAREN expression RIGHT_PAREN
  statement ELSE statement
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
returnStatement = do
  _ <- returnToken
  term <- expr
  _ <- semicolon
  pure (Return term)

expressionStatement :: Parser Expr
expressionStatement = do
  term <- expr
  _ <- semicolon
  pure term

ifStatement :: Parser Expr
ifStatement = do
  _ <- ifToken
  condition <- parenthesized expr
  consequent <- statement
  _ <- elseToken
  alternate <- statement
  pure (If condition consequent alternate)

whileStatement :: Parser Expr
whileStatement = do
  _ <- whileToken
  condition <- parenthesized expr
  body <- statement
  pure (While condition body)

varStatement :: Parser Expr
varStatement = do
  _ <- varToken
  name <- identifier
  _ <- assign
  value <- expr
  _ <- semicolon
  pure (Var name value)

assignmentStatement :: Parser Expr
assignmentStatement = do
  name <- identifier
  _ <- assign
  value <- expr
  _ <- semicolon
  pure (Assign name value)

blockStatement :: Parser Expr
blockStatement = do
  _ <- leftBrace
  statements <- many statement
  _ <- rightBrace
  pure (Block statements)

functionStatement :: Parser Expr
functionStatement = do
  _ <- functionToken
  name <- identifier
  params <- parenthesized (many identifier)
  block <- blockStatement
  pure (Function name params block)

statement :: Parser Expr
statement =
  returnStatement
    <|> functionStatement
    <|> ifStatement
    <|> whileStatement
    <|> varStatement
    <|> assignmentStatement
    <|> blockStatement
    <|> expressionStatement

parseProgram :: Text -> FilePath -> Either String Expr
parseProgram input filename =
  let outputE = parse (between skipSpace eof (many statement >>= (pure . Block))) filename input
   in case outputE of
        Left err -> Left (errorBundlePretty err)
        Right output -> Right output

parseProgramFile :: FilePath -> IO ()
parseProgramFile path = do
  source <- TIO.readFile path
  case parseProgram source path of
    Left errMsg -> putStr errMsg
    Right expr -> print expr
