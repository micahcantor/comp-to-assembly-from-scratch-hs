{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Expr
import Parser
import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ

testParse :: Parser a -> Text -> a
testParse p src =
  case parse p "ParserSpec" src of
    Left err -> error (errorBundlePretty err)
    Right expr -> expr

testNumberExpr :: Spec
testNumberExpr = do
  describe "numberExpr" $ do
    it "parses 0" $ do
      testParse expr "0" `shouldBe` Number 0

    it "parses 42" $ do
      testParse expr "42" `shouldBe` Number 42

testIdentifierExpr :: Spec
testIdentifierExpr = do
  describe "identifierExpr" $ do
    it "parses x" $ do
      testParse expr "x" `shouldBe` Identifier "x"

    it "parses hello" $ do
      testParse expr "hello" `shouldBe` Identifier "hello"

    it "parses _hello" $ do
      testParse expr "_hello" `shouldBe` Identifier "_hello"

testBooleanExpr :: Spec
testBooleanExpr = do
  describe "booleanExpr" $ do
    it "parses true" $ do
      testParse expr "true" `shouldBe` Boolean True

    it "parses false" $ do
      testParse expr "false" `shouldBe` Boolean False

testNullExpr :: Spec
testNullExpr = do
  describe "nullExpr" $ do
    it "parses null" $ do
      testParse expr "null" `shouldBe` Null

testUndefinedExpr :: Spec
testUndefinedExpr = do
  describe "undefinedExpr" $ do
    it "parses undefined" $ do
      testParse expr "undefined" `shouldBe` Undefined

testArrayLiteralExpr :: Spec
testArrayLiteralExpr = do
  describe "arrayLiteralExpr" $ do
    it "parses [1, 2, 3]" $ do
      testParse expr "[1, 2, 3]" `shouldBe` ArrayLiteral (map Number [1, 2, 3])

    it "parses [x, y]" $ do
      testParse expr "[x, y]" `shouldBe` ArrayLiteral (map Identifier ["x", "y"])

    it "parses [x, 1, y, 2]" $ do
      testParse expr "[x, 1, y, 2]" `shouldBe` ArrayLiteral [Identifier "x", Number 1, Identifier "y", Number 2]

    it "parses []" $ do
      testParse expr "[]" `shouldBe` ArrayLiteral []

testArrayLookupExpr :: Spec
testArrayLookupExpr = do
  describe "arrayLookupExpr" $ do
    it "parses arr[0]" $ do
      testParse expr "arr[0]" `shouldBe` ArrayLookup (Identifier "arr") (Number 0)
    
    it "parses arr[1 + 2]" $ do
      testParse expr "arr[1 + 2]" `shouldBe` ArrayLookup (Identifier "arr") (Add (Number 1) (Number 2))

    it "parses arr[i]" $ do
      testParse expr "arr[i]" `shouldBe` ArrayLookup (Identifier "arr") (Identifier "i")

    it "parses [1, 2, 3][0]" $ do
      testParse expr "[1, 2, 3][0]" `shouldBe` ArrayLookup (ArrayLiteral (map Number [1, 2, 3])) (Number 0)


testNotToken :: Spec
testNotToken = do
  describe "notToken" $ do
    it "parses !x" $ do
      testParse expr "!x" `shouldBe` Not (Identifier "x")

    it "parses !42" $ do
      testParse expr "!42" `shouldBe` Not (Number 42)

testEqualToken :: Spec
testEqualToken = do
  describe "equal" $ do
    it "parses x == y" $ do
      testParse expr "x == y" `shouldBe` Equal (Identifier "x") (Identifier "y")

    it "parses 2 == 4" $ do
      testParse expr "2 == 4" `shouldBe` Equal (Number 2) (Number 4)

testNotEqualToken :: Spec
testNotEqualToken = do
  describe "notEqual" $ do
    it "parses 10 != 25" $ do
      testParse expr "10 != 25" `shouldBe` NotEqual (Number 10) (Number 25)

    it "parses x != y" $ do
      testParse expr "x != y" `shouldBe` NotEqual (Identifier "x") (Identifier "y")

testPlusToken :: Spec
testPlusToken = do
  describe "plus" $ do
    it "parses 2 + 2" $ do
      testParse expr "2 + 2" `shouldBe` Add (Number 2) (Number 2)

testMinusToken :: Spec
testMinusToken = do
  describe "minus" $ do
    it "parses 2 - 2" $ do
      testParse expr "2 - 2" `shouldBe` Subtract (Number 2) (Number 2)

testStarToken :: Spec
testStarToken = do
  describe "star" $ do
    it "parses 2 * 2" $ do
      testParse expr "2 * 2" `shouldBe` Multiply (Number 2) (Number 2)

testSlashToken :: Spec
testSlashToken = do
  describe "slash" $ do
    it "parses 2 / 2" $ do
      testParse expr "2 / 2" `shouldBe` Divide (Number 2) (Number 2)

testNested :: Spec
testNested = do
  describe "infix" $ do
    it "parses 1+1 == 2" $ do
      testParse expr "1+1 == 2" `shouldBe` Equal (Add (Number 1) (Number 1)) (Number 2)

    it "parses 1 - 1 == 0" $ do
      testParse expr "1-1 == 0" `shouldBe` Equal (Subtract (Number 1) (Number 1)) (Number 0)

    it "parses 3 - 2 == 1" $ do
      testParse expr "3 - 2 == 1" `shouldBe` Equal (Subtract (Number 3) (Number 2)) (Number 1)

    it "parses 1 + (1+2) == 4" $ do
      testParse expr "1+(1+2) == 4" `shouldBe` Equal (Add (Number 1) (Add (Number 1) (Number 2))) (Number 4)

    it "parses (1+2) + 2 == 4" $ do
      testParse expr "(1+2) + 2 == 4" `shouldBe` Equal (Add (Add (Number 1) (Number 2)) (Number 2)) (Number 4)

    it "parses 1 * 2 + 1 == 3" $ do
      testParse expr "1 * 2 + 1 == 3" `shouldBe` Equal (Add (Multiply (Number 1) (Number 2)) (Number 1)) (Number 3)

    it "parses 42 == 4 + 2 * (12 - 2) + 3 * (5 + 1)" $ do
      testParse expr "42 == 4 + 2 * (12 - 2) + 3 * (5 + 1)"
        `shouldBe` Equal
          (Number 42)
          ( Add
              (Add (Number 4) (Multiply (Number 2) (Subtract (Number 12) (Number 2))))
              (Multiply (Number 3) (Add (Number 5) (Number 1)))
          )

testCallExpr :: Spec
testCallExpr = do
  describe "callExpr" $ do
    it "parses f(x, y)" $ do
      testParse expr "f(x, y)" `shouldBe` Call "f" [Identifier "x", Identifier "y"]

    it "parses factorial(n-1)" $ do
      testParse expr "factorial(n - 1)" `shouldBe` Call "factorial" [Subtract (Identifier "n") (Number 1)]

testReturnStatement :: Spec
testReturnStatement = do
  describe "returnStatement" $ do
    it "parses return 0" $ do
      testParse statement "return 0;" `shouldBe` Return (Number 0)

    it "parses return n-1" $ do
      testParse statement "return n-1;" `shouldBe` Return (Subtract (Identifier "n") (Number 1))

    it "parses return false" $ do
      testParse statement "return false;" `shouldBe` Return (Boolean False)

    it "parses return null" $ do
      testParse statement "return null;" `shouldBe` Return Null

testBlockStatement :: Spec
testBlockStatement = do
  describe "blockStatement" $ do
    it "parses empty block" $ do
      testParse statement "{}" `shouldBe` Block []

    it "parses two-line block" $ do
      testParse statement block `shouldBe` Block [Call "f" [Identifier "x"], Return (Identifier "y")]
  where
    block =
      [r|{
        f(x);
        return y;
      }|]

testIfStatement :: Spec
testIfStatement = do
  describe "ifStatement" $ do
    it "parses if statement with blocks" $ do
      testParse statement ifStatementBlocks
        `shouldBe` If
          (Equal (Identifier "x") (Number 42))
          (Block [Return (Identifier "x")])
          (Block [Return (Identifier "y")])

    it "parses if statement without blocks" $ do
      testParse statement ifStatementExpressions
        `shouldBe` If
          (Equal (Identifier "x") (Number 42))
          (Return (Identifier "x"))
          (Return (Identifier "y"))
  where
    ifStatementBlocks =
      [r|if (x==42) {
          return x;
        } else {
          return y;
        }|]
    ifStatementExpressions =
      [r|if (x == 42)
          return x;
         else 
          return y;
      |]

testFunctionStatement :: Spec
testFunctionStatement = do
  describe "functionStatement" $ do
    it "parses function declaration" $ do
      testParse statement functionDeclaration
        `shouldBe` Function "f" ["x", "y"] (Block [Return (Identifier "y")])

    it "parses function with zero parameters" $ do
      testParse statement functionDeclarationNoParams
        `shouldBe` Function "f" [] (Block [Return (Number 0)])
  where
    functionDeclaration =
      [r|function f(x, y) {
          return y;
        }|]

    functionDeclarationNoParams =
      [r|function f() {
          return 0;
        }|]

testVarStatement :: Spec
testVarStatement = do
  describe "varStatement" $ do
    it "parses variable declaration to addition" $ do
      testParse statement "var y = a + b;"
        `shouldBe` Var "y" (Add (Identifier "a") (Identifier "b"))

    it "parses variable declaration to number" $ do
      testParse statement "var x = 42;"
        `shouldBe` Var "x" (Number 42)

testAssignmentStatement :: Spec
testAssignmentStatement = do
  describe "assignmentStatement" $ do
    it "parses assignment to number" $ do
      testParse statement "x = 42;" `shouldBe` Assign "x" (Number 42)

    it "parses assignment to addition" $ do
      testParse statement "y = a + b;"
        `shouldBe` Assign "y" (Add (Identifier "a") (Identifier "b"))

testWhileStatement :: Spec
testWhileStatement = do
  describe "whileStatement" $ do
    it "parses while statement with variable" $ do
      testParse statement whileStatementVariable
        `shouldBe` While (Identifier "x") (Block [Call "f" []])

    it "parses while statement with comparison" $ do
      testParse statement whileStatementComparison
        `shouldBe` While (Equal (Identifier "n") (Number 1)) (Block [Call "f" []])
  where
    whileStatementVariable =
      [r|while(x) {
          f();
        }
      |]

    whileStatementComparison =
      [r|while(n == 1) {
          f();
        }
      |]

spec :: Spec
spec = do
  testNumberExpr
  testIdentifierExpr
  testNotToken
  testEqualToken
  testBooleanExpr
  testNullExpr
  testUndefinedExpr
  testArrayLiteralExpr
  testArrayLookupExpr
  testNotEqualToken
  testPlusToken
  testMinusToken
  testStarToken
  testSlashToken
  testNested
  testCallExpr
  testReturnStatement
  testBlockStatement
  testIfStatement
  testFunctionStatement
  testVarStatement
  testAssignmentStatement
  testWhileStatement
