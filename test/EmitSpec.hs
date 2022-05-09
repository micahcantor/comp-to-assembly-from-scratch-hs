module EmitSpec where

import Compile ( compile )
import System.FilePath ( (-<.>), dropExtension, replaceDirectory )
import System.Process (callProcess, readProcess)
import Test.Hspec

runAsm :: FilePath -> IO String
runAsm src = do
  let asmSrc = (replaceDirectory src "test/dist") -<.> ".s"
  callProcess "mkdir" ["-p", "test/dist/"]
  compile src asmSrc
  let cc = "arm-none-linux-gnueabihf-gcc"
  let binPath = dropExtension asmSrc
  callProcess cc ["-static", asmSrc, "-o", binPath]
  let qemu = "qemu-arm-static"
  out <- readProcess qemu [binPath] []
  pure out

testCompile :: Spec
testCompile = do
  describe "compile" $ do
    it "compiles number.js" $ do
      let src = "test/data/number.js"
      out <- runAsm src
      out `shouldBe` "."

    it "compiles equal.js" $ do
      let src = "test/data/equal.js"
      out <- runAsm src
      out `shouldBe` ".."

    it "compiles not-equal.js" $ do
      let src = "test/data/not-equal.js"
      out <- runAsm src
      out `shouldBe` ".."

    it "compiles boolean.js" $ do
      let src = "test/data/boolean.js"
      out <- runAsm src
      out `shouldBe` ".."

    it "compiles null.js" $ do
      let src = "test/data/null.js"
      out <- runAsm src
      out `shouldBe` "."

    it "compiles math.js" $ do
      let src = "test/data/math.js"
      out <- runAsm src
      out `shouldBe` "...."

    it "compiles if-else.js" $ do
      let src = "test/data/if-else.js"
      out <- runAsm src
      out `shouldBe` ".."

    it "compiles var.js" $ do
      let src = "test/data/var.js"
      out <- runAsm src
      out `shouldBe` "."

    it "compiles assignment.js" $ do
      let src = "test/data/assignment.js"
      out <- runAsm src
      out `shouldBe` ".."

    it "compiles while.js" $ do
      let src = "test/data/while.js"
      out <- runAsm src
      out `shouldBe` "."

    it "compiles parameters.js" $ do
      let src = "test/data/parameters.js"
      out <- runAsm src
      out `shouldBe` "....."

    it "compiles return.js" $ do
      let src = "test/data/return.js"
      out <- runAsm src
      out `shouldBe` "....."

    it "compiles factorial.js" $ do
      let src = "test/data/factorial.js"
      out <- runAsm src
      out `shouldBe` "."

    it "compiles recursive.js" $ do
      let src = "test/data/recursive.js"
      out <- runAsm src
      out `shouldBe` "."

spec :: Spec
spec =
  testCompile