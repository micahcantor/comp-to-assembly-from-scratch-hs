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
    it "compiles if-else.js" $ do
      let src = "test/data/if-else.js"
      out <- runAsm src
      out `shouldBe` ".."
    
    it "compiles factorial.js" $ do
      let src = "test/data/factorial.js"
      out <- runAsm src
      out `shouldBe` "."

spec :: Spec
spec =
  testCompile