module EmitSpec where

import Test.Hspec
import Compile
import System.Process
import qualified System.FilePath as FilePath

runAsm :: FilePath -> IO String
runAsm file = do
  let cc = "arm-none-linux-gnueabihf-gcc"
  let binPath = FilePath.dropExtension file
  callProcess cc ["-static", file, "-o", binPath]
  let qemu = "qemu-arm-static"
  out <- readProcess qemu [binPath] []
  pure out

testIfElse :: Spec
testIfElse = do
  describe "end-to-end" $ do
    it "executes if-else.js" $ do
      let src = "test/data/if-else.js"
      let asm = "test/dist/if-else.s"
      compile src asm
      out <- runAsm asm
      out `shouldBe` ".."

spec :: Spec
spec = do
  testIfElse