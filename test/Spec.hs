module Main where

import qualified ParserSpec
import qualified EmitSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ParserSpec" ParserSpec.spec
  describe "EmitSpec" EmitSpec.spec