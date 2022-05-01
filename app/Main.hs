module Main where

import Emit
import Parser
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inPath, outPath] -> compile inPath outPath
    _ -> putStrLn ("Expected two arguments, got '" ++ show args ++ "'.")

compile :: FilePath -> FilePath -> IO ()
compile inPath outPath = do
  parseOutput <- parseProgramFile inPath
  case parseOutput of
    Left err -> putStr err
    Right ast -> do
      print parseOutput
      let code = emit ast
          compileOutput = execEmitDefault code
      case compileOutput of
        Left err -> print err
        Right asm -> TIO.writeFile outPath asm
