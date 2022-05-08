module Main where

import System.Environment (getArgs)
import Compile ( compile )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inPath, outPath] -> compile inPath outPath
    _ -> putStrLn ("Expected two arguments, got '" ++ show args ++ "'.")


