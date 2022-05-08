module Compile where

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TIO
import Emit (emit, execEmitDefault)
import Parser (parseProgramFile)

compile :: FilePath -> FilePath -> IO ()
compile inPath outPath = do
  parseOutput <- parseProgramFile inPath
  case parseOutput of
    Left err -> putStr err
    Right ast -> do
      let code = emit ast
          compileOutput = execEmitDefault code
      case compileOutput of
        Left err -> print err
        Right asm -> TIO.writeFile outPath (T.stripStart (toLazyText asm))