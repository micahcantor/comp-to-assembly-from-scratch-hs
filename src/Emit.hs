{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Emit where

import Control.Monad.Except
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
import Data.Text (Text)
import Expr
import qualified Data.Text as T

data CompilerError
  = CompilerError Text
  | BadSyntax Text
  | Default
  deriving (Eq, Show)

newtype Emit a = Emit {unEmit :: StateT Text (ExceptT CompilerError Identity) a}
  deriving (Functor, Applicative, Monad, MonadState Text, MonadError CompilerError)

runEmit :: Text -> Emit a -> Either CompilerError (a, Text)
runEmit source emit = runIdentity (runExceptT (runStateT (unEmit emit) source))

execEmit :: Text -> Emit a -> Either CompilerError Text
execEmit source emit = runIdentity (runExceptT (execStateT (unEmit emit) source))

execEmitDefault :: Emit a -> Either CompilerError Text
execEmitDefault = execEmit ""

addLine :: Text -> Emit ()
addLine ln = modify (<> ln <> "\n")

toText :: Show a => a -> Text
toText = T.pack . show

emit :: Expr -> Emit ()
emit expr = case expr of
  Main _ -> emitMain expr
  Assert _ -> emitAssert expr
  Number _ -> emitNumber expr
  Block _ -> emitBlock expr
  _ -> throwError Default

emitMain :: Expr -> Emit ()
emitMain = \case
  Main block -> do
    case block of
      Block stmts -> do
        addLine ".global main"
        addLine "main:"           -- define main procedure
        addLine "  push {fp, lr}" -- push lr, align stack with fp
        forM_ stmts emit
        addLine "  mov r0, #0"    -- set return value to 0
        addLine "  pop {fp, pc}"  -- restore fp, push lr into pc to return from main
      _ -> throwError (BadSyntax "main")
  _ -> throwError Default

emitAssert :: Expr -> Emit ()
emitAssert = \case
  Assert condition -> do
    emit condition
    addLine "  cmp r0, #1"      -- compare to 1 to see if truthy
    addLine "  moveq r0, #'.'"  -- save ASCII dot to signify success
    addLine "  movne r0, #'F'"  -- save code F to signify failure
    addLine "  bl putchar"      -- call libc putchar to print code
  _ -> throwError Default

emitNumber :: Expr -> Emit ()
emitNumber = \case
  Number val ->
    -- load integer into r0, use ldr in case value can't fit in immediate.
    addLine ("  ldr r0, =" <> toText val) 
  _ -> throwError Default

emitBlock :: Expr -> Emit ()
emitBlock = \case
  Block stmts -> forM_ stmts emit
  _ -> throwError Default