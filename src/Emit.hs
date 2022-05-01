{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DoAndIfThenElse #-}

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

trimDouble :: Text -> Text
trimDouble txt =
  case T.stripSuffix ".0" txt of
    Just digits -> digits
    Nothing -> txt

emit :: Expr -> Emit ()
emit expr = case expr of
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

  Assert condition -> do
    emit condition
    addLine "  cmp r0, #1"      -- compare to 1 to see if truthy
    addLine "  moveq r0, #'.'"  -- save ASCII dot to signify success
    addLine "  movne r0, #'F'"  -- save code F to signify failure
    addLine "  bl putchar"      -- call libc putchar to print code
  
  Number val ->
    -- load integer into r0, use ldr in case value can't fit in immediate.
    addLine ("  ldr r0, =" <> (trimDouble (toText val))) 

  Not expr -> do
    emit expr 
    addLine "  cmp r0, #0"    -- compare expr to zero
    addLine "  moveq r0, #1"  -- move 1 (true) into r0 if expr is 0
    addLine "  movne r0, #0"  -- move 0 (false) into r0 if expr is 1

  Add left right -> emitInfix left right $
    addLine "add r0, r0, r1"

  Subtract left right -> emitInfix left right $
    addLine "sub r0, r0, r1"

  Multiply left right -> emitInfix left right $
    addLine "mul r0, r0, r1"

  Divide left right -> emitInfix left right $
    addLine "udiv r0, r0, r1"

  Equal left right -> emitInfix left right $ do
    addLine "cmp r0, r1"    -- compare left to right
    addLine "moveq r0, #1"  -- if equal, store 1
    addLine "moveq r0, #0"  -- otherwise store 0

  NotEqual left right -> emitInfix left right $ do
    addLine "cmp r0, r1"    -- compare left to right
    addLine "moveq r0, #0"  -- if equal, store 0
    addLine "moveq r0, #1"  -- otherwise store 1

  Call callee args -> do
    let count = length args
    if count == 1 then
      emit (head args) -- if one arg, just emit it
    else if count >= 2 && count <= 4 then do
      addLine "  sub sp, sp, #16" -- allocate 16 bytes for up to four args
      forM_ (zip args [0..4]) $ \(arg, i) -> do
        emit arg
        addLine ("  str r0, [sp, #" <> (toText (4 * i)) <> "]") -- store each arg in stack, offset in multiples of 4
      addLine "  pop {r0, r1, r2, r3}" -- pop args from stack into registers
    else 
      throwError (CompilerError "More than 4 arguments not supported")
    addLine ("  bl " <> callee) -- branch and link to function name

  Block stmts -> 
    forM_ stmts emit
  _ -> throwError Default

emitInfix :: Expr -> Expr -> Emit () -> Emit ()
emitInfix left right action = do
  emit left
  addLine "push {r0, ip}"     -- push left (r0) onto stack with alignment
  emit right
  addLine "pop {r1, ip}"      -- restore left result from stack
  action                      -- perform action on r0 and r1

