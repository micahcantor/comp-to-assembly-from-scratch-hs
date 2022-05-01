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
  Main _ -> emitMain expr
  Assert _ -> emitAssert expr
  Number _ -> emitNumber expr
  Not _ -> emitNot expr
  Add _ _ -> emitAdd expr
  Subtract _ _ -> emitSubtract expr
  Multiply _ _ -> emitMultiply expr
  Divide _ _ -> emitDivide expr
  Equal _ _ -> emitEqual expr
  NotEqual _ _ -> emitNotEqual expr
  Call _ _ -> emitCall expr
  Block _ -> emitBlock expr
  _ -> throwError Default

emitMain :: Expr -> Emit ()
emitMain = \case
  
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
    addLine ("  ldr r0, =" <> (trimDouble (toText val))) 
  _ -> throwError Default

emitNot :: Expr -> Emit ()
emitNot = \case
  Not expr -> do
    emit expr 
    addLine "  cmp r0, #0"    -- compare expr to zero
    addLine "  moveq r0, #1"  -- move 1 (true) into r0 if expr is 0
    addLine "  movne r0, #0"  -- move 0 (false) into r0 if expr is 1
  _ -> throwError Default

emitInfix :: Expr -> Expr -> Emit () -> Emit ()
emitInfix left right action = do
  emit left
  addLine "push {r0, ip}"     -- push left (r0) onto stack with alignment
  emit right
  addLine "pop {r1, ip}"      -- restore left result from stack
  action                      -- perform action on r0 and r1

emitAdd :: Expr -> Emit ()
emitAdd = \case
  Add left right -> emitInfix left right $
    addLine "add r0, r0, r1"
  _ -> throwError Default

emitSubtract :: Expr -> Emit ()
emitSubtract = \case
  Subtract left right -> emitInfix left right $
    addLine "sub r0, r0, r1"
  _ -> throwError Default

emitMultiply :: Expr -> Emit ()
emitMultiply = \case
  Multiply left right -> emitInfix left right $
    addLine "mul r0, r0, r1"
  _ -> throwError Default

emitDivide :: Expr -> Emit ()
emitDivide = \case
  Divide left right -> emitInfix left right $
    addLine "udiv r0, r0, r1"
  _ -> throwError Default

emitEqual :: Expr -> Emit ()
emitEqual = \case
  Equal left right -> emitInfix left right $ do
    addLine "cmp r0, r1"    -- compare left to right
    addLine "moveq r0, #1"  -- if equal, store 1
    addLine "moveq r0, #0"  -- otherwise store 0
  _ -> throwError Default

emitNotEqual :: Expr -> Emit ()
emitNotEqual = \case
  NotEqual left right -> emitInfix left right $ do
    addLine "cmp r0, r1"    -- compare left to right
    addLine "moveq r0, #0"  -- if equal, store 0
    addLine "moveq r0, #1"  -- otherwise store 1
  _ -> throwError Default

emitCall :: Expr -> Emit ()
emitCall = \case
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
  _ -> throwError Default

emitBlock :: Expr -> Emit ()
emitBlock = \case
  Block stmts -> forM_ stmts emit
  _ -> throwError Default