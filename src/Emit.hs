{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns #-}

module Emit where

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
import Data.Text (Text)
import Expr (Expr(..))
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (forM_)

data CompilerError
  = Unsupported Text
  | BadSyntax Text
  | UndefinedVariable Text
  deriving (Eq, Show)

data Context = Context 
  { locals :: Map Text Int, 
    labelCounter :: Int, 
    nextLocalOffset :: Int
  }

type Asm = Builder

newtype Emit a = Emit {unEmit :: StateT Context (WriterT Asm (ExceptT CompilerError Identity)) a}
  deriving (Functor, Applicative, Monad, MonadState Context, MonadError CompilerError, MonadWriter Asm)

execEmit :: Context -> Emit a -> Either CompilerError Asm
execEmit state emit = runIdentity (runExceptT (execWriterT (execStateT (unEmit emit) state)))

execEmitDefault :: Emit a -> Either CompilerError Asm
execEmitDefault = execEmit defaultContext

emit :: Expr -> Emit ()
emit expr = case expr of
  Assert condition -> do
    emit condition
    addLine "  cmp r0, #1"      -- compare to 1 to see if truthy
    addLine "  moveq r0, #'.'"  -- save ASCII dot to signify success
    addLine "  movne r0, #'F'"  -- save code F to signify failure
    addLine "  bl putchar"      -- call libc putchar to print code
  
  Number val ->
    -- load integer into r0, use ldr in case value can't fit in immediate.
    addLine ("  ldr r0, =" <> trimDouble (toText val))

  Not expr -> do
    emit expr 
    addLine "  cmp r0, #0"    -- compare expr to zero
    addLine "  moveq r0, #1"  -- move 1 (true) into r0 if expr is 0
    addLine "  movne r0, #0"  -- move 0 (false) into r0 if expr is 1

  Add left right -> emitInfix left right $
    addLine "  add r0, r1, r0"

  Subtract left right -> emitInfix left right $
    addLine "  sub r0, r1, r0"

  Multiply left right -> emitInfix left right $
    addLine "  mul r0, r1, r0"

  Divide left right -> emitInfix left right $
    addLine "  udiv r0, r1, r0"

  Equal left right -> emitInfix left right $ do
    addLine "  cmp r1, r0"    -- compare left to right
    addLine "  moveq r0, #1"  -- if equal, store 1
    addLine "  movne r0, #0"  -- otherwise store 0

  NotEqual left right -> emitInfix left right $ do
    addLine "  cmp r1, r0"    -- compare left to right
    addLine "  moveq r0, #0"  -- if equal, store 0
    addLine "  movne r0, #1"  -- otherwise store 1

  Call callee args -> do
    let count = length args
    if count == 0 then
      addLine ("  bl " <> callee) -- branch and link to function name
    else if count == 1 then do
      emit (head args) -- if one arg, just emit it
      addLine ("  bl " <> callee)
    else if count >= 2 && count <= 4 then do
      addLine "  sub sp, sp, #16" -- allocate 16 bytes for up to four args
      forM_ (zip args [0..4]) $ \(arg, i) -> do
        emit arg
        addLine ("  str r0, [sp, #" <> (toText (4 * i)) <> "]") -- store each arg in stack, offset in multiples of 4
      addLine "  pop {r0, r1, r2, r3}" -- pop args from stack into registers
      addLine ("  bl " <> callee)
    else
      throwError (Unsupported "More than 4 arguments not supported")
    
  If condition consequent alternate -> do
    ifFalseLabel <- makeLabel -- unique label for falsey branch
    endIfLabel <- makeLabel   -- unique label for after if is evaluated
    emit condition            -- emit condition code
    addLine "  cmp r0, #0"    -- check if condition is false
    addLine ("  beq " <> ifFalseLabel) -- if so, branch to ifFalse label
    emit consequent           -- emit consequent, executed only if we do not branch to ifFalse
    addLine ("  b " <> endIfLabel) -- branch to endIf label either way
    addLine (ifFalseLabel <> ":")  -- define ifFalse label
    emit alternate                 -- ifFalse label contains alternate code
    addLine (endIfLabel <> ":")    -- define endIf label with whatever comes next

  Function name params body -> do
    if length params > 4 then 
      throwError (Unsupported "More than 4 params not supported")
    else do
      addLine ""
      addLine (".global " <> name)
      addLine (name <> ":")
      -- prologue
      addLine "  push {fp, lr}"  -- save the current frame pointer and link register
      addLine "  mov fp, sp"     -- set new frame pointer to current stack pointer
      addLine "  push {r0, r1, r2, r3}" -- save argument registers
      -- body
      withContext (bind params) (emit body)
      -- epilogue
      addLine "  mov sp, fp"   -- deallocate stack space used for current frame
      addLine "  mov r0, #0"   -- implicitly set return value to 0
      addLine "  pop {fp, pc}" -- restore fp, pop the saved link register into pc to return

  Identifier name -> do
    withVarLookup name $ \offset ->
      addLine ("  ldr r0, [fp, #" <> toText offset <> "]")
  
  Return term -> do
    emit term
    addLine "  mov sp, fp"   -- reset the stack pointer to current frame pointer
    addLine "  pop {fp, pc}" -- reset fp, pop lr into pc to return

  Var name value -> do
    emit value
    addLine "  push {r0, ip}"
    Context{locals, nextLocalOffset} <- get
    setLocals (Map.insert name (nextLocalOffset - 4) locals)
    setNextLocalOffset (nextLocalOffset - 8)

  Assign name value -> do
    emit value
    withVarLookup name $ \offset ->
      addLine ("  str r0, [fp, #" <> toText offset <> "]")

  While condition body -> do
    loopStart <- makeLabel
    loopEnd <- makeLabel
    addLine (loopStart <> ":") -- define loopStart label
    emit condition
    addLine "  cmp r0, #0"  -- compare condition to 0
    addLine ("  beq " <> loopEnd) -- if falsey, branch to loopEnd
    emit body
    addLine ("  b " <> loopStart) -- branch back to loopStart after body
    addLine (loopEnd <> ":") -- define loopEnd label with remaining code

  Block stmts ->
    forM_ stmts emit

addLine :: Text -> Emit ()
addLine ln = 
  let builder = Builder.fromText (ln <> "\n")
   in pass (pure ((), (<> builder)))

emitInfix :: Expr -> Expr -> Emit () -> Emit ()
emitInfix left right action = do
  emit left
  addLine "  push {r0, ip}"     -- push left (r0) onto stack with alignment
  emit right
  addLine "  pop {r1, ip}"      -- restore left result from stack
  action                      -- perform action on r0 and r1

incrementLabelCounter :: Emit ()
incrementLabelCounter = modify (\s -> s {labelCounter = (labelCounter s) + 1})

makeLabel :: Emit Text
makeLabel = do
  incrementLabelCounter
  counter <- gets labelCounter
  pure (".L" <> toText counter)

defaultContext :: Context
defaultContext = Context Map.empty 0 0

bind :: [Text] -> Context
bind params = defaultContext {locals = locals, nextLocalOffset = nextLocalOffset}
  where
    offsets = [4 * i - 16 | i <- [0..]]
    locals = Map.fromList (zip params offsets)
    nextLocalOffset = -20 -- 4 bytes after the allocated 4 params

setLocals :: Map Text Int -> Emit ()
setLocals locals = modify (\s -> s {locals = locals})

setNextLocalOffset :: Int -> Emit ()
setNextLocalOffset offset = modify (\s -> s {nextLocalOffset = offset})

withContext :: Context -> Emit () -> Emit ()
withContext ctx action = do
  oldCtx <- get
  put ctx
  action
  put oldCtx

withVarLookup :: Text -> (Int -> Emit ()) -> Emit ()
withVarLookup name withOffset = do
  env <- gets locals
  case Map.lookup name env of
    Just offset -> 
      withOffset offset
    Nothing ->
      throwError (UndefinedVariable name)

toText :: Show a => a -> Text
toText = T.pack . show

trimDouble :: Text -> Text
trimDouble txt =
  case T.stripSuffix ".0" txt of
    Just digits -> digits
    Nothing -> txt