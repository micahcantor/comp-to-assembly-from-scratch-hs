module Expr where

import Data.Text (Text)

data Expr
  = Number Double
  | Identifier Text
  | Not Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Call Text [Expr] -- name, args
  | Return Expr
  | Block [Expr]
  | If Expr Expr Expr
  | Function Text [Text] Expr -- name, params, body
  | Var Text Expr -- name, value
  | Assign Text Expr -- name, value
  | While Expr Expr -- condition, body
  deriving (Eq, Show)