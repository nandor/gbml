-- This file is part of the GBC-ML project.
-- Licensing information is available in the LICENSE file.
-- (C) 2020 Nandor Licker. All rights reserved.

module VM where

import qualified AST


data Record
  = Record [(String, Integer)]
  deriving (Show)

data LogicalOp
  = And | Or | Xor
  deriving (Show)

data ArithmeticOp
  = Add | Sub
  deriving (Show)

data ComparisonOp
  = Gt | Eq | Ne
  deriving (Show)

data Expr
  = Ident String Integer
  | Extend Expr Integer Integer
  | Range Expr Integer Integer Integer
  | Arithmetic ArithmeticOp Expr Expr Integer
  | Comparison ComparisonOp Expr Expr Integer
  | Logical LogicalOp Expr Expr Integer
  | Not Expr Integer
  | HorzOr Expr Integer
  | Const AST.Value
  | Ternary Expr Expr Expr Integer
  deriving (Show)

data Flow
  = Case Expr (Maybe Flow) (Maybe Flow) (Maybe Flow) (Maybe Flow)
  | Let Flow Flow
  | With String Expr
  deriving (Show)

data Module
  = Module
    { stateTy :: Record
    , inputTy :: Record
    , outputTy :: Record
    , wires :: [(String, Expr)]
    }
  deriving (Show)
