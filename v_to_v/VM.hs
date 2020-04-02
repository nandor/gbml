-- This file is part of the GBC-ML project.
-- Licensing information is available in the LICENSE file.
-- (C) 2020 Nandor Licker. All rights reserved.

module VM where

import qualified AST


type DeclMap = [(String, Integer)]

data LogicalOp
  = And | Or | Xor
  deriving (Show)

data ArithmeticOp
  = Add | Sub
  deriving (Show)

data ComparisonOp
  = Gt | Eq | Ne | EqZ
  deriving (Show)

data Expr
  = Ident String Integer
  | Extend Expr Integer Integer
  | Range Expr Integer Integer Integer
  | Arithmetic ArithmeticOp Expr Expr Integer
  | Comparison ComparisonOp Expr Expr Integer
  | Logical LogicalOp Expr Expr Integer
  | Shl Expr Expr Integer Integer
  | Not Expr Integer
  | HorzOr Expr Integer
  | Const AST.Value
  | Ternary Expr Expr Expr Integer
  | Cons [Expr] Integer
  deriving (Show)

data Flow
  = If Expr Flow Flow
  | Seq Flow Flow
  | Assign String Expr
  | Nop
  deriving (Show)

data Update
  = Update (Maybe [(AST.Edge, String)]) Flow
  deriving (Show)

data Module
  = Module
    { inWires    :: DeclMap
    , outWires   :: DeclMap
    , outRegs    :: DeclMap
    , stateWires :: DeclMap
    , stateRegs  :: DeclMap
    , updates    :: [Update]
    }
  deriving (Show)
