-- This file is part of the GBC-ML project.
-- Licensing information is available in the LICENSE file.
-- (C) 2020 Nandor Licker. All rights reserved.

module AST where


data Type
  = In
  | Out
  | InOut
  deriving (Eq, Show)

data Parameter
  = Parameter Type Bool Integer String
  deriving (Show)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Range Integer Integer
  | Cons [Expr]
  deriving (Show)

data Edge
  = Pos
  | Neg
  | All
  deriving (Eq, Show)

data Item
  = RegDecl String Integer
  | WireDecl String Integer (Maybe Expr)
  | AlwaysBlock [String]
  deriving (Show)

data Module
  = Module
    { name :: String
    , params :: [Parameter]
    , items :: [Item]
    }
  deriving (Show)
