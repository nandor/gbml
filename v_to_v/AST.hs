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

data Digit
  = X
  | Z
  | H
  | L
  deriving (Show)

data Value
  = Value [Digit]
  deriving (Show)

data Expr
  = Ternary Expr Expr Expr
  | Or Expr Expr
  | And Expr Expr
  | BitOr Expr Expr
  | BitAnd Expr Expr
  | BitXor Expr Expr
  | Gt Expr Expr
  | Eq Expr Expr
  | Ne Expr Expr
  | Shl Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Not Expr
  | Inv Expr
  | HorzOr Expr
  | Range Expr Integer Integer
  | Index Expr Expr
  | Cons [Expr]
  | Ident String
  | Const Value
  deriving (Show)

data Edge
  = Pos
  | Neg
  | All
  deriving (Eq, Show)

data CaseKind
  = Case | CaseX | CaseZ
  deriving (Show)

data Statement
  = Block [Statement]
  | Switch CaseKind Expr [(Value, Statement)]
  | If Expr Statement (Maybe Statement)
  | NonBlocking [String] Expr
  deriving (Show)

data Item
  = RegDecl String Integer
  | WireDecl String Integer
  | Always [(Edge, Expr)] Statement
  | Assign String Expr
  deriving (Show)

data Module
  = Module
    { name :: String
    , params :: [Parameter]
    , items :: [Item]
    }
  deriving (Show)
