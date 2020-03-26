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

data Module
  = Module
    { name :: String
    , params :: [Parameter]
    }
  deriving (Show)
