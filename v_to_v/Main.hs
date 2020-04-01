-- This file is part of the GBC-ML project.
-- Licensing information is available in the LICENSE file.
-- (C) 2020 Nandor Licker. All rights reserved.

module Main where

import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit

import Translate
import Parser


data Options
  = Options
  { output :: Maybe String
  }
  deriving (Show)

options :: [OptDescr (Options -> Options)]
options =
  [ Option
    ['o']
    ["output"]
    (ReqArg (\arg opt -> opt { output = Just arg }) "FILE")
    "output to FILE"
  ]

defaultOpts :: Options
defaultOpts = Options
  { output = Nothing
  }

main :: IO ()
main = do
  args <- getArgs
  let (actions, files, errors) = getOpt Permute options args
  let opts = foldl (flip id) defaultOpts actions
  case (opts, files, errors) of
    (Options { output = Just path }, [file], []) -> do
      source <- readFile file
      case parseVerilog file source of
        Left err -> do
          putStrLn (show err)
          exitWith (ExitFailure 1)
        Right ast ->
          putStrLn (show (translate ast))
    (_, files, errs) -> do
      putStrLn (concat errs ++ usageInfo "v_to_v [ARGS] INPUT" options)
      exitWith (ExitFailure 255)
