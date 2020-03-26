-- This file is part of the GBC-ML project.
-- Licensing information is available in the LICENSE file.
-- (C) 2020 Nandor Licker. All rights reserved.

module Parser (parseVerilog) where

import Data.Char
import Control.Monad
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Error

import Debug.Trace

import AST



comment :: GenParser Char st ()
comment = void $ do
  char '/'
  char '/'
  skipMany (satisfy (\c -> c /= '\n'))
  newline

lexeme :: GenParser Char st a -> GenParser Char st a
lexeme parser = do
  skipMany (skipMany1 space <|> comment)
  parser

commaSep :: GenParser Char st a -> GenParser Char st [a]
commaSep tk = sepBy tk (try $ lexeme (char ','))

decimal :: GenParser Char st Integer
decimal = parse 0
  where
    parse :: Integer -> GenParser Char st Integer
    parse n = optionMaybe digit >>= \case
      Nothing -> return n
      Just n' -> parse (n * 10 + toInteger (digitToInt n'))

keyword :: String -> GenParser Char st ()
keyword kw = lexeme (void (string kw))

symbol :: String -> GenParser Char st ()
symbol sym = lexeme (void (string sym))

identifier :: GenParser Char st String
identifier = lexeme $ do
  start <- upper <|> lower <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return (start : rest)

topmod :: GenParser Char st Module
topmod = do
  keyword "module"
  name <- identifier
  symbol "("
  params <- commaSep $ do
    ty <- msum
      [ try $ keyword "input"  *> return In
      , try $ keyword "output" *> return Out
      , try $ keyword "inout"  *> return InOut
      ]
    reg <- option False (try $ keyword "reg" *> return True)
    traceShowM (ty, reg)
    bits <- option 1 . try $ do
      symbol "["
      bits <- lexeme decimal
      symbol ":"
      symbol "0"
      symbol "]"
      return bits
    ident <- identifier
    traceShowM (ty, reg, bits, ident)
    return $ Parameter ty reg bits ident
  symbol ")"
  symbol ";"
  traceShowM params
  return $ Module undefined params

parseVerilog :: FilePath -> String -> Either ParseError Module
parseVerilog path source = runParser topmod () path source
