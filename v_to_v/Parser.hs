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

import AST



keywords :: [String]
keywords =
  [ "always"
  , "begin"
  , "case"
  , "else"
  , "end"
  , "endcase"
  , "endmodule"
  , "if"
  , "input"
  , "module"
  , "negedge"
  , "or"
  , "output"
  , "posedge"
  , "reg"
  , "reg"
  , "wire"
  , "assign"
  ]

oneLineComment :: GenParser Char st ()
oneLineComment = void $ do
  string "//"
  skipMany (satisfy (\c -> c /= '\n'))
  newline

multiLineComment :: GenParser Char st ()
multiLineComment = void $ do
  string "/*"
  manyTill anyChar (try (string "*/"))

whiteSpace :: GenParser Char st ()
whiteSpace =
  skipMany (skipMany1 space <|> oneLineComment <|> multiLineComment)

lparen = lexeme (void (char '('))
rparen = lexeme (void (char ')'))
lbrace = lexeme (void (char '{'))
rbrace = lexeme (void (char '}'))
lbracket = lexeme (void (char '['))
rbracket = lexeme (void (char ']'))
semi = lexeme (void (char ';'))
colon = lexeme (void (char ':'))

lexeme :: GenParser Char st a -> GenParser Char st a
lexeme parser = do
  x <- parser
  whiteSpace
  return x

commaSep :: GenParser Char st a -> GenParser Char st [a]
commaSep tk = sepBy tk (lexeme (char ','))

decimal :: GenParser Char st Integer
decimal = lexeme $ do
  xs <- many1 digit
  return $ foldl (\n d -> n * 10 + (toInteger (digitToInt d))) 0 xs

keyword :: String -> GenParser Char st ()
keyword kw = lexeme . try $ do
  token <- many1 lower
  if token /= kw
    then unexpected ("unexpected keyword " ++ token)
    else return ()

symbol :: String -> GenParser Char st ()
symbol sym = lexeme . try $ do
  token <- many1 (oneOf "~|&^!+-*=<>?")
  if token /= sym
    then unexpected ("unexpected symbol " ++ token)
    else return ()

identifier :: GenParser Char st String
identifier = lexeme . try $ do
  start <- upper <|> lower <|> char '_'
  rest <- many (alphaNum <|> char '_')
  let ident = start : rest
  if elem ident keywords
    then unexpected ("unexpected keyword " ++ ident)
    else return ident

busWidth :: GenParser Char st Integer
busWidth =
 option 1 $ do
    lbracket
    bits <- decimal
    colon
    _ <- digit
    rbracket
    return (bits + 1)

value :: GenParser Char st Value
value = lexeme . try $ do
  width <- decimal
  char '\''
  val <- msum
    [ char 'b' *> many1 (dchi <|> oneOf "01") >>=
        \xs -> return $ binary width (reverse xs)
    , char 'd' *> many1 digit    >>=
        number width 10
    , char 'h' *> many1 hexDigit >>=
        number width 16
    ]
  return $ Value val
  where
    dchi = char 'x' <|> char '?'

    number width base xs =
      let n = foldl (\n d -> n * base + toInteger (digitToInt d)) 0 xs in
      return $ toBinary width n []

    binary 0 _ = []
    binary n [] = X : binary (n - 1) []
    binary n (d:ds) = toDigit d : binary (n - 1) ds

    toDigit '?' = Z
    toDigit 'x' = X
    toDigit '0' = L
    toDigit '1' = H

    toBinary 0 _ acc = reverse acc
    toBinary d n acc =
      let n' = n `div` 2 in
      toBinary (d - 1) n' ((if n `mod` 2 == 0 then L else H) : acc)

expr :: GenParser Char st Expr
expr = try ternary <|> or
  where
    ternary = do
      cond <- or
      symbol "?"
      vt <- expr
      colon
      vf <- expr
      return $ Ternary cond vt vf

    or = do
      x <- and
      xs <- many $ do
        symbol "||"
        arg <- and
        return $ \lhs -> Or lhs arg
      return $ foldl (\x f -> f x) x xs

    and = do
      x <- bitOr
      xs <- many $ do
        symbol "&&"
        arg <- bitOr
        return $ \lhs -> And lhs arg
      return $ foldl (\x f -> f x) x xs

    bitOr = do
      x <- bitXor
      xs <- many $ do
        symbol "|"
        arg <- bitXor
        return $ \lhs -> BitOr lhs arg
      return $ foldl (\x f -> f x) x xs

    bitXor = do
      x <- bitAnd
      xs <- many $ do
        symbol "^"
        arg <- bitAnd
        return $ \lhs -> BitXor lhs arg
      return $ foldl (\x f -> f x) x xs

    bitAnd = do
      x <- neExpr
      xs <- many $ do
        symbol "&"
        arg <- neExpr
        return $ \lhs -> BitAnd lhs arg
      return $ foldl (\x f -> f x) x xs

    neExpr = do
      x <- eqExpr
      xs <- many $ do
        op <- msum
          [ symbol ">" *> return Gt
          ]
        arg <- eqExpr
        return $ \lhs -> op lhs arg
      return $ foldl (\x f -> f x) x xs

    eqExpr = do
      x <- shiftExpr
      xs <- many $ do
        op <- msum
          [ symbol "==" *> return Eq
          , symbol "!=" *> return Ne
          ]
        arg <- shiftExpr
        return $ \lhs -> op lhs arg
      return $ foldl (\x f -> f x) x xs

    shiftExpr = do
      x <- additiveExpr
      xs <- many $ do
        op <- msum
          [ symbol "<<" *> return Shl
          ]
        arg <- additiveExpr
        return $ \lhs -> Shl lhs arg
      return $ foldl (\x f -> f x) x xs

    additiveExpr = do
      x <- prefixExpr
      xs <- many $ do
        op <- msum
          [ symbol "+" *> return Add
          , symbol "-" *> return Sub
          ]
        arg <- prefixExpr
        return $ \lhs -> op lhs arg
      return $ foldl (\x f -> f x) x xs

    prefixExpr = msum
      [ do
        symbol "!"
        arg <- prefixExpr
        return $ Not arg
      , do
        symbol "|"
        arg <- prefixExpr
        return $ HorzOr arg
      , do
        symbol "~"
        arg <- prefixExpr
        return $ Inv arg
      , rangeOrIndexExpr
      ]

    rangeOrIndexExpr = do
      base <- atomExpr
      rangesOrIndices <- many $ do
        lbracket
        msum
          [ try $ do
            idx <- decimal
            colon
            en <- decimal
            rbracket
            return $ \e -> Range e idx en
          , do
            idx <- decimal
            rbracket
            return $ \e -> Index e idx
          ]
      return $ foldl (\x f -> f x) base rangesOrIndices

    atomExpr = msum
      [ (lparen *> expr <* rparen)
      , consExpr
      , (Const <$> value)
      , identExpr
      ]

    identExpr = Ident <$> identifier

    consExpr = do
      lbrace
      vals <- commaSep expr
      rbrace
      return $ Cons vals


statement :: GenParser Char st Statement
statement = msum
  [ blockStatement
  , switchStatement
  , ifElseStatement
  , nonBlockingStatement
  ]
  where
    blockStatement = do
      keyword "begin"
      stmts <- many statement
      keyword "end"
      return $ Block stmts

    switchStatement = do
      keyword "casez"
      lparen
      cond <- identifier
      rparen
      cases <- many1 $ do
        val <- value
        colon
        body <- statement
        return (val, body)
      keyword "endcase"
      return $ CaseZ cond cases

    ifElseStatement = do
      keyword "if"
      lparen
      cond <- expr
      rparen
      branchTrue <- statement
      branchFalse <- optionMaybe (keyword "else" *> statement)
      return $ If cond branchTrue branchFalse

    nonBlockingStatement = do
      target <- identifier
      symbol "<="
      value <- expr
      semi
      return $ NonBlocking target value

itemRegDecl :: GenParser Char st Item
itemRegDecl = do
  keyword "reg"
  width <- busWidth
  name <- identifier
  semi
  return $ RegDecl name width

itemWireDecl :: GenParser Char st Item
itemWireDecl = do
  keyword "wire"
  width <- busWidth
  name <- identifier
  semi
  return $ WireDecl name width

edge :: GenParser Char st (Edge, String)
edge = msum
  [ keyword "posedge" *> ((Pos, ) <$> identifier)
  , keyword "negedge" *> ((Neg, ) <$> identifier)
  , (All, ) <$> identifier
  ]

itemAlways :: GenParser Char st Item
itemAlways = do
  keyword "always"
  char '@'
  cond <- msum
    [ symbol "*" *> return []
    , do
        lparen
        triggers <- sepBy edge (keyword "or")
        rparen
        return $ triggers
    ]
  body <- statement
  return $ Always cond body

itemAssign :: GenParser Char st Item
itemAssign = do
  keyword "assign"
  dest <- identifier
  symbol "="
  val <- expr
  semi
  return $ Assign dest val

topmod :: GenParser Char st Module
topmod = do
  keyword "module"
  name <- identifier
  lparen
  params <- commaSep $ do
    ty <- msum $
      [ keyword "input"  *> return In
      , keyword "output" *> return Out
      ]
    reg <- option False (keyword "reg" *> return True)
    bits <- busWidth
    ident <- identifier
    return $ Parameter ty reg bits ident
  rparen
  semi
  items <- manyTill (msum
    [ itemRegDecl
    , itemWireDecl
    , itemAlways
    , itemAssign
    ]) (keyword "endmodule")
  return $ Module name params items

parseVerilog :: FilePath -> String -> Either ParseError Module
parseVerilog path source = runParser (whiteSpace *> topmod) () path source
