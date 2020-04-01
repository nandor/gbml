-- This file is part of the GBC-ML project.
-- Licensing information is available in the LICENSE file.
-- (C) 2020 Nandor Licker. All rights reserved.

module Translate where

import           Control.Monad.Except
import           Data.List

import qualified AST
import qualified VM

import Debug.Trace


type IdentMap = [(String, Integer)]



findAssign :: String -> [AST.Item] -> Either String AST.Expr
findAssign name [] =
  throwError ("wire " ++ name ++ " not assigned")
findAssign name (AST.Assign name' e : items) =
  if name == name' then return e else findAssign name items
findAssign name (_ : items) =
  findAssign name items


valueToInteger :: AST.Value -> Maybe Integer
valueToInteger (AST.Value vs) =
  foldM (\acc v ->
      case v of
        AST.X -> Nothing
        AST.Z -> Nothing
        AST.H -> Just (acc * 2 + 1)
        AST.L -> Just (acc * 2)
    ) 0 (reverse vs)


truncateOrExtend :: VM.Expr -> Integer -> Integer -> VM.Expr
truncateOrExtend expr bitsFrom bitsTo =
  if bitsFrom == bitsTo then expr
  else if bitsFrom > bitsTo then VM.Range expr bitsFrom bitsTo 0
  else VM.Extend expr bitsFrom bitsTo


truncateOrExtendPair :: VM.Expr -> Integer -> VM.Expr -> Integer -> (VM.Expr, VM.Expr, Integer)
truncateOrExtendPair lhs lhsBits rhs rhsBits =
  let bits = max lhsBits rhsBits in
  let lhs' = truncateOrExtend lhs lhsBits bits in
  let rhs' = truncateOrExtend rhs rhsBits bits in
  (lhs', rhs', bits)


translateExpr :: IdentMap -> AST.Expr -> Either String (VM.Expr, Integer)
translateExpr wires e =
  case e of
    AST.Ternary cond t f -> do
      (cond', condBits) <- translateExpr wires cond
      let cond'' = truncateOrExtend cond' condBits 1
      (t', lhsBits) <- translateExpr wires t
      (f', rhsBits) <- translateExpr wires t
      let (t'', f'', bits) = truncateOrExtendPair t' lhsBits f' rhsBits
      return (VM.Ternary cond'' t'' f'' bits, bits)

    AST.Or lhs rhs -> logical VM.Or lhs rhs
    AST.And lhs rhs -> logical VM.And lhs rhs
    AST.BitOr lhs rhs -> bitwise VM.Or lhs rhs
    AST.BitAnd lhs rhs -> bitwise VM.And lhs rhs
    AST.BitXor lhs rhs -> bitwise VM.Xor lhs rhs

    AST.Gt lhs rhs -> comparison VM.Gt lhs rhs
    AST.Eq lhs rhs -> comparison VM.Eq lhs rhs
    AST.Ne lhs rhs -> comparison VM.Ne lhs rhs

    AST.Shl lhs rhs -> throwError "Shl"

    AST.Add lhs rhs -> arithmetic VM.Add lhs rhs
    AST.Sub lhs rhs -> arithmetic VM.Sub lhs rhs

    AST.Not arg -> do
      (arg', argBits) <- translateExpr wires arg
      if argBits == 1
        then return (VM.Not arg' argBits, 1)
        else do
          throwError "Not"

    AST.Inv arg -> do
      (arg', argBits) <- translateExpr wires arg
      return (VM.Not arg' argBits, argBits)

    AST.HorzOr arg -> do
      (arg', argBits) <- translateExpr wires arg
      return (VM.HorzOr arg' argBits, 1)

    AST.Range arg st en -> do
      (arg', argBits) <- translateExpr wires arg
      when (st < 0) $ throwError "range start index negative"
      when (en < 0) $ throwError "range end index negative"
      when (st <= en) $ throwError "empty range"
      return (VM.Range arg' argBits st en, st - en + 1)

    AST.Index arg (AST.Const v) ->
      case valueToInteger v of
        Nothing ->
          throwError "invalid index"
        Just idx -> do
          (arg', argBits) <- translateExpr wires arg
          when (idx >= argBits) $ throwError "bit out of range"
          return (VM.Range arg' argBits idx idx, 1)

    AST.Index arg idx ->
      throwError "Index"

    AST.Cons args ->
      throwError "Cons"

    AST.Ident var -> do
      case lookup var wires of
        Nothing -> throwError ("wire not found: " ++ var)
        Just bits -> return (VM.Ident var bits, bits)

    AST.Const (AST.Value val) ->
      return (VM.Const (AST.Value val), toInteger (length val))

  where
    arithmetic op lhs rhs = do
      (lhs', lhsBits) <- translateExpr wires lhs
      (rhs', rhsBits) <- translateExpr wires lhs
      let (lhs'', rhs'', bits) = truncateOrExtendPair lhs' lhsBits rhs' rhsBits
      return (VM.Arithmetic op lhs' rhs' bits, bits + 1)

    logical op lhs rhs = do
      (lhs', lhsBits) <- translateExpr wires lhs
      (rhs', rhsBits) <- translateExpr wires lhs
      let lhs'' = truncateOrExtend lhs' lhsBits 1
      let rhs'' = truncateOrExtend rhs' rhsBits 1
      return (VM.Logical op lhs'' rhs'' 1, 1)

    bitwise op lhs rhs = do
      (lhs', lhsBits) <- translateExpr wires lhs
      (rhs', rhsBits) <- translateExpr wires lhs
      let (lhs'', rhs'', bits) = truncateOrExtendPair lhs' lhsBits rhs' rhsBits
      return (VM.Logical op lhs'' rhs'' bits, bits)

    comparison op lhs rhs = do
      (lhs', lhsBits) <- translateExpr wires lhs
      (rhs', rhsBits) <- translateExpr wires lhs
      let (lhs'', rhs'', bits) = truncateOrExtendPair lhs' lhsBits rhs' rhsBits
      return (VM.Comparison op lhs'' rhs'' bits, 1)


translateStmt :: IdentMap -> IdentMap -> AST.Statement -> Either String VM.Flow
translateStmt vars regs st =
  case st of
    AST.Block [st'] ->
      translateStmt vars regs st'
    AST.Block sts ->
      foldl1 VM.Let <$> mapM (translateStmt vars regs) sts
    AST.CaseZ cond cases ->
      undefined
    AST.If cond bt bf -> do
      undefined
    AST.NonBlocking reg val ->
      case lookup reg regs of
        Nothing ->
          throwError ("reg not found: " ++ reg)
        Just bits -> do
          (expr, bits') <- translateExpr vars val
          let expr' = truncateOrExtend expr bits' bits
          return $ VM.With reg expr'


translate :: AST.Module -> Either String VM.Module
translate (AST.Module{ AST.name, AST.params, AST.items }) = do
  let inWires    = [(name, n) | AST.Parameter AST.In  False n name <- params]
  let inRegs     = [(name, n) | AST.Parameter AST.In  True  n name <- params]
  let outWires   = [(name, n) | AST.Parameter AST.Out False n name <- params]
  let outRegs    = [(name, n) | AST.Parameter AST.Out True  n name <- params]
  let stateWires = [(name, n) | AST.WireDecl name n <- items]
  let stateRegs  = [(name, n) | AST.RegDecl name n <- items]

  unless (null inRegs) $ do
    throwError ("inputs cannot be registers")

  -- Validate all declared names
  let allRegs = outRegs ++ stateRegs
  let allWires = concat [inWires, outWires, stateWires] ++ allRegs
  let ids = map fst allWires
  let duplicates = nub (ids \\ nub ids)
  unless (null duplicates) $
    throwError ("duplicate IDs: " ++ intercalate ", " duplicates)

  wires <- forM (outWires ++ stateWires) $ \(name, bits) -> do
    expr <- findAssign name items
    (expr', bits') <- translateExpr allWires expr
    return (name, truncateOrExtend expr' bits' bits)

  -- Translate all always posedge blocks
  let alwaysBlocks = [(cond, st) | AST.Always cond st <- items]

  _ <- forM alwaysBlocks $ \(cond, st) -> do
    flow <- translateStmt allWires allRegs st
    throwError "DONE"

  Right $ VM.Module
    { VM.stateTy = VM.Record stateRegs
    , VM.inputTy = VM.Record $ inWires
    , VM.outputTy = VM.Record $ outWires ++ outRegs
    , VM.wires = wires
    }
