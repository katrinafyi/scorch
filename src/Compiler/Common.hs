{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Common where

import Chip.Decoder
import Chip.Instruction
import qualified Data.Map

data CompilerImplementation operand m = CompilerImplementation
  { -- partWidth :: Int,
    width :: operand -> m Int,
    int :: Int -> Integer -> operand,
    lshr :: operand -> operand -> m operand,
    trunc :: operand -> Int -> m operand,
    compileInst :: Instruction operand -> m (),
    switch :: Data.Map.Map Integer (m ()) -> m () -> operand -> m (),
    switchDefault :: m ()
  }

operandDecoder ::
  Monad m =>
  CompilerImplementation operand m ->
  operand ->
  (Int, Int) ->
  m operand
operandDecoder comp operand (lo, hi) = do
  -- let (lo,hi) = (lo' * 4, hi' * 4)
  opwd <- comp.width operand
  let wd1 = opwd - lo
  x1 <-
    if lo == 0
      then pure operand
      else comp.lshr operand (comp.int (opwd) (fromIntegral lo))
  -- let x1 = undefined
  if hi - lo + 1 == wd1
    then pure x1
    else comp.trunc x1 (hi - lo + 1)

-- | Compiles an abstract decode tree into LLVM.
compileDecoder ::
  forall operand part m.
  (Monad m, Enum part) =>
  CompilerImplementation operand m ->
  Decoder part ->
  Int ->
  operand ->
  m ()
compileDecoder c (Case inst) _ operand = do
  inst' <- mapM (operandDecoder c operand) inst
  c.compileInst inst'
compileDecoder c (Switch n cases) wd operand = do
  let compileCase x = compileDecoder c x wd operand
  opwd <- c.width operand
  x1 <- if n == 0 then pure operand else c.lshr operand (c.int opwd (toInteger $ n * wd))
  x2 <- c.trunc x1 wd
  c.switch (Data.Map.mapKeys (toInteger . fromEnum) $ fmap compileCase cases) c.switchDefault x2

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' (Left _) = error "fromRight'"
