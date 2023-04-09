{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Backend where

import Compiler.Common
import Chip.Decoder
import qualified Data.Map as Map


slice ::
  Monad m =>
  CompilerImplementation operand m ->
  operand ->
  Slice ->
  m operand
slice comp operand (Slice lo wd) = do
  -- let (lo,hi) = (lo' * 4, hi' * 4)
  opwd <- comp.width operand
  x1 <-
    if lo == 0
      then pure operand
      else comp.lshr operand (comp.int opwd (fromIntegral lo))
  -- let x1 = undefined
  wdx1 <- comp.width x1
  if wdx1 == wd
    then pure x1
    else comp.trunc x1 wd

-- | Compiles an abstract decode tree into LLVM.
compileDecoder ::
  forall operand part m.
  (Monad m, Enum part) =>
  CompilerImplementation operand m ->
  Decoder part ->
  operand ->
  m ()
compileDecoder c (Case inst) operand = do
  inst' <- mapM (slice c operand) inst
  c.compileInst inst'
compileDecoder c (Switch sl cases) operand = do
  let compileCase x = compileDecoder c x operand
  let cases' = fmap compileCase cases

  x <- slice c operand sl
  c.switch (Map.mapKeys (toInteger . fromEnum) cases') c.switchDefault x