{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Common where

import Chip.Instruction
import qualified Data.Map

data CompilerImplementation operand m = CompilerImplementation
  { width :: operand -> m Int,
    int :: Int -> Integer -> operand,
    lshr :: operand -> operand -> m operand,
    trunc :: operand -> Int -> m operand,
    compileInst :: Instruction operand -> m (),
    switch :: Data.Map.Map Integer (m ()) -> m () -> operand -> m (),
    switchDefault :: m ()
  }