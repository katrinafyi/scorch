{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Chip.LLVM.Extern where

import Chip.Instruction
import Control.Monad (void)
import LLVM.AST (Type (VoidType))
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.IRBuilder as IRB

data Externs operand m = Externs
  { chip_error :: String -> m (),
    chip_warn :: String -> m (),
    pc_read :: m operand,
    pc_write :: operand -> m (),
    stack_push :: operand -> m (),
    stack_pop :: m operand,
    reg_read :: Register operand -> m operand,
    reg_write :: Register operand -> operand -> m (),
    rnd_byte :: m operand,
    mem_read :: operand -> m operand,
    mem_write :: operand -> operand -> m (),
    display_clear :: m (),
    display_draw :: operand -> operand -> operand -> m operand,
    display_sprite :: operand -> m operand,
    key_down :: operand -> m operand
  }

wordType :: AST.Type
wordType = AST.IntegerType 16

stringType :: AST.Type
stringType = AST.PointerType (AST.AddrSpace 0)

call ::
  (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) =>
  Int ->
  AST.Operand ->
  [AST.Operand] ->
  m AST.Operand
call n f args
  | n == length args = IRB.call (AST.FunctionType wordType (replicate n wordType) False) f (fmap (,[]) args)
  | otherwise = error "call: argument length mismatch"

callVoid ::
  (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) =>
  Int ->
  AST.Operand ->
  [AST.Operand] ->
  m ()
callVoid n f args = void $ call n f args

str :: (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) => String -> m AST.Operand
str x = do
  nm <- IRB.freshUnName
  ptr <- IRB.globalStringPtr x nm
  pure (AST.ConstantOperand ptr)

llvmChipExterns :: (IRB.MonadModuleBuilder m, IRB.MonadIRBuilder m2, IRB.MonadModuleBuilder m2) => m (Externs AST.Operand m2)
llvmChipExterns = do
  let w = wordType
  chip_error_fun <- IRB.extern "chip_error" [stringType] VoidType
  chip_warn_fun <- IRB.extern "chip_warn" [stringType] VoidType

  pc_read_fun <- IRB.extern "pc_read" [] w
  pc_write_fun <- IRB.extern "pc_write" [w] VoidType
  stack_push_fun <- IRB.extern "stack_push" [w] VoidType
  stack_pop_fun <- IRB.extern "stack_pop" [] w

  reg_read_v <- IRB.extern "reg_read_v" [w] w
  reg_write_v <- IRB.extern "reg_write_v" [w, w] VoidType
  reg_read_i <- IRB.extern "reg_read_i" [] w
  reg_write_i <- IRB.extern "reg_write_i" [w] VoidType
  reg_read_dt <- IRB.extern "reg_read_dt" [] w
  reg_write_dt <- IRB.extern "reg_write_dt" [w] VoidType
  reg_read_st <- IRB.extern "reg_read_st" [] w
  reg_write_st <- IRB.extern "reg_write_st" [w] VoidType

  rnd_byte_fun <- IRB.extern "rnd_byte" [] w

  mem_read_fun <- IRB.extern "mem_read" [w] w
  mem_write_fun <- IRB.extern "mem_write" [w, w] VoidType

  display_clear_fun <- IRB.extern "display_clear" [] VoidType
  display_draw_fun <- IRB.extern "display_draw" [w, w, w] VoidType
  display_sprite_fun <- IRB.extern "display_sprite" [w] w

  key_down_fun <- IRB.extern "key_down" [w] w

  let pc_read = call 0 pc_read_fun []
  let pc_write x = callVoid 1 pc_write_fun [x]
  let stack_push x = callVoid 1 stack_push_fun [x]
  let stack_pop = call 0 stack_pop_fun []
  let reg_read =
        \case
          V v -> call 1 reg_read_v [v]
          I -> call 0 reg_read_i []
          DT -> call 0 reg_read_dt []
          ST -> call 0 reg_read_st []
  let reg_write =
        \case
          V v -> \x -> callVoid 2 reg_write_v [v, x]
          I -> \x -> callVoid 1 reg_write_i [x]
          DT -> \x -> callVoid 1 reg_write_dt [x]
          ST -> \x -> callVoid 1 reg_write_st [x]
  let rnd_byte = call 0 rnd_byte_fun []
  let mem_read x = call 1 mem_read_fun [x]
  let mem_write x y = callVoid 2 mem_write_fun [x, y]

  let key_down x = call 1 key_down_fun [x]

  let display_clear = callVoid 0 display_clear_fun []
  let display_draw a b c = call 3 display_draw_fun [a, b, c]
  let display_sprite a = call 1 display_sprite_fun [a]

  let chip_error x = str x >>= \s -> callVoid 1 chip_error_fun [s]
  let chip_warn x = str x >>= \s -> callVoid 1 chip_warn_fun [s]

  pure $ Externs {..}

-- undefined
