{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chip.LLVM.Semantics where

import Chip.Instruction
import Chip.LLVM.Extern
import Compiler.LLVM
import Control.Applicative (Alternative (empty), liftA2)
import Control.Monad (join, void, (>=>))
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.IRBuilder as IRB hiding (lshr)

bindM2 :: Monad m => (a1 -> b -> m a2) -> m a1 -> m b -> m a2
bindM2 f a b = join $ liftA2 f a b

if' :: (IRB.MonadIRBuilder m) => AST.Operand -> m () -> m () -> m ()
if' c t f = do
  t' <- nestedBlock t
  f' <- nestedBlock f
  IRB.condBr c t' f'

word :: Integral a => a -> AST.Operand
word n = AST.ConstantOperand $ AST.Int 16 $ fromIntegral n

ext :: (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) => AST.Operand -> m AST.Operand
ext x = do
  wd <- width x
  if wd == 16
    then pure x
    else IRB.zext x wordType

vf :: Register AST.Operand
vf = V (word 0xf)

pc_inc2 ::
  (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) =>
  Externs AST.Operand m ->
  m ()
pc_inc2 Externs {..} = do
  pc <- pc_read
  pc' <- IRB.add (word 2) pc
  pc_write pc'

ireg_loop ::
  (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) =>
  Externs AST.Operand m ->
  -- | i num -> v num -> m ()
  (AST.Operand -> AST.Operand -> m ()) ->
  AST.Operand ->
  m ()
ireg_loop Externs {..} f nmax = do
  let load x = IRB.load wordType x 8
  let store x = IRB.store x 8
  let inc x = load x >>= IRB.add (word 1) >>= store x

  i <- IRB.alloca wordType Nothing 8
  store i =<< reg_read I

  n <- IRB.alloca wordType Nothing 8
  store n (word 0)

  start <- IRB.fresh
  end <- IRB.fresh

  IRB.br start

  IRB.emitBlockStart start
  n' <- load n
  cond <- IRB.icmp AST.ULE n' nmax

  let loop = do
        f i n
        inc n
        inc i
        IRB.br start

  if' cond loop (IRB.br end)

  IRB.emitBlockStart end

semantics' :: forall m. (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) => Externs AST.Operand m -> Instruction AST.Operand -> m ()
semantics' externs@Externs {..} = \case
  Inst CLS _ _ _ ->
    display_clear
  Inst RET _ _ _ ->
    stack_pop >>= pc_write
  Inst SYS _ _ _ -> pure ()
  Inst JP (Imm addr) Blank Blank ->
    pc_write addr
  Inst JP Blank (Imm shift) Blank ->
    reg_read (V (word 0))
      >>= IRB.add shift
      >>= pc_write
  Inst CALL (Imm addr) _ _ ->
    pc_read
      >>= stack_push
      >> pc_write addr
  Inst SE (Reg reg) (Imm kk) _ ->
    skipIf AST.EQ (reg_read reg) (ext kk)
  Inst SNE (Reg reg) (Imm kk) _ ->
    skipIf AST.NE (reg_read reg) (ext kk)
  Inst SE (Reg reg) (Reg r2) _ ->
    skipIf AST.EQ (reg_read reg) (reg_read r2)
  Inst SNE (Reg reg) (Reg r2) _ ->
    skipIf AST.NE (reg_read reg) (reg_read r2)
  Inst LD (Reg r) (Imm kk) _ ->
    reg_write r kk
  Inst ADD (Reg r) (Imm kk) _ ->
    reg_modify (IRB.add kk) r
  Inst LD (Reg dst) (Reg src) _ ->
    reg_read src
      >>= reg_write dst
  Inst OR (Reg dst) (Reg src) _ ->
    void $ reg_modify2 IRB.or dst src
  Inst AND (Reg dst) (Reg src) _ ->
    void $ reg_modify2 IRB.and dst src
  Inst XOR (Reg dst) (Reg src) _ ->
    void $ reg_modify2 IRB.xor dst src
  Inst ADD (Reg I) (Reg src) _ ->
    void $ reg_modify2 IRB.add I src -- no carry
  Inst ADD (Reg dst) (Reg src) _ ->
    do
      x <- reg_modify2 IRB.add dst src
      reg_write vf =<< IRB.icmp AST.UGT x (word 255)
  Inst SUB (Reg dst) (Reg src) _ ->
    do
      x <- reg_read dst
      y <- reg_read src
      vf' <- IRB.icmp AST.UGT x y
      reg_write dst =<< IRB.sub x y
      reg_write vf vf'
  Inst SHR (Reg dst) _ _ ->
    do
      x <- reg_read dst
      lsb <- IRB.and x (word 1)
      reg_write dst =<< lshr x (word 1)
      reg_write vf lsb
  Inst SUBN (Reg dst) (Reg src) _ ->
    do
      x <- reg_read dst
      y <- reg_read src
      vf' <- IRB.icmp AST.UGT y x
      reg_write dst =<< IRB.sub y x
      reg_write vf vf'
  Inst SHL (Reg dst) _ _ ->
    do
      x <- reg_read dst
      msb <- IRB.and (word 1) =<< lshr x (word 7)
      reg_write dst =<< IRB.shl x (word 1)
      reg_write vf msb
  Inst RND (Reg dst) (Imm mask) _ ->
    rnd_byte
      >>= IRB.and mask
      >>= reg_write dst
  Inst DRW (Reg (V vx)) (Reg (V vy)) (Imm n) ->
    display_draw vx vy n
      >>= reg_write vf
  Inst SKP (Reg (V v)) _ _ ->
    skipIf AST.NE (pure $ word 0) (key_down v)
  Inst SKNP (Reg (V v)) _ _ ->
    skipIf AST.EQ (pure $ word 0) (key_down v)
  Inst LD F (Reg (V digit)) _ ->
    display_sprite digit
      >>= reg_write I
  Inst LD B (Reg (V v)) _ ->
    pure ()
  Inst LD Iref (Reg (V nmax)) _ ->
    ireg_loop
      externs
      (\i v -> reg_read (V v) >>= mem_write i)
      nmax
  Inst LD (Reg (V nmax)) Iref _ ->
    ireg_loop
      externs
      (\i v -> mem_read i >>= reg_write (V v))
      nmax
  -- Inst SCD _ _ _ -> undefined
  -- Inst SCR _ _ _ -> undefined
  -- Inst SCL _ _ _ -> undefined
  -- Inst EXIT _ _ _ -> undefined
  -- Inst LOW _ _ _ -> undefined
  -- Inst HIGH _ _ _ -> undefined
  inst -> chip_warn (show inst)
  where
    skipIf cond x y = do
      c <- bindM2 (IRB.icmp cond) x y
      if' c (pc_inc2 externs) (pure ())

    reg_modify :: (AST.Operand -> m AST.Operand) -> Register AST.Operand -> m ()
    reg_modify f r =
      reg_read r >>= (f >=> reg_write r)

    reg_modify2 :: (AST.Operand -> AST.Operand -> m AST.Operand) -> Register AST.Operand -> Register AST.Operand -> m AST.Operand
    reg_modify2 f dst src = do
      x <- bindM2 f (reg_read dst) (reg_read src)
      reg_write dst x
      pure x

semantics :: forall m. (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) => Externs AST.Operand m -> Instruction AST.Operand -> m ()
semantics externs@Externs {..} inst = do
  inst' <- traverse ext inst
  semantics' externs inst'
  case inst of
    Inst JP _ _ _ -> pure ()
    Inst CALL _ _ _ -> pure ()
    _ -> pc_inc2 externs
