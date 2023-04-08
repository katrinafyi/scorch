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
import Control.Applicative (liftA2)
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

wordType :: AST.Type
wordType = AST.IntegerType 16

word :: Integral a => a -> AST.Operand
word n = AST.ConstantOperand $ AST.Int 16 $ fromIntegral n

vf :: Register AST.Operand
vf = V (word 0)

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
  i <- IRB.alloca wordType Nothing 8
  IRB.store i 8 =<< reg_read I

  n <- IRB.alloca wordType Nothing 8
  IRB.store n 8 (word 0)

  let loop =  undefined

  start <- nestedBlock $
    do
      n' <- IRB.load wordType n 8
      cond <- IRB.icmp AST.ULE n' nmax

      if' cond

  IRB.br start

semantics :: forall m. (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) => Externs AST.Operand m -> Instruction AST.Operand -> m ()
semantics externs@Externs {..} = \case
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
    skipIf AST.EQ (reg_read reg) (pure kk)
  Inst SNE (Reg reg) (Imm kk) _ ->
    skipIf AST.NE (reg_read reg) (pure kk)
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
      vf' <- bindM2 (IRB.icmp AST.UGT) (reg_read dst) (reg_read src)
      void $ reg_modify2 IRB.sub dst src
      reg_write vf vf'
  Inst SHR (Reg dst) _ _ ->
    do
      x <- reg_read dst
      lsb <- IRB.and x (word 1)
      reg_write dst =<< lshr x (word 1)
      reg_write vf lsb
  Inst SUBN (Reg dst) (Reg src) _ ->
    do
      vf' <- bindM2 (IRB.icmp AST.ULT) (reg_read dst) (reg_read src)
      void $ reg_modify2 (flip IRB.sub) dst src
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
    undefined
  Inst LD Iref (Reg (V v)) _ ->
    undefined
  Inst LD (Reg (V v)) Iref _ ->
    undefined
  Inst SCD _ _ _ -> undefined
  Inst SCR _ _ _ -> undefined
  Inst SCL _ _ _ -> undefined
  Inst EXIT _ _ _ -> undefined
  Inst LOW _ _ _ -> undefined
  Inst HIGH _ _ _ -> undefined
  _ -> undefined
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
