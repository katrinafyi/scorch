{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler where

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Constant             as AST.C
import qualified LLVM.AST.Type                 as AST.T
import qualified LLVM.AST.Typed                as AST.T

import qualified LLVM.IRBuilder.Constant       as LL.C
import qualified LLVM.IRBuilder.Instruction    as LL.I
import qualified LLVM.IRBuilder.Module         as LL.M
import qualified LLVM.IRBuilder.Monad          as LL

import           Chip.Decoder
import           Chip.Instruction
import Control.Monad.Trans.State.Strict
import Data.Foldable (toList)
import Data.Bifunctor (second)
import qualified Data.Map
import qualified LLVM.AST.AddrSpace as AST
import Data.List (genericLength)

int n = AST.ConstantOperand . AST.C.Int (fromIntegral n) . fromIntegral
intType = AST.T.IntegerType . fromIntegral

operandWidth :: (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) => AST.Operand -> m Int
operandWidth operand = do
  aa <- AST.T.typeOf operand
  let Right (AST.T.IntegerType wd) = aa
  pure $ fromIntegral wd

operandDecoder :: (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) => AST.Operand -> (Int, Int) -> m AST.Operand
operandDecoder operand (lo, hi) = do
  wd <- operandWidth operand
  let wd1 = fromIntegral wd - lo
  x1 <- if lo == 0 then pure operand else LL.I.lshr operand (int wd1 lo)
  -- let x1 = undefined
  if hi - lo + 1 == wd1 then pure x1 else LL.I.trunc x1 (intType (hi - lo + 1))


-- | Compiles an abstract decode tree into LLVM.
compileDecoder
  :: forall m part. (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m, Bounded part, Enum part, Ord part) =>
  (Instruction AST.Operand -> m AST.Name)
  -> Decoder part
  -> AST.Operand
  -> m AST.Name
compileDecoder compileInst (Case inst) operand = do
  inst' <- mapM (operandDecoder operand) inst
  compileInst inst'
compileDecoder compileInst (Switch n cases) operand = do
  LL.ensureBlock
  name <- LL.currentBlock

  wd <- operandWidth operand
  x1 <- if n == 0 then pure operand else LL.I.lshr operand (int wd n)
  x2 <- LL.I.trunc x1 (AST.T.IntegerType 1)

  -- compile each branch into a block
  let compileCase x = nested $ compileDecoder compileInst x operand
  cases' <- nested $ traverse compileCase cases

  let bbName (AST.BasicBlock nm _ _) = nm
  errorBlock <- pure ()

  -- build jump table 
  let func = AST.Name "f"
  let keys = [minBound..maxBound] :: [part]
  let jumpTy = AST.T.PointerType (AST.AddrSpace 0)
  let jumpsTy = AST.T.VectorType (genericLength keys) jumpTy
  jumpsName <- LL.freshName "decode_table"
  let jumpTarget key = maybe (AST.C.Int 1 0) (AST.C.BlockAddress func) $ Data.Map.lookup key cases'
  _ <- LL.M.global jumpsName jumpsTy (AST.C.Array jumpTy (fmap jumpTarget keys))

  -- branch based on bit
  LL.emitTerm $ AST.IndirectBr x2 (Data.Map.elems cases') []

  pure name

nested :: (LL.MonadIRBuilder m) => m a -> m a
nested ir = do 
  before <- LL.liftIRState $ gets LL.builderBlock 
  tailName <- LL.fresh
  LL.I.br tailName

  LL.block
  result <- ir 

  LL.emitBlockStart tailName
  pure result

{-
we have `Instruction (Int,Int)`, instructions whose arguments are (Int,Int)
-}
