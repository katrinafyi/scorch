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
import Data.Foldable (toList)
import Data.Bifunctor (second)
import qualified Data.Map

int n = AST.ConstantOperand . AST.C.Int (fromIntegral n) . fromIntegral
intType = AST.T.IntegerType . fromIntegral

operandWidth :: (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) => AST.Operand -> m Int 
operandWidth operand = do
  aa <- AST.T.typeOf operand
  let Right (AST.T.IntegerType wd) = aa
  pure wd

operandDecoder :: (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) => AST.Operand -> (Int, Int) -> m AST.Operand
operandDecoder operand (lo, hi) = do
  wd <- operandWidth operand
  let wd1 = fromIntegral wd - lo
  x1 <- if lo == 0 then pure operand else LL.I.lshr operand (int wd1 lo)
  -- let x1 = undefined
  if hi - lo + 1 == wd1 then pure x1 else LL.I.trunc x1 (intType (hi - lo + 1))


-- | Compiles an abstract decode tree into LLVM.
compileDecoder
  :: (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) =>
  (Instruction AST.Operand -> m a)
  -> Decoder part
  -> AST.Operand
  -> m a
compileDecoder compileInst (Case inst) operand = do
  inst' <- mapM (operandDecoder operand) inst
  compileInst inst'
compileDecoder compileInst (Switch n cases) operand = do  
  wd <- operandWidth operand
  x1 <- if n == 0 then pure operand else LL.I.lshr operand (int wd n)
  x2 <- LL.I.trunc x1 (AST.T.IntegerType 1)

  -- compile each branch into a block
  let rec x = compileDecoder compileInst x operand
  cases' <- Data.Map.fromList <$> traverse (\(a,b) -> (a,) <$> rec b) (Data.Map.toList cases)

  -- build jump table 
  


  -- branch based on bit
  pure _

{-
we have `Instruction (Int,Int)`, instructions whose arguments are (Int,Int)
-}
