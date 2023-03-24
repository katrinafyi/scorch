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

int n = AST.ConstantOperand . AST.C.Int (fromIntegral n) . fromIntegral
intType = AST.T.IntegerType . fromIntegral

operandDecoder :: Monad m => Int -> AST.Operand -> (Int, Int) -> LL.IRBuilderT m AST.Operand
operandDecoder wd operand (lo, hi) = do
  let wd1 = wd - lo
  x1 <- if lo == 0 then pure operand else LL.emitInstr (intType wd1) $ AST.LShr False operand (int wd1 lo)  []
  -- let x1 = undefined
  if hi - lo + 1 == wd1 then pure x1 else LL.I.trunc x1 (intType (hi - lo + 1))


-- | Compiles an abstract decode tree into LLVM.
compileDecoder
  :: (Instruction AST.Operand -> LL.IRBuilder a)
  -> Decoder part
  -> Int
  -> AST.Operand
  -> LL.IRBuilder a
compileDecoder compileInst (Case inst) wd operand = do
  inst' <- mapM (operandDecoder wd operand) inst
  compileInst inst'
compileDecoder compileInst (Switch n _) wd operand = undefined

{-
we have `Instruction (Int,Int)`, instructions whose arguments are (Int,Int)
-}
