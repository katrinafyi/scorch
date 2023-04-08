{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Backend where

import Chip.Instruction
import Compiler.Common
import Control.Arrow
  ( first,
  )
import Control.Monad ((>=>))
import qualified Data.Map
import Data.String (fromString)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST (Constant (Int))
import qualified LLVM.AST.Typed as AST
import qualified LLVM.IRBuilder.Instruction as IRB
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Monad as IRB

-- int n = AST.ConstantOperand . AST.C.Int (fromIntegral n) . fromIntegral
intType :: Integral a => a -> AST.Type
intType = AST.IntegerType . fromIntegral

nested :: (IRB.MonadIRBuilder m) => m a -> m a
nested = nestedWithName ""

nestedWithName :: (IRB.MonadIRBuilder m) => String -> m a -> m a
nestedWithName nm ir = do
  tailName <- IRB.freshName "cont"
  IRB.br tailName

  name <- if null nm then IRB.fresh else IRB.freshName (fromString nm)
  IRB.emitBlockStart name
  result <- ir

  IRB.emitBlockStart tailName
  pure result

llvmCompilerImpl ::
  forall m.
  (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) =>
  (Instruction AST.Operand -> m ()) ->
  CompilerImplementation AST.Operand m
llvmCompilerImpl compileInst = CompilerImplementation {..}
  where
    width =
      AST.typeOf >=> \case
        Right (AST.IntegerType wd) -> pure (fromIntegral wd)
        _ -> error "unsupported type for width calculation"

    int wd n =
      AST.ConstantOperand $
        AST.Int (fromIntegral wd) (fromIntegral n)

    lshr x y =
      AST.typeOf x
        >>= \t -> IRB.emitInstr (fromRight' t) $ AST.LShr False x y []

    trunc x n = IRB.trunc x (intType n)

    switch :: Data.Map.Map Integer (m ()) -> m () -> AST.Operand -> m ()
    switch cases def x = do
      errorBlock <- nestedWithName "default" (IRB.currentBlock <* def)
      -- IRB.emitTerm $ AST.IndirectBr x (Data.Map.elems cases') []
      let makeCase c = nested (IRB.currentBlock <* c)
      cases' <- Data.Map.toList <$> traverse makeCase cases
      wd <- fromIntegral <$> width x
      let k = first (AST.Int wd) <$> cases'
      IRB.switch x errorBlock k

    switchDefault = pure ()