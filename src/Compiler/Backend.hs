{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Compiler.Backend where

import Chip.Decoder
import Chip.Instruction
import Compiler.Common
import Control.Arrow
  ( first,
  )
import Control.Monad ((>=>))
import qualified Data.Map
import Data.String (fromString)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST.C
import qualified LLVM.AST.Instruction as AST.I
import qualified LLVM.AST.Typed as AST.T
import qualified LLVM.IRBuilder.Instruction as LL.I
import qualified LLVM.IRBuilder.Module as LL.M
import qualified LLVM.IRBuilder.Monad as LL

nested :: (LL.MonadIRBuilder m) => m a -> m a
nested = nestedWithName ""

nestedWithName :: (LL.MonadIRBuilder m) => String -> m a -> m a
nestedWithName nm ir = do
  tailName <- LL.freshName "cont"
  LL.I.br tailName

  name <- if null nm then LL.fresh else LL.freshName (fromString nm)
  LL.emitBlockStart name
  result <- ir

  LL.emitBlockStart tailName
  pure result

llvmCompilerImpl ::
  forall m.
  (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) =>
  (Instruction AST.Operand -> m ()) ->
  CompilerImplementation
    AST.Operand
    m
llvmCompilerImpl compileInst = CompilerImplementation {..}
  where
    width =
      AST.T.typeOf >=> \case
        Right (AST.IntegerType wd) -> pure (fromIntegral wd)
        _ -> error "unsupported type for width calculation"

    int wd n =
      AST.ConstantOperand $
        AST.C.Int (fromIntegral wd) (fromIntegral n)

    lshr x y =
      AST.T.typeOf x
        >>= \t -> LL.emitInstr (fromRight' t) $ AST.I.LShr False x y []

    trunc x n = LL.I.trunc x (intType n)

    switch :: Data.Map.Map Integer (m ()) -> m () -> AST.Operand -> m ()
    switch cases def x = do
      errorBlock <- nestedWithName "default" (LL.currentBlock <* def)
      -- LL.emitTerm $ AST.IndirectBr x (Data.Map.elems cases') []
      let makeCase c = nested (LL.currentBlock <* c)
      cases' <- Data.Map.toList <$> traverse makeCase cases
      let k = first (AST.C.Int 4) <$> cases'
      LL.I.switch x errorBlock k

    switchDefault = pure ()
