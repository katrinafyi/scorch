{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module Compiler where

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Constant             as AST.C
import qualified LLVM.AST.Type                 as AST.T
import qualified LLVM.AST.Typed                as AST.T
import qualified LLVM.AST.Instruction          as AST.I

import qualified LLVM.IRBuilder.Constant       as LL.C
import qualified LLVM.IRBuilder.Instruction    as LL.I
import qualified LLVM.IRBuilder.Module         as LL.M
import qualified LLVM.IRBuilder.Monad          as LL

import           Chip.Decoder
import           Chip.Instruction
import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                , first
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor                 ( second )
import           Data.Foldable                  ( toList )
import           Data.List                      ( genericLength )
import qualified Data.Map
import           Data.String                    ( fromString )
import qualified LLVM.AST.AddrSpace            as AST
import           LLVM.IRBuilder.Internal.SnocList
                                                ( SnocList(..) )
import Data.Either (fromRight)

-- int n = AST.ConstantOperand . AST.C.Int (fromIntegral n) . fromIntegral
intType = AST.T.IntegerType . fromIntegral

data CompilerImplementation operand part m =
  CompilerImplementation {
    partWidth :: Int,
    operandWidth :: Int,

    int :: Int -> Int -> operand,
    lshr :: operand -> operand -> m operand,
    trunc :: operand -> Int -> m operand,

    compileInst :: Instruction operand -> m (),
    switch :: Data.Map.Map part (m ()) -> operand -> m ()
  }

-- operandWidth
--   :: (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) => AST.Operand -> m Int
-- operandWidth operand = do
--   aa <- AST.T.typeOf operand
--   let Right (AST.T.IntegerType wd) = aa
--   pure $ fromIntegral wd

operandDecoder
  ::
  Monad m =>
  CompilerImplementation operand part m
  -> operand
  -> (Int, Int)
  -> m operand
operandDecoder comp operand (lo, hi) = do
  -- let (lo,hi) = (lo' * 4, hi' * 4)
  let wd1 = comp.operandWidth - lo
  x1 <- if lo == 0
    then pure operand
    else comp.lshr operand (comp.int comp.operandWidth (lo * comp.partWidth))
  -- let x1 = undefined
  if hi - lo + 1 == wd1
    then pure x1
    else comp.trunc x1 (comp.partWidth * (hi - lo + 1))


{-
compileDecoder
  :: forall m part
   . ( LL.MonadIRBuilder m
     , LL.M.MonadModuleBuilder m
     , Bounded part
     , Enum part
     , Ord part
     )
  => (Instruction AST.Operand -> m AST.Name)
  -> Int
  -> Decoder part
  -> AST.Operand
  -> m AST.Name
compileDecoder compileInst bits (Case inst) operand = do
  let f = AST.Name (fromString $ show inst)
  fn    <- LL.M.extern f [] AST.T.VoidType
  _     <- LL.I.call (AST.T.FunctionType AST.T.VoidType [] False) fn []
  -- LL.modifyBlock (\p -> p { LL.partialBlockName = f })
  inst' <- mapM (operandDecoder bits operand) inst
  compileInst inst'

compileDecoder compileInst bits (Switch n cases) operand = do
  LL.ensureBlock
  name       <- LL.currentBlock

  wd         <- operandWidth operand
  x1 <- if n == 0 then pure operand else LL.I.lshr operand (int wd (bits * n))
  x2         <- LL.I.trunc x1 (AST.T.IntegerType (fromIntegral bits))

  -- compile each branch into a block
  errorBlock <- nestedWithName "error" LL.currentBlock
  let compileCase x = nested $ compileDecoder compileInst bits x operand
  cases' <- traverse compileCase cases


  -- build jump table 
  -- let func = AST.Name "f"
  -- let keys = [minBound..maxBound] :: [part]
  -- let jumpTy = AST.T.PointerType (AST.AddrSpace 0)
  -- let jumpsTy = AST.T.VectorType (genericLength keys) jumpTy
  -- jumpsName <- LL.freshName "decode_table"
  -- let jumpTarget key = maybe (AST.C.Int 1 0) (AST.C.BlockAddress func) $ Data.Map.lookup key cases'
  -- _ <- LL.M.global jumpsName jumpsTy (AST.C.Array jumpTy (fmap jumpTarget keys))

  -- branch based on bit
  -- LL.emitTerm $ AST.IndirectBr x2 (Data.Map.elems cases') []
  let k = first (AST.C.Int 4 . fromIntegral . fromEnum)
        <$> Data.Map.toList cases'
  LL.I.switch x2 errorBlock k

  pure name
-}

nested :: (LL.MonadIRBuilder m) => m a -> m a
nested = nestedWithName ""

nestedWithName :: (LL.MonadIRBuilder m) => String -> m a -> m a
nestedWithName nm ir = do
  tailName <- LL.freshName "cont"
  LL.I.br tailName

  -- LL.liftIRState $ modify $ \s -> s { LL.builderBlock = Nothing }
  name <- if null nm then LL.fresh else LL.freshName (fromString nm)
  LL.emitBlockStart name
  result <- ir

  LL.emitBlockStart tailName
  pure result

{-
we have `Instruction (Int,Int)`, instructions whose arguments are (Int,Int)
-}



-- | Compiles an abstract decode tree into LLVM.
compileDecoder
  :: forall operand part m.
  (Monad m) =>
  CompilerImplementation operand part m ->
  Decoder part
  -> operand
  -> m ()
compileDecoder c (Case inst) operand = do
  -- let f = AST.Name (fromString $ show inst)
  -- fn    <- LL.M.extern f [] AST.T.VoidType
  -- _     <- LL.I.call (AST.T.FunctionType AST.T.VoidType [] False) fn []
  -- LL.modifyBlock (\p -> p { LL.partialBlockName = f })
  inst' <- mapM (operandDecoder c operand) inst
  c.compileInst inst'

compileDecoder c (Switch n cases) operand = do
  -- LL.ensureBlock
  -- name       <- LL.currentBlock


  -- compile each branch into a block
  -- errorBlock <- nestedWithName "error" LL.currentBlock
  let compileCase x = compileDecoder c x operand
  -- cases' <- nest c $ traverse compileCase cases


  -- build jump table 
  -- let func = AST.Name "f"
  -- let keys = [minBound..maxBound] :: [part]
  -- let jumpTy = AST.T.PointerType (AST.AddrSpace 0)
  -- let jumpsTy = AST.T.VectorType (genericLength keys) jumpTy
  -- jumpsName <- LL.freshName "decode_table"
  -- let jumpTarget key = maybe (AST.C.Int 1 0) (AST.C.BlockAddress func) $ Data.Map.lookup key cases'
  -- _ <- LL.M.global jumpsName jumpsTy (AST.C.Array jumpTy (fmap jumpTarget keys))

  -- branch based on bit
  -- LL.emitTerm $ AST.IndirectBr x2 (Data.Map.elems cases') []
  -- let k = first (AST.C.Int 4 . fromIntegral . fromEnum)
  --       <$> Data.Map.toList cases'
  -- LL.I.switch x2 errorBlock k

  let wd = c.operandWidth
  x1 <- if n == 0 then pure operand else c.lshr operand (c.int wd (c.partWidth * n))
  x2         <- c.trunc x1 c.partWidth
  c.switch (fmap compileCase cases) x2

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' (Left _) = error "fromRight'"

llvmCompilerImpl ::
  forall m.
  (LL.MonadIRBuilder m, LL.M.MonadModuleBuilder m) =>
  (Instruction AST.Operand -> m ())
  -> CompilerImplementation
      AST.Operand Hex m
llvmCompilerImpl compileInst = CompilerImplementation 4 16 int lshr trunc compileInst switch
  where
    int wd n = AST.ConstantOperand $ AST.C.Int (fromIntegral wd) (fromIntegral n)
    lshr x y = AST.T.typeOf x >>= \t -> LL.emitInstr (fromRight' t) $ AST.I.LShr False x y []
    trunc x n = LL.I.trunc x (intType n)
    switch :: Data.Map.Map Hex (m ()) -> AST.Operand -> m ()
    switch cases x = do
      errorBlock <- nestedWithName "error" LL.currentBlock
      -- LL.emitTerm $ AST.IndirectBr x (Data.Map.elems cases') []
      let makeCase c = nested (LL.currentBlock <* c)
      cases' <- Data.Map.toList <$> traverse makeCase cases
      let k = first (AST.C.Int 4 . fromIntegral . fromEnum) <$> cases'
      LL.I.switch x errorBlock k