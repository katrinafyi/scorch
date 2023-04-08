{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main,
  )
where

import qualified Chip.Decoder
import qualified Compiler.Backend as Compiler
import Compiler.Common (CompilerImplementation)
import qualified Compiler.Common as Compiler
import qualified Compiler.Frontend as Compiler
import Data.Functor (void)
import qualified Data.Map as Map
import Data.String
import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as AST
import qualified LLVM.AST.CallingConvention as AST.CC
import qualified LLVM.AST.Type as AST.T
import qualified LLVM.Context as LL.C
import LLVM.IRBuilder (MonadIRBuilder)
import qualified LLVM.IRBuilder.Constant as IRB.C
import qualified LLVM.IRBuilder.Instruction as IRB.I
import qualified LLVM.IRBuilder.Module as IRB.M
import qualified LLVM.IRBuilder.Monad as IRB
import qualified LLVM.Module as LL.M
import System.Environment (getArgs)

llmodule :: AST.Module
llmodule = IRB.M.buildModule "asdf" $ do
  let i31 = AST.T.IntegerType 31
  IRB.M.function
    "f"
    [(AST.T.IntegerType 10, "param")]
    i31
    -- (const $ IRB.emitTerm
    -- (AST.I.Ret (Just (AST.ConstantOperand (AST.C.Int 31 23))) [])
    -- )
    --  (\[x] -> void $ Compiler.operandDecoder 4 x (1, 3))
    undefined

chipImpl :: (IRB.MonadIRBuilder m, IRB.M.MonadModuleBuilder m) => CompilerImplementation AST.Operand m
chipImpl =
  Compiler.llvmCompilerImpl
    ( \x -> do
        f <- IRB.M.extern (AST.Name $ fromString $ show x) [] AST.T.VoidType
        _ <- IRB.I.call (AST.T.FunctionType AST.T.VoidType [] False) f []
        pure ()
    )

llmodule2 = IRB.M.buildModule "df" $ do
  IRB.M.function "f" [(AST.T.IntegerType 16, "param")] AST.T.VoidType $ \[x] ->
    do
      _ <-
        Compiler.compileDecoder
          ( chipImpl
          )
          Chip.Decoder.decode
          4
          x
      _ <- IRB.block
      pure ()

llmodule3 = IRB.M.buildModule "df" $ do
  IRB.M.function "f" [(AST.T.IntegerType 16, "param")] AST.T.i8 $ \[x] -> do
    _ <- IRB.I.add x x
    i <- Compiler.nested $ do
      _ <- IRB.I.sub x x
      _ <- Compiler.nested $ do
        _ <- IRB.I.xor x x
        pure ()
      IRB.currentBlock
    _ <- IRB.I.add x x
    _ <- Compiler.nested $ do
      _ <- IRB.I.mul x x
      pure ()
    _ <- IRB.I.add x x

    _ <- IRB.I.ret (IRB.C.int8 10)
    IRB.I.br i

emitDecoder :: (IRB.MonadIRBuilder m, IRB.M.MonadModuleBuilder m) => m (AST.T.Type, AST.Operand)
emitDecoder = do
  let argtys = [AST.T.IntegerType 16]
  let retty = AST.T.VoidType
  fun <- IRB.M.function "interpret" (fmap (,IRB.M.NoParameterName) argtys) retty $ \[x] -> do
    Compiler.compileDecoder
      chipImpl
      Chip.Decoder.decode
      4
      x

  pure (AST.T.FunctionType retty argtys False, fun)

callInst ::
  AST.T.Type ->
  AST.Operand ->
  [(AST.Operand, [AST.ParameterAttribute])] ->
  AST.Instruction
callInst funty fun args =
  AST.Call
    { AST.tailCallKind = Nothing,
      AST.callingConvention = AST.CC.C,
      AST.returnAttributes = [],
      AST.type' = funty,
      AST.function = Right fun,
      AST.arguments = args,
      AST.functionAttributes = [],
      AST.metadata = []
    }

llmodule4 :: AST.Module
llmodule4 = IRB.M.buildModule "module" $ do
  IRB.M.function "run" [(AST.T.IntegerType 16, "param")] AST.T.VoidType $ \[pc] ->
    do
      op <- IRB.I.add pc pc
      (interpretTy, interpretFun) <- emitDecoder
      interpretBlock <-
        Compiler.nestedWithName "default_interpret" $
          IRB.currentBlock <* IRB.I.call interpretTy interpretFun [(op, [])]
      let interpret op = do
            let (ty, fun) = (interpretTy, interpretFun)
            let args = [(op, [])]
            case op of
              AST.ConstantOperand _ ->
                IRB.emitInstrVoid $
                  (callInst ty fun args) {AST.functionAttributes = [Right AST.AlwaysInline]}
              _ -> IRB.I.br interpretBlock
      _ <-
        Compiler.frontend
          chipImpl
          interpret
          (Map.singleton 10 0x8123)
          pc
          op
      pure ()

main :: IO ()
main = do
  [fname] <- getArgs

  LL.C.withContext
    ( \c ->
        LL.M.withModuleFromAST
          c
          llmodule4
          (LL.M.writeLLVMAssemblyToFile (LL.M.File fname))
    )
