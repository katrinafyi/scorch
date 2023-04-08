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
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.Context as LL
import qualified LLVM.IRBuilder.Constant as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Monad as IRB
import qualified LLVM.Module as LL
import System.Environment (getArgs)

llmodule :: AST.Module
llmodule = IRB.buildModule "asdf" $ do
  let i31 = AST.IntegerType 31
  IRB.function
    "f"
    [(AST.IntegerType 10, "param")]
    i31
    -- (const $ IRB.emitTerm
    -- (AST.Ret (Just (AST.ConstantOperand (AST.Int 31 23))) [])
    -- )
    --  (\[x] -> void $ Compiler.operandDecoder 4 x (1, 3))
    undefined

chipImpl :: (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) => CompilerImplementation AST.Operand m
chipImpl =
  Compiler.llvmCompilerImpl
    ( \x -> do
        f <- IRB.extern (AST.Name $ fromString $ show x) [] AST.VoidType
        _ <- IRB.call (AST.FunctionType AST.VoidType [] False) f []
        pure ()
    )

llmodule2 = IRB.buildModule "df" $ do
  IRB.function "f" [(AST.IntegerType 16, "param")] AST.VoidType $ \[x] ->
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

llmodule3 = IRB.buildModule "df" $ do
  IRB.function "f" [(AST.IntegerType 16, "param")] AST.i8 $ \[x] -> do
    _ <- IRB.add x x
    i <- Compiler.nested $ do
      _ <- IRB.sub x x
      _ <- Compiler.nested $ do
        _ <- IRB.xor x x
        pure ()
      IRB.currentBlock
    _ <- IRB.add x x
    _ <- Compiler.nested $ do
      _ <- IRB.mul x x
      pure ()
    _ <- IRB.add x x

    _ <- IRB.ret (IRB.int8 10)
    IRB.br i

emitDecoder :: (IRB.MonadIRBuilder m, IRB.MonadModuleBuilder m) => m (AST.Type, AST.Operand)
emitDecoder = do
  let argtys = [AST.IntegerType 16]
  let retty = AST.VoidType
  fun <- IRB.function "interpret" (fmap (,IRB.NoParameterName) argtys) retty $ \[x] -> do
    Compiler.compileDecoder
      chipImpl
      Chip.Decoder.decode
      4
      x

  pure (AST.FunctionType retty argtys False, fun)

callInst ::
  AST.Type ->
  AST.Operand ->
  [(AST.Operand, [AST.ParameterAttribute])] ->
  AST.Instruction
callInst funty fun args =
  AST.Call
    { AST.tailCallKind = Nothing,
      AST.callingConvention = AST.C,
      AST.returnAttributes = [],
      AST.type' = funty,
      AST.function = Right fun,
      AST.arguments = args,
      AST.functionAttributes = [],
      AST.metadata = []
    }

llmodule4 :: AST.Module
llmodule4 = IRB.buildModule "module" $ do
  IRB.function "run" [(AST.IntegerType 16, "param")] AST.VoidType $ \[pc] ->
    do
      op <- IRB.add pc pc
      (interpretTy, interpretFun) <- emitDecoder
      interpretBlock <-
        Compiler.nestedWithName "default_interpret" $
          IRB.currentBlock <* IRB.call interpretTy interpretFun [(op, [])]
      let interpret op = do
            let (ty, fun) = (interpretTy, interpretFun)
            let args = [(op, [])]
            case op of
              AST.ConstantOperand _ ->
                IRB.emitInstrVoid $
                  (callInst ty fun args) {AST.functionAttributes = [Right AST.AlwaysInline]}
              _ -> IRB.br interpretBlock
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

  LL.withContext
    ( \c ->
        LL.withModuleFromAST
          c
          llmodule4
          (LL.writeLLVMAssemblyToFile (LL.File fname))
    )
