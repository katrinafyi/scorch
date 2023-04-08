{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main,
  )
where

import qualified Chip.Decoder as Chip
import qualified Chip.Instruction as Chip
import qualified Chip.LLVM.Extern as Chip
import qualified Chip.LLVM.Semantics as Chip
import qualified Compiler.Backend as Compiler
import Compiler.Common (CompilerImplementation (CompilerImplementation))
import qualified Compiler.Frontend as Compiler
import qualified Compiler.LLVM as Compiler
import qualified Compiler.Loader as Compiler
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy as B
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
import qualified LLVM.Passes as LL
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

compile :: Map.Map Integer Integer -> AST.Module
compile prog = IRB.buildModule "module" $ do
  externs <- Chip.llvmChipExterns
  let semantics = Chip.semantics externs
  let chipImpl = Compiler.llvmCompilerImpl semantics

  let argtys = [AST.IntegerType 16]
  let retty = AST.VoidType
  let interpretTy = AST.FunctionType retty argtys False
  interpretFun <- IRB.function "interpret" (fmap (,IRB.NoParameterName) argtys) retty $ \[x] -> do
    Compiler.compileDecoder
      chipImpl
      Chip.decode
      4
      x

  IRB.function "run" [(AST.IntegerType 16, "pc")] AST.VoidType $ \[pc] ->
    do
      op <- IRB.add pc pc
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
          prog
          pc
          op
      pure ()

main :: IO ()
main = do
  [fname, oname] <- getArgs

  bin <- B.readFile fname
  let prog = Compiler.loadProgram bin

  let passes =
        LL.PassSetSpec
          { LL.passes = [LL.AlwaysInline False, LL.CuratedPassSet 3],
            LL.targetMachine = Nothing
          }

  LL.withContext
    ( \c ->
        LL.withModuleFromAST
          c
          (compile prog)
          ( \m -> do
              -- LL.runPasses passes m
              LL.writeLLVMAssemblyToFile (LL.File oname) m
          )
    )
