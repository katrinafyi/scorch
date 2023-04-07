{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import qualified Chip.Decoder
import qualified Compiler.Backend as Compiler
import qualified Compiler.Common as Compiler
import Data.String
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST.T
import qualified LLVM.Context as LL.C
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

llmodule2 = IRB.M.buildModule "df" $ do
  IRB.M.function "f" [(AST.T.IntegerType 16, "param")] AST.T.VoidType $ \[x] ->
    do
      _ <-
        Compiler.compileDecoder
          ( Compiler.llvmCompilerImpl
              ( \x -> do
                  f <- IRB.M.extern (AST.Name $ fromString $ show x) [] AST.T.VoidType
                  IRB.I.call (AST.T.FunctionType AST.T.VoidType [] False) f []
                  pure ()
              )
          )
          Chip.Decoder.decode
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

main :: IO ()
main = do
  [fname] <- getArgs

  LL.C.withContext
    ( \c ->
        LL.M.withModuleFromAST
          c
          llmodule2
          (LL.M.writeLLVMAssemblyToFile (LL.M.File fname))
    )
