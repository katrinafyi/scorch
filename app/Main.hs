{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Constant             as AST.C
import qualified LLVM.AST.Instruction          as AST.I
import qualified LLVM.AST.Type                 as AST.T
import qualified LLVM.IRBuilder.Instruction    as IRB.I
import qualified LLVM.IRBuilder.Module         as IRB.M
import qualified LLVM.IRBuilder.Monad          as IRB

import qualified LLVM.Context                  as LL.C
import qualified LLVM.Module                   as LL.M

import qualified Chip.Decoder
import qualified Compiler
import           System.Environment             ( getArgs )

import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.String

llmodule :: AST.Module
llmodule = IRB.M.buildModule "asdf" $ do
  let i31 = AST.T.IntegerType 31
  IRB.M.function "f"
                 [(AST.T.IntegerType 10, "param")]
                 i31
    -- (const $ IRB.emitTerm
      -- (AST.I.Ret (Just (AST.ConstantOperand (AST.C.Int 31 23))) [])
    -- )
                 (\[x] -> void $ Compiler.operandDecoder x (1, 3))

llmodule2 = IRB.M.buildModule "df" $ do
  IRB.M.function "f" [(AST.T.IntegerType 10, "param")] AST.T.VoidType $ \[x] ->
    do
      
      nm <- Compiler.nested $ Compiler.compileDecoder
        (\x -> IRB.block)
        Chip.Decoder.decode
        x
      IRB.I.br nm


main :: IO ()
main = do
  [fname] <- getArgs

  LL.C.withContext
    (\c -> LL.M.withModuleFromAST
      c
      llmodule2
      (LL.M.writeLLVMAssemblyToFile (LL.M.File fname))
    )


