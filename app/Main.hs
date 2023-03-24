{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Constant             as AST.C
import qualified LLVM.AST.Instruction          as AST.I
import qualified LLVM.AST.Type                 as AST.T
import qualified LLVM.IRBuilder.Monad          as IRB
import qualified LLVM.IRBuilder.Module         as IRB.M

import qualified LLVM.Module as LL.M
import qualified LLVM.Context as LL.C

import System.Environment (getArgs)
import qualified Compiler

import Control.Monad

import Control.Monad.Trans.Class

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
    (\[x] -> void $ Compiler.operandDecoder 10 x (1,3))

main :: IO ()
main = do
  [fname] <- getArgs
  LL.C.withContext
    (\c -> LL.M.withModuleFromAST c llmodule (LL.M.writeLLVMAssemblyToFile (LL.M.File fname)))


