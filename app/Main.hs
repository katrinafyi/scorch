{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Constant             as AST.C
import qualified LLVM.AST.Instruction          as AST.I
import qualified LLVM.AST.Type                 as AST.T
import qualified LLVM.IRBuilder.Module         as IRB
import qualified LLVM.IRBuilder.Monad          as IRB

import qualified LLVM.Module as LL.M

import qualified LLVM.Context as LL.C
import           Lib
import System.Environment (getArgs)

llmodule :: AST.Module
llmodule = IRB.buildModule "asdf" $ do
  let i31 = AST.T.IntegerType 31
  IRB.function
    "f"
    []
    i31
    (const $ IRB.emitTerm
      (AST.I.Ret (Just (AST.ConstantOperand (AST.C.Int 31 23))) [])
    )

main :: IO ()
main = do
  [fname] <- getArgs
  LL.C.withContext
    (\c -> LL.M.withModuleFromAST c llmodule (LL.M.writeLLVMAssemblyToFile (LL.M.File fname)))


