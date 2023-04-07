{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Frontend where

import Compiler.Common (CompilerImplementation (..))
import Control.Applicative (liftA2)
import Data.Binary
import Data.Binary.Get
  ( getWord16be,
    isEmpty,
  )
import Data.Map (Map, (!))
import qualified Data.Map as Map

{-

abstract compiled llir:

def interpret(op):
    ...

def step(pc):
    op = mem[pc]
    BranchTaken = False
    if pc == 0:
        if op == ORIGINAL_PROGRAM[0]:
            do ORIGINAL_PROGRAM
        else:
            interpret(op)
    elif pc == 1:
        ...
    else:
        ASSERT FALSE
    if not BranchTaken:
        pc += 2
-}

many :: Get a -> Get [a]
many g = do
  b <- isEmpty
  if b then pure [] else liftA2 (:) g (many g)

parse = many getWord16be

frontend ::
  (Monad m) =>
  CompilerImplementation operand m ->
  (operand -> m ()) ->
  (operand -> m operand) ->
  Map Integer Integer ->
  operand ->
  m ()
frontend c runInst readMem prog pc = do
  op <- readMem pc
  wd <- c.width op

  let constant = c.int wd
  let makePcCase i op = let progop = prog ! i in switch c (Map.singleton progop (runInst (constant $ prog ! i))) (runInst op) op

  pure ()
