{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Frontend where

import Compiler.Common (CompilerImplementation (..))
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

-- | Emits frontend which implements the main instruction execution steps.
-- This switches on program counter values and either executes
-- the inlined code (if it matches the original program code) or interprets the opcode.
frontend ::
  (Monad m) =>
  CompilerImplementation operand m ->
  (operand -> m ()) ->    -- ^ opcode execution function
  Map Integer Integer ->  -- ^ map of pc to op in original program code
  operand -> -- ^ program counter
  operand -> -- ^ opcode at pc
  m ()
frontend c runInst prog pc op = do
  wd <- c.width op

  let constant = c.int wd
  let makePcCase i =
        let progop = prog ! i
         in c.switch
              (Map.singleton progop (runInst (constant $ prog ! i)))
              (runInst op)
              op

  () <- c.switch (Map.mapWithKey (const . makePcCase) prog) (runInst op) pc

  pure ()
