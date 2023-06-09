{-# LANGUAGE DeriveTraversable #-}

module Chip.Instruction where

data Register a =
  V a
  | I
  | DT
  | ST
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Param a =
    Blank
    | Reg (Register a)
    | Imm a
    | K
    | F
    | B
    | Iref
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Mnemonic =
    CLS
    | RET
    | SYS
    | JP
    | CALL
    | SE
    | SNE
    | LD
    | ADD
    | OR
    | AND
    | XOR
    | SUB
    | SHR
    | SUBN
    | SHL
    | RND
    | DRW
    | SKP
    | SKNP
    | SCD
    | SCR
    | SCL
    | EXIT
    | LOW
    | HIGH
    deriving (Eq, Show)

data Instruction a = Inst Mnemonic (Param a) (Param a) (Param a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

inst0 :: Mnemonic -> Instruction a
inst1 :: Mnemonic -> Param a -> Instruction a
inst2 :: Mnemonic -> Param a -> Param a -> Instruction a
inst3 :: Mnemonic -> Param a -> Param a -> Param a -> Instruction a

inst0 mn = Inst mn Blank Blank Blank
inst1 mn x = Inst mn x Blank Blank
inst2 mn x y = Inst mn x y Blank
inst3 = Inst
