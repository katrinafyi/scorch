{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Decoder where


import           Data.Map.Lazy                  ( Map )

import           Chip.Instruction
-- an abstract specification for decoding opcodes

data Hex = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | Ha | Hb | Hc | Hd | He | Hf
    deriving (Eq, Ord, Enum, Bounded, Show)

data Decoder part inst =
    Case (Instruction (Int,Int))
    | Switch Int (Map part (Decoder part inst))

case1 :: Ord p => Int -> p -> Decoder p i -> Decoder p i
case1 i n x = Switch i [(n, x)]

case2 :: Ord p => (Int, p) -> (Int, p) -> Decoder p i -> Decoder p i
case2 (i1, n1) (i2, n2) x = case1 i1 n1 (case1 i2 n2 x)

vx, vy, kk, nnn, n :: Param (Int, Int)
vx = Reg (V (2, 2))
vy = Reg (V (1, 1))

kk = Imm (0, 1)
nnn = Imm (0, 2)
n = Imm (0, 0)

decode, decode_0xxx, decode_8xxx, decode_Fxxx, decode_Exxx :: Decoder Hex ()
decode = Switch
  3
  [ (H0, decode_0xxx)
  , (H1, Case (inst1 JP nnn))
  , (H2, Case (inst1 CALL nnn))
  , (H3, Case (inst2 SE vx kk))
  , (H4, Case (inst2 SNE vx kk))
  , (H5, case1 0 H0 $ Case (inst2 SE vx vy))
  , (H6, Case (inst2 LD vx kk))
  , (H7, Case (inst2 ADD vx kk))
  , (H8, decode_8xxx)
  , (H9, case1 0 H0 $ Case (inst2 SNE vx vy))
  , (Ha, Case (inst2 LD (Reg I) nnn))
  , (Hb, Case (inst2 JP Blank nnn))
  , (Hc, Case (inst2 RND vx kk))
  , (Hd, Case (inst3 DRW vx vy n))
  , (He, decode_Exxx)
  , (Hf, decode_8xxx)
  ]


decode_0xxx = case1
  2
  H0
  (case1 1 He (Switch 0 [(H0, Case (inst0 CLS)), (He, Case (inst0 RET))]))

decode_8xxx = Switch
  0
  [ (H0, Case (inst2 LD vx vy))
  , (H1, Case (inst2 OR vx vy))
  , (H2, Case (inst2 AND vx vy))
  , (H3, Case (inst2 XOR vx vy))
  , (H4, Case (inst2 ADD vx vy))
  , (H5, Case (inst2 SUB vx vy))
  , (H6, Case (inst2 SHR vx vy))
  , (H7, Case (inst2 SUBN vx vy))
  , (He, Case (inst2 SHL vx vy))
  ]

decode_Exxx = Switch
  0
  [ (He, case1 1 H9 $ Case (inst1 SKP vx))
  , (H1, case1 1 Ha $ Case (inst1 SKNP vx))
  ]

decode_Fxxx = Switch
  1
  [ (H0, decode_Fx0x)
  , (H1, decode_Fx1x)
  , (H2, case1 0 H9 $ Case (inst2 LD F vx))
  , (H3, case1 0 H3 $ Case (inst2 LD B vx))
  , (H5, case1 0 H5 $ Case (inst2 LD Iref vx))
  , (H6, case1 0 H5 $ Case (inst2 LD vx Iref))
  ]
 where
  decode_Fx0x =
    Switch 0 [(H7, Case (inst2 LD vx (Reg DT))), (Ha, Case (inst2 LD vx K))]
  decode_Fx1x = Switch
    0
    [ (H5, Case (inst2 LD (Reg DT) vx))
    , (H8, Case (inst2 LD (Reg ST) vx))
    , (He, Case (inst2 ADD (Reg I) vx))
    , (H8, Case (inst2 LD (Reg ST) vx))
    ]
