{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chip.Decoder where

import Chip.Instruction
import Data.Map.Lazy (Map)

-- an abstract specification for decoding opcodes

data Hex = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | Ha | Hb | Hc | Hd | He | Hf
  deriving (Eq, Ord, Enum, Bounded, Show)

data Slice
  = -- | a range for a bit slice. low bit position + width.
    Slice Int Int

data Decoder part
  = Case (Instruction Slice)
  | Switch Slice (Map part (Decoder part))

p0, p1, p2, p3 :: Slice
p0 = Slice 0 4
p1 = Slice 4 4
p2 = Slice 8 4
p3 = Slice 12 4

case1 :: Ord p => Slice -> p -> Decoder p -> Decoder p
case1 i n x = Switch i [(n, x)]

vx, vy, kk, nnn, n :: Param Slice
vx = Reg (V (Slice 8 4))
vy = Reg (V (Slice 4 4))
kk = Imm (Slice 0 8)
nnn = Imm (Slice 0 12)
n = Imm (Slice 0 4)

decode, decode_0xxx, decode_8xxx, decode_Fxxx, decode_Exxx :: Decoder Hex
decode =
  Switch
    p3
    [ (H0, decode_0xxx),
      (H1, Case (inst1 JP nnn)),
      (H2, Case (inst1 CALL nnn)),
      (H3, Case (inst2 SE vx kk)),
      (H4, Case (inst2 SNE vx kk)),
      (H5, case1 p0 H0 $ Case (inst2 SE vx vy)),
      (H6, Case (inst2 LD vx kk)),
      (H7, Case (inst2 ADD vx kk)),
      (H8, decode_8xxx),
      (H9, case1 p0 H0 $ Case (inst2 SNE vx vy)),
      (Ha, Case (inst2 LD (Reg I) nnn)),
      (Hb, Case (inst2 JP Blank nnn)),
      (Hc, Case (inst2 RND vx kk)),
      (Hd, Case (inst3 DRW vx vy n)),
      (He, decode_Exxx),
      (Hf, decode_Fxxx)
    ]
decode_0xxx =
  case1
    p2
    H0
    (case1 p1 He (Switch p0 [(H0, Case (inst0 CLS)), (He, Case (inst0 RET))]))
decode_8xxx =
  Switch
    p0
    [ (H0, Case (inst2 LD vx vy)),
      (H1, Case (inst2 OR vx vy)),
      (H2, Case (inst2 AND vx vy)),
      (H3, Case (inst2 XOR vx vy)),
      (H4, Case (inst2 ADD vx vy)),
      (H5, Case (inst2 SUB vx vy)),
      (H6, Case (inst2 SHR vx vy)),
      (H7, Case (inst2 SUBN vx vy)),
      (He, Case (inst2 SHL vx vy))
    ]
decode_Exxx =
  Switch
    p0
    [ (He, case1 p1 H9 $ Case (inst1 SKP vx)),
      (H1, case1 p1 Ha $ Case (inst1 SKNP vx))
    ]
decode_Fxxx =
  Switch
    p1
    [ (H0, decode_Fx0x),
      (H1, decode_Fx1x),
      (H2, case1 p0 H9 $ Case (inst2 LD F vx)),
      (H3, case1 p0 H3 $ Case (inst2 LD B vx)),
      (H5, case1 p0 H5 $ Case (inst2 LD Iref vx)),
      (H6, case1 p0 H5 $ Case (inst2 LD vx Iref))
    ]
  where
    decode_Fx0x =
      Switch p0 [(H7, Case (inst2 LD vx (Reg DT))), (Ha, Case (inst2 LD vx K))]
    decode_Fx1x =
      Switch
        p0
        [ (H5, Case (inst2 LD (Reg DT) vx)),
          (H8, Case (inst2 LD (Reg ST) vx)),
          (He, Case (inst2 LD (Reg I) vx)),
          (H8, Case (inst2 LD (Reg ST) vx))
        ]
