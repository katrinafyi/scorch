module Chip.LLVM.Extern where

import Chip.Instruction

data Externs operand m = Externs {
    chip_error :: String -> m (),

    pc_read :: m operand,
    pc_write :: operand -> m (),

    stack_push :: operand -> m (),
    stack_pop :: m operand,

    reg_read :: Register operand -> m operand,
    reg_write :: Register operand -> operand -> m (),

    rnd_byte :: m operand,

    mem_read :: operand -> m operand,
    mem_write :: operand -> operand -> m (),

    display_clear :: m (),
    display_draw :: operand -> operand -> operand -> m operand,
    display_sprite :: operand -> m operand,

    key_down :: operand -> m operand
}