# scorch

_scorch_ is a hybrid compiler-interpreter for CHIP-8 to LLVM.
Specifically, it compiles the main CHIP-8 ROM into bitcode 
and bundles an interpreter to handle modifications to the original program code. 

This is non-trivial architecture. It'll need to decode CHIP-8 opcodes in both Haskell and the compiled code,
