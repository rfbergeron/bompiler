# Intermediate language and assembly generation
This document will hold some design considerations regarding assembly
generation and the intermediate language used by the compiler, if I ever get
around to designing one.

## RISC vs CISC
The intermediate language should be RISC. It is easier to compose multiple,
simple instructions into a single, complex instruction than it is to decompose
a complex instruction into its components. RISC also allows for greater
reordering of instructions, all of which would (I think) be possible in a CISC
instruction set, though I am not sure if it would always be more optimal.

## Stack alignment
The System-V amd64 ABI requires that it is 16-byte aligned when making function
calls. The Windows x64 ABI, AArch64 and RISC-V ABIs require 16-byte alignment at
all times. It may be okay to violate this rule within a subroutine, but, like
the System-V ABI it would still need to be aligned when making function calls.

Since stack space should be allocated once, at the beginning of a subroutine, it
would make the most since to always align it to 16 bytes during the function
prolog.

In some cases, it may be possible to save space by waiting to allocate space for
volatile registers until a function call must be made, but that is an
optimization and not one that I am sure is worthwhile.

## Registers
Registers will be divided into 4 classes: argument registers, return registers,
volatile (caller-saved) registers, and stable (callee-saved) registers. To my
knowledge, argument and return registers are mutually exclusive with stable
registers, and are automatically considered to be volatile.

### Argument registers
Zero or more registers will be designated as argument registers. These registers
will be pushed to the stack in the function prolog so that they may be freed up
for general use.

### Return registers
One or more registers will be designated as return registers. Function return
values will be written to these registers at the end of a function.

### Volatile registers
One or more registers will be designated as volatile registers. These values must
be saved on the stack before making function calls.

### Stable registers
One or more registers will be designated as stable registers. These value must
be saved on the stack at the beginning of the function.
