# Clarifications/Preramble
It is important to distinguish between some concepts that tend to trip me up
when discussing what a register allocator is doing.

## Objects
While "variable" is the name used casually to refer to the named locations at
which information is stored in a program in C and other high-level languages,
for the purposes of this document, I will be using the term "object" to describe
this same concept.

Using this terminology matches the language used in The Book, and disambiguates
the use of "variable" in the dragon book. In this context, a "variable" has some
overlap with an "object", but they are not the same thing.

An object has three components: its name, its storage location, and its value.
Here, the name refers to both the human-readable name explicity specified by the
programmer and the context it was declared in. This context is necessary to
distinguish between human-readable names which may not be unique within the
program. Names may not be unique due to shadowing, multiple uses of the name to
declare static objects in separate translation units, multiple block-scope
objects whose enclosing scopes do not have a parent-child relationship, and so
on.

During execution, only the value of an object may change. In C, you may assume
that the value of any object is capable of changing, since naughty programmers
may cast away const, which may result in const-qualified objects having their
value changed. This does not apply to statically allocated objects whose
declarations are visible to the compiler, since they will be located in read-
only memory, and write to those memory locations should result in program
termination. (At least, that is my understanding.)

## Variables
Variables in the parlance of the dragon book's section on register allocation
and intermediate code optimization refer to both objects and the intermediate
values in expressions. It is disconnected from registers and memory locations,
since a variable could be located in either: objects will always live in memory,
but temporaries could be in either of registers and memory, since they may be
spilled.

## Values
Values can be stored in registers or in memory locations. These memory
locations may or may not be the component of an object. In most cases, they will
be, at least indirectly as a sequence of operations applied to the location
associated with an object. Association of a memory location with an
object/variable cannot, however, be established when the memory location was
obtained from a pointer, since the value of the pointer may vary arbitrarily at
runtime.

# Focus
The dragon book goes into detail about the optimization of registers globally
and the optimal selection of instructions on CISC machines.

I don't need to worry about this to get a working compiler. I think that basic
block allocation should be efficient to generate code that an assembler will
accept. However, there are some limitations with this approach that will need
to be worked around.

## Register Allocation in the Context of Bompiler
Most of the techniques described for register allocation are not necessary in
the context of the bompiler. Instructions have already been selected, no common
subexpression elemination is done, and achieving the highest performance is not
a concern.

The code generator technically performs one of the initial steps of graph
coloring: assigning symbolic/virtual registers to the operands of instructions.

We will not, however, be doing register allocation via graph coloring, at least
not at first. The plan currently is to perform some version of linear scan.
Under this algorithm, virtual registers can be thought of as names for
temporaries.

## Basic Blocks in C
While the dragon book also mentions making subroutine calls into basic block
leaders, this should not be necessary for C. While subroutines may have
arbitrary side effects, the standard makes only one guarantee regarding
subrouitines and side effects: the side effects of the expressions that make up
the subroutine call's arguments must be completed before the function call.

While subroutine calls will invalidate the contents of volatile registers, loads
and stores for all of these registers are already generated unconditionally by
the compiler for simplicity; we do not have to worry about the validity of the
values in these registers since it has already been handled.

Assignments through pointers may also change arbitrary regions of memory, since
it is difficult to disambiguate memory in C, let alone intermediate
code/assembly.

This is not of concern in either case: currently, the compiler always loads
objects from memory every single time they are used, so the freshest values
will always be used no matter what.

## Expressions With Multiple Basic Blocks
Logical and ternary operators are effectively if statements that can be
embedded within an expression, so they result in the expression they are a part
of being split into 3 or 4 basic blocks, respectively.

My solution is to do a backward pass over the entire subroutine. Individual
basic blocks will still have liveness information computed using a slightly
modified version of the algorithm presented in the dragon book; however, the
basic blocks themselves will have this algorithm applied to them in reverse
order.

The algorithm for computing variable liveness will be modified slightly from the
one presented in the dragon book. Tracking temporary variables is important in
our case, as they can span multiple basic blocks. Rather than using the symbol
table to track liveness information, a separate liveness table will be created
that stores the liveness of temporaries and objects for the duration of an
expression. For the first basic block evaluated, the algorithm proceeds as
normal. For subsequent basic blocks in the same group, rather than assuming
that all objects are live and all temporaries are dead, the algorithm instead
propogates the liveness information from the beginning of the last basic block
evaluated, and uses that for the initial table state.

A basic block is considered to be grouped with the previous basic block if the
current basic block is a subexpression of a logical or ternary operator. This
can, at the moment, be identified by the label of the first instruction in the
basic block. Labels generated by logical and ternary operators always are
formatted using `TRUE_FMT` or `FALSE_FMT`.

# Values
There are two ways we could uniquely identify values: using the variable and a
count representing sequence they occur in, or the values they are composed of,
or both.

Both can be computed in a single forward pass over the basic block. The
sequence count would start at 0, and be incremented each time a new value for a
variable is seen. Therefore, a given variable which has `n` values over the
course of the basic block would have an initial value with sequence `0` upon
entering the basic block and would have a final value with sequence `n-1` upon
exiting the basic block.

# Liveness
Liveness is calculated in a backward pass over a basic block. Initially, the
liveness table indicates that all objects are live and their next use is at the
leader of the basic block after the current one, and that all temporaries are
dead, which is indicated by the next use field being `NULL`.

Take a statement _s_ of the form `x = y OP z`. Attach to _s_ the current
contents of the liveness table entries for `x`, `y`, and `z` using pointers.
Change the liveness table to reflect that `x` is not live, `y` and `z` are
live, and their next use is at _s_.

Since we are working with x86 assembly directly, most two-operand operators are
destructive, so their destination is always changed. So, given instructions of
the form `OP x, y` (AT&T syntax), we would instead change the table to say that
`y` is dead and `x` is live and next used at _s_.

This information could, potentially, be calculated at the same time as liveness
and next-use information. However, since that information is calculated using
a backward pass over the instructions, the sequence counts for the values would
be in the reverse of the intuitive order. The components of a value would also
be ambiguous until we have seen more of the statements. For example, say we are
examining a statement which computes a value with sequence count `i` for a
variable `x`, called `x_i`. One of the variables used to calculate `x_i` is
`y`. The value of `y` could either be `y_j` or `y_[j+1]`, `j` being the current
sequence count for variable `y`.

# Structures
The register allocator will require the definition of multiple data structures
to assist in its function, maybe more than any of the other parts of the
compiler.

These data structures have the ability to handle errors in a way that is not ad-
hoc, unlike the AST. This will be accomplished using tagged unions. While the
register allocator should not, at the moment, encounter an error during program
execution (all errors should be detected and reported during code generation and
semantic analysis), it's a good idea to leave the option open in case I would
like to defer error handling until register allocation in the future.

## Liveness Table
The liveness table will track the liveness of variables. Variables can be
objects, uniquely identified by their symbol table entry, or temporaries,
identified by their virtual register number. The stored value will be an
iterator to the next instruction where they are used, or `NULL` if the
variable is dead.

The dragon book reccomends initializing liveness information like this:
initially, at the end of a basic block, all temporaries are marked dead,
while objects are marked live.

I will be performing a slight variation on this: at the end of a group of basic
blocks, the liveness table is initialized according to the book's
reccomendations. However, if the next (prior) basic block being examined is in
the same group as the current basic block, the liveness table from the current
basic block is reused for the next (prior) basic block. This way, temporaries
which span multiple basic blocks have their liveness information calculated
correctly, and are preserved accross basic blocks.

I could perhaps take this even farther: I could start at the last basic block in
the entire subroutine and work backwards. The initial liveness table could then
say that not only are all temporaries dead, but also all objects with automatic
storage duration are dead, since they cannot be used outside of the subroutine.
Objects with static or external storage would still be marked as live initially.
I am not certain of how useful this approach would be in generating good code.

## Basic Block
While the compiler will not initially be performing any fancy control flow
analysis or statement reordering, the data structure for basic blocks will be
designed with these possibilities in mind, so that they can be added in the
future if I am interested.

Basic blocks will store an iterator into the instruction list. This iterator's
value will be the instruction which is the leader for this basic block.

Basic blocks will store an array of pointers to other basic blocks which succeed
it in the control flow graph. For now, only one element will be stored in this
array, that being the next basic block which follows sequentially when reading
the program from top to bottom. Even when other basic blocks are added to this
array, the next basic block in sequence should be the first element of this
array.

The last instruction of the basic block is taken to be the leader of the first
basic block in the array that we just defined.
