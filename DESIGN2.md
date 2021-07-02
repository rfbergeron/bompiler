# BOMPILER: a bad c compiler
### Author: Robert Bergeron
### Email: rbergero@ucsc.edu

This is a complete rewrite of the existing design document to go along with my
second approach to the compiler. It will not be a full rewrite, but will have
a couple significant changes:

1. Instead of being written in ISO C11, it will be written in ISO C89/C90, and
   it will also aim to be compliant with this standard.
2. The compiler will not generate an intermediate representation and will
   directly generate amd64 assembly. I do not plan to make this compiler target
   other architectures and do not plan to make the compiler perform any
   optimization, so I do not think the time spent designing an intermediate
   language would be worthwhile.
3. The compiler will use my own data structures library, badlib, instead of
   klib. As of the writing of this document, this has already been accomplished.
4. I will try to correct some mistakes I made when designing some parts of the
   compiler. The most glaring one is that struct initialization functions
   allocate space, which I have decided is bad style. Additionally, I will do
   my best to separate symbol table/value related code and type checking code,
   since those are both currently in the same source/header file.

## Approach
I will also be changing my approach to expanding the functionality of this
compiler. Instead of focusing on getting type checking and syntax tree building,
I will try to generate a small subset of C all the way down to assembly, and
add the entire vertical chain with each new feature. This approach was
recommended in some blog (I'll link it eventually). This sounds more gratifying
and will be a better way to verify my implementation is correct.

Switching to this approach will require a lot of work up front, since my compiler
currently generates an intermediate language that may not map well to x86, and
struggles to do even that.

## Order of implementation
Some components of the compiler seem like low-hanging fruit, while others are
effectively prerequesites for more complex features. For example:
- Nested scoping (with curly braces) is required for `switch`, `if`, `for`, and
  `while` statements, as well as functions
- Labels are a required feature for `break`, `case`, and `goto`. However, the
  implementation of labels in case statements and labels for others will differ
  a bit.

## Floating point
The x87 stack and instructions are deprecated; modern ABIs use the MMX/SSE/AVX
registers and instructions.

## Type and attribute implementation
A variable's/value's type will be represented by a list of type specifiers,
which have a set of flags used to track modifications to the type.

The symbol table will have two types of entries:
    The first kind of entry is used for the identifiers (objects and functions)
    themselves. They will hold a name that refers to a typeid.

    The second kind of entry stores information referring to typeids. Each of
    these entries will have:
    - The base type (integer, float, double, void, array, pointer, function, struct, typedef?)
    - The signedness of the type?
    - The width of the type
    - The alignment of the type (differs from width for structs)
    - A void pointer to additional data about the type (member info for structs,
      length for arrays, parameters for functions, and nested type for arrays
      and pointers)

Pointers and arrays will have 8-byte alignment, since they are both represented
internally as addresses in memory. (the members of arrays have their own respective
alignment)

Should vardecls have their type validated when the syntax tree is generated, or
when type checking occurs?

If the type is a structure value, all of the contents of the type will be stored
in the table in an entry associated with the struct's name. The same goes for
any of the primitive types, if I choose to just put everything into the type map
globally. Typedefs could be implemented in a similar manner.

If the type has any amount of indirection (arrays or pointers), then resolving
the type might be more complicated, as we would have to account for some degree
of nesting of arrays and pointers. The order in which the nesting occurs would
matter.

Perhaps the compiler should internally generate a typedef for "pointer to x",
and store that in the symbol table. The entry would have to include a 

Any function that compares types should make the assumption (or have a flag
indicating) that

IMPORTANT: I think that the spec's statement that any type smaller than signed
int can be promoted to signed int means that values are loaded as though they
are their original width/signedness, but in any operation they are used in
(including the first one) they are promoted

### Type checking procedures

All of these functions should call a common function that take a single typename
and identifier as input.

It may be helpful to change the `extract_type` function to handle the resolution
of typeids, but that can be dealt with later.

#### `validate_variable`
Takes two arguments: the tree node representing the type and the tree node
representing the identifier.

Checks that the type exists in the table and assigns it to the identifier if it
does. Creates an entry in the symbol table for the identifier.

#### `validate_function`
Takes one argument: the node representing the function, whose token should be
the "TOK_FUNCTION" token generated by the parser.

Calls `validate_type_id` on the node representing the function's identifier and
its return type, and on the parameters in the function prototype/definition.

#### `validate_call`
Takes one argument: the node representing the function call.

Compares the children of the argument with the types of the parameters required
by this function's entry in the symbol table.

#### `validate_struct`
Takes one argument: the node in the tree representing the structure definition.

Creates an entry in the symbol table for the structure, calls `validate_type_id`
on each of the struct's members, and includes these in the structure's type
definition.

## Checking types of return statements
The statement checking function must be able to check the type of a return
statement against the return type of the function being checked. This could be
accomplished by simply storing the name of the function being checked in a
global variable, so we can get the function type from the symbol table when we
need to.

## Labels
Labels have their own special scope; they are visible anywhere within their
function, regardless of where they are declared. Since we are keeping the name
of the current function stored in a global variable, getting the symbol table
associated with that function and putting the label into it should be easy.
However, becasue `goto` can jump forwards, we must allow `goto` statements that
refer to labels that are not yet defined. Labels will be verified after the
function has been completely checked.

## Scope
The standard defines three scopes: The first is file scope, which is occupied by
all objects not declared within a block, and which is visible anywhere within
the file. The second is block scope, in which all declarations are visible for
the duration of that block, including in nested blocks, so long as the object is
not masked by another declaration.

The third type is called "function prototype scope". Really all this scope means
is that the parameter names in a function type don't matter beyond that
prototype. When type checking function calls and definitions, the names of the
parameters do not matter; only the types.

As a consequence of this, the names of the parameters of a function prototype
should be overwritten by the parameters used in the definition.

Each new scope will need:
- its own depth
- a counter for the number 

Each scope will have its own unique map used to track symbols. These maps will
be pushed onto a stack in order of increasing depth, and new symbols will always
be placed into the map on the top of the stack. The map at the bottom of the
stack will always be global/file scope.

Each scope will also have sequence numbers, which are used in ordering variables
by declaration location. This number will be pushed and popped in the same way
as the symbol tables are.

To handle labels, which have function scope, the name of the function being
worked with will be stored as a global variable. When function labels need to be
verified or inserted into the symbol table, the function's symbol table will be
retrieved from the global table for use.

The function which resolves symbols (besides labels) will search each symbol
table, starting at the top, for the corresponding symbol.

## Promotion and casting
Promotion rules are as follows:
- unsigned types are promoted to signed types
- integers are promoted to wider types
- in particular, any integer less wide than `signed int` is promoted to
  `signed int` when used; the increase in width can be optimized away by
  compilers but not the change in signedness.
- integer types are promoted to floating point types of the appropriate width

I will be refraining from implementing floating point arithmetic for now, so we
only need to worry about integer width and signedness promotions.

## Operations that will be identical during assembly generation
Some operations map pretty well to assembly instructions:
- derefrencing pointers maps identically to `lea`
- taking the address of something is pretty self-explanatory in assembly
- float/double to int and back maps to a couple of simd instructions
- anything that is an address (pointer, array) can be considered an int

## Assembler nuances
I'll stick to the following rules when generating assembly to simplify the
process:
- At the beginning of the function, enough space will be allocated to store:
  - all variables declared within the function
  - all parameters
  - all stable registers
- all parameters will be pushed onto the stack at the beginning of the function
- Before each function call, all volatile registers will be pushed to the stack
- functions may not have more than 6 normal arguments and 8 floating point
  arguments (avoid having to implement stack-based arguments)
- I will come up with a simple register allocation scheme later; at first,
  registers will simply be numbered in increasing order like the virtual
  registers were in the original intermediate language
- every variable assignment is automatically put on the stack once it has been
  computed
- I will not use the x86 feature of using a value directly from memory in
  operations. This is inefficient, but I feel it will simplify code generation.
  Values will enter registers via the `mov` instruction.

# Assembler generation
The most basic assembly to be generated is the assembly having to do with
integer/arithmetic expressions. This involves loading objects from memory,
performing operations on them, then storing new values back to memory in the
appropriate location.

I will try to list all of the components needed to do this:
1. Each (stack-allocated) object will need to have a particular address on the
   stack assigned to it. I'm not sure if it will ever be correct to have
   memory addresses for a single object. Probably not.
2. Values should probably not be loaded into multiple registers at the same
   time; instead, they should be reused where possible. However, there may be
   instruction sequences that explicitly move/copy the contents of a register
   to another register. This should be allowed.
3. Intermediate values in computations may need to be temporarily stored on the
   stack. They will have a fake object name generated to track them.
4. Some data structure must be used to track where a value is currently located
   in memory/registers, and updated when this value is destroyed by operators or
   when the value changes and other instances are invalid.
5. Some data structure must be used to track whether a register currently
   contains a (valid) value.
6. It may make sense to have the system that tracks whether or not values are
   currently loaded into registers also be used to determine whether or not an
   operand should be addressed from memory, instead of being loaded first.

The initial implementation will not do any register allocation, or even refer
to registers by their proper names. Instead, it will refer registers as r0, r1,
r2, etc. in ascending order. These fake registers will not be reused. They are
not immutable; because of the way x64 works, they will be clobbered by
arithmetic operations. However, there will be at most one MOV/LOAD instruction
where that register is the destination. This includes subregisters; if a value
is moved into r0d, then at no point will values be moved into r0, r0w, or r0b.

The generator will need to distinguish between `.data`, `.text`, and `.bss`
sections, and decide which symbols belong in which location. It will also need
to decide whether or not a symbol should be exported with the `global`
directive, and whether or not a symbol needs to/should be imported with the
`extern` directive. There is also `static` to consider, but I don't know if
it is actually necessary.

The generator will need to have access to the root of the abstract syntax tree.
I do not think it will need to access the symbol tables, since all type info
should be encoded in the tree already.

All file-scope declarations will go in either the `.data` or `.bss` sections,
depending on whether or not they have been initialized, and will be exported
with the `global` directive unless they are declared `static` in the source
file.

All function declarations will be exported with the `global` directive, unless
they were declared with the `static` keyword in the source file.

The order in which these must be done is:
1. other directives (`global`, `extern`, `default rel`, etc.)
2. text segment
3. data segment
4. bss segment

Since I am allowing declarations to occur anywhere, I should use a stack upon
which function and global variable declarations are pushed so that they can be
emitted later, and so that I can determine whether symbols need to be declared
`extern`.

Notes:
- Division and multiplication are special; the destination operand is implied
  and hardcoded to AX. AX is divided by the single operand. The lower half of
  the result or the dividend will be stored in AX. The upper half of the result
  or the remainder is stored in DX.
- Unlike C, `extern` can't be used to reference any (non-static) function; the
  function must have been exported with the `global` directive in the module
  that defined it.

# Assembly generation procudures
Assembly generation for each instruction or type of instruction will have a
dedicated method, each with responsibilities mirroring the ones in the type
checker. There will be one "primary" function, `write_instruction`, that is
mutually recursive with all other `write_X` functions. These functions will
only recursively call into `write_instruction`.

All of these functions will have three parameters:
- the `ASTree` node to generate instruction(s) for
- the `InstructionData` object, which will hold the output of the function
- an `unsigned int`, with some flags holding information about what's going on
  higher up in the recursion

# Assembly Expression Generation
By default, the left(destination) operand of every instruction is a register,
while the right operand may be addressed from memory. This is not a restriction
of x64, but it makes generation a little simpler.

The function definition for most assembly evaluation functions will look like
this:
- the tree node to operate on
- a set of flags indicating whether the result will be the source or destination
  of the parent assembly instruction, among other things
- the `InstructionData` structure, which the function will output its results to

Structure of a generator function:
- Recursively call into generator for first subtree(left/unary operand), if any
- Recursively call into generator for second subtree(right operand), if any
- Determine operation performed and the corresponding assembly instruction to be
  emitted

General procedure by token type:
- For binary and unary ops, subexpressions are evaluated. The locations returned
  are used as the operands in the instruction data.
- For postfix increment and decrement specifically, the single subexpression is
  evaluated, and the value stored at the resulting address is loaded into
  another register; this register will be the one returned by the function. An
  add command is emitted, with the left operand being the memory address of the
  lval and the right being either an immediate 1 or -1.
- For identifiers, the address of the identifier is loaded into a register,
  which will be returned.
- For constants/immediate integers and characters:
  - if a left operand, the value will be loaded into a register with the `mov`
    instruction
  - if a right operand, the raw value will be returned

# Instruction/Source Line Data
Source line data will be tracked as follows:

## Label
Since some labels will be pre-existing `const char *`, while others will be
short, simple and generated like `.S0, .L1, .C2, .E3` etc., the `label` field
of the struct will be a union of `size_t` and `const char *`. There will be
an additional field indicating the type of the label (string constant, function,
global variable, conditional, loop body, loop end, etc.) which the label printer
function will use to determine how the label should be printed.

# Register Allocation
Before register allocation is done, all lines that will be written to the output
file will first be stored in a linearly traversible and maybe random access data
structure. Instructions will use virtual registers as operands.

The x64 use of the left operand as destination will be respected when these
lines are generated; the function that puts instruction data into the list will
return the name of the register the result was placed into.

The contents of source lines to be written out will be tracked in a struct with
fields for label, instruction, left/right operands, and comment.
