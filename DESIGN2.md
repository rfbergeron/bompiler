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

## Promotion and casting
Promotion rules are as follows:
- unsigned types are promoted to signed types
- integers are promoted to wider types
- in particular, any integer less wide than `signed int` is promoted to
  `signed int` when used; the increase in width can be optimized away by
  compilers but not the change in signedness.
- integer types are promoted to floating point types of the appropriate width

Casting rules are as follows:
- any operation 

Casting and promotion will be implented as follows:
- The tree node 

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
