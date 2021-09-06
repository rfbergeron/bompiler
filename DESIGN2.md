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
- arithmetic/bitwise/logical/comparison operators on integers
- integer conversions
- function definitions
- function calls
- nested scoping
- if/else statements
- for/while/do while statements
- labels
- case statements
- pointers and pointer math
- arrays
- structures and unions
- register allocation
- minor optimizations

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
    - The base type (signed, unsigned, void, array, pointer, function, struct,
      union, typedef)
    - The width of the type
    - The alignment of the type (differs from width for structs)
    - A linked list or array list containing additional type info (for pointers,
      arrays, functions, and structs/unions). The entries in this list will be
      ordered in the way one would read them when describing the type.

The flags, width, and alignment stored in a `struct TypeSpec` are attributes of
the base type. Information related to pointers, functions, etc. will be stored
in `struct auxspec`s in a linked list.

A correctly formed type will have a linked list within the following
constraints:
1. zero or more entries for pointers and arrays, followed by
2. one or zero entries for a typedef, struct, union, or function

The alignment of pointers and arrays will be implicit (8 bytes in the case of
x64), since they are both represented internally as addresses in memory. The
alignment of the members themselves will be specified in the `struct typespec`
itself.

## Checking types of return statements
The statement checking function must be able to check the type of a return
statement against the return type of the function being checked. This could be
accomplished by simply storing the name of the function being checked in a
global variable, so we can get the function type from the symbol table when we
need to.

## Labels and goto
Labels have their own special scope; they are visible anywhere within their
function, regardless of where they are declared. Since we are keeping the name
of the current function stored in a global variable, getting the symbol table
associated with that function and putting the label into it should be easy.

Becasue `goto` can jump forwards, we must allow `goto` statements that
refer to labels that are not yet defined. Labels will be verified with a second
pass, after the function has been completely validated.
