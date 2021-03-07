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
- Nested scoping (with curly braces) is required for _switch_ and _if_
  statements, as well as functions
- Labels are a required feature for break, case, and goto. In fact, there may be
  an even more base concept behind labels, that being function calls. I'll have
  to look at the semantics of function calls in x86, but I believe functions are
  identified by assembler labels, much like break, case, and goto would be.

## Type and attribute implementation
A variable's/value's type will be represented by a list of type specifiers,
which have a set of flags used to track modifications to the type
