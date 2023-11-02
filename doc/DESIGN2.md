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

## New Type System
The new type system is cumbersome, confusing, and requires a lot of extra work
in order to manipulate types. So, I will be designing a new, simpler, and better
way of representing types.

Types will no longer use the doubly-linked list data structure from badlib.
Instead, they will be represented as a custom singly-linked list. The nodes of
this linked list will be a union of structs.

```
typedef enum type_code {
    TYPE_CODE_NONE,
    TYPE_CODE_STRUCT,
    TYPE_CODE_UNION,
    TYPE_CODE_ENUM,
    TYPE_CODE_POINTER,
    TYPE_CODE_FUNCTION,
    TYPE_CODE_ARRAY,
    TYPE_CODE_BASE
} TypeCode;

typedef enum base_type {
    BASE_TYPE_VOID = 0 ,
    BASE_TYPE_CHAR_ONLY = 1, /* used for `char` */
    BASE_TYPE_UCHAR = 2,
    BASE_TYPE_SCHAR = 3,
 /* 
  * BASE_TYPE_FLOAT = 4,
  * BASE_TYPE_DOUBLE = 5,
  * BASE_TYPE_LONG_DOUBLE = 6,
  * BASE_TYPE_BOOL = 7,
  */
    BASE_TYPE_UINT, /* use 3rd bit as flag, bits 0-2 as a 3-bit integer */
    BASE_TYPE_USHRT,
    BASE_TYPE_ULONG,
 /* BASE_TYPE_ULLONG, */
    BASE_TYPE_SINT,
    BASE_TYPE_SSHRT,
    BASE_TYPE_SLONG,
 /* BASE_TYPE_SLLONG, */
} BaseType;

/* Qualifier and storage class flags will be specified such that they do not
   overlap with any of the base type enums, even if it is not valid for the
   given base type to have a given flag.

   Storage class flags are technically for symbols, not for types, but because
   of the compiler's structure the storage class information must be propogated
   through the types system in order for the symbol to receive the info.

   Integral values have 4 possible sizes: none, short, long, and long long.
   They also have a signedness flag.

   This representation is not explicit; base type codes are enumerated in such
   a way that they can be treated as a 3-bit integer: one bit for the signedness
   and two more bits for the size.
 */

typedef enum type_qual_flag {
    QUAL_FLAG_CONST = 1 << 4,
    QUAL_FLAG_VOLATILE = 1 << 5,
    QUAL_FLAG_TYPEDEF = 1 << 6,
 /* 
  * QUAL_FLAG_RESTRICT = 1 << 7,
  * QUAL_FLAG_ATOMIC = 1 << 8,
  */
} QualFlag;

typedef enum type_stor_flag {
    STOR_FLAG_AUTO = 1 << 9,
    STOR_FLAG_REGISTER = 1 << 10,
    STOR_FLAG_STATIC = 1 << 11,
    STOR_FLAG_EXTERN = 1 << 12,
    STOR_FLAG_TYPEDEF = 1 << 13,
} StorFlag;

typedef union type Type;
union type {
    struct any_type {
        TypeCode code;
    } any;
    struct pointer_type {
        TypeCode code;
        unsigned int qualifiers;
        Type *next;
    } pointer;
    struct array_type {
        TypeCode code;
        int deduce_length;
        Type *next;
        size_t length;
    } array;
    /* functions whose number and types of parameters are not specified will
       have a types_size of 1; this indicates that only the return type was
       specified and that the parameters were specified as '()'. functions with
       an explicit '(void)' will have a second entry in `types` with type void.

       should the types of a function's parameters be stored in a function's
       type node, or should the names of the parameters be stored instead?
     */
    struct function_type {
        TypeCode code;
        int variadic;
        Type **types; /* first type is always the return/base type */
        size_t types_size; /* minimum 1 */
    } function;
    struct tag_type {
        TypeCode code;
        unsigned int qualifiers;
        const char *tag_name;
        TagValue *tag_value;
    } tag;
    /* base types don't need their alignment and size recorded here; provide
     * functions which return alignment and size based on hard-coded values in
     * a switch statement.
     */
    struct base_type {
        TypeCode code;
        unsigned int type_flags;
    } base;
};
```

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
