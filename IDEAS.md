# Ideas
This document will be where I do my thinking. It will probably be messy and not
very readable, but that's fine because my brain garbage needs to go somewhere,
and that somewhere can't be my design document like it was before

## Things to hold off on
There are a few features that seem difficult to implement, not important for the
purpose of achieving self-hosting, or both. These include:
- ~~Variably modified types: I don't think I'll be using these, and they seem
  like a pain in the ass to implement~~ I just realized that I've been looking
  at a draft of C99, not C89, so I might not have even needed to worry about
  this in the first place.
- Variadic functions: I'm not sure how much of the heavy lifting is done by the
  standard library here, but we'll see.
- typedefs: might not be that much of a headache to implement, but we'll see
- floating point: learning x86 is going to be enough trouble without dealing
  with AVX/SSE extensions.
- passing structs by value: So far as I can tell the standard does not specify
  how this should be done, only that it must be done. GCC packs them into the
  registers if they fit. I don't want to worry about this and my compiler will
  not be doing it so I won't need this to achieve self-hosting.
- returning structs by value: Not required by the standard, but would be a neat
  extension. The Microsoft x64 ABI defines how to do this, and it seems fairly
  straightforward.

## Identifier Labels
Based on the language definition, it looks like labels are attached to the
statement immediately following them. They are not a valid statement all on
their own. Just something to note for later.

## Increment and decrement
These operators must have an lval argument. Postfix and prefix versions must
have unique tokens so that they may be distinguished outside of the parser.

## Storage of string constants
Currently, the type checker/symbol table is repsonsible for tracking string
constants. It would make more sense for the string set to track them in some
way; it would also make more sense for the constants to be registered when the
abstract syntax tree is built, since the value and type of constants are known
when the tree node is generated.

## Scoping
Instead of the current scheme, where we only differentiate between local and
global scope, we should implement a 'scope stack' that is pushed onto with
each nested block. There should be a function that searches for a given typeid
in the stack from the top down.

The function for making global and local entries should be condensed into a
single function that just makes the entry in the scope that is on the top of the
stack if it is not already present.

Labels have their own namespace. Structures and unions share a namespace
separate from other types of declarations, and functions and objects share a
namespace.

## Storing types
Currently, types are stored by value. Types of syntax tree nodes are set by:
- copying the type from a symbol entry, in the case of an identifier
- copying the type from a constant declared elsewhere, in the case of literals
  and constants
- copied from one of the operands, in the case of operator expressions
- constructed, in the case of the promotion operator.

This is convenient because it means that modifications to types that occur
during the evaluation of the expression can be done easily by copying the base
or child type to another tree node and modifying it as needed.

However, it does make working with nested types more difficult. Nested types
cannot simply be stored by value, because the storage can (or should) be
arbitrarily deep (a pointer to a pointer to a pointer...), so nested type
specifiers must be stored using pointers.

This means that when cleanup occurs, nested type specifiers must be freed. This
would require its own function `typespec_destroy` or similiar.

However, this would become a problem when copying typespecs naively, with the
equals operator. There would need to be a function dedicated to copying, or this
functionality could be included in a `typespec_init` function.

Even with a dedicated copy function, recursively copying certain components of
type information, like linked lists and maps, would be complicated, because
currently badlib does not implement copying functions, and implementing
copying functions would be difficult since badlib data structures have no
knowledge of what their elements are, so they cannot be easily copied.

Copying is not necessary. Anytime a time needs to be assigned to a node, you can
get it from one of three places:
- we are promoting a value to a `signed int` from a narrower type, in which case
  we point to a constant structure
- we are working with a constant or literal, whose type can be taken from the
  symbol table or destination in the case of structures and unions, or whose
  type has limited possibilities, in the case of integer and character constants.
- we are doing an explicit cast, in which case the type can be constructed from
  the type specified, and assigned a dummy symbol
- we are handling an identifier (either referencing the value or assigning it
  a new one, which may require an implicit cast) in which case we refer to the
  identifier's symbol entry

No copying necessary, and dummy symbols only need to be created for explicit
casts.

## Comparing types
The `types_compatible` function should return more than just an enum describing
the compatibility of its arguments. It should return a struct that includes the
enum already returned, in addition to a valid type that is compatible with both
of the arguments. The returned type is needed to be able to do implicit casting
and promotion.

Arithmetic promotions may need to be separated from other conversions.

Return types depending on whether or not the arguments are being used in an
intermediate computation (and will therefore be promoted) or actually need to be
assigned to a destination, in which case the 

The function(s) responsible for handling casting will be reused when handling
promotion, and as such promotion will internally be considered as a specific
case of implicit casting.

## Parsing integer/character types
Integer and character types are surprisingly complicated to parse.
There are a couple of rules governing their specifiers:
- each specifier can only occur once (long long does not exist in ANSI)
- char and int are mutually exclusive
- long and short are mutually exclusive
- signed and unsigned are mutually exclusive
- long and short may not occur in a specifier that includes char

The standard lists valid combinations. It appears that the standard intends
that the order of specifiers matters, which would make legal combinations
less permissive, but much like requiring variable declarations at the beginning
of the block, this is more difficult to implement than allowing them to occur in
any order.

Actually, it doesn't really make sense to have type flags for signedness, since
they only get used for integers, unlike all of the other type flags, which are
storage class modifiers and type qualifiers.

So going back to SIGNED and UNSIGNED instead of INT might make sense. It would
require a little more thought when processing integer types though. The above
rules need to be mapped to a base type and a width.

An integer will be passed around to the integer type checking functions, which
is used to track which tokens have occurred so far in the specifier. Each of
"short", "long", "int", "char", "signed", and "unsigned" will have their own
bit in this integer.

The function will descend into the list of specifiers, marking which ones have
occurred as it goes. If any of the flags are set twice, the function errors. If
any flags that may not co-occur are set, the function errors.

Afterwards, there will be a switch statement with a case corresponding to valid
combinations of flags. The default case will result in an error.

## How to evaluate types
With only one token of lookahead, the parser cannot distinguish between an
identifier that refers to a typedef'd type and an identifier that refers to an
object when it occurs between parentheses.

This may mean that the type checker will be responsible for determining whether
such statements are valid. This would mean that the following construction would
be legal:
```
cast_expr : pexpr_list expr
          ;
pexpr_list: pexpr_list pexpr
          | pexpr
          ;
pexpr     : '(' expr ')'
          ;
```

----------------------------------------------------------------
Expressions may only occur directly next to each other if all of them,
with the exception of the rightmost expression, are surrounded by parentheses.
If this is the case, all but the rightmost expression are interpreted as being
a part of a cast operator.

`pexpr` must have its own rule in the grammar, and it will occur in both the
rules for expressions and for casting. Expressions cannot normally occur
immediately next to each other. The parser should be able to reduce an expression
in parentheses to a pexpr, at which point it begins constructing the next
nonterminal. If the next nonterminal is another pexpr, or a 

## Tree node attributes
Aside from types, tree nodes (I think just those related to expressions, but
possibly others as well) should have an attributes field. This field should
contain information about the expression that does not belong with the type of
the expression.
- compile time constants
- lvalues (applies to all identifiers)
- boolean (applies to integers guaranteed to be zero or one)
- virtual address (applies to integers of the appropriate width)
- requires a virtual register (intermediate value)

The oc language defined a vaddr (virtual address) attribute, though I am not
sure this would be useful here. Pointers are already codified as types.
It would be possible to reduce pointers and arrays to integers of the
appropriate width with a vaddr attribute set on the nodes they appear in, but
that may be complicated. It would however reduce the gap between type
information and the actualy assembly generated, since, much like with booleans,
most instruction sets do not differentiate between integers and memory addresses
and treat them equally.

On the x64 platform, arrays and pointers would have the the base type
`TYPE_UNSIGNED`, with a width of 8 bytes and the virtual address attribute set.

## Parsing function types
Structure of a function definition in the syntax tree, as a reminder:
- FUNCTION
  - TYPE_ID
    - TYPE
    - IDENT
  - PARAM
    - TYPE_ID ...
  - BLOCK (optional)

First, validate_type_id will be called on the TYPE_ID node. The type of this
node will be assigned to the nested field of FUNCTION's type. The data field
of the function's type structure will be allocated and initialized as a linked
list for storing its parameters. A SymbolValue structure associated with this
function will be initialized, but not inserted into the global table yet. 

Second, validate_type_id will be called on each TYPE_ID child of PARAM. Once
validated, a SymbolValue structure will be initialized for each parameter and
appended to the list in FUNCTION's type structure.

Once all parameters have been validated, only then will the type checker attempt
to insert FUNCTION's symbol into the table.

## Casting
In C, there aren't as many legal casts as I had previously assumed. You may not
cast between struct types, and casting to and from unions is only allowed via an
extension. This leaves us with few casts which need consideration:
- Casting between pointer types. Doing so explicitly is allowed, but is
  undefined behavior if the destination pointer has stricter alignment
  requirements.
- Casting and promotion between arithmetic types.
- Casting between integers and pointers.
- Casting between function pointer types. I may also allow casting functions to
  and from void pointers, even though that is not required by the standard.

## The arrow operator
Accessing members of pointers to structs with the arrow operator is equivalent
to dereferencing them and then accessing them with the dot operator, so maybe
the parser should just add the necessary tree nodes to make those two operations
look equivalent in the abstract syntax tree.

## Passing information during type checking
The return values of the type checker's internal functions should be a flagset
which can be used to indicate different things about the status of that branch
of the recursion. The most obvious one I can think about right now is one where
the value of an expression is a constant, which is a requirement of global
variables.

## Determining what instructions and registers to output
My current thinking is that there should be a single function with a switch
statement, `write_instruction`, which is mutually recursive with a bunch of
other functions that do all the work related to writing the outputs of other
functions. For uniformity's sake, this function should not allocate any space
itself and should be provided the space that the handler function should write
to, which it passes along.

## Mapping operators to instructions
It would be convenient to have a mapping from tokens to instructions (a map from
`enum` to `const char *`, but because of the way my map data structure is
written this is currently not possible, as both arguments must be pointers and
you cannot take the address of an enum.

## Resolving symbols in assembly
Actually why not just have a character field in each symbol value that stores
a string identifying the address. Labels are addresses, as are offset in the
form `rsp + N`. Both can be put in square brackets to refer to the location
in memory instead of the value. Both can be used in both the `lea` and `mov`
instructions.

The function that resolves symbols should, for now, just move the location
where the symbol lives into a register using the `lea` instruction, so that
either representation can have the same code emitted.

## Handling function parameters
To make accessing variables uniform, parameters should be immediately written
to the stack at the beginning of the function. This sequence of operations will
look similar to the way a local variable declaration appears in assembly.

## postfix and prefix increment and decrement
Increment and decrement require an lval as their argument, which means that the
InstructionData associated with its argument should be a MOV and its source
should be the memory location of the object.

This memory location can be copied and used as the argument for the increment or
decrement.

On x64, the same two instructions can be used for the prefix and postfix
versions of these operators:
- inc/dec in memory
- mov from memory to a register

Prefix operations perform the operations in the order above, while postfix operations
reverse the order.

## Assigning special roles to vregs
In assembly languages, registers broadly fall into three categories:
- saved general purpose registers
- temporary general purpose registers
- other registers

These registers may have other, more specific roles that they can or must be
used for, including:
- return address/link
- frame/base pointer
- stack pointer
- subroutine arguments
- subroutine return values
- syscall number

For now, the generator only needs to be told the number of volatile (N+M) and
nonvolatile (P) registers. It will assume that the 0th volatile register is used
for subroutine return values, N volatile registers are used for subroutine
arguments, and one nonvolatile register is used to store the stack pointer.

So, with the above constraints, the register layout will look like this:
- vr0 is for subroutine return values
- vr1-vrN are for subroutine arguments
- vr(N+1)-vrM are volatile general purpose registers
- vr(M+1)-vrP are nonvolatile general purpose registers; vr(M+1) holds the stack pointer

Later, there will be a way to inform the generator about other special registers
listed above.
