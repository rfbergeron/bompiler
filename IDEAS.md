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
  this in the first place
- Variadic functions: I'm not sure how much of the heavy lifting is done by the
  standard library here, but we'll see
- typedefs: might not be that much of a headache to implement, but we'll see
- floating point: learning x86 is going to be enough trouble without dealing
  with AVX/SSE extensions
- passing structs by value: so far as I can tell the standard does not specify
  how this should be done, only that it must be done. GCC packs them into the
  registers if they fit. I don't want to worry about this and my compiler will
  not be doing it so I won't need this to achieve self-hosting

## Storage of string constants
Currently, the type checker/symbol table is repsonsible for tracking string
constants. It would make more sense for the string set to track them in some
way; it would also make more sense for the constants to be registered when the
abstract syntax tree is built, since the value and type of constants are known
when the tree node is generated.

## Statements and expressions
Validation of statements and expressions should be cleanly separated, instead
of grouped up in a single massive switch statement.

## Scoping
Instead of the current scheme, where we only differentiate between local and
global scope, we should implement a 'scope stack' that is pushed onto with
each nested block. There should be a function that searches for a given typeid
in the stack from the top down.

The function for making global and local entries should be condensed into a
single function that just makes the entry in the scope that is on the top of the
stack if it is not already present.

Looking at the standard, it looks as though declarations of any kind can occur
at any scope (ie function and struct declarations can occur inside of another
block), so it would probably make sense to have only a single stack on which
all declarations, be they of functions, structs, or variables, are pushed. This
is opposed to the current scheme where global scope, local scope, and type scope
are all considered to be separate things.

As an aside, should labels be aware of their scope? I'm not sure if the type
checker needs to be aware of scope when it comes to labels.

## Type ids
Currently, types besides structs are hard-coded attributes. It may be easier to
incrementally add types if all types (including primitives) are stored in the
'type_names' map.

The 'attributes' enum would probably have to be expanded out to a full struct,
with a bitset used to define attributes like 'lval' or 'vreg', while other type
info would be stored as a pointer to an entry in the 'type_names' map.

Entries in the 'type_names' map should then additionally include the size of the
type, and, as they do now, the number and names of its member types.

Storing type names like this might make determining whether types are compatible
easier, since types that are synonymous on amd64 could store the same values,
which would be valuable when comparing types.

## Comparing types
The `types_comatible` function should return more than just an enum describing
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

## Collapse type/variable names
Instead of keeping the full declaration of a symbol in the syntax tree, it might
be easier to remove all nodes except for the identifier and immediately store
the symbol's information in the table. This way, no special handling of typeids
would be required during the type checking process.

Doing this for functions, structures and unions would be somewhat complicated,
however. It would also require the inclusion of `typecheck.h` in `astree.c`,
which may cause dependency issues.

## The arrow operator
Accessing members of pointers to structs with the arrow operator is equivalent
to dereferencing them and then accessing them with the dot operator, so maybe
the parser should just add the necessary tree nodes to make those two operations
look equivalent in the abstract syntax tree.
and then accessing them with the dot operator, 

## Passing information during type checking
The return values of the type checker's internal functions should be a flagset
which can be used to indicate different things about the status of that branch
of the recursion. The most obvious one I can think about right now is one where
the value of an expression is a constant, which is a requirement of global
variables.
