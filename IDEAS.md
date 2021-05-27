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
