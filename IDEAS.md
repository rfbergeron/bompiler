# Ideas
This document will be where I do my thinking. It will probably be messy and not
very readable, but that's fine because my brain garbage needs to go somewhere,
and that somewhere can't be my design document like it was before

## Things to hold off on
There are a few features that seem difficult to implement, not important for the
purpose of achieving self-hosting, or both. These include:
- Variably modified types: I don't think I'll be using these, and they seem like
  a pain in the ass to implement
- Variadic functions: I'm not sure how much of the heavy lifting is done by the
  standard library here, but we'll see
- typedefs: might not be that much of a headache to implement, but we'll see

## Storage of string constants
Currently, the type checker/symbol table is repsonsible for tracking string
constants. It would make more sense for the string set to track them in some
way, it would also make more sense for the constants to be registered when the
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

## Collapse type/variable names
Working with names and typeids might be easier if they are somehow collapsed
into a single node in the tree, instead of separated out
