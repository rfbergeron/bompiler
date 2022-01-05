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

## Labels
Based on the language definition, it looks like labels are attached to the
statement immediately following them. They are not a valid statement all on
their own. Just something to note for later.

## Namespaces
Labels have their own namespace. Structures and unions share a namespace
separate from other types of declarations, and functions and objects share a
namespace.

## Increment and decrement
These operators must have an lval argument. Postfix and prefix versions must
have unique tokens so that they may be distinguished outside of the parser.

## Storage of string constants
Currently, the type checker/symbol table is repsonsible for tracking string
constants. It would make more sense for the string set to track them in some
way; it would also make more sense for the constants to be registered when the
abstract syntax tree is built, since the value and type of constants are known
when the tree node is generated.

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

## Declarators and abstract declarators
A declarator is a syntactic unit which includes an identifier and type
information (type specifiers and qualifiers, pointers, array dimensions,
function parameters).

An abstract declaractor is the same, except that it does not include the
identifier; it is free-floating type information.

IMPORTANT NOTE: the declarator form `direct-declarator ( identifier-list )` is
only used when writing old style functions, which this compiler will not be
doing, so it should be ignored (for now).

## Storing type information
Type information will have two structs created for its storage: `TypeSpec` and
`AuxSpec`. `TypeSpec` will directly store information that is derived from
declaration specifiers. It will also contain a list of `AuxSpec` structs, which
will store other type information.

Both of these structures will have a copy, init, and destroy function associated
with them, to make (re)using the information stored in them easier.

## Special handling of function identifiers
Function identifiers and pointers receive special treatment based on their
location in an expression. When used on their own in/as an expression, they
are automatically converted to a pointer to a function of their type. When
function pointers are used during a call, the type of the expression is
to a function of the appropriate type, minus the pointer specifier.

Within the implementation, it may be more convenient to treat lone identifiers
as the special case when used as the identifier of a function call.

## Propagating and resolving type information
Currently, the "primary" storage location for type information is in the symbol
table. This information is accessed from other locations, namely nodes of the
syntax tree, via reference. Syntax tree nodes are only indirectly responsible
for managing this memory by allocating and freeing these tables as necessary.

Because it is sometimes convenient to construct new `TypeSpec` objects, rather
than referring to existing symbol table entries (for example, when using the
indirection, address-of, and subscript operators) it may be sensible to
break the rule of having the symbol table be solely responsible for all type
information in one-off instances like this.

Specific syntax tree nodes require a new type object to be constructed:
- the subscript operator `[]`
- the indirection operator `*`
- the address-of operator `&`
- isolated function identifiers, which become a pointer to a function of that
  identifier's type (unless the address-of operator is used explicitly)
- implicit and explicit casts

Isolated function identifiers could be handled more uniformly by inserting an
address-of operator into the syntax tree and creating the new type object there
instead of creating a special case for lone function identifiers.

## Parsing complex (function, pointer, array) types
This implementation will support arbitrarily complex types, which means
arbitrarily nested pointers and n-dimensional arrays, which may co-occur.

First, the `TOK_SPEC` associated with a declaration will be validated, and
the type information recorded in a structure. This information will be
copied once for each declarator.

Second, the type information associated with each declarator will be processed
and used to fully specify the type of the identifier, which will then be
validated.

When comprehending a type, information between parentheses takes highest priority.
Array and function information is read from left to right. Pointers are read
from left to right, and finally the information included in the declaration
specifiers is read out.

## Tree structure of declarations
Each declaration has a top level node constructed by the parser with the token
`TOK_DECLARATION`. This node carries no type information and is used to group
the components of a declaration together.

The first child of a `TOK_DECLARATION` node is always a `TOK_SPEC` node
constructed by the parser, which groups together declaration specifiers.

All other children are either declarator nodes or expression nodes containing
the initial value of an object.

Declarator nodes are constructed by the parser and have the token
`TOK_DECLARATOR`. These are used to organize the components of a declarator in
a sensible way. Iterating over their child nodes from smallest to largest index
gives an accurate reading of the type they describe. The final child of such a
node should be a `TOK_IDENT`.

Each declarator node may be followed by an expression node if the declared
object is initialized at the same time.

```
TOK_DECLARATOR = ( TOK_DECLARATOR | TOK_IDENT ) ( TOK_INDEX | TOK_FUNCTION )* TOK_POINTER*
```

## Increasing speed of type and expression comparison
To make identifying common subexpressions and types easier, it may be worthwhile
to implement some kind of recursive hashing mechanism like those seen in
distributed databases, so that common subexpressions can be identified with
simple checks.

It would obviously not be useful in quite the same way, since recursively
examining hashes of subexpressions may prove fruitless.

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

## The arrow and square bracket operator
Accessing members of pointers to structs with the arrow operator is equivalent
to dereferencing them and then accessing them with the dot operator, so maybe
the parser should just add the necessary tree nodes to make those two operations
look equivalent in the abstract syntax tree. A similar thing could be done with
the square bracket (array indexing) operator.

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

## Pointer arithmetic
Integer values may be added or subtracted from a pointer; when this happens, the
pointer is assumed to be the member of an array, and the integer value is
multiplied by the width of the object the pointer refers to before the operation
is carried out. The result is a pointer to another object in the array, or is
otherwise undefined.

Two pointer values may be subtracted from one another. According to the
standard, they must be of the same type and located in the same array; this
implementation will forgo that for now for the sake of simplicity. The result
of this operation is an integer whose exact type is implementation-defined
(recommended to be ptrdiff_t by the standard) that represents the displacement
between the two objects. The displacement can be calculated by performing the
subtraction, then dividing the result by the width of the objects pointed
to by the operands.

The standard does not mention anything about adding pointers. The type checker
should refuse attempts to add pointers of any type, or to add or subtract any
two pointers of differing types.

This shouldn't be too difficult to implement with the current structure of the
type checker; most of the above is ruled out by disallowing implicit casts
to and from pointer types besides `void`, which the standard already requires.

It may additionally be required that adding/subtracting a void pointer from any
other pointer not be allowed (no implicit conversions and the fact that the
object underlying the pointer has no specified width), but I am not certain of
this.

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
Promotion rules for arithmetic values are as follows:
- unsigned types are promoted to signed types
- integers are promoted to wider types
- any integer less wide than `signed int` is promoted to `signed int` when used;
  the increase in width can be optimized away by compilers but not the change in
  signedness.

Casting/compatibility rules are as follows:
- arrays and pointers are compatible, so long as the underlying types are compatible
- pointers to functions and functions are compatible, so long as the underlying types are
  compatible and the pointer is on the left hand/destination side
- all explicit casts are allowed, without exception; be mindful of width

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

# Printing of symbol table
Unfortunately, because of the way symbols are stored, it would be difficult to
organize them in a way that is readable in the same way as the abstract syntax
tree.

For now, the thing that makes the most sense is to list the details of the
abstract syntax tree node, followed by a list of all the symbols stored in the
table associated with that node.

## Functions
### `symbol_value_print`
Takes as arguments the `SymbolValue` to be printed, the buffer to be printed to,
and the length of the buffer. Prints the declaration location, followed by the
base type and auxiliary information. More information will be printed out as
those features are implemented.

### `symbol_table_print_table`
Takes as an argument the `const map *` to be printed. This function will use
`map_foreach_pair` to iterate over every symbol in the current scope. Pairs will
be passed as arguments to `symbol_table_print_entry`.

### `astree_print_symbols`
Takes as an argument the `ASTree *` node whose symbol table is to be printed,
the output file to be printed to, and the depth of the node in the abstract
syntax tree.

## Silly extensions
A fun extension would be the ability to return fixed-length arrays by value.
The function declaration/definition would need the size of the returned array
to be specified in the declaration. The appropriate amount of space would be
allocated on the stack by the caller, and a pointer to this space would be
passed as an additional parameter to the function. Attempts to convert the
returned value directly into a pointer would fail, since semantically the
returned value is an lvalue, whose address cannot be taken? I think?
