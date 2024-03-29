# Ideas
This document will be where I do my thinking. It will probably be messy and not
very readable, but that's fine because my brain garbage needs to go somewhere,
and that somewhere can't be my design document like it was before

## Tempering expectations
A couple of non-goals:
- Full standards compliance: I will relax the standard if it makes the
  implementation simpler without introducing ambiguity; accepting code that a
  standards compliant compiler would complain about isn't a big deal.
- Floating point: Floating point arithmetic is a secondary concern.
- Preprocessor: I do not plan to implement a preprocessor.
- Lexing/Parsing: the compiler uses Flex and Bison. It does not implement its
  own lexer and parser.
- Assembler: The compiler will not include an assembler.

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

## Things that I DO NOT have to do
A problem that I have noticed when designing the compiler is that I made a lot
of assumptions about what the C type checker is required to do. I have learned
that has does not do most of the things that I thought:
- You do not need to explicitly convert integer types to a narrower types
- You do not need to explicitly convert signed types to unsigned types
- You do not need to check the bounds of an array access when the expression
  between the brackets is a constant.

## Strange things that I have learned are syntactically valid
- You may use the arrow operator directly on an expression of array type, which
  treats the array as a pointer to the first element of the array (which it
  is, more or less), which you can then perform member access on if the
  underlying type is a struct or union.
- In fact, in all but a few cases, arrays are treated as above.
- The initializer for objects of arithmetic or pointer type can be enclosed in
  braces
- Function parameters can be declared with array or function type. However, they
  are treated as if they were declared as pointers (pointer/pointer to function)
- You can only implicitly convert between `void*` and other pointer types, not
  any other kind of void pointer (for example `void**`).
- Struct and union members may not have storage class specifiers
- Tags and enumeration constants declared as struct and union members are
  "hoisted" up into the enclosing scope

## Directive insertion for strings and static locals
The declarations of static local variables and string constants should not be
interleaved with the definitions of functions or global variables. To prevent
this, there must be another insertion point that can be used besides the end of
the instruction list or the iterators in the syntax tree.

A single iterator should be sufficient, based on the following observations:
1. Function definitions are translated in two phases, one before and one after
   the body. The iterator can be adjusted before the body is even parsed.
2. Variable declarations/definitions are parsed from the bottom up. String
   constants used in variables should be emitted before the code initializing
   the variables without any interference. This is true for global and static
   local variables.
3. Since variable declarations/definitions are parsed from the bottom up, the
   iterator can only be adjusted after the definition has been completed, and
   should point to the last instruction emitted for the declaration/definition.

## Directive Usage
### File Scope Variables
```
        (.bss|.data|.section .rodata)
        .globl NAME (unless linkage is static)
        .align ALIGNMENT
        .type NAME, @object
        .size NAME, WIDTH
NAME:
        (.zero|.byte|.value|.long|.quad) (INITIALIZER|COUNT)
        ...
```

### Functions
NOTE: in gas, when a directive expects a numeric argument and `.` appears in
the text, it is treated as the address/program counter/whatever of the current
line. In the following text, `.-NAME` would be the number of bytes between the
label `NAME` and the end of the function.
```
        .text
        .globl NAME (unless linkage is static)
        .type NAME, @function
NAME:
        ...
        .size NAME, .-NAME
```

### String Constants
NOTE: string constants will need to be relocated outside of functions and file
scope initializers somehow.
```
        .section .rodata
        .align ALIGNMENT (optional?)
GEN_LABEL:
        .string STRING
```

## Generalizing Initialization Code
Initialization is a similar procedure at both file and block scope, so the same
functions should be used for both of them. In order to generalize the existing
functions to be able to handle block scope variables, we need at least one
additional piece of information: the displacement of the aggregate a member
belongs to.

This displacement will be passed as an additional parameter to all initialization
functions. We can use some constraints on the code generated by the compiler to
determine whether or not the symbol is global based on the value of the offset
alone.

Local variables use `rbp`-relative addressing, therefore their offsets should
be strictly negative. Parameters passed on the stack alse use `rbp`-relative
addressing, but their offsets should be strictly positive. (`rbp`+0 is a saved
register) Global variables use `rip`-relative addressing, so their
offsets should always be zero. Members of aggregate types should always have
offsets >= 0.

So, if the offset is strictly negative, the variable is on the stack, and this
knowledge can be used to determine which code to emit.

A special case is made for scalar variables which are not a member of an
aggregate type; if the displacement parameter is zero, it must be made sure that
the displacement of the scalar itself is also zero before deciding that it is
a global variable.

## File-scope Initialization Code
For now, initializer lists will just be traversed a second time, after the type
checker has finished analyzing the whole list.

Most of the logic is broken up into functions which initialize a specific types:
one for scalars, one for structs, one for unions, and one for arrays.

The scalar takes as arguments a type and and initializer, and returns a status
code. The enclosing braces of the initializer are stripped away, if present, and
emits the appropriate directive for the size of the type, along with the value
of the initializer.

The aggregate functions take the same arguments as the scalar function, plus an
index, and return the number of initializers consumed from the list. If this
number is negative, the function encountered an error. If there remain any
uninitialized bytes after all possible initializers have been from the
initializer list, this function emits a `.zero` directive whose argument is the
number of bytes not explicitly initialized. This process also happens for unions
whose first member's size is smaller than the union's size.

The aggregate function select which function to recursively call to initialize
a member based on the type of the member and the symbol of its corresponding
initializer:
- If the type is scalar `init_global_scalar` is called
- If the type is aggregate and the initializer is scalar,
  `init_global_aggregate` is called with the member type, the current
  initializer list, and starting index equal to the current index in the current
  initializer list
- If the type is aggregate and the initializer is an initializer list,
  `init_global_aggregate` is called with the member type, the new initializer
  list, and starting index of 0

The struct function tracks the total number of bytes initialized as it iterates
over the initializer list. If this number is not properly aligned before it
attempts to translate the initializer for the next member, it emits a `.zero`
directive whose argument is the number of padding bytes needed to align the next
member.

## Empty Expressions, Uninitialized Variables and For Loops
Because for loops might not have all of their control expressions set, no-ops
need to be generated so that the iterators are set correctly and labels can be
inserted into the correct locations in the generated code. This is hard with
the way for loops are currently parsed.

`EMPTY_EXPR` will be renamed to `EMPTY_NODE` or similar. Empty
statement-expressions will not use `EMPTY_NODE`, instead creating a dummy node.
This node will have a single no-op instruction associated with it.

For loop control expressions will instead be parsed as if they were two
statement-expressions, optionally followed by a third expression. If absent,
the reinit expression will be treated the same as an empty statement-expression.
If the conditional expression is empty (signified by the node having the symbol
';'), an unconditional jump to the for-loop body will be emitted, instead of the
usual test-and-jmp sequence.

Block scope declarations that have no initializer(s) or that have static storage
will also have a single no-op instruction associated with them. This is to
ensure that their parent node can always copy the iterators which point to the
first and last instructions of its children.

## Pointer Dereferencing/Indexing and Arrays
Accessing the underlying elements of pointers and arrays is different when the
element type is an array. Because the elements of multidimensional arrays are
allocated sequentially, the bracket and indirection operators should use `lea`
when accessing elements of array type, instead of `mov`.

## Emitting Conversions
The IL generator needs to select and emit conversions for operands based on the
desired type and whether or not the operand is an lvalue, and whether or not
the lvalue should be converted to an rvalue. This is partially covered by a
series of macros, but they are insufficient and poorly named.

There should be a function or macro that does the following:
1. Converts lvalues into rvalues
2. Converts the rvalue to the desired type
3. If nothing else, puts the rvalue into a register

Since aggregates and functions (not function pointers) cannot be cast and are
either unassignable or have complicated assignments, this function should not
be called on them. Expressions of these types should have their own separate
code paths, or should not appear on the right hand side of an expression.

The code generator should make sure that lvalues are always passed in registers,
so that no other conversion function is necessary.

## Correct Parameter and Return Value Passing
The AMD64 System V ABI specification contains an algorithm describing how
parameters are to be passed. Fortunately, since I do not (at the moment) plan
to implement floating point arithmetic, a lot of the algorithm can be ignored,
but it does include some important information.

Aggregates can be passed through registers as arguments or return values under
the following circumstances:
1. The aggregate may not be more than 16 bytes wide.
2. The aggregate may not contain unaligned fields (not a problem here)
3. When passing parameters, there must be enough registers available to hold the
   entire parameter in registers. If the aggregate is <= 8 bytes in width, there
   must be one register available. Otherwise, if the aggregate is <= 16 bytes in
   width, there must be two registers available.

And that's it for now. It would behoove me to actually implement the algorithm
in full at a later date, but sticking to these rules should be fine for now.

The ABI does not care about the layout or the members of the aggregate. The
contents are simply copied into the registers as they are. If the aggregate is
greater than 8 bytes in width, but less than 16 (including padding), then the
upper portion of the aggregate is expanded, such that it occupies a whole 8 byte
register.

Parameters that do not fit in registers are passed on the stack in reverse
order. Parameters on the stack always occupy 8 bytes, regardless of their acutal
size. If `rsp + 8` would not be a multiple of 16 after pushing the arguments on
the stack, 8 must be subtracted from `rsp` before pushing the arguments to
ensure proper alignment.

## Underaligned Aggregates as Parameters
GCC handles aggregates with alignment requirements less than 8 by moving the
components individually and shifting, or by doing unaligned memory accesses.

This is complicated, so I'm going to take a few shortcuts. Aggregates shall
always occupy stack space in multiples of 8. So, 1, 4, 5, etc. byte aggregates
would occupy 8 bytes on the stack, 9 bytes -> 16 bytes, 23 -> 24 bytes, and
so on.

This makes passing aggregates easier, both on the stack and in registers.
Simple `mov` instructions can be used to accomplish either.

Since the GNU allocator implementations always return 8-byte aligned pointers,
this should also be safe when dereferencing dynamically allocated memory.

## Stack alignment
Function stack frames abide by the following rules to maintain alignment:
1. The size of the space occupied by the local variables and spilled registers
   shall always be a multiple of 16.
2. All preserved registers will be pushed to the stack in the function prologue,
   and all volatile register will be pushed to the stack before function calls.
3. If the number of eightbytes passed to a function on the stack is even or
   zero, 8 must be subtracted from `rsp`.

## Eightbyte and Parameter Information
Aggregates and functions do not need to carry eightbyte information as a
separate member; it can be calculated from existing type information without
much trouble.

## Parameter Locations
Parameters passed in registers will be moved onto the stack at the beginning of
the function. Aggregates will occupy stack space in multiples of 8, as specified
above, while integral types will occupy only as much space as they need, with
padding inserted between.

Parameters passed on the stack will be left where they are.

## Intermediate Language Generator Refactor
I will not be generating the elaborate language or three-address code that has
been described in `INTLANG.md` or `TAC.md`. Instead, I will continue to generate
what is for the most part AMD64 assembly with an infinite number of registers.

However, the code that accomplishes this will need to be changed to accomodate
recent changes to the symbol table, constant expressions, and jumps. At the same
time, I would like to reimplement the way that instruction data is stored.

### Instruction Data
Instruction data is currently stored largely as formatted strings. While
convenient for printing, it makes determining what kind an operand is difficult.
Instructions and operands should be stored as numbers and unions of the
different addressing modes.

The new format will be a union whose members are structs which roughly
correspond to the different addressing modes under AMD64. These members are:

#### `MODE_IMMEDIATE`
Operand will be an immediate value, stored as a `uintmax_t`.

#### `MODE_REGISTER`
Operand will be a single register. Width is specified using another struct
member.

#### `MODE_DIRECT`
Operand will be a label/address, with an offset. Offset will be zero if no
offset is necessary.

#### `MODE_INDIRECT`
Operand will be a single register, with an offset. Offset will be zero if no
offset is necessary.

#### `MODE_SCALE`
Operand will be two registers, a base and a scaled index, along with an offset.
Offset will be zero if no offset is necessary. Scale is specified using another
struct member. Register width is assumed to be 64 bits.

There will additionally be a `MODE_NONE` with value zero. If an expected operand
has this mode, the instruction is invalid. Can be used in asserts.

`InstructionData` structs will also be allocated by the function populating
them, rather than their caller.

### Addressing Struct/Union Members
The `offset` member of `SymbolValue` is already used to represent the offset of
the members of structs and unions. This usage is fine and can be used as-is to
address members of global structs that are not nested, but is unsuitable for
accessing members of structs on the stack or for accessing members of nested
structs in any case.

We could use one `mov`/`lea` per arrow/struct reference operator, or we could
collapse these moves into a single instruction by taking the sum of the offsets
at generation time. This would require its own separate function.

Perhaps at a later date I will perform the offset calculation at generation
time as an optimization, but for now multiple instructions are fine.

### No more flags
Since intermediate language generation will be done on the same pass as lexical,
syntactic, and semantic analysis, functions do not know whether their caller
wants the value or location of identifiers, or whether or not their sibling
operands placed their result in a register.

This is an easy fix. Expressions which produce lvalues will always return and
address, and not a value. Expressions will always place their result in a
register.

### List initialization and constant values
List initializers are required to have their components be a constant
expression. The rules defining what is legal in an initializer list constant
expression effectively limit the value of the constant expression to one of two
things under x86: an arithmetic constant or a label which represents the address
of a static or extern object.

This means that the initialization of aggregate types, and initialization by
constant expression in general, could potentially take as little as one
instruction.

There should be a helper function called to initialize persistent objects. The
arguments to this function would be a little different from others.

Translating list initializations will require duplicating the code for
traversing initializer lists that is present in `tchk_decl.c`. I could avoid
the complexity and mistakes that arise from this by having the current traversal
create a list of the values of each member of the struct.

## Single pass refactor
Intermediate language generation will be performed in the same pass as lexing,
parsing, and type checking. This will require a couple changes to the global
state and also possibly to `ASTree`.

### Jump statement tracking
The break, continue, and selection stacks in the global state will store more
information as follows:

The continue/iteration stack will store:
- the jump id of the statement it manipulates
- a pointer to the label its jump targets

The break stack will store:
- the jump id of the statement it manipulates
- a pointer to the end label its jump targets

The selection/switch stack will store:
- the jump id of the selection
- a count of the number of case statements belonging to the switch statement
- a pointer to the label of its default statement
- a pointer to the label of the next case statement
- the `TypeSpec*` for its controlling statement
- a set containing all values of its case statements

The reason the labels are included is so that the labels are not duplicated for
each instruction that uses them. The label is malloced once and responsibility
for the resource is passed on to the instruction holding the end and condition
labels, to the case statement, or to the default statement.

The latter two entries don't have anything to do with intermediate language
generation, but are necessary for the compiler to operate according to the
standard. Currently the compiler does not convert the constant expressions for
case statements to the type of the controlling statement, nor does it report an
error when multiple case statements have the same value after promotion.

Including type information with switch statement entries will require a minor
change to the parser to get the type of the controlling expression before the
body of the switch statement is parsed.

### `lyutils` additions
Rather than have the parser call type checker functions, which then call
intermediate language generation functions depending on the success or failure
of the type checker, more functions will be introduced to `lyutils` and to the
end of `parser.y` which call the type checker and assembly generator depending
on command line flags and the success of the individual components.

The names of these functions will have the form `produce_XXX`. These functions
will respect the command line flags for skipping type checking and intermediate
language generation. Intermediate language code will not be generated if type
checking fails.

### Parameters for translation functions
Intermediate language translation functions currently take as arguments an
`InstructionData` to place their output into, the compiler state, and a the
syntax tree node to translate. The data for the instructions producing the
operands are allocated by the function and populated by recursive calls to other
translation functions.

Without recursive calls, the functions will have to get data about their
operands from their arguments. `ASTree` nodes will need to have a field added
representing the instruction(s) that represent the subtree it is the root of.

This will be accomplished using a new badlib structure, `llist_iter`, an
iterator into a linked list. Iterators will just be a typedef of the llist
`Node` struct. Tree nodes will store two iterators, one to the first instruction
of the subtree, and one to the last instruction of the subtree.

Parent nodes can then insert instructions before, after, or in between the
instructions of their child nodes using these iterators. They can also use
these iterators to add labels or modify the instructions.

## Unique Identifier Construction
It must be possible to create unnamed, unique structs, unions and enums. These
values do not really need to go into the symbol table; none of the operations
that use tags need any more information than a special value (like the empty
string) indicating that the tag is unique.

However, ownership of the resources allocated to store the tag's information
becomes more difficult: the tag cannot go into the symbol table, so a tree
node must be responsible for freeing the tag's resources.

Similar complications happen when dealing with type names as function
parameters, and type names used in casts and in the `sizeof` operator.

I think a nice solution would be to construct unique names for these types that
cannot conflict with other type names, and to put these types into the symbol
table.

Since the names aren't important beyond the fact that they need to be unique,
they can be constructed from the source location and the type information.

## Valid redeclarations
The rules dictating when redeclarations are valid do not directly have to do
with whether or not a symbol is external or block.

If there is already at least one declaration for a symbol, subsequent
declarations are only valid if the following are true:
- neither symbol may have no linkage
- if the symbol was declared with external linkage, it may not be redeclared
  with internal linkage
- if the symbol was declared with internal linkage, it may not be redeclared
  with external linkage. as an exception, if the second declaration has external
  storage class, then the declaration is valid

## Enumeration constants and meaning of enum objects
While most compilers will provide warnings when trying to assign an enum of a
different type to another enum, they are not required to do so, since the
standard says to evaluate enumeration constants and objects with enum type as
if they were integers. Functions checking if a value is arithmetic, scalar, etc.
should reflect this.

## Minor type checker refactor
I think part of the reason I get so confused when going through the type checker
code, particularly for expressions, is because functions like `types_compatible`
and `determine_conversion` are poorly named and not terribly useful. Many of the
type checker functions for operators do their own checks on the validity of the
types of their operands, so having a function check the compatibility of the
common type they are converted to after the fact is unnecessary.

### `types_compatible`
`types_compatible` should be replaced with a `types_equivalent` function, that
determines type equivalence as defined in the standard, and returns a boolean
as opposed an enum representing the "degree" of compatibility.

A new function will be added which determines compatibility as required by the
assignment operator, for use in the assignment operator and also for function
call arguments.

Initialization will have its own dedicated function for determining
compatibility between symbols and their initializers. This function may use the
function(s) for determining compatibility for assigment and function call
arguments in simple cases, but those functions are not to be dependent on any
of the functions for type checking initializers.

In all other places where `types_compatible` was previously used, the logic for
determining the compatibility of expressions will be written directly.

### `determine_conversion`
This function will be replaced with a much more limited version that only
operates on arithmetic types. Operators that require behavior for non-arithmetic
types should encode that behavior in their own functions. This replacement
function is called `arithmetic_conversions`.

### `perform_pointer_conv`
This function will be renamed and repurposed much like `determine_conversion`.
Instead of inserting nodes into the syntax tree, it will replace the type of
nodes which need to be converted. This replacement is named
`pointer_conversions`.

The only thing to watch out for in this function is to make sure that the
original type still exists somewhere and can be freed. The standard uses
language that makes it possible for expressions besides identifiers to have
array or function type, but I can see no way that this is possible.

### `convert_type`
Will also be removed and not replaced. The assembly generator would have to do
much of the same work regardless of whether or not automatic conversions get
their own dedicated node.

## Declaration refactor
To accomodate storage class and linkage, the functions handling declarations
must be fixed to better reflect the language of the standard and to make them
easier to understand.

For now, I will attempt to use `types_compatible` to determine equivalence of
two types, but I am not sure that this function will produce the correct result.
Type equivalence in the standard only takes into account type specifiers, not
type qualifiers or storage class specifiers, so those should be compared
separately.

### External declarations
External symbols may be declared multiple times, so long as they are defined
at most once and their types are considered equivalent. If a an external symbol
is meant to have internal linkage, the first declaration of the symbol must have
the `static` keyword, and so must all subsequent declarations. Declarations of
symbols with and without the `extern` keyword may be mixed arbitrarily, with the
only restriction that an `extern` object must have storage allocated in exactly
one other translation unit.

### Block declarations
Block symbols may be declared at most once in their scope. The exception to this
rule is that block symbols with the `extern` keyword may be declared multiple
times, but each declaration must use the `extern` keyword.

### Initialization
Currently, type checking for initilization reuses the code for type checking
assignments. This seemed like a good idea at the time, but the type checking for
initialization is actually much more complicated than for assignments, so the
code would probably be considerably more understandable if I further separated
the logic.

## Simpler structure handling
Structure handling could be simplified, removing the need to have a second pass
over the declarators of a structure's members. In this scheme, the members of a
structure or union would be added to the tag value's information and have their
width and alignment accounted for when they are declared.

However, to accomplish this, we need to add more global state. It is a single
`TagValue*`, pointing at the entry of the tag being defined. It is set in
`validate_tag_def` and unset in `finalize_tag_def`

## Mistake regarding struct/union members and enumeration constants
There are two mistakes I made in the way I handle struct and union member
declarations:
1. Struct and union members may not have storage class specifiers.
2. Struct, union, and enum tag definitions, as well as enumeration constants,
   declared inside of a struct or union definition, are "hoisted" out into
   the enclosing block scope.

The latter is an important consequence of the fact that struct and union members
have their own namespace, NOT their own scope, so the "scope" of member
declarations is still the enclosing block/file/function scope. The former makes
intuitive sense, and conveniently means I do not have to care about how to treat
typedefs defined inside of structs and unions.

I did not realize this until it was mentioned in passing in a cppconf/ndc talk.

## Scope/name space fix

### Incremental Solution

#### Tag declaration/definition fix
An enum member will be added to the symbol table. This enum must be set by a
parameter to `symbol_table_init`. It indicates what kind of symbol table is
being created. Valid enumeration constants will be:
- `MEMBER_TABLE`
- `TRANS_UNIT_TABLE`
- `FUNCTION_TABLE`
- `BLOCK_TABLE`

Roughly corresponding to the types of scope. Members technically occupy a
separate name space, not scope, but this namespace only holds functions and
objects, so for now we will continue treating it as a scope.

The separate name spaces in each symbol table will be initialized conditionally
on which enumeration constant was passed as a parameter.

The `state.c` functions will search down the table stack until they find a
symbol table with the appropriate map initialized. If they are unable to find
one, they will return failure.

#### Enumeration constant fix
Since the primary name space is used for struct and union members as well as
objects, functions, enumeration constants and typedef names, there needs to be
a way to bypass struct and union "scopes" so that enumeration constants can be
hoisted into the enclosing scope.

To preserve existing code, this will be implemented by popping all symbol tables
with type `MEMBER_TABLE` and storing them temporarily in the enum tag. When this
tag is finalized, they will be put back on the stack.

This should work without issue since enumeration constants shouldn't be
referring to objects or functions of any sort in their definition, and to do so
would be an error anyways. Error messages will be less helpful, however.
`TagValue` will also look a lot uglier.

### Refactoring
To better mirror the language of the standard, it may be best to split the
`SymbolTable` stack up, and change some names. A new union will be created,
referred to using the typedef name `NameSpace`. A `NameSpace` will consist of
an enum identifying the type of name space. Other name spaces will have
additional members, based on their needs:
- `LabelNameSpace` just has a single `SymbolTable`
- `DefaultNameSpace` has a stack
- `TagNameSpace` also has a stack
- `MemberNameSpace` also has a stack

These stacks are manipulated as follows:
- At the beginning of a translation unit, one entry is pushed to the default and
  tag name spaces. This entry is popped at the end of the translation unit.
- At the beginning of a function declaration, one entry is pushed to the default,
  tag, and label namespaces. These entries are popped at the end of the
  declaration, or at the end of the definition, if the declaration is also a
  definition.
- At the beginning of a struct or union definition, one entry is pushed to the
  member name space stack, which is popped at the end of the definition.
- At the beginning of a block, one entry is push to the default and tag name
  spaces, which is popped at the end of the block.

Unless special measures are taken, getting and inserting symbols from a given
name space always searches the table at the top of the stack first, followed by
the table below, and so on. The only exception to this is the label name space,
since there can only ever be one table.

## More constant expression stuff
After working with the compiler again, I've noticed a few flaws with the way I
have been handling constant expressions. The flags, as I am using them, are not
adequate for representing all the possible states a constant expression can be
in.

It is possible for an initializer constant to consist of an operation involving
the subtraction of two pointers, given that the two pointers lie in the same
static or extern array. This expression does not have an address in its value;
it is just an offset, but it is no longer a valid arithmetic constant
expression.

It is also possible for an initializer constant to be an integral value that has
been cast to a pointer and vice versa. Under these circumstances, the type of
the expression is not enough to determine which flags need to be checked on
which nodes.

The set of flags will indicate the following:
- the node as being a constant expression
- whether or not the node is an initializer constant
- whether or not the node's value includes an address

These three flags should be sufficient to determine how to compute the value of
a constant expression, and whether or not the constant expression is valid.

### General logic
If any of the operands of a node have the initializer constant flag set, then
the node should set the initializer constant flag.

### Making things easier
To make this easier to do, I could omit constant expression evaluation for
initializer constants. Unlike constants for case statements and array bounds,
which must be known at compile time to check for semantic correctness, the
actual values of these expressions do not need to be known. The only thing
that needs to be tracked to ensure correctness is the presence of an address
component in an initializer constant expression, which can be checked without
knowing the actual value of the expression.

If we do it this way, the only things that need to be tracked to ensure validity
are a flag indicating that a value is a constant expression and a flag indicating
that the constant expression is a valid array initializer.

When the initializer flag is set, the tracked value is just an address. This
value is used to make sure that when multiple addresses are used in an
initializer constant certain conditions are met, namely that when subtracting,
both addresses are the same, and that the address component of arguments to
relation operators are identical. If the address is zero, that is taken to mean
that the current value of the constant expression does not, at that point,
have an address component.

### Making things "harder"
I've come up with some simplifications and additions that should make it
possible to do a reasonable job evaluating expressions at compile time.

I have made the following decisions about how the compiler should behave when
evaluating constant expressions:
1. For the purposes of the conditional operator and logical operators, the
address of a static or extern object plus or minus an offset is considered to
always be nonzero and therefore true.
2. Relational operations whose operands are pointers to different static or
extern objects are not considered to be constant expressions.
3. Equality and relational operations whose operands consist of one pointer plus
or minus an offset and one nonzero integral constant are not considered to be
constant expressions.

Attributes will need to be redone:
1. mark expression as a constant expression
2. indicate that a constant expression can only be used for initialization
3. indicate that the value of the constant expression includes an address

Fortunately, it seems like I got the logic right when just taking into account
the addresses of initializer constants, and not too much work needs to be done
to include offset calculations in addition.

### Addition/subtraction logic
If both operands contain an address, the operator must be subtraction and both
addresses must be identical, in which case the value of the constant expression
is the difference between the offsets.

If only the right operand contains an address, the operator may not be
subtraction.

### Mul/div/mod, shift, and bitwise logic
If the operands to any of these expressions are a static/extern address that has
been cast to an arithmetic type, the result is not a constant expression.
Operands may only be arithmetic constants.

### Logical logic
Static/extern addresses without an offset are always taken to be true. The
operands can be any combination of addresses, offsets, and integral values.

If either operand is an address with an offset, the result can't be known, but
the expression is still a valid initializer constant expression.

## Constant expression flags
Currently, there are two flags used to identify constant expressions: `CONST`
and `ARITH`. These aren't very good, since `ARITH` has no use on its own, and is
only used to identify that an expression is an arithmetic constant expression,
given that it is already a constant expression (ie, that `CONST` is true).

I could make better and clearer use by combining the two flags into a two-bit
value, indicating levels or types of constness:
- "00": not any form of constant
- "01": weakest form of constant expression, used in initializers
- "10": required by array bounds, enum constants, and `case`
- "11": required for preprocessor `#if` statements. Will be used when/if I
  implement my own preprocessor.

## Constant expression evaluation
The rules defining constant expressions are complicated; there are many
restrictions, which are used in various combinations depending on the type of
constant expression the grammar is referring to.

The most lax category allows two "forms" of constant expressions: those that
can be reduced/evaluated to a single constant, or those that can be reduced to
a memory address (represented in assembly as a label) +/- a constant value. This
type of constant expression is used in brace-enclosed initializers.

The next category allows only expressions that can be evaluated to a single
constant at compile time. The expression may not modify any objects and, I
think, may not have any side effects in general. Pointers and arrays and
operations upon them may not be used. This category has a subcategory that
requires that the expression be of integral type. The standard is wordy here;
but it seems like as long as the final result is of integral type, the same
rules apply. The operand of `sizeof` also technically falls into this category.
For my own sake, I will just allow any expression whose resulting size is known
at compile time as an argument. The argument isn't even evaluated, anyways.

The final category is used in the preprocessor; it is the strictest, allowing
only integer, character, and floating constants. No casts are allowed, nor
is `sizeof`, nor are enumerators; only arithmetic, bitwise, comparison and
conditional operations are allowed.

The standard says nothing about whether or not these expressions must be
evaluated at compile time. Evaluating expressions involving the addresses of
static and extern objects and functions is difficult. The conversion is simple,
but I am unsure of how and when to prevent over/underflow and when to indicate
that the offset is meant to be subtracted from the address, rather than added.

However, evaluating expressions involving only arithmetic/character/enumeration
constants is not as difficult, and is necessary in order to determine the width
of and typecheck arrays.

### Implementation
Support for static and extern objects must be added in order to implement
constant expressions involving addresses/labels. `ASTree` must have new fields
added, which will be a `const char *` and a `unsigned long`.

The `const char *` is optional and points to the label of a static or extern
object. The `unsigned long` holds integral values.

Based on the output of clang and gcc, I have decided that offsets from static
and extern objects will always be treated as though they are signed 64-bit
values. When the integral component is not an offset, the sign of the stored
value will be determined from the type member of the syntax tree node.

## Simpler init list handling
Initializer list handling could be simplified as well, again using global state.
In this case, the state would need to be more complicated, since the way nested
initializers work is a little complicated. There will need to be a stack used to
track nested initializer lists for aggregate types with aggregate members.
Entries on this stack will need a member holding the type of the aggregate, as
well as the a counter for the number of initializers for the members of the
aggregate type that have been seen so far.

When a left brace marking the beginning of a new initializer list is seen, a new
entry is created on the stack. If there is already an entry on the stack, the
type for the new entry is computed from the type of the entry on the stack. This
type is the type of the member at the current member index if the type on the
stack is a struct or union, or the underlying type of the array if it is an
array.

When a member of an initializer list for an object of aggregate type is seen,
the type of the member being initialized is checked. If the member also has
aggregate type, a new entry is created on the stack using this type. The
initializer is treated as an initializer for the first member of this type.
This process happens recursively until the type of the member is not an
aggregate.

If the member counter for the current entry equals the number of members of the
aggregate type when an initializers is seen, the entry is popped from the stack
and the initializer is treated as if it were for the next entry. If the stack
is empty when this occurs, too many initializers were provided for the
aggregate, and a semantic error will be generated.

As an exception, if the aggregate type is an array with deduced length, the
deduced length of the array is adjusted to accomodate the initializer.

When a right brace marking the end of an initializer list is seen, the topmost
entry on the stack is popped. The parser would not accept a string of tokens
such that there would be more right braces than left braces, so if the stack
happens to be empty when a right brace is seen, it is indicative of a bug and
the compiler will abort.

### Implementation
The aggregate stack needs to be ready to go before the parser productions for
declarations with initializer lists are executed. So, the production rules for
structures and arrays need to push entries to the stack. These are the last
production rules fired before the production rules for initializer lists are
used, and therefore the last time the type information for the declarator that
the initializer list is for will be seen.

### Version with less globals
A less intrusive way to solve this problem would be to provide a function that
does the same thing, but with fewer globals. The reason I didn't pick this as
my first choice was because it requires traversing the tree again.

This solution will make use of two functions: `validate_initializer` and
`validate_init_list`. The first will be the base case, called when the
initializer is no longer an init list.

Both of these functions will have two arguments: the `TypeSpec*` for the
destination type, and a `ASTree*` for the initializer.

Here, since unions, pointers, and objects of arithmetic type may be initialized
with a single expression of appropriate type enclosed in braces, we will treat
these destinations as though they were aggregate types with a single member, and
refer to them as aggregate types when their initializer is enclosed in braces.

To begin, the type of the first member of the aggregate type (or just the type
itself, for other types) is compared to the type of the first initializer in the
list:
- If the destination is not of aggregate type and the initializer is not of
  aggregate type, `validate_initializer` is called. If the return value
  indicates an error, this function terminates and returns this error code.
- If the destination is not of aggregate type and the initializer is an
  initializer list, `validate_init_list` is called, the arguments being the
  current member type and its corresponding initializer list. If the return
  value indicates an error, the function terminates and returns this error code.
- If the destination is of aggregate type and the initializer is not of
  aggregate type, `validate_init_list` is called, the arguments being the
  current member type and the initializer list provided to this call.
- If the destination is of aggregate type and the initializer is an init list,
  `validate_init_list` is called, the arguments being the current member type
  and its corresponding init list.

When `validate_init_list` has consumed all of the initializers in the init list
that the given type needs, it returns the number of initializers consumed to the
caller. If the caller was `validate_init_list`, the caller adds this number to
its own count of the number of processed initializers, and continues. If the
caller was anyone else, this indicates that the init list contained too many
initializers and is semantically invalid, and should return an error.

If `validate_init_list` runs out of initializers, that is fine, and the number
of initializers in the init list is returned. The intermediate language
generator will make sure that the remaining members are zero initialized.

### Stack based solution
`validate_init_list` need to return two values: the number of initializers
consumed and an optional error code. These two values will not fit within a
single 64-bit register, so returning these two in a struct by value is not the
greatest idea.

This could be fixed by using stacks instead of recursion to track state. The
stack(s) would need to track the following:
- initializer nodes
- type that the initializer node is for
- index in the initializer/type that is being operated on
- offset into the initializer list

This version of `validate_init_list` would proceed as follows:

The first entry on the stack is created using the arguments passed to the
function and an index and offset of zero.

During each iteration, the destination type is compared to the initializer:
- If the destination is not of aggregate type, `validate_initializer` is called
  with the current destination type and initializer as arguments.
  `validate_initializer` will accept init lists as an argument, so long as the
  init list only has one child. The index for the entry on top of the stack is
  incremented.
- If the destination is of aggregate type and the initializer is not an init
  list, the destination type is pushed onto the stack with the initializer
  already present on top of the stack. The offset for this entry is equal to
  the index of the previous entry, and its index is zero.
- If the destination is of aggregate type and the initializer is an init list,
  the member at the index in the entry and its corresponding initializer are
  pushed to the stack with a count and offset of zero.

Then, the index of the current entry is compared to the destination type and
initializer:
- If the index equals the number of members of the destination type, the
  destination cannot consume any more initializers. If the entry below shares
  the same initializer list, their indices are added and 

When `validate_init_list` has consumed all of the initializers in the init list
that the given type needs, it returns the number of initializers consumed to the
caller. If the caller was `validate_init_list`, the caller adds this number to
its own count of the number of processed initializers, and continues. If the
caller was anyone else, this indicates that the init list contained too many
initializers and is semantically invalid, and should return an error.

If `validate_init_list` runs out of initializers, that is fine, and the number
of initializers in the init list is returned. The intermediate language
generator will make sure that the remaining members are zero initialized.

### Hybrid solution
Based on what I've brainstormed, the neatest solution would make use of a stack
and recursion. Recursion is used for nested initializer lists, while the stack
is for flat initializer lists for aggregate types with aggregate members.

Entries on this stack include a type and a count indicating which member of the
type is to be compared. There is an additional counter, not on the stack, which
tracks which initializer is being compared. The function starts by pushing the
type provided as an argument with an index of zero, then setting the initializer
counter to zero as well.

During each iteration, the member of the type at the top of the stack at the
index at the top of the stack is compared to the child of the init list at
the counter:
- If the destination is not of aggregate type, `validate_initializer` is called
  with the current destination type and initializer as arguments.
  `validate_initializer` will accept init lists as an argument, so long as the
  init list only has one child. The index for the entry on top of the stack is
  incremented.
- If the destination is of aggregate type and the initializer is not an init
  list, the destination type is pushed onto the stack with an index of zero.
- If the destination is of aggregate type and the initializer is an init list,
  `validate_initializer` is called, with the destination type and init list
  as arguments.

The index at the top of the stack is compared to the number of members of the
aggregate at the top of the stack. If the index at the top of the stack equals
the number of members of the aggregate at the top of the stack, the entry is
popped from the stack. If the stack is empty and the initializer counter does
not equal the number of children of the init list, too many initializers were
provided, and an error code is returned.

The initializer counter is compared to the number of children of the init list.
If the initializer counter equals the number of children of the init list, the
stack is cleaned up and the function returns, indicating success. The
intermediate language generator will ensure that the other members are zero-
initialized.

## Linkage, `static`, and `extern`
In order to implement constant expressions, the compiler must first be aware of
the linkage of symbols, so that it may determine which symbols may have their
address taken in a constant expression.

The default linkage of symbols, and their linkage when declared with the
`static` or `extern` keyword, is determined by their scope.

Also important is that `static` and `extern` can change linkage and storage
class, which are two different things. Linkage can be internal, external, or
none, and storage class can be static, automatic, or external.

For external declarations:
- external linkage, static storage class by default
- `static` gives the symbol internal linkage
- `extern` gives the symbol external storage class; this has no effect on
  functions but causes no storage to be allocated for objects
- `auto` and `register` are illegal

If an external symbol is to have internal linkage, the `static` specifier must
be given in the first declaration, and all subsequent declarations must also
have the `static` keyword.

For all other declarations:
- no linkage, automatic storage class by default for objects and external
  linkage, external storage class for functions
- `auto` and `register` do nothing to objects, but are illegal for functions
- `static` gives the symbol static storage class. objects are zero initialized
  if no initializer is given. illegal for use on functions.
- `extern` does one of two things:
  - if the symbol is defined in an outer scope, this declaration basically does
    nothing: the linkage matches that of the outer symbol, and this symbol uses
    the same storage as the outer symbol
  - if the symbol is not defined in an outer scope, give the symbol external
    linkage and external storage class

## External declarations
External declarations behave differently from internal declarations. External
symbols may be declared multiple times so long as they are defined at most once,
and the types of the declarations are considered equivalent.

## Single pass to intermediate language
With all the changes made to the type checker and parser, it is now possible to
generate the intermediate code in a single pass. Since the behavior of the code
would become more complicated if the parser still called the type checker
directly, which would then call the intermediate language generator, it will be
better to write functions that the parser can call, which in turn call the
functions for type checking and assembly generation based on the status of the
called functions.

Instead, there should be functions that dictating what processing happens to
the node. These wrap the type checking and assembly generation functions.

## Type checker refactor
This is supposed to be a (relatively) small refactor of the type checker to make
unit testing it easier, since it's over 2000 lines long.

The type checker will consist of 4 translation units:
- `tchk_stmt`
- `tchk_expr`
- `tchk_decl`
- `tchk_common`

Each will have their own header file. `tchk_stmt` and `tchk_decl` will both be
dependent on `tchk_expr`. All will be dependent on `tchk_common`. The contents
of each unit should be self-explanatory.

## New jump resource ownership idea
Jump statements could have all relevant information they need assigned to them
during type checking by having the lexer execute special actions based on the
tokens being matched.

The tokens `for`, `do`, and `switch` all mean that a loop/selection statement
is coming up, and that the meanings of jumps within those statements is about
to change. We manipulate the compiler state, pushing jump information to the
stack before the parser actions.

We can do almost the same for `while` tokens. However, because `while` is also
a part of do-while statements, do-while statements will need to do some extra
state manipulation to fix what would be a mistake made be the lexer action.

### State additions
The current jump stack would need to be split up and kept separate from the
scope stack, since the state of the two would not match completely.

There would be multiple stacks: one for `continue`, one for `case` and `default`
and one for `break`.

`continue` and `break` statements pull from their own stack of `size_t`
identifying the construct that these statements are associated with. `case` and
`default` statements pull from a stack of structures with two `size_t` members.
One is the id of the `switch` statement, the other is a count of the number of
`case` statements for the `switch` statement.

### Lexer/Parser behavior
When the lexer matches a `switch` token, new entries are created on the
`case`/`default` and `break` stacks.

When the lexer matches a `for`, `while` or `do` token, new entries are created
on the `continue` and `break` stacks.

The parser productions for each of the rules including these token pop the entry
they created from the same stacks they pushed to, with the exception that the
production for do-while statements pops one additional entry from the `break`
and `continue` stacks. This is to account for the bogus entry the lexer creates
when it matches tho `while` token used in the do-while statement.

These actions are independent of the scoping rules, so there would not need to
be special cases for iteration/selection statements whose body is not a compound
statement.

### Selection/iteration statement information
Selection and iteration statements will need to have the information used by
jump statements encoded in their own nodes, so that they emit the same labels
that were chosen during type checking. This information is stored in an `ASTree`
member, and is a `size_t`.

### Entry information and emitted code
`break`, `continue`, and `default` statements only need the id of the statement
they are associated with. The other parts of their emitted code remain the same:
`break` statements have the form `jmp .En`, `continue` statements have the
form `jmp .Cn`, and `default` statements are labels of the form `.Dn`, `n` being
the id of the statement.

`case` statements also need the id of the statement they are associated with.
In addition, they need their own id. The id of the following case will be the
next id in ascending order, so that does not need to be stored. `case`
statements emit the following code:
1. an unconditional jump to a label marking the "body" of this `case`
2. a label marking the condition for this `case`
3. the condition code for this `case`
4. a label marking the "body" of this `case`
5. the code for the "body" of this `case`

The "body" referred to above refers to the statement immediately following the
`case` statement and all subsequent statements until the next `case`.

The purpose of the second label and the preceeding jump is to allow fallthrough
to occur. That way, any jump to a destination before this case statement skips
the associated condition check.

So that the behavior is consistent regardless of where the default statement
occurs in the switch, the following "epilogue" is emitted at the end of every
switch statement:
- the default label (if it was not defined previously)
- a jump to the end of the switch statement
- a label, which looks like the condition label for another case statement
- a jump to the default label
- a label marking the end of the switch statement

### Output format
```
// break
jmp .E0

// continue
jmp .C0

// case
jmp .S0B0
.S0C0
/* ... evaluate condition */
jne .S0C1
.S0B0
/* ... case body */

// default
.S0D

// switch epilogue
.S0D
jmp .E0
.S0C2
jmp .S0D
.E0
```

## The lexer hack
I did not realize until now that identifiers are context sensitive. I had
assumed that it was some cognitive deficiency of my own that the parser that I
have written gets confused when dealing with typedefs, but it actually is a
problem.

In order to fix this, I must have a backchannel into the lexer from the type
checker. This means that the type checker must run in parallel with the lexer
and parser, and can no longer be run after the syntax tree has been built. This
means another major rewrite is in order.

Having the type checker run in parallel will open the door for a couple of other
nice features:
- labels could (potentially) be checked for validity using a stack which is
  examined at the end of the parser directive for functions; the same goes for
  case and default labels
- additional tree nodes can be inserted more freely during type checking, namely
  for implicit conversions

### Implementation
#### Lexer and Parser
To implement the hack, a new token, `TOK_TYPEDEF_NAME`, will be added to the parser,
which represents identifiers that refer to an existing declaration that has the
`typedef` storage class specifier.

When the lexer matches an identifier, it will not immediately produce a syntax
tree node. Instead, it will call another function. This function will look up the
identifier in the symbol table, and additionally get the semantic value produced
by the last matched parser rule.

The identifier will produce a `TOK_TYPEDEF_NAME` if and only if the following are true:
1. There must be an existing symbol for the identifier
2. This symbol must have the `typedef` bit set in its `flags` member
3. The semantic value is not a specifier list, UNLESS:
4. The specifier list DOES NOT have a valid type specified; ie, its list of
   children may only contain nodes with the following symbols: `const`,
   `volatile`, `typedef`, `auto`, `static`, `extern`, `register`, `restrict`

If these conditions are met, then the identifier is certainly meant to be interpreted
as a typedef name; otherwise, the code would result in a syntax error. If they are not
met, we instead interpret the input as a normal declaration or as a redefinition of
a typedef name.

This method does not produce the correct result in all situations; since tags, labels,
and struct members have separate namespaces, they do not conflict with typedef names
and will need to have their symbol changed to `TOK_IDENT` in the parser rules where
they appear.

#### Consequences
No negative impact compared to previous approach, and much less code. It turns out
that `yyval` is a local variable inside one of the generated parser's functions,
so it cannot be accessed anyways. Instead, each parser rule assigns to
`bcc_yyval`, which is defined in `lyutils.h`. This solution can probably be cut
down to something less tedious and invasive at a later date.

## More useful jump tracking
Because of the type checking occurs from the bottom up, a return statement
cannot know the return type of its enclosing function because the production for
the function will not have been matched until after the return statement has
been processed. So, program state will also record return statements and they
will be processed during `define_function`.

We should also change the purpose of the jump tracker regarding iteration and
switch statements. The jump tracker should be used to verify that break, case,
default, and continue statements are in a valid context during type checking,
instead of during intermediate language generation.

Because of these changes, it may make more sense to have the symbol table hold
these jump records. Then, whenever an iteration statement, switch statement, or
function definition is produced, the function responsible can go through the
stack and process the entries that it is responsible for. This would only need
to be done if the statement matched during the production rule is a block.

Since the statement may not be at the topmost level, there may be enclosing
statements that handle leftover entries (for example, a switch statement
enclosed in a for loop should leave `continue` entries on the stack). These
leftover statements would have to be merged with the stack in the symbol table
above.

When the function body is processed, it checks and removes all return
statements. Then, any leftover entries would be errors, since they must be
enclosed within an iteration or switch statement to be valid.

### Implementation
All control statement tracking functions will be associated with the symbol
table. When popping symbol tables from the stack, the state is responsible for
merging the contents of the popped table and the table that is now present at
the top of the stack.

We also need a way to handle the special case where a switch/iteration statement
does not have a block as its body, but rather only has a single statement which
is a jump statement.

#### `symbol_table_add_control`
#### `symbol_table_remove_control`
#### `symbol_table_get_control`
#### `symbol_table_count_control`

## Concurrent type check
For concurrent type checking, my current idea is to have the parser directives
call type checking functions, or to have initialization functions for each type
of expression.

### Smarter errors
Something that I think might be feasible is communicating type checking errors
by inserting new nodes into the syntax tree whose entire purpose is to propogate
error information. I would need to add a new token code for type checker errors,
and would need to use one of the members (probably one of the type members) to
communicate detailed error information.

These errors should also be tracked in a list in the global state.

Handling errors like this would allow the type checker to detect when an error
has occurred, but keep going in spite of it, so long as the error is not
catastrophic. Then, the error can be reported once type checking has been
completed, allowing for the type checker to produce output on valid code.

However, when nothing can be done with an error and a function cannot continue
because the node contains important information, the error node should "float"
up the syntax tree. This will need its own function. Doing this will be a little
less hacky than the way promotions and automatic conversions are inserted, since
error nodes should have exactly one child.

#### Implementation

##### `propogate_err`
Takes as arguments a parent and child node, at least one of which must be an error
node. Aggregates errors into a single node and places this node at the top of the
subtree, followed by the parent, then the child.

##### `propogate_err_v`
Takes as arguments a parent node, a count of the number of children it is to adopt,
and a varargs list of child nodes. Aggregates errors into a single node at the top
of the subtree through calls to `propogate_err`.

##### `propogate_err_a`
Takes as arguments a parent node, a count of the number of children it is to adopt,
and an `ASTree**` that points to an array of child nodes. Aggregates errors into a
single node at the top of the subtree through calls to `propogate_err`. For later
use in error-handling macros, since proper ANSI C does not support variadic macros.

### Printing errors
I have removed calls to printf from the type checker so that errors are not
reported until type checking has been completed, and to remove duplicate code in
the printing of errors.

Error printing functions will be located in `bcc_err.c`. They will operate
primarily on `TypeSpec`s and `AuxSpec`s, since that is what we are using to store
error information.

#### Functions
There will be one primary error handling function, most of whose body will be a
massive switch statement that selects which format to use, or which function to
call to print the error.

The functions for creating and propogating type errors will also be moved into
this function and modified to accomodate the extra information.

#### Error contents
The `AuxSpec`s for errors will have an `void **` member, an array of all of the
relevant error information in the form of pointers to `TypeSpec`s, `SymbolValue`s,
and `ASTree`s. They will also have a member indicating the length of this array.
The array will be dynamically allocated and must be freed when the `AuxSpec` is
destroyed.

### Type checker constructed nodes
The lexer (obviously) creates syntax tree nodes, but the parser does so as well
in a handful of cases, like for type specifiers, declarations, declarators, etc.
With the concurrent type checker it may be intuitive to have the type checker
itself introduce nodes to the syntax tree. The first would be the the error
nodes mentioned above, but also nodes for automatic conversions and promotions.
The type checker already does this, but it could be done in a cleaner and more
regular manner.

### Parser constructed nodes
Instead of the `parser_make_XX` functions currently in the parser, I will
instead call `astree_init` with whatever arguments necessary, and pass the
result as an argument to the appropriate type checking functions.

### Minimal mutability in the syntax tree
The main issue I'm having is the following: I would like to not have to use
`astree_extract` or `astree_inject` to insert automatic conversions into the
syntax tree.

`astree_extract` was originally used to replace nodes in the tree
with conversion nodes, but I deemed it ugly because it needed the index of the
node to replace, and its parent, as an argument. Also, updating the nodes
surrounding the node being extracted was clunky: the `next_sibling` member of
the prior sibling needs to be updated, and all of the node's siblings need to
be updated if it is the `firstborn` node. (I could, I believe, do without these
two members if I reworked the parser rules. This would reduce the complexity of
the "tree" but would also be time consuming and probably not as good as just
finding another way to shuffle tree nodes around.)

`astree_inject` is, internally, even less elegant: while it only needs the only
node and the node to replace it with as arguments, it just swaps the values of
all of the members, which is much goofier than twiddling around with some
pointers. (Well, it would be if the child nodes weren't stored as a linked list,
which is dumb and means that removing the old node and inserting the new one are
both O(n) operations.)

There are a couple ways to solve this:
1. Have the type checking functions for expressions take the expression and its
   sub-expressions as separate arguments, perform promotions and conversions, and
   only then have the expression adopt its sub-expressions.
2. Add an additional member to `ASTree` that holds the promoted type. This could
   also be used for casts, making the syntax tree for casts look simpler.
3. Encode the cast as part of the type information itself: the base type or one
   of the auxiliary types would contain a pointer to the original type info,
   while the rest of the type would represent the promoted/casted type.

I think I will be going with number 2. I already have the `extract_type`
function, which could be modified to return the promoted type if it is non-null.
There are still a handful of places where `extract_type` is not used to retrieve
the type of a node, so those may have to be fixed if this change breaks the
behavior of those locations in the code.

### Passing state
Since the the type checker will be run in parallel with the parser, it will also
need to take a different approach: bottom-up instead of top-down. With this
approach, state cannot be communicated using a single instance as a parameter to
each function call. Instead, it must be moved back into a global variable, or
composed from the arguments that build the tree.

The state for the type checker will be moved back into a global. It will,
however, be a single global structure, to make keeping track of it and replacing
it at a later date easier.

## Location of error nodes
If I encode error information as syntax tree nodes, the location of these nodes
in the tree is important when considering the behavior of the program. For
example, if a declarator contains errors, should the declarator be adopted by
the error node, or should the declaration which it is a part of be adopted by
the error node? The other declarations may be fine, after all.

## Labels
Based on the language definition, it looks like labels are attached to the
statement immediately following them. They are not a valid statement all on
their own. Just something to note for later.

## Namespaces
Labels have their own namespace. Structures and unions share a namespace
separate from other types of declarations, and functions and objects share a
namespace.

## Struct padding
The auxiliary info for struct should include a way to record the amount of
extra padding that each field needs. The padding value for a member specifies
the number of extra bits to be inserted before their storage location in the
struct. When defining a structure, the memory offset given the current size
of the structure is compared with the alignment requirements of the member
being added. If this value does not meet alignment requirements, padding bytes
are added until the current structure size matches, at which point the member
is registered with the current memory address, and, if necessary, the alignment
requirements of the structure are raised to match that of the member. The first
member of the structure will always have zero padding bytes.

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

## Name spaces
These namespaces are different from the namespaces described in C++. They
provide separation for names, but they are not something that can be manipulated
using the syntax by the programmer. Instead, they are an internal feature of
the language.

The name spaces present in ANSI C are:
1. Objects, functions, typedef names, and enumeration constants.
2. Structs, unions, and enum tags.
3. Labels.
4. Individual members of each struct or union.

Labels may only appear within a function body and all labels in a function body
share a single, flat scope, with no nesting of scopes.

Struct and union members are only valid inside and only conflict with members of
the same struct/union tag. Nesting is possible when declaring the members of a
struct or union, since struct, union, and enum tags may be declared within a
struct or union tag definition.

## Typedefs
Typedefs can be redeclared at inner scopes, but objects already declared do not
change type when this happens, so the full type must be resolved when the object
is declared and copied into the object's type. This will make comparing types
easier, since the type checker can just go about its buisiness as usual when
comparing objects declared using a typedef.

Typedefs declared at inner scopes that do not have any type information are not
an actual redeclaration, and can be ignored if the type name being declared has
already been declared. The same goes for typedefs at the same level with no
information.

Typedefs at the same level without any type information and without any prior
declaration are treated as incomplete types, and are valid so long as they are
not used in any context where the size or other information about the type are
required; ie they may only be used to declare pointers to the type, but not
plain objects of the type or arrays of the type, struct and union member
access may not be performed on them, and the memory where objects of this type
are stored may not be dereferenced.

Objects and functions whose type includes a typedef'ed name can resolve this
name at declaration time by either copying the whole type or by storing an
`AuxSpec *` containing the name of the typedef and the symbol value associated
with it.

The former approach makes type checking objects and functions using typedefs
easier, since the typedef becomes invisible to the type checker once delared.
However, it makes incomplete typedefs harder to deal with, since the base type
information would need to be replaced with a placeholder indicating that the
type has yet to be fully defined. This placeholder would also have to be
replaced if, later on, at the same scope, the typedef received a full
definition.

The latter approach means the type checker has to do more work when handling
typedef'ed types, and opens the possibility for more bugs in the implementation.

## Incomplete types
There are three ways to create an incomplete type: tags, arrays, and void. Tags
are incomplete if their members or enumerators have not been specified, arrays
are incomplete if their dimensions haven't been specified, and the void type is
always incomplete.

Contrary to what I previously believed, handling of incomplete types can be
handled using existing fields of the type system:
- tags already have a flag field that indicates whether or not they have been
  completed
- arrays of dimension zero may not be explicitly defined, so we can take that to
  mean that the array has complete type
- void is always incomplete, so we have no need to explicitly mark it

Typedefs cannot, on their own, be incomplete; their declaration specifiers must
include one of the incomplete types listed above.

## Struct and label information
Struct/union definitions and labels do not need to have the same information as
symbols, so they should have their own structures used to track them.

## Handling function parameter types and identifiers
Another problem I noticed with the way I've been handling function parameters is
that function prototype parameters don't need to be named, or to have the same
names as those used in the function definition.

## A better symbol table
Currently, symbols are stored directly in a map data structure from badlib. This
has been convenient, but it has some limitations that make using it awkward:
- Symbol tables are not structured relative to one another in any way.
  Relationships can only be established between them using the syntax tree,
  which has been inconvenient.
- Two stack data structures are used to track the number and order of nested
  scopes and object declarations, which could be combined with the symbol map
  into another data structure.

The ideal symbol table would have the following:
- A `Map`, used for the primary namespace.
- A `Map *`, used for the struct/union/enum tag namespace.
- A `Map *`, used for the label namespace.

The latter two maps are pointers because most table scopes would not need
either, since labels only occur at function scope and most tags are declared
at file scope, in my experience.

The tag and label namespaces will not use normal `SymbolValue` structs to store
their information, and will have dedicated ones instead.

### Definition
```
typedef struct symbol_table {
  Map primary_namespace;
  Map *tag_namespace;
  Map *label_namespace;
  struct symbol_table *parent;
  LinkedList *children;
} SymbolTable;

typedef struct symbol_value {
  size_t sequence;  /* used to order declarations in a given block */
  Location loc;     /* declaration location */
  TypeSpec type;    /* type of symbol */
  int is_defined;   /* whether this function/struct/union has been
                       specified/defined */
  char obj_loc[64]; /* location, represented as a string */
} SymbolValue;

typedef enum tag_type {
  TAG_STRUCT,
  TAG_UNION,
  TAG_ENUM
} TagType;

typedef struct tag_value {
  size_t width;
  size_t alignment;
  union {
    Map enumerators;       /* mapping from names to integer constants */
    struct {
      SymbolTable by_name; /* struct members by name */
      LinkedList in_order; /* struct members in declaration order */
    } members;
  } data;
  TagType tag;
} TagValue;

typedef struct label_value {
  Location *loc;
  int is_defined;
} LabelValue;
```

## Redoing the symbol table (again)
The symbol table is still clunky. I think I will solve this by reducing the
symbol table data structure down to the maps for the namespaces. Nested scopes
will be tracked using a stack inside a separate data structure.

## Recording type checker/assembly generator state
The extra information besides symbols in a given scope will be tracked using a
separate data structure which records all of the state that the type checker and
assembly generator need.

This structure will have two stacks: one for symbol tables for nested scopes,
and another for iteration and switch statements. The latter is so that the
assembly generator knows where `break` and `continue` statements should jump
to. It will also hold the type information for the enclosing function, if
applicable.

This is still a little clunky, since symbol table information is needed in so
few contexts in the assembly generator, but I cannot think of a better way to
track where symbols live on the stack.

## Switch statements and case/default labels:
Validity checks for case and default labels will be deferred until assembly
generation, just like continue and break statements. More data will have to be
added to jump stack entries, though. It will be implemented as a union of two
kinds of entries: an iteration entry and a switch entry, differentiated by an
enum field. Instead of just checking for the presence of any entries in the
stack, there will instead be two resolution functions: one for continues, which
searches for iteration entries, one for case and default statements, which
searches for switch entries, and one for break statements, which returns the
entry on the top of the stack.

Jump entries for switch statements will have a dynamically resizable array, so
that they can store an arbitrary number of case labels. They will also have a
member for the default label, and the label past the end of the switch
statement.

```
typedef struct jump_entry {
  union {
    struct {
      char cond_label[MAX_LABEL_LENGTH];
      char stmt_label[MAX_LABEL_LENGTH];
      char end_label[MAX_LABEL_LENGTH];
    } iteration;
    struct {
      LinkedList *case_labels;
      char default_label[MAX_LABEL_LENGTH];
      char end_label[MAX_LABEL_LENGTH];
    } switch;
  } data;
  JumpType type;
} JumpEntry;
```

## Struct and union member tables
The current method for storing struct and union members is no longer adequate.
While it would be convenient to reuse the scoping functions, scopes are
currently tracked based on their relationship to their parent scope. Tag members
need to be resolved in scopes that are not related to the current scope by the
parent-child relationship, because they have been defined elsewhere.

The resolution of tag members also does not need to be as robust as the
resolution of normal variables. While tags do need full symbol tables to handle
more complicated tags (like tags which themselves contain definitions of other
tags), tag members do not need to be resolved at depths beyond one. The right
hand side of the reference operators should just be an identifier that is the
name of a member. The left hand side should have its type resolved already.

The problem is actually that when we resolve the type of the left hand argument,
we don't remember the members of the structure. If the type of the left operand
of the reference operator is a tag defined only within another structure,
resolving the member's info may sometimes require us to dig through the syntax
tree again to recover that information. So, either tag type information needs
to be stored along with the name of the tag, or we need to push tag info to the
stack. Tag info cannot be stored on the stack because of the way scoping is
tracked, so it must be stored along with the tag name.

We can deprecate the functions for entering and leaving scopes in `symtable.c`
and instead make that functionality local to `asmgen.c` and `typecheck.c`. We
would also remove the `current_table` global variable, and instead put that in
the files mentioned previously.

All of the symbol table functions would need to take an additional argument,
which would be the symbol table to operate on, instead of relying on a global
variable.

Tag tables that are not declared within other tag tables (those declared at
global or block scope) would then need to have a NULL parent reference. Tags
declared within other tags would not have a NULL parent reference.

## Tracking table state
Instead of tracking table state in a global variable, it should be passed as an
argument to the all of the `SymbolTable` functions, as well as the type checker
and assembly/intermediate language generator functions. This is (I think) a more
robust way to implement variable scoping.

## Nested structure and union declarations
Structs (and unions) declared within other structs and unions pose a unique
problem: the tag information for the nested struct is stored in the symbol
table of its parent struct. This table is (currently) only accessible while
resolving the right hand side of the arrow and dot operators. 

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

## Conditionals and loops
`if` statements need only one label to function: a label following the body of
the if statement, so that it may be skipped.

`if-else` statements with an unconditional else clause also only require a
single label. However, if the else statement is conditional, an additional
label is required at the end of each else body.

`while` and `for` loops require three labels: one at the start of the condition
expression, one at the beginning of the loop body, and one at the end of the
loop body.

This is tweaked slightly for do-while statements, where the end of the loop
body and the start of the conditional are the same.

## The arrow and square bracket operator
Accessing members of pointers to structs with the arrow operator is equivalent
to dereferencing them and then accessing them with the dot operator, so maybe
the parser should just add the necessary tree nodes to make those two operations
look equivalent in the abstract syntax tree. A similar thing could be done with
the square bracket (array indexing) operator.

## Constant value storage
Enumeration and arithmetic constants (as in TOK_INTCON and TOK_CHARCON) should
have a way to store the actual value of their constant, so that it only needs
to be converted from a string once. For arithmetic constants, this is
straightforward, since it can just be stored on the syntax tree node. However,
ever occurrence of the enumeration constant needs the value to be propogated to
it, so it must be stored in the symbol table in some way.

The `Map` member of `TagValue` is supposed to be used for this purpose, but the
type of enumeration objects and enumeration constants is identical. The way they
are distinguished is using the `flag` member of `SymbolValue`. If `flag` is set,
then it is a constant, and the value of the constant should be fetched from the
`TagValue`.

This is a cryptic and terrible way to do this and it will be replaced at a later
date, but it is sufficient for now.

## Constant expression evaluation
NOTE: I have decided that the computation of constant expressions should not be
done until assembly/intermediate language generation. They will still be
validated using flags in the syntax tree, but the actual result will not be
calculated or stored on the tree. The mechanisms for the evaluation of constant
expressions will probably share some machinery with the mechanisms for
optimization.

Constant expressions are required:
- after `case`
- as array bounds and bit-field lengths
- enum values
- in initializers
- some preprocessor expressions (we don't need to worry about that one)
- as the argument of `sizeof` (technically)

`sizeof` doesn't really need a constant expression as its argument; it can work
with any expression whose result has a known size. The result of sizeof is a
constant expression, though. NOTE: `sizeof` doesn't actually evaluate its
argument; it just uses it to determine the type whose size it is calculating.

There are two denominations of constant expressions: one only allowing operands
of arithmetic, character, or enumeration types, and one allowing any constants.
The former is for enumerator values, case labels, array bounds, and bit-field
lengths. The latter is for initializers of static objects and arrays, as well
as automatic objects and arrays that are initialized with a brace-list.

Since there are two classes of constant expressions, the `ASTree` flag field
will need two separate bits marking constant expressions. This technically
allows us to specify a third level of constness, for later use.

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

## Handling function return values
Function return values always end up in the same virtual register, `vr0`. This
register must also be restored by the caller before it can continue, even
though its contents are needed when the function returns a value. So, after each
function call and before the volatile registers are restored, the return value
should automatically be moved to a new vreg.

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

## Short-circuiting of logical operators
The logical operators `&&` and `||` must short-circuit. If the left-hand side of `&&`
is false, the right hand side is never evaluated; likewise, if the left hand side of
`||` is true, the right hand side is never evaluated.

This means that the `AND` and `OR` instructions are not useful in implementing these
operations.

Instead, these operators must be implemented using `TEST` and `Jcc` instructions.
The code evaluating the left operand will be emitted first, followed by a `TEST`
instruction and a `JZ` or `JNZ` instruction for `&&` and `||` operators, respectively.
Then, the code evaluating the right operand will be emitted, followed by the label
which serves as the target for the mentioned jump above.

Additionally, all subexpressions of a boolean-valued expression will jump to the same
label at the end of the expression. There must be a way to communicate the register
of the final result and the name of this label.

There must also be a result register for the sequence of boolean operations, in case
the value is assigned.

Structurally, I would like the assembly to work like a sled into a fall-through condition
which can be skipped if we reach a point of no return, which would be any false value
for `&&` and any true value for `||`.

This scheme would require that we set a default value which is returned unless all
subexpressions are executed, in which case a different value is returned. For `&&`, we
would default to false, setting the value only to true if all expressions are executed.
For `||`, we would default to true, setting the value to false only if all expressions
are executed.

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

## Type checker error handling
Currently, type checker errors result in an message printed to standard error,
and a nonzero integer return value, which is usually -1, though the caller
just checks whether or not it is zero.

I would like to reduce the number of bare calls to printf floating about in my
code, so that it is more compact and readable. This would also later allow the
caller to handle errors, if it is capable of doing so, instead of just passing
it on.

Type checker errors will not be printed out unless they reach the top level, in
which case, for now, the compiler will exit, reporting the failure.

I would also like to add error handling without having to add another parameter
to every function to hold the output or error information. Instead, I think I
will add error info to the `CompilerState`, and have each function return a real
status code instead of just -1.

Error codes will be stored as an enum and will be shared across the compiler.

## Pointer conversions
According to the standard, expressions with array type decay to pointer type
except when their parent expression is "&", "++", "--", or "sizeof", or as the
left operand of the assignment and dot operators. In all cases except for the
"sizeof" operator, this is so that the compiler issues errors or warnings,
since expressions of array type are not lvalues and cannot be changed, and the
dot operator would otherwise give crytic errors about member access with the dot
operator.

The "sizeof" operator gives the number of elements in the array.

Similarly, expressions of function type become expressions of function pointer
type except when their parent expression is the address-of operator.

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

## Forward declarations
Forward declarations, as they are currently handled, are fine so far as I can
tell, with the exception that debug information about types may not make sense.

The location associated with a type is supposed to be the declaration location,
which should not be overwritten when the function is defined. However, the names
of variables should be overwritten, so we cannot just reuse the symbol value and
table defined by the function prototype, because the function prototype may have
different names for parameters or none at all.

## Ownership of type information
The current invariant for ownership of type information that I have been trying
to maintain is that the symbol table "owns" the type information and is
responsible for freeing it. This has proven difficult for representing functions
and their parameters.

Currently, it is unclear whether a function is responsible for the types of its
parameters, or the block scope associated with the function. This is because
function prototypes do not have a block associated with them, so the normal
location for the symbol table is unavailable.

The cleanest solution I can come up with is to have the declarator node of
function prototypes store the "function prototype scope" variables.

## Promotions, conversions, and casting
Promotion for arithmetic values is:
- Any character or short integer whose value can be represented by signed by
  `signed int` is converted to `signed int`. Otherwise, it is converted to
  `unsigned int`.

Conversions:
- If one of the operands of the expression is a floating point value, the result
  is a floating point value and the operands are converted to floating point
  values.
- Otherwise, the result and operands are converted to be the widest integral
  type present in the expression after integral promotions are performed. The
  widest type is preferred; if two types are of equal width, then the unsigned
  type is preferred.
- Expressions of array type are converted to pointer type, except when the
  expression is the operand of `&`, `++`, `--`, `sizeof`, or on the left side
  of the `.`

Casting/compatibility rules are as follows:
- arrays and pointers are compatible, so long as the underlying types are compatible
- pointers to functions and functions are compatible, so long as the underlying types are
  compatible and the pointer is on the left hand/destination side
- all explicit casts are allowed, without exception; be mindful of width

## Printing of symbol table
Unfortunately, because of the way symbols are stored, it would be difficult to
organize them in a way that is readable in the same way as the abstract syntax
tree.

For now, the thing that makes the most sense is to list the details of the
abstract syntax tree node, followed by a list of all the symbols stored in the
table associated with that node.

### Functions
#### `symbol_value_print`
Takes as arguments the `SymbolValue` to be printed, the buffer to be printed to,
and the length of the buffer. Prints the declaration location, followed by the
base type and auxiliary information. More information will be printed out as
those features are implemented.

#### `symbol_table_print_table`
Takes as an argument the `const map *` to be printed. This function will use
`map_foreach_pair` to iterate over every symbol in the current scope. Pairs will
be passed as arguments to `symbol_table_print_entry`.

#### `astree_print_symbols`
Takes as an argument the `ASTree *` node whose symbol table is to be printed,
the output file to be printed to, and the depth of the node in the abstract
syntax tree.

## Simplifying type specifier verification in general
The verification of type specifiers as a whole could be reduced to a single
function that compares and modifies bitmasks as opposed to the current system
that separates functionality into multiple functions depending on the base type.

There are a few "base" types:
- integer types (including characters)
- enums
- structs
- unions
- void

When declared, objects must specify exactly one of these types.

Simplyfing the parser and having the type checker catch these errors might be
helpful and/or easier.

It would also mean less `TypeSpec` constants would have to be defined, as the
only "default" ones would be `signed int` and `char`.

## Restructuring type checking
There are many functions that share the burden of type checking, and their
relationship is not very clear. These functions are:
- `compare_declspecs`
- `compare_auxspecs`
- `determine_promotion`
- `types_compatible`
- `init_list_compatible`, which I am already working on replacing

First, `determine_promotion` is called to figure out a possible common type for
its two arguments. Then, `types_compatible` checks that this type is actually
compatible. It performs some checks of its own, then calls `compare_auxspecs`,
an then `compare_declspecs`. `init_list_compatible` is only called when
initializing a struct, union, or array with an initializer list.

It would make more sense to call a single function that handles promotion and
type checking, instead of having to perform those two steps separately at the
call site. It would also make more sense for this function to also be capable
of checking the compatibility of initializer lists and string constants, rather
than having a separate function handle that.

Special cases:
1. Any pointer may be implicitly converted to/from `void *` (with type
   qualifiers and storage class specifiers) without warning or error.
2. Any pointer may be implicitly converted to/from any other pointer, with a
   warning.
3. Binary arithmetic operations may not be performed on two pointers, except for
   subtraction between compatible types.
4. Implicit conversion between integers and pointers is allowed, with a warning,
   and explicit conversion is allowed without a warning or error.
5. `void` is never a valid type, and the type checker should report an error if
   an expression of type `void` is evaluated by this function(s).

### Steps
```
function check(src, dest):
    for each pair of auxspecs:
        if dest is a pointer:
            if src a pointer or array:
                verify that the element type is the same
            else if src a multicharacter constant:
                
            else:
        else if dest is an array:
            if src an initializer list:
                verify that each element has the same type as the array
            else if src a multicharacter constant:
                verify that base type of dest is any (wide) char type
            else: 
        else if dest is a struct:
            if src an initializer list:
                verify that types match
            else if src a struct:
                verify that struct type is identical
            else:
        else if dest is a union:
            if src an initializer list:
                verify that it is a brace-enclosed initializer for the
                first member of the structure
            else if src a union:
                verify that union type is identical
            else:
        end
    end

    if src or dest has more auxspecs:
        if 
    else if src and dest have the same struct type:
    else if src and dest have the same union type:
    else if src is an initializer list:
    else if src is a string constant:
    else if src and dest are of integral type:
    else if 
end
```

## Storing type information of initializer lists
Currently, syntax tree nodes with the token `TOK_INIT_LIST` do not store any
type information. If they auxiliary type auxiliary type information, much like
structs and functions do about members and parameters respectively, checking
the types of initializer lists may be simpler and easier to understand.

A "fake" base type, `TYPE_INIT_LIST`, will be added, along with an auxilary type
`AUX_INIT_LIST`. Every `TypeSpec` with this base type will also have exactly one
piece of auxiliary type information with this auxilary type, the same way that
structures and unions are handled.

This type can be implicitly converted to struct or array types, given that the
component types are appropriate. It cannot be converted or casted to any other
type.

## Storing type information of multicharacter constants
Multicharacter constants cannot be treated the same as initializer lists. You
cannot assign a pointer a value that is an initializer list, but you can assign
a `char *`, or any pointer or integer value if you use casts, a value that is a
string constant.

Multicharacter constants will have their own distinct base type,
`TYPE_MULTICHAR_CONST`, and their own auxiliary type, `AUX_MULTICHAR_CONST`.

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

## Assembler generation
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

## Assembly generation procudures
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

## Assembly Expression Generation
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

## String literals
All string literals get stored in the `.data` section of the assembly file. When
assigning a string literal to a `char[]` or a `char*`, one of two things happen.
- When assigning to a `char[]`, the contents of the string literal are copied
  into the array.
- When assigning to a `char*`, the variable is made to point at the string
  literal's location in the `.data` section, which may or may not be read-only.

## Register Allocation
Before register allocation is done, all lines that will be written to the output
file will first be stored in a linearly traversible and maybe random access data
structure. Instructions will use virtual registers as operands.

The x64 use of the left operand as destination will be respected when these
lines are generated; the function that puts instruction data into the list will
return the name of the register the result was placed into.

The contents of source lines to be written out will be tracked in a struct with
fields for label, instruction, left/right operands, and comment.
