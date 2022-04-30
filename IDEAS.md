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
- You can only implicitly convert between `void*` and other pointer types, not
  any other kind of void pointer (for example `void**`).

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
