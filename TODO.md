# Parser and syntax tree changes
I will be paring down the syntax tree data structure and rewriting some parser
rules to accomodate for this. There is some unnecessary complexity and
connectivity in the syntax tree that I would like to eliminate, namely:
- the `astree_twin`, `astree_descend`, and `astree_inject` functions
- the `firstborn` and `next_sibling` fields
- the `astree_first`, etc. functions
- the linked list adoption behavior of `astree_adopt`

# Type checker fixes
- handle new forms for the abstract declarators featured in casts and sizeof
- handle new flat structure for declarators and make sure that the type can be
  read easily be iterating over the list
- handle new tag structure, which groups members and enumerators under the brace
  instead of directly under the identifier
- in a call expression, the subtree that identifies the function to be called is
  now actually the LAST subtree, not the first
- handle '=' top level symbol for simultaneous declarations and definitions
- handle or at least implement stopgap for the lack of `astree_inject` and
  similar functions until the type checker runs in tandem with the lexer and
  parser
- see if the syntax tree has been simplified enough to remove the
  `extract_ident`, `extract_type`, and `extract_loc` functions from astree
- try to reuse more code from `declare_symbol` and `define_symbol` in
  `define_function`, or merge `define_function` into the other two completely.

# When to use pointers
Currently, some fields of syntax tree nodes are pointers, even though they could
instead be nested structures. This includes the child lists and symbol tables.
These do not have to be pointers, since only the one node should be responsible
for these things.

The typespec, on the other hand, should probably be a pointer, at least in the
case of syntax tree nodes. It could be a nested structure within symbol values.

In terms of optimization, making large structures like maps and linked lists
a direct field of the syntax tree may not be great for space, since many nodes
will not have children or a scope associated with them, so the space would be
wasted. If it was a pointer, significantly less space would be needed for a
feature that is not used.

On the other hand, the fewer pointers, the better.

# Symbol entry creation order
Before, type information was constructed in an ASTree node. This is no longer
possible, since nodes should be holding type information from a constant or
symbol entry.

So, either symbol entries must be created before type information is processed
so that it can be assigned to the correct location, or a typespec structure
must be allocated on the stack to serve as a temporary location for type info
before it is copied to its final location.

The former would be ideal and would lead to fewer errors, but would also require
large portions of the type checker to be rewritten. The rewrite will probably
make the code clearer, though.

# Badlib improvements
While giving each data structure an internal status field is convenient, it
means that the arguments to data structures cannot truly be `const`; the status
field may be modified in the event of a failure. I could just cast away const
but that's not ideal.

A potentially better idea may be having a status function to which the caller
provides the data structure to. This function queries a map based on the memory
address of the data structure and returns the error value associated with it,
which defaults to a success status. Since the interface is a function that takes
as an argument the structure in question, a simpler version that just returns
the value of a simple variable in the source file could be implemented without
changing the api later.

This scheme is more complicated because it would require the library to
implement thread-safe access to this map if it would ever be used in a multi-
threaded environment. However, pointers are shared by threads and it wouldn't
make sense to have thread-specific statuses, so if I made the map data structure
thread-safe, that alone would probably be sufficient.
