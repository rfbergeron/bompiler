# Expected Behavior
This file lists the expected behavior of the various procedures and data
structures used in this project.

## `astree.h`

#### Changes
For all of these functions, it may be best to use assertions to detect bad
input, rather than return codes. Many times, if an index is out of bounds, or
a `NULL` pointer is passed to any of these functions, it means that some other
part of the program is behaving incorrectly, and no recovery should be
attempted.

### `astree_init`

### `astree_destroy`
Frees the resources allocated for `ASTree` nodes.

If the provided tree node is `NULL`, no operations are performed, and an error
code indicating failure is returned.

If the provided tree is the empty node, no operations are performed, and an
error code indicating success is returned.

The linked list storing this node's children is freed.

If this node's `symbol_table` field is not `NULL`, then it is freed.

During type checking, some tree nodes copy and modify type information from
their child nodes, rather than using the child's pointer. This additional type
information is freed:
- nodes with the symbols `SUBSCRIPT`, `INDIRECTION`, `CALL`, and `TYPE_ERROR`
  have their type information freed.
- nodes with the symbol `ADDROF` free the resources allocated for the first
  auxiliary specifier of their type information freed; then, as above, their
  type information is freed

Finally, the memory for the node itself is freed, and an error code indicating
success is returned.

#### Changes
Assert that the node is not `NULL`, rather than returning an error code.

### `astree_adopt`
A parent syntax tree node adopts a number of child nodes.

#### Assumptions
If the parent node's symbol is `TYPE_ERROR`, it shall not have more than one
child.

Nodes are not adopted by more than one parent.

Nodes are not adopted by the same parent more than once.

#### Changes
Assert that nodes are not `NULL`, rather than return error code.

Assert that parent is not the empty node, since that node should never adopt
any children.

### `astree_replace`
A parent syntax tree node has one of its child nodes replaced, and the
replaced node is returned.

If the index is out of bounds, no operations are performed, and `NULL` is
returned.

Badlib already does its own bounds checks.

However, without performing bounds/`NULL` checks here, we have
undesired behavior: if the given index equals the number of children, the call
to `llist_extract` will fail, but the call to `llist_insert` will succeed.
Effectively, the node would adopt a new child instead of replacing one.

#### Changes
Assert that nodes are not `NULL`, instead of relying on badlib return codes.

### `astree_get`
Retrieves the parent node's `N`th child.

### `astree_remove`
Removes the parent node's `N`th child and returns it.

### `astree_count`
Returns the number of children a node has.

### `astree_to_string`
Prints the contents of the node to a buffer and returns the number of characters
written, as output by `snprintf`.

If any of the helper string functions fail, nothing is written to the buffer and
the helper function's return value is returned.

If the symbol is an artificial token defined by the parser, the `TOK_` prefix is
omitted from the output.

#### Changes
Assert that the node is not `NULL`.

### `astree_print_tree`

### `astree_print_symbols`
