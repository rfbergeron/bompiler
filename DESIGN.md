# BOMPILER: a boolin c compiler
### Author: Robert Bergeron
### Email: rbergero@ucsc.edu

## 1. Tools/Languages
The compiler will be written in C. This is mostly so that, if I at some point
manage to actually complete this monstrosity, my compiler will be able to
compile itself, which I think is a pretty big flex.

The build tool I will be using is make. On Archlinux, the distribution I will
initially be doing development on, the default implementation of make is GNU
Make.

Non-code documents (like this one) will be in markdown format for now. I plan
to switch to LaTeX at a later date once I have figured out how it works and
have set up an editor and viewer for it.

I will be using flex and bison to generate the scanner and parser, and git will
be used for version control.

Testing and auxiliary code will be written in plain bash script if necessary.

## 2. Format
This section will describe details about the formatting of files for this
project. It will be expanded as I become aware of issues that I have not yet
addressed and believe I will forget or overlook if they are not written down.

The tool I will use to format code is GNU Indent. Since Indent only supports
C as far as I know, other files will be formatted by hand or not at all. I
may switch to astyle or clang-format if this becomes a nuisance.

Formatting Rules/Preferences:
1. No files, with the exception of flex and bison source files, will have lines
   over 80 columns in width. This will be expanded to include LaTeX source files
   when I begin using that.
2. The `#pragma` directive shall not be used
3. The following GNU Indent options shall be set:
   - --line-length 80                           or -l80
   - --dont-format-comments                     or -nfca
   - --honour-newlines                          or -hnl
   - --no-comment-delimiters-on-blank-lines     or -ncdb
   - --blank-lines-after-declarations           or -bad
   - --blank-lines-after-procedures             or -bap
   - --no-blank-lines-after-commas              or -nbc
   - --break-before-boolean-operator            or -bbo
   - --dont-break-function-decl-args            or -nbfda
   - --dont-break-procedure-type                or -npsl
   - --braces-on-if-line                        or -br
   - --braces-on-struct-decl-line               or -brs
   - --braces-on-func-def-line                  or -brf
   - --cuddle-else                              or -ce
   - --cuddle-do-while                          or -cdw
   - --leave-preprocessor-space                 or -lps
   - --space-after-for                          or -saf
   - --space-after-if                           or -sai
   - --space-after-while                        or -saw
   - --space-after-procedure-calls              or -pcs
   - --space-after-cast                         or -cs
   - --blank-before-sizeof                      or -bs
   - --no-tabs                                  or -nut
   - --tab-size 4                               or -ts4
   - --indent-level 4                           or -i4
   - --paren-indentation 8                      or -pi4
   - --continuation-indentation 8               or -ci4
   - --case-indentation 4                       or -cli4
   - --preprocessor-indentation 4               or -ppi4
   - --case-brace-indentation 0                 or -cbi0
   - --brace-indent 0                           or -bli0
                                                

## 3. Tasks
It may be helpful to write a scanner and parser for the existing intermediate
language specified for F19 CSE110A by Wesley Mackey. The parser will be rather
simple, and will simply translate the intermediate language into an actual
ISA. The first ISA that I will translate shall be MIPS, because it is the ISA
I am most familiar with. x86-64 will come second. I will likely continue to use
this intermediate language since it seems sufficient to express most operations
that I am aware of, though it will likely need to be extended.

The compiler, which is currently written in C++, will be rewritten in C.
This means that I must write my own implementation of `std::unordered_map` and
scrub out the use of `std::string`, which may prove to be difficult, because
if there's anything I have learned, it's that I don't know how to handle
C-strings correctly. flex and bison will also need to be tweaked to output
C code instead of C++.

After looking at the scanner I have decided it would be helpful to write a
replacement for `std::vector` since I need a way to keep track of an arbitrary
number of filename includes, and I imagine there will be more in the future.

## 4. Design

### 4.1 `map.h map.c`
This files will contain the replacement for `std::unordered_map`. It will use
Knuth's multiplicative method to hash values.

#### `struct map`
This structure will contain the actual map data. Only capable of storing
pointers. Keys and values shall be stored in arrays of type `void **` allocated
on the stack. Has a field indicating the current size of the map and the number
of values currently stored within the map. Has two dynamically allocated arrays,
one of. Should only be interacted with using the functions provided by the
header file, for consistency, the only exception to this being structure
initialization.

#### `map_put`
Takes three arguments: the map and the key and value to emplace in the map.
Calls the hash function to compute the index the pair should be mapped to.
Then, checks the values at that location. If the keys match or there is no key
present, write the pair to that location. If there has been a collision, use
open addressing to map the key-value pair to a different location. If there are
no more open slots, call `map_expand`. Key may not be `NULL`.

#### `map_get`
Takes two arguments: the map and the key to search for in the table.
Call the hash function to compute the index to check at. Then, check the key at
the location. If the key at that location does not match, or there is no key,
search over the table to make sure the key/value pair has not been open
addressed. Return `NULL` if no matching entry was found. Key may not be `NULL`.

#### `map_remove`
Takes two arguments: the map and the key of the value to be removed. Computes
index with hash function and compares given value with stored value. If not
present, iterates over open addressed values until finds a key that matches or
reacheds the end. Removes key and value from map if present. Returns nothing.
Argument may not be `NULL`.

#### `map_free`
Takes one argument: a pointer to the map that is to be freed. Calls `free` on
the key and value arrays (if they are not null), and the map itself.

#### `map_init`
Takes one argument: the size of the map to be created. Allocates space for a
map and the key-value pairs. Initializes entry and open address count to 0.

#### `map_expand`
Takes one argument: the map that is to be expanded. Allocates a new array that
is twice the size of the original and copies the values into it. Since the
values returned by the hash will be different after expansion, the entire thing
might need to be remapped? That's a really terrible way to do it so there may
be a better alternative. Will have to see.

### 4.2 `string_set.h string_set.c`
These files will contain the code responsible for storing and tracking unique
tokens inside of the source file(s). Internally, the set of tokens shall be
represented by a `map` with key and value both being the string itself.

#### `string_set_intern`
Takes one argument, the cstring to enter into the set. Returns the string that
was inserted.

#### `string_set_dump`
Takes one argument, a `FILE *` that the contents of the set will dump dumped to.
Formats and prints out the contents of the map in a pretty way.

### 4.3 `vector.h vector.c`
These files will contain the replacement for `std::vector`. Like `map`, stored
values must be pointers.

#### `struct vector`
This structure shall contain 3 fields: a dynamically allocated array to store
the contents in, a `size_t` containing the length of the array, and the number
of elements that currently exist in the vector as a stack. The internal array
shall be of type `void **`.

While a vector can be used as both an array and a stack, elements inserted into
the vector without the stack's knowledge (elements inserted at indices greater
than `stack_size`) may be ignored by the stack operations.

#### `vector_get`
Takes 2 arguments: the vector to retrieve the element from and the index of the
element to be retrieved. Returns the element at the provided index, or `NULL` if
none is present. The index argument may not be larger than (size of the
array - 1) or less than 0.

#### `vector_put`
Takes 3 arguments: the vector to insert the element into, the index at which the
element is to be inserted, and a pointer to the element to be inserted. Inserts
the object at the specified index. Returns nothing. The index argument may not
be larger than (size of the array - 1) or less than 0.

#### `vector_remove`
Takes 2 arguments: the vector to remove an element from and the index of the
element to be removed. Returns the value that was located at that index, or
`NULL` if there was none.

#### `vector_append`
Takes 2 arguments: the vector to append the element to, and a pointer to the
element to append. Appends the element to the end of the vector.

#### `vector_push`
Takes 2 arguments: the vector to push the element onto and a pointer. Pushes
the pointer onto the front of the vector as if it were a stack. Automatically
doubles the size of the vector if it is full.

#### `vector_pop`
Takes one argument: the vector to pop the first element of. Takes the first
element of the vector and removes it, then returns it. Returns `NULL` if the
vector is empty.

#### `vector_peek`
Takes one argument: the vector to peek at the first elemnt of. Returns the first
element of the vector, or `NULL` if the vector is empty.

#### `vector_empty`
Takes one argument: the vector to check the contents of. Returns whether or not
the vector is empty (whether or not all entries are `NULL`).

#### `vector_expand`
Takes one argument: the vector to expand. Doubles the length of the vector.
Returns nothing.

#### `vector_init`
Takes one argument: the size of the vector to be created. Allocates space for
the vector and it's contents. Returns a pointer to the created vector.

#### `vector_free`
Takes one argument: a pointer to the vector to be freed. Frees the `struct
vector` and the internal array. Returns nothing.

### 4.X `test.c test.h`
This file will contain functions for testing components of the compiler, like
the hashmap, type checking, AST construction, etc. I will not be specifying
most of the functions in this file in the design document, for my own

Runs tests. In the future, this may take command line arguments that specify
what tests to execute/exclude.

### TODO: this will be copied in from documents for the original compiler
