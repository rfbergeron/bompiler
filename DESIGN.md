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
   Long:                                        Short:
   - --line-length 80                           -l80
   - --dont-format-comments                     -nfca
   - --honour-newlines                          -hnl
   - --no-comment-delimiters-on-blank-lines     -ncdb
   - --blank-lines-after-declarations           -bad
   - --blank-lines-after-procedures             -bap
   - --no-blank-lines-after-commas              -nbc
   - --break-before-boolean-operator            -bbo
   - --dont-break-function-decl-args            -nbfda
   - --dont-break-procedure-type                -npsl
   - --braces-on-if-line                        -br
   - --braces-on-struct-decl-line               -brs
   - --braces-on-func-def-line                  -brf
   - --cuddle-else                              -ce
   - --cuddle-do-while                          -cdw
   - --leave-preprocessor-space                 -lps
   - --space-after-for                          -saf
   - --space-after-if                           -sai
   - --space-after-while                        -saw
   - --space-after-procedure-calls              -pcs
   - --space-after-cast                         -cs
   - --blank-before-sizeof                      -bs
   - --no-tabs                                  -nut
   - --tab-size 4                               -ts4
   - --indent-level 4                           -i4
   - --paren-indentation 8                      -pi4
   - --continuation-indentation 8               -ci4
   - --case-indentation 4                       -cli4
   - --preprocessor-indentation 4               -ppi4
   - --case-brace-indentation 0                 -cbi0
   - --brace-indent 0                           -bli0
                                                

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

## 4. Design

### 4.1 `map.h map.c`
This files will contain the replacement for `std::unordered_map`. It will use
Knuth's multiplicative method to hash values.

#### `struct map`
This structure will contain the actual map data. Only capable of storing
pointers. Will store values internally as type `uintptr_t`. Has a field
indicating the current size of the map and the number of values currently
stored within the map. Has two dynamically allocated arrays, one of type
`uint32_t` for the keys and another of type `uintptr_t` for the values. Should
only be interacted with using the functions provided by the header file, for
consistency, the only exception to this being structure initialization.

#### `put`
Takes three arguments: the map and the key and value to emplace in the map.
Calls the hash function to compute the index the pair should be mapped to.
Then, checks the values at that location. If the keys match or there is no key
present, write the pair to that location. If there has been a collision, use
open addressing to map the key-value pair to a different location. If there are
no more open slots, indicate that there has been an error.

#### `get`
Takes two arguments: the map and the key to search for in the table.
Call the hash function to compute the index to check at. Then, check the key at
the location. If the key at that location does not match, or there is no key,
search over the table to make sure the key/value pair has not been open
addressed.

#### `free_map`
Takes one argument: the map that is to be freed. Calls `free` on the key and
value arrays. Argument may need to be a pointer? Not sure.

#### `expand`
Takes one argument: the map that is to be expanded. Allocates a new array that
is twice the size of the original and copies the values into it. Since the
values returned by the hash will be different after expansion, the entire thing
might need to be remapped? That's a really terrible way to do it so there may
be a better alternative. Will have to see.

### 4.X `test.c test.h`
This file will contain functions for testing components of the compiler, like
the hashmap, type checking, AST construction, etc. I will not be specifying
most of the functions in this file in the design document, for my own
convenievce and because I don't actually know what tests I'll be running.

The tests will be contained in a separate executeable.

#### `main`
Runs tests. In the future, this may take command line arguments that specify
what tests to execute/exclude.

### TODO: this will be copied in from documents for the original compiler
