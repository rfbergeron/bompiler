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
have set up an editor and document viewer for it.

I will be using flex and bison to generate the scanner and parser, and git will
be used for version control.

Testing and auxiliary code will be written in plain bash script if necessary.

## 2. Format
This section will describe details about the formatting of files for this
project. It will be expanded as I become aware of issues that I have not yet
addressed and believe I will forget or overlook if they are not written down.

The tool I will use to format code is GNU Indent. Since Indent only supports
C/C++ as far as I know, other files will be formatted by hand or not at all. I
may switch to astyle or clang-format if this becomes a nuisance.

Formatting Rules/Preferences:
1. No files, with the exception of flex and bison source files, will have lines
over 80 columns in width. This will be expanded to include LaTeX source files
when I begin using that.
2. the `#pragma` directive shall not be used

## 3. Plan
Since I am very impatient, the first thing I shall do is write a scanner and
parser for the existing intermediate language specified for F19 CSE110A by
Wesley Mackey. The parser will be rather simple, and will simply translate
the intermediate language into an actuall ISA. The first ISA that I will
translate shall be MIPS, because it is the ISA I am most familiar with.
x86-64 will come second. I will likely continue to use this intermediate
language since it seems sufficient to express most operations that I am
aware of, though it will likely need to be extended.

Then, the compiler, which is currently written in C++, will be rewritten in C.
This means that I must write my own implementation of `std::unordered_map` and
scrub out the use of `std::string`, which may prove to be difficult, because
if there's anything I have learned, it's that I don't know how to handle
C-strings correctly. flex and bison will also need to be tweaked to output
C code instead of C++.

The compiler must then also be changed to match actual C syntax, while not
adding additional functionality. This means that pointer, struct, array,
and memory allocation syntax must be altered significantly. It may be better
to rewrite this bit from scratch instead of trying to shoehorn in something
else. Actual support for `char` will need to be added; currently `char` is
simply a macro for `int` handled by the preprocessor.

## 4. Design
###TODO: this will be copied in from documents for the original compiler
