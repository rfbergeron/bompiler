# bompiler

A very work-in-progress compiler for a large subset of the ANSI C/ISO C89/C90
programming language, written in the same (mostly) language, though the lexer
and parser are generated by flex and bison, respectively.

It currently does not generate code, but it can parse its own code and can
nearly accurately typecheck itself, with the exception that it does not yet
check for lvalue expressions. Additionally, I do not plan to support floating
point arithmetic, at least not until the compiler is self-hosting.

This project has its roots in this assignment, which I completed for a compilers
course during my undergraduate degree:
https://github.com/rfbergeron/UCSC/tree/master/cse110a/asg5

As you can see, the contents of this repo bear little resemblance to the one
linked above, save for names. The code above is all very poorly written C++,
specifically the gnu++17 dialect. The code in this repo is somewhat less poorly
written ANSI C. If you'd like to view the primordial soup from which this
wretched program emerged, feel free to follow the link above, or just go back
far enough into this repo's commit history.
