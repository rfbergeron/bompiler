# TODO
1. switch from khaoslib to my own data structures
2. downgrade from gnu11 to ANSI C
3. separate typechecker and symbol table into their own source and header files
   for the sake of organization

# Order of implementation
This list will serve as a rough outline of the order in which I want to implement
compiler features
1. function calls and definitions
2. structure definitions
3. variable declarations
4. pointers/arrow operator
4. nested scope using the brace operator, which will be important when
   when implementing other things
4. if statements
5. switch statements

# Current commit plans
- Make attribute and type handling more flexible/extensible
