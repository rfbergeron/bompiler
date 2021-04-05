# What order am I doing things in?
- Get type checking in order
  - fix the actual types system
    - the only types needed for base functionality are different integer widths
      and void (for return types)
  - restructure the switch statements in `validate_expr` and `validate_stmt`,
    moving code into separate functions
  - implement nested scoping
    - stack for symbol tables
    - stack for function tables to allow nesting of functions
    - stack for sequence numbers to count and order declarations in each block
- Basic generation of assembly:
  - allocating stack space for functions, scopes, etc.
    - it would be more natural to allocate more stack space each time a new
      scope is entered, instead of just doing all of it at the beginning of the
      function, since I would have to write an entire function just to
      recursively count the number of bytes needed
    - on Linux, the stack needs to be 16-byte aligned before function calls, so
      I will just always ensure that new scopes do this
    - i believe the simplest way to allocate stack space efficiently would be to
      put the values on the stack in order of decreasing alignment requirements
  - Integer expressions with operands of varying width
    - the width of the instruction will be the width of the widest of its
      operands
  - Function calls
    - integer arguments and void or integer return types
    - on windows, only 4 registers are available for non-floating arguments, so
      for now I will limit the compiler to that many registers, even though 6
      are available on Unix-like systems
  - Do not bother with register allocation yet; for now output placeholder
    values for registers so that other aspects of the compiler can be checked
  - emit conversions for differing integer widths
    - to make code generation easier, all types will be loaded as their native
      width initially
    - use bitmasks and sign extension to preform conversions after the fact
- Pointers and pointer math
  - automatic casting to and from void pointers
  - array element access
  - nested pointers and nested types in general
  - keep in mind that the only implicit casting allowed occurs during promotion
    of arithmetic types and when going to and from `void*`. Explicit casts
    are only valid between any other types of pointers and arithmetic types.
    Casting to and from a union and its members is valid, but I will omit it
    for now
- Structures
  - pointers to structures
  - struct member access with the dot and arrow operators
- Arrays
  - allocating stack space
  - member access, keeping in mind the stride of the array
  - decay to and compatiblility with pointers
    - arrays can be used for functions with pointer arguments
  - array arguments? (allows the length to be checked against)
