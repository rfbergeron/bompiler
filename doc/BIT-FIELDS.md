# How To Bitfield

## ABI Corrections

So, UNFORTUNATELY, the use of specifiers other than int is not really a GCC
extension on account of the fact that the AMD64 ABI actually changes the way
bit fields are allocated depending on the type specifiers. While the ANSI C
standard seems to imply that only `int`, possibly with `signed` or `unsigned`
are valid type specifiers, the ABI takes precedence over it.

Bit fields do affect the alignment of records, so padding bytes may need to be
added depending on the layout.

The integer type specifiers affect the allocation unit of the bit field. This
is important because not only may bit fields not be specified to have more bits
than their underlying type, they also may not occupy more than one unit of
storage. I think the simplest way to state this constraint is the following:
the value of a bit field must be retrievable from memory using a single load
aligned appropriately for the underlying type, followed optionally by a logical
right shift, a bit mask, and sign extension.

Zero-width unnamed bit fields also behave slightly differently; they close off
the allocation unit appropriate for their underlying type from further usage.
This also prevents member that are not bit fields from being stored in that
allocation unit. For example, a member of type `unsigned short` followed by a
member of type `long :0` would insert 6 bytes of padding. However, if the
second member has type `short :0`, no padding bytes would be inserted, since
they previous allocation unit is already fully occupied.

## Representation Corrections

Bit fields should probably be represented using a flag since preserving the
underlying integer type is important. Zero-width bit fields should also have
their own flag.

The width and offset of bit fields will need 6 bits each to store. These will
be put at some arbitrary offset in the arithmetic flags.

## Implementation Corrections

Laying out bit fields is going to be a little more complicated.

The struct will still track the byte offset and bit offset. The bit offset will
still be limited to the number of bits in a byte.

When there is a partial allocation unit from the previous bit field, we need to
make sure that the size of the allocation unit is appropriate for the current
bit field. This cannot be determined from the bit offset alone.

We don't want to have to worry about bit offsets in allocation units larger
than one byte because if we did then we'd have to do bit math for every single
struct member, which doesn't sound fun.

So, we have to calculate the allocation unit offset every time we handle a bit
field. The allocation unit offset is `((byte_offset * CHAR_BIT) - unused_bits)
% (alignment * CHAR_BIT)`. if `unit_offset + bit_width < alignment * CHAR_BIT`,
then we can pack the bit field into the current allocation unit.

``` Pseudocode
input: byte_offset, bit_offset [0...7], alignment, bit_width
output: byte_offset, bit_offset [0...7]

if bit_offset == 0:
    total_offset = byte_offset * CHAR_BIT
else:
    total_offset = ((byte_offset - 1) * CHAR_BIT) + bit_offset

unit_offset = total_offset % (alignment * CHAR_BIT)

if unit_offset + bit_width > CHAR_BIT * alignment:
    pad struct to alignment
    set bit field byte offset to size + padding
    set bit field bit offset to zero
    increase struct size by number of bytes in bit field
    set struct bit offset tracker to bit_width % CHAR_BIT
else:
    set bit field byte offset to (total_offset - unit_offset) % CHAR_BIT
    set bit field bit offset to unit_offset
    increase size by (bit_width - bit_offset + CHAR_BIT) / CHAR_BIT
    set bit offset to (bit_width + unit_offset) % CHAR_BIT
```

## Layout, Packing and Padding

Packing works in the expected way: the leftmost bitfield in a declaration
occupies the least significant bits of the region it is stored in, and further
bitfields are assigned in ascending order. Bit fields have no alignment
requirements (byte alignment) and can occupy an arbitrary number of bits. A bit
field declared immediately after a struct member that is not a bit field
occupies the byte immediately after the last byte occupied by the previous
member declaration. A struct member declared immediately after a bit field that
is not itself a bit field is aligned according to the normal rules.

Bit fields of size zero causes the compiler to stop packing bits into the
storage unit occupied by the previous bitfield (if any). Bit fields of size
zero must be unnamed; named bit fields of size zero result in a compilation
error. This means that if the previous struct member is not a bit field or if
the storage unit occupied by the previous bit field is fully utilized, such a
declaration does nothing. Since bit fields have no alignment requirements, this
means that the remaining unused bits of the previous byte, between 0 and 7
bits, will become padding. Only adjacent bit fields are packed; if a member
that is not a bit field is declared between two bit fields, they are not
packed.

## Reading

The simplest, most reliable way to access the bytes of bit fields would be to
use byte load instructions on each byte occupied by a bit field, with shifts
and bitwise or instructions interleaved to construct the value in a register.
A bit mask is applied to the constructed value, which is then either sign or
zero extended.

Integers are already extended to the native arithmetic size of the platform, in
this case 32 bits, for intermediate operations; this is just a special case of
that. Bit fields greater than 32 bits in size are not required by the standard
and will not be supported unless made necessary by glibc.

## Writing

After arithmetic operations have been applied, the bit field is shifted to the
appropriate offset, such that its least significant bit is in the same position
as the least significant bit in the first storage unit it occupies.

The value is written to its storage units in byte sized chunks. Each occupied
storage unit is first loaded into a register. The original values of the bits
that portion of the field would occupy are masked out. The new values are or'd
in, and the byte is written back to memory. The least significant byte of the
value to be written is discarded with a right shift and the process repeats
until each byte of the bit field has been written to memory.

## Representation

### The "None" Type

A type node with all bits set to zero indicates an empty declaration specifier
list.

A type node with a non-zero flags member, arithmetic type kind and an
arithmetic flags member set to zero indicates a declaration specifier list with
no type specifiers. Type nodes of this form are converted to implicit integers
where appropriate.

### Arrays

Bit 0 is used to indicate whether or not the type is a complete array (an array
with known length). If set, the remaining bits are used to store the length of
the array minus 1.

Bit 1 is used to indicate any other kind of array. If set, the other bits,
excepting bit 0, are used to store more information about the array. Currently,
this is only used to represent incomplete array types (arrays with indefinite
or unknown length).

### Type Kinds

Bits 2 and 3 are used to distinguish between four type "kinds": arithmetic,
pointer, function and "other". If bits 0 and 1 are unset, these bits are used
to determine how to interpret the other bits in the flags field. These types do
not make use of bit 8, which is currently reserved for future use.

### Type Qualifiers

If the type kind is pointer, arithmetic or other bits 9 through 12 (inclusive)
are used as flags for type qualifiers. Bits 9 and 10 are used for `const` and
`volatile`, respectively. Bits 11 and 12 are reserved for `restrict` and
`atomic`, respectively.

### Storage Class Specifiers

If the type kind is arithmetic or other bits 13 through 15 (inclusive) are used
as a 3 bit integer representing the optional storage class specifier. If no
storage class specifier is present, these bits are set to zero. The five
storage class specifiers are assigned to other values in the range, and the
remaining two values are invalid/reserved for future use.

### Functions

If the type kind is function, bits 4 and 5 are used as flags to mark the
function as an old style function and/or variadic function, respectively. These
flags are somewhat redundant, as an old style function could be interpreted as
a variadic function with zero parameters, since variadic functions are required
to have at least one named parameter (until C23). Bits 6 and 7 are reserved for
the `inline` and `_Noreturn` keywords. Bits 9 and above are used to store the
number of named function parameters. This representation allows the compiler to
reach the minimum 127 function parameters required for a conforming
implementation even if the flags field is restricted to 16 bits. The second
member of the struct is used as a dynamically allocated array of pointers to
the parameter and return type information. The return type is the last element
of the array.

### Pointers

If the type kind is pointer, bits 4 through 7 (inclusive) and bits 13 through
15 (inclusive) are unused. The second member of the struct is used as a pointer
to further type information (the type being pointed to).

### Arithmetic Types

If the type kind is arithmetic, bits 4 through 7 (inclusive) are unused. The
second member of the struct is used as an additional flags field holding more
specific information about which arithmetic type it is.

Arithmetic types store most of their type information in the second member of
the struct. Type specifiers which cannot occur together are be grouped together
into bit integers. Others receive their own flags. Which representation is used
depends on which makes it more convenient to check for valid combinations of
arithmetic type specifiers.

Bit fields use the same representation as regular integers except that they
have some additional flags set.

#### Bits [0, 3]

Each of the following arithmetic types/arithmetic type specifiers are
represented as a value in a 4 bit integer in bits 0 - 3 (inclusive). These
types/type specifiers are mutually incompatible with each other:

- `int`
- `char`
- `float`
- `double`
- implicit integers (no type specifiers)

The zero value in this range is used to indicate a declaration specifier list
where none of these type specifiers has occured. Unassigned values are reserved
for future use.

#### Bits 4 and 5

Bits 4 and 5 are used as flags to indicate the `signed` and `unsigned`
arithmetic type specifiers.

#### Bits 6 and 7

Bits 6 and 7 are used as a 2-bit integer to store the `short` and `long` type
specifiers. If these bits are unset, neither of these type specifiers is
present in the declaration specifier list. Setting both bits is reserved for
`long long`.

#### Bits [8, 21]

Bits 8 and 9 are use to store bit flags indicating that an integer type is
actually a bit field. Bit 8 indicates a normal bit field, while bit 9 indicates
a zero-width bit field.

Bits 10 through 15 (inclusive) are used to store the offset of a bit field
within its allocation unit.

Bits 16 through 21 (inclusive) are used to store the width of a bit field,
minus one.

### Other Types

If the type kind is other, bits 4 through 7 are used to indicate whether the
type is `void`, `struct`, `union` or `enum`. The other 12 values in this range
are reserved for future use. If the type is a `struct`, `union` or `enum`, the
second member of the struct is used as a pointer to further type information.

<!-- vim: set tw=79 ts=8 sts=4 sw=4 et fo-=l: -->
