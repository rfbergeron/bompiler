# How To Bitfield

## Type Representation

So, fortunately, the use of specifiers other than `int`, `unsigned` or `signed`
are a GCC extension so we don't have to worry about implementing them. However,
we still need to find space in the type representation where bitfields can fit.

The GCC extension would also not be hard to support; the type specifier(s) used
do not affect the size or alignment of the bitfield. They only serve to
instruct the compiler to produce a warning if the number of specified bits
exceeds that of the type.

I don't think it would be reasonable to try to squeeze any additional space out
of the existing representation. We could fix the design quirk of keeping the
storage class specifier with the type information until the symbol can be
created, but that would require storing that information somewhere else, either
by allocating the symbol earlier or by creating another place for state to be
stored, which sounds messy.

Instead, we should reuse the space that additional information for tags is
stored in, turning it into a union. Size would not be a concern here; an
`unsigned char` would be more than enough. We could even use a bit field. In
the flags information, bit fields are indicated using three encodings as
"other" types: one each for explicitly signed, explicitly unsigned, and
unspecified signedness.

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

<!-- vim: set tw=79 ts=8 sts=4 sw=4 et fo-=l: -->
