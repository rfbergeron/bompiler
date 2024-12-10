#ifndef BCC_TYPES_H
#define BCC_TYPES_H
#include "stdio.h"

#define X64_SIZEOF_LONG ((size_t)8UL)
#define X64_ALIGNOF_LONG ((size_t)8UL)
#define X64_SIZEOF_INT ((size_t)4UL)
#define X64_ALIGNOF_INT ((size_t)4UL)
#define X64_SIZEOF_SHRT ((size_t)2UL)
#define X64_ALIGNOF_SHRT ((size_t)2UL)
#define X64_SIZEOF_CHAR ((size_t)1UL)
#define X64_ALIGNOF_CHAR ((size_t)1UL)

#define X64_SIZEOF_FLT ((size_t)4UL)
#define X64_ALIGNOF_FLT ((size_t)4UL)
#define X64_SIZEOF_DBL ((size_t)8UL)
#define X64_ALIGNOF_DBL ((size_t)8UL)
#define X64_SIZEOF_LDBL ((size_t)16UL)
#define X64_ALIGNOF_LDBL ((size_t)16UL)

#define BCC_DEDUCE_ARR ((size_t)0UL)

typedef union type Type;
union tag;

extern const Type *const TYPE_VOID;
extern const Type *const TYPE_POINTER;
extern const Type *const TYPE_CHAR;
extern const Type *const TYPE_INT;
extern const Type *const TYPE_LONG;
extern const Type *const TYPE_UNSIGNED_INT;
extern const Type *const TYPE_UNSIGNED_LONG;
/* this one is in `parser.c` */
extern const Type *TYPE_VA_LIST_POINTER;
/* TODO(Robert): Type: is this necessary anymore? */
extern const Type *const TYPE_NONE;
extern const Type *const TYPE_VA_SPILL_REGION;

void type_init_globals(void);

/* Type *type_init_decl_specs(unsigned int flags, union tag *tag); */
Type *type_init_basic(void);
Type *type_init_tag(union tag *tag);
Type *type_init_pointer(void);
Type *type_init_array(size_t length);
Type *type_init_function(size_t parameters_size, Type **parameters,
                         int variadic);

void type_destroy(Type *type);

int type_to_str(const Type *type, char *buf);

int type_is_void(const Type *type);
int type_is_integer(const Type *type);
int type_is_signed(const Type *type);
int type_is_unsigned(const Type *type);
int type_is_enum(const Type *type);
int type_is_integral(const Type *type);
int type_is_arithmetic(const Type *type);
int type_is_pointer(const Type *type);
int type_is_scalar(const Type *type);
int type_is_array(const Type *type);
int type_is_deduced_array(const Type *type);
int type_is_function(const Type *type);
int type_is_variadic_function(const Type *type);
int type_is_void_pointer(const Type *type);
int type_is_function_pointer(const Type *type);
int type_is_struct(const Type *type);
int type_is_struct_pointer(const Type *type);
int type_is_union(const Type *type);
int type_is_union_pointer(const Type *type);
int type_is_record(const Type *type);
int type_is_record_pointer(const Type *type);
int type_is_aggregate(const Type *type);
int type_is_char_array(const Type *type);
int type_is_const(const Type *type);
int type_is_volatile(const Type *type);
int type_is_qualified(const Type *type);
int type_is_declarator(const Type *type);
int type_is_none(const Type *type);
int type_is_incomplete(const Type *type);

struct symbol *type_get_member_name(const Type *type, const char *name);
struct symbol *type_get_member_index(const Type *type, size_t index);
size_t type_get_member_count(const Type *type);

size_t type_get_elem_count(const Type *type);
void type_set_elem_count(Type *type, size_t count);

Type *type_get_param(const Type *type, size_t index);
size_t type_get_param_count(const Type *type);

size_t type_get_alignment(const Type *type);
size_t type_get_width(const Type *type);
int type_retrieve_storage_specifier(Type *type);
size_t type_get_elem_width(const Type *type);
size_t type_get_eightbytes(const Type *type);
size_t type_get_padding(const Type *type, size_t to_pad);
union tag *type_get_tag(const Type *type);
int types_compatible(const Type *type1, const Type *type2, int qualified);
int types_assignable(const Type *dest, const Type *src, int is_const_zero);

Type *type_strip_declarator(const Type *type);
Type *type_get_decl_specs(Type *type);
Type *type_append(Type *dest, Type *src);
Type *type_detach(Type *type);
Type *type_copy(const Type *type);
Type *type_common_qualified_pointer(const Type *type1, const Type *type2);
int type_add_decl_spec(Type *type, int tok_kind);
Type *type_merge(const Type *dest, const Type *src);
int type_normalize(Type *type);
int type_validate(const Type *type);
Type *type_pointer_conversions(Type *type);
Type *type_arithmetic_conversions(Type *type1, Type *type2);
int type_compose(Type *dest, Type *src, int qualified);

#endif
