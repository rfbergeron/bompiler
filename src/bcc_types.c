#include "bcc_types.h"
#include "stdlib.h"
#include "assert.h"
#include "badllist.h"
#include "badmap.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

int type_init(Type **out, TypeCode code) {
    if (out == NULL) return -1;
    *out = malloc(sizeof(Type));
    (*out)->any.code = code;
    return 0;
}

int type_destroy(Type *type) {
    while (type != NULL) {
        Type *current = type;
        switch (type->any.code) {
            default:
                abort();
            case TYPE_CODE_NONE:
                free(current);
                return 0;
            case TYPE_CODE_STRUCT:
                /* fallthrough */
            case TYPE_CODE_UNION:
                /* fallthrough */
            case TYPE_CODE_ENUM:
                free(current);
                return 0;
            case TYPE_CODE_POINTER:
                type = type->pointer.next;
                free(current);
                break;
            case TYPE_CODE_FUNCTION:
                type = type->function.next;
                free(current->function.parameters);
                free(current);
                break;
            case TYPE_CODE_ARRAY:
                type = type->array.next;
                free(current);
                break;
            case TYPE_CODE_BASE:
                free(current);
                return 0;
        }
    }
    /* a properly formed type should never reach this point, but if the type
     * was formed erroneously and we're performing cleanup it is possible that
     * this singly-linked list may be NULL, so it's not necessarily an error.
     */
    return 0;
}

static const char *qual_get_str(unsigned int qualifiers) {
    switch (qualifiers & QUAL_FLAG_MASK) {
        case QUAL_FLAG_CONST | QUAL_FLAG_VOLATILE:
            return "const volatile";
        case QUAL_FLAG_CONST:
            return "const";
        case QUAL_FLAG_VOLATILE:
            return "volatile";
        default:
            return "";
    }
}

static const char *base_get_str(unsigned int flags) {
    switch (flags & SPEC_FLAG_MASK) {
        case SPEC_FLAG_NONE:
            /* fallthrough */
        default:
            abort();
        case SPEC_FLAG_VOID:
            return "void";
        case SPEC_FLAG_CHAR:
            return "char";
        case SPEC_FLAGS_SCHAR:
            return "signed char";
        case SPEC_FLAGS_UCHAR:
            return "unsigned char";
        case SPEC_FLAGS_SINT:
            return "signed int";
        case SPEC_FLAGS_UINT:
            return "unsigned int";
        case SPEC_FLAGS_SSHRT:
            return "signed short";
        case SPEC_FLAGS_USHRT:
            return "unsigned short";
        case SPEC_FLAGS_SLONG:
            return "signed long";
        case SPEC_FLAGS_ULONG:
            return "unsigned long";
    }
}

int specifiers_to_str(const Type *type, char *buf, size_t size) {
    assert(type->any.code == TYPE_CODE_BASE
            || type->any.code == TYPE_CODE_ENUM
            || type->any.code == TYPE_CODE_UNION
            || type->any.code == TYPE_CODE_STRUCT);
    /* this array assumes that the type codes for base, struct, union, and enum
     * are 1, 2, 3 and 4, respectively. the type code is used as a key to map
     * to the format strings, with an offset of 4 if the type has qualifiers
     */
    static const char *FORMAT_STRINGS[] = {
        "%s",           /* unqualified scalar/void */
        "struct %s",    /* unqualified struct */
        "union %s",     /* unqualified union */
        "enum %s",      /* unqualified enum */
        "%s %s",        /* qualified scalar/void */
        "%s struct %s", /* qualified struct */
        "%s union %s",  /* qualified union */
        "%s enum %s"    /* qualified enum */
    };
    static const size_t QUALIFIED_OFFSET = 4;
    (void)size; /* unused because no snprintf until C99 */
    const char *type_string = type->any.code == TYPE_CODE_BASE
        ? base_get_str(type->base.type_flags) : type->tag.tag_name;
    const char *qualifier_string = qual_get_str(type->any.code == TYPE_CODE_BASE
            ? type->base.type_flags : type->tag.qualifiers);

    size_t format_index = type->any.code - TYPE_CODE_BASE;
    if (type_is_qualified(type))
        return sprintf(buf, FORMAT_STRINGS[format_index + QUALIFIED_OFFSET],
                qualifier_string, type_string);
    else
        return sprintf(buf, FORMAT_STRINGS[format_index], type_string);
}

int type_to_str(const Type *type, char *buf, size_t size) {
    (void)size; /* unused because no snprintf until C99 */
    buf[0] = '\0';
    int ret = 0;
    const Type *current = type;
    while (current != NULL) {
        switch (current->any.code) {
            case TYPE_CODE_NONE:
                /* fallthrough */
            default:
                abort();
            case TYPE_CODE_BASE:
                /* fallthrough */
            case TYPE_CODE_STRUCT:
                /* fallthrough */
            case TYPE_CODE_UNION:
                /* fallthrough */
            case TYPE_CODE_ENUM:
                ret += specifiers_to_str(current, buf + ret, size - ret);
                current = NULL;
                break;
            case TYPE_CODE_FUNCTION:
                if (current->function.variadic)
                    ret += sprintf(buf + ret, "variadic function with parameters (");
                else
                    ret += sprintf(buf + ret, "function with parameters (");
                size_t i;
                for (i = 0; i < current->function.parameters_size; ++i) {
                    if (i > 0) ret += sprintf(buf + ret, ", ");
                    /* TODO(Robert): make SymbolValue use new type
                    ret += type_to_str(&current->function.parameters[i]->type,
                            buf + ret, size - ret);
                    */
                }
                ret += sprintf(buf + ret, ") returning ");
                current = current->function.next;
                break;
            case TYPE_CODE_ARRAY:
                if (current->array.deduce_length)
                    ret += sprintf(buf + ret, "array of deduced size of ");
                else
                    ret += sprintf(buf + ret, "array of size %lu of ",
                            current->array.length);
                current = current->array.next;
                break;
            case TYPE_CODE_POINTER:
                if (type_is_qualified(current))
                    ret += sprintf(buf + ret, "%s pointer to",
                            qual_get_str(current->pointer.qualifiers));
                else
                    ret += sprintf(buf + ret, "pointer to");
                current = current->pointer.next;
                break;
        }
    }
}

int type_is_void(const Type *type) {
    return type->any.code == TYPE_CODE_BASE
        && ((type->base.type_flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_VOID);
}

int type_is_integer(const Type *type) {
    return type->any.code == TYPE_CODE_BASE
        && ((type->base.type_flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_INT
                || (type->base.type_flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_CHAR);
}

int type_is_signed(const Type *type) {
    return type_is_integer(type)
        && !(type->base.type_flags & SPEC_FLAG_UNSIGNED);
}

int type_is_unsigned(const Type *type) {
    return type_is_integer(type)
        && !(type->base.type_flags & SPEC_FLAG_SIGNED);
}

int type_is_enum(const Type *type) {
    return type->any.code == TYPE_CODE_ENUM;
}

int type_is_arithmetic(const Type *type) {
    return type_is_integer(type) || type_is_enum(type);
}

int type_is_pointer(const Type *type) {
    return type->any.code == TYPE_CODE_POINTER;
}

int type_is_scalar(const Type *type) {
    return type_is_pointer(type) || type_is_arithmetic(type);
}

int type_is_array(const Type *type) {
    return type->any.code == TYPE_CODE_ARRAY;
}

int type_is_deduced_array(const Type *type) {
    return type->any.code == TYPE_CODE_ARRAY && type->array.deduce_length;
}

int type_is_function(const Type *type) {
    return type->any.code == TYPE_CODE_FUNCTION;
}

/* TODO(Robert): determine if this function should accept both function and
 * function pointer types as arguments
 */
int type_is_variadic_function(const Type *type) {
    return type->any.code == TYPE_CODE_FUNCTION && type->function.variadic;
}

int type_is_void_pointer(const Type *type) {
    return type_is_pointer(type) && type_is_void(type->pointer.next);
}

int type_is_function_pointer(const Type *type) {
    return type_is_pointer(type) && type_is_function(type->pointer.next);
}

int type_is_struct(const Type *type) {
    return type->any.code == TYPE_CODE_STRUCT;
}

int type_is_struct_pointer(const Type *type) {
    return type_is_pointer(type) && type_is_struct(type->pointer.next);
}

int type_is_union(const Type *type) {
    return type->any.code == TYPE_CODE_UNION;
}

int type_is_union_pointer(const Type *type) {
    return type_is_pointer(type) && type_is_union(type->pointer.next);
}

int type_is_aggregate(const Type *type) {
    return type_is_struct(type) || type_is_union(type) || type_is_array(type);
}

int type_is_char_array(const Type *type) {
    return type_is_array(type) && type->array.next->any.code == TYPE_CODE_BASE
        && (type->array.next->base.type_flags & SPEC_FLAG_LOW_MASK)
            == SPEC_FLAG_CHAR;
}

int type_is_const(const Type *type) {
    switch (type->any.code) {
        case TYPE_CODE_NONE:
            /* fallthrough */
        case TYPE_CODE_FUNCTION:
            /* fallthrough */
        default:
            abort();
        case TYPE_CODE_ENUM:
            /* fallthrough */
        case TYPE_CODE_STRUCT:
            /* fallthrough */
        case TYPE_CODE_UNION:
            return !!(type->tag.qualifiers & QUAL_FLAG_CONST);
        case TYPE_CODE_POINTER:
            return !!(type->pointer.qualifiers & QUAL_FLAG_CONST);
        case TYPE_CODE_ARRAY:
            /* TODO(Robert): determine if arrays should be considered const */
            return 0;
        case TYPE_CODE_BASE:
            return !!(type->base.type_flags & QUAL_FLAG_CONST);
    }
}

int type_is_volatile(const Type *type) {
    switch (type->any.code) {
        case TYPE_CODE_NONE:
            /* fallthrough */
        case TYPE_CODE_FUNCTION:
            /* fallthrough */
        default:
            abort();
        case TYPE_CODE_ENUM:
            /* fallthrough */
        case TYPE_CODE_STRUCT:
            /* fallthrough */
        case TYPE_CODE_UNION:
            return !!(type->tag.qualifiers & QUAL_FLAG_VOLATILE);
        case TYPE_CODE_POINTER:
            return !!(type->pointer.qualifiers & QUAL_FLAG_VOLATILE);
        case TYPE_CODE_ARRAY:
            /* TODO(Robert): determine if arrays should be considered const */
            return 0;
        case TYPE_CODE_BASE:
            return !!(type->base.type_flags & QUAL_FLAG_VOLATILE);
    }
}

int type_is_qualified(const Type *type) {
    return type_is_const(type) || type_is_volatile(type);
}

/* TODO(Robert): make sure that the incompleteness check on arrays with deduced
 * length makes sense
 */
int type_is_incomplete(const Type *type) {
    if (type_is_void(type)) {
        return 1;
    } else if (type_is_deduced_array(type) && type->array.length == 0) {
        return 1;
    } else if ((type_is_union(type) || type_is_struct(type))
            && !type->tag.tag_value->is_defined) {
        return 1;
    } else {
        return 0;
    }
}

SymbolValue *type_member_name(const Type *type, const char *name) {
    assert(type_is_union(type) || type_is_struct(type));
    return symbol_table_get(type->tag.tag_value->data.members.by_name, name,
            strlen(name));
}

SymbolValue *type_member_index(const Type *type, size_t index) {
    assert(type_is_union(type) || type_is_struct(type));
    return llist_get(&type->tag.tag_value->data.members.in_order, index);
}

size_t type_member_count(const Type *type) {
    assert(type_is_aggregate(type));
    switch (type->any.code) {
        case TYPE_CODE_UNION:
            return 1;
        case TYPE_CODE_STRUCT:
            return llist_size(&type->tag.tag_value->data.members.in_order);
        case TYPE_CODE_ARRAY:
            return type->array.length;
        default:
            abort();
    }
}

SymbolValue *type_param_index(const Type *type, size_t index) {
    assert(type_is_function(type) && index < type->function.parameters_size);
    return type->function.parameters[index];
}

size_t type_param_count(const Type *type) {
    return type->function.parameters_size;
}

size_t type_base_alignment(const Type *type) {
    assert(type->any.code == TYPE_CODE_BASE);
    switch (type->base.type_flags & SPEC_FLAG_MASK) {
        case SPEC_FLAG_VOID:
            return 0;
        case SPEC_FLAG_CHAR:
            /* fallthrough */
        case SPEC_FLAGS_SCHAR:
            /* fallthrough */
        case SPEC_FLAGS_UCHAR:
            return X64_ALIGNOF_CHAR;
        case SPEC_FLAGS_SINT:
            /* fallthrough */
        case SPEC_FLAGS_UINT:
            return X64_ALIGNOF_INT;
        case SPEC_FLAGS_SSHRT:
            /* fallthrough */
        case SPEC_FLAGS_USHRT:
            return X64_ALIGNOF_SHORT;
        case SPEC_FLAGS_SLONG:
            /* fallthrough */
        case SPEC_FLAGS_ULONG:
            return X64_ALIGNOF_LONG;
        default:
            abort();
    }
}

/* TODO(Robert): determine if getting the width/alignment of a function is ever
 * valid or necessary
 */
size_t type_get_alignment(const Type *type) {
    switch (type->any.code) {
        case TYPE_CODE_NONE:
            /* fallthrough */
        case TYPE_CODE_FUNCTION:
            /* fallthrough */
        default:
            abort();
        case TYPE_CODE_STRUCT:
            /* fallthrough */
        case TYPE_CODE_UNION:
            /* fallthrough */
        case TYPE_CODE_ENUM:
            return type->tag.tag_value->alignment;
        case TYPE_CODE_POINTER:
            return X64_ALIGNOF_LONG;
        case TYPE_CODE_ARRAY:
            return type_get_alignment(type->array.next);
        case TYPE_CODE_BASE:
            return type_base_alignment(type);
    }
}

size_t type_base_width(const Type *type) {
    assert(type->any.code == TYPE_CODE_BASE);
    switch (type->base.type_flags & SPEC_FLAG_MASK) {
        case SPEC_FLAG_VOID:
            return 0;
        case SPEC_FLAG_CHAR:
            /* fallthrough */
        case SPEC_FLAGS_SCHAR:
            /* fallthrough */
        case SPEC_FLAGS_UCHAR:
            return X64_SIZEOF_CHAR;
        case SPEC_FLAGS_SINT:
            /* fallthrough */
        case SPEC_FLAGS_UINT:
            return X64_SIZEOF_INT;
        case SPEC_FLAGS_SSHRT:
            /* fallthrough */
        case SPEC_FLAGS_USHRT:
            return X64_SIZEOF_SHORT;
        case SPEC_FLAGS_SLONG:
            /* fallthrough */
        case SPEC_FLAGS_ULONG:
            return X64_SIZEOF_LONG;
        default:
            abort();
    }
}

size_t type_get_width(const Type *type) {
    switch (type->any.code) {
        case TYPE_CODE_NONE:
            /* fallthrough */
        case TYPE_CODE_FUNCTION:
            /* fallthrough */
        default:
            abort();
        case TYPE_CODE_ENUM:
            return X64_SIZEOF_INT;
        case TYPE_CODE_UNION:
            /* fallthrough */
        case TYPE_CODE_STRUCT:
            return type->tag.tag_value->width;
        case TYPE_CODE_POINTER:
            return X64_SIZEOF_LONG;
        case TYPE_CODE_ARRAY:
            return type->array.length * type_elem_width(type);
        case TYPE_CODE_BASE:
            return type_base_width(type);
    }
}

size_t type_elem_width(const Type *type) {
    assert(type_is_array(type) || type_is_pointer(type));
    if (type->any.code == TYPE_CODE_ARRAY) {
        return type_get_width(type->array.next);
    } else if (type->any.code == TYPE_CODE_POINTER) {
        return type_get_width(type->pointer.next);
    } else {
        abort();
    }
}

size_t type_get_eightbytes(const Type *type) {
  size_t width = type_get_width(type);
  return (width / 8) + ((width % 8 == 0) ? 0 : 1);
}

int type_strip_declarator(const Type **dest, const Type *src) {
    switch (src->any.code) {
        case TYPE_CODE_NONE:
            /* fallthrough */
        case TYPE_CODE_STRUCT:
            /* fallthrough */
        case TYPE_CODE_ENUM:
            /* fallthrough */
        case TYPE_CODE_UNION:
            /* fallthrough */
        case TYPE_CODE_BASE:
            /* fallthrough */
        default:
            return -1;
        case TYPE_CODE_ARRAY:
            *dest = src->array.next;
            return 0;
        case TYPE_CODE_FUNCTION:
            *dest = src->function.next;
            return 0;
        case TYPE_CODE_POINTER:
            *dest = src->pointer.next;
            return 0;
    }
}

int type_append(Type *dest, Type *src) {
    if (dest == NULL || src == NULL) return -1;
    Type *current = dest;
    while ((type_is_array(current) && current->array.next != NULL)
            || (type_is_pointer(current) && current->pointer.next != NULL)
            || (type_is_function(current) && current->function.next != NULL)) {
        switch (current->any.code) {
            case TYPE_CODE_ARRAY:
                current = current->array.next;
                break;
            case TYPE_CODE_POINTER:
                current = current->pointer.next;
                break;
            case TYPE_CODE_FUNCTION:
                current = current->function.next;
                break;
            default:
                abort();
        }
    }

    switch (current->any.code) {
        default:
            abort();
        case TYPE_CODE_NONE:
            /* fallthrough */
        case TYPE_CODE_BASE:
            /* fallthrough */
        case TYPE_CODE_STRUCT:
            /* fallthrough */
        case TYPE_CODE_UNION:
            /* fallthrough */
        case TYPE_CODE_ENUM:
            return -1;
        case TYPE_CODE_POINTER:
            current->pointer.next = src;
            return 0;
        case TYPE_CODE_ARRAY:
            current->array.next = src;
            return 0;
        case TYPE_CODE_FUNCTION:
            current->function.next = src;
            return 0;
    }
}

int type_copy(Type **out, const Type *type) {
    if (out == NULL || type == NULL) return -1;
    Type anchor;
    anchor.pointer.code = TYPE_CODE_POINTER;
    anchor.pointer.next = NULL;
    Type **out_current = &anchor.pointer.next;
    const Type *in_current = type;

    while (in_current != NULL) {
        *out_current = malloc(sizeof(Type));
        (*out_current)->any.code = in_current->any.code;
        switch (in_current->any.code) {
            case TYPE_CODE_NONE:
                /* fallthrough */
            default:
                abort();
            case TYPE_CODE_BASE:
                (*out_current)->base.type_flags = in_current->base.type_flags;
                in_current = NULL;
                break;
            case TYPE_CODE_ENUM:
                /* fallthrough */
            case TYPE_CODE_STRUCT:
                /* fallthrough */
            case TYPE_CODE_UNION:
                (*out_current)->tag.qualifiers = in_current->tag.qualifiers;
                (*out_current)->tag.tag_name = in_current->tag.tag_name;
                (*out_current)->tag.tag_value = in_current->tag.tag_value;
                in_current = NULL;
                break;
            case TYPE_CODE_ARRAY:
                (*out_current)->array.deduce_length = in_current->array.deduce_length;
                (*out_current)->array.length = in_current->array.length;
                out_current = &((*out_current)->array.next);
                in_current = in_current->array.next;
                break;
            case TYPE_CODE_POINTER:
                (*out_current)->pointer.qualifiers = in_current->pointer.qualifiers;
                out_current = &((*out_current)->pointer.next);
                in_current = in_current->pointer.next;
                break;
            case TYPE_CODE_FUNCTION:
                (*out_current)->function.variadic = in_current->function.variadic;
                (*out_current)->function.parameters_size
                    = in_current->function.parameters_size;
                (*out_current)->function.parameters
                    = malloc(in_current->function.parameters_size
                            * sizeof(*in_current->function.parameters));
                size_t i;
                for (i = 0; i < in_current->function.parameters_size; ++i)
                    (*out_current)->function.parameters[i]
                        = in_current->function.parameters[i];
                out_current = &((*out_current)->function.next);
                in_current = in_current->function.next;
                break;
        }
    }
    *out = anchor.pointer.next;
    return 0;
}

int type_common_qualified_pointer(Type **out, const Type *type1,
        const Type *type2) {
    Type *new_pointer = malloc(sizeof(Type));
    new_pointer->pointer.code = TYPE_CODE_POINTER;
    new_pointer->pointer.qualifiers
        = type1->pointer.qualifiers | type2->pointer.qualifiers;
    new_pointer->pointer.next = type1->pointer.next;
    *out = new_pointer;
    return 0;
}
