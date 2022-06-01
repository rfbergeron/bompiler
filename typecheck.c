#include "typecheck.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "ctype.h"
#include "debug.h"
#include "err.h"
#include "lyutils.h"
#include "math.h"
#include "simplestack.h"
#include "state.h"
#include "stdint.h"
#include "string.h"
#include "symtable.h"
/* TODO(Robert): Linux-specific; replace with strtoul or similar */
#include "inttypes.h"

/* "char" is not included in any of these groups */
#define TYPESPEC_FLAGS_INTEGER                                    \
  (TYPESPEC_FLAG_INT | TYPESPEC_FLAG_SHORT | TYPESPEC_FLAG_LONG | \
   TYPESPEC_FLAG_LONG_LONG)
#define TYPESPEC_FLAGS_NON_INTEGER                                   \
  (TYPESPEC_FLAG_VOID | TYPESPEC_FLAG_STRUCT | TYPESPEC_FLAG_UNION | \
   TYPESPEC_FLAG_ENUM)
#define TYPESPEC_FLAGS_SIGNEDNESS \
  (TYPESPEC_FLAG_SIGNED | TYPESPEC_FLAG_UNSIGNED)
#define TYPESPEC_FLAGS_STORAGE_CLASS (TYPESPEC_FLAG_TYPEDEF)

/* TODO(Robert): Implement "long long" integer type. For now, the type checker
 * should report an error if "long" is specified twice.
 */
const unsigned int INCOMPATIBLE_FLAGSETS[] = {
    TYPESPEC_FLAG_INT | TYPESPEC_FLAG_CHAR |
        TYPESPEC_FLAGS_NON_INTEGER, /* int */
    TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_INTEGER |
        TYPESPEC_FLAGS_NON_INTEGER, /* char */
    TYPESPEC_FLAG_SHORT | TYPESPEC_FLAG_LONG | TYPESPEC_FLAG_LONG_LONG |
        TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_NON_INTEGER, /* short */
    TYPESPEC_FLAG_LONG | TYPESPEC_FLAG_LONG_LONG | TYPESPEC_FLAG_SHORT |
        TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_NON_INTEGER, /* long */
    TYPESPEC_FLAG_LONG | TYPESPEC_FLAG_LONG_LONG | TYPESPEC_FLAG_SHORT |
        TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_NON_INTEGER,    /* long long */
    TYPESPEC_FLAGS_SIGNEDNESS | TYPESPEC_FLAGS_NON_INTEGER, /* signed */
    TYPESPEC_FLAGS_SIGNEDNESS | TYPESPEC_FLAGS_NON_INTEGER, /* unsigned */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* void */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* struct */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* union */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* enum */
};

enum type_checker_action {
  TCHK_COMPATIBLE,
  TCHK_IMPLICIT_CAST,
  TCHK_EXPLICIT_CAST,
  TCHK_INCOMPATIBLE,
  TCHK_E_NO_FLAGS
};

typedef struct bcc_status {
  const char *string1;
  const char *string2;
  const char *string3;
  int code;
} BCCStatus;

CompilerState *state; /* back to global state... */

SymbolValue *sym_from_type(TypeSpec *type) {
  SymbolValue temp;
  /* struct members have addresses increasing in order of declarations */
  ptrdiff_t offset = (char *)&temp.type - (char *)&temp;
  return (SymbolValue *)((char *)type - offset);
}

ASTree *create_type_error(ASTree *child, int errcode) {
  ASTree *errnode = astree_init(TOK_TYPE_ERROR, child->loc, "_terr");
  TypeSpec *errtype = calloc(1, sizeof(TypeSpec));
  errtype->base = TYPE_ERROR;
  errtype->flags = errcode;
  errnode->type = errtype;
  int status = state_push_type_error(state, errtype);
  if (status) return NULL;
  return astree_adopt(errnode, 1, child);
}

ASTree *propogate_type_error(ASTree *parent, ASTree *errnode) {
  ASTree *realnode = llist_extract(&errnode->children, 0);
  return astree_adopt(errnode, 1, astree_adopt(parent, 1, realnode));
}

void create_error_symbol(SymbolValue *symval, int errcode) {}

/*
 * TODO(Robert): recursively setting the block number no longer works because
 * of nested scoping; instead block numbers should be set during the validation
 * of expressions, at which point it is not possible to further nest scopes.
 */

ASTree *perform_pointer_conv(ASTree *expr) {
  const TypeSpec *spec = expr->type;
  if (!typespec_is_array(spec) && !typespec_is_function(spec)) {
    return expr;
  } else {
    TypeSpec *pointer_spec = malloc(sizeof(*pointer_spec));
    if (typespec_is_array(spec)) {
      int status = strip_aux_type(pointer_spec, spec);
      if (status) {
        free(pointer_spec);
        return create_type_error(expr, status);
      }
    } else {
      int status = typespec_copy(pointer_spec, spec);
      if (status) {
        free(pointer_spec);
        return create_type_error(expr, status);
      }
    }
    AuxSpec *pointer_aux = calloc(1, sizeof(*pointer_aux));
    pointer_aux->aux = AUX_POINTER;
    int status = llist_push_front(&pointer_spec->auxspecs, pointer_aux);
    if (status) {
      llist_destroy(&pointer_spec->auxspecs);
      free(pointer_spec);
      return create_type_error(expr, status);
    }
    ASTree *cast = astree_init(TOK_CAST, expr->loc, "_cast");
    cast->type = pointer_spec;
    cast->attributes |=
        expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
    return astree_adopt(cast, 1, expr);
  }
}

int is_const_zero(ASTree *tree) {
  if (tree->symbol != TOK_INTCON) {
    return 0;
  } else {
    return strtol(tree->lexinfo, NULL, 0) == 0;
  }
}

int types_compatible(const TypeSpec *type1, const TypeSpec *type2);

ASTree *convert_type(ASTree *expr, const TypeSpec *type) {
  int compatibility = types_compatible(type, expr->type);
  if (compatibility == TCHK_COMPATIBLE) {
    return expr;
  } else if (compatibility == TCHK_INCOMPATIBLE) {
    return create_type_error(expr, BCC_TERR_INCOMPATIBLE_TYPES);
  }
  TypeSpec *cast_spec = malloc(sizeof(*cast_spec));
  int status = typespec_copy(cast_spec, type);
  if (status) {
    free(cast_spec);
    return create_type_error(expr, status);
  }
  ASTree *cast = astree_init(TOK_CAST, expr->loc, "_cast");
  cast->type = cast_spec;
  cast->attributes |=
      expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
  return expr;
}

int compare_params(LinkedList *dests, LinkedList *srcs) {
  size_t list_count = llist_size(dests) > llist_size(srcs) ? llist_size(dests)
                                                           : llist_size(srcs);
  size_t i;
  for (i = 0; i < list_count; ++i) {
    SymbolValue *dest = llist_get(dests, i);
    SymbolValue *src = llist_get(srcs, i);
    if (dest == NULL || src == NULL) {
      return TCHK_EXPLICIT_CAST;
    } else {
      int symbol_compat = types_compatible(&dest->type, &src->type);
      if (symbol_compat == TCHK_COMPATIBLE) continue;
      return TCHK_EXPLICIT_CAST;
    }
  }
  return TCHK_COMPATIBLE;
}

int compare_members(LinkedList *dests, LinkedList *srcs) {
  size_t max_len = dests->size > srcs->size ? dests->size : srcs->size;
  size_t i;
  for (i = 0; i < max_len; ++i) {
    SymbolValue *dest = llist_get(dests, i);
    SymbolValue *src = llist_get(srcs, i);

    if (dest == NULL || src == NULL) {
      return TCHK_EXPLICIT_CAST;
    } else {
      int ret = types_compatible(&dest->type, &src->type);
      if (ret != TCHK_COMPATIBLE) return ret;
    }
  }
  return TCHK_COMPATIBLE;
}

int compare_declspecs(const TypeSpec *dest, const TypeSpec *src) {
  /* TODO(Robert): check qualifiers */
  if (typespec_is_integer(dest) && typespec_is_integer(src)) {
    return TCHK_COMPATIBLE;
  } else if ((dest->base == TYPE_STRUCT && src->base == TYPE_STRUCT) ||
             (dest->base == TYPE_UNION && src->base == TYPE_UNION) ||
             (dest->base == TYPE_ENUM && src->base == TYPE_ENUM)) {
    return TCHK_COMPATIBLE;
  } else {
    return TCHK_INCOMPATIBLE;
  }
}

int compare_auxspecs(const LinkedList *dests, const LinkedList *srcs) {
  size_t aux_count = llist_size(dests) > llist_size(srcs) ? llist_size(dests)
                                                          : llist_size(srcs);
  size_t i;
  for (i = 0; i < aux_count; ++i) {
    AuxSpec *dest = llist_get(srcs, i);
    AuxSpec *src = llist_get(srcs, i);

    if (dest == NULL || src == NULL) {
      return TCHK_INCOMPATIBLE;
    } else if (dest->aux == AUX_POINTER && src->aux == AUX_POINTER) {
      continue;
    } else if ((dest->aux == AUX_FUNCTION && src->aux == AUX_FUNCTION)) {
      int ret = compare_params(dest->data.params, src->data.params);
      if (ret != TCHK_COMPATIBLE) return ret;
    } else if ((dest->aux == AUX_STRUCT && src->aux == AUX_STRUCT) ||
               (dest->aux == AUX_UNION && src->aux == AUX_UNION) ||
               (dest->aux == AUX_ENUM && src->aux == AUX_ENUM)) {
      int tags_different = strcmp(dest->data.tag.name, src->data.tag.name);
      if (tags_different) return TCHK_EXPLICIT_CAST;
    } else {
      return TCHK_EXPLICIT_CAST;
    }
  }

  return TCHK_COMPATIBLE;
}

/* This function determines compatibility in situations where there is a
 * distinct 'destination' and 'source' type. It answers the question "does X
 * need to be converted to Y, and if so, does that need to be done explicitly".
 *
 * It is used to determine conversions after promotions, casts, assignments, and
 * function calls/definitions/declarations.
 */
int types_compatible(const TypeSpec *type1, const TypeSpec *type2) {
  int action = TCHK_COMPATIBLE;

  const LinkedList *auxspecs1 = &type1->auxspecs;
  const LinkedList *auxspecs2 = &type2->auxspecs;
  /* special cases */
  if (typespec_is_pointer(type1) && typespec_is_pointer(type2) &&
      (typespec_is_voidptr(type1) || typespec_is_voidptr(type2))) {
    /* pointer to/from void pointer */
    return TCHK_COMPATIBLE;
  } else if ((typespec_is_pointer(type1) || typespec_is_enum(type1)) &&
             typespec_is_integer(type2) && llist_size(auxspecs2) == 0) {
    /* int to pointer or enum */
    return TCHK_IMPLICIT_CAST;
  } else if ((typespec_is_pointer(type2) || typespec_is_enum(type2)) &&
             typespec_is_integer(type1) && llist_size(auxspecs1) == 0) {
    /* pointer or enum to int */
    return TCHK_IMPLICIT_CAST;
  }

  int ret = compare_auxspecs(auxspecs1, auxspecs2);
  if (ret != TCHK_COMPATIBLE) return ret;

  return compare_declspecs(type1, type2);
}

/*
 * integer types, in order of priority, are as follows:
 * NOTE: long long won't be supported for the moment; words in parentheses are
 * optional and may be omitted; the signedness of char is implementation-defined
 * and here I have chosen that it is signed by default
 *
 * unsigned long (int)
 * (signed) long (int)
 * unsigned (int)
 * signed, int, singed int
 * unsigned short (int)
 * (signed) short (int)
 * unsigned char
 * (signed) char
 */
int determine_conversion(const TypeSpec *type1, const TypeSpec *type2,
                         const TypeSpec **out) {
  if (typespec_is_pointer(type1)) {
    *out = type1;
  } else if (typespec_is_pointer(type2)) {
    *out = type2;
  } else if (type1->base == TYPE_STRUCT || type1->base == TYPE_UNION) {
    *out = type1;
  } else if (type2->base == TYPE_STRUCT || type2->base == TYPE_UNION) {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  } else if (type1->width < X64_SIZEOF_INT && type2->width < X64_SIZEOF_INT) {
    /* promote to signed int if both operands could be represented as one */
    *out = &SPEC_INT;
  } else if (type1->width > type2->width) {
    /* promote to wider type; disregard signedness of type2 */
    *out = type1;
  } else if (type1->width < type2->width) {
    /* promote to wider type; disregard signedness of type1 */
    *out = type2;
  } else if (type1->base == TYPE_UNSIGNED) {
    /* same width, but type1 is unsigned so prefer it */
    *out = type1;
  } else if (type2->base == TYPE_UNSIGNED) {
    /* same width, but type2 is unsigned so prefer it */
    *out = type2;
  } else if (type1->base == TYPE_SIGNED && type2->base == TYPE_SIGNED) {
    /* both are signed integers, so just pick the left-hand type */
    *out = type1;
  } else {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  }
  return BCC_TERR_SUCCESS;
}

int validate_intcon(ASTree *intcon) {
  DEBUGS('t', "Validating integer constant %s", intcon->lexinfo);
  int status = 0;
  long signed_value = strtol(intcon->lexinfo, NULL, 10);
  /* TODO(Robert): I should define the compiler's own size of min and max
   * values; this would be necessary for cross-compilation from an
   * architecture with different sizes of integer types.
   */
  if (signed_value == LONG_MIN) {
    return BCC_TERR_CONST_TOO_SMALL;
  } else if (signed_value == LONG_MAX) {
    unsigned long unsigned_value = strtoul(intcon->lexinfo, NULL, 10);
    if (unsigned_value == UINT64_MAX) {
      return BCC_TERR_CONST_TOO_LARGE;
    } else {
      intcon->type = &SPEC_ULONG;
    }
  } else if (signed_value > INT8_MIN && signed_value < INT8_MAX) {
    intcon->type = &SPEC_SCHAR;
  } else if (signed_value > INT16_MIN && signed_value < INT16_MAX) {
    intcon->type = &SPEC_SHRT;
  } else if (signed_value > INT32_MIN && signed_value < INT32_MAX) {
    intcon->type = &SPEC_INT;
  } else {
    intcon->type = &SPEC_LONG;
  }

  intcon->attributes |= ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST;
  return status;
}

int validate_charcon(ASTree *charcon) {
  const char *const_str = charcon->lexinfo + 1;
  size_t const_str_len = strlen(const_str) - 1;
  /* TODO(Robert): validate constant information (during assembly generation?)
   */
  if (const_str[0] == '\\') {
    if (const_str[1] == 'x') {
      /* hex number */
    } else if (isalpha(const_str[1])) {
      /* ASCII control sequence */
    } else if (isdigit(const_str[1])) {
      /* octal number */
    } else {
      /* \?, \", \', or \\ */
    }
  } else {
  }
  charcon->type = &SPEC_CHAR;
  charcon->attributes |= ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST;
  return BCC_TERR_SUCCESS;
}

int validate_stringcon(ASTree *stringcon) {
  TypeSpec *stringcon_type = malloc(sizeof(*stringcon->type));
  *stringcon_type = SPEC_CHAR;
  int status = typespec_init(stringcon_type);
  if (status) return status;

  AuxSpec *array_aux = calloc(1, sizeof(*array_aux));
  array_aux->aux = AUX_ARRAY;
  /* Normally, we would subtract 2 to omit the starting and ending doublequote,
   * but since strlen does not include the terminating null byte, we only
   * subtract one.
   */
  array_aux->data.memory_loc.length = strlen(stringcon->lexinfo) - 1;

  stringcon->type = stringcon_type;
  stringcon->attributes |= ATTR_EXPR_CONST;
  return BCC_TERR_SUCCESS;
}

int validate_integer_typespec(TypeSpec *out, enum typespec_index i,
                              enum typespec_flag f, size_t bytes) {
  if (out->flags & INCOMPATIBLE_FLAGSETS[i]) {
    return BCC_TERR_INCOMPATIBLE_SPEC;
  } else {
    out->flags |= f;
    if (bytes > 0) {
      out->width = bytes;
      out->alignment = bytes;
    } else if (f == TYPESPEC_FLAG_SIGNED) {
      out->base = TYPE_SIGNED;
    } else {
      out->base = TYPE_UNSIGNED;
    }
    return BCC_TERR_SUCCESS;
  }
}

BaseType type_from_tag(TagType tag) {
  switch (tag) {
    case TAG_STRUCT:
      return TYPE_STRUCT;
    case TAG_UNION:
      return TYPE_UNION;
    case TAG_ENUM:
      return TYPE_ENUM;
    default:
      return -1;
  }
}

AuxType aux_from_tag(TagType tag) {
  switch (tag) {
    case TAG_STRUCT:
      return AUX_STRUCT;
    case TAG_UNION:
      return AUX_UNION;
    case TAG_ENUM:
      return AUX_ENUM;
    default:
      return -1;
  }
}

/* tags will need two passes to define their members: the first for inserting
 * the symbols into the symbol table, and the second to swipe the symbols from
 * the members and put them into an auxspec
 */
int validate_tag_typespec(ASTree *type, TypeSpec *out) {
  /* TODO(Robert): get unique tag name/info somehow */
  const char *tag_name = astree_get(type, 0)->lexinfo;
  size_t tag_name_len = strlen(tag_name);
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, tag_name_len, &tagval);
  if (tagval == NULL) {
    return BCC_TERR_TAG_NOT_FOUND;
  }
  out->base = type_from_tag(tagval->tag);
  out->width = tagval->width;
  out->alignment = tagval->alignment;

  if (out->auxspecs.anchor == NULL) {
    typespec_init(out);
  }

  AuxSpec *tag_aux = calloc(1, sizeof(*tag_aux));
  tag_aux->aux = aux_from_tag(tagval->tag);
  tag_aux->data.tag.name = tag_name;
  tag_aux->data.tag.val = tagval;
  llist_push_back(&out->auxspecs, tag_aux);
  return BCC_TERR_SUCCESS;
}

int validate_typedef_typespec(TypeSpec *out) {
  if (out->flags & TYPESPEC_FLAGS_STORAGE_CLASS) {
    return BCC_TERR_INCOMPATIBLE_SPEC;
  } else {
    out->flags |= TYPESPEC_FLAG_TYPEDEF;
    return BCC_TERR_SUCCESS;
  }
}

int validate_type_id_typespec(ASTree *type, TypeSpec *out) {
  const char *type_name = type->lexinfo;
  size_t type_name_len = strlen(type_name);
  SymbolValue *symval = NULL;
  state_get_symbol(state, type_name, type_name_len, &symval);
  if (!symval) {
    return BCC_TERR_TYPEID_NOT_FOUND;
  } else if ((symval->type.flags & TYPESPEC_FLAG_TYPEDEF) == 0) {
    return BCC_TERR_EXPECTED_TYPEID;
  } else {
    out->base = symval->type.base;
    out->width = symval->type.width;
    out->alignment = symval->type.alignment;
    out->flags = symval->type.flags;

    if (out->auxspecs.anchor == NULL) {
      typespec_init(out);
    }

    int status = typespec_append_auxspecs(out, &symval->type);
    if (status) return status;
    return BCC_TERR_SUCCESS;
  }
}

ASTree *validate_typespec(ASTree *spec_list, ASTree *type) {
  if (spec_list->symbol == TOK_TYPE_ERROR) {
    ASTree *real_spec_list = astree_get(spec_list, 0);
    astree_adopt(real_spec_list, 1, type);
    return spec_list;
  }

  if (spec_list->type == NULL) {
    spec_list->type = calloc(1, sizeof(TypeSpec));
  }

  TypeSpec *out = (TypeSpec *)spec_list->type;
  int status = BCC_TERR_SUCCESS;
  switch (type->symbol) {
    case TOK_VOID:
      status = out->flags & INCOMPATIBLE_FLAGSETS[TYPESPEC_INDEX_VOID];
      out->base = TYPE_VOID;
      out->flags |= TYPESPEC_FLAG_VOID;
      break;
    case TOK_INT:
      status = validate_integer_typespec(out, TYPESPEC_INDEX_INT,
                                         TYPESPEC_FLAG_INT, X64_SIZEOF_INT);
      break;
    case TOK_LONG:
      status = validate_integer_typespec(out, TYPESPEC_INDEX_LONG,
                                         TYPESPEC_FLAG_LONG, X64_SIZEOF_LONG);
      break;
    case TOK_SHORT:
      status = validate_integer_typespec(out, TYPESPEC_INDEX_SHORT,
                                         TYPESPEC_FLAG_SHORT, X64_SIZEOF_SHORT);
      break;
    case TOK_CHAR:
      status = validate_integer_typespec(out, TYPESPEC_INDEX_CHAR,
                                         TYPESPEC_FLAG_CHAR, X64_SIZEOF_CHAR);
      break;
    case TOK_SIGNED:
      status = validate_integer_typespec(out, TYPESPEC_INDEX_SIGNED,
                                         TYPESPEC_FLAG_SIGNED, 0);
      break;
    case TOK_UNSIGNED:
      status = validate_integer_typespec(out, TYPESPEC_INDEX_UNSIGNED,
                                         TYPESPEC_FLAG_UNSIGNED, 0);
      break;
    case TOK_UNION:
    case TOK_STRUCT:
    case TOK_ENUM:
      status = validate_tag_typespec(type, out);
      break;
    case TOK_CONST:
      break;
    case TOK_VOLATILE:
      break;
    case TOK_IDENT:
      status = validate_type_id_typespec(type, out);
      break;
    case TOK_TYPEDEF:
      status = validate_typedef_typespec(out);
      break;
    default:
      status = BCC_TERR_FAILURE;
      break;
  }

  if (status != BCC_TERR_SUCCESS) {
    return create_type_error(astree_adopt(spec_list, 1, type), status);
  } else {
    return astree_adopt(spec_list, 1, type);
  }
}

ASTree *validate_typespec_list(ASTree *spec_list) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if ((out->flags & (TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAG_CHAR)) &&
      !(out->flags & TYPESPEC_FLAGS_SIGNEDNESS)) {
    /* default to signed if not specified */
    out->base = TYPE_SIGNED;
    return spec_list;
  } else if ((out->flags & TYPESPEC_FLAGS_SIGNEDNESS) &&
             !(out->flags & (TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAG_CHAR))) {
    /* default to width of "int" if not specified */
    out->width = X64_SIZEOF_INT;
    out->alignment = X64_ALIGNOF_INT;
    return spec_list;
  } else if (out->base == TYPE_NONE) {
    return create_type_error(spec_list, BCC_TERR_INCOMPLETE_TYPE);
  } else {
    return spec_list;
  }
}

ASTree *validate_ident(ASTree *ident) {
  DEBUGS('t', "Attempting to assign a type");
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strnlen(id_str, MAX_IDENT_LEN);
  SymbolValue *symval = NULL;
  int in_current_scope = state_get_symbol(state, id_str, id_str_len, &symval);
  if (symval) {
    DEBUGS('t', "Assigning %s a symbol", id_str);
    ident->type = &(symval->type);
    return ident;
  } else {
    return create_type_error(ident, BCC_TERR_SYM_NOT_FOUND);
  }
}

ASTree *finalize_call(ASTree *call) {
  if (call->symbol == TOK_TYPE_ERROR) {
    return call;
  }
  ASTree *function = astree_get(call, 0);
  TypeSpec *function_spec = (TypeSpec *)function->type;
  /* second auxspec will be the function; first is pointer */
  AuxSpec *param_spec = llist_get(&function_spec->auxspecs, 1);
  LinkedList *param_list = param_spec->data.params;
  /* subtract one since function expression is also a child */
  if (astree_count(call) - 1 > llist_size(param_list)) {
    return create_type_error(call, BCC_TERR_INSUFF_PARAMS);
  }
  return call;
}

ASTree *validate_arg(ASTree *call, ASTree *arg) {
  if (call->symbol == TOK_TYPE_ERROR) {
    ASTree *real_call = astree_get(call, 0);
    astree_adopt(real_call, 1, arg);
    return call;
  } else if (arg->symbol == TOK_TYPE_ERROR) {
    return propogate_type_error(call, arg);
  }
  /* functon subtree is the first child of the call node */
  ASTree *function = astree_get(call, 0);
  TypeSpec *function_spec = (TypeSpec *)function->type;
  /* second auxspec will be the function; first is pointer */
  AuxSpec *param_spec = llist_get(&function_spec->auxspecs, 1);
  LinkedList *param_list = param_spec->data.params;
  /* subtract one since function expression is also a child */
  size_t param_index = astree_count(call) - 1;
  if (param_index >= llist_size(param_list)) {
    return create_type_error(astree_adopt(call, 1, arg),
                             BCC_TERR_EXCESS_PARAMS);
  }
  DEBUGS('t', "Validating argument %d", param_index);
  SymbolValue *symval = llist_get(param_list, param_index);
  DEBUGS('t', "Comparing types");
  int compatibility = types_compatible(&symval->type, arg->type);
  if (compatibility == TCHK_INCOMPATIBLE ||
      compatibility == TCHK_EXPLICIT_CAST) {
    return create_type_error(astree_adopt(call, 1, arg),
                             BCC_TERR_INCOMPATIBLE_TYPES);
  } else {
    return astree_adopt(call, 1, arg);
  }
}

ASTree *validate_call(ASTree *call, ASTree *function) {
  function = perform_pointer_conv(function);
  if (function->symbol == TOK_TYPE_ERROR) {
    return propogate_type_error(call, function);
  }
  TypeSpec *function_spec = (TypeSpec *)function->type;
  if (!typespec_is_fnptr(function_spec)) {
    return create_type_error(astree_adopt(call, 1, function),
                             BCC_TERR_EXPECTED_FUNCTION);
  }

  /* strip pointer */
  TypeSpec temp_spec = SPEC_EMPTY;
  int status = strip_aux_type(&temp_spec, function_spec);
  if (status) {
    return create_type_error(astree_adopt(call, 1, function), status);
  }
  /* strip function */
  TypeSpec *return_spec = malloc(sizeof(*return_spec));
  status = strip_aux_type(return_spec, &temp_spec);
  if (status) {
    return create_type_error(astree_adopt(call, 1, function), status);
  }
  /* free temporaries created by stripping */
  typespec_destroy(&temp_spec);
  call->type = return_spec;
  return astree_adopt(call, 1, function);
}

ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
  }

  if (!typespec_is_scalar(condition->type)) {
    return create_type_error(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_EXPECTED_SCALAR);
  }

  true_expr = perform_pointer_conv(true_expr);
  if (true_expr->symbol == TOK_TYPE_ERROR) {
  }

  false_expr = perform_pointer_conv(false_expr);
  if (false_expr->symbol == TOK_TYPE_ERROR) {
  }

  /* TODO(Robert): the rules for conversion on the output of the ternary
   * operator are different from usual conversions and compatibility rules, and
   * should have their own function
   */
  int status =
      determine_conversion(true_expr->type, false_expr->type, &qmark->type);
  if (status) {
  }

  return astree_adopt(qmark, 3, condition, true_expr, false_expr);
}

ASTree *validate_comma(ASTree *comma, ASTree *left_expr, ASTree *right_expr) {
  right_expr = perform_pointer_conv(right_expr);
  if (right_expr->symbol == TOK_TYPE_ERROR) {
  }
  comma->attributes |=
      right_expr->attributes & (ATTR_EXPR_ARITHCONST | ATTR_EXPR_CONST);

  comma->type = right_expr->type;
  return astree_adopt(comma, 2, left_expr, right_expr);
}

ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src) {
  src = perform_pointer_conv(src);
  if (src->symbol == TOK_TYPE_ERROR) {
  };
  src = convert_type(src, dest->type);
  if (src->symbol == TOK_TYPE_ERROR) {
  };
  assignment->attributes |=
      src->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);

  assignment->type = dest->type;
  return astree_adopt(assignment, 2, dest, src);
}

int spec_list_includes_type(ASTree *spec_list) {
  size_t i;
  for (i = 0; i < astree_count(spec_list); ++i) {
    ASTree *decl_spec = astree_get(spec_list, i);
    switch (decl_spec->symbol) {
      case TOK_INT:
      case TOK_SHORT:
      case TOK_LONG:
      case TOK_CHAR:
      case TOK_STRUCT:
      case TOK_UNION:
      case TOK_ENUM:
      case TOK_SIGNED:
      case TOK_UNSIGNED:
      case TOK_VOID:
        return 1;
      default:
        continue;
    }
  }
  return 0;
}

ASTree *validate_declaration(ASTree *declaration, ASTree *declarator) {
  DEBUGS('t', "Making object entry for value %s", declarator->lexinfo);
  if (declaration->symbol == TOK_TYPE_ERROR ||
      declarator->symbol == TOK_TYPE_ERROR) {
    return declarator;
  }
  ASTree *spec_list = astree_get(declaration, 0);
  const TypeSpec *decl_specs = spec_list->type;
  SymbolValue *symbol = sym_from_type((TypeSpec *)declarator->type);
  symbol->type.base = decl_specs->base;
  symbol->type.alignment = decl_specs->alignment;
  symbol->type.width = decl_specs->width;
  symbol->type.flags = decl_specs->flags;
  int status = typespec_append_auxspecs(&symbol->type, (TypeSpec *)decl_specs);
  if (status) {
    return create_type_error(declarator, status);
  }

  const char *identifier = declarator->lexinfo;
  size_t identifier_len = strnlen(identifier, MAX_IDENT_LEN);
  SymbolValue *exists = NULL;
  int is_redefinition =
      state_get_symbol(state, identifier, identifier_len, &exists);
  if (!is_redefinition) {
    if (exists && (exists->type.flags & TYPESPEC_FLAG_TYPEDEF) &&
        !spec_list_includes_type(astree_get(declaration, 0))) {
      /* don't redeclare typedefs if type was not specified in declaration */
      symbol_value_destroy(symbol);
      declarator->type = &exists->type;
      return declarator;
    } else if (declarator->symbol == TOK_TYPE_NAME) {
      /* type name in cast or sizeof; do not insert symbol */
      /* TODO(Robert): copy and free the information in the symbol we just
       * created so that the syntax tree node can free it later, or use fancy
       * pointer math to get back the address in the heap of the SymbolValue
       * that the type information is embedded in.
       */
      return declarator;
    } else {
      /* TODO(Robert): check for incomplete type */
      int status =
          state_insert_symbol(state, identifier, identifier_len, symbol);
      if (status) {
        symbol_value_destroy(symbol);
        declarator->type = NULL;
        return create_type_error(declarator, BCC_TERR_LIBRARY_FAILURE);
      }
      declarator->type = &symbol->type;
      return declarator;
    }
  } else if ((typespec_is_function(&exists->type) &&
              typespec_is_function(&symbol->type)) ||
             ((exists->type.flags & TYPESPEC_FLAG_TYPEDEF) &&
              (symbol->type.flags & TYPESPEC_FLAG_TYPEDEF))) {
    int compatibility = types_compatible(&exists->type, &symbol->type);
    symbol_value_destroy(symbol);
    declarator->type = NULL;
    if (compatibility != TCHK_COMPATIBLE) {
      return create_type_error(declarator, BCC_TERR_REDEFINITION);
    } else {
      return declarator;
    }
  } else {
    /* TODO(Robert): allow redefinition of extern symbols so long as types
     * are compatible
     */
    symbol_value_destroy(symbol);
    declarator->type = NULL;
    return create_type_error(declarator, BCC_TERR_REDEFINITION);
  }
}

ASTree *finalize_declaration(ASTree *declaration) {
  if (declaration->symbol == TOK_TYPE_ERROR)
    declaration = astree_get(declaration, 0);
  ASTree *spec_list = astree_get(declaration, 0);
  if (spec_list->type != NULL) {
    free((TypeSpec *)spec_list->type);
    spec_list->type = NULL;
  }
  return declaration;
}

ASTree *validate_array(ASTree *array, ASTree *expr) {
  int status = validate_expr(expr);
  if (status) {
    return create_type_error(astree_adopt(array, 1, expr), status);
  }
  if ((expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST)) == 0) {
    return create_type_error(astree_adopt(array, 1, expr),
                             BCC_TERR_EXPECTED_INTCONST);
  }
  return astree_adopt(array, 1, expr);
}

ASTree *validate_param_list(ASTree *param_list) {
  SymbolTable *param_table = symbol_table_init();
  param_list->symbol_table = param_table;
  int status = state_push_table(state, param_table);
  if (status)
    return create_type_error(param_list, status);
  else
    return param_list;
}

ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator) {
  if (param_list->symbol == TOK_TYPE_ERROR) {
    ASTree *real_param_list = astree_get(param_list, 0);
    astree_adopt(real_param_list, 1, astree_adopt(declaration, 1, declarator));
    return param_list;
  } else if (declaration->symbol == TOK_TYPE_ERROR) {
    ASTree *real_declaration = astree_get(declaration, 0);
    astree_adopt(real_declaration, 1, declarator);
    return propogate_type_error(param_list, declaration);
  } else if (declarator->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(declaration, declarator);
    return propogate_type_error(param_list, errnode);
  }

  ASTree *err_or_declaration = declare_symbol(declaration, declarator);
  if (err_or_declaration->symbol == TOK_TYPE_ERROR) {
    return propogate_type_error(param_list, err_or_declaration);
  }

  return astree_adopt(param_list, 1, astree_adopt(declaration, 1, declarator));
}

ASTree *finalize_param_list(ASTree *param_list) {
  int status = state_pop_table(state);
  if (status)
    return create_type_error(param_list, status);
  else
    return param_list;
}

ASTree *define_params(ASTree *declarator, ASTree *param_list) {
  AuxSpec *aux_function = calloc(1, sizeof(*aux_function));
  aux_function->aux = AUX_FUNCTION;
  aux_function->data.params = malloc(sizeof(*aux_function->data.params));
  /* type is not resposible for freeing symbol information */
  llist_init(aux_function->data.params, NULL, NULL);
  LinkedList *param_entries = aux_function->data.params;
  size_t i;
  for (i = 0; i < astree_count(param_list); ++i) {
    ASTree *declaration = astree_get(param_list, i);
    ASTree *declarator = astree_get(declaration, 1);
    SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
    int status = llist_push_back(param_entries, symval);
    if (status) {
      return create_type_error(astree_adopt(declarator, 1, param_list), status);
    }
  }
  TypeSpec *out = (TypeSpec *)declarator->type;
  int status = llist_push_back(&out->auxspecs, aux_function);
  if (status) {
    return create_type_error(astree_adopt(declarator, 1, param_list),
                             BCC_TERR_LIBRARY_FAILURE);
  }
  return astree_adopt(declarator, 1, param_list);
}

ASTree *define_array(ASTree *declarator, ASTree *array) {
  TypeSpec *spec = (TypeSpec *)declarator->type;
  if (typespec_is_incomplete(spec)) {
    return create_type_error(astree_adopt(declarator, 1, array),
                             BCC_TERR_INCOMPLETE_TYPE);
  }
  AuxSpec *aux_array = calloc(1, sizeof(*aux_array));
  aux_array->aux = AUX_ARRAY;
  /* TODO(Robert): evaluate array size during three address code generation */
  /* set array size to any nonzero value, for now */
  aux_array->data.memory_loc.length = -1;
  int status = llist_push_back(&spec->auxspecs, aux_array);
  if (status) {
    free(aux_array);
    return create_type_error(astree_adopt(declarator, 1, array), status);
  } else {
    return astree_adopt(declarator, 1, array);
  }
}

ASTree *define_pointer(ASTree *declarator, ASTree *pointer) {
  AuxSpec *aux_pointer = calloc(1, sizeof(*aux_pointer));
  aux_pointer->aux = AUX_POINTER;
  TypeSpec *spec = (TypeSpec *)declarator->type;
  int status = llist_push_back(&spec->auxspecs, aux_pointer);
  if (status) {
    free(aux_pointer);
    return create_type_error(astree_adopt(declarator, 1, pointer),
                             BCC_TERR_LIBRARY_FAILURE);
  } else {
    return astree_adopt(declarator, 1, pointer);
  }
}

ASTree *define_dirdecl(ASTree *declarator, ASTree *dirdecl) {
  /* TODO(Robert): do not allow multiple function dirdecls to occur, and do not
   * allow functions to return array types
   */
  /* TODO(Robert): possibly add external function to check validity of linked
   * list and other badlib data structures
   * TODO(Robert): initialize auxspec list in a more predictable and centralized
   * way, and rename or repurpose typespec_init to make it more clear what it
   * does
   */
  if (declarator->symbol == TOK_TYPE_ERROR) {
    ASTree *real_declarator = astree_get(declarator, 0);
    astree_adopt(real_declarator, 1, dirdecl);
    return declarator;
  }
  TypeSpec *out = (TypeSpec *)declarator->type;
  if (out->auxspecs.anchor == NULL) {
    typespec_init(out);
  }

  switch (dirdecl->symbol) {
    case TOK_ARRAY:
      return define_array(declarator, dirdecl);
    case TOK_PARAM_LIST:
      return define_params(declarator, dirdecl);
    case TOK_TYPE_ERROR:
      return propogate_type_error(declarator, dirdecl);
    default:
      return create_type_error(astree_adopt(declarator, 1, dirdecl),
                               BCC_TERR_UNEXPECTED_TOKEN);
  }
}

ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr) {
  expr = perform_pointer_conv(expr);
  if (expr->symbol == TOK_TYPE_ERROR) {
  }

  ASTree *type_name = astree_get(declaration, 1);
  int compatibility = types_compatible(type_name->type, expr->type);
  if (compatibility == TCHK_INCOMPATIBLE) {
    return create_type_error(astree_adopt(cast, 2, declaration, expr),
                             BCC_TERR_INCOMPATIBLE_TYPES);
  } else {
    cast->type = type_name->type;
    cast->attributes |=
        expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
    return astree_adopt(cast, 2, declaration, expr);
  }
}

ASTree *typecheck_addop(ASTree *operator, ASTree * left, ASTree *right) {
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    int status = determine_conversion(left_type, right_type, &operator->type);
    if (status) {
      return create_type_error(astree_adopt(operator, 2, left, right), status);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = propogate_type_error(operator, left);
      astree_adopt(operator, 1, right);
      return errnode;
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return propogate_type_error(astree_adopt(operator, 1, left), right);
    }
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_integer(right_type)) {
    operator->type = left_type;
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_integer(left_type) &&
             typespec_is_pointer(right_type)) {
    operator->type = right_type;
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type) && operator->symbol == '-') {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE) {
      /* types should be the same; just pick the left one */
      operator->type = left_type;
      return astree_adopt(operator, 2, left, right);
    } else {
      return create_type_error(astree_adopt(operator, 2, left, right),
                               BCC_TERR_INCOMPATIBLE_TYPES);
    }
  } else {
    return create_type_error(astree_adopt(operator, 2, left, right),
                             BCC_TERR_INCOMPATIBLE_TYPES);
  }
}

ASTree *typecheck_logop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_scalar(left->type) && typespec_is_scalar(right->type)) {
    operator->type = & SPEC_INT;
    return astree_adopt(operator, 2, left, right);
  } else {
    return create_type_error(astree_adopt(operator, 2, left, right),
                             BCC_TERR_EXPECTED_SCALAR);
  }
}

ASTree *typecheck_relop(ASTree *operator, ASTree * left, ASTree *right) {
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;
  operator->type = & SPEC_INT;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    const TypeSpec *common_type;
    int status = determine_conversion(left_type, right_type, &common_type);
    if (status) {
      return create_type_error(astree_adopt(operator, 2, left, right), status);
    }
    left = convert_type(left, common_type);
    if (left->symbol == TOK_TYPE_ERROR) {
    }
    right = convert_type(right, common_type);
    if (right->symbol == TOK_TYPE_ERROR) {
    }
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type)) {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE || typespec_is_voidptr(left_type) ||
        typespec_is_voidptr(right_type)) {
      return astree_adopt(operator, 2, left, right);
    } else {
      return create_type_error(astree_adopt(operator, 2, left, right),
                               BCC_TERR_INCOMPATIBLE_TYPES);
    }
  } else if (((typespec_is_pointer(left_type) && is_const_zero(right)) ||
              (is_const_zero(left) && typespec_is_pointer(right_type))) &&
             (operator->symbol == TOK_EQ || operator->symbol == TOK_NE)) {
    return astree_adopt(operator, 2, left, right);
  } else {
    return create_type_error(astree_adopt(operator, 2, left, right),
                             BCC_TERR_INCOMPATIBLE_TYPES);
  }
}

ASTree *typecheck_mulop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_arithmetic(left->type) &&
      typespec_is_arithmetic(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) {
      return create_type_error(astree_adopt(operator, 2, left, right), status);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return create_type_error(astree_adopt(operator, 2, left, right),
                             BCC_TERR_EXPECTED_ARITHMETIC);
  }
}

ASTree *typecheck_shfop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, &SPEC_INT, &operator->type);
    if (status) {
      return create_type_error(astree_adopt(operator, 2, left, right), status);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = propogate_type_error(operator, left);
      astree_adopt(operator, 1, right);
      return errnode;
    }
    /* promote right operand independently of left */
    const TypeSpec *dummy;
    status = determine_conversion(right->type, &SPEC_INT, &dummy);
    if (status) {
      return create_type_error(astree_adopt(operator, 2, left, right), status);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      astree_adopt(operator, 1, left);
      return propogate_type_error(operator, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return create_type_error(astree_adopt(operator, 2, left, right),
                             BCC_TERR_EXPECTED_INTEGER);
  }
}

ASTree *typecheck_bitop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) {
      return create_type_error(astree_adopt(operator, 2, left, right), status);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return create_type_error(astree_adopt(operator, 2, left, right),
                             BCC_TERR_EXPECTED_INTEGER);
  }
}

ASTree *validate_binop(ASTree *operator, ASTree * left_operand,
                       ASTree *right_operand) {
  DEBUGS('t', "Validating binary operator %c", operator->symbol);

  left_operand = perform_pointer_conv(left_operand);
  if (left_operand->symbol == TOK_TYPE_ERROR) {
  }

  right_operand = perform_pointer_conv(right_operand);
  if (right_operand->symbol == TOK_TYPE_ERROR) {
  }

  ASTree *result = NULL;
  switch (operator->symbol) {
    case TOK_SHL:
    case TOK_SHR:
      result = typecheck_shfop(operator, left_operand, right_operand);
      break;
    case '&':
    case '|':
    case '^':
      result = typecheck_bitop(operator, left_operand, right_operand);
      break;
    case '*':
    case '/':
    case '%':
      result = typecheck_mulop(operator, left_operand, right_operand);
      break;
    case '+':
    case '-':
      result = typecheck_addop(operator, left_operand, right_operand);
      break;
    case TOK_EQ:
    case TOK_NE:
    case TOK_GE:
    case TOK_LE:
    case '>':
    case '<':
      result = typecheck_relop(operator, left_operand, right_operand);
      break;
    case TOK_AND:
    case TOK_OR:
      result = typecheck_logop(operator, left_operand, right_operand);
      break;
    default:
      result = create_type_error(
          astree_adopt(operator, 2, left_operand, right_operand),
          BCC_TERR_UNEXPECTED_TOKEN);
  }

  if (result->symbol == TOK_TYPE_ERROR) {
    return result;
  }
  unsigned int result_attrs =
      left_operand->attributes & right_operand->attributes;
  result->attributes |= result_attrs & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
  return result;
}

int is_increment(const int symbol) {
  return symbol == TOK_INC || symbol == TOK_DEC || symbol == TOK_POST_INC ||
         symbol == TOK_POST_DEC;
}

ASTree *validate_unop(ASTree *operator, ASTree * operand) {
  DEBUGS('t', "Validating unary operator %c", operator->symbol);
  if (operator->symbol == TOK_TYPE_ERROR) {
    ASTree *real_operator = astree_get(operator, 0);
    astree_adopt(real_operator, 1, operand);
    return operator;
  } else if (operand->symbol == TOK_TYPE_ERROR) {
    return propogate_type_error(operator, operand);
  }
  const TypeSpec *operand_type = operand->type;

  if (is_increment(operator->symbol) && !typespec_is_scalar(operand_type)) {
    return create_type_error(astree_adopt(operator, 1, operand),
                             BCC_TERR_EXPECTED_SCALAR);
  } else if ((operator->symbol == TOK_NEG || operator->symbol == TOK_POS) &&
             !typespec_is_arithmetic(operand_type)) {
    return create_type_error(astree_adopt(operator, 1, operand),
                             BCC_TERR_EXPECTED_ARITHMETIC);
  } else if (operator->symbol == '~' && !typespec_is_integer(operand_type)) {
    return create_type_error(astree_adopt(operator, 1, operand),
                             BCC_TERR_EXPECTED_INTEGER);
  } else if (operator->symbol == '!') {
    if (typespec_is_scalar(operand_type)) {
      operator->type = & SPEC_INT;
      return astree_adopt(operator, 1, operand);
    } else {
      return create_type_error(astree_adopt(operator, 1, operand),
                               BCC_TERR_EXPECTED_SCALAR);
    }
  } else {
    if (!is_increment(operator->symbol)) {
      operand = perform_pointer_conv(operand);
      if (operand->symbol == TOK_TYPE_ERROR) {
      }
    }
    int status = determine_conversion(operand_type, &SPEC_INT, &operator->type);
    if (status)
      return create_type_error(astree_adopt(operator, 1, operand), status);
    operand = convert_type(operand, operator->type);
    if (operand->symbol == TOK_TYPE_ERROR) {
    }
    return astree_adopt(operator, 1, operand);
  }
}

ASTree *validate_indirection(ASTree *indirection, ASTree *operand) {
  operand = perform_pointer_conv(operand);
  if (operand->symbol == TOK_TYPE_ERROR) {
  }

  if (typespec_is_pointer(indirection->type)) {
    TypeSpec *indirection_spec = malloc(sizeof(*indirection_spec));
    int status = strip_aux_type(indirection_spec, operand->type);
    if (status) {
      return create_type_error(astree_adopt(indirection, 1, operand), status);
    }
    indirection->type = indirection_spec;
    return astree_adopt(indirection, 1, operand);
  } else {
    return create_type_error(astree_adopt(indirection, 1, operand),
                             BCC_TERR_EXPECTED_POINTER);
  }
}

ASTree *validate_addrof(ASTree *addrof, ASTree *operand) {
  /* TODO(Robert): check that operand is an lval */
  /* TODO(Robert): set constexpr attribute if operand is static/extern */
  TypeSpec *addrof_spec = malloc(sizeof(*addrof_spec));
  int status = typespec_copy(addrof_spec, operand->type);
  if (status) {
    return create_type_error(astree_adopt(addrof, 1, operand), status);
  }
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  llist_push_front(&addrof_spec->auxspecs, ptr_aux);
  addrof->type = addrof_spec;
  return astree_adopt(addrof, 1, operand);
}

ASTree *validate_sizeof(ASTree *sizeof_, ASTree *type_node) {
  if (type_node->symbol == TOK_TYPE_ERROR) {
    return propogate_type_error(sizeof_, type_node);
  } else if (type_node->symbol == TOK_DECLARATION) {
    type_node = astree_get(type_node, 1);
  }

  if (typespec_is_incomplete(type_node->type)) {
    return create_type_error(astree_adopt(sizeof_, 1, type_node),
                             BCC_TERR_INCOMPLETE_TYPE);
  }
  /* TODO(Robert): compute actual size and also probably make sure that this
   * is actually the correct type name for the output of sizeof on this
   * platform
   */
  sizeof_->type = &SPEC_ULONG;
  sizeof_->attributes |= (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
  return astree_adopt(sizeof_, 1, type_node);
}

ASTree *validate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  pointer = perform_pointer_conv(pointer);
  if (pointer->symbol == TOK_TYPE_ERROR) {
  }

  index = perform_pointer_conv(index);
  if (index->symbol == TOK_TYPE_ERROR) {
  }

  if (!typespec_is_pointer(pointer->type)) {
    return create_type_error(astree_adopt(subscript, 2, pointer, index),
                             BCC_TERR_EXPECTED_POINTER);
  } else if (!typespec_is_integer(index->type)) {
    return create_type_error(astree_adopt(subscript, 2, pointer, index),
                             BCC_TERR_EXPECTED_INTEGER);
  } else {
    TypeSpec *subscript_spec = malloc(sizeof(*subscript_spec));
    int status = strip_aux_type(subscript_spec, pointer->type);
    if (status)
      return create_type_error(astree_adopt(subscript, 2, pointer, index),
                               status);
    subscript->type = subscript_spec;
    return astree_adopt(subscript, 2, pointer, index);
  }
}

ASTree *validate_reference(ASTree *reference, ASTree *struct_,
                           ASTree *member_name_node) {
  const TypeSpec *struct_type = struct_->type;
  if (!typespec_is_struct(struct_type) && !typespec_is_union(struct_type)) {
    return create_type_error(
        astree_adopt(reference, 2, struct_, member_name_node),
        BCC_TERR_EXPECTED_TAG);
  }

  const char *member_name = member_name_node->lexinfo;
  const size_t member_name_len = strlen(member_name);
  AuxSpec *struct_aux = llist_front(&struct_type->auxspecs);
  SymbolTable *member_table = struct_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return create_type_error(
        astree_adopt(reference, 2, struct_, member_name_node),
        BCC_TERR_SYM_NOT_FOUND);
  } else {
    reference->type = &symval->type;
    return astree_adopt(reference, 2, struct_, member_name_node);
  }
}

ASTree *validate_arrow(ASTree *arrow, ASTree *struct_,
                       ASTree *member_name_node) {
  struct_ = perform_pointer_conv(struct_);
  if (struct_->symbol == TOK_TYPE_ERROR) {
  }

  const TypeSpec *struct_type = struct_->type;
  if (!typespec_is_structptr(struct_type) &&
      !typespec_is_unionptr(struct_type)) {
    return create_type_error(astree_adopt(arrow, 2, struct_, member_name_node),
                             BCC_TERR_EXPECTED_TAG);
  }
  const char *member_name = member_name_node->lexinfo;
  const size_t member_name_len = strlen(member_name);
  /* first auxtype is pointer; second is struct/union */
  AuxSpec *strunion_aux = llist_get(&struct_type->auxspecs, 1);
  SymbolTable *member_table = strunion_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return create_type_error(astree_adopt(arrow, 2, struct_, member_name_node),
                             BCC_TERR_SYM_NOT_FOUND);
  } else {
    arrow->type = &symval->type;
    return astree_adopt(arrow, 2, struct_, member_name_node);
  }
}

/*
ASTree *validate_initialization(ASTree *declarator, ASTree *initializer) {}
*/

/* New, new strategy: since this function (and define function, and define
 * symbol) are all routed through validate_declaration, we can have this
 * function return the declarator (or an error node), and the adoption process
 * will occur in validate_declaration.
 */

int typecheck_array_initializer(ASTree *declarator, ASTree *init_list) {
  /* TODO(Robert): evaluate array size when generating three address code */
  const TypeSpec *array_type = declarator->type;
  TypeSpec element_type = SPEC_EMPTY;
  int status = strip_aux_type(&element_type, array_type);
  if (status) return status;
  size_t i;
  for (i = 0; i < astree_count(init_list); ++i) {
    ASTree *initializer = astree_get(init_list, i);
    initializer = perform_pointer_conv(initializer);
    if (initializer->symbol == TOK_TYPE_ERROR) {
    }
    int status = convert_type(init_list, &initializer, &element_type);
    if (status) return status;
  }
  return BCC_TERR_SUCCESS;
}

int typecheck_union_initializer(ASTree *declarator, ASTree *init_list) {
  ASTree *identifier = declarator;
  AuxSpec *union_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = union_aux->data.tag.name;
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, strlen(tag_name), &tagval);
  if (tagval == NULL) {
    return BCC_TERR_TAG_NOT_FOUND;
  }

  const LinkedList *members = &tagval->data.members.in_order;
  if (astree_count(init_list) > 1) {
    return BCC_TERR_EXCESS_INITIALIZERS;
  } else {
    /* there should be one initializer of a type compatible with the type of the
     * first member of the union
     */
    ASTree *initializer = astree_get(init_list, 0);
    initializer = perform_pointer_conv(initializer);
    if (initializer->symbol == TOK_TYPE_ERROR) {
    }
    SymbolValue *member_symbol = llist_front((LinkedList *)members);
    return convert_type(init_list, &initializer, &member_symbol->type);
  }
}

int typecheck_struct_initializer(ASTree *declarator, ASTree *init_list) {
  ASTree *identifier = declarator;
  AuxSpec *struct_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = struct_aux->data.tag.name;
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, strlen(tag_name), &tagval);
  if (tagval == NULL) {
    return BCC_TERR_TAG_NOT_FOUND;
  }

  const LinkedList *members = &tagval->data.members.in_order;
  if (members->size < astree_count(init_list)) {
    return BCC_TERR_EXCESS_INITIALIZERS;
  } else {
    size_t i;
    for (i = 0; i < astree_count(init_list); ++i) {
      ASTree *initializer = astree_get(init_list, i);
      initializer = perform_pointer_conv(initializer);
      if (initializer->symbol == TOK_TYPE_ERROR) {
      }
      SymbolValue *member_symbol = llist_get((LinkedList *)members, i);
      int status = convert_type(init_list, &initializer, &member_symbol->type);
      if (status) return status;
    }
    return BCC_TERR_SUCCESS;
  }
}

ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer) {
  if (declaration->symbol == TOK_TYPE_ERROR) {
    ASTree *real_declaration = astree_get(declaration, 0);
    astree_adopt(real_declaration, 1,
                 astree_adopt(equal_sign, 2, declarator, initializer));
  }
  ASTree *err_or_decl = validate_declaration(declaration, declarator);
  if (err_or_decl->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(equal_sign, declarator);
    astree_adopt(equal_sign, 1, initializer);
    return propogate_type_error(declaration, errnode);
  } else if (initializer->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(
        astree_adopt(equal_sign, 1, declarator), initializer);
    return propogate_type_error(declaration, errnode);
  } else if (initializer->symbol == TOK_INIT_LIST) {
    const TypeSpec *decl_type = declarator->type;
    int status = BCC_TERR_SUCCESS;
    if (typespec_is_array(decl_type)) {
      status = typecheck_array_initializer(declarator, initializer);
    } else if (decl_type->base == TYPE_UNION) {
      status = typecheck_union_initializer(declarator, initializer);
    } else if (decl_type->base == TYPE_STRUCT) {
      status = typecheck_struct_initializer(declarator, initializer);
    } else {
      status = BCC_TERR_UNEXPECTED_LIST;
    }

    if (status != BCC_TERR_SUCCESS) {
      return create_type_error(
          astree_adopt(declaration, 1,
                       astree_adopt(equal_sign, 2, declarator, initializer)),
          status);
    } else {
      return astree_adopt(declaration, 1,
                          astree_adopt(equal_sign, 2, declarator, initializer));
    }
  } else {
    initializer = perform_pointer_conv(initializer);
    if (initializer->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = propogate_type_error(
          astree_adopt(equal_sign, 1, declarator), initializer);
      return propogate_type_error(declaration, errnode);
    }
    int status = convert_type(equal_sign, &initializer, declarator->type);
    if (status) {
      return create_type_error(
          astree_adopt(declaration, 1,
                       astree_adopt(equal_sign, 2, declarator, initializer)),
          status);
    }
    return astree_adopt(declaration, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  }
}

ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body) {
  /* TODO(Robert): make sure that the declaration location is that of the
   * earliest forward declaration, but that the names of the parameters are
   * from the actual definition of the function.
   */
  if (declaration->symbol == TOK_TYPE_ERROR) {
    ASTree *real_declaration = astree_get(declaration, 0);
    astree_adopt(real_declaration, 2, declarator, body);
    return declaration;
  } else if (declarator->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(declaration, declarator);
    astree_adopt(declaration, 1, body);
    return errnode;
  }
  ASTree *err_or_decl = validate_declaration(declaration, declarator);
  if (err_or_decl->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(declaration, err_or_decl);
    astree_adopt(declaration, 1, body);
    return errnode;
  }
  SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
  if (symval == NULL) {
    return create_type_error(astree_adopt(declaration, 2, declarator, body),
                             BCC_TERR_FAILURE);
  } else if (!typespec_is_function(&symval->type)) {
    return create_type_error(astree_adopt(declaration, 2, declarator, body),
                             BCC_TERR_EXPECTED_FUNCTION);
  } else if (symval->flags & SYMFLAG_FUNCTION_DEFINED) {
    return create_type_error(astree_adopt(declaration, 2, declarator, body),
                             BCC_TERR_REDEFINITION);
  }

  /* TODO(Robert): check that there are no invalid flow control statements */
  /* mark function as defined */
  symval->flags |= SYMFLAG_FUNCTION_DEFINED;
  return astree_adopt(declaration, 2, declarator, body);
}

TagType tag_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return TAG_STRUCT;
    case TOK_UNION:
      return TAG_UNION;
    case TOK_ENUM:
      return TAG_ENUM;
    default:
      return -1;
  }
}

int errcode_from_tagtype(TagType tag_type) {
  switch (tag_type) {
    case TAG_STRUCT:
      return BCC_TERR_EXPECTED_STRUCT;
    case TAG_UNION:
      return BCC_TERR_EXPECTED_UNION;
    case TAG_ENUM:
      return BCC_TERR_EXPECTED_ENUM;
    default:
      return BCC_TERR_FAILURE;
  }
}

ASTree *validate_tag_def(ASTree *tag_type_node, ASTree *tag_name_node,
                         ASTree *left_brace) {
  const char *tag_name = tag_name_node->lexinfo;
  const size_t tag_name_len = strnlen(tag_name, MAX_IDENT_LEN);
  TagValue *exists = NULL;
  int is_redefinition = state_get_tag(state, tag_name, tag_name_len, &exists);
  int tag_declares_members = left_brace != NULL;
  TagType tag_type = tag_from_symbol(tag_type_node->symbol);
  if (is_redefinition) {
    if (tag_type != exists->tag) {
      return create_type_error(
          astree_adopt(tag_type_node, 2, tag_name_node, left_brace),
          errcode_from_tagtype(exists->tag));
    } else if (tag_declares_members) {
      if (exists->is_defined) {
        return create_type_error(
            astree_adopt(tag_type_node, 2, tag_name_node, left_brace),
            BCC_TERR_REDEFINITION);
      } else {
        /* TODO(Robert): error handling */
        tag_type_node->type = calloc(1, sizeof(TypeSpec));
        int status = typespec_init((TypeSpec *)tag_type_node->type);
        AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
        tag_aux->aux = aux_from_tag(tag_type);
        tag_aux->data.tag.name = tag_name;
        tag_aux->data.tag.val = exists;
        status = llist_push_back((LinkedList *)&tag_type_node->type->auxspecs,
                                 tag_aux);
        if (tag_type != TAG_ENUM) {
          int status = state_push_table(state, exists->data.members.by_name);
        }
        return astree_adopt(tag_type_node, 2, tag_name_node, left_brace);
      }
    } else {
      return astree_adopt(tag_type_node, 1, tag_name_node);
    }
  } else if (tag_declares_members) {
    /* TODO(Robert): error handling */
    TagValue *tagval = tag_value_init(tag_type);
    int status = state_insert_tag(state, tag_name, tag_name_len, tagval);
    tag_type_node->type = calloc(1, sizeof(TypeSpec));
    status = typespec_init((TypeSpec *)tag_type_node->type);
    AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
    tag_aux->aux = aux_from_tag(tag_type);
    tag_aux->data.tag.name = tag_name;
    tag_aux->data.tag.val = tagval;
    status =
        llist_push_back((LinkedList *)&tag_type_node->type->auxspecs, tag_aux);
    if (tag_type != TAG_ENUM) {
      int status = state_push_table(state, tagval->data.members.by_name);
    }
    return astree_adopt(tag_type_node, 2, tag_name_node, left_brace);
  } else {
    return astree_adopt(tag_type_node, 1, tag_name_node);
  }
}

ASTree *finalize_tag_def(ASTree *tag) {
  ASTree *errnode = NULL;
  if (tag->symbol == TOK_TYPE_ERROR) {
    errnode = tag;
    tag = astree_get(tag, 0);
  }
  if (tag->symbol != TOK_ENUM) {
    int status = state_pop_table(state);
  }
  AuxSpec *struct_aux = llist_back(&tag->type->auxspecs);
  TagValue *tagval = struct_aux->data.tag.val;
  tagval->is_defined = 1;
  int status = typespec_destroy((TypeSpec *)tag->type);
  free((TypeSpec *)tag->type);
  if (errnode != NULL)
    return errnode;
  else
    return tag;
}

ASTree *define_enumerator(ASTree *enum_, ASTree *ident_node, ASTree *equal_sign,
                          ASTree *expr) {
  if (enum_->symbol == TOK_TYPE_ERROR) {
    ASTree *real_enum = astree_get(enum_, 0);
    ASTree *left_brace = astree_get(real_enum, 1);
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return enum_;
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return enum_;
    }
  }
  ASTree *left_brace = astree_get(enum_, 1);
  const char *ident = ident_node->lexinfo;
  const size_t ident_len = strnlen(ident, MAX_IDENT_LEN);

  SymbolValue *exists = NULL;
  int is_redefinition = state_get_symbol(state, ident, ident_len, &exists);
  if (is_redefinition) {
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return create_type_error(enum_, BCC_TERR_REDEFINITION);
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return create_type_error(enum_, BCC_TERR_REDEFINITION);
    }
  }

  SymbolValue *symval =
      symbol_value_init(&ident_node->loc, state_get_sequence(state));
  int status = typespec_init(&symval->type);
  if (status) {
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return create_type_error(enum_, status);
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return create_type_error(enum_, status);
    }
  }
  symval->type.alignment = X64_ALIGNOF_INT;
  symval->type.width = X64_SIZEOF_INT;
  symval->type.base = TYPE_ENUM;
  /* mark as enumeration consntant */
  symval->flags |= SYMFLAG_ENUM_CONST;

  AuxSpec *enum_aux = calloc(1, sizeof(*enum_aux));
  enum_aux->aux = AUX_ENUM;
  enum_aux->data.tag.name = (astree_get(enum_, 0))->lexinfo;
  llist_push_back(&symval->type.auxspecs, enum_aux);

  status = state_insert_symbol(state, ident, ident_len, symval);
  if (status) {
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return create_type_error(enum_, status);
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return create_type_error(enum_, status);
    }
  }

  ident_node->type = &symval->type;

  if (equal_sign != NULL) {
    /* TODO(Robert): evaluate enumeration constants */
    if (expr->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode =
          propogate_type_error(astree_adopt(equal_sign, 1, ident_node), expr);
      /* TODO(Robert): have error propogation function handle more complex
       * syntax tree structures
       */
      return propogate_type_error(enum_, errnode);
    }
    if ((expr->attributes & ATTR_EXPR_ARITHCONST) == 0) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return create_type_error(enum_, BCC_TERR_EXPECTED_ARITHCONST);
    }
    astree_adopt(left_brace, 1, astree_adopt(equal_sign, 2, ident_node, expr));
    return enum_;
  } else {
    astree_adopt(left_brace, 1, ident_node);
    return enum_;
  }
}

ASTree *define_struct_member(ASTree *struct_, ASTree *member) {
  if (struct_->symbol == TOK_TYPE_ERROR) {
    ASTree *real_struct = astree_get(struct_, 0);
    ASTree *left_brace = astree_get(real_struct, 1);
    astree_adopt(left_brace, 1, member);
    return struct_;
  } else if (member->symbol == TOK_TYPE_ERROR) {
    ASTree *left_brace = astree_get(struct_, 1);
    ASTree *errnode = propogate_type_error(left_brace, member);
    return propogate_type_error(struct_, errnode);
  }
  AuxSpec *struct_aux = llist_back(&struct_->type->auxspecs);
  TagValue *tagval = struct_aux->data.tag.val;
  LinkedList *member_list = &tagval->data.members.in_order;
  size_t i;
  /* skip first child, which is the typespec list */
  for (i = 1; i < astree_count(member); ++i) {
    ASTree *declarator = astree_get(member, i);
    SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
    if (tagval->alignment < symval->type.alignment) {
      tagval->alignment = symval->type.alignment;
    }
    if (tagval->tag == TAG_STRUCT) {
      sprintf(symval->obj_loc, "%%s+%zu", tagval->width);
      size_t padding =
          symval->type.alignment - (tagval->width % symval->type.alignment);
      tagval->width += padding + symval->type.width;
    } else if (tagval->width < symval->type.width) {
      tagval->width = symval->type.width;
    }
    llist_push_back(member_list, symval);
  }
  ASTree *left_brace = astree_get(struct_, 1);
  astree_adopt(left_brace, 1, member);
  return struct_;
}

ASTree *validate_declarator(ASTree *declarator) {
  SymbolValue *symval =
      symbol_value_init(&declarator->loc, state_get_sequence(state));
  declarator->type = &symval->type;
  return declarator;
}

ASTree *declare_symbol(ASTree *declaration, ASTree *declarator) {
  ASTree *err_or_decl = validate_declaration(declaration, declarator);
  if (err_or_decl->symbol == TOK_TYPE_ERROR) {
    return propogate_type_error(declaration, err_or_decl);
  } else {
    return astree_adopt(declaration, 1, declarator);
  }
}

ASTree *validate_return(ASTree *ret, ASTree *expr) {
  TypeSpec ret_spec = SPEC_EMPTY;
  /* strip function type */
  SymbolValue *function_symval = state_get_function(state);
  int status = strip_aux_type(&ret_spec, &function_symval->type);
  if (status) {
    typespec_destroy(&ret_spec);
    if (expr != NULL) astree_adopt(ret, 1, expr);
    return create_type_error(ret, status);
  }
  if (expr != NULL) {
    expr = perform_pointer_conv(expr);
    if (expr->symbol == TOK_TYPE_ERROR) {
      /* free temporary spec */
      typespec_destroy(&ret_spec);
      return propogate_type_error(ret, expr);
    }
    expr = convert_type(expr, &ret_spec);
    if (expr->symbol == TOK_TYPE_ERROR) {
      /* free temporary spec */
      typespec_destroy(&ret_spec);
      return propogate_type_error(ret, expr);
    }
    /* free temporary spec */
    typespec_destroy(&ret_spec);
    return astree_adopt(ret, 1, expr);
  } else {
    int compatibility = types_compatible(&ret_spec, &SPEC_VOID);
    if (compatibility != TCHK_COMPATIBLE) {
      /* free temporary spec */
      typespec_destroy(&ret_spec);
      return create_type_error(ret, BCC_TERR_EXPECTED_RETVAL);
    } else {
      /* free temporary spec */
      typespec_destroy(&ret_spec);
      return ret;
    }
  }
}

ASTree *validate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                        ASTree *else_body) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
  }

  if (!typespec_is_scalar(condition->type)) {
    return create_type_error(
        astree_adopt(ifelse, 3, condition, if_body, else_body),
        BCC_TERR_EXPECTED_SCALAR);
  }

  return astree_adopt(ifelse, 3, condition, if_body, else_body);
}

ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt) {
  if (!typespec_is_integer(expr->type)) {
    return create_type_error(astree_adopt(switch_, 2, expr, stmt),
                             BCC_TERR_EXPECTED_INTEGER);
  }

  return astree_adopt(switch_, 2, expr, stmt);
}

ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(while_, condition);
    astree_adopt(while_, 1, stmt);
    return errnode;
  }
  if (!typespec_is_scalar(condition->type)) {
    return create_type_error(astree_adopt(while_, 2, condition, stmt),
                             BCC_TERR_EXPECTED_INTEGER);
  }
  return astree_adopt(while_, 2, condition, stmt);
}

ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(do_, condition);
    astree_adopt(do_, 1, stmt);
    return errnode;
  }
  if (!typespec_is_scalar(condition->type)) {
    return create_type_error(astree_adopt(do_, 2, condition, stmt),
                             BCC_TERR_EXPECTED_INTEGER);
  }
  return astree_adopt(do_, 2, condition, stmt);
}

ASTree *validate_for_exprs(ASTree *left_paren, ASTree *init_expr,
                           ASTree *pre_iter_expr, ASTree *reinit_expr) {
  if (init_expr->symbol == TOK_TYPE_ERROR) {
  } else if (pre_iter_expr->symbol == TOK_TYPE_ERROR) {
  } else if (reinit_expr->symbol == TOK_TYPE_ERROR) {
  }
  if (pre_iter_expr != &EMPTY_EXPR) {
    if (!typespec_is_scalar(pre_iter_expr->type)) {
      return create_type_error(
          astree_adopt(left_paren, 3, init_expr, pre_iter_expr, reinit_expr),
          BCC_TERR_EXPECTED_SCALCONST);
    }
  }
  return astree_adopt(left_paren, 3, init_expr, pre_iter_expr, reinit_expr);
}

ASTree *validate_for(ASTree *for_, ASTree *left_paren, ASTree *stmt) {
  if (left_paren->symbol == TOK_TYPE_ERROR) {
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
  }
  return astree_adopt(for_, 2, left_paren, stmt);
}

/* TODO(Robert): decide whether or not to create empty labels for goto
 * statements whose label has not been defined yet.
 */
ASTree *validate_label(ASTree *label, ASTree *ident_node, ASTree *stmt) {
  if (stmt->symbol == TOK_TYPE_ERROR) {
    astree_adopt(label, 1, ident_node);
    return propogate_type_error(label, stmt);
  }
  const char *ident = ident_node->lexinfo;
  size_t ident_len = strlen(ident);
  LabelValue *existing_entry = state_get_label(state, ident, ident_len);
  if (existing_entry) {
    if (existing_entry->is_defined) {
      return create_type_error(astree_adopt(label, 2, ident_node, stmt),
                               BCC_TERR_REDEFINITION);
    } else {
      existing_entry->loc = &ident_node->loc;
      existing_entry->is_defined = 1;
      return astree_adopt(label, 2, ident_node, stmt);
    }
  } else {
    LabelValue *labval = malloc(sizeof(*labval));
    labval->loc = &ident_node->loc;
    labval->is_defined = 1;
    int status = state_insert_label(state, ident, ident_len, labval);
    if (status) {
      return create_type_error(astree_adopt(label, 2, ident_node, stmt),
                               status);
    }
    return astree_adopt(label, 2, ident_node, stmt);
  }
}

ASTree *validate_case(ASTree *case_, ASTree *constexpr, ASTree *stmt) {
  if (constexpr->symbol == TOK_TYPE_ERROR) {
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
  }

  const TypeSpec *case_const_spec = constexpr->type;
  if (!typespec_is_integer(case_const_spec) ||
      !(constexpr->attributes | ATTR_EXPR_ARITHCONST)) {
    return create_type_error(astree_adopt(case_, 2, constexpr, stmt),
                             BCC_TERR_EXPECTED_INTCONST);
  }

  return astree_adopt(case_, 2, constexpr, stmt);
}

ASTree *validate_block_content(ASTree *block, ASTree *block_content) {
  if (block->symbol == TOK_TYPE_ERROR) {
    ASTree *real_block = astree_get(block, 0);
    astree_adopt(block, 1, block_content);
    return block;
  } else if (block_content->symbol == TOK_TYPE_ERROR) {
    return propogate_type_error(block, block_content);
  }
  return astree_adopt(block, 1, block_content);
}

/*
 * external functions
 */

int type_checker_make_table(ASTree *root) {
  DEBUGS('t', "Making symbol table");
  state = state_init();
  root->symbol_table = symbol_table_init();
  state_push_table(state, root->symbol_table);
  size_t i;
  for (i = 0; i < astree_count(root); ++i) {
    ASTree *child = astree_get(root, i);
    if (child == &EMPTY_EXPR) {
      continue;
    } else if (child->symbol == TOK_DECLARATION) {
      /*
      int status = validate_declaration(child);
      if (status) return status;
      */
    } else {
      return BCC_TERR_UNEXPECTED_TOKEN;
    }
  }
  int status = state_pop_table(state);
  if (status) return status;
  return state_destroy(state);
}
