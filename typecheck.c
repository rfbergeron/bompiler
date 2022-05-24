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

/* forward declarations for mutual recursion */
int validate_expr(ASTree *expression);
int validate_stmt(ASTree *statement);
int types_compatible(
    const TypeSpec *type1,
    const TypeSpec *type2); /* required to check param and member types */
int resolve_tag(ASTree *tag);

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

int convert_type(ASTree *parent, ASTree **out, const TypeSpec *type) {
  size_t index = llist_find(&parent->children, *out);
  int compatibility = types_compatible(type, (*out)->type);
  if (compatibility == TCHK_COMPATIBLE) {
    return BCC_TERR_SUCCESS;
  } else if (compatibility == TCHK_INCOMPATIBLE) {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  }
  TypeSpec *cast_spec = malloc(sizeof(*cast_spec));
  int status = typespec_copy(cast_spec, type);
  if (status) return status;
  ASTree *cast = astree_init(TOK_CAST, (*out)->loc, "_cast");
  cast->type = cast_spec;
  cast->attributes |=
      (*out)->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
  /* remove child from parent's list */
  llist_extract(&parent->children, index);
  status = llist_insert(&parent->children, astree_adopt(cast, 1, *out), index);
  if (status) return status;
  *out = cast;
  return BCC_TERR_SUCCESS;
}

int perform_pointer_conv(ASTree *parent, ASTree **out) {
  size_t index = llist_find(&parent->children, *out);
  const TypeSpec *child_spec = (*out)->type;
  if (!typespec_is_array(child_spec) && !typespec_is_function(child_spec)) {
    return BCC_TERR_SUCCESS;
  } else {
    TypeSpec *pointer_spec = malloc(sizeof(*pointer_spec));
    if (typespec_is_array(child_spec)) {
      int status = strip_aux_type(pointer_spec, child_spec);
      if (status) return status;
    } else {
      int status = typespec_copy(pointer_spec, child_spec);
      if (status) return status;
    }
    AuxSpec *pointer_aux = calloc(1, sizeof(*pointer_aux));
    pointer_aux->aux = AUX_POINTER;
    int status = llist_push_front(&pointer_spec->auxspecs, pointer_aux);
    if (status) return status;
    ASTree *cast = astree_init(TOK_CAST, (*out)->loc, "_cast");
    cast->type = pointer_spec;
    cast->attributes |=
        (*out)->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
    /* remove child from parent's list */
    llist_extract(&parent->children, index);
    status =
        llist_insert(&parent->children, astree_adopt(cast, 1, *out), index);
    if (status) return status;
    *out = cast;
    return BCC_TERR_SUCCESS;
  }
}

int is_const_zero(ASTree *tree) {
  if (tree->symbol != TOK_INTCON) {
    return 0;
  } else {
    return strtol(tree->lexinfo, NULL, 0) == 0;
  }
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

int validate_tag_typespec(ASTree *type, TypeSpec *out) {
  int status = resolve_tag(type);
  if (status) return status;
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

int validate_ident(ASTree *ident) {
  DEBUGS('t', "Attempting to assign a type");
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strnlen(id_str, MAX_IDENT_LEN);
  SymbolValue *symval = NULL;
  int in_current_scope = state_get_symbol(state, id_str, id_str_len, &symval);
  if (symval) {
    DEBUGS('t', "Assigning %s a symbol", id_str);
    ident->type = &(symval->type);
    return BCC_TERR_SUCCESS;
  } else {
    return BCC_TERR_SYM_NOT_FOUND;
  }
}

int validate_call(ASTree *call) {
  /* functon subtree is the last child of the call node */
  ASTree *function = astree_get(call, astree_count(call) - 1);
  int status = validate_expr(function);
  if (status) return status;
  status = perform_pointer_conv(call, &function);
  if (status) return status;

  TypeSpec *function_spec = (TypeSpec *)function->type;
  if (!typespec_is_fnptr(function_spec)) {
    return BCC_TERR_EXPECTED_FUNCTION;
  }

  /* second auxspec will be the function; first is pointer */
  AuxSpec *param_spec = llist_get(&function_spec->auxspecs, 1);
  LinkedList *param_list = param_spec->data.params;
  /* subtract one since function expression is also a child */
  if (astree_count(call) <= llist_size(param_list)) {
    return BCC_TERR_EXCESS_PARAMS;
  } else if (astree_count(call) - 1 > llist_size(param_list)) {
    return BCC_TERR_INSUFF_PARAMS;
  }

  DEBUGS('t', "Validating %d arguments for function call",
         astree_count(call) - 1);
  size_t i;
  for (i = 0; i < llist_size(param_list); ++i) {
    DEBUGS('t', "Validating argument %d", i);
    ASTree *call_param = astree_get(call, i);
    int status = validate_expr(call_param);
    if (status) return status;
    status = perform_pointer_conv(call, &call_param);
    if (status) return status;

    DEBUGS('t', "Comparing types");
    SymbolValue *param_symval = llist_get(param_list, i);
    int compatibility = types_compatible(&param_symval->type, call_param->type);
    if (compatibility == TCHK_INCOMPATIBLE ||
        compatibility == TCHK_EXPLICIT_CAST) {
      return BCC_TERR_INCOMPATIBLE_TYPES;
    }
  }

  /* strip pointer */
  TypeSpec temp_spec = SPEC_EMPTY;
  status = strip_aux_type(&temp_spec, function_spec);
  if (status) return status;
  /* strip function */
  TypeSpec *return_spec = malloc(sizeof(*return_spec));
  status = strip_aux_type(return_spec, &temp_spec);
  if (status) return status;
  /* free temporaries created by stripping */
  typespec_destroy(&temp_spec);
  call->type = return_spec;
  return BCC_TERR_SUCCESS;
}

int validate_conditional(ASTree *conditional) {
  ASTree *condition = astree_get(conditional, 0);
  int status = validate_expr(condition);
  if (status) return status;
  status = perform_pointer_conv(conditional, &condition);
  if (status) return status;

  if (!typespec_is_scalar(condition->type)) {
    return BCC_TERR_EXPECTED_SCALAR;
  }

  ASTree *left = astree_get(conditional, 1);
  status = validate_expr(left);
  if (status) return status;
  status = perform_pointer_conv(conditional, &left);
  if (status) return status;

  ASTree *right = astree_get(conditional, 2);
  status = validate_expr(right);
  if (status) return status;
  status = perform_pointer_conv(conditional, &right);
  if (status) return status;

  /* TODO(Robert): the rules for conversion on the output of the ternary
   * operator are different from usual conversions and compatibility rules, and
   * should have their own function
   */
  return determine_conversion(left->type, right->type, &conditional->type);
}

int validate_comma(ASTree *comma) {
  ASTree *left = astree_get(comma, 0);
  int status = validate_expr(left);
  if (status) return status;

  ASTree *right = astree_get(comma, 1);
  status = validate_expr(right);
  if (status) return status;
  status = perform_pointer_conv(comma, &right);
  if (status) return status;
  comma->attributes |=
      right->attributes & (ATTR_EXPR_ARITHCONST | ATTR_EXPR_CONST);

  comma->type = right->type;
  return BCC_TERR_SUCCESS;
}

int validate_assignment(ASTree *assignment) {
  ASTree *dest = astree_get(assignment, 0);
  int status = validate_expr(dest);
  if (status) return status;

  ASTree *src = astree_get(assignment, 1);
  status = validate_expr(src);
  if (status) return status;
  status = perform_pointer_conv(assignment, &src);
  if (status) return status;
  assignment->attributes |=
      src->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);

  assignment->type = dest->type;
  return convert_type(assignment, &src, dest->type);
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

int validate_cast(ASTree *cast) {
  ASTree *declaration = astree_get(cast, 0);
  /*
  int status = validate_declaration(declaration);
  if (status) return status;
  */
  ASTree *expr = astree_get(cast, 1);

  int status = validate_expr(expr);
  if (status) return status;
  status = perform_pointer_conv(cast, &expr);
  if (status) return status;

  ASTree *type_name = astree_get(declaration, 1);
  int compatibility = types_compatible(type_name->type, expr->type);
  if (compatibility == TCHK_INCOMPATIBLE) {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  } else {
    cast->type = type_name->type;
    cast->attributes |=
        expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
    return BCC_TERR_SUCCESS;
    ;
  }
}

int typecheck_addop(ASTree *operator, ASTree * left, ASTree *right) {
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    int status = determine_conversion(left_type, right_type, &operator->type);
    if (status) return status;
    status = convert_type(operator, & left, operator->type);
    if (status) return status;
    return convert_type(operator, & right, operator->type);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_integer(right_type)) {
    operator->type = left_type;
    return BCC_TERR_SUCCESS;
  } else if (typespec_is_integer(left_type) &&
             typespec_is_pointer(right_type)) {
    operator->type = right_type;
    return BCC_TERR_SUCCESS;
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type) && operator->symbol == '-') {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE) {
      /* types should be the same; just pick the left one */
      operator->type = left_type;
      return BCC_TERR_SUCCESS;
    } else {
      return BCC_TERR_INCOMPATIBLE_TYPES;
    }
  } else {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  }
}

int typecheck_logop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_scalar(left->type) && typespec_is_scalar(right->type)) {
    operator->type = & SPEC_INT;
    return BCC_TERR_SUCCESS;
  } else {
    return BCC_TERR_EXPECTED_SCALAR;
  }
}

int typecheck_relop(ASTree *operator, ASTree * left, ASTree *right) {
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;
  operator->type = & SPEC_INT;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    const TypeSpec *common_type;
    int status = determine_conversion(left_type, right_type, &common_type);
    if (status) return status;
    status = convert_type(operator, & left, common_type);
    if (status) return status;
    return convert_type(operator, & right, common_type);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type)) {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE || typespec_is_voidptr(left_type) ||
        typespec_is_voidptr(right_type)) {
      return BCC_TERR_SUCCESS;
    } else {
      return BCC_TERR_INCOMPATIBLE_TYPES;
    }
  } else if (((typespec_is_pointer(left_type) && is_const_zero(right)) ||
              (is_const_zero(left) && typespec_is_pointer(right_type))) &&
             (operator->symbol == TOK_EQ || operator->symbol == TOK_NE)) {
    return BCC_TERR_SUCCESS;
  } else {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  }
}

int typecheck_mulop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_arithmetic(left->type) &&
      typespec_is_arithmetic(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) return status;
    status = convert_type(operator, & left, operator->type);
    if (status) return status;
    return convert_type(operator, & right, operator->type);
  } else {
    return BCC_TERR_EXPECTED_ARITHMETIC;
  }
}

int typecheck_shfop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, &SPEC_INT, &operator->type);
    if (status) return status;
    status = convert_type(operator, & left, operator->type);
    /* promote right operand independently of left */
    const TypeSpec *dummy;
    status = determine_conversion(right->type, &SPEC_INT, &dummy);
    if (status) return status;
    return convert_type(operator, & right, operator->type);
  } else {
    return BCC_TERR_EXPECTED_INTEGER;
  }
}

int typecheck_bitop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) return status;
    status = convert_type(operator, & left, operator->type);
    if (status) return status;
    return convert_type(operator, & right, operator->type);
  } else {
    return BCC_TERR_EXPECTED_INTEGER;
  }
}

int validate_binop(ASTree *operator) {
  DEBUGS('t', "Validating binary operator %c", operator->symbol);

  ASTree *left = astree_get(operator, 0);
  int status = validate_expr(left);
  if (status) return status;
  status = perform_pointer_conv(operator, & left);
  if (status) return status;

  ASTree *right = astree_get(operator, 1);
  status = validate_expr(right);
  if (status) return status;
  status = perform_pointer_conv(operator, & right);
  if (status) return status;

  switch (operator->symbol) {
    case TOK_SHL:
    case TOK_SHR:
      status = typecheck_shfop(operator, left, right);
      break;
    case '&':
    case '|':
    case '^':
      status = typecheck_bitop(operator, left, right);
      break;
    case '*':
    case '/':
    case '%':
      status = typecheck_mulop(operator, left, right);
      break;
    case '+':
    case '-':
      status = typecheck_addop(operator, left, right);
      break;
    case TOK_EQ:
    case TOK_NE:
    case TOK_GE:
    case TOK_LE:
    case '>':
    case '<':
      status = typecheck_relop(operator, left, right);
      break;
    case TOK_AND:
    case TOK_OR:
      status = typecheck_logop(operator, left, right);
      break;
    default:
      return BCC_TERR_FAILURE;
  }

  unsigned int result_attrs = left->attributes & right->attributes;
  operator->attributes |=
      result_attrs &(ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
  return status;
}

int is_increment(const int symbol) {
  return symbol == TOK_INC || symbol == TOK_DEC || symbol == TOK_POST_INC ||
         symbol == TOK_POST_DEC;
}

int validate_unop(ASTree *operator) {
  DEBUGS('t', "Validating unary operator %c", operator->symbol);
  ASTree *operand = astree_get(operator, 0);
  int status = validate_expr(operand);
  if (status != 0) return status;
  const TypeSpec *operand_type = operand->type;

  if (is_increment(operator->symbol) && !typespec_is_scalar(operand_type)) {
    return BCC_TERR_EXPECTED_SCALAR;
  } else if ((operator->symbol == TOK_NEG || operator->symbol == TOK_POS) &&
             !typespec_is_arithmetic(operand_type)) {
    return BCC_TERR_EXPECTED_ARITHMETIC;
  } else if (operator->symbol == '~' && !typespec_is_integer(operand_type)) {
    return BCC_TERR_EXPECTED_INTEGER;
  } else if (operator->symbol == '!') {
    if (typespec_is_scalar(operand_type)) {
      operator->type = & SPEC_INT;
      return BCC_TERR_SUCCESS;
      ;
    } else {
      return BCC_TERR_EXPECTED_SCALAR;
    }
  } else {
    if (!is_increment(operator->symbol)) {
      int status = perform_pointer_conv(operator, & operand);
      if (status) return status;
    }
    status = determine_conversion(operand_type, &SPEC_INT, &operator->type);
    if (status) return status;
    return convert_type(operator, & operand, operator->type);
  }
}

int validate_indirection(ASTree *indirection) {
  ASTree *subexpr = astree_get(indirection, 0);
  int status = validate_expr(subexpr);
  if (status) return status;
  status = perform_pointer_conv(indirection, &subexpr);
  if (status) return status;

  if (typespec_is_pointer(indirection->type)) {
    TypeSpec *indirection_spec = malloc(sizeof(*indirection_spec));
    int status = strip_aux_type(indirection_spec, subexpr->type);
    if (status) return status;
    indirection->type = indirection_spec;
    return BCC_TERR_SUCCESS;
  } else {
    return BCC_TERR_EXPECTED_POINTER;
  }
}

int validate_addrof(ASTree *addrof) {
  ASTree *subexpr = astree_get(addrof, 0);
  int status = validate_expr(subexpr);
  if (status) return status;
  /* TODO(Robert): check that operand is an lval */
  /* TODO(Robert): set constexpr attribute if operand is static/extern */
  TypeSpec *addrof_spec = malloc(sizeof(*addrof_spec));
  status = typespec_copy(addrof_spec, subexpr->type);
  if (status) return status;
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  llist_push_front(&addrof_spec->auxspecs, ptr_aux);
  addrof->type = addrof_spec;
  return BCC_TERR_SUCCESS;
}

int validate_sizeof(ASTree *sizeof_) {
  if (astree_get(sizeof_, 0)->symbol == TOK_DECLARATION) {
    ASTree *declaration = astree_get(sizeof_, 0);
    /*
    int status = validate_declaration(declaration);
    if (status) return status;
    */
    ASTree *type_name = astree_get(declaration, 1);
    if (typespec_is_incomplete(type_name->type)) {
      return BCC_TERR_INCOMPLETE_TYPE;
    }
  } else {
    ASTree *expr = astree_get(sizeof_, 0);
    int status = validate_expr(expr);
    if (status) return status;
    if (typespec_is_incomplete(expr->type)) {
      return BCC_TERR_INCOMPLETE_TYPE;
    }
  }
  /* TODO(Robert): compute actual size and also probably make sure that this
   * is actually the correct type name for the output of sizeof on this
   * platform
   */
  sizeof_->type = &SPEC_ULONG;
  sizeof_->attributes |= (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
  return BCC_TERR_SUCCESS;
}

int validate_subscript(ASTree *subscript) {
  ASTree *composite_object = astree_get(subscript, 0);
  int status = validate_expr(composite_object);
  if (status) return status;
  status = perform_pointer_conv(subscript, &composite_object);
  if (status) return status;

  ASTree *index = astree_get(subscript, 1);
  status = validate_expr(index);
  if (status) return status;
  status = perform_pointer_conv(subscript, &index);
  if (status) return status;

  if (!typespec_is_pointer(composite_object->type)) {
    return BCC_TERR_EXPECTED_POINTER;
  } else if (!typespec_is_integer(index->type)) {
    return BCC_TERR_EXPECTED_INTEGER;
  } else {
    TypeSpec *subscript_spec = malloc(sizeof(*subscript_spec));
    int status = strip_aux_type(subscript_spec, composite_object->type);
    if (status) return status;
    subscript->type = subscript_spec;
    return BCC_TERR_SUCCESS;
  }
}

int validate_reference(ASTree *reference) {
  ASTree *strunion = astree_get(reference, 0);
  int status = validate_expr(strunion);
  if (status) return status;
  const TypeSpec *strunion_type = strunion->type;
  if (!typespec_is_struct(strunion_type) && !typespec_is_union(strunion_type)) {
    return BCC_TERR_EXPECTED_TAG;
  }

  ASTree *member = astree_get(reference, 1);
  const char *member_name = member->lexinfo;
  const size_t member_name_len = strlen(member_name);
  AuxSpec *strunion_aux = llist_front(&strunion_type->auxspecs);
  SymbolTable *member_table = strunion_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return BCC_TERR_SYM_NOT_FOUND;
  } else {
    reference->type = &symval->type;
    return BCC_TERR_SUCCESS;
  }
}

int validate_arrow(ASTree *arrow) {
  ASTree *strunion = astree_get(arrow, 0);
  int status = validate_expr(strunion);
  if (status) return status;
  status = perform_pointer_conv(arrow, &strunion);
  if (status) return status;

  const TypeSpec *strunion_type = strunion->type;
  if (!typespec_is_structptr(strunion_type) &&
      !typespec_is_unionptr(strunion_type)) {
    return BCC_TERR_EXPECTED_TAG;
  }
  ASTree *member = astree_get(arrow, 1);
  const char *member_name = member->lexinfo;
  const size_t member_name_len = strlen(member_name);
  /* first auxtype is pointer; second is struct/union */
  AuxSpec *strunion_aux = llist_get(&strunion_type->auxspecs, 1);
  SymbolTable *member_table = strunion_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return BCC_TERR_SYM_NOT_FOUND;
  } else {
    arrow->type = &symval->type;
    return BCC_TERR_SUCCESS;
  }
}

int validate_expr(ASTree *expression) {
  int status;
  const char *ident;
  ASTree *left;
  ASTree *right;

  DEBUGS('t', "Validating next expression");
  switch (expression->symbol) {
    case '?':
      status = validate_conditional(expression);
      break;
    case ',':
      status = validate_comma(expression);
      break;
    case '=':
    case TOK_ADDEQ:
    case TOK_SUBEQ:
    case TOK_MULEQ:
    case TOK_DIVEQ:
    case TOK_REMEQ:
    case TOK_ANDEQ:
    case TOK_OREQ:
    case TOK_XOREQ:
    case TOK_SHREQ:
    case TOK_SHLEQ:
      status = validate_assignment(expression);
      break;
    case TOK_EQ:
    case TOK_NE:
    case TOK_OR:
    case TOK_AND:
    case TOK_LE:
    case TOK_GE:
    case '>':
    case '<':
    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
    case '|':
    case '^':
    case '&':
    case TOK_SHL:
    case TOK_SHR:
      status = validate_binop(expression);
      break;
    case '!':
    case TOK_POS: /* promotion operator */
    case TOK_NEG:
    case '~':
    case TOK_INC:
    case TOK_DEC:
    case TOK_POST_INC:
    case TOK_POST_DEC:
      status = validate_unop(expression);
      break;
    case TOK_INDIRECTION:
      status = validate_indirection(expression);
      break;
    case TOK_ADDROF:
      status = validate_addrof(expression);
      break;
    case TOK_SIZEOF:
      status = validate_sizeof(expression);
      break;
    case TOK_CALL:
      /* expression->attributes |= ATTR_EXPR_VREG; */
      status = validate_call(expression);
      break;
    case TOK_SUBSCRIPT:
      status = validate_subscript(expression);
      break;
    case '.':
      status = validate_reference(expression);
      break;
    case TOK_ARROW:
      status = validate_arrow(expression);
      break;
    case TOK_INTCON:
      status = validate_intcon(expression);
      break;
    case TOK_CHARCON:
      status = validate_charcon(expression);
      break;
    case TOK_STRINGCON:
      status = validate_stringcon(expression);
      break;
    case TOK_IDENT:
      DEBUGS('t', "bonk");
      status = validate_ident(expression);
      break;
    case TOK_CAST:
      status = validate_cast(expression);
      break;
    default:
      status = BCC_TERR_UNEXPECTED_TOKEN;
  }
  return status;
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
    int status = validate_expr(initializer);
    if (status) return status;
    status = perform_pointer_conv(init_list, &initializer);
    if (status) return status;
    status = convert_type(init_list, &initializer, &element_type);
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
    int status = validate_expr(initializer);
    if (status) return status;
    status = perform_pointer_conv(init_list, &initializer);
    if (status) return status;
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
      int status = validate_expr(initializer);
      if (status) return status;
      status = perform_pointer_conv(init_list, &initializer);
      if (status) return status;
      SymbolValue *member_symbol = llist_get((LinkedList *)members, i);
      status = convert_type(init_list, &initializer, &member_symbol->type);
      if (status) return status;
    }
    return BCC_TERR_SUCCESS;
  }
}

ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer) {
  ASTree *err_or_decl = validate_declaration(declaration, declarator);
  if (err_or_decl->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = propogate_type_error(equal_sign, declarator);
    astree_adopt(equal_sign, 1, initializer);
    return propogate_type_error(declaration, errnode);
  }
  if (initializer->symbol == TOK_INIT_LIST) {
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
    int status = validate_expr(initializer);
    if (status) {
      return create_type_error(
          astree_adopt(declaration, 1,
                       astree_adopt(equal_sign, 2, declarator, initializer)),
          status);
    }
    status = perform_pointer_conv(equal_sign, &initializer);
    if (status) {
      return create_type_error(
          astree_adopt(declaration, 1,
                       astree_adopt(equal_sign, 2, declarator, initializer)),
          status);
    }
    status = convert_type(equal_sign, &initializer, declarator->type);
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
  SymbolValue *symval = NULL;
  state_get_symbol(state, declarator->lexinfo, strlen(declarator->lexinfo),
                   &symval);
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

  int status = state_set_function(state, symval);
  if (status) {
    return create_type_error(astree_adopt(declaration, 2, declarator, body),
                             status);
  }
  body->symbol_table = declarator->symbol_table;
  status = validate_stmt(body);
  if (status) {
    state_unset_function(state);
    return create_type_error(astree_adopt(declaration, 2, declarator, body),
                             status);
  }
  status = state_unset_function(state);
  if (status) {
    return create_type_error(astree_adopt(declaration, 2, declarator, body),
                             status);
  }
  /* mark function as defined */
  symval->flags |= SYMFLAG_FUNCTION_DEFINED;
  return astree_adopt(declaration, 2, declarator, body);
}

/* TODO(Robert): in the interest of code reuse I used the existing functions
 * for entering and leaving scopes and making entries within a scope that
 * are normally used for objects and functions, even though struct members
 * don't work quite the same way. check to make sure everything is doing
 * okay later on
 */
int define_members(ASTree *composite_type, TagValue *tagval) {
  SymbolTable *member_table = tagval->data.members.by_name;
  int status = state_push_table(state, member_table);
  if (status) return status;
  LinkedList *member_list = &tagval->data.members.in_order;
  /* TODO(Robert): handle unique tags */
  ASTree *members = astree_get(composite_type, 1);
  size_t i;
  for (i = 0; i < astree_count(members); ++i) {
    ASTree *member = astree_get(members, i);
    ASTree *declarator = astree_get(member, 1);
    const char *member_id_str = declarator->lexinfo;
    size_t member_id_str_len = strnlen(member_id_str, MAX_IDENT_LEN);
    DEBUGS('t', "Found composite type member: %s", member_id_str);
    /*
    int status = validate_declaration(member);
    if (status != 0) return status;
    */
    SymbolValue *member_entry =
        symbol_table_get(member_table, member_id_str, member_id_str_len);
    if (tagval->alignment < member_entry->type.alignment) {
      tagval->alignment = member_entry->type.alignment;
    }
    if (tagval->tag == TAG_STRUCT) {
      sprintf(member_entry->obj_loc, "%%s+%zu", tagval->width);
      size_t padding = member_entry->type.alignment -
                       (tagval->width % member_entry->type.alignment);
      tagval->width += padding + member_entry->type.width;
    } else if (tagval->width < member_entry->type.width) {
      tagval->width = member_entry->type.width;
    }
    llist_push_back(member_list, member_entry);
    DEBUGS('t', "Field inserted at %s", declarator->lexinfo);
  }
  status = state_pop_table(state);
  if (status) return status;
  tagval->is_defined = 1;
  return BCC_TERR_SUCCESS;
}

/* Enumerations are structured as follows: the enum keyword, with an optional
 * identifier (the tag) as its child. This is followed by a sequence of nodes,
 * which are either a TOK_IDENT or an '=', with two children, those being the
 * TOK_IDENT for the enumeration consntant and the value it represents.
 */
int define_enumerators(ASTree *enum_, TagValue *tagval) {
  /* TODO(Robert): Enumerators are compile time constants. Figure them out
   * at compile time for real, like all other compile time consntants.
   */
  ASTree *enumerators = astree_get(enum_, 1);
  size_t i;
  for (i = 0; i < astree_count(enumerators); ++i) {
    ASTree *enumerator = astree_get(enumerators, i);
    ASTree *value_node = NULL;
    if (enumerator->symbol == '=') {
      value_node = astree_get(enumerator, 1);
      enumerator = astree_get(enumerator, 0);
    }

    const char *enum_ident = enumerator->lexinfo;
    size_t enum_ident_len = strlen(enum_ident);
    SymbolValue *exists = NULL;
    int is_redefinition =
        state_get_symbol(state, enum_ident, enum_ident_len, &exists);
    if (is_redefinition) {
      return BCC_TERR_REDEFINITION;
    }

    SymbolValue *symval =
        symbol_value_init(&enumerator->loc, state_get_sequence(state));
    int status = typespec_init(&symval->type);
    if (status) return status;
    symval->type.alignment = X64_ALIGNOF_INT;
    symval->type.width = X64_SIZEOF_INT;
    symval->type.base = TYPE_ENUM;
    /* mark as enumeration consntant */
    symval->flags |= SYMFLAG_ENUM_CONST;

    AuxSpec *enum_aux = calloc(1, sizeof(*enum_aux));
    enum_aux->aux = AUX_ENUM;
    enum_aux->data.tag.name = (astree_get(enum_, 0))->lexinfo;
    llist_push_back(&symval->type.auxspecs, enum_aux);

    status = state_insert_symbol(state, enum_ident, enum_ident_len, symval);
    if (status) return status;

    enumerator->type = &symval->type;

    if (value_node != NULL) {
      /* TODO(Robert): evaluate enumeration constants */
      int status = validate_expr(value_node);
      if (status) return status;
      if ((value_node->attributes & ATTR_EXPR_ARITHCONST) == 0) {
        return BCC_TERR_EXPECTED_ARITHCONST;
      }
    }
  }

  /* TODO(Robert): adjust enum size based on enumerator sizes */
  tagval->width = X64_SIZEOF_INT;
  tagval->alignment = X64_ALIGNOF_INT;
  tagval->is_defined = 1;
  return BCC_TERR_SUCCESS;
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

/* Tag declaration/definition steps:
 * 1. Declarations that only declare a tag and no associated object receive
 *    special treatement.
 * 2. Declarations that specify unique tags are reported as errors, for now,
 *    but will need special treatment in that they create a unique name for
 *    the tag and return the tag information to the call site somehow.
 */

int declare_tag(ASTree *tag, TagValue **out) {
  ASTree *first_child = astree_get(tag, 0);
  if (first_child->symbol != TOK_IDENT) {
    /* TODO(Robert): allow unique tags */
    return BCC_TERR_FAILURE;
  }
  TagValue *exists = NULL;
  const char *tag_name = first_child->lexinfo;
  size_t tag_name_len = strlen(tag_name);
  int is_redefinition = state_get_tag(state, tag_name, tag_name_len, &exists);
  if (is_redefinition) {
    return BCC_TERR_REDEFINITION;
  }
  TagValue *tagval = tag_value_init(tag_from_symbol(tag->symbol));
  if (tagval == NULL) return BCC_TERR_FAILURE;
  int status = state_insert_tag(state, tag_name, tag_name_len, tagval);
  if (status) return status;
  *out = tagval;
  return BCC_TERR_SUCCESS;
}

int complete_tag(ASTree *tag, TagValue *tagval) {
  if (tagval->tag == TAG_ENUM)
    return define_enumerators(tag, tagval);
  else
    return define_members(tag, tagval);
}

int define_tag(ASTree *tag) {
  TagValue *tagval = NULL;
  int status = declare_tag(tag, &tagval);
  if (status) return status;
  return complete_tag(tag, tagval);
}

int illegal_tag_redefinition(ASTree *tag, TagValue *existing) {
  ASTree *first_child = astree_get(tag, 0);
  const char *tag_name = first_child->lexinfo;
  int tag_declares_members =
      astree_count(tag) > 1 || first_child->symbol != TOK_IDENT;
  if (tag->symbol == TOK_STRUCT && existing->tag != TAG_STRUCT) {
    return BCC_TERR_EXPECTED_STRUCT;
  } else if (tag->symbol == TOK_UNION && existing->tag != TAG_UNION) {
    return BCC_TERR_EXPECTED_UNION;
  } else if (tag->symbol == TOK_ENUM && existing->tag != TAG_ENUM) {
    return BCC_TERR_EXPECTED_ENUM;
  } else if (tag_declares_members && existing->is_defined) {
    /* TODO(Robert): allow identical redefinitions */
    return BCC_TERR_REDEFINITION;
  } else {
    return BCC_TERR_SUCCESS;
  }
}

int resolve_tag(ASTree *tag) {
  ASTree *first_child = astree_get(tag, 0);
  if (first_child->symbol != TOK_IDENT) {
    /* TODO(Robert): allow unique tags */
    return BCC_TERR_FAILURE;
  }
  const char *tag_name = first_child->lexinfo;
  const size_t tag_name_len = strnlen(tag_name, MAX_IDENT_LEN);
  TagValue *exists = NULL;
  int is_redefinition = state_get_tag(state, tag_name, tag_name_len, &exists);
  int tag_declares_members =
      astree_count(tag) > 1 || first_child->symbol != TOK_IDENT;
  if (exists) {
    if (is_redefinition) {
      if (illegal_tag_redefinition(tag, exists)) {
        return BCC_TERR_REDEFINITION;
      } else if (tag_declares_members) {
        return complete_tag(tag, exists);
      } else {
        return BCC_TERR_SUCCESS;
      }
    } else if (tag_declares_members) {
      return define_tag(tag);
    } else if (illegal_tag_redefinition(tag, exists)) {
      return BCC_TERR_REDEFINITION;
    } else {
      return BCC_TERR_SUCCESS;
    }
  } else {
    return define_tag(tag);
  }
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

int validate_return(ASTree *ret) {
  TypeSpec ret_spec = SPEC_EMPTY;
  /* strip function type */
  SymbolValue *function_symval = state_get_function(state);
  int status = strip_aux_type(&ret_spec, &function_symval->type);
  if (status) return status;
  if (astree_count(ret) > 0) {
    ASTree *expr = astree_get(ret, 0);
    int status = validate_expr(expr);
    if (status) return status;
    status = perform_pointer_conv(ret, &expr);
    if (status) return status;
    status = convert_type(ret, &expr, &ret_spec);
  } else {
    int compatibility = types_compatible(&ret_spec, &SPEC_VOID);
    if (compatibility != TCHK_COMPATIBLE) {
      return BCC_TERR_EXPECTED_RETVAL;
    }
  }

  /* free temporary spec */
  typespec_destroy(&ret_spec);
  return status;
}

int validate_ifelse(ASTree *ifelse) {
  ASTree *expr = astree_get(ifelse, 0);
  int status = validate_expr(expr);
  if (status) return status;
  status = perform_pointer_conv(ifelse, &expr);
  if (status) return status;

  if (!typespec_is_scalar(expr->type)) {
    return BCC_TERR_EXPECTED_SCALAR;
  }

  ASTree *if_body = astree_get(ifelse, 1);
  status = validate_stmt(if_body);
  if (status) return status;

  if (astree_count(ifelse) == 3) {
    ASTree *else_body = astree_get(ifelse, 2);
    status = validate_stmt(else_body);
    if (status) return status;
  }
  return BCC_TERR_SUCCESS;
}

int validate_switch(ASTree *switch_) {
  ASTree *expr = astree_get(switch_, 0);
  ASTree *stmt = astree_get(switch_, 1);

  int status = validate_expr(expr);
  if (status) return status;
  if (!typespec_is_integer(expr->type)) {
    return BCC_TERR_EXPECTED_INTEGER;
  }

  return validate_stmt(stmt);
}

int validate_while(ASTree *while_) {
  ASTree *expr = while_->symbol == TOK_WHILE ? astree_get(while_, 0)
                                             : astree_get(while_, 1);
  int status = validate_expr(expr);
  if (status) return status;
  status = perform_pointer_conv(while_, &expr);
  if (status) return status;
  if (!typespec_is_scalar(expr->type)) {
    return BCC_TERR_EXPECTED_INTEGER;
  }
  ASTree *while_body = while_->symbol == TOK_WHILE ? astree_get(while_, 1)
                                                   : astree_get(while_, 0);
  return validate_stmt(while_body);
}

int validate_for(ASTree *for_) {
  ASTree *first_expr = astree_get(for_, 0);
  if (first_expr != &EMPTY_EXPR) {
    int status = validate_expr(first_expr);
    if (status) return status;
  }

  ASTree *second_expr = astree_get(for_, 1);
  if (second_expr != &EMPTY_EXPR) {
    int status = validate_expr(second_expr);
    if (status) return status;
    if (!typespec_is_scalar(second_expr->type)) {
      return BCC_TERR_EXPECTED_SCALCONST;
    }
  }

  ASTree *third_expr = astree_get(for_, 2);
  if (third_expr != &EMPTY_EXPR) {
    int status = validate_expr(third_expr);
    if (status) return status;
  }

  ASTree *for_stmt = astree_get(for_, 3);
  return validate_stmt(for_stmt);
}

/* TODO(Robert): decide whether or not to create empty labels for goto
 * statements whose label has not been defined yet.
 */
int validate_label(ASTree *label) {
  ASTree *ident = astree_get(label, 0);
  const char *ident_str = ident->lexinfo;
  size_t ident_str_len = strlen(ident_str);
  LabelValue *existing_entry = state_get_label(state, ident_str, ident_str_len);
  if (existing_entry) {
    if (existing_entry->is_defined) {
      return BCC_TERR_EXPECTED_CONST;
    } else {
      existing_entry->loc = &ident->loc;
      existing_entry->is_defined = 1;
      return validate_stmt(astree_get(label, 1));
    }
  } else {
    LabelValue *labval = malloc(sizeof(*labval));
    labval->loc = &ident->loc;
    labval->is_defined = 1;
    int status = state_insert_label(state, ident_str, ident_str_len, labval);
    if (status) return status;
    return validate_stmt(astree_get(label, 1));
  }
}

int validate_case(ASTree *case_) {
  ASTree *constexpr = astree_get(case_, 0);
  int status = validate_expr(constexpr);
  if (status) return status;
  const TypeSpec *case_const_spec = constexpr->type;
  if (!typespec_is_integer(case_const_spec) ||
      !(constexpr->attributes | ATTR_EXPR_ARITHCONST)) {
    return BCC_TERR_EXPECTED_INTCONST;
  }

  ASTree *statement = astree_get(case_, 1);
  return validate_stmt(statement);
}

int validate_block(ASTree *block) {
  size_t i;
  /* don't overwrite scope if this block belongs to a function */
  if (block->symbol_table == NULL)
    block->symbol_table = symbol_table_init(state);
  int status = state_push_table(state, block->symbol_table);
  if (status) return status;
  for (i = 0; i < astree_count(block); ++i) {
    ASTree *statement = astree_get(block, i);
    int status = validate_stmt(statement);
    if (status) return status;
  }
  return state_pop_table(state);
}

int validate_stmt(ASTree *statement) {
  if (statement == &EMPTY_EXPR) return BCC_TERR_SUCCESS;
  int status;
  DEBUGS('t', "Validating next statement");
  switch (statement->symbol) {
    case TOK_RETURN:
      status = validate_return(statement);
      break;
    case TOK_IF:
      status = validate_ifelse(statement);
      break;
    case TOK_SWITCH:
      status = validate_switch(statement);
      break;
    case TOK_DO:
    case TOK_WHILE:
      status = validate_while(statement);
      break;
    case TOK_BLOCK:
      status = validate_block(statement);
      break;
    case TOK_DECLARATION:
      /*
      status = validate_declaration(statement);
      */
      break;
    case TOK_FOR:
      status = validate_for(statement);
      break;
    case TOK_LABEL:
      status = validate_label(statement);
      break;
    case TOK_CASE:
      status = validate_case(statement);
      break;
    case TOK_DEFAULT:
      status = validate_stmt(astree_get(statement, 0));
      break;
    case TOK_CONTINUE:
    case TOK_BREAK:
    case TOK_GOTO:
      /* handle this during assembly generation */
      status = BCC_TERR_SUCCESS;
      break;
    default:
      /* parser will catch anything that we don't want, so at this point the
       * only thing left that this could be is an expression-statement
       */
      status = validate_expr(statement);
      break;
  }
  return status;
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
