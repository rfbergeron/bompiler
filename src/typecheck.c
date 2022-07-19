#include "typecheck.h"

#include "assert.h"
#include "astree.h"
#include "attributes.h"
#include "badllist.h"
#include "badmap.h"
#include "bcc_err.h"
#include "ctype.h"
#include "debug.h"
#include "err.h"
#include "lyutils.h"
#include "math.h"
#include "simplestack.h"
#include "state.h"
#include "stdint.h"
#include "symtable.h"
/* TODO(Robert): Linux-specific; replace with strtoul or similar */
#include "inttypes.h"

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

SymbolValue *sym_from_type(TypeSpec *type) {
  SymbolValue temp;
  /* struct members have addresses increasing in order of declarations */
  ptrdiff_t offset = (char *)&temp.type - (char *)&temp;
  return (SymbolValue *)((char *)type - offset);
}

/*
 * TODO(Robert): recursively setting the block number no longer works because
 * of nested scoping; instead block numbers should be set during the validation
 * of expressions, at which point it is not possible to further nest scopes.
 */

/*
 * Performs automatic conversions from function and array types to pointer
 * types. Can be called safely without first checking for errors so that the
 * passed expression does not need to be checked twice.
 */
ASTree *perform_pointer_conv(ASTree *expr) {
  if (expr->symbol == TOK_TYPE_ERROR) return expr;
  const TypeSpec *spec = expr->type;
  if (!typespec_is_array(spec) && !typespec_is_function(spec)) {
    return expr;
  } else {
    TypeSpec *pointer_spec = malloc(sizeof(*pointer_spec));
    if (typespec_is_array(spec)) {
      int status = strip_aux_type(pointer_spec, spec);
      if (status) {
        free(pointer_spec);
        return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
      }
    } else {
      int status = typespec_copy(pointer_spec, spec);
      if (status) {
        free(pointer_spec);
        return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
      }
    }
    AuxSpec *pointer_aux = calloc(1, sizeof(*pointer_aux));
    pointer_aux->aux = AUX_POINTER;
    int status = llist_push_front(&pointer_spec->auxspecs, pointer_aux);
    if (status) {
      llist_destroy(&pointer_spec->auxspecs);
      free(pointer_spec);
      return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
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

/*
 * Creates tree node for automatic conversions, if necessary and possible. Error
 * nodes can safely be passed to this function.
 */
ASTree *convert_type(ASTree *expr, const TypeSpec *type) {
  if (expr->symbol == TOK_TYPE_ERROR) return expr;
  if (type->base == TYPE_ERROR) return expr;
  int compatibility = types_compatible(type, expr->type);
  if (compatibility == TCHK_COMPATIBLE) {
    return expr;
  } else if (compatibility == TCHK_INCOMPATIBLE) {
    return astree_create_errnode(expr, BCC_TERR_INCOMPATIBLE_TYPES, 3, expr, expr->type,
                       type);
  }
  TypeSpec *cast_spec = malloc(sizeof(*cast_spec));
  int status = typespec_copy(cast_spec, type);
  if (status) {
    free(cast_spec);
    return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
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

ASTree *validate_intcon(ASTree *intcon) {
  DEBUGS('t', "Validating integer constant %s", intcon->lexinfo);
  int status = 0;
  long signed_value = strtol(intcon->lexinfo, NULL, 10);
  /* TODO(Robert): I should define the compiler's own size of min and max
   * values; this would be necessary for cross-compilation from an
   * architecture with different sizes of integer types.
   */
  if (signed_value == LONG_MIN) {
    return astree_create_errnode(intcon, BCC_TERR_CONST_TOO_SMALL, 1, intcon);
  } else if (signed_value == LONG_MAX) {
    unsigned long unsigned_value = strtoul(intcon->lexinfo, NULL, 10);
    if (unsigned_value == UINT64_MAX) {
      return astree_create_errnode(intcon, BCC_TERR_CONST_TOO_LARGE, 1, intcon);
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
  return intcon;
}

ASTree *validate_charcon(ASTree *charcon) {
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
  return charcon;
}

ASTree *validate_stringcon(ASTree *stringcon) {
  TypeSpec *stringcon_type = malloc(sizeof(*stringcon->type));
  *stringcon_type = SPEC_CHAR;
  int status = typespec_init(stringcon_type);
  if (status) return astree_create_errnode(stringcon, BCC_TERR_LIBRARY_FAILURE, 0);

  AuxSpec *array_aux = calloc(1, sizeof(*array_aux));
  array_aux->aux = AUX_ARRAY;
  /* Normally, we would subtract 2 to omit the starting and ending doublequote,
   * but since strlen does not include the terminating null byte, we only
   * subtract one.
   */
  array_aux->data.memory_loc.length = strlen(stringcon->lexinfo) - 1;

  stringcon->type = stringcon_type;
  stringcon->attributes |= ATTR_EXPR_CONST;
  return stringcon;
}

ASTree *validate_integer_typespec(ASTree *spec_list, enum typespec_index i,
                                  enum typespec_flag f, size_t bytes) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & INCOMPATIBLE_FLAGSETS[i]) {
    return astree_create_errnode(spec_list, BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                       astree_get(spec_list, astree_count(spec_list) - 1));
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
    return spec_list;
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

unsigned int flag_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return TYPESPEC_FLAG_STRUCT;
    case TOK_UNION:
      return TYPESPEC_FLAG_UNION;
    case TOK_ENUM:
      return TYPESPEC_FLAG_ENUM;
    default:
      return TYPESPEC_FLAG_NONE;
  }
}

unsigned int index_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return TYPESPEC_INDEX_STRUCT;
    case TOK_UNION:
      return TYPESPEC_INDEX_UNION;
    case TOK_ENUM:
      return TYPESPEC_INDEX_ENUM;
    default:
      return TYPESPEC_INDEX_COUNT;
  }
}

/* tags will need two passes to define their members: the first for inserting
 * the symbols into the symbol table, and the second to swipe the symbols from
 * the members and put them into an auxspec
 */
ASTree *validate_tag_typespec(ASTree *spec_list, ASTree *tag) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & INCOMPATIBLE_FLAGSETS[index_from_symbol(tag->symbol)]) {
    return astree_create_errnode(astree_adopt(spec_list, 1, tag),
                       BCC_TERR_INCOMPLETE_SPEC, 2, spec_list, tag);
  }
  /* TODO(Robert): get unique tag name/info somehow */
  const char *tag_name = astree_get(tag, 0)->lexinfo;
  size_t tag_name_len = strlen(tag_name);
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, tag_name_len, &tagval);
  if (tagval == NULL) {
    return astree_create_errnode(astree_adopt(spec_list, 1, tag), BCC_TERR_TAG_NOT_FOUND,
                       1, tag);
  }

  out->base = type_from_tag(tagval->tag);
  out->width = tagval->width;
  out->alignment = tagval->alignment;
  out->flags |= flag_from_symbol(tag->symbol);

  if (out->auxspecs.anchor == NULL) {
    typespec_init(out);
  }

  AuxSpec *tag_aux = calloc(1, sizeof(*tag_aux));
  tag_aux->aux = aux_from_tag(tagval->tag);
  tag_aux->data.tag.name = tag_name;
  tag_aux->data.tag.val = tagval;
  llist_push_back(&out->auxspecs, tag_aux);
  return astree_adopt(spec_list, 1, tag);
}

ASTree *validate_typedef_typespec(ASTree *spec_list, ASTree *typedef_) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & TYPESPEC_FLAGS_STORAGE_CLASS) {
    return astree_create_errnode(astree_adopt(spec_list, 1, typedef_),
                       BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list, typedef_);
  } else {
    out->flags |= TYPESPEC_FLAG_TYPEDEF;
    return astree_adopt(spec_list, 1, typedef_);
  }
}

ASTree *validate_type_id_typespec(ASTree *spec_list, ASTree *type_id) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  const char *type_name = type_id->lexinfo;
  size_t type_name_len = strlen(type_name);
  SymbolValue *symval = NULL;
  state_get_symbol(state, type_name, type_name_len, &symval);
  if (!symval) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                       BCC_TERR_TYPEID_NOT_FOUND, 2, spec_list, type_id);
  } else if ((symval->type.flags & TYPESPEC_FLAG_TYPEDEF) == 0) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                       BCC_TERR_EXPECTED_TYPEID, 2, spec_list, type_id);
  } else {
    out->base = symval->type.base;
    out->width = symval->type.width;
    out->alignment = symval->type.alignment;
    out->flags |= symval->type.flags & (~TYPESPEC_FLAG_TYPEDEF);

    if (out->auxspecs.anchor == NULL) {
      typespec_init(out);
    }

    int status = typespec_append_auxspecs(out, &symval->type);
    if (status) {
      return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    }
    return astree_adopt(spec_list, 1, type_id);
  }
}

ASTree *validate_typespec(ASTree *spec_list, ASTree *spec) {
  if (spec_list->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(spec_list, spec);
  } else if (spec->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(spec_list, spec);
  }

  if (spec_list->type == NULL) {
    spec_list->type = calloc(1, sizeof(TypeSpec));
  }

  switch (spec->symbol) {
    case TOK_VOID: {
      TypeSpec *out = (TypeSpec *)spec_list->type;
      int incompatible =
          out->flags & INCOMPATIBLE_FLAGSETS[TYPESPEC_INDEX_VOID];
      if (incompatible) {
        return astree_create_errnode(astree_adopt(spec_list, 1, spec),
                           BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list, spec);
      } else {
        out->base = TYPE_VOID;
        out->flags |= TYPESPEC_FLAG_VOID;
        return astree_adopt(spec_list, 1, spec);
      }
    }
    case TOK_INT:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_INT, TYPESPEC_FLAG_INT,
                                       X64_SIZEOF_INT);
    case TOK_LONG:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_LONG, TYPESPEC_FLAG_LONG,
                                       X64_SIZEOF_LONG);
    case TOK_SHORT:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_SHORT,
                                       TYPESPEC_FLAG_SHORT, X64_SIZEOF_SHORT);
    case TOK_CHAR:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_CHAR, TYPESPEC_FLAG_CHAR,
                                       X64_SIZEOF_CHAR);
    case TOK_SIGNED:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_SIGNED,
                                       TYPESPEC_FLAG_SIGNED, 0);
    case TOK_UNSIGNED:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_UNSIGNED,
                                       TYPESPEC_FLAG_UNSIGNED, 0);
    case TOK_UNION:
    case TOK_STRUCT:
    case TOK_ENUM:
      return validate_tag_typespec(spec_list, spec);
    case TOK_CONST:
      /* fall through */
    case TOK_VOLATILE:
      return astree_adopt(spec_list, 1, spec);
    case TOK_TYPEDEF_NAME:
      return validate_type_id_typespec(spec_list, spec);
    case TOK_TYPEDEF:
      return validate_typedef_typespec(spec_list, spec);
    default:
      return astree_create_errnode(astree_adopt(spec_list, 1, spec), BCC_TERR_FAILURE, 0);
  }
}

ASTree *validate_typespec_list(ASTree *spec_list) {
  if (spec_list->symbol == TOK_TYPE_ERROR) return spec_list;
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
    return astree_create_errnode(spec_list, BCC_TERR_INCOMPLETE_TYPE, 1, spec_list);
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
    return astree_create_errnode(ident, BCC_TERR_SYM_NOT_FOUND, 1, ident);
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
    return astree_create_errnode(call, BCC_TERR_INSUFF_PARAMS, 1, call);
  }
  return call;
}

ASTree *validate_arg(ASTree *call, ASTree *arg) {
  if (call->symbol == TOK_TYPE_ERROR || arg->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(call, arg);
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
    return astree_create_errnode(astree_adopt(call, 1, arg), BCC_TERR_EXCESS_PARAMS, 1,
                       call);
  }
  DEBUGS('t', "Validating argument %d", param_index);
  SymbolValue *symval = llist_get(param_list, param_index);
  DEBUGS('t', "Comparing types");
  int compatibility = types_compatible(&symval->type, arg->type);
  if (compatibility == TCHK_INCOMPATIBLE ||
      compatibility == TCHK_EXPLICIT_CAST) {
    return astree_create_errnode(astree_adopt(call, 1, arg), BCC_TERR_INCOMPATIBLE_TYPES,
                       3, arg, arg->type, &symval->type);
  } else {
    return astree_adopt(call, 1, arg);
  }
}

ASTree *validate_call(ASTree *expr, ASTree *call) {
  if (call->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(call, expr);
  }
  expr = perform_pointer_conv(expr);
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(call, expr);
  }
  TypeSpec *expr_spec = (TypeSpec *)expr->type;
  if (!typespec_is_fnptr(expr_spec)) {
    return astree_create_errnode(astree_adopt(call, 1, expr), BCC_TERR_EXPECTED_FN_PTR, 2,
                       call, expr);
  }

  /* strip pointer */
  TypeSpec temp_spec = SPEC_EMPTY;
  int status = strip_aux_type(&temp_spec, expr_spec);
  if (status) {
    return astree_create_errnode(astree_adopt(call, 1, expr), BCC_TERR_LIBRARY_FAILURE,
                       0);
  }
  /* strip function */
  TypeSpec *return_spec = malloc(sizeof(*return_spec));
  status = strip_aux_type(return_spec, &temp_spec);
  if (status) {
    return astree_create_errnode(astree_adopt(call, 1, expr), BCC_TERR_LIBRARY_FAILURE,
                       0);
  }
  /* free temporaries created by stripping */
  typespec_destroy(&temp_spec);
  call->type = return_spec;
  return astree_adopt(call, 1, expr);
}

ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr, false_expr);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(qmark, 3, condition, true_expr, false_expr),
                       BCC_TERR_EXPECTED_SCALAR, 2, qmark, condition);
  }

  true_expr = perform_pointer_conv(true_expr);
  if (true_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr, false_expr);
  }

  false_expr = perform_pointer_conv(false_expr);
  if (false_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr, false_expr);
  }

  /* TODO(Robert): the rules for conversion on the output of the ternary
   * operator are different from usual conversions and compatibility rules, and
   * should have their own function
   */
  int status =
      determine_conversion(true_expr->type, false_expr->type, &qmark->type);
  if (status) {
    return astree_create_errnode(astree_adopt(qmark, 3, condition, true_expr, false_expr),
                       BCC_TERR_INCOMPATIBLE_TYPES, 3, qmark, true_expr->type,
                       false_expr->type);
  }

  return astree_adopt(qmark, 3, condition, true_expr, false_expr);
}

ASTree *validate_comma(ASTree *comma, ASTree *left_expr, ASTree *right_expr) {
  if (left_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(comma, 2, left_expr, right_expr);
  }
  right_expr = perform_pointer_conv(right_expr);
  if (right_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(comma, 2, left_expr, right_expr);
  }
  comma->attributes |=
      right_expr->attributes & (ATTR_EXPR_ARITHCONST | ATTR_EXPR_CONST);

  comma->type = right_expr->type;
  return astree_adopt(comma, 2, left_expr, right_expr);
}

/* TODO(Robert): handle more complicated (nested list) initializers */

ASTree *typecheck_array_initializer(ASTree *equal_sign, ASTree *declarator,
                                    ASTree *init_list) {
  /* TODO(Robert): evaluate array size when generating three address code */
  const TypeSpec *array_type = declarator->type;
  TypeSpec element_type = SPEC_EMPTY;
  int status = strip_aux_type(&element_type, array_type);
  if (status) {
    return astree_create_errnode(astree_adopt(equal_sign, 2, declarator, init_list),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  ASTree *ret = astree_adopt(equal_sign, 2, declarator, init_list);
  size_t i;
  for (i = 0; i < astree_count(init_list); ++i) {
    ASTree *initializer = astree_get(init_list, i);
    ASTree *errnode = perform_pointer_conv(initializer);
    errnode = convert_type(errnode, &element_type);
    /* insert any successfully created conversion nodes into the tree, but do
     * not insert the error node, since that goes at the top of the tree */
    (void)astree_replace(init_list, i, UNWRAP(errnode));
    if (errnode->symbol == TOK_TYPE_ERROR) {
      /* remove initializer tree from errnode's list of children */
      (void)llist_extract(&errnode->children, 0);
      if (ret->symbol == TOK_TYPE_ERROR) {
        int status = typespec_append_auxspecs((TypeSpec *)ret->type,
                                              (TypeSpec *)errnode->type);
        /* TODO(Robert): check for and indicate errors */
        if (status) abort();
        status = astree_destroy(errnode);
        if (status) abort();
      } else {
        ret = astree_adopt(errnode, 1, ret);
      }
    }
  }
  return ret;
}

ASTree *typecheck_union_initializer(ASTree *equal_sign, ASTree *declarator,
                                    ASTree *init_list) {
  ASTree *identifier = declarator;
  AuxSpec *union_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = union_aux->data.tag.name;
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, strlen(tag_name), &tagval);
  /* tag was found previously when the declarator was validated, so if it is not
   * found now, there is a bug in the compiler, not the input.
   */
  assert(tagval != NULL);

  const LinkedList *members = &tagval->data.members.in_order;
  if (astree_count(init_list) > 1) {
    return astree_create_errnode(astree_adopt(equal_sign, 2, declarator, init_list),
                       BCC_TERR_EXCESS_INITIALIZERS, 1, declarator);
  } else {
    /* there should be one initializer of a type compatible with the type of the
     * first member of the union
     */
    ASTree *initializer = astree_get(init_list, 0);
    ASTree *errnode = perform_pointer_conv(initializer);
    SymbolValue *member_symbol = llist_front((LinkedList *)members);
    errnode = convert_type(errnode, &member_symbol->type);
    (void)astree_replace(init_list, 0, UNWRAP(errnode));
    if (errnode->symbol == TOK_TYPE_ERROR) {
      (void)llist_extract(&errnode->children, 0);
      return astree_adopt(errnode, 1,
                          astree_adopt(equal_sign, 2, declarator, init_list));
    } else {
      return astree_adopt(equal_sign, 2, declarator, init_list);
    }
  }
}

ASTree *typecheck_struct_initializer(ASTree *equal_sign, ASTree *declarator,
                                     ASTree *init_list) {
  ASTree *identifier = declarator;
  AuxSpec *struct_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = struct_aux->data.tag.name;
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, strlen(tag_name), &tagval);
  /* tag was found previously when the declarator was validated, so if it is not
   * found now, there is a bug in the compiler, not the input.
   */
  assert(tagval != NULL);

  const LinkedList *members = &tagval->data.members.in_order;
  if (members->size < astree_count(init_list)) {
    return astree_create_errnode(astree_adopt(equal_sign, 2, declarator, init_list),
                       BCC_TERR_EXCESS_INITIALIZERS, 1, declarator);
  } else {
    ASTree *ret = astree_adopt(equal_sign, 2, declarator, init_list);
    size_t i;
    for (i = 0; i < astree_count(init_list); ++i) {
      ASTree *initializer = astree_get(init_list, i);
      ASTree *errnode = perform_pointer_conv(initializer);
      SymbolValue *member_symbol = llist_get((LinkedList *)members, i);
      errnode = convert_type(errnode, &member_symbol->type);
      (void)astree_replace(init_list, i, UNWRAP(errnode));
      if (errnode->symbol == TOK_TYPE_ERROR) {
        /* remove initializer tree from errnode's list of children */
        (void)llist_extract(&errnode->children, 0);
        if (ret->symbol == TOK_TYPE_ERROR) {
          int status = typespec_append_auxspecs((TypeSpec *)ret->type,
                                                (TypeSpec *)errnode->type);
          /* TODO(Robert): check for and indicate errors */
          if (status) abort();
          status = astree_destroy(errnode);
          if (status) abort();
        } else {
          ret = astree_adopt(errnode, 1, ret);
        }
      }
    }
    return ret;
  }
}

ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src) {
  if (src->symbol == TOK_INIT_LIST) {
    const TypeSpec *decl_type = dest->type;
    if (typespec_is_array(decl_type)) {
      return typecheck_array_initializer(assignment, dest, src);
    } else if (decl_type->base == TYPE_UNION) {
      return typecheck_union_initializer(assignment, dest, src);
    } else if (decl_type->base == TYPE_STRUCT) {
      return typecheck_struct_initializer(assignment, dest, src);
    } else {
      return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                         BCC_TERR_UNEXPECTED_LIST, 2, dest, src);
    }
  } else {
    src = perform_pointer_conv(src);
    src = convert_type(src, dest->type);
    if (dest->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(assignment, 2, dest, src);
    } else if (src->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(assignment, 2, dest, src);
    }
    assignment->attributes |=
        src->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);

    assignment->type = dest->type;
    return astree_adopt(assignment, 2, dest, src);
  }
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

/*
 * Combines type specifier and declarator information and inserts symbol value
 * into the table at the current scope. Returns the declarator node passed in
 * as the second argument, or an error node enclosing this node. Can be safely
 * called with error nodes as arguments.
 */
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
    return astree_create_errnode(declarator, BCC_TERR_LIBRARY_FAILURE, 0);
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
        declarator->type = &SPEC_EMPTY;
        return astree_create_errnode(declarator, BCC_TERR_LIBRARY_FAILURE, 0);
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
    declarator->type = &SPEC_EMPTY;
    if (compatibility != TCHK_COMPATIBLE) {
      return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1, declarator);
    } else {
      return declarator;
    }
  } else {
    /* TODO(Robert): allow redefinition of extern symbols so long as types
     * are compatible
     */
    symbol_value_destroy(symbol);
    declarator->type = &SPEC_EMPTY;
    return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1, declarator);
  }
}

ASTree *finalize_declaration(ASTree *declaration) {
  if (declaration == &EMPTY_EXPR) return declaration;
  ASTree *errnode = NULL;
  if (declaration->symbol == TOK_TYPE_ERROR) {
    errnode = declaration;
    declaration = astree_get(declaration, 0);
  }
  ASTree *spec_list = astree_get(declaration, 0);
  if (spec_list->type != &SPEC_EMPTY) {
    int status = typespec_destroy((TypeSpec *)spec_list->type);
    free((TypeSpec *)spec_list->type);
    spec_list->type = &SPEC_EMPTY;
  }
  return errnode != NULL ? errnode : declaration;
}

ASTree *validate_array_size(ASTree *array, ASTree *expr) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(array, expr);
  }
  if ((expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST)) == 0) {
    return astree_create_errnode(astree_adopt(array, 1, expr), BCC_TERR_EXPECTED_INTCONST,
                       2, array, expr);
  }
  return astree_adopt(array, 1, expr);
}

ASTree *validate_param_list(ASTree *param_list) {
  param_list->symbol_table = symbol_table_init();
  int status = state_push_table(state, param_list->symbol_table);
  if (status)
    return astree_create_errnode(param_list, BCC_TERR_LIBRARY_FAILURE, 0);
  else
    return param_list;
}

ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator) {
  declarator = validate_declaration(declaration, declarator);
  if (param_list->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(param_list, astree_propogate_errnode(declaration, declarator));
  }

  return astree_adopt(param_list, 1, astree_adopt(declaration, 1, declarator));
}

ASTree *finalize_param_list(ASTree *param_list) {
  int status = state_pop_table(state);
  if (status)
    return astree_create_errnode(param_list, BCC_TERR_LIBRARY_FAILURE, 0);
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
      return astree_create_errnode(astree_adopt(declarator, 1, param_list),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    }
  }
  TypeSpec *out = (TypeSpec *)declarator->type;
  int status = llist_push_back(&out->auxspecs, aux_function);
  if (status) {
    return astree_create_errnode(astree_adopt(declarator, 1, param_list),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return astree_adopt(declarator, 1, param_list);
}

ASTree *define_array(ASTree *declarator, ASTree *array) {
  TypeSpec *spec = (TypeSpec *)declarator->type;
  if (typespec_is_incomplete(spec)) {
    return astree_create_errnode(astree_adopt(declarator, 1, array),
                       BCC_TERR_INCOMPLETE_TYPE, 2, array, spec);
  }
  AuxSpec *aux_array = calloc(1, sizeof(*aux_array));
  aux_array->aux = AUX_ARRAY;
  /* TODO(Robert): evaluate array size during three address code generation */
  /* set array size to any nonzero value, for now */
  aux_array->data.memory_loc.length = -1;
  int status = llist_push_back(&spec->auxspecs, aux_array);
  if (status) {
    free(aux_array);
    return astree_create_errnode(astree_adopt(declarator, 1, array),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  } else {
    return astree_adopt(declarator, 1, array);
  }
}

ASTree *define_pointer(ASTree *declarator, ASTree *pointer) {
  if (declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declarator, pointer);
  }
  AuxSpec *aux_pointer = calloc(1, sizeof(*aux_pointer));
  aux_pointer->aux = AUX_POINTER;
  TypeSpec *spec = (TypeSpec *)declarator->type;
  int status = llist_push_back(&spec->auxspecs, aux_pointer);
  if (status) {
    free(aux_pointer);
    return astree_create_errnode(astree_adopt(declarator, 1, pointer),
                       BCC_TERR_LIBRARY_FAILURE, 0);
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
    return astree_propogate_errnode(declarator, dirdecl);
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
      return astree_propogate_errnode(declarator, dirdecl);
    default:
      return astree_create_errnode(astree_adopt(declarator, 1, dirdecl),
                         BCC_TERR_UNEXPECTED_TOKEN, 1, dirdecl);
  }
}

ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr) {
  expr = perform_pointer_conv(expr);
  if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  } else if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  }

  ASTree *type_name = astree_get(declaration, 1);
  int compatibility = types_compatible(type_name->type, expr->type);
  if (compatibility == TCHK_INCOMPATIBLE) {
    return astree_create_errnode(astree_adopt(cast, 2, declaration, expr),
                       BCC_TERR_INCOMPATIBLE_TYPES, 3, cast, type_name->type,
                       expr->type);
  } else {
    cast->type = type_name->type;
    cast->attributes |=
        expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
    return astree_adopt(cast, 2, declaration, expr);
  }
}

ASTree *typecheck_addop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    int status = determine_conversion(left_type, right_type, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left_type,
                         right_type);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
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
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left_type,
                         right_type);
    }
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                       BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left_type,
                       right_type);
  }
}

ASTree *typecheck_logop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_scalar(left->type) && typespec_is_scalar(right->type)) {
    operator->type = & SPEC_INT;
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                       BCC_TERR_EXPECTED_SCALAR, 3, operator, left, right);
  }
}

ASTree *typecheck_relop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;
  operator->type = & SPEC_INT;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    const TypeSpec *common_type;
    int status = determine_conversion(left_type, right_type, &common_type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left->type,
                         right->type);
    }
    left = convert_type(left, common_type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, common_type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type)) {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE || typespec_is_voidptr(left_type) ||
        typespec_is_voidptr(right_type)) {
      return astree_adopt(operator, 2, left, right);
    } else {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left->type,
                         right->type);
    }
  } else if (((typespec_is_pointer(left_type) && is_const_zero(right)) ||
              (is_const_zero(left) && typespec_is_pointer(right_type))) &&
             (operator->symbol == TOK_EQ || operator->symbol == TOK_NE)) {
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                       BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left->type,
                       right->type);
  }
}

ASTree *typecheck_mulop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_arithmetic(left->type) &&
      typespec_is_arithmetic(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left->type,
                         right->type);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                       BCC_TERR_EXPECTED_ARITHMETIC, 3, operator, left, right);
  }
}

ASTree *typecheck_shfop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, &SPEC_INT, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left->type,
                         &SPEC_INT);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    /* promote right operand independently of left */
    const TypeSpec *dummy;
    status = determine_conversion(right->type, &SPEC_INT, &dummy);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, right->type,
                         &SPEC_INT);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                       BCC_TERR_EXPECTED_INTEGER, 3, operator, left, right);
  }
}

ASTree *typecheck_bitop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, left->type,
                         right->type);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                       BCC_TERR_EXPECTED_INTEGER, 3, operator, left, right);
  }
}

ASTree *validate_binop(ASTree *operator, ASTree * left_operand,
                       ASTree *right_operand) {
  DEBUGS('t', "Validating binary operator %c", operator->symbol);
  left_operand = perform_pointer_conv(left_operand);
  right_operand = perform_pointer_conv(right_operand);

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
      result =
          astree_create_errnode(astree_adopt(operator, 2, left_operand, right_operand),
                      BCC_TERR_UNEXPECTED_TOKEN, 1, operator);
  }

  unsigned int result_attrs =
      UNWRAP(left_operand)->attributes & UNWRAP(right_operand)->attributes;
  UNWRAP(result)->attributes |=
      result_attrs & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);
  return result;
}

int is_increment(const int symbol) {
  return symbol == TOK_INC || symbol == TOK_DEC || symbol == TOK_POST_INC ||
         symbol == TOK_POST_DEC;
}

ASTree *validate_unop(ASTree *operator, ASTree * operand) {
  DEBUGS('t', "Validating unary operator %c", operator->symbol);
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }
  const TypeSpec *operand_type = operand->type;

  if (is_increment(operator->symbol) && !typespec_is_scalar(operand_type)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                       BCC_TERR_EXPECTED_SCALAR, 2, operator, operand);
  } else if ((operator->symbol == TOK_NEG || operator->symbol == TOK_POS) &&
             !typespec_is_arithmetic(operand_type)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                       BCC_TERR_EXPECTED_ARITHMETIC, 2, operator, operand);
  } else if (operator->symbol == '~' && !typespec_is_integer(operand_type)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                       BCC_TERR_EXPECTED_INTEGER, 2, operator, operand);
  } else if (operator->symbol == '!') {
    if (typespec_is_scalar(operand_type)) {
      operator->type = & SPEC_INT;
      return astree_adopt(operator, 1, operand);
    } else {
      return astree_create_errnode(astree_adopt(operator, 1, operand),
                         BCC_TERR_EXPECTED_SCALAR, 2, operand, operand);
    }
  } else {
    if (!is_increment(operator->symbol)) {
      operand = perform_pointer_conv(operand);
      if (operand->symbol == TOK_TYPE_ERROR) {
        return astree_propogate_errnode(operator, operand);
      }
    }
    int status = determine_conversion(operand_type, &SPEC_INT, &operator->type);
    if (status)
      return astree_create_errnode(astree_adopt(operator, 1, operand),
                         BCC_TERR_INCOMPATIBLE_TYPES, 3, operator, operand_type,
                         &SPEC_INT);
    operand = convert_type(operand, operator->type);
    if (operand->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode(operator, operand);
    }
    return astree_adopt(operator, 1, operand);
  }
}

ASTree *validate_indirection(ASTree *indirection, ASTree *operand) {
  operand = perform_pointer_conv(operand);
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(indirection, operand);
  }

  if (typespec_is_pointer(operand->type)) {
    TypeSpec *indirection_spec = malloc(sizeof(*indirection_spec));
    int status = strip_aux_type(indirection_spec, operand->type);
    if (status) {
      return astree_create_errnode(astree_adopt(indirection, 1, operand),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    }
    indirection->type = indirection_spec;
    return astree_adopt(indirection, 1, operand);
  } else {
    return astree_create_errnode(astree_adopt(indirection, 1, operand),
                       BCC_TERR_EXPECTED_POINTER, 2, indirection, operand);
  }
}

ASTree *validate_addrof(ASTree *addrof, ASTree *operand) {
  /* TODO(Robert): check that operand is an lval */
  /* TODO(Robert): set constexpr attribute if operand is static/extern */
  if (operand->symbol == TOK_TYPE_ERROR) return astree_propogate_errnode(addrof, operand);
  TypeSpec *addrof_spec = malloc(sizeof(*addrof_spec));
  int status = typespec_copy(addrof_spec, operand->type);
  if (status) {
    return astree_create_errnode(astree_adopt(addrof, 1, operand),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  llist_push_front(&addrof_spec->auxspecs, ptr_aux);
  addrof->type = addrof_spec;
  return astree_adopt(addrof, 1, operand);
}

ASTree *validate_sizeof(ASTree *sizeof_, ASTree *type_node) {
  const TypeSpec *spec = NULL;
  if (type_node->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(sizeof_, type_node);
  } else if (type_node->symbol == TOK_DECLARATION) {
    spec = astree_get(type_node, 1)->type;
  } else {
    spec = type_node->type;
  }

  if (typespec_is_incomplete(spec)) {
    return astree_create_errnode(astree_adopt(sizeof_, 1, type_node),
                       BCC_TERR_INCOMPLETE_TYPE, 2, sizeof_, spec);
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
    return astree_propogate_errnode_v(subscript, 2, pointer, index);
  }

  index = perform_pointer_conv(index);
  if (index->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(subscript, 2, pointer, index);
  }

  if (!typespec_is_pointer(pointer->type)) {
    return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                       BCC_TERR_EXPECTED_POINTER, 2, subscript, pointer);
  } else if (!typespec_is_integer(index->type)) {
    return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                       BCC_TERR_EXPECTED_INTEGER, 2, subscript, index);
  } else {
    TypeSpec *subscript_spec = malloc(sizeof(*subscript_spec));
    int status = strip_aux_type(subscript_spec, pointer->type);
    if (status)
      return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    subscript->type = subscript_spec;
    return astree_adopt(subscript, 2, pointer, index);
  }
}

ASTree *validate_reference(ASTree *reference, ASTree *struct_,
                           ASTree *member_name_node) {
  const TypeSpec *struct_type = struct_->type;
  if (!typespec_is_struct(struct_type) && !typespec_is_union(struct_type)) {
    return astree_create_errnode(astree_adopt(reference, 2, struct_, member_name_node),
                       BCC_TERR_EXPECTED_TAG, 2, reference, struct_);
  }

  const char *member_name = member_name_node->lexinfo;
  const size_t member_name_len = strlen(member_name);
  AuxSpec *struct_aux = llist_front(&struct_type->auxspecs);
  SymbolTable *member_table = struct_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return astree_create_errnode(astree_adopt(reference, 2, struct_, member_name_node),
                       BCC_TERR_SYM_NOT_FOUND, 1, member_name_node);
  } else {
    reference->type = &symval->type;
    return astree_adopt(reference, 2, struct_, member_name_node);
  }
}

ASTree *validate_arrow(ASTree *arrow, ASTree *struct_,
                       ASTree *member_name_node) {
  struct_ = perform_pointer_conv(struct_);
  if (struct_->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(arrow, 2, struct_, member_name_node);
  }

  const TypeSpec *struct_type = struct_->type;
  if (!typespec_is_structptr(struct_type) &&
      !typespec_is_unionptr(struct_type)) {
    return astree_create_errnode(astree_adopt(arrow, 2, struct_, member_name_node),
                       BCC_TERR_EXPECTED_TAG_PTR, 2, arrow, struct_type);
  }
  const char *member_name = member_name_node->lexinfo;
  const size_t member_name_len = strlen(member_name);
  /* first auxtype is pointer; second is struct/union */
  AuxSpec *strunion_aux = llist_get(&struct_type->auxspecs, 1);
  SymbolTable *member_table = strunion_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return astree_create_errnode(astree_adopt(arrow, 2, struct_, member_name_node),
                       BCC_TERR_SYM_NOT_FOUND, 1, member_name_node);
  } else {
    arrow->type = &symval->type;
    return astree_adopt(arrow, 2, struct_, member_name_node);
  }
}

ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer) {
  declarator = validate_declaration(declaration, declarator);
  if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(
        declaration, 1,
        astree_propogate_errnode_v(equal_sign, 2, declarator, initializer));
  }
  equal_sign = validate_assignment(equal_sign, declarator, initializer);
  if (equal_sign->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declaration, equal_sign);
  }
  return astree_adopt(declaration, 1, equal_sign);
}

ASTree *validate_return(ASTree *ret, ASTree *expr) {
  SymbolValue *symval = state_get_function(state);
  TypeSpec ret_spec = SPEC_EMPTY;
  int status = strip_aux_type(&ret_spec, &symval->type);
  if (status) {
    typespec_destroy(&ret_spec);
    return astree_create_errnode(astree_adopt(ret, 1, expr), BCC_TERR_FAILURE, 0);
  }
  if (expr != &EMPTY_EXPR) {
    expr = perform_pointer_conv(expr);
    if (expr->symbol == TOK_TYPE_ERROR) {
      typespec_destroy(&ret_spec);
      return astree_propogate_errnode(ret, expr);
    }
    expr = convert_type(expr, &ret_spec);
    if (expr->symbol == TOK_TYPE_ERROR) {
      typespec_destroy(&ret_spec);
      return astree_propogate_errnode(ret, expr);
    }
    typespec_destroy(&ret_spec);
    return astree_adopt(ret, 1, expr);
  } else {
    int compatibility = types_compatible(&ret_spec, &SPEC_VOID);
    if (compatibility != TCHK_COMPATIBLE) {
      typespec_destroy(&ret_spec);
      return astree_create_errnode(astree_adopt(ret, 1, expr), BCC_TERR_EXPECTED_RETVAL,
                         0);
    }
    typespec_destroy(&ret_spec);
    return astree_adopt(ret, 1, expr);
  }
}

int resolve_label(ASTree *ident_node) {
  const char *ident = ident_node->lexinfo;
  size_t ident_len = strnlen(ident, MAX_IDENT_LEN);
  LabelValue *labval = state_get_label(state, ident, ident_len);
  if (labval == NULL) {
    return BCC_TERR_SYM_NOT_FOUND;
  } else {
    return BCC_TERR_SUCCESS;
  }
}

ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body) {
  /* TODO(Robert): make sure that the declaration location is that of the
   * earliest forward declaration, but that the names of the parameters are
   * from the actual definition of the function.
   */
  declarator = validate_declaration(declaration, declarator);
  if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(declaration, 2, declarator, body);
  } else if (declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(declaration, 2, declarator, body);
  }

  SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
  if (symval == NULL) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_FAILURE, 0);
  } else if (!typespec_is_function(&symval->type)) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_EXPECTED_FUNCTION, 1, declarator);
  } else if (symval->flags & SYMFLAG_FUNCTION_DEFINED) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_REDEFINITION, 1, declarator);
  }

  /* param list should be first child for properly defined functions */
  ASTree *param_list = astree_get(declarator, 0);
  assert(param_list->symbol == TOK_PARAM_LIST);
  /* treat body like a normal block statement, but move param table to body node
   * and define function before entering body scope */
  body->symbol_table = param_list->symbol_table;
  param_list->symbol_table = NULL;
  int status = state_push_table(state, body->symbol_table);
  if (status) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }

  /* TODO(Robert): set function to a dummy value even in the event of failure so
   * that return statements in the body may be processed in some way
   */
  status = state_set_function(state, symval);
  if (status)
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_FAILURE, 0);
  return astree_adopt(declaration, 2, declarator, body);
}

int merge_block_controls(ASTree *block, ASTree *stmt) {
  ASTree *sub_block = NULL;
  switch (stmt->symbol) {
    case TOK_FOR:
    case TOK_WHILE:
    case TOK_SWITCH:
      if (astree_get(stmt, 1)->symbol == TOK_BLOCK)
        sub_block = astree_get(stmt, 1);
      break;
    case TOK_DO:
      if (astree_get(stmt, 0)->symbol == TOK_BLOCK)
        sub_block = astree_get(stmt, 0);
      break;
    case TOK_BLOCK:
      sub_block = stmt;
      break;
    default:
      break;
  }

  if (sub_block != NULL) {
    return symbol_table_merge_control(block->symbol_table,
                                      sub_block->symbol_table);
  } else {
    return 0;
  }
}

ASTree *validate_fnbody_content(ASTree *function, ASTree *fnbody_content) {
  /* we can't reuse validate_block_content here because all that function does
   * is perform adoption and propogate errors, both of which are different here
   * because of the tree structure of function definitions
   */
  ASTree *fnbody = astree_get(UNWRAP(function), 2);
  (void)astree_adopt(fnbody, 1, UNWRAP(fnbody_content));
  if (fnbody_content->symbol == TOK_TYPE_ERROR) {
    (void)astree_remove(fnbody_content, 0);
    if (function->symbol == TOK_TYPE_ERROR) {
      /* TODO(Robert): copy-pasted from astree_propogate_errnode; deduplicate later */
      TypeSpec *parent_errs = (TypeSpec *)function->type;
      TypeSpec *child_errs = (TypeSpec *)fnbody_content->type;
      int status = typespec_append_auxspecs(parent_errs, child_errs);
      /* TODO(Robert): be a man */
      if (status) abort();
      status = astree_destroy(fnbody_content);
      if (status) abort();
      return function;
    } else {
      return astree_adopt(fnbody_content, 1, function);
    }
  }

  /* TODO(Robert): handle control flow statements even in the case of errors */
  int status = merge_block_controls(fnbody, fnbody_content);
  if (status) {
    return astree_create_errnode(astree_adopt(fnbody, 1, fnbody_content),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return function;
}

ASTree *finalize_function(ASTree *function) {
  ASTree *body = astree_get(UNWRAP(function), 2);
  ASTree *ret = function;
  SymbolTable *table = body->symbol_table;
  SymbolValue *symval = state_get_function(state);
  size_t i;
  for (i = 0; i < symbol_table_count_control(table); ++i) {
    ControlValue *ctrlval = symbol_table_get_control(table, i);
    /* TODO(Robert): create label information */
    if (ctrlval->type == CTRL_GOTO) {
      int status = resolve_label(ctrlval->tree);
      if (status) {
        ret = astree_create_errnode(ret, BCC_TERR_SYM_NOT_FOUND, 1, ctrlval->tree);
      }
      symbol_table_remove_control(table, i--);
      free(ctrlval);
    } else {
      ret = astree_create_errnode(ret, BCC_TERR_UNEXPECTED_TOKEN, 1, ctrlval->tree);
      symbol_table_remove_control(table, i--);
      free(ctrlval);
    }
  }
  symval->flags |= SYMFLAG_FUNCTION_DEFINED;
  int status = state_unset_function(state);
  if (status) ret = astree_create_errnode(ret, BCC_TERR_FAILURE, 0);
  /* do not use finalize_block because it will put the error in an awkward place
   */
  status = state_pop_table(state);
  if (status) ret = astree_create_errnode(ret, BCC_TERR_LIBRARY_FAILURE, 0);
  return ret;
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
      return astree_create_errnode(
          astree_adopt(tag_type_node, 2, tag_name_node, left_brace),
          errcode_from_tagtype(exists->tag), 2, tag_type_node, tag_name_node);
    } else if (tag_declares_members) {
      if (exists->is_defined) {
        return astree_create_errnode(
            astree_adopt(tag_type_node, 2, tag_name_node, left_brace),
            BCC_TERR_REDEFINITION, 1, tag_name_node);
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
  tag->type = &SPEC_EMPTY;
  return errnode == NULL ? tag : errnode;
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
      return astree_create_errnode(enum_, BCC_TERR_REDEFINITION, 1, ident_node);
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return astree_create_errnode(enum_, BCC_TERR_REDEFINITION, 1, ident_node);
    }
  }

  SymbolValue *symval =
      symbol_value_init(&ident_node->loc, state_get_sequence(state));
  int status = typespec_init(&symval->type);
  if (status) {
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return astree_create_errnode(enum_, BCC_TERR_LIBRARY_FAILURE, 0);
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return astree_create_errnode(enum_, BCC_TERR_LIBRARY_FAILURE, 0);
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
      return astree_create_errnode(enum_, BCC_TERR_LIBRARY_FAILURE, 0);
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return astree_create_errnode(enum_, BCC_TERR_LIBRARY_FAILURE, 0);
    }
  }

  ident_node->type = &symval->type;

  if (equal_sign != NULL) {
    /* TODO(Robert): evaluate enumeration constants */
    if (expr->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode =
          astree_propogate_errnode(astree_adopt(equal_sign, 1, ident_node), expr);
      /* TODO(Robert): have error propogation function handle more complex
       * syntax tree structures
       */
      return astree_propogate_errnode(enum_, errnode);
    }
    if ((expr->attributes & ATTR_EXPR_ARITHCONST) == 0) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return astree_create_errnode(enum_, BCC_TERR_EXPECTED_ARITHCONST, 2, equal_sign,
                         expr);
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
    ASTree *left_brace = astree_get(UNWRAP(struct_), 1);
    (void)astree_adopt(left_brace, 1, UNWRAP(member));
    if (member->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = member;
      (void)llist_extract(&member->children, 0);
      int status = typespec_append_auxspecs((TypeSpec *)struct_->type,
                                            (TypeSpec *)errnode->type);
      if (status) abort();
      status = astree_destroy(errnode);
      if (status) abort();
    }
    return struct_;
  } else if (member->symbol == TOK_TYPE_ERROR) {
    ASTree *left_brace = astree_get(struct_, 1);
    (void)astree_adopt(left_brace, 1, llist_extract(&member->children, 0));
    return astree_adopt(member, 1, struct_);
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
    return astree_propogate_errnode(declaration, err_or_decl);
  } else if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declaration, err_or_decl);
  } else {
    return astree_adopt(declaration, 1, declarator);
  }
}

ASTree *validate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                        ASTree *else_body) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  } else if (if_body->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  } else if (else_body->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(ifelse, 3, condition, if_body, else_body),
                       BCC_TERR_EXPECTED_SCALAR, 2, ifelse, condition);
  }

  return astree_adopt(ifelse, 3, condition, if_body, else_body);
}

ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(switch_, 2, expr, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(switch_, 2, expr, stmt);
  }
  if (stmt->symbol == TOK_BLOCK) {
    SymbolTable *table = stmt->symbol_table;
    size_t i;
    for (i = 0; i < symbol_table_count_control(table); ++i) {
      ControlValue *ctrlval = symbol_table_get_control(table, i);
      /* TODO(Robert): create label information */
      if (ctrlval->type == CTRL_BREAK) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      } else if (ctrlval->type == CTRL_CASE) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      } else if (ctrlval->type == CTRL_DEFAULT) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      }
    }
  } else if (stmt->symbol == CTRL_CASE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_DEFAULT) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  if (!typespec_is_integer(expr->type)) {
    return astree_create_errnode(astree_adopt(switch_, 2, expr, stmt),
                       BCC_TERR_EXPECTED_INTEGER, 2, switch_, expr);
  }

  return astree_adopt(switch_, 2, expr, stmt);
}

ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt) {
  /* TODO(Robert): safely process flow control statements before checking error
   * codes so that more things are cleaned up in the event of an error.
   */
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(while_, 2, condition, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(while_, 2, condition, stmt);
  }

  if (stmt->symbol == TOK_BLOCK) {
    SymbolTable *table = stmt->symbol_table;
    size_t i;
    for (i = 0; i < symbol_table_count_control(table); ++i) {
      ControlValue *ctrlval = symbol_table_get_control(table, i);
      /* TODO(Robert): create label information */
      if (ctrlval->type == CTRL_BREAK) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      } else if (ctrlval->type == CTRL_CONTINUE) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      }
    }
  } else if (stmt->symbol == CTRL_CONTINUE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(while_, 2, condition, stmt),
                       BCC_TERR_EXPECTED_INTEGER, 2, while_, condition);
  }
  return astree_adopt(while_, 2, condition, stmt);
}

ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(do_, 2, stmt, condition);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(do_, 2, stmt, condition);
  }

  if (stmt->symbol == TOK_BLOCK) {
    SymbolTable *table = stmt->symbol_table;
    size_t i;
    for (i = 0; i < symbol_table_count_control(table); ++i) {
      ControlValue *ctrlval = symbol_table_get_control(table, i);
      /* TODO(Robert): create label information */
      if (ctrlval->type == CTRL_BREAK) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      } else if (ctrlval->type == CTRL_CONTINUE) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      }
    }
  } else if (stmt->symbol == CTRL_CONTINUE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(do_, 2, condition, stmt),
                       BCC_TERR_EXPECTED_INTEGER, 2, do_, condition);
  }
  return astree_adopt(do_, 2, condition, stmt);
}

ASTree *validate_for_exprs(ASTree *left_paren, ASTree *init_expr,
                           ASTree *pre_iter_expr, ASTree *reinit_expr) {
  if (init_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(left_paren, 3, init_expr, pre_iter_expr,
                           reinit_expr);
  } else if (pre_iter_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(left_paren, 3, init_expr, pre_iter_expr,
                           reinit_expr);
  } else if (reinit_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(left_paren, 3, init_expr, pre_iter_expr,
                           reinit_expr);
  }

  if (pre_iter_expr != &EMPTY_EXPR) {
    if (!typespec_is_scalar(pre_iter_expr->type)) {
      return astree_create_errnode(
          astree_adopt(left_paren, 3, init_expr, pre_iter_expr, reinit_expr),
          BCC_TERR_EXPECTED_SCALCONST, 2, left_paren, pre_iter_expr);
    }
  }
  return astree_adopt(left_paren, 3, init_expr, pre_iter_expr, reinit_expr);
}

ASTree *validate_for(ASTree *for_, ASTree *left_paren, ASTree *stmt) {
  if (left_paren->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(for_, 2, left_paren, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(for_, 2, left_paren, stmt);
  }

  if (stmt->symbol == TOK_BLOCK) {
    SymbolTable *table = stmt->symbol_table;
    size_t i;
    for (i = 0; i < symbol_table_count_control(table); ++i) {
      ControlValue *ctrlval = symbol_table_get_control(table, i);
      /* TODO(Robert): create label information */
      if (ctrlval->type == CTRL_BREAK) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      } else if (ctrlval->type == CTRL_CONTINUE) {
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      }
    }
  } else if (stmt->symbol == CTRL_CONTINUE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  return astree_adopt(for_, 2, left_paren, stmt);
}

/* TODO(Robert): decide whether or not to create empty labels for goto
 * statements whose label has not been defined yet.
 */
ASTree *validate_label(ASTree *label, ASTree *ident_node, ASTree *stmt) {
  if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(astree_adopt(label, 1, ident_node), stmt);
  }

  const char *ident = ident_node->lexinfo;
  size_t ident_len = strlen(ident);
  LabelValue *existing_entry = state_get_label(state, ident, ident_len);
  if (existing_entry) {
    if (existing_entry->is_defined) {
      return astree_create_errnode(astree_adopt(label, 2, ident_node, stmt),
                         BCC_TERR_REDEFINITION, 1, ident_node);
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
      return astree_create_errnode(astree_adopt(label, 2, ident_node, stmt),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    }
    return astree_adopt(label, 2, ident_node, stmt);
  }
}

ASTree *validate_case(ASTree *case_, ASTree *expr, ASTree *stmt) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(case_, 2, expr, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(case_, 2, expr, stmt);
  }

  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_CASE;
  ctrlval->tree = case_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(astree_adopt(case_, 1, stmt), BCC_TERR_LIBRARY_FAILURE,
                       0);
  }

  const TypeSpec *case_const_spec = expr->type;
  if (!typespec_is_integer(case_const_spec) ||
      !(expr->attributes | ATTR_EXPR_ARITHCONST)) {
    return astree_create_errnode(astree_adopt(case_, 2, expr, stmt),
                       BCC_TERR_EXPECTED_INTCONST, 2, case_, expr);
  }

  return astree_adopt(case_, 2, expr, stmt);
}

ASTree *validate_default(ASTree *default_, ASTree *stmt) {
  if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(default_, stmt);
  }
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_DEFAULT;
  ctrlval->tree = default_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(astree_adopt(default_, 1, stmt),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return astree_adopt(default_, 1, stmt);
}

ASTree *validate_goto(ASTree *goto_, ASTree *ident) {
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_GOTO;
  ctrlval->tree = goto_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(astree_adopt(goto_, 1, ident), BCC_TERR_LIBRARY_FAILURE,
                       0);
  }
  return astree_adopt(goto_, 1, ident);
}

ASTree *validate_continue(ASTree *continue_) {
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_CONTINUE;
  ctrlval->tree = continue_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(continue_, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return continue_;
}

ASTree *validate_break(ASTree *break_) {
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_BREAK;
  ctrlval->tree = break_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(break_, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return break_;
}

ASTree *validate_block(ASTree *block) {
  block->symbol_table = symbol_table_init();
  int status = state_push_table(state, block->symbol_table);
  if (status) {
    return astree_create_errnode(block, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return block;
}

ASTree *validate_block_content(ASTree *block, ASTree *block_content) {
  if (block->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(block, block_content);
  } else if (block_content->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(block, block_content);
  }

  /* TODO(Robert): handle control flow statements even in the case of errors */
  int status = merge_block_controls(block, block_content);
  if (status) {
    return astree_create_errnode(astree_adopt(block, 1, block_content),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return astree_adopt(block, 1, block_content);
}

ASTree *finalize_block(ASTree *block) {
  int status = state_pop_table(state);
  if (status) {
    return astree_create_errnode(block, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return block;
}

ASTree *validate_topdecl(ASTree *root, ASTree *topdecl) {
  if (root->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else if (topdecl->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else {
    return astree_adopt(root, 1, topdecl);
  }
}
