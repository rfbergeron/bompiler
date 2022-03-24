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

/* forward declarations for mutual recursion */
int validate_expr(ASTree *expression, SymbolTable *table);
int validate_stmt(ASTree *statement, SymbolTable *table);
int validate_declaration(ASTree *statement,
                         SymbolTable *table); /* required to process params */
int types_compatible(
    const TypeSpec *type1,
    const TypeSpec *type2); /* required to check param and member types */

int assign_type(ASTree *tree, SymbolTable *table) {
  DEBUGS('t', "Attempting to assign a type");
  ASTree *identifier = extract_ident(tree);
  if (identifier == NULL) return -1;
  const char *id_str = identifier->lexinfo;
  size_t id_str_len = strnlen(id_str, MAX_IDENT_LEN);
  SymbolValue *symval = NULL;
  int in_current_scope = symbol_table_get(table, id_str, id_str_len, &symval);
  if (symval) {
    DEBUGS('t', "Assigning %s a symbol", id_str);
    identifier->type = &(symval->type);
    return 0;
  } else {
    fprintf(stderr, "ERROR: could not resolve symbol: %s %s\n", (tree->lexinfo),
            parser_get_tname(tree->symbol));
    return -1;
  }
}

/*
 * TODO(Robert): recursively setting the block number no longer works because
 * of nested scoping; instead block numbers should be set during the validation
 * of expressions, at which point it is not possible to further nest scopes.
 */

int convert_type(ASTree *tree, const TypeSpec *type) {
  /* since the linked list has a destructor defined for elements, we can't just
   * use llist_get and llist_delete here, since that will call astree_destroy
   * on the node. Instead we call extract, which removes the element from the
   * list and returns the value
   */
  int compatibility = types_compatible(type, extract_type(tree));
  /* TODO(Robert): warn about conversions that should be explicit */
  if (compatibility == TCHK_COMPATIBLE) {
    return 0;
  } else if (compatibility == TCHK_INCOMPATIBLE) {
    fprintf(stderr, "ERROR: attempt to convert between incompatible types.\n");
    return -1;
  }
  TypeSpec *cast_spec = malloc(sizeof(*cast_spec));
  int status = typespec_copy(cast_spec, type);
  if (status) return status;
  ASTree *cast = astree_init(TOK_CAST, tree->loc, "_cast");
  cast->type = cast_spec;
  astree_inject(tree, cast);
  return 0;
}

int perform_pointer_conv(ASTree *tree) {
  const TypeSpec *tree_spec = extract_type(tree);
  if (!typespec_is_array(tree_spec) && !typespec_is_function(tree_spec)) {
    return 0;
  } else {
    TypeSpec *pointer_spec = malloc(sizeof(*pointer_spec));
    if (typespec_is_array(tree_spec)) {
      int status = strip_aux_type(pointer_spec, tree_spec);
      if (status) return status;
    } else {
      int status = typespec_copy(pointer_spec, tree_spec);
      if (status) return status;
    }
    AuxSpec *pointer_aux = calloc(1, sizeof(*pointer_aux));
    pointer_aux->aux = AUX_POINTER;
    int status = llist_push_front(&pointer_spec->auxspecs, pointer_aux);
    if (status) return status;
    ASTree *cast = astree_init(TOK_CAST, tree->loc, "_cast");
    cast->type = pointer_spec;
    astree_inject(tree, cast);
    return 0;
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
  if (type1 == NULL) {
    fprintf(stderr, "First argument not provided to promotion routine\n");
    return -1;
  } else if (type2 == NULL) {
    fprintf(stderr, "Second argument not provided to promotion routine\n");
    return -1;
  } else if (typespec_is_pointer(type1)) {
    *out = type1;
  } else if (typespec_is_pointer(type2)) {
    *out = type2;
  } else if (type1->base == TYPE_STRUCT || type1->base == TYPE_UNION) {
    *out = type1;
  } else if (type2->base == TYPE_STRUCT || type2->base == TYPE_UNION) {
    fprintf(stderr,
            "ERROR: unable to promote struct or union type to other type\n");
    return -1;
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
    /* some invalid type was passed */
    fprintf(stderr, "ERORR: unable to determine promotion for types\n");
    return -1;
  }
  return 0;
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
    /* error: constant too small */
  } else if (signed_value == LONG_MAX) {
    unsigned long unsigned_value = strtoul(intcon->lexinfo, NULL, 10);
    if (unsigned_value == UINT64_MAX) {
      /* error: constant too large */
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
  return 0;
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
  return 0;
}

int validate_integer_typespec(TypeSpec *out, enum typespec_index i,
                              enum typespec_flag f, size_t bytes) {
  if (out->flags & INCOMPATIBLE_FLAGSETS[i]) {
    char buf[1024];
    flags_to_string(f, buf, 1024);
    char buf2[1024];
    flags_to_string(out->flags, buf2, 1024);
    fprintf(stderr,
            "ERROR: typespec flag \"%s\" incompatible with flagset \"%s\"\n",
            buf, buf2);
    return -1;
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
    return 0;
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

int validate_tag_typespec(ASTree *type, SymbolTable *table, TypeSpec *out) {
  ASTree *tag = astree_first(type);
  const char *tag_name = tag->lexinfo;
  size_t tag_name_len = strlen(tag_name);
  TagValue *tagval = symbol_table_get_tag(table, tag_name, tag_name_len);
  if (!tagval) {
    fprintf(stderr, "ERROR: structure type %s is not defined\n", tag_name);
    return -1;
  } else if (type->symbol == TOK_STRUCT && tagval->tag != TAG_STRUCT) {
    fprintf(stderr, "ERROR: type %s is not a struct\n", tag_name);
    return -1;
  } else if (type->symbol == TOK_UNION && tagval->tag != TAG_UNION) {
    fprintf(stderr, "ERROR: type %s is not a union\n", tag_name);
    return -1;
  } else if (type->symbol == TOK_ENUM && tagval->tag != TAG_ENUM) {
    fprintf(stderr, "ERROR: type %s is not an enum\n", tag_name);
    return -1;
  } else {
    out->base = type_from_tag(tagval->tag);
    out->width = tagval->width;
    out->alignment = tagval->alignment;

    if (out->auxspecs.anchor == NULL) {
      typespec_init(out);
    }

    AuxSpec *tag_aux = calloc(1, sizeof(*tag_aux));
    tag_aux->aux = aux_from_tag(tagval->tag);
    tag_aux->data.tag.name = tag_name;
    if (tagval->tag == TAG_STRUCT || tagval->tag == TAG_UNION) {
      tag_aux->data.tag.members.by_name = tagval->data.members.by_name;
      tag_aux->data.tag.members.in_order = &tagval->data.members.in_order;
    }
    llist_push_back(&out->auxspecs, tag_aux);
    return 0;
  }
}

int validate_typespec_list(ASTree *spec_list, SymbolTable *table,
                           TypeSpec *out) {
  int status = 0;
  size_t i;
  for (i = 0; i < astree_count(spec_list); ++i) {
    ASTree *type = astree_get(spec_list, i);
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
        status = validate_integer_typespec(
            out, TYPESPEC_INDEX_SHORT, TYPESPEC_FLAG_SHORT, X64_SIZEOF_SHORT);
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
        status = validate_tag_typespec(type, table, out);
        break;
      case TOK_CONST:
        break;
      case TOK_VOLATILE:
        break;
      case TOK_IDENT:
        break;
      default:
        fprintf(stderr, "ERROR: unimplemented type: %s\n",
                parser_get_tname(type->symbol));
        status = -1;
        break;
    }
  }

  if ((out->flags & (TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAG_CHAR)) &&
      !(out->flags & TYPESPEC_FLAGS_SIGNEDNESS)) {
    /* default to signed if not specified */
    out->base = TYPE_SIGNED;
  } else if ((out->flags & TYPESPEC_FLAGS_SIGNEDNESS) &&
             !(out->flags & (TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAG_CHAR))) {
    /* default to width of "int" if not specified */
    out->width = X64_SIZEOF_INT;
    out->alignment = X64_ALIGNOF_INT;
  } else if (out->base == TYPE_NONE) {
    /* error; only storage class and qualifiers provided */
    status = -1;
  }
  return status;
}

int validate_ident(ASTree *ident, SymbolTable *table) {
  return assign_type(ident, table);
}

int validate_call(ASTree *call, SymbolTable *table) {
  ASTree *function = astree_first(call);
  int status = validate_expr(function, table);
  if (status) return status;
  status = perform_pointer_conv(function);
  if (status) return status;

  TypeSpec *function_spec = (TypeSpec *)extract_type(function);
  if (!typespec_is_fnptr(function_spec)) {
    char buf[4096];
    int chars_written = type_to_string(function_spec, buf, 4096);
    if (chars_written > 4096) {
      abort();
    }

    fprintf(stderr,
            "ERROR: cannot call non-function %s, whose type is \"%s\"\n",
            extract_ident(function)->lexinfo, buf);
    return -1;
  }

  /* second auxspec will be the function; first is pointer */
  AuxSpec *param_spec = llist_get(&function_spec->auxspecs, 1);
  LinkedList *param_list = param_spec->data.params;
  /* subtract one since function identifier is also a child */
  if (astree_count(call) - 1 != llist_size(param_list)) {
    fprintf(stderr, "ERROR: incorrect number of arguments for function call\n");
    return -1;
  }

  DEBUGS('t', "Validating %d arguments for function call",
         astree_count(call) - 1);
  size_t i;
  for (i = 0; i < llist_size(param_list); ++i) {
    DEBUGS('t', "Validating argument %d", i);
    /* add 1 to index to skip function identifier */
    ASTree *call_param = astree_get(call, i + 1);
    int status = validate_expr(call_param, table);
    if (status != 0) return status;
    status = perform_pointer_conv(call_param);
    if (status) return status;

    DEBUGS('t', "Comparing types");
    SymbolValue *param_symval = llist_get(param_list, i);
    int compatibility =
        types_compatible(&param_symval->type, extract_type(call_param));
    if (compatibility == TCHK_INCOMPATIBLE ||
        compatibility == TCHK_EXPLICIT_CAST) {
      fprintf(stderr, "ERROR: incompatible type for argument: %s\n",
              call_param->lexinfo);
      return -1;
    }
  }

  /* strip pointer */
  TypeSpec temp_spec = SPEC_EMPTY;
  status = strip_aux_type(&temp_spec, function_spec);
  if (status) return status;
  /* strip function */
  TypeSpec *return_spec = malloc(sizeof(*return_spec));
  status = strip_aux_type(return_spec, function_spec);
  if (status) return status;
  /* free temporaries created by stripping */
  typespec_destroy(&temp_spec);
  call->type = return_spec;
  return 0;
}

int validate_assignment(ASTree *assignment, SymbolTable *table) {
  ASTree *dest = astree_first(assignment);
  int status = validate_expr(dest, table);
  if (status) return status;

  ASTree *src = astree_second(assignment);
  status = validate_expr(src, table);
  if (status) return status;
  status = perform_pointer_conv(src);
  if (status) return status;

  assignment->type = dest->type;
  return convert_type(src, dest->type);
}

int define_params(ASTree *params, ASTree *ident, SymbolTable *file_table,
                  TypeSpec *out) {
  AuxSpec *aux_function = calloc(1, sizeof(*aux_function));
  aux_function->aux = AUX_FUNCTION;
  aux_function->data.params = malloc(sizeof(*aux_function->data.params));
  LinkedList *param_entries = aux_function->data.params;
  /* type is not resposible for freeing symbol information */
  llist_init(param_entries, NULL, NULL);
  /* remember to use function_table and not file_table */
  SymbolTable *function_table = symbol_table_init(file_table);
  ident->symbol_table = function_table;
  size_t i;
  for (i = 0; i < astree_count(params); ++i) {
    ASTree *param = astree_get(params, i);
    ASTree *spec = astree_get(param, 0);
    ASTree *declarator = astree_get(param, 1);
    const char *param_id_str = extract_ident(declarator)->lexinfo;
    DEBUGS('t', "Defining function parameter %s", param_id_str);
    int status = validate_declaration(param, function_table);
    if (status) return status;
    size_t param_id_str_len = strnlen(param_id_str, MAX_IDENT_LEN);
    SymbolValue *param_entry = NULL;
    int param_defined = symbol_table_get(function_table, param_id_str,
                                         param_id_str_len, &param_entry);
    if (!param_defined) {
      fprintf(stderr,
              "ERROR: parameter symbol was not inserted into function table\n");
      return -1;
    }
    status = llist_push_back(param_entries, param_entry);
    if (status) return status;
  }
  return llist_push_back(&out->auxspecs, aux_function);
}

int define_array(ASTree *array, TypeSpec *spec) {
  AuxSpec *aux_array = calloc(1, sizeof(*aux_array));
  aux_array->aux = AUX_ARRAY;
  if (astree_count(array) > 0) {
    aux_array->data.memory_loc.length =
        strtoumax(astree_first(array)->lexinfo, NULL, 0);
    if (aux_array->data.memory_loc.length == ULONG_MAX) {
      fprintf(stderr, "ERROR: array size too large\n");
      return -1;
    }
  }
  return llist_push_back(&spec->auxspecs, aux_array);
}

int define_pointer(ASTree *pointer, TypeSpec *spec) {
  AuxSpec *aux_pointer = calloc(1, sizeof(*aux_pointer));
  aux_pointer->aux = AUX_POINTER;
  return llist_push_back(&spec->auxspecs, aux_pointer);
}

int validate_dirdecl(ASTree *dirdecl, ASTree *ident, SymbolTable *table,
                     TypeSpec *out) {
  /* TODO(Robert): do not allow multiple function dirdecls to occur, and do not
   * allow functions to return array types
   */
  /*
  AuxSpec *prev = llist_back(&symbol->type.auxspecs);
  if (prev && prev->aux == AUX_FUNCTION) {
    fprintf(stderr,
            "ERROR: function declarator must be last in list of direct "
            "declarators\n");
    return -1;
  } else if (prev && prev->aux == AUX_ARRAY &&
             dirdecl->symbol == TOK_FUNCTION) {
    fprintf(stderr, "ERROR: function and array specifiers may not co-occur\n");
    return -1;
  }
  */
  /* TODO(Robert): possibly add external function to check validity of linked
   * list and other badlib data structures
   * TODO(Robert): initialize auxspec list in a more predictable and centralized
   * way, and rename or repurpose typespec_init to make it more clear what it
   * does
   */
  if (out->auxspecs.anchor == NULL) {
    typespec_init(out);
  }

  switch (dirdecl->symbol) {
    case TOK_DECLARATOR: {
      size_t i;
      for (i = 0; i < astree_count(dirdecl); ++i) {
        int status =
            validate_dirdecl(astree_get(dirdecl, i), ident, table, out);
        if (status) return status;
      }
    }
      return 0;
      break;
    case TOK_IDENT:
      /* do nothing */
      return 0;
      break;
    case TOK_ARRAY:
      return define_array(dirdecl, out);
      break;
    case TOK_POINTER:
      return define_pointer(dirdecl, out);
      break;
    case TOK_FUNCTION:
      return define_params(dirdecl, ident, table, out);
      break;
    default:
      fprintf(stderr, "ERROR: invalid direct declarator: %s\n",
              parser_get_tname(dirdecl->symbol));
      return -1;
      break;
  }
}

int validate_cast(ASTree *cast, SymbolTable *table) {
  ASTree *spec_list = astree_first(cast);
  TypeSpec *spec = calloc(1, sizeof(*cast->type));
  cast->type = spec;
  ASTree *expr = NULL;

  if (astree_second(cast)->symbol == TOK_DECLARATOR) {
    ASTree *abstract_decl = astree_second(cast);
    expr = astree_third(cast);
    size_t i;
    for (i = 0; i < astree_count(abstract_decl); ++i) {
      int status =
          validate_dirdecl(astree_get(abstract_decl, i), cast, table, spec);
      if (status) return status;
    }
  } else {
    expr = astree_second(cast);
  }

  int status = validate_typespec_list(spec_list, table, spec);
  if (status) return status;
  status = validate_expr(expr, table);
  if (status) return status;
  status = perform_pointer_conv(expr);
  if (status) return status;

  int compatibility = types_compatible(extract_type(cast), extract_type(expr));
  if (compatibility == TCHK_INCOMPATIBLE)
    return -1;
  else
    return 0;
}

int typecheck_addop(ASTree *operator, ASTree * left, ASTree *right) {
  const TypeSpec *left_type = extract_type(left);
  const TypeSpec *right_type = extract_type(right);

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    int status = determine_conversion(left_type, right_type, &operator->type);
    if (status) return status;
    status = convert_type(left, operator->type);
    if (status) return status;
    return convert_type(right, operator->type);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_integer(right_type)) {
    operator->type = left_type;
    return 0;
  } else if (typespec_is_integer(left_type) &&
             typespec_is_pointer(right_type)) {
    operator->type = right_type;
    return 0;
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type) && operator->symbol == '-') {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE) {
      /* types should be the same; just pick the left one */
      operator->type = left_type;
      return 0;
    } else {
      fprintf(stderr, "ERROR: Cannot subtract incompatible pointer types.\n");
      return -1;
    }
  } else {
    fprintf(stderr, "ERROR: Incompatible operands for operator \"%s\".\n",
            parser_get_tname(operator->symbol));
    return -1;
  }
}

int typecheck_logop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_scalar(extract_type(left)) &&
      typespec_is_scalar(extract_type(right))) {
    operator->type = & SPEC_INT;
    return 0;
  } else {
    fprintf(stderr,
            "ERROR: cannot evaluate a non-scalar type as boolean value.\n");
    return -1;
  }
}

int typecheck_relop(ASTree *operator, ASTree * left, ASTree *right) {
  const TypeSpec *left_type = extract_type(left);
  const TypeSpec *right_type = extract_type(right);
  operator->type = & SPEC_INT;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    const TypeSpec *common_type;
    int status = determine_conversion(left_type, right_type, &common_type);
    if (status) return status;
    status = convert_type(left, common_type);
    if (status) return status;
    return convert_type(right, common_type);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type)) {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE || typespec_is_voidptr(left_type) ||
        typespec_is_voidptr(right_type)) {
      return 0;
    } else {
      fprintf(stderr,
              "ERROR: incompatible pointer types for operator \"%s\".\n",
              parser_get_tname(operator->symbol));
      return -1;
    }
  } else if (((typespec_is_pointer(left_type) && is_const_zero(right)) ||
              (is_const_zero(left) && typespec_is_pointer(right_type))) &&
             (operator->symbol == TOK_EQ || operator->symbol == TOK_NE)) {
    return 0;
  } else {
    fprintf(stderr, "ERROR: uncomparable types for operator \"%s\".\n",
            parser_get_tname(operator->symbol));
    return -1;
  }
}

int typecheck_mulop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_arithmetic(extract_type(left)) &&
      typespec_is_arithmetic(extract_type(right))) {
    int status = determine_conversion(extract_type(left), extract_type(right),
                                      &operator->type);
    if (status) return status;
    status = convert_type(left, operator->type);
    if (status) return status;
    return convert_type(right, operator->type);
  } else {
    fprintf(stderr,
            "ERROR: operator \"%s\" must have arguments of integral type.\n",
            parser_get_tname(operator->symbol));
    return -1;
  }
}

int typecheck_shfop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_integer(extract_type(left)) &&
      typespec_is_integer(extract_type(right))) {
    int status =
        determine_conversion(extract_type(left), &SPEC_INT, &operator->type);
    if (status) return status;
    status = convert_type(left, operator->type);
    /* promote right operand independently of left */
    const TypeSpec *dummy;
    status = determine_conversion(extract_type(right), &SPEC_INT, &dummy);
    if (status) return status;
    return convert_type(right, operator->type);
  } else {
    fprintf(stderr,
            "ERROR: operator \"%s\" must have arguments of integral type.\n",
            parser_get_tname(operator->symbol));
    return -1;
  }
}

int typecheck_bitop(ASTree *operator, ASTree * left, ASTree *right) {
  if (typespec_is_integer(extract_type(left)) &&
      typespec_is_integer(extract_type(right))) {
    int status = determine_conversion(extract_type(left), extract_type(right),
                                      &operator->type);
    if (status) return status;
    status = convert_type(left, operator->type);
    if (status) return status;
    return convert_type(right, operator->type);
  } else {
    fprintf(stderr,
            "ERROR: operator \"%s\" must have arguments of integral type.\n",
            parser_get_tname(operator->symbol));
    return -1;
  }
}

int validate_binop(ASTree *operator, SymbolTable * table) {
  DEBUGS('t', "Validating binary operator %c", operator->symbol);

  ASTree *left = astree_first(operator);
  int status = validate_expr(left, table);
  if (status) return status;
  status = perform_pointer_conv(left);
  if (status) return status;

  ASTree *right = astree_second(operator);
  status = validate_expr(right, table);
  if (status) return status;
  status = perform_pointer_conv(right);
  if (status) return status;

  switch (operator->symbol) {
    case TOK_SHL:
    case TOK_SHR:
      return typecheck_shfop(operator, left, right);
    case '%':
    case '&':
    case '|':
    case '^':
      return typecheck_bitop(operator, left, right);
    case '*':
    case '/':
      return typecheck_mulop(operator, left, right);
    case '+':
    case '-':
      return typecheck_addop(operator, left, right);
    case TOK_EQ:
    case TOK_NE:
    case TOK_GE:
    case TOK_LE:
    case '>':
    case '<':
      return typecheck_relop(operator, left, right);
    case TOK_AND:
    case TOK_OR:
      return typecheck_logop(operator, left, right);
    default:
      fprintf(stderr, "ERROR: unimplemented binary operator \"%s\"\n.",
              parser_get_tname(operator->symbol));
      return -1;
  }
}

int is_increment(const int symbol) {
  return symbol == TOK_INC || symbol == TOK_DEC || symbol == TOK_POST_INC ||
         symbol == TOK_POST_DEC;
}

int validate_unop(ASTree *operator, SymbolTable * table) {
  DEBUGS('t', "Validating unary operator %c", operator->symbol);
  ASTree *operand = astree_first(operator);
  int status = validate_expr(operand, table);
  if (status != 0) return status;
  const TypeSpec *operand_type = extract_type(operand);

  if (is_increment(operator->symbol) && !typespec_is_scalar(operand_type)) {
    fprintf(
        stderr,
        "ERROR: '%s' argument must be of scalar type.\n", operator->lexinfo);
    return -1;
  } else if ((operator->symbol == TOK_NEG || operator->symbol == TOK_POS) &&
             !typespec_is_arithmetic(operand_type)) {
    fprintf(stderr, "ERROR: '%s' argument must be of arithmetic type.\n",
                    operator->lexinfo);
    return -1;
  } else if (operator->symbol == '~' && !typespec_is_integer(operand_type)) {
    fprintf(
        stderr,
        "ERROR: '%s' argument must be of integral type.\n", operator->lexinfo);
    return -1;
  } else if (operator->symbol == '!') {
    if (typespec_is_scalar(operand_type)) {
      operator->type = & SPEC_INT;
      return 0;
    } else {
      fprintf(
          stderr,
          "ERROR: '%s' argument must be of scalar type.\n", operator->lexinfo);
      return -1;
    }
  } else {
    if (!is_increment(operator->symbol)) {
      int status = perform_pointer_conv(operand);
      if (status) return status;
    }
    status = determine_conversion(operand_type, &SPEC_INT, &operator->type);
    if (status) return status;
    return convert_type(operand, operator->type);
  }
}

int validate_indirection(ASTree *indirection, SymbolTable *table) {
  ASTree *subexpr = astree_first(indirection);
  int status = validate_expr(subexpr, table);
  if (status) return status;
  status = perform_pointer_conv(subexpr);
  if (status) return status;

  if (typespec_is_pointer(indirection->type)) {
    TypeSpec *indirection_spec = malloc(sizeof(*indirection_spec));
    int status = strip_aux_type(indirection_spec, extract_type(subexpr));
    if (status) return status;
    indirection->type = indirection_spec;
    return 0;
  } else {
    /* error; indirection can only be used on pointers and arrays */
    return -1;
  }
}

int validate_addrof(ASTree *addrof, SymbolTable *table) {
  ASTree *subexpr = astree_first(addrof);
  int status = validate_expr(subexpr, table);
  if (status) return status;
  /* TODO(Robert): check that operand is an lval */
  TypeSpec *addrof_spec = malloc(sizeof(*addrof_spec));
  status = typespec_copy(addrof_spec, extract_type(subexpr));
  if (status) return status;
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  llist_push_front(&addrof_spec->auxspecs, ptr_aux);
  addrof->type = addrof_spec;
  return 0;
}

int validate_subscript(ASTree *subscript, SymbolTable *table) {
  ASTree *composite_object = astree_first(subscript);
  int status = validate_expr(composite_object, table);
  if (status) return status;
  status = perform_pointer_conv(composite_object);
  if (status) return status;

  ASTree *index = astree_second(subscript);
  status = validate_expr(index, table);
  if (status) return status;
  status = perform_pointer_conv(composite_object);
  if (status) return status;

  if (typespec_is_pointer(extract_type(composite_object)) &&
      typespec_is_integer(extract_type(index))) {
    TypeSpec *subscript_spec = malloc(sizeof(*subscript_spec));
    int status = strip_aux_type(subscript_spec, extract_type(composite_object));
    if (status) return status;
    subscript->type = subscript_spec;
    return 0;
  } else {
    /* error; subscript can only be used on pointers and arrays with integer
     * indices
     */
    return -1;
  }
}

int validate_reference(ASTree *reference, SymbolTable *table) {
  ASTree *strunion = astree_first(reference);
  int status = validate_expr(strunion, table);
  if (status) return status;
  const TypeSpec *strunion_type = extract_type(strunion);
  if (!typespec_is_struct(strunion_type) && !typespec_is_union(strunion_type)) {
    /* error: cannot access member of non struct/union type */
    fprintf(stderr,
            "ERROR: cannot reference member of type that is not struct "
            "or union.\n");
    return -1;
  }

  ASTree *member = astree_second(reference);
  const char *member_name = member->lexinfo;
  const size_t member_name_len = strlen(member_name);
  AuxSpec *strunion_aux = llist_front(&strunion_type->auxspecs);
  const char *tag_name = strunion_aux->data.tag.name;
  TagValue *tagval = symbol_table_get_tag(table, tag_name, strlen(tag_name));
  if (tagval == NULL) {
    fprintf(stderr, "ERROR: unable to locate struct/union tag %s\n.", tag_name);
    return -1;
  }

  SymbolTable *member_table = tagval->data.members.by_name;
  SymbolValue *symval = NULL;
  int is_member = symbol_table_get(member_table, (char *)member_name,
                                   member_name_len, &symval);

  if (symval == NULL || !is_member) {
    fprintf(stderr, "ERROR: structure does not have member named %s.\n",
            member_name);
    return -1;
  } else {
    reference->type = &symval->type;
    return 0;
  }
}

int validate_arrow(ASTree *arrow, SymbolTable *table) {
  ASTree *strunion = astree_first(arrow);
  int status = validate_expr(strunion, table);
  if (status) return status;
  status = perform_pointer_conv(strunion);
  if (status) return status;

  const TypeSpec *strunion_type = extract_type(strunion);
  if (!typespec_is_structptr(strunion_type) &&
      !typespec_is_unionptr(strunion_type)) {
    /* error: cannot access member of non struct/union type */
    fprintf(stderr,
            "ERROR: cannot reference member of type that is not struct "
            "or union.\n");
    return -1;
  }
  ASTree *member = astree_second(arrow);
  const char *member_name = member->lexinfo;
  const size_t member_name_len = strlen(member_name);
  /* first auxtype is pointer; second is struct/union */
  AuxSpec *strunion_aux = llist_get(&strunion_type->auxspecs, 1);
  const char *tag_name = strunion_aux->data.tag.name;
  TagValue *tagval = symbol_table_get_tag(table, tag_name, strlen(tag_name));
  if (tagval == NULL) {
    fprintf(stderr, "ERROR: unable to locate struct/union tag %s\n.", tag_name);
    return -1;
  }

  SymbolTable *member_table = tagval->data.members.by_name;
  SymbolValue *symval = NULL;
  int is_member = symbol_table_get(member_table, (char *)member_name,
                                   member_name_len, &symval);

  if (symval == NULL || !is_member) {
    fprintf(stderr, "ERROR: structure does not have member named %s.\n",
            member_name);
    return -1;
  } else {
    arrow->type = &symval->type;
    return 0;
  }
}

int validate_expr(ASTree *expression, SymbolTable *table) {
  int status;
  const char *ident;
  ASTree *left;
  ASTree *right;

  DEBUGS('t', "Validating next expression");
  switch (expression->symbol) {
    case '=':
      status = validate_assignment(expression, table);
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
      status = validate_binop(expression, table);
      break;
    case '!':
    case TOK_POS: /* promotion operator */
    case TOK_NEG:
    case '~':
    case TOK_INC:
    case TOK_DEC:
    case TOK_POST_INC:
    case TOK_POST_DEC:
      status = validate_unop(expression, table);
      break;
    case TOK_INDIRECTION:
      status = validate_indirection(expression, table);
      break;
    case TOK_ADDROF:
      status = validate_addrof(expression, table);
      break;
    case TOK_CALL:
      /* expression->attributes |= ATTR_EXPR_VREG; */
      status = validate_call(expression, table);
      break;
    case TOK_SUBSCRIPT:
      status = validate_subscript(expression, table);
      break;
    case '.':
      status = validate_reference(expression, table);
      break;
    case TOK_ARROW:
      status = validate_arrow(expression, table);
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
      status = assign_type(expression, table);
      break;
    case TOK_CAST:
      status = validate_cast(expression, table);
      break;
    default:
      fprintf(stderr, "ERROR: UNEXPECTED TOKEN IN EXPRESSION: %s\n",
              expression->lexinfo);
      status = -1;
  }
  return status;
}

int declare_symbol(ASTree *declaration, ASTree *declarator,
                   SymbolTable *table) {
  ASTree *identifier = extract_ident(declarator);
  DEBUGS('t', "Making object entry for value %s", identifier->lexinfo);
  SymbolValue *symbol =
      symbol_value_init(&declarator->loc, map_size(&table->primary_namespace));

  size_t j;
  for (j = 0; j < astree_count(declarator); ++j) {
    int status = validate_dirdecl(astree_get(declarator, j), identifier, table,
                                  &symbol->type);
    if (status) return status;
  }

  int status =
      validate_typespec_list(astree_first(declaration), table, &symbol->type);
  if (status) return status;

  size_t identifier_len = strnlen(identifier->lexinfo, MAX_IDENT_LEN);
  SymbolValue *exists = NULL;
  int is_redefinition =
      symbol_table_get(table, identifier->lexinfo, identifier_len, &exists);
  if (exists && is_redefinition) {
    if (typespec_is_function(&exists->type) &&
        typespec_is_function(&symbol->type)) {
      int compatibility = types_compatible(&exists->type, &symbol->type);
      if (compatibility != TCHK_COMPATIBLE) {
        fprintf(stderr, "ERROR: redefinition of symbol %s\n",
                identifier->lexinfo);
        return -1;
      }
    } else {
      /* TODO(Robert): allow redefinition of extern symbols so long as types are
       * compatible */
      fprintf(stderr, "ERROR: redefinition of symbol %s\n",
              identifier->lexinfo);
      return -1;
    }
  } else {
    int status =
        symbol_table_insert(table, identifier->lexinfo, identifier_len, symbol);
    if (status) {
      fprintf(stderr, "ERROR: your data structure library sucks.\n");
      abort();
    }
  }

  return assign_type(identifier, table);
}

int typecheck_array_initializer(ASTree *declarator, ASTree *init_list,
                                SymbolTable *table) {
  const TypeSpec *array_type = extract_type(declarator);
  AuxSpec *array_aux = llist_front((LinkedList *)&array_type->auxspecs);
  if (array_aux->data.memory_loc.length > 0 &&
      array_aux->data.memory_loc.length < astree_count(init_list)) {
    fprintf(stderr, "ERROR: too many elements in array initializer.\n");
    return -1;
  } else {
    TypeSpec element_type = SPEC_EMPTY;
    int status = strip_aux_type(&element_type, array_type);
    if (status) return status;
    size_t i;
    for (i = 0; i < astree_count(init_list); ++i) {
      ASTree *initializer = astree_get(init_list, i);
      int status = validate_expr(initializer, table);
      if (status) return status;
      status = perform_pointer_conv(initializer);
      if (status) return status;
      status = convert_type(initializer, &element_type);
      if (status) return status;
    }
    return 0;
  }
}

int typecheck_union_initializer(ASTree *declarator, ASTree *init_list,
                                SymbolTable *table) {
  ASTree *identifier = extract_ident(declarator);
  AuxSpec *union_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = union_aux->data.tag.name;
  TagValue *tagval = symbol_table_get_tag(table, tag_name, strlen(tag_name));
  if (tagval == NULL) {
    fprintf(stderr, "ERROR: unable to locate struct/union tag %s\n.", tag_name);
    return -1;
  }

  const LinkedList *members = &tagval->data.members.in_order;
  if (astree_count(init_list) > 1) {
    fprintf(stderr, "ERROR: too many initializers provided for union type\n");
    return -1;
  } else {
    /* there should be one initializer of a type compatible with the type of the
     * first member of the union
     */
    ASTree *initializer = astree_first(init_list);
    int status = validate_expr(initializer, table);
    if (status) return status;
    status = perform_pointer_conv(initializer);
    if (status) return status;
    SymbolValue *member_symbol = llist_front((LinkedList *)members);
    return convert_type(initializer, &member_symbol->type);
  }
}

int typecheck_struct_initializer(ASTree *declarator, ASTree *init_list,
                                 SymbolTable *table) {
  ASTree *identifier = extract_ident(declarator);
  AuxSpec *struct_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = struct_aux->data.tag.name;
  TagValue *tagval = symbol_table_get_tag(table, tag_name, strlen(tag_name));
  if (tagval == NULL) {
    fprintf(stderr, "ERROR: unable to locate struct/union tag %s\n.", tag_name);
    return -1;
  }

  const LinkedList *members = &tagval->data.members.in_order;
  if (members->size < astree_count(init_list)) {
    fprintf(stderr,
            "ERROR: struct has %zu members, but %zu were provided, "
            "which is too many.\n",
            llist_size(members), astree_count(init_list));
    return -1;
  } else {
    size_t i;
    for (i = 0; i < astree_count(init_list); ++i) {
      ASTree *initializer = astree_get(init_list, i);
      int status = validate_expr(initializer, table);
      if (status) return status;
      status = perform_pointer_conv(initializer);
      if (status) return status;
      SymbolValue *member_symbol = llist_get((LinkedList *)members, i);
      status = convert_type(initializer, &member_symbol->type);
      if (status) return status;
    }
    return 0;
  }
}

int define_symbol(ASTree *declaration, ASTree *declarator, ASTree *initializer,
                  SymbolTable *table) {
  int status = declare_symbol(declaration, declarator, table);
  if (status) return status;
  if (initializer->symbol == TOK_INIT_LIST) {
    const TypeSpec *decl_type = extract_type(declarator);
    if (typespec_is_array(decl_type)) {
      return typecheck_array_initializer(declarator, initializer, table);
    } else if (decl_type->base == TYPE_UNION) {
      return typecheck_union_initializer(declarator, initializer, table);
    } else if (decl_type->base == TYPE_STRUCT) {
      return typecheck_struct_initializer(declarator, initializer, table);
    } else {
      fprintf(stderr, "ERROR: type cannot be initialized by list.\n");
      return -1;
    }
  } else {
    status = validate_expr(initializer, table);
    if (status) return status;
    status = perform_pointer_conv(initializer);
    if (status) return status;
    return convert_type(initializer, extract_type(declarator));
  }
}

int define_function(ASTree *function, SymbolTable *table) {
  ASTree *declaration = function;
  ASTree *declarator = astree_second(function);
  ASTree *identifier = extract_ident(declarator);

  SymbolValue *symbol = symbol_value_init(extract_loc(declarator),
                                          map_size(&table->primary_namespace));

  size_t i;
  for (i = 0; i < astree_count(declarator); ++i) {
    int status = validate_dirdecl(astree_get(declarator, i), identifier, table,
                                  &symbol->type);
    if (status) return status;
  }

  int status =
      validate_typespec_list(astree_first(function), table, &symbol->type);
  if (status) return status;

  const char *function_ident = extract_ident(declarator)->lexinfo;
  size_t function_ident_len = strnlen(function_ident, MAX_IDENT_LEN);
  SymbolValue *existing_entry = NULL;
  symbol_table_get(table, function_ident, function_ident_len, &existing_entry);
  if (existing_entry) {
    if (types_compatible(&existing_entry->type, &symbol->type) ==
        TCHK_INCOMPATIBLE) {
      fprintf(stderr, "ERROR: redeclaration of function: %s\n", function_ident);
      return -1;
    } else if (existing_entry->is_defined) {
      fprintf(stderr, "ERROR: redefinition of function: %s\n", function_ident);
      return -1;
    } else if (astree_count(function) == 3) {
      /* TODO(Robert): set function table's function type in define_params so
       * that all of the function table setup is done in a single location
       */
      identifier->symbol_table->function = existing_entry;
      ASTree *body = astree_third(function);
      body->symbol_table = identifier->symbol_table;
      int status = validate_stmt(body, table);
      if (status) return status;
    }
    /* TODO(Robert): keep declaration location of function prototype but use
     * the parameter names from the definition
     */
    symbol_value_destroy(symbol);
    return assign_type(identifier, table);
  } else {
    /* TODO(Robert): set function table's function type in define_params so that
     * all of the function table setup is done in a single location
     */
    identifier->symbol_table->function = symbol;

    int status =
        symbol_table_insert(table, function_ident, function_ident_len, symbol);
    if (status) return status;
    if (astree_count(function) == 3) {
      ASTree *body = astree_third(function);
      body->symbol_table = identifier->symbol_table;
      int status = validate_stmt(body, table);
      if (status) return status;
    }
    return assign_type(identifier, table);
  }
}

int define_members(ASTree *composite_type, TagValue *tagval,
                   SymbolTable *table) {
  /* TODO(Robert): in the interest of code reuse I used the existing functions
   * for entering and leaving scopes and making entries within a scope that
   * are normally used for objects and functions, even though struct members
   * don't work quite the same way. check to make sure everything is doing
   * okay later on
   */
  SymbolTable *member_table = tagval->data.members.by_name;
  LinkedList *member_list = &tagval->data.members.in_order;
  /* start from 2nd child; 1st was type name */
  size_t i;
  for (i = 1; i < astree_count(composite_type); ++i) {
    ASTree *member = astree_get(composite_type, i);
    ASTree *declarator = astree_second(member);
    const char *member_id_str = extract_ident(declarator)->lexinfo;
    size_t member_id_str_len = strnlen(member_id_str, MAX_IDENT_LEN);
    DEBUGS('t', "Found composite type member: %s", member_id_str);
    SymbolValue *member_entry = NULL;
    int member_exists = symbol_table_get(member_table, member_id_str,
                                         member_id_str_len, &member_entry);
    if (member_exists) {
      fprintf(stderr, "ERROR: Duplicate declaration of member: %s\n",
              member_id_str);
      return -1;
    } else {
      int status = validate_declaration(member, member_table);
      if (status != 0) return status;
      symbol_table_get(member_table, member_id_str, member_id_str_len,
                       &member_entry);
      if (tagval->alignment < member_entry->type.alignment) {
        tagval->alignment = member_entry->type.alignment;
      }
      if (tagval->tag == TAG_STRUCT) {
        sprintf(member_entry->obj_loc, "%%s+%zu", composite_type->type->width);
        size_t padding = member_entry->type.alignment -
                         (tagval->width % member_entry->type.alignment);
        tagval->width += padding + member_entry->type.width;
      } else if (tagval->width < member_entry->type.width) {
        tagval->width = member_entry->type.width;
      }
      llist_push_back(member_list, member_entry);
      DEBUGS('t', "Field inserted at %s", extract_ident(declarator)->lexinfo);
    }
  }
  tagval->is_defined = 1;
  return 0;
}

/* Enumerations are structured as follows: the enum keyword, with an optional
 * identifier (the tag) as its child. This is followed by a sequence of nodes,
 * which are either a TOK_IDENT or an '=', with two children, those being the
 * TOK_IDENT for the enumeration consntant and the value it represents.
 */
int define_enumerators(ASTree *enum_, TagValue *tagval, SymbolTable *table) {
  /* TODO(Robert): Enumerators are compile time constants. Figure them out
   * at compile time for real, like all other compile time consntants.
   */
  size_t i;
  for (i = 1; i < astree_count(enum_); ++i) {
    ASTree *enum_node = astree_get(enum_, i);
    ASTree *value_node = NULL;
    if (enum_node->symbol == '=') {
      value_node = astree_second(enum_node);
      enum_node = astree_first(enum_node);
    }

    const char *enum_ident = extract_ident(enum_node)->lexinfo;
    size_t enum_ident_len = strlen(enum_ident);
    SymbolValue *existing = NULL;
    int is_redefinition =
        symbol_table_get(table, enum_ident, enum_ident_len, &existing);
    if (is_redefinition) {
      fprintf(stderr, "ERROR: redefinition of enumeration constant %s\n",
              enum_ident);
      return -1;
    }

    SymbolValue *symval =
        symbol_value_init(&enum_->loc, map_size(&table->primary_namespace));
    int status = typespec_init(&symval->type);
    if (status) return status;
    symval->type.alignment = X64_ALIGNOF_INT;
    symval->type.width = X64_SIZEOF_INT;
    symval->type.base = TYPE_ENUM;

    AuxSpec *enum_aux = calloc(1, sizeof(*enum_aux));
    enum_aux->aux = AUX_ENUM;
    enum_aux->data.tag.name = extract_ident(enum_)->lexinfo;
    llist_push_back(&symval->type.auxspecs, enum_aux);

    status = symbol_table_insert(table, enum_ident, enum_ident_len, symval);
    if (status) return status;

    enum_node->type = &symval->type;

    if (value_node != NULL) {
      /* TODO(Robert): constexprs */
      int status = validate_expr(value_node, table);
      if (status) return status;
    }
  }

  tagval->is_defined = 1;
  return 0;
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

int define_tag(ASTree *tag, SymbolTable *table) {
  const char *tag_name = extract_ident(tag)->lexinfo;
  const size_t tag_name_len = strnlen(tag_name, MAX_IDENT_LEN);
  DEBUGS('t', "Defining tag: %s", tag_name);
  TagValue *exists = symbol_table_get_tag(table, tag_name, tag_name_len);
  TagValue *tagval = tag_value_init(tag_from_symbol(tag->symbol), table);
  if (tagval == NULL) return -1;

  if (astree_count(tag) < 2 && !exists) {
    return symbol_table_insert_tag(table, tag_name, tag_name_len, tagval);
  } else if (exists) {
    /* TODO(Robert): allow identical redefinitions */
    fprintf(stderr, "ERROR: redefinition of tag %s\n", tag_name);
    return -1;
  } else {
    int status = symbol_table_insert_tag(table, tag_name, tag_name_len, tagval);
    if (status) return status;
    if (tagval->tag == TAG_ENUM)
      return define_enumerators(tag, tagval, table);
    else
      return define_members(tag, tagval, table);
  }
}

int validate_declaration(ASTree *declaration, SymbolTable *table) {
  size_t i;
  for (i = 1; i < astree_count(declaration); ++i) {
    ASTree *current = astree_get(declaration, i);
    ASTree *next = astree_get(declaration, i + 1);
    if (next && next->symbol == TOK_BLOCK) {
      /* function definition */
      return define_function(declaration, table);
    } else if (next && next->symbol != TOK_DECLARATOR) {
      /* variable declaration and initialization */
      return define_symbol(declaration, current, next, table);
    } else {
      /* variable/function declaration */
      return declare_symbol(declaration, current, table);
    }
  }
  return 0;
}

int validate_return(ASTree *ret, SymbolTable *table) {
  TypeSpec ret_spec = SPEC_EMPTY;
  /* strip function type */
  SymbolValue *function_symval = symbol_table_get_function(table);
  int status = strip_aux_type(&ret_spec, &function_symval->type);
  if (status) return status;
  if (astree_count(ret) > 0) {
    ASTree *expr = astree_first(ret);
    int status = validate_expr(expr, table);
    if (status) return status;
    status = perform_pointer_conv(expr);
    if (status) return status;
    status = convert_type(expr, &ret_spec);
  } else {
    int compatibility = types_compatible(&ret_spec, &SPEC_VOID);
    if (compatibility != TCHK_COMPATIBLE) {
      fprintf(stderr, "ERROR: non-void function should return a value.\n");
      status = -1;
    }
  }

  /* free temporary spec */
  typespec_destroy(&ret_spec);
  return status;
}

int validate_ifelse(ASTree *ifelse, SymbolTable *table) {
  ASTree *expr = astree_first(ifelse);
  int status = validate_expr(expr, table);
  if (status) return status;
  status = perform_pointer_conv(expr);
  if (status) return status;

  if (!typespec_is_scalar(extract_type(expr))) {
    /* error: conditional expression must be of type int */
    status = -1;
    return status;
  }

  ASTree *if_body = astree_second(ifelse);
  status = validate_stmt(if_body, table);
  if (status) return status;

  if (astree_count(ifelse) == 3) {
    ASTree *else_body = astree_third(ifelse);
    status = validate_stmt(else_body, table);
    if (status) return status;
  }
  return 0;
}

int validate_while(ASTree *while_, SymbolTable *table) {
  ASTree *expr = while_->symbol == TOK_WHILE ? astree_first(while_)
                                             : astree_second(while_);
  int status = validate_expr(expr, table);
  if (status) return status;
  status = perform_pointer_conv(expr);
  if (status) return status;
  if (!typespec_is_scalar(extract_type(expr))) {
    /* error: conditional expression must be of type int */
    status = -1;
    return status;
  }
  ASTree *while_body = while_->symbol == TOK_WHILE ? astree_second(while_)
                                                   : astree_first(while_);
  return validate_stmt(while_body, table);
}

int validate_block(ASTree *block, SymbolTable *table) {
  size_t i;
  int status = 0;
  /* don't overwrite scope if this block belongs to a function */
  if (block->symbol_table == NULL)
    block->symbol_table = symbol_table_init(table);
  for (i = 0; i < astree_count(block); ++i) {
    ASTree *statement = astree_get(block, i);
    /* remember to use this block's table, and not its parent's */
    status = validate_stmt(statement, block->symbol_table);
    if (status) break;
  }
  return status;
}

int validate_stmt(ASTree *statement, SymbolTable *table) {
  int status;
  DEBUGS('t', "Validating next statement");
  switch (statement->symbol) {
    case TOK_RETURN:
      status = validate_return(statement, table);
      break;
    case TOK_IF:
      status = validate_ifelse(statement, table);
      break;
    case TOK_DO:
    case TOK_WHILE:
      status = validate_while(statement, table);
      break;
    case TOK_BLOCK:
      status = validate_block(statement, table);
      break;
    case TOK_DECLARATION:
      status = validate_declaration(statement, table);
      break;
    default:
      /* parser will catch anything that we don't want, so at this point the
       * only thing left that this could be is an expression-statement
       */
      status = validate_expr(statement, table);
      break;
  }
  return status;
}

/*
 * external functions
 */

int type_checker_make_table(ASTree *root) {
  DEBUGS('t', "Making symbol table");
  root->symbol_table = symbol_table_init(NULL);
  size_t i;
  for (i = 0; i < astree_count(root); ++i) {
    ASTree *child = astree_get(root, i);
    int status;
    switch (child->symbol) {
      /* only way to distinguish between a function definition and other
       * declarations is the symbol of the third child
       */
      case TOK_DECLARATION:
        status = validate_declaration(child, root->symbol_table);
        if (status) return status;
        break;
      case TOK_STRUCT:
      case TOK_UNION:
      case TOK_ENUM:
        status = define_tag(child, root->symbol_table);
        if (status) return status;
        break;
      default:
        fprintf(stderr, "ERROR: Unexpected symbol at top level: %s\n",
                parser_get_tname(child->symbol));
        return -1;
    }
  }
  return 0;
}
