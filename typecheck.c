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
int validate_expr(ASTree *statement);
int validate_stmt(ASTree *statement);
int validate_declaration(ASTree *statement); /* required to process params */
int types_compatible(
    const TypeSpec *type1,
    const TypeSpec *type2); /* required to check param and member types */

int assign_type(ASTree *tree) {
  DEBUGS('t', "Attempting to assign a type");
  ASTree *identifier = extract_ident(tree);
  if (identifier == NULL) return -1;
  const char *id_str = identifier->lexinfo;
  size_t id_str_len = strnlen(id_str, MAX_IDENT_LEN);
  SymbolValue *symval = NULL;
  int in_current_scope = locate_symbol(id_str, id_str_len, &symval);
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
      int ret = compare_params(&dest->data.params, &src->data.params);
      if (ret != TCHK_COMPATIBLE) return ret;
    } else if ((dest->aux == AUX_STRUCT && src->aux == AUX_STRUCT) ||
               (dest->aux == AUX_UNION && src->aux == AUX_UNION)) {
      int ret = compare_members(&dest->data.composite.members,
                                &src->data.composite.members);
      if (ret != TCHK_COMPATIBLE) return ret;
    } else {
      return TCHK_EXPLICIT_CAST;
    }
  }

  return TCHK_COMPATIBLE;
}

int compare_declspecs(const TypeSpec *dest, const TypeSpec *src) {
  /* TODO(Robert): check qualifiers */
  if (typespec_is_integer(dest) && typespec_is_integer(src)) {
    if (dest->base == src->base && dest->width == src->width) {
      return TCHK_COMPATIBLE;
    } else {
      return TCHK_IMPLICIT_CAST;
    }
  } else if ((dest->base == TYPE_STRUCT && src->base == TYPE_STRUCT) ||
             (src->base == TYPE_UNION && src->base == TYPE_UNION)) {
    return TCHK_COMPATIBLE;
  } else {
    return TCHK_INCOMPATIBLE;
  }
}

/* This function determines compatibility in situations where there is a
 * distinct 'destination' and 'source' type. It answers the question "does X
 * need to be converted to Y, and if so, does that need to be done explicity".
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
  } else if (typespec_is_pointer(type1) && typespec_is_integer(type2) &&
             llist_size(auxspecs2) == 0) {
    /* int to pointer */
    return TCHK_EXPLICIT_CAST;
  } else if (typespec_is_pointer(type2) && typespec_is_integer(type1) &&
             llist_size(auxspecs1) == 0) {
    /* pointer to int */
    return TCHK_EXPLICIT_CAST;
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
  array_aux->data.ptr_or_arr.length = strlen(stringcon->lexinfo) - 1;

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

int validate_composite_typespec(ASTree *type, TypeSpec *out) {
  ASTree *composite_type = astree_first(type);
  const char *composite_type_name = composite_type->lexinfo;
  size_t composite_type_name_len = strlen(composite_type_name);
  SymbolValue *exists = NULL;
  locate_symbol(composite_type_name, composite_type_name_len, &exists);
  if (!exists) {
    fprintf(stderr, "ERROR: structure type %s is not defined\n",
            composite_type_name);
    return -1;
  } else if (type->symbol == TOK_STRUCT && exists->type.base != TYPE_STRUCT) {
    fprintf(stderr, "ERROR: type %s is not a struct\n", composite_type_name);
    return -1;
  } else if (type->symbol == TOK_UNION && exists->type.base != TYPE_UNION) {
    fprintf(stderr, "ERROR: type %s is not a union\n", composite_type_name);
    return -1;
  } else {
    out->base = exists->type.base;
    out->width = exists->type.width;
    out->alignment = exists->type.alignment;

    if (out->auxspecs.anchor == NULL) {
      typespec_init(out);
    }

    size_t i;
    for (i = 0; i < llist_size(&exists->type.auxspecs); ++i) {
      AuxSpec *dest = calloc(1, sizeof(*dest));
      AuxSpec *src = llist_get(&exists->type.auxspecs, i);
      int status = auxspec_copy(dest, src);
      if (status) return status;
      status = llist_push_back(&out->auxspecs, dest);
      if (status) return status;
    }
    return 0;
  }
}

int validate_typespec_list(ASTree *spec_list, TypeSpec *out) {
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
        status = validate_composite_typespec(type, out);
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

int validate_ident(ASTree *ident) { return assign_type(ident); }

int validate_call(ASTree *call) {
  ASTree *function = astree_first(call);
  int status = validate_expr(function);
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
  LinkedList *param_list = &param_spec->data.params;
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
    int status = validate_expr(call_param);
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

  TypeSpec *return_spec = malloc(sizeof(*return_spec));
  /* strip pointer */
  status = strip_aux_type(return_spec, function_spec);
  if (status) return status;
  /* strip function */
  /* TODO(Robert): free temporary copies created by the first strip */
  status = strip_aux_type(return_spec, function_spec);
  if (status) return status;
  call->type = return_spec;
  return 0;
}

int validate_assignment(ASTree *assignment) {
  ASTree *dest = astree_first(assignment);
  int status = validate_expr(dest);
  if (status) return status;

  ASTree *src = astree_second(assignment);
  status = validate_expr(src);
  if (status) return status;
  status = perform_pointer_conv(src);
  if (status) return status;

  assignment->type = dest->type;
  return convert_type(src, dest->type);
}

int define_params(ASTree *params, ASTree *ident, TypeSpec *spec) {
  AuxSpec *aux_function = calloc(1, sizeof(*aux_function));
  aux_function->aux = AUX_FUNCTION;
  LinkedList *param_entries = &(aux_function->data.params);
  /* type is not resposible for freeing symbol information */
  llist_init(param_entries, NULL, NULL);
  create_scope(&(ident->symbol_table));
  size_t i;
  for (i = 0; i < astree_count(params); ++i) {
    ASTree *param = astree_get(params, i);
    ASTree *spec = astree_get(param, 0);
    ASTree *declarator = astree_get(param, 1);
    const char *param_id_str = extract_ident(declarator)->lexinfo;
    DEBUGS('t', "Defining function parameter %s", param_id_str);
    int status = validate_declaration(param);
    if (status) return status;
    size_t param_id_str_len = strnlen(param_id_str, MAX_IDENT_LEN);
    SymbolValue *param_entry = NULL;
    int in_current_scope =
        locate_symbol(param_id_str, param_id_str_len, &param_entry);
    if (!in_current_scope) {
      fprintf(stderr,
              "ERROR: parameter symbol was not inserted into function table\n");
      return -1;
    }
    status = llist_push_back(param_entries, param_entry);
    if (status) return status;
  }
  int status = llist_push_back(&spec->auxspecs, aux_function);
  if (status) return status;
  /* temporarily leave function prototype scope to work on global table */
  return leave_scope(&(ident->symbol_table));
}

int define_array(ASTree *array, TypeSpec *spec) {
  AuxSpec *aux_array = calloc(1, sizeof(*aux_array));
  aux_array->aux = AUX_ARRAY;
  if (astree_count(array) > 0) {
    aux_array->data.ptr_or_arr.length =
        strtoumax(astree_first(array)->lexinfo, NULL, 0);
    if (aux_array->data.ptr_or_arr.length == ULONG_MAX) {
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

int validate_dirdecl(ASTree *dirdecl, ASTree *ident, TypeSpec *spec) {
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
  if (spec->auxspecs.anchor == NULL) {
    typespec_init(spec);
  }

  switch (dirdecl->symbol) {
    case TOK_DECLARATOR: {
      size_t i;
      for (i = 0; i < astree_count(dirdecl); ++i) {
        int status = validate_dirdecl(astree_get(dirdecl, i), ident, spec);
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
      return define_array(dirdecl, spec);
      break;
    case TOK_POINTER:
      return define_pointer(dirdecl, spec);
      break;
    case TOK_FUNCTION:
      return define_params(dirdecl, ident, spec);
      break;
    default:
      fprintf(stderr, "ERROR: invalid direct declarator: %s\n",
              parser_get_tname(dirdecl->symbol));
      return -1;
      break;
  }
}

int validate_cast(ASTree *cast) {
  ASTree *spec_list = astree_first(cast);
  TypeSpec *spec = calloc(1, sizeof(*cast->type));
  cast->type = spec;
  ASTree *expr = NULL;

  if (astree_second(cast)->symbol == TOK_DECLARATOR) {
    ASTree *abstract_decl = astree_second(cast);
    expr = astree_third(cast);
    size_t i;
    for (i = 0; i < astree_count(abstract_decl); ++i) {
      int status = validate_dirdecl(astree_get(abstract_decl, i), cast, spec);
      if (status) return status;
    }
  } else {
    expr = astree_second(cast);
  }

  int status = validate_typespec_list(spec_list, spec);
  if (status) return status;
  status = validate_expr(expr);
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

int validate_binop(ASTree *operator) {
  DEBUGS('t', "Validating binary operator %c", operator->symbol);

  ASTree *left = astree_first(operator);
  int status = validate_expr(left);
  if (status) return status;
  status = perform_pointer_conv(left);
  if (status) return status;

  ASTree *right = astree_second(operator);
  status = validate_expr(right);
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

int validate_unop(ASTree *operator) {
  DEBUGS('t', "Validating unary operator %c", operator->symbol);
  ASTree *operand = astree_first(operator);
  int status = validate_expr(operand);
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

int validate_indirection(ASTree *indirection) {
  ASTree *subexpr = astree_first(indirection);
  int status = validate_expr(subexpr);
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

int validate_addrof(ASTree *addrof) {
  ASTree *subexpr = astree_first(addrof);
  int status = validate_expr(subexpr);
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

int validate_subscript(ASTree *subscript) {
  ASTree *composite_object = astree_first(subscript);
  int status = validate_expr(composite_object);
  if (status) return status;
  status = perform_pointer_conv(composite_object);
  if (status) return status;

  ASTree *index = astree_second(subscript);
  status = validate_expr(index);
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

int validate_reference(ASTree *reference) {
  ASTree *strunion = astree_first(reference);
  int status = validate_expr(strunion);
  if (status) return status;

  const TypeSpec *strunion_type = extract_type(strunion);
  if (!typespec_is_struct(strunion_type) && !typespec_is_union(strunion_type)) {
    /* error: cannot access member of non struct/union type */
    fprintf(stderr,
            "ERROR: cannot reference member of type that is not struct "
            "or union.\n");
    return -1;
  } else {
    ASTree *member = astree_second(reference);
    const char *member_name = member->lexinfo;
    const size_t member_name_len = strlen(member_name);
    AuxSpec *strunion_aux = llist_front(&strunion_type->auxspecs);
    Map *strunion_members = strunion_aux->data.composite.symbol_table;
    SymbolValue *member_symbol =
        map_get(strunion_members, (char *)member_name, member_name_len);
    if (member_symbol == NULL) {
      fprintf(stderr, "ERROR: structure does not have member named %s.\n",
              member_name);
      return -1;
    } else {
      reference->type = &member_symbol->type;
      return 0;
    }
  }
}

int validate_arrow(ASTree *arrow) {
  ASTree *strunion = astree_first(arrow);
  int status = validate_expr(strunion);
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
  } else {
    ASTree *member = astree_second(arrow);
    const char *member_name = member->lexinfo;
    const size_t member_name_len = strlen(member_name);
    /* first auxtype is pointer; second is struct/union */
    AuxSpec *strunion_aux = llist_get(&strunion_type->auxspecs, 1);
    Map *strunion_members = strunion_aux->data.composite.symbol_table;
    SymbolValue *member_symbol =
        map_get(strunion_members, (char *)member_name, member_name_len);
    if (member_symbol == NULL) {
      fprintf(stderr, "ERROR: structure does not have member named %s.\n",
              member_name);
      return -1;
    } else {
      arrow->type = &member_symbol->type;
      return 0;
    }
  }
}

int validate_expr(ASTree *expression) {
  int status;
  const char *ident;
  ASTree *left;
  ASTree *right;

  DEBUGS('t', "Validating next expression");
  switch (expression->symbol) {
    case '=':
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
      status = assign_type(expression);
      break;
    case TOK_CAST:
      status = validate_cast(expression);
      break;
    default:
      fprintf(stderr, "ERROR: UNEXPECTED TOKEN IN EXPRESSION: %s\n",
              expression->lexinfo);
      status = -1;
  }
  return status;
}

int declare_symbol(ASTree *declaration, size_t i) {
  ASTree *declarator = astree_get(declaration, i);
  ASTree *identifier = extract_ident(declarator);
  DEBUGS('t', "Making object entry for value %s", identifier->lexinfo);
  SymbolValue *symbol = symbol_value_init(&declarator->loc);

  size_t j;
  for (j = 0; j < astree_count(declarator); ++j) {
    int status =
        validate_dirdecl(astree_get(declarator, j), identifier, &symbol->type);
    if (status) return status;
  }

  int status = validate_typespec_list(astree_first(declaration), &symbol->type);
  if (status) return status;

  size_t identifier_len = strnlen(identifier->lexinfo, MAX_IDENT_LEN);
  SymbolValue *exists = NULL;
  int in_current_scope =
      locate_symbol(identifier->lexinfo, identifier_len, &exists);
  if (exists && in_current_scope) {
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
    int status = insert_symbol(identifier->lexinfo, identifier_len, symbol);
    if (status) {
      fprintf(stderr, "ERROR: your data structure library sucks.\n");
      abort();
    }
  }

  return assign_type(identifier);
}

int typecheck_array_initializer(ASTree *declarator, ASTree *init_list) {
  const TypeSpec *array_type = extract_type(declarator);
  AuxSpec *array_aux = llist_front((LinkedList *)&array_type->auxspecs);
  if (array_aux->data.ptr_or_arr.length > 0 &&
      array_aux->data.ptr_or_arr.length < astree_count(init_list)) {
    fprintf(stderr, "ERROR: too many elements in array initializer.\n");
    return -1;
  } else {
    TypeSpec element_type = SPEC_EMPTY;
    int status = strip_aux_type(&element_type, array_type);
    if (status) return status;
    size_t i;
    for (i = 0; i < astree_count(init_list); ++i) {
      ASTree *initializer = astree_get(init_list, i);
      int status = validate_expr(initializer);
      if (status) return status;
      status = perform_pointer_conv(initializer);
      if (status) return status;
      status = convert_type(initializer, &element_type);
      if (status) return status;
    }
    return 0;
  }
}

int typecheck_union_initializer(ASTree *declarator, ASTree *init_list) {
  ASTree *identifier = extract_ident(declarator);
  AuxSpec *union_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const LinkedList *members = &union_aux->data.composite.members;
  if (astree_count(init_list) > 1) {
    fprintf(stderr, "ERROR: too many initializers provided for union type\n");
    return -1;
  } else {
    /* there should be one initializer of a type compatible with the type of the
     * first member of the union
     */
    ASTree *initializer = astree_first(init_list);
    int status = validate_expr(initializer);
    if (status) return status;
    status = perform_pointer_conv(initializer);
    if (status) return status;
    SymbolValue *member_symbol = llist_front((LinkedList *)members);
    return convert_type(initializer, &member_symbol->type);
  }
}

int typecheck_struct_initializer(ASTree *declarator, ASTree *init_list) {
  ASTree *identifier = extract_ident(declarator);
  AuxSpec *struct_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const LinkedList *members = &struct_aux->data.composite.members;
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
      int status = validate_expr(initializer);
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

int define_symbol(ASTree *declaration, size_t i) {
  ASTree *declarator = astree_get(declaration, i);
  ASTree *initializer = astree_get(declaration, i + 1);
  int status = declare_symbol(declaration, i);
  if (status) return status;
  if (initializer->symbol == TOK_INIT_LIST) {
    const TypeSpec *decl_type = extract_type(declarator);
    if (typespec_is_array(decl_type)) {
      return typecheck_array_initializer(declarator, initializer);
    } else if (decl_type->base == TYPE_UNION) {
      return typecheck_union_initializer(declarator, initializer);
    } else if (decl_type->base == TYPE_STRUCT) {
      return typecheck_struct_initializer(declarator, initializer);
    } else {
      fprintf(stderr, "ERROR: type cannot be initialized by list.\n");
      return -1;
    }
  } else {
    status = validate_expr(initializer);
    if (status) return status;
    status = perform_pointer_conv(initializer);
    if (status) return status;
    return convert_type(initializer, extract_type(declarator));
  }
}

int define_body(ASTree *function, SymbolValue *entry) {
  int status = 0;
  ASTree *declarator = astree_second(function);
  ASTree *identifier = extract_ident(declarator);
  enter_body(&identifier->symbol_table, entry);

  if (astree_count(function) == 3) {
    DEBUGS('t', "Defining function: %s", identifier->lexinfo);
    ASTree *body = astree_third(function);
    /* set return status */
    status = validate_stmt(body);
  }

  /* finalize scope */
  leave_body(&identifier->symbol_table, entry);
  return status;
}

int define_function(ASTree *function) {
  ASTree *declaration = function;
  ASTree *declarator = astree_second(function);
  ASTree *identifier = extract_ident(declarator);

  SymbolValue *symbol = symbol_value_init(extract_loc(declarator));

  size_t i;
  for (i = 0; i < astree_count(declarator); ++i) {
    int status =
        validate_dirdecl(astree_get(declarator, i), identifier, &symbol->type);
    if (status) return status;
  }

  int status = validate_typespec_list(astree_first(function), &symbol->type);
  if (status) return status;

  const char *function_ident = extract_ident(declarator)->lexinfo;
  size_t function_ident_len = strnlen(function_ident, MAX_IDENT_LEN);
  SymbolValue *existing_entry = NULL;
  locate_symbol(function_ident, function_ident_len, &existing_entry);
  if (existing_entry) {
    if (types_compatible(&existing_entry->type, &symbol->type) ==
        TCHK_INCOMPATIBLE) {
      fprintf(stderr, "ERROR: redeclaration of function: %s\n", function_ident);
      return -1;
    } else if (existing_entry->is_defined) {
      fprintf(stderr, "ERROR: redefinition of function: %s\n", function_ident);
      return -1;
    } else if (astree_count(function) == 3) {
      int status = define_body(function, existing_entry);
      if (status) return status;
    }
    /* use existing type info */
    symbol_value_destroy(symbol);
    return assign_type(identifier);
  } else {
    int status = insert_symbol(function_ident, function_ident_len, symbol);
    if (status) return status;
    status = define_body(function, symbol);
    if (status) return status;
    return assign_type(identifier);
  }
}

int define_members(ASTree *composite_type, SymbolValue *composite_type_entry) {
  AuxSpec *composite_aux = calloc(1, sizeof(*composite_aux));
  if (composite_type->symbol == TOK_STRUCT) {
    composite_aux->aux = AUX_STRUCT;
  } else if (composite_type->symbol == TOK_UNION) {
    composite_aux->aux = AUX_UNION;
  } else {
    /* should not happen, but I have been wrong before */
    fprintf(stderr, "ERROR: invalid token for composite type: %s\n",
            parser_get_tname(composite_type->symbol));
    return -1;
  }
  LinkedList *members = &composite_aux->data.composite.members;
  llist_init(members, NULL, NULL);
  /* TODO(Robert): in the interest of code reuse I used the existing functions
   * for entering and leaving scopes and making entries within a scope that
   * are normally used for objects and functions, even though struct members
   * don't work quite the same way. check to make sure everything is doing
   * okay later on
   */
  create_scope(&composite_type->symbol_table);
  composite_aux->data.composite.symbol_table = &composite_type->symbol_table;
  /* start from 2nd child; 1st was type name */
  size_t i;
  for (i = 1; i < astree_count(composite_type); ++i) {
    ASTree *member = astree_get(composite_type, i);
    ASTree *declarator = astree_second(member);
    const char *member_id_str = extract_ident(declarator)->lexinfo;
    size_t member_id_str_len = strnlen(member_id_str, MAX_IDENT_LEN);
    DEBUGS('t', "Found composite type member: %s", member_id_str);
    SymbolValue *member_entry = NULL;
    int member_exists =
        locate_symbol(member_id_str, member_id_str_len, &member_entry);
    if (member_exists) {
      fprintf(stderr, "ERROR: Duplicate declaration of member: %s\n",
              member_id_str);
      return -1;
    } else {
      int status = validate_declaration(member);
      if (status != 0) return status;
      locate_symbol(member_id_str, member_id_str_len, &member_entry);
      if (composite_type_entry->type.alignment < member_entry->type.alignment) {
        composite_type_entry->type.alignment = member_entry->type.alignment;
      }
      if (composite_type_entry->type.base == TYPE_STRUCT) {
        size_t padding =
            member_entry->type.alignment -
            (composite_type_entry->type.width % member_entry->type.alignment);
        composite_type_entry->type.width += padding + member_entry->type.width;
      } else if (composite_type_entry->type.width < member_entry->type.width) {
        composite_type_entry->type.width = member_entry->type.width;
      }
      llist_push_back(members, member_entry);
      DEBUGS('t', "Field inserted at %s", astree_second(member)->lexinfo);
    }
  }
  finalize_scope(&composite_type->symbol_table);
  llist_push_back(&composite_type_entry->type.auxspecs, composite_aux);
  return 0;
}

int define_composite_type(ASTree *composite_type) {
  const char *composite_type_name = extract_ident(composite_type)->lexinfo;
  const size_t composite_type_name_len =
      strnlen(composite_type_name, MAX_IDENT_LEN);
  DEBUGS('t', "Defining composite type: %s", composite_type_name);
  SymbolValue *exists = NULL;
  /* TODO(Robert): do not cast away const */
  locate_symbol((char *)composite_type_name, composite_type_name_len, &exists);
  SymbolValue *composite_type_symbol =
      symbol_value_init(extract_loc(composite_type));
  if (composite_type->symbol == TOK_STRUCT) {
    composite_type_symbol->type.base = TYPE_STRUCT;
  } else if (composite_type->symbol == TOK_UNION) {
    composite_type_symbol->type.base = TYPE_UNION;
  } else {
    /* should not happen, but I have been wrong before */
    fprintf(stderr, "ERROR: invalid token for composite type: %s\n",
            parser_get_tname(composite_type->symbol));
    return -1;
  }

  typespec_init(&composite_type_symbol->type);

  if (astree_count(composite_type) < 2 && !exists) {
    composite_type_symbol->is_defined = 0;
    return insert_symbol(composite_type_name, composite_type_name_len,
                         composite_type_symbol);
  } else {
    if (exists) {
      int status = define_members(composite_type, composite_type_symbol);
      if (status) return status;
      if (types_compatible(&exists->type, &composite_type_symbol->type) !=
          TCHK_COMPATIBLE) {
        fprintf(stderr, "ERROR: redefinition of composite_type %s\n",
                composite_type_name);
        return -1;
      }
      /* discard duplicate */
      return symbol_value_destroy(composite_type_symbol);
    } else {
      int status = insert_symbol(composite_type_name, composite_type_name_len,
                                 composite_type_symbol);
      if (status) return status;
      composite_type_symbol->is_defined = 1;
      return define_members(composite_type, composite_type_symbol);
    }
  }
}

int validate_declaration(ASTree *declaration) {
  size_t i;
  for (i = 1; i < astree_count(declaration); ++i) {
    /* call a function that constructs the rest of the type */
    ASTree *next = astree_get(declaration, i + 1);
    if (next && next->symbol == TOK_BLOCK) {
      /* function definition */
      return define_function(declaration);
    } else if (next && next->symbol != TOK_DECLARATOR) {
      /* variable declaration and initialization */
      return define_symbol(declaration, i);
    } else {
      /* variable/function declaration */
      return declare_symbol(declaration, i);
    }
  }
  return 0;
}

int validate_return(ASTree *ret) {
  TypeSpec ret_spec = SPEC_EMPTY;
  int status = get_ret_type(&ret_spec);
  if (status) return status;
  if (astree_count(ret) > 0) {
    ASTree *expr = astree_first(ret);
    status = validate_expr(expr);
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

int validate_ifelse(ASTree *ifelse) {
  ASTree *expr = astree_first(ifelse);
  int status = validate_expr(expr);
  if (status) return status;
  status = perform_pointer_conv(expr);
  if (status) return status;

  if (!typespec_is_scalar(extract_type(expr))) {
    /* error: conditional expression must be of type int */
    status = -1;
    return status;
  }
  /* Note: no matter whether or not there is an actual block here or just a
   * single statement, a new scope still needs to be created for it. For
   * example, if the single statement was a declaration, that object would not
   * be visible anywhere
   */
  ASTree *if_body = astree_second(ifelse);
  create_scope(&if_body->symbol_table);
  status = validate_stmt(if_body);
  finalize_scope(&if_body->symbol_table);
  if (status) return status;

  if (astree_count(ifelse) == 3) {
    ASTree *else_body = astree_third(ifelse);
    create_scope(&else_body->symbol_table);
    status = validate_stmt(else_body);
    finalize_scope(&else_body->symbol_table);
    if (status) return status;
  }
  return 0;
}

int validate_while(ASTree *while_) {
  ASTree *expr = while_->symbol == TOK_WHILE ? astree_first(while_)
                                             : astree_second(while_);
  int status = validate_expr(expr);
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
  create_scope(&while_body->symbol_table);
  status = validate_stmt(while_body);
  finalize_scope(&while_body->symbol_table);
  return status;
}

int validate_block(ASTree *block) {
  size_t i;
  int status = 0;
  /* always safe; a scope should never be created twice in the same place */
  create_scope(&(block->symbol_table));
  for (i = 0; i < astree_count(block); ++i) {
    ASTree *statement = astree_get(block, i);
    status = validate_stmt(statement);
    if (status) break;
  }
  /* always safe; a scope should never be finalized unless it is at the top
   * of the stack
   */
  finalize_scope(&block->symbol_table);
  return status;
}

int validate_stmt(ASTree *statement) {
  int status;
  DEBUGS('t', "Validating next statement");
  switch (statement->symbol) {
    case TOK_RETURN:
      status = validate_return(statement);
      break;
    case TOK_IF:
      status = validate_ifelse(statement);
      break;
    case TOK_DO:
    case TOK_WHILE:
      status = validate_while(statement);
      break;
    case TOK_BLOCK:
      status = validate_block(statement);
      break;
    case TOK_DECLARATION:
      status = validate_declaration(statement);
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
  create_scope(&(root->symbol_table));
  size_t i;
  for (i = 0; i < astree_count(root); ++i) {
    ASTree *child = astree_get(root, i);
    int status;
    switch (child->symbol) {
      /* only way to distinguish between a function definition and other
       * declarations is the symbol of the third child
       */
      case TOK_DECLARATION:
        status = validate_declaration(child);
        if (status) return status;
        break;
      case TOK_STRUCT:
      case TOK_UNION:
        status = define_composite_type(child);
        if (status) return status;
        break;
      default:
        fprintf(stderr, "ERROR: Unexpected symbol at top level: %s\n",
                parser_get_tname(child->symbol));
        return -1;
    }
  }
  finalize_scope(&root->symbol_table);
  return 0;
}
