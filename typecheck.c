#include "typecheck.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
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

#define ARG1_AST (1 << 0)
#define ARG1_SMV (1 << 1)
#define ARG1_TYPE (1 << 2)
#define ARG2_AST (1 << 3)
#define ARG2_SMV (1 << 4)
#define ARG2_TYPE (1 << 5)

#define IFLAG_INT (1 << 0)
#define IFLAG_CHAR (1 << 1)
#define IFLAG_SHRT (1 << 2)
#define IFLAG_LONG (1 << 3)
#define IFLAG_SIGNED (1 << 4)
#define IFLAG_UNSIGNED (1 << 5)
#define IFLAG_VOID (1 << 6)

enum type_checker_action {
  TCHK_COMPATIBLE,
  TCHK_IMPLICIT_CAST,
  TCHK_EXPLICIT_CAST,
  TCHK_INCOMPATIBLE,
  TCHK_E_NO_FLAGS
};

static SymbolValue *current_function = NULL;

/* forward declarations for mutual recursion */
int validate_expr(ASTree *statement);
int validate_stmt(ASTree *statement);
int validate_declaration(ASTree *statement); /* required to process params */
int types_compatible(
    const void *arg1, const void *arg2,
    unsigned int flags); /* required to check param and member types */

ASTree *extract_ident(ASTree *tree) {
  switch (tree->symbol) {
    /*
    case TOK_TYPE_ID:
      break;
    */
    case TOK_STRUCT:
    case TOK_UNION:
    case TOK_CALL:
      return astree_first(tree);
    case TOK_IDENT:
      return tree;
    case TOK_DECLARATOR:
      if (astree_first(tree)->symbol == TOK_DECLARATOR) {
        return extract_ident(astree_first(tree));
      } else {
        size_t i;
        for (i = 0; i < astree_count(tree); ++i) {
          ASTree *direct_decl = astree_get(tree, i);
          if (direct_decl->symbol == TOK_IDENT) {
            return direct_decl;
          }
        }
      }
      /* do not break; fall through and return error when no ident */
    default:
      fprintf(stderr, "ERROR: unable to get identifier for tree node %s\n",
              parser_get_tname(tree->symbol));
      return NULL;
  }
}

const TypeSpec *extract_type(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_STRUCT:
    case TOK_UNION:
    case TOK_FUNCTION:
    case TOK_DECLARATOR:
    case TOK_CALL:
      return extract_ident(tree)->type;
    case TOK_IDENT:
    default:
      return tree->type;
  }
}

const Location *extract_loc(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_STRUCT:
    case TOK_UNION:
    case TOK_FUNCTION:
    case TOK_DECLARATOR:
    case TOK_CALL:
      return &extract_ident(tree)->loc;
    case TOK_IDENT:
    default:
      return &tree->loc;
  }
}

int assign_type(ASTree *tree) {
  DEBUGS('t', "Attempting to assign a type");
  ASTree *identifier = extract_ident(tree);
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

int type_is_arithmetic(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED);
}

int type_is_integer(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED);
}

int type_is_aux(const TypeSpec *type, const AuxType aux) {
  AuxSpec *auxspec = llist_front((LinkedList *)&type->auxspecs);
  return auxspec != NULL && auxspec->aux == aux;
}

int type_is_pointer(const TypeSpec *type) {
  return type_is_aux(type, AUX_POINTER);
}

int type_is_array(const TypeSpec *type) { return type_is_aux(type, AUX_ARRAY); }

int type_is_function(const TypeSpec *type) {
  return type_is_aux(type, AUX_FUNCTION);
}

int type_is_int_or_ptr(const TypeSpec *type) {
  return (type_is_integer(type) || type_is_pointer(type));
}

int type_is_scalar(const TypeSpec *type) {
  return type_is_pointer(type) || type_is_arithmetic(type);
}

int type_is_comparable(const TypeSpec *type) {
  if (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED ||
      type_is_pointer(type)) {
    return 1;
  } else {
    return 0;
  }
}

int strip_aux_type(TypeSpec *dest, const TypeSpec *src) {
  int status = typespec_copy(dest, src);
  if (status) return status;
  void *stripped = llist_pop_front(&dest->auxspecs);
  if (stripped == NULL) {
    fprintf(stderr, "ERROR: unable to strip auxiliary type information\n");
    return -1;
  }
  return 0;
}

int insert_cast(ASTree *tree, const TypeSpec *type) {
  /* since the linked list has a destructor defined for elements, we can't just
   * use llist_get and llist_delete here, since that will call astree_destroy
   * on the node. Instead we call extract, which removes the element from the
   * list and returns the value
   */
  ASTree *cast = astree_init(TOK_CAST, tree->loc, "_cast");
  TypeSpec *cast_spec = malloc(sizeof(*cast_spec));
  int status = typespec_copy(cast_spec, type);
  if (status) return status;
  cast->type = cast_spec;
  astree_inject(tree, cast);
  return 0;
}

int insert_indirection(ASTree *tree) {
  ASTree *indirection = astree_init(TOK_INDIRECTION, tree->loc, "*");
  TypeSpec *indirection_spec = malloc(sizeof(*indirection_spec));
  int status = strip_aux_type(indirection_spec, tree->type);
  if (status) return status;
  indirection->type = indirection_spec;
  astree_inject(tree, indirection);
  return 0;
}

int insert_addrof(ASTree *tree) {
  ASTree *addrof = astree_init(TOK_ADDROF, tree->loc, "&");
  TypeSpec *addrof_spec = malloc(sizeof(*addrof_spec));
  int status = typespec_copy(addrof_spec, tree->type);
  if (status) return status;
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  llist_push_front(&addrof_spec->auxspecs, ptr_aux);
  addrof->type = addrof_spec;
  astree_inject(tree, addrof);
  return 0;
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
      int symbol_compat = types_compatible(dest, src, ARG1_SMV | ARG2_SMV);
      if (symbol_compat == TCHK_COMPATIBLE) continue;
      return TCHK_EXPLICIT_CAST;
    }
  }
  return TCHK_COMPATIBLE;
}

int compare_members(Map *dests, Map *srcs) {
  LinkedList keys = BLIB_LLIST_EMPTY;
  llist_init(&keys, NULL, NULL);
  /* get all keys; there will be duplicates but that is fine */
  map_keys(dests, &keys);
  map_keys(srcs, &keys);

  size_t i;
  for (i = 0; i < llist_size(&keys); ++i) {
    char *key = llist_get(&keys, i);
    SymbolValue *dest = map_get(dests, key, strlen(key));
    SymbolValue *src = map_get(srcs, key, strlen(key));

    if (dest == NULL || src == NULL) {
      return TCHK_EXPLICIT_CAST;
    } else {
      int ret = types_compatible(dest, src, ARG1_SMV | ARG2_SMV);
      if (ret != TCHK_COMPATIBLE) return ret;
    }
  }
  return TCHK_COMPATIBLE;
}

int compare_auxspecs(LinkedList *dests, LinkedList *srcs) {
  size_t aux_count = llist_size(dests) > llist_size(srcs) ? llist_size(dests)
                                                          : llist_size(srcs);
  size_t i;
  for (i = 0; i < aux_count; ++i) {
    AuxSpec *dest = llist_get(srcs, i);
    AuxSpec *src = llist_get(srcs, i);

    if (dest == NULL || src == NULL) {
      return TCHK_EXPLICIT_CAST;
    } else if ((dest->aux == AUX_POINTER || dest->aux == AUX_ARRAY) &&
               (src->aux == AUX_POINTER || src->aux == AUX_ARRAY) &&
               !((dest->data.ptr_or_arr.qualifiers ^
                  src->data.ptr_or_arr.qualifiers) &
                 (TYPE_FLAG_CONST | TYPE_FLAG_VOLATILE))) {
      continue;
    } else if ((dest->aux == AUX_FUNCTION && src->aux == AUX_FUNCTION)) {
      int ret = compare_params(&dest->data.params, &src->data.params);
      if (ret != TCHK_COMPATIBLE) return ret;
    } else if ((dest->aux == AUX_STRUCT && src->aux == AUX_STRUCT) ||
               (dest->aux == AUX_UNION && src->aux == AUX_UNION)) {
      int ret = compare_members(&dest->data.members, &src->data.members);
      if (ret != TCHK_COMPATIBLE) return ret;
    } else {
      return TCHK_EXPLICIT_CAST;
    }
  }

  return TCHK_COMPATIBLE;
}

int compare_declspecs(const TypeSpec *dest, const TypeSpec *src) {
  /* TODO(Robert): check qualifiers */
  int ret = TCHK_COMPATIBLE;
  if (type_is_integer(dest) && type_is_integer(src)) {
    if (dest->width > src->width) {
      ret = TCHK_IMPLICIT_CAST;
    } else if (dest->width == src->width) {
      if (dest->base == TYPE_SIGNED && src->base == TYPE_UNSIGNED) {
        ret = TCHK_EXPLICIT_CAST;
      } else if (dest->base == TYPE_UNSIGNED && src->base == TYPE_SIGNED) {
        ret = TCHK_IMPLICIT_CAST;
      } else {
        ret = TCHK_COMPATIBLE;
      }
    } else {
      return TCHK_EXPLICIT_CAST;
    }
  } else if ((dest->base == TYPE_STRUCT && src->base == TYPE_STRUCT) ||
             (src->base == TYPE_UNION && src->base == TYPE_UNION)) {
    ret = TCHK_COMPATIBLE;
  } else {
    ret = TCHK_EXPLICIT_CAST;
  }

  return ret;
}

/* This function determines compatibility in situations where there is a
 * distinct 'destination' and 'source' type. It answers the question "does X
 * need to be converted to Y, and if so, does that need to be done explicity".
 *
 * It is used to determine conversions after promotions, casts, assignments, and
 * function calls/definitions/declarations.
 */
int types_compatible(const void *arg1, const void *arg2, unsigned int flags) {
  const TypeSpec *type1 = NULL;
  const TypeSpec *type2 = NULL;
  int action = CONV_COMPATIBLE;

  /* extract type from first argument */
  if (flags & ARG1_AST) {
    type1 = extract_type((ASTree *)arg1);
  } else if (flags & ARG1_SMV) {
    type1 = &(((SymbolValue *)arg1)->type);
  } else if (flags & ARG1_TYPE) {
    type1 = arg1;
  } else {
    fprintf(stderr, "ERROR: no flags provided for argument 1\n");
    return TCHK_E_NO_FLAGS;
  }

  /* extract type from second argument */
  if (flags & ARG2_AST) {
    type2 = extract_type((ASTree *)arg2);
  } else if (flags & ARG2_SMV) {
    type2 = &(((SymbolValue *)arg2)->type);
  } else if (flags & ARG2_TYPE) {
    type2 = arg2;
  } else {
    fprintf(stderr, "ERROR: no flags provided for argument 2\n");
    return TCHK_E_NO_FLAGS;
  }

  LinkedList *auxspecs1 = (LinkedList *)&type1->auxspecs;
  LinkedList *auxspecs2 = (LinkedList *)&type2->auxspecs;
  /* special cases */
  if (type_is_pointer(type1) && type_is_pointer(type2) &&
      (type1->base == TYPE_VOID || type2->base == TYPE_VOID)) {
    /* pointer to/from void pointer */
    return TCHK_COMPATIBLE;
  } else if (type_is_pointer(type1) && type_is_integer(type2) &&
             llist_size(auxspecs2) == 0) {
    /* int to pointer */
    return TCHK_IMPLICIT_CAST;
  } else if (type_is_pointer(type2) && type_is_integer(type1) &&
             llist_size(auxspecs1) == 0) {
    /* pointer to int */
    return TCHK_IMPLICIT_CAST;
  }

  int ret = compare_auxspecs(auxspecs1, auxspecs2);
  if (ret != TCHK_COMPATIBLE) return ret;

  return compare_declspecs(type1, type2);
}

/* this function only determines the promoted type of its arguments, and even
 * then only if the arguments are valid
 *
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
int determine_promotion(ASTree *arg1, ASTree *arg2, const TypeSpec **out) {
  const struct typespec *type1 = arg1 ? extract_type(arg1) : NULL;
  const struct typespec *type2 = arg2 ? extract_type(arg2) : &SPEC_INT;

  if (type1 == NULL) {
    fprintf(stderr, "First argument not provided to promotion routine\n");
    return -1;
  } else if (type_is_pointer(type1)) {
    *out = type1;
  } else if (type_is_pointer(type2)) {
    *out = type2;
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
    intcon->type = &SPEC_CHAR;
  } else if (signed_value > INT16_MIN && signed_value < INT16_MAX) {
    intcon->type = &SPEC_SHRT;
  } else if (signed_value > INT32_MIN && signed_value < INT32_MAX) {
    intcon->type = &SPEC_INT;
  } else {
    intcon->type = &SPEC_LONG;
  }

  return status;
}

int validate_spec(ASTree *spec_list, TypeSpec *out) {
  int status = 0;
  unsigned int flags = 0;

  /* TODO(Robert): this is hideous and a roundabout way of figuring out the
   * the integer type; this should probably be handled by the parser in some
   * way
   */
  size_t i;
  for (i = 0; i < astree_count(spec_list); ++i) {
    ASTree *type = astree_get(spec_list, i);
    switch (type->symbol) {
      case TOK_VOID:
        if (flags) {
          fprintf(stderr, "ERROR: bad occurrence of void type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else {
          *out = SPEC_VOID;
        }
        flags |= IFLAG_VOID;
        break;
      case TOK_INT:
        if (flags & (IFLAG_INT | IFLAG_CHAR | IFLAG_VOID)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (!(flags & (IFLAG_LONG | IFLAG_SHRT | IFLAG_UNSIGNED))) {
          *out = SPEC_INT;
        }
        flags |= IFLAG_INT;
        break;
      case TOK_CHAR:
        if (flags &
            (IFLAG_CHAR | IFLAG_INT | IFLAG_SHRT | IFLAG_LONG | IFLAG_VOID)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (flags & IFLAG_UNSIGNED) {
          *out = SPEC_UCHAR;
        } else {
          *out = SPEC_CHAR;
        }
        flags |= IFLAG_CHAR;
        break;
      case TOK_LONG:
        if (flags & (IFLAG_LONG | IFLAG_SHRT | IFLAG_CHAR | IFLAG_VOID)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (flags & IFLAG_UNSIGNED) {
          *out = SPEC_ULONG;
        } else {
          *out = SPEC_LONG;
        }
        flags |= IFLAG_LONG;
        break;
      case TOK_SHORT:
        if (flags & (IFLAG_SHRT | IFLAG_LONG | IFLAG_CHAR | IFLAG_VOID)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (flags & IFLAG_UNSIGNED) {
          *out = SPEC_USHRT;
        } else {
          *out = SPEC_SHRT;
        }
        flags |= IFLAG_SHRT;
        break;
      case TOK_SIGNED:
        if (flags & (IFLAG_SIGNED | IFLAG_UNSIGNED | IFLAG_VOID)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (!(flags & (IFLAG_LONG | IFLAG_SHRT | IFLAG_CHAR))) {
          *out = SPEC_INT;
        }
        flags |= IFLAG_SIGNED;
        break;
      case TOK_UNSIGNED:
        if (flags & (IFLAG_UNSIGNED | IFLAG_SIGNED | IFLAG_VOID)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (flags & IFLAG_LONG) {
          *out = SPEC_ULONG;
        } else if (flags & IFLAG_SHRT) {
          *out = SPEC_USHRT;
        } else if (flags & IFLAG_CHAR) {
          *out = SPEC_UCHAR;
        } else {
          *out = SPEC_UINT;
        }
        flags |= IFLAG_UNSIGNED;
        break;
      default:
        fprintf(stderr, "ERROR: unexpected type specifier %s\n",
                parser_get_tname(type->symbol));
        status = -1;
        break;
    }

    if (status) {
      fprintf(stderr, "fuck\n");
      return status;
    }
  }

  return status;
}

int validate_type(ASTree *type, TypeSpec *out) {
  int status = 0;
  switch (type->symbol) {
    /*
    case TOK_FUNCTION:
    case TOK_STRUCT:
    case TOK_UNION:
    */
    case TOK_SPEC:
      /* go to integer validation function */
      status = validate_spec(type, out);
      break;
    case TOK_IDENT:
      fprintf(stderr,
              "ERROR: type specifier in incorrect"
              "location\n");
      status = -1;
      break;
    /*
    case TOK_CONST:
    break;
    */
    default:
      fprintf(stderr, "ERROR: unimplemented type: %s\n",
              parser_get_tname(type->symbol));
      status = -1;
      break;
  }
  return status;
}

int validate_ident(ASTree *ident) {
  int status = assign_type(ident);
  if (status) return status;
  if (type_is_function(ident->type)) {
    insert_addrof(ident);
  }
  return 0;
}

int validate_call(ASTree *call) {
  if (astree_first(call)->symbol == TOK_IDENT) {
    /* circumvent normal procedure of turning identifier for function to a
     * pointer to a function
     */
    int status = assign_type(astree_first(call));
    if (status) return status;
  } else {
    ASTree *function = astree_first(call);
    int status = validate_expr(function);
    if (status) return status;
  }

  if (type_is_pointer(astree_first(call)->type)) {
    /* automatic dereference of function pointer */
    int status = insert_indirection(astree_first(call));
    if (status) return status;
  }

  TypeSpec *function_spec = (TypeSpec *)astree_first(call)->type;
  if (!type_is_function(function_spec)) {
    fprintf(stderr,
            "ERROR: cannot call identifier whose type is not function: %s\n",
            extract_ident(astree_first(call))->lexinfo);
    return -1;
  }

  AuxSpec *param_spec = llist_front(&function_spec->auxspecs);
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
    DEBUGS('t', "Comparing types");
    int compatibility = types_compatible(llist_get(param_list, i), call_param,
                                         ARG1_SMV | ARG2_AST);
    if (compatibility == TCHK_INCOMPATIBLE ||
        compatibility == TCHK_EXPLICIT_CAST) {
      fprintf(stderr, "ERROR: incompatible type for argument: %s\n",
              call_param->lexinfo);
      return -1;
    }
  }

  TypeSpec *return_spec = malloc(sizeof(*return_spec));
  int status = strip_aux_type(return_spec, function_spec);
  if (status) return status;
  call->type = return_spec;
  return 0;
}

int validate_assignment(ASTree *assignment) {
  ASTree *dest = astree_first(assignment);
  ASTree *src = astree_second(assignment);
  int status = 0;

  status = validate_expr(dest);
  if (status != 0) return status;
  status = validate_expr(src);
  if (status != 0) return status;

  int compatibility = types_compatible(
      astree_first(assignment), astree_second(assignment), ARG1_AST | ARG2_AST);
  if (compatibility == TCHK_INCOMPATIBLE ||
      compatibility == TCHK_EXPLICIT_CAST) {
    fprintf(stderr, "ERROR: Incompatible types for tokens: %s,%s %s,%s\n",
            parser_get_tname(astree_first(assignment)->symbol),
            astree_first(assignment)->lexinfo,
            parser_get_tname(astree_second(assignment)->symbol),
            astree_second(assignment)->lexinfo);
    status = -2;
  } else if (compatibility == TCHK_IMPLICIT_CAST) {
    insert_cast(assignment, dest->type);
  } /* else if (!(astree_first(assignment)->attributes & ATTR_EXPR_LVAL)) {
     fprintf(stderr, "ERROR: Destination is not an LVAL\n");
     status = -1;
   } */
  /* type is the type of the left operand */
  assignment->type = astree_first(assignment)->type;
  return status;
}

int validate_cast(ASTree *cast) {
  ASTree *cast_spec = astree_first(cast);
  cast->type = calloc(1, sizeof(*cast->type));
  typespec_init((TypeSpec *)cast->type);
  int status = validate_type(cast_spec, (TypeSpec *)cast->type);
  if (status) return status;

  ASTree *to_cast = astree_second(cast);
  status = validate_expr(to_cast);
  if (status) return status;
  int compatibility = types_compatible(cast, to_cast, ARG1_AST | ARG2_AST);
  if (compatibility == TCHK_INCOMPATIBLE)
    return -1;
  else
    return 0;
}

int is_logical_op(ASTree *operator) {
  switch (operator->symbol) {
    case TOK_EQ:
    case TOK_NE:
    case TOK_LE:
    case TOK_GE:
    case '<':
    case '>':
    case '!':
      return 1;
    default:
      return 0;
  }
}

int is_bitwise_op(ASTree *operator) {
  switch (operator->symbol) {
    case TOK_SHR:
    case TOK_SHL:
    case '|':
    case '&':
    case '^':
    case '~':
      return 1;
    default:
      return 0;
  }
}

int validate_logical_op(ASTree *operator) {
  DEBUGS('t', "Validating logical operator %c", operator->symbol);
  ASTree *left = astree_first(operator);
  ASTree *right = astree_second(operator);

  int status = validate_expr(left);
  if (status != 0) return status;
  status = validate_expr(right);
  if (status != 0) return status;

  if (!type_is_int_or_ptr(left->type) || !type_is_int_or_ptr(right->type)) {
    fprintf(stderr, "ERROR: cannot compare non-arithmetic types\n");
    return -1;
  }

  operator->type = & SPEC_INT;
  return 0;
}

int validate_binop(ASTree *operator) {
  DEBUGS('t', "Validating binary operator %c", operator->symbol);
  ASTree *left = astree_first(operator);
  ASTree *right = astree_second(operator);
  int status = 0;

  status = validate_expr(left);
  if (status != 0) return status;
  status = validate_expr(right);
  if (status != 0) return status;

  const TypeSpec *promoted_type = &SPEC_EMPTY;
  status = determine_promotion(left, right, &promoted_type);
  if (status != 0) return status;

  int compatibility =
      types_compatible(promoted_type, left, ARG1_TYPE | ARG2_AST);
  if (compatibility == CONV_IMPLICIT_CAST) {
    insert_cast(operator, promoted_type);
  } else if (compatibility == CONV_INCOMPATIBLE ||
             compatibility == CONV_EXPLICIT_CAST) {
    /* should not happen but I am bad at programming so it would be worth
     * checking for
     */
    fprintf(stderr, "Promotion failed on left operand; aborting.\n");
    abort();
  }

  compatibility = types_compatible(promoted_type, right, ARG1_TYPE | ARG2_AST);
  if (compatibility == CONV_IMPLICIT_CAST) {
    insert_cast(operator, promoted_type);
  } else if (compatibility == CONV_INCOMPATIBLE ||
             compatibility == CONV_EXPLICIT_CAST) {
    /* should not happen but I am bad at programming so it would be worth
     * checking for
     */
    fprintf(stderr, "Promotion failed on right operand; aborting.\n");
    abort();
  }

  if (is_logical_op(operator)) {
    if (!(type_is_comparable(left->type) && type_is_comparable(right->type))) {
      fprintf(stderr,
              "ERROR: '%s' arguments were not comparable\n", operator->lexinfo);
      status = -1;
    } else {
      operator->type = & SPEC_INT;
    }
  } else if (is_bitwise_op(operator) && !(type_is_int_or_ptr(left->type) &&
                                          type_is_int_or_ptr(right->type))) {
    fprintf(stderr,
            "ERROR: '%s' arguments must be of type int\n", operator->lexinfo);
    status = -1;
  } else {
    operator->type = promoted_type;
  }

  return status;
}

int validate_indirection(ASTree *indirection) {
  int status = validate_expr(astree_first(indirection));
  if (status) return status;
  if (!type_is_pointer(indirection->type) &&
      !type_is_array(indirection->type)) {
    /* error; indirection can only be used on pointers and arrays */
  } else {
    TypeSpec *indirection_spec = malloc(sizeof(*indirection_spec));
    int status =
        typespec_copy(indirection_spec, astree_first(indirection)->type);
    if (status) return status;
    llist_pop_front(&indirection_spec->auxspecs);
    indirection->type = indirection_spec;
  }
  return 0;
}

int validate_addrof(ASTree *addrof) {
  int status = validate_expr(astree_first(addrof));
  if (status) return status;
  /* TODO(Robert): check that operand is an lval */
  TypeSpec *addrof_spec = malloc(sizeof(*addrof_spec));
  status = typespec_copy(addrof_spec, astree_first(addrof)->type);
  if (status) return status;
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  llist_push_front(&addrof_spec->auxspecs, ptr_aux);
  addrof->type = addrof_spec;
  return 0;
}

int validate_subscript(ASTree *subscript) {
  int status = validate_expr(astree_first(subscript));
  if (status) return status;
  if (!type_is_pointer(subscript->type) && !type_is_array(subscript->type)) {
    /* error; subscript can only be used on pointers and arrays */
  } else {
    TypeSpec *subscript_spec = malloc(sizeof(*subscript_spec));
    int status = typespec_copy(subscript_spec, astree_first(subscript)->type);
    if (status) return status;
    llist_pop_front(&subscript_spec->auxspecs);
    subscript->type = subscript_spec;
  }
  return 0;
}

int validate_unop(ASTree *operator) {
  DEBUGS('t', "Validating unary operator %c", operator->symbol);
  int status = 0;
  ASTree *operand = astree_first(operator);
  status = validate_expr(operand);
  if (status != 0) return status;

  if (is_logical_op(operator)) {
    if (!type_is_comparable(operand->type)) {
      fprintf(stderr,
              "ERROR: '%s' argument must be comparable\n", operator->lexinfo);
      status = -1;
    } else {
      operator->type = & SPEC_INT;
    }
  } else {
    if (is_bitwise_op(operator) && !type_is_int_or_ptr(operand->type)) {
      fprintf(stderr, "ERROR: '%s' argument must be an int or pointer\n",
                      operator->lexinfo);
      status = -1;
    } else if (operator->symbol == '+' && !type_is_arithmetic(operand->type)) {
      fprintf(stderr, "ERROR: '%s' argument must be of arithmetic type\n",
                      operator->lexinfo);
      status = -1;
    } else {
      status = determine_promotion(operand, NULL, &operator->type);
    }
  }

  return status;
}

int validate_equality(ASTree *operator) {
  DEBUGS('t', "Validating binary operator %s",
         parser_get_tname(operator->symbol));
  ASTree *left = astree_first(operator);
  ASTree *right = astree_second(operator);

  int status = 0;
  status = validate_expr(left);
  if (status != 0) return status;
  status = validate_expr(right);
  if (status != 0) return status;

  operator->type = & SPEC_INT;
  /* comparison of types is limited to the following:
   * 1. between arithmetic types
   * 2. between pointers, which can be qualified, unqualified, incomplete,
   *    and/or a pointer to void
   * 3. between pointers and NULL
   */
  if (type_is_arithmetic(left->type) && type_is_arithmetic(right->type)) {
    /* determine promotions and insert casts where necessary */
    const TypeSpec *promoted_type = NULL;
    status = determine_promotion(left, right, &promoted_type);
    if (status) return status;
    int compatibility =
        types_compatible(promoted_type, left, ARG1_TYPE | ARG2_AST);
    if (compatibility == CONV_IMPLICIT_CAST) {
      insert_cast(operator, promoted_type);
    } else if (compatibility == CONV_INCOMPATIBLE ||
               compatibility == CONV_EXPLICIT_CAST) {
      /* should not happen but I am bad at programming so it would be worth
       * checking for
       */
      fprintf(stderr, "Promotion failed on left operand; aborting.");
      abort();
    }

    compatibility =
        types_compatible(promoted_type, right, ARG1_TYPE | ARG2_AST);
    if (compatibility == CONV_IMPLICIT_CAST) {
      insert_cast(operator, promoted_type);
    } else if (compatibility == CONV_INCOMPATIBLE ||
               compatibility == CONV_EXPLICIT_CAST) {
      /* should not happen but I am bad at programming so it would be worth
       * checking for
       */
      fprintf(stderr, "Promotion failed on right operand; aborting.");
      abort();
    }
  } else if (type_is_pointer(left->type) && type_is_pointer(right->type)) {
    /* do nothing if both are pointers */
  } else {
    fprintf(stderr,
            "ERROR: Incompatible types for operator: %i\n", operator->symbol);
    status = -1;
  }
  return status;
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
      status = validate_equality(expression);
      break;
    case TOK_OR:
    case TOK_AND:
      status = validate_logical_op(expression);
      break;
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
    case TOK_CALL:
      /* expression->attributes |= ATTR_EXPR_VREG; */
      status = validate_call(expression);
      break;
    case TOK_SUBSCRIPT:
      status = validate_subscript(expression);
      ;
      break;
    case TOK_INDIRECTION:
      status = validate_indirection(expression);
      ;
      break;
    case TOK_ADDROF:
      status = validate_addrof(expression);
      ;
      break;
    case TOK_ARROW:
      /* TODO(Robert): pointers and struct member access */
      /* evaluate left but not right since right
       * is always an ident
       */
      status = -1;
      break;
    case TOK_INTCON:
      status = validate_intcon(expression);
      break;
    case TOK_CHARCON:
      /* types are set on construction and we don't need to do
       * anything else
       */
      break;
    case TOK_STRINGCON:
      /* do nothing? this will get taking of during assembly generation */
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

int define_params(ASTree *params, ASTree *ident, SymbolValue *function_entry) {
  AuxSpec *function_aux = calloc(1, sizeof(*function_aux));
  function_aux->aux = AUX_FUNCTION;
  LinkedList *param_entries = &(function_aux->data.params);
  /* type is not resposible for freeing symbol information */
  llist_init(param_entries, NULL, NULL);
  create_scope(&(ident->symbol_table));
  size_t i;
  for (i = 0; i < astree_count(params); ++i) {
    ASTree *param = astree_get(params, i);
    const char *param_id_str = extract_ident(param)->lexinfo;
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
    llist_push_back(param_entries, param_entry);
  }
  /* temporarily leave function prototype scope to work on global table */
  leave_scope(&(ident->symbol_table));
  return 0;
}

int define_array(ASTree *array, SymbolValue *symbol) {
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
  llist_push_back(&symbol->type.auxspecs, aux_array);
  return 0;
}

int define_pointer(ASTree *pointer, SymbolValue *symbol) {
  AuxSpec *aux_pointer = calloc(1, sizeof(*aux_pointer));
  aux_pointer->aux = AUX_POINTER;
  llist_push_back(&symbol->type.auxspecs, aux_pointer);
  return 0;
}

int validate_dirdecl(ASTree *dirdecl, ASTree *ident, SymbolValue *symbol) {
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

  switch (dirdecl->symbol) {
    case TOK_DECLARATOR: {
      size_t i;
      for (i = 0; i < astree_count(dirdecl); ++i) {
        int status = validate_dirdecl(astree_get(dirdecl, i), ident, symbol);
        if (status) return status;
      }
    }
      return 0;
      break;
    case TOK_ARRAY:
      return define_array(dirdecl, symbol);
      break;
    case TOK_POINTER:
      return define_pointer(dirdecl, symbol);
      break;
    case TOK_FUNCTION:
      return define_params(dirdecl, ident, symbol);
      break;
    default:
      fprintf(stderr, "ERROR: invalid direct declarator for symbol\n");
      return -1;
      break;
  }
}

int declare_symbol(ASTree *declaration, size_t i, TypeSpec *declspecs) {
  ASTree *declarator = astree_get(declaration, i);
  ASTree *identifier = extract_ident(declarator);
  DEBUGS('t', "Making object entry for value %s", identifier->lexinfo);
  SymbolValue *symbol = symbol_value_init(&declarator->loc);
  symbol->type = *declspecs;

  size_t j;
  for (j = 0; j < astree_count(declarator); ++j) {
    int status =
        validate_dirdecl(astree_get(declarator, j), identifier, symbol);
    if (status) return status;
  }

  size_t identifier_len = strnlen(identifier->lexinfo, MAX_IDENT_LEN);
  SymbolValue *exists = NULL;
  int in_current_scope =
      locate_symbol(identifier->lexinfo, identifier_len, &exists);
  if (exists && in_current_scope) {
    if (type_is_function(&exists->type) && type_is_function(&symbol->type)) {
      int compatibility =
          types_compatible(&exists->type, &symbol->type, ARG1_SMV | ARG2_SMV);
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

int define_symbol(ASTree *declaration, size_t i, TypeSpec *declspecs) {
  ASTree *declarator = astree_get(declaration, i);
  ASTree *expr = astree_get(declaration, i + 1);
  int status = declare_symbol(declaration, i, declspecs);
  if (status) return status;
  status = validate_expr(expr);
  if (status) return status;

  const TypeSpec *promoted_type = &SPEC_EMPTY;
  status = determine_promotion(declarator, expr, &promoted_type);
  if (status) return status;

  int compatibility = types_compatible(declarator, expr, ARG1_AST | ARG2_AST);
  if (compatibility == TCHK_INCOMPATIBLE ||
      compatibility == TCHK_EXPLICIT_CAST) {
    fprintf(stderr, "ERROR: Incompatible type for object %s\n",
            extract_ident(declarator)->lexinfo);
    return -1;
  } else if (compatibility == TCHK_IMPLICIT_CAST) {
    insert_cast(declaration, promoted_type);
  }
  return 0;
}

int define_body(ASTree *function, SymbolValue *entry) {
  int status = 0;
  ASTree *identifier = extract_ident(function);
  enter_scope(&identifier->symbol_table);

  if (astree_count(function) == 3) {
    DEBUGS('t', "Defining function: %s", identifier->lexinfo);
    ASTree *body = astree_third(function);
    current_function = entry;
    /* set return status */
    status = validate_stmt(body);
    current_function = NULL;
    entry->is_defined = 1;
  }

  /* finalize scope */
  finalize_scope(&identifier->symbol_table);
  return status;
}

int define_function(ASTree *function, TypeSpec *declspecs) {
  ASTree *declaration = function;
  ASTree *declarator = astree_second(function);
  ASTree *identifier = extract_ident(declarator);

  SymbolValue *symbol = symbol_value_init(extract_loc(declarator));
  symbol->type = *declspecs;
  size_t i;
  for (i = 0; i < astree_count(declarator); ++i) {
    int status =
        validate_dirdecl(astree_get(declarator, i), identifier, symbol);
    if (status) return status;
  }

  const char *function_ident = extract_ident(function)->lexinfo;
  size_t function_ident_len = strnlen(function_ident, MAX_IDENT_LEN);
  SymbolValue *existing_entry = NULL;
  locate_symbol(function_ident, function_ident_len, &existing_entry);
  if (existing_entry) {
    if (types_compatible(existing_entry, symbol, ARG1_SMV | ARG2_SMV) ==
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
    return assign_type(function);
  } else {
    int status = insert_symbol(function_ident, function_ident_len, symbol);
    if (status) return status;
    status = define_body(function, symbol);
    if (status) return status;
    return assign_type(function);
  }
}

int define_members(ASTree *structure, SymbolValue *structure_entry) {
  AuxSpec *struct_aux = calloc(1, sizeof(*struct_aux));
  Map *members = &struct_aux->data.members;
  /* TODO(Robert): in the interest of code reuse I used the existing functions
   * for entering and leaving scopes and making entries within a scope that
   * are normally used for objects and functions, even though struct members
   * don't work quite the same way. check to make sure everything is doing
   * okay later on
   */
  create_scope(members);
  /* start from 2nd child; 1st was type name */
  size_t i;
  for (i = 1; i < astree_count(structure); ++i) {
    ASTree *member = astree_get(structure, i);
    const char *member_id_str = extract_ident(member)->lexinfo;
    size_t member_id_str_len = strnlen(member_id_str, MAX_IDENT_LEN);
    DEBUGS('t', "Found structure member: %s", member_id_str);
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
      DEBUGS('t', "Field inserted at %s", astree_second(member)->lexinfo);
    }
  }
  finalize_scope(members);
  llist_push_front(&structure_entry->type.auxspecs, struct_aux);
  return 0;
}

int define_structure(ASTree *structure) {
  const char *structure_type = extract_ident(structure)->lexinfo;
  const size_t structure_type_len = strnlen(structure_type, MAX_IDENT_LEN);
  DEBUGS('t', "Defining structure type: %s", structure_type);
  SymbolValue *exists = NULL;
  locate_symbol((char *)structure_type, structure_type_len, &exists);
  SymbolValue *structure_value = symbol_value_init(extract_loc(structure));

  if (exists) {
    int status = define_members(structure, structure_value);
    if (status) return status;
    if (types_compatible(exists, structure_value, ARG1_SMV | ARG2_SMV) !=
        TCHK_COMPATIBLE) {
      fprintf(stderr, "ERROR: redefinition of structure %s\n", structure_type);
      return -1;
    }
    /* discard duplicate */
    return symbol_value_destroy(structure_value);
  } else {
    int status =
        insert_symbol(structure_type, structure_type_len, structure_value);
    if (status) return status;
    return define_members(structure, structure_value);
  }
}

int validate_declaration(ASTree *declaration) {
  ASTree *spec_list = astree_first(declaration);
  TypeSpec declspecs = SPEC_EMPTY;
  int status = validate_type(spec_list, &declspecs);
  if (status) return status;

  size_t i;
  for (i = 1; i < astree_count(declaration); ++i) {
    /* call a function that constructs the rest of the type */
    ASTree *next = astree_get(declaration, i + 1);
    if (next && next->symbol == TOK_BLOCK) {
      /* function definition */
      return define_function(declaration, &declspecs);
    } else if (next && next->symbol != TOK_DECLARATOR) {
      /* variable declaration and initialization */
      return define_symbol(declaration, i, &declspecs);
    } else {
      /* variable/function declaration */
      return declare_symbol(declaration, i, &declspecs);
    }
  }
  return 0;
}

int validate_return(ASTree *ret) {
  TypeSpec ret_spec = SPEC_EMPTY;
  int status = strip_aux_type(&ret_spec, &current_function->type);
  if (status) return status;
  if (astree_count(ret) > 0) {
    status = validate_expr(astree_first(ret));
    if (status) return status;
  }

  const TypeSpec *expr_spec =
      astree_count(ret) > 0 ? astree_first(ret)->type : &SPEC_VOID;
  int compatibility =
      types_compatible(&ret_spec, expr_spec, ARG1_TYPE | ARG2_TYPE);
  if (compatibility == TCHK_INCOMPATIBLE ||
      compatibility == TCHK_EXPLICIT_CAST) {
    fprintf(stderr, "ERROR: Incompatible return type\n");
    status = -1;
  } else if (compatibility == TCHK_IMPLICIT_CAST) {
    insert_cast(ret, &ret_spec);
    status = 0;
    /* ret->attributes |= ATTR_EXPR_VREG;
    ret->attributes |= astree_first(ret)->attributes; */
  } else {
    status = 0;
    /* ret->attributes |= ATTR_EXPR_VREG;
    ret->attributes |= astree_first(ret)->attributes; */
  }

  /* free temporary spec */
  typespec_destroy(&ret_spec);
  return status;
}

int validate_ifelse(ASTree *ifelse) {
  int status = validate_expr(astree_first(ifelse));
  if (status) return status;
  if (!type_is_integer(astree_first(ifelse)->type)) {
    /* error: conditional expression must be of type int */
    status = -1;
    return status;
  }
  /* Note: no matter whether or not there is an actual block here or just a
   * single statement, a new scope still needs to be created for it. For
   * example, if the single statement was a declaration, that object would not
   * be visible anywhere
   */
  create_scope(&(astree_second(ifelse)->symbol_table));
  status = validate_stmt(astree_second(ifelse));
  finalize_scope(&(astree_second(ifelse)->symbol_table));
  if (status) return status;
  if (astree_count(ifelse) == 3) {
    create_scope(&(astree_third(ifelse)->symbol_table));
    status = validate_stmt(astree_third(ifelse));
    finalize_scope(&(astree_third(ifelse)->symbol_table));
  }
  return status;
}

int validate_while(ASTree *whole) {
  int status = validate_expr(astree_first(whole));
  if (status != 0) return status;
  if (!type_is_integer(astree_first(whole)->type)) {
    /* error: conditional expression must be of type int */
    status = -1;
    return status;
  }
  create_scope(&(astree_second(whole)->symbol_table));
  status = validate_stmt(astree_second(whole));
  finalize_scope(&(astree_second(whole)->symbol_table));
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
       * only thing left that this could be is an expression
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
      default:
        fprintf(stderr, "ERROR: Unexpected symbol at top level: %s\n",
                parser_get_tname(child->symbol));
        return -1;
    }
  }
  finalize_scope(&root->symbol_table);
  return 0;
}
