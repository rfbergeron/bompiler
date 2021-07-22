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
#include "strset.h"
#include "symtable.h"

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

enum type_checker_action {
  TCHK_COMPATIBLE,
  TCHK_IMPLICIT_CAST,
  TCHK_EXPLICIT_CAST,
  TCHK_INCOMPATIBLE,
  TCHK_PROMOTE_LEFT,
  TCHK_PROMOTE_RIGHT,
  TCHK_PROMOTE_BOTH,
  TCHK_E_NO_FLAGS
};

static SymbolValue *current_function = NULL;

/* forward declarations for mutual recursion */
int validate_expr(ASTree *statement);
int validate_stmt(ASTree *statement);

const char *extract_ident(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_TYPE_ID:
      return astree_second(tree)->lexinfo;
      break;
    case TOK_STRUCT:
      return astree_first(tree)->lexinfo;
      break;
    case TOK_FUNCTION:
      return astree_second(astree_first(tree))->lexinfo;
      break;
    case TOK_IDENT:
      return tree->lexinfo;
      break;
    default:
      fprintf(stderr, "ERROR: unable to get identifier for tree node %s\n",
              parser_get_tname(tree->symbol));
      return NULL;
      break;
  }
}

const TypeSpec *extract_type(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_TYPE_ID:
      return astree_second(tree)->type;
      break;
    case TOK_STRUCT:
      return astree_first(tree)->type;
      break;
    case TOK_FUNCTION:
      return astree_second(astree_first(tree))->type;
      break;
    case TOK_CALL:
    case TOK_IDENT:
      return tree->type;
      break;
    default:
      fprintf(stderr, "Unable to extract type from symbol %s\n",
              parser_get_tname(tree->symbol));
      return NULL;
      break;
  }
}

const Location *extract_loc(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_TYPE_ID:
      return &astree_second(tree)->loc;
      break;
    case TOK_STRUCT:
      return &astree_first(tree)->loc;
      break;
    case TOK_FUNCTION:
      return &astree_second(astree_first(tree))->loc;
      break;
    case TOK_CALL:
    case TOK_IDENT:
      return &tree->loc;
      break;
    default:
      fprintf(stderr, "Unable to extract location from symbol %s\n",
              parser_get_tname(tree->symbol));
      return NULL;
      break;
  }
}

int assign_type(ASTree *tree) {
  DEBUGS('t', "Attempting to assign a type");
  const char *id_str = extract_ident(tree);
  size_t id_str_len = strnlen(id_str, MAX_IDENT_LEN);
  SymbolValue *symval = NULL;
  int in_current_scope = locate_symbol(id_str, id_str_len, &symval);

  if (symval) {
    DEBUGS('t', "Assigning %s a symbol", id_str);
    switch (tree->symbol) {
      case TOK_FUNCTION:
        astree_second(astree_first(tree))->type = &(symval->type);
        break;
      case TOK_TYPE_ID:
        astree_second(tree)->type = &(symval->type);
        break;
      case TOK_STRUCT:
        astree_first(tree)->type = &(symval->type);
        break;
      case TOK_CALL:
        tree->type = symval->type.nested;
        astree_first(tree)->type = &(symval->type);
        break;
      case TOK_IDENT:
      default:
        tree->type = &(symval->type);
        break;
    }
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

void insert_cast(ASTree *tree, size_t index, const TypeSpec *type) {
  /* since the linked list has a destructor defined for elements, we can't just
   * use llist_get and llist_delete here, since that will call astree_destroy
   * on the node. Instead we call extract, which removes the element from the
   * list and returns the value
   */
  ASTree *to_cast = llist_extract(&tree->children, index);
  ASTree *cast = astree_init(TOK_CAST, to_cast->loc, "_cast");
  cast->type = type;
  llist_insert(&tree->children, astree_adopt(cast, to_cast, NULL, NULL), index);
}

int type_is_arithmetic(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED ||
          type->base == TYPE_FLOAT || type->base == TYPE_DOUBLE);
}

int type_is_integer(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED);
}

int type_is_int_or_ptr(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED ||
          type->base == TYPE_POINTER);
}

int type_is_comparable(const TypeSpec *type) {
  switch (type->base) {
    case TYPE_SIGNED:
    case TYPE_UNSIGNED:
    case TYPE_POINTER:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
      return 1;
    default:
      return 0;
  }
}

/* Remember that this function is not directly called during arithmetic,
 * bitwise, or boolean operations; it is only called for assignments, function
 * calls, casts, and as a helper function when preforming promotions.
 *
 * Arithmetic operations use the "determine_promotion" function, while
 * boolean and bitwise operations check that their operands are integers or
 * pointers
 *
 * whenever it matters, arg1 is taken to be the type being cast to, while arg2
 * is the type being cast from
 */
int types_compatible(const void *arg1, const void *arg2, unsigned int flags) {
  const TypeSpec *type1 = NULL;
  const TypeSpec *type2 = NULL;
  int action = CONV_COMPATIBLE;

  /* extract type from first argument */
  if (flags & ARG1_AST) {
    type1 = ((ASTree *)arg1)->type;
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
    type2 = ((ASTree *)arg2)->type;
  } else if (flags & ARG2_SMV) {
    type2 = &(((SymbolValue *)arg2)->type);
  } else if (flags & ARG2_TYPE) {
    type2 = arg2;
  } else {
    fprintf(stderr, "ERROR: no flags provided for argument 2\n");
    return TCHK_E_NO_FLAGS;
  }

  /* TODO(Robert): check for arrays, pointers, structs, unions, and valid
   * conversions between types
   */
  int ret = TCHK_COMPATIBLE;
  if (type_is_int_or_ptr(type1) && type_is_int_or_ptr(type2)) {
    if ((type1->base == TYPE_SIGNED) && (type2->base == TYPE_UNSIGNED)) {
      ret = TCHK_EXPLICIT_CAST;
    } else if ((type1->base == TYPE_UNSIGNED) && (type2->base == TYPE_SIGNED)) {
      ret = TCHK_IMPLICIT_CAST;
    }
    if (type1->width > type2->width) {
      ret = TCHK_IMPLICIT_CAST;
    } else if (type1->width < type2->width) {
      ret = TCHK_EXPLICIT_CAST;
    }
  } else if (type1->base == TYPE_FUNCTION && type2->base == TYPE_FUNCTION) {
    /* compare return types, then parameter types, returning the weakest
     * compatibilty
     */
  } else {
    ret = TCHK_INCOMPATIBLE;
  }
  return ret;
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
  const struct typespec *type1 = arg1 ? arg1->type : NULL;
  const struct typespec *type2 = arg2 ? arg2->type : &SPEC_INT;

  if (type1 == NULL) {
    fprintf(stderr, "First argument not provided to promotion routine\n");
    return -1;
  } else if (type1->base == TYPE_POINTER) {
    /* promote to pointer if either operand is one, prefer type of left op */
    *out = type1;
  } else if (type2->base == TYPE_POINTER) {
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

int validate_int_spec(ASTree *spec_list, TypeSpec *out) {
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
      case TOK_INT:
        if (flags & (IFLAG_INT | IFLAG_CHAR)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (!(flags & (IFLAG_LONG | IFLAG_SHRT | IFLAG_UNSIGNED))) {
          *out = SPEC_INT;
        }
        flags |= IFLAG_INT;
        break;
      case TOK_CHAR:
        if (flags & (IFLAG_CHAR | IFLAG_INT | IFLAG_SHRT | IFLAG_LONG)) {
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
        if (flags & (IFLAG_LONG | IFLAG_SHRT | IFLAG_CHAR)) {
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
        if (flags & (IFLAG_SHRT | IFLAG_LONG | IFLAG_CHAR)) {
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
        if (flags & (IFLAG_SIGNED | IFLAG_UNSIGNED)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else if (!(flags & (IFLAG_LONG | IFLAG_SHRT | IFLAG_CHAR))) {
          *out = SPEC_INT;
        }
        flags |= IFLAG_SIGNED;
        break;
      case TOK_UNSIGNED:
        if (flags & (IFLAG_UNSIGNED | IFLAG_SIGNED)) {
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
    case TOK_INT_SPEC:
      /* go to integer validation function */
      status = validate_int_spec(type, out);
      break;
    case TOK_IDENT:
      fprintf(stderr,
              "ERROR: type specifier in incorrect"
              "location\n");
      status = -1;
      break;
    case TOK_VOID:
      out->base = TYPE_VOID;
      break;
      /*
      case TOK_CONST:
      break;
      */
    default:
      fprintf(stderr, "ERROR: unimplemented type: %s",
              parser_get_tname(type->symbol));
      status = -1;
      break;
  }
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

int validate_call(ASTree *call) {
  const char *identifier = astree_first(call)->lexinfo;
  /* params are at the same level as the id */
  SymbolValue *function = NULL;
  locate_symbol(identifier, strlen(identifier), &function);

  if (function) {
    struct llist *params = &(function->type.data.params);
    /* subtract one since function identifier is also a child */
    if (astree_count(call) - 1 != llist_size(params)) {
      fprintf(stderr, "ERROR: incorrect number of arguments %s\n", identifier);
      return -1;
    }
    DEBUGS('t', "Validating arguments for call to %s, num call->children: %d",
           identifier, astree_count(call) - 1);
    size_t i;
    for (i = 0; i < llist_size(params); ++i) {
      DEBUGS('t', "Validating argument %d", i);
      /* add 1 to index to skip function identifier */
      ASTree *param = astree_get(call, i + 1);
      int status = validate_expr(param);

      if (status != 0) return status;
      DEBUGS('t', "Comparing types");
      int compatibility =
          types_compatible(param, llist_get(params, i), ARG1_AST | ARG2_SMV);
      if (compatibility == TCHK_INCOMPATIBLE ||
          compatibility == TCHK_EXPLICIT_CAST) {
        fprintf(stderr, "ERROR: incompatible type for argument: %s\n",
                param->lexinfo);
        return -1;
      }
    }

    call->type = function->type.nested;
    astree_first(call)->type = &(function->type);
    return 0;
  } else {
    fprintf(stderr, "ERROR: Invalid call to function %s\n", identifier);
    return -1;
  }
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
    insert_cast(assignment, 1, dest->type);
  } else if (!(astree_first(assignment)->attributes & ATTR_EXPR_LVAL)) {
    fprintf(stderr, "ERROR: Destination is not an LVAL\n");
    status = -1;
  }
  /* type is the type of the left operand */
  assignment->type = astree_first(assignment)->type;
  return status;
}

const char *make_dummy_string(const Location *loc) {
  char dummy[1024];
  dummy[0] = '_';
  location_to_string(loc, dummy + 1, 255);
  strcat(dummy, "_dummy_entry");

  /* TODO(Robert): pulling in strset just to do this seems a little silly;
   * figure out a better way
   */
  return string_set_intern(dummy);
}

int validate_cast(ASTree *cast) {
  ASTree *to_cast = NULL;
  int status = 0;

  if (astree_count(cast) > 1) {
    /* explicit cast; process type specifiers */
    ASTree *cast_spec = astree_first(cast);
    /* create dummy symbol entry for cast specifier */
    SymbolValue *dummy_entry = symbol_value_init(&cast_spec->loc);
    status = validate_type(cast_spec, &dummy_entry->type);
    if (status) return status;
    /* NOTE: this is one of two places in the code where a new value is
     * inserted into the string set
     */
    const char *dummy_ident = make_dummy_string(&cast_spec->loc);
    insert_symbol(dummy_ident, strlen(dummy_ident), dummy_entry);
    cast->type = &dummy_entry->type;
    to_cast = astree_second(cast);
  } else {
    /* implicit cast/promotion; no type specifiers */
    to_cast = astree_first(cast);
  }

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
    case TOK_AND:
    case TOK_OR:
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
    insert_cast(operator, 0, promoted_type);
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
    insert_cast(operator, 1, promoted_type);
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
        types_compatible(left, &promoted_type, ARG1_AST | ARG2_TYPE);
    if (compatibility == CONV_IMPLICIT_CAST) {
      insert_cast(operator, 0, promoted_type);
    } else if (compatibility == CONV_INCOMPATIBLE ||
               compatibility == CONV_EXPLICIT_CAST) {
      /* should not happen but I am bad at programming so it would be worth
       * checking for
       */
      fprintf(stderr, "Promotion failed on left operand; aborting.");
      abort();
    }

    compatibility =
        types_compatible(right, &promoted_type, ARG1_AST | ARG2_TYPE);
    if (compatibility == CONV_IMPLICIT_CAST) {
      insert_cast(operator, 1, promoted_type);
    } else if (compatibility == CONV_INCOMPATIBLE ||
               compatibility == CONV_EXPLICIT_CAST) {
      /* should not happen but I am bad at programming so it would be worth
       * checking for
       */
      fprintf(stderr, "Promotion failed on right operand; aborting.");
      abort();
    }
  } else if ((left->type->base == TYPE_POINTER) &&
             (right->type->base == TYPE_POINTER)) {
    /* good; nothing else to do */
  } else {
    fprintf(stderr,
            "ERROR: Incompatible types for operator: %i\n", operator->symbol);
    status = -1;
  }
  return status;
}

int validate_return(ASTree *ret) {
  int status = 0;
  if (astree_count(ret) > 0) {
    if (current_function->type.nested->base == TYPE_VOID) {
      fprintf(stderr,
              "ERROR: Return statement with value in void "
              "function\n");
      status = -1;
    } else {
      status = validate_expr(astree_first(ret));
      if (status) return status;
      TypeSpec *return_type = current_function->type.nested;
      int compatibility = types_compatible(return_type, astree_first(ret),
                                           ARG1_TYPE | ARG2_AST);
      if (compatibility == TCHK_INCOMPATIBLE ||
          compatibility == TCHK_EXPLICIT_CAST) {
        fprintf(stderr, "ERROR: Incompatible return type\n");
        status = -1;
      } else {
        ret->attributes |= ATTR_EXPR_VREG;
        ret->attributes |= astree_first(ret)->attributes;
      }
    }
  } else if (current_function->type.nested->base != TYPE_VOID) {
    fprintf(stderr, "ERROR: Function does not return promised value\n");
    status = -1;
  }
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

int make_object_entry(ASTree *object) {
  ASTree *type = astree_first(object);
  ASTree *identifier = astree_second(object);
  DEBUGS('t', "Making object entry for value %s", identifier->lexinfo);
  identifier->attributes |= ATTR_EXPR_LVAL;
  SymbolValue *symbol = symbol_value_init(&(astree_second(object)->loc));
  int status = validate_type(type, &symbol->type);
  if (status) return status;
  size_t identifier_len = strnlen(identifier->lexinfo, MAX_IDENT_LEN);
  status = insert_symbol(identifier->lexinfo, identifier_len, symbol);
  if (status) {
    fprintf(stderr, "your data structure library sucks.\n");
    abort();
  }
  status = assign_type(object);
  if (status) return status;

  if (astree_count(object) == 3) {
    ASTree *init_value = astree_third(object);
    status = validate_expr(init_value);
    if (status) return status;
    const TypeSpec *promoted_type = &SPEC_EMPTY;
    status = determine_promotion(identifier, init_value, &promoted_type);
    if (status) return status;
    int compatibility =
        types_compatible(identifier, init_value, ARG1_AST | ARG2_AST);
    if (compatibility == TCHK_INCOMPATIBLE ||
        compatibility == TCHK_EXPLICIT_CAST) {
      fprintf(stderr, "ERROR: Incompatible type for object %s\n",
              identifier->lexinfo);
      return -1;
    } else if (compatibility == TCHK_IMPLICIT_CAST) {
      insert_cast(object, 2, promoted_type);
    }
  }

  return 0;
}

int define_members(ASTree *structure, SymbolValue *structure_entry) {
  Map *members = &(structure_entry->type.data.members);
  create_scope(members);
  /* start from 2nd child; 1st was type name */
  size_t i;
  for (i = 1; i < astree_count(structure); ++i) {
    /* TODO(Robert): Originally, the symbol's member table was not set until
     * after they were all parsed. Make sure that it is okay to do it right
     * away
     */
    ASTree *member = astree_get(structure, i);
    const char *member_id_str = extract_ident(member);
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
      int status = make_object_entry(member);
      if (status != 0) return status;
      DEBUGS('t', "Field inserted at %s", astree_second(member)->lexinfo);
    }
  }
  finalize_scope(members);
  return 0;
}

int make_structure_entry(ASTree *structure) {
  const char *structure_type = extract_ident(structure);
  const size_t structure_type_len = strnlen(structure_type, MAX_IDENT_LEN);
  DEBUGS('t', "Defining structure type: %s", structure_type);
  SymbolValue *structure_value = NULL;
  locate_symbol((char *)structure_type, structure_type_len, &structure_value);
  if (structure_value) {
    /* TODO(Robert): allow duplicates if definition matches */
    fprintf(stderr, "ERROR: Duplicate definition of structure %s\n",
            structure_type);
    return -1;
  } else {
    structure_value = symbol_value_init(extract_loc(structure));
    /* TODO(Robert): assign struct type information (structure name, base type,
     * etc.
     */
    int status =
        insert_symbol(structure_type, structure_type_len, structure_value);
    if (status) return status;
    /* TODO(Robert): in the interest of code reuse I used the existing functions
     * for entering and leaving scopes and making entries within a scope that
     * are normally used for objects and functions, even though struct members
     * don't work quite the same way. check to make sure everything is doing
     * okay later on
     */
    status = define_members(structure, structure_value);
    if (status) return status;
    return assign_type(structure);
  }
}

int define_params(ASTree *params, SymbolValue *function_entry) {
  LinkedList *param_entries = &(function_entry->type.data.params);
  /* type is not resposible for freeing symbol information */
  llist_init(param_entries, NULL, NULL);
  create_scope(&(params->symbol_table));
  size_t i;
  for (i = 0; i < astree_count(params); ++i) {
    ASTree *param = astree_get(params, i);
    const char *param_id_str = extract_ident(param);
    DEBUGS('t', "Defining function parameter %s", param_id_str);
    /*
     * TODO(Robert): implement find function(s) in badlib and search for
     * duplicate parameters
     */
    int status = make_object_entry(param);
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
  leave_scope(&(params->symbol_table));
  return 0;
}

int define_body(ASTree *function, SymbolValue *entry) {
  DEBUGS('t', "Defining function: %s", extract_ident(function));
  ASTree *params = astree_second(function);
  ASTree *body = astree_third(function);
  /* move symbol table from parameters to function body */
  body->symbol_table = params->symbol_table;
  memset(&params->symbol_table, 0, sizeof(params->symbol_table));
  /* enter block scope again */
  enter_scope(&body->symbol_table);
  current_function = entry;
  int status = validate_stmt(body);
  if (status) return status;
  current_function = NULL;
  entry->is_defined = 1;
  /* finalize scope */
  finalize_scope(&body->symbol_table);
  return 0;
}

int make_function_entry(ASTree *function) {
  ASTree *type_id = astree_first(function);
  SymbolValue *function_entry =
      symbol_value_init(&(astree_second(type_id)->loc));
  function_entry->type = SPEC_FUNCTION;
  function_entry->type.nested = malloc(sizeof(TypeSpec));
  /* validate return type */
  int status =
      validate_type(astree_first(type_id), function_entry->type.nested);
  if (status) return status;
  if (function_entry->type.nested->base == TYPE_ARRAY) {
    fprintf(stderr, "ERROR: Function %s has an array return type.\n",
            extract_ident(type_id));
    /* TODO(rbergero): keep going if we can */
    return -1;
  }

  ASTree *params = astree_second(function);
  status = define_params(params, function_entry);
  if (status) return status;

  const char *function_ident = extract_ident(function);
  size_t function_ident_len = strnlen(function_ident, MAX_IDENT_LEN);
  SymbolValue *existing_entry = NULL;
  locate_symbol(function_ident, function_ident_len, &existing_entry);
  if (existing_entry) {
    if (types_compatible(existing_entry, function_entry, ARG1_SMV | ARG2_SMV) ==
        TCHK_INCOMPATIBLE) {
      fprintf(stderr, "ERROR: redeclaration of function: %s\n", function_ident);
      return -1;
    } else if (existing_entry->is_defined) {
      fprintf(stderr, "ERROR: redefinition of function: %s\n", function_ident);
      return -1;
    } else if (astree_count(function) == 3) {
      status = define_body(function, existing_entry);
      if (status) return status;
    }
    /* use existing type info */
    symbol_value_destroy(function_entry);
    return assign_type(function);
  } else {
    status = insert_symbol(function_ident, function_ident_len, function_entry);
    if (status) return status;
    if (astree_count(function) == 3) {
      status = define_body(function, function_entry);
      if (status) return status;
    } else {
      enter_scope(&params->symbol_table);
      finalize_scope(&params->symbol_table);
    }
    return assign_type(function);
  }
}

int validate_stmt(ASTree *statement) {
  int status;
  DEBUGS('t', "Validating next statement");
  switch (statement->symbol) {
    case TOK_RETURN:
      status = validate_return(statement);
      break;
    case TOK_TYPE_ID:
      status = make_object_entry(statement);
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
    default:
      /* parser will catch anything that we don't want, so at this point the
       * only thing left that this could be is an expression
       */
      status = validate_expr(statement);
      break;
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
      expression->attributes |= ATTR_EXPR_VREG;
      status = validate_call(expression);
      break;
    case TOK_INDEX:
      /* TODO(Robert): indexing */
      status = -1;
      break;
    case TOK_ARROW:
      /* TODO(Robert): pointers and struct member access */
      /* evaluate left but not right since right
       * is always an ident
       */
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
      case TOK_FUNCTION:
        status = make_function_entry(child);
        if (status != 0) return status;
        break;
      case TOK_TYPE_ID:
        status = make_object_entry(child);
        if (status != 0) return status;
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
