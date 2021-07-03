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
#include "string.h"
#include "symval.h"
#include "stdint.h"

#define ARG1_AST (1 << 0)
#define ARG1_SMV (1 << 1)
#define ARG1_TYPE (1 << 2)
#define ARG2_AST (1 << 3)
#define ARG2_SMV (1 << 4)
#define ARG2_TYPE (1 << 5)

#define IFLAG_INT (1 << 0)
#define IFLAG_CHAR (1 << 1)
#define IFLAG_SHORT (1 << 2)
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
static const size_t MAX_STRING_LENGTH = 31;

/* for traversing the syntax tree */
int validate_intcon(ASTree *intcon);
int validate_type_id(ASTree *tid);
int validate_call(ASTree *call);
int validate_block(ASTree *block);
int validate_expr(ASTree *statement);
int validate_stmt(ASTree *statement);
int validate_assignment(ASTree *assignment);
int validate_binop(ASTree *operator);
int validate_cast(ASTree *cast);
int validate_unop(ASTree *operator);
int validate_equality(ASTree *operator);
int validate_return(ASTree *loop);
int validate_ifelse(ASTree *loop);
int validate_while(ASTree *loop);

/* for making entries in the symbol table */
int make_object_entry(ASTree *object);
int make_function_entry(ASTree *function);
int make_structure_entry(ASTree *structure);
int make_union_entry(ASTree *onion);
int make_label_entry(ASTree *label);
int make_typedef_entry(ASTree *tipedef);

/*
 * wrapper functions for use with badlib
 */
static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_STRING_LENGTH);
  }
  return ret;
}

static void symbol_table_destroy(void *table) {
  /* cleanup symbol values */
  map_foreach_value(table, (void (*)(void *))symbol_value_destroy);
  /* destroy table, which frees symbol values */
  map_destroy(table);
}

/*
 * internal functions
 */
ASTree *extract_param(ASTree *function, size_t index) {
  return llist_get(astree_second(function)->children, index);
}

const char *extract_ident(ASTree *type_id) {
  return astree_second(type_id)->lexinfo;
}

const char *extract_type(ASTree *type_id) {
  return astree_first(type_id)->lexinfo;
}

/*
 * TODO(Robert): recursively setting the block number no longer works because
 * of nested scoping; instead block numbers should be set during the validation
 * of expressions, at which point it is not possible to further nest scopes.
 */

void insert_cast(ASTree *tree, size_t index, struct typespec *type) {
  /* since the linked list has a destructor defined for elements, we can't just
   * use llist_get and llist_delete here, since that will call astree_destroy
   * on the node. Instead we call extract, which removes the element from the
   * list and returns the value
   */
  ASTree *to_cast = llist_extract(tree->children, index);
  ASTree *cast = malloc(sizeof(*cast));
  astree_init(cast, TOK_CAST, to_cast->loc, "_cast");
  cast->type = *type;
  llist_insert(tree->children, astree_adopt(cast, to_cast, NULL, NULL), index);
}

int assign_type_id(ASTree *ident) {
  DEBUGS('t', "Attempting to assign a type");
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strnlen(id_str, MAX_STRING_LENGTH);
  SymbolValue *symval = NULL;
  int in_current_scope = locate_symbol(id_str, id_str_len, &symval);

  if (symval) {
    DEBUGS('t', "Assigning %s a symbol", id_str);
    ident->type = symval->type;
  } else {
    fprintf(stderr, "ERROR: could not resolve symbol: %s %s\n",
            (ident->lexinfo), parser_get_tname(ident->symbol));
    return -1;
  }
  return 0;
}

int type_is_arithmetic(struct typespec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED ||
          type->base == TYPE_FLOAT || type->base == TYPE_DOUBLE);
}

int type_is_integer(struct typespec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED);
}

int type_is_int_or_ptr(struct typespec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED ||
          type->base == TYPE_POINTER);
}

int type_is_comparable(TypeSpec *type) {
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
int types_compatible(void *arg1, void *arg2, unsigned int flags) {
  struct typespec *type1 = NULL;
  struct typespec *type2 = NULL;
  int action = CONV_COMPATIBLE;

  /* extract type from first argument */
  if (flags & ARG1_AST) {
    type1 = &(((ASTree *)arg1)->type);
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
    type2 = &(((ASTree *)arg2)->type);
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
    if ((type1->base == TYPE_UNSIGNED) && (type2->base == TYPE_SIGNED)) {
      ret = TCHK_IMPLICIT_CAST;
    }
    if (type1->width > type2->width) {
      ret = TCHK_IMPLICIT_CAST;
    }
    if ((type1->base == TYPE_SIGNED) && (type2->base == TYPE_UNSIGNED)) {
      ret = TCHK_EXPLICIT_CAST;
    }
    if (type1->width < type2->width) {
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
int determine_promotion(ASTree *arg1, ASTree *arg2, struct typespec *out) {
  int status = 0;
  const struct typespec *type1 = arg1 ? &(arg1->type) : NULL;
  const struct typespec *type2 = arg2 ? &(arg2->type) : &SPEC_INT;

  if (type1 == NULL) {
    fprintf(stderr, "First argument not provided to promotion routine\n");
    status = -1;
  } else if (type1->base == TYPE_POINTER || type2->base == TYPE_POINTER) {
    /* promote to pointer if either operand is one */
    out->base = TYPE_POINTER;
    out->width = X64_SIZEOF_LONG;
    out->width = X64_ALIGNOF_LONG;
  } else if (type1->width < X64_SIZEOF_INT && type2->width < X64_SIZEOF_INT) {
    /* promote to signed int if both operands could be represented as one */
    out->base = TYPE_SIGNED;
    out->width = X64_SIZEOF_INT;
    out->alignment = X64_ALIGNOF_INT;
  } else if (type1->width > type2->width) {
    /* promote to wider type; disregard signedness of type2 */
    out->base = type1->base;
    out->width = type1->width;
    out->alignment = type1->alignment;
  } else if (type1->width < type2->width) {
    /* promote to wider type; disregard signedness of type1 */
    out->base = type2->base;
    out->width = type2->width;
    out->alignment = type2->alignment;
  } else if (type1->base == TYPE_UNSIGNED || type2->base == TYPE_UNSIGNED) {
    /* promote to unsigned if either argument is */
    out->base = TYPE_UNSIGNED;
    out->width = type1->width;
    out->alignment = type1->alignment;
  } else if (type1->base == TYPE_SIGNED && type2->base == TYPE_SIGNED) {
    /* if both of them are signed then make it signed */
    out->base = TYPE_SIGNED;
    out->width = type1->width;
    out->alignment = type1->alignment;
  } else {
    fprintf(stderr, "ERORR: unable to determine promotion for types\n");
    status = -1;
  }

  return status;
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
      status = assign_type_id(expression);
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

int validate_intcon(ASTree *intcon) {
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
    if (unsigned_value == ULONG_MAX) {
      /* error: constant too large */
    } else {
      intcon->type.base = TYPE_UNSIGNED;
      intcon->type.width = X64_SIZEOF_LONG;
      intcon->type.alignment = X64_ALIGNOF_LONG;
    }
  } else if (signed_value > INT8_MIN && signed_value < INT8_MAX) {
    intcon->type.base = TYPE_SIGNED;
    intcon->type.width = X64_SIZEOF_CHAR;
    intcon->type.alignment = X64_ALIGNOF_CHAR;
  } else if (signed_value > INT16_MIN && signed_value < INT16_MAX) {
    intcon->type.base = TYPE_SIGNED;
    intcon->type.width = X64_SIZEOF_SHORT;
    intcon->type.alignment = X64_ALIGNOF_SHORT;
  } else if (signed_value > INT32_MIN && signed_value < INT32_MAX) {
    intcon->type.base = TYPE_SIGNED;
    intcon->type.width = X64_SIZEOF_INT;
    intcon->type.alignment = X64_ALIGNOF_INT;
  } else {
    intcon->type.base = TYPE_SIGNED;
    intcon->type.width = X64_SIZEOF_LONG;
    intcon->type.alignment = X64_ALIGNOF_LONG;
  }

  return status;
}

int validate_int_spec(ASTree *spec_list, TypeSpec *out) {
  int status = 0;
  int flags = 0;

  /* TODO(Robert): this is hideous and a roundabout way of figuring out the
   * the integer type; this should probably be handled by the parser in some
   * way
   */
  size_t i;
  for (i = 0; i < llist_size(spec_list->children); ++i) {
    ASTree *type = llist_get(spec_list->children, i);
    switch (type->symbol) {
      case TOK_INT:
        if (flags & (IFLAG_INT | IFLAG_CHAR)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else {
          flags &= IFLAG_INT;
        }
        break;
      case TOK_CHAR:
        if (flags & (IFLAG_CHAR | IFLAG_INT | IFLAG_SHORT | IFLAG_LONG)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else {
          flags &= IFLAG_CHAR;
          out->width = 1;
          out->alignment = 1;
        }
        break;
      case TOK_LONG:
        if (flags & (IFLAG_LONG | IFLAG_SHORT | IFLAG_CHAR)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else {
          flags &= IFLAG_LONG;
          out->width = 8;
          out->alignment = 8;
        }
        break;
      case TOK_SHORT:
        if (flags & (IFLAG_SHORT | IFLAG_LONG | IFLAG_CHAR)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else {
          flags &= IFLAG_SHORT;
          out->width = 2;
          out->alignment = 2;
        }
        break;
      case TOK_SIGNED:
        if (flags & (IFLAG_SIGNED | IFLAG_UNSIGNED)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else {
          flags &= IFLAG_SIGNED;
          out->base = TYPE_SIGNED;
        }
        break;
      case TOK_UNSIGNED:
        if (flags & (IFLAG_UNSIGNED | IFLAG_SIGNED)) {
          fprintf(stderr, "ERROR: bad occurrence of int type secifier %s\n",
                  parser_get_tname(type->symbol));
          status = -1;
        } else {
          flags &= IFLAG_UNSIGNED;
          out->base = TYPE_UNSIGNED;
        }
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

  if (!type_is_integer(out)) {
    /* set base type if that hasn't already been done */
    out->base = TYPE_SIGNED;
  }
  if (!(out->alignment || out->width)) {
    /* set width and alignment if necessary */
    out->alignment = X64_ALIGNOF_INT;
    out->width = X64_SIZEOF_INT;
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
  }
  return status;
}

int validate_type_id(ASTree *tid) {
  ASTree *type = astree_first(tid);
  ASTree *ident = astree_second(tid);
  int status = validate_type(type, &(ident->type));
  if (status) return status;
  SymbolValue *type_sym = NULL;
  locate_symbol(ident->lexinfo, strlen(ident->lexinfo), &type_sym);
  if (type_sym) {
    /* error: symbol already defined */
    fprintf(stderr, "ERROR: redefinition of symbol %s\n", ident->lexinfo);
    return -1;
  }
  return 0;
}

int validate_block(ASTree *block) {
  size_t i;
  /* create a new scope if it hasn't already been */
  int status = 0, needs_scope = !block->symbol_table;
  if(needs_scope)
      create_scope(&(block->symbol_table));
  for (i = 0; i < llist_size(block->children); ++i) {
    ASTree *statement = llist_get(block->children, i);
    status = validate_stmt(statement);
    if (status) break;
  }
  if(needs_scope)
      finalize_scope();
  return status;
}

int validate_call(ASTree *call) {
  const char *identifier = astree_first(call)->lexinfo;
  /* params are at the same level as the id */
  SymbolValue *function = NULL;
  locate_symbol(identifier, strlen(identifier), &function);

  if (function) {
    struct llist *params = function->type.data.params;
    /* subtract one since function identifier is also a child */
    if (llist_size(call->children) - 1 != llist_size(params)) {
      fprintf(stderr, "ERROR: incorrect number of arguments %s\n", identifier);
      return -1;
    }
    DEBUGS('t', "Validating arguments for call to %s, num call->children: %d",
           identifier, llist_size(call->children) - 1);
    size_t i;
    for (i = 0; i < llist_size(params); ++i) {
      DEBUGS('t', "Validating argument %d", i);
      /* add 1 to index to skip function identifier */
      ASTree *param = llist_get(call->children, i + 1);
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

    call->type = *(function->type.nested);
    astree_first(call)->type = function->type;
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
    insert_cast(assignment, 1, &(dest->type));
  } else if (!(astree_first(assignment)->attributes & ATTR_EXPR_LVAL)) {
    fprintf(stderr, "ERROR: Destination is not an LVAL\n");
    status = -1;
  }
  /* type is the type of the left operand */
  assignment->type = astree_second(assignment)->type;
  return status;
}

int validate_cast(ASTree *cast) {
  ASTree *to_cast = NULL;
  int status = 0;

  if (llist_size(cast->children) > 1) {
    ASTree *cast_spec = astree_first(cast);
    status = validate_type(cast_spec, &(cast->type));
    if (status) return status;

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
  ASTree *left = astree_first(operator);
  ASTree *right = astree_second(operator);
  int status = 0;

  status = validate_expr(left);
  if (status != 0) return status;
  status = validate_expr(right);
  if (status != 0) return status;

  TypeSpec promoted_type = SPEC_EMPTY;
  status = determine_promotion(left, right, &promoted_type);
  if (status != 0) return status;

  int compatibility =
      types_compatible(&promoted_type, left, ARG1_TYPE | ARG2_AST);
  if (compatibility == CONV_IMPLICIT_CAST) {
    insert_cast(operator, 0, &promoted_type);
  } else if (compatibility == CONV_INCOMPATIBLE ||
             compatibility == CONV_EXPLICIT_CAST) {
    /* should not happen but I am bad at programming so it would be worth
     * checking for
     */
    fprintf(stderr, "Promotion failed on left operand; aborting.");
    abort();
  }

  compatibility = types_compatible(&promoted_type, right, ARG1_TYPE | ARG2_AST);
  if (compatibility == CONV_IMPLICIT_CAST) {
    insert_cast(operator, 1, &promoted_type);
  } else if (compatibility == CONV_INCOMPATIBLE ||
             compatibility == CONV_EXPLICIT_CAST) {
    /* should not happen but I am bad at programming so it would be worth
     * checking for
     */
    fprintf(stderr, "Promotion failed on right operand; aborting.");
    abort();
  }

  if (is_logical_op(operator)) {
    if (!(type_is_comparable(&(left->type)) &&
          type_is_comparable(&(right->type)))) {
      fprintf(stderr,
              "ERROR: '%s' arguments were not comparable\n", operator->lexinfo);
      status = -1;
    } else {
      operator->type = SPEC_INT;
    }
  } else if (is_bitwise_op(operator) && !(type_is_int_or_ptr(&(left->type)) &&
                                          type_is_int_or_ptr(&(right->type)))) {
    fprintf(stderr,
            "ERROR: '%s' arguments must be of type int\n", operator->lexinfo);
    status = -1;
  } else {
    operator->type = promoted_type;
  }

  return status;
}

/* TODO(Robert): postfix increment/decrement and promotion operators should get
 * their own token codes to make assembly generation easier.
 */
int validate_unop(ASTree *operator) {
  int status = 0;
  ASTree *operand = astree_first(operator);
  status = validate_expr(operand);
  if (status != 0) return status;

  if (is_logical_op(operator)) {
    if (!type_is_comparable(&(operand->type))) {
      fprintf(stderr,
              "ERROR: '%s' argument must be comparable\n", operator->lexinfo);
      status = -1;
    } else {
      operator->type = SPEC_INT;
    }
  } else {
    if (is_bitwise_op(operator) && !type_is_int_or_ptr(&(operand->type))) {
      fprintf(stderr, "ERROR: '%s' argument must be an int or pointer\n",
                      operator->lexinfo);
      status = -1;
    } else if (operator->symbol ==
               '+' && !type_is_arithmetic(&(operand->type))) {
      fprintf(stderr, "ERROR: '%s' argument must be of arithmetic type\n",
                      operator->lexinfo);
      status = -1;
    } else {
      status = determine_promotion(operand, NULL, &(operator->type));
    }
  }

  return status;
}

int validate_equality(ASTree *operator) {
  ASTree *left = astree_first(operator);
  ASTree *right = astree_second(operator);

  int status = 0;
  status = validate_expr(left);
  if (status != 0) return status;
  status = validate_expr(right);
  if (status != 0) return status;

  operator->type = SPEC_INT;
  /* comparison of types is limited to the following:
   * 1. between arithmetic types
   * 2. between pointers, which can be qualified, unqualified, incomplete,
   *    and/or a pointer to void
   * 3. between pointers and NULL
   */
  if (type_is_arithmetic(&(left->type)) && type_is_arithmetic(&(right->type))) {
    /* determine promotions and insert casts where necessary */
    TypeSpec promoted_type = SPEC_EMPTY;
    status = determine_promotion(left, right, &promoted_type);
    if (status) return status;
    int compatibility =
        types_compatible(left, &promoted_type, ARG1_AST | ARG2_TYPE);
    if (compatibility == CONV_IMPLICIT_CAST) {
      insert_cast(operator, 0, &promoted_type);
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
      insert_cast(operator, 1, &promoted_type);
    } else if (compatibility == CONV_INCOMPATIBLE ||
               compatibility == CONV_EXPLICIT_CAST) {
      /* should not happen but I am bad at programming so it would be worth
       * checking for
       */
      fprintf(stderr, "Promotion failed on right operand; aborting.");
      abort();
    }
  } else if ((left->type.base == TYPE_POINTER) &&
             (right->type.base == TYPE_POINTER)) {
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
  if (llist_size(ret->children) > 0) {
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
  if (!type_is_integer(&(astree_first(ifelse)->type))) {
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
  finalize_scope();
  if (status) return status;
  if (llist_size(ifelse->children) == 3) {
    create_scope(&(astree_third(ifelse)->symbol_table));
    status = validate_stmt(astree_third(ifelse));
    finalize_scope();
  }
  return status;
}

int validate_while(ASTree *whole) {
  int status = validate_expr(astree_first(whole));
  if (status != 0) return status;
  if (!type_is_integer(&(astree_first(whole)->type))) {
    /* error: conditional expression must be of type int */
    status = -1;
    return status;
  }
  create_scope(&(astree_second(whole)->symbol_table));
  status = validate_stmt(astree_second(whole));
  finalize_scope();
  return status;
}

int make_structure_entry(ASTree *structure) {
  const char *structure_type = extract_type(structure);
  DEBUGS('t', "ree");
  size_t structure_type_len = strnlen(structure_type, MAX_STRING_LENGTH);
  DEBUGS('t', "Defining structure type: %s", structure_type);
  SymbolValue *structure_value = NULL;
  int status = 0;
  locate_symbol((char *)structure_type, structure_type_len, &structure_value);
  if (structure_value) {
    /* TODO(Robert): allow duplicates if definition matches */
    fprintf(stderr, "ERROR: Duplicate definition of structure %s\n",
            structure_type);
    return -1;
  } else {
    structure_value = symbol_value_init(&(astree_first(structure)->type),
            &(astree_first(structure)->loc));
    insert_symbol(structure_type, structure_type_len, structure_value);
    struct map *fields = malloc(sizeof(*fields));
    map_init(fields, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
    /* start from 2nd child; 1st was type name */
    size_t i;
    for (i = 1; i < llist_size(structure->children); ++i) {
      ASTree *field = llist_get(structure->children, i);
      const char *field_id_str = extract_ident(field);
      size_t field_id_str_len = strnlen(field_id_str, MAX_STRING_LENGTH);
      DEBUGS('t', "Found structure field: %s", field_id_str);
      SymbolValue *field_value =
          map_get(fields, (char *)field_id_str, field_id_str_len);
      if (field_value) {
        fprintf(stderr, "ERROR: Duplicate declaration of field: %s\n",
                field_id_str);
        return -1;
      } else {
        status = validate_type_id(field);
        if (status != 0) return status;
        field_value = symbol_value_init(&(astree_second(field)->type),
                          &(astree_second(field)->loc));
        map_insert(fields, (char *)field_id_str, field_id_str_len, field_value);
        DEBUGS('t', "Field inserted at %s", astree_second(field)->lexinfo);
      }
    }
    structure_value->type.data.members = fields;
  }
  return 0;
}

int nest_type(ASTree *ident, const TypeSpec *to_copy) {
  TypeSpec *inner_type = malloc(sizeof(*inner_type));
  *inner_type = ident->type;
  ident->type = *to_copy;
  ident->type.nested = inner_type;
  return 0;
}

/* TODO(Robert): some of this should be put into separate functions (parameters?) */
int make_function_entry(ASTree *function) {
  int status = 0;

  DEBUGS('t', "Making entry for node with symbol: %s",
         parser_get_tname(function->symbol));
  ASTree *type_id = astree_first(function);
  status = validate_type_id(type_id);
  if (status != 0) return status;
  if (type_id->type.base == TYPE_ARRAY) {
    fprintf(stderr, "ERROR: Function %s has an array return type.\n",
            extract_ident(type_id));
    /* TODO(rbergero): keep going if we can */
    return -1;
  }

  /* Currently, a new scope is created for function prototypes, which only holds
   * the parameters. This is fine; the standard even specifies a scope
   * specifically for function declarations
   */
  create_scope(&(function->symbol_table));
  ASTree *ident = astree_second(type_id);
  nest_type(ident, &SPEC_FUNCTION);
  ident->type.data.params = malloc(sizeof(struct llist));
  llist_init(ident->type.data.params, free, NULL);
  ASTree *params = astree_second(function);
  size_t i;
  for (i = 0; i < llist_size(params->children); ++i) {
    ASTree *param = llist_get(params->children, i);
    const char *param_id_str = extract_ident(param);
    /*
     * TODO(Robert): implement find function(s) in badlib and search for
     * duplicate parameters
     */
    ASTree *param_identifier = astree_second(param);
    DEBUGS('t', "Defining function parameter %s", extract_ident(param));
    status = make_object_entry(param);
    if (status != 0) return status;
    size_t param_id_str_len = strnlen(param_id_str, MAX_STRING_LENGTH);
    SymbolValue *param_entry = NULL;
    locate_symbol(param_id_str, param_id_str_len, &param_entry);
    if (param_entry == NULL) {
      fprintf(stderr,
              "ERROR: parameter symbol was not inserted into function table\n");
      return -1;
    }
    llist_push_back(ident->type.data.params, param_entry);
  }

  const char *function_id = extract_ident(type_id);
  size_t function_id_len = strnlen(function_id, MAX_STRING_LENGTH);
  SymbolValue *existing_entry = NULL;
  locate_symbol(function_id, function_id_len, &existing_entry);
  if (existing_entry) {
    if (types_compatible(existing_entry, ident, ARG1_SMV | ARG2_AST) ==
        TCHK_INCOMPATIBLE) {
      fprintf(stderr, "ERROR: redefinition of function: %s\n", function_id);
      return -1;
    } else if (existing_entry->is_defined) {
      fprintf(stderr, "ERROR: function has already been defined: %s\n",
              function_id);
      return -1;
    } else if (llist_size(function->children) == 3) {
      DEBUGS('t', "Completing entry for prototype %s", function_id);
      current_function = existing_entry;
      status = validate_stmt(astree_third(function));
      if (status) return status;
      current_function = NULL;
      existing_entry->is_defined = 1;
    }
  } else {
    /* create function symbol */
    SymbolValue *function_entry = symbol_value_init(&(ident->type), &(ident->loc));
    insert_symbol(function_id, function_id_len, function_entry);
    if (llist_size(function->children) == 3) {
      /* define function */
      DEBUGS('t', "No protoype; defining function %s", function_id);
      current_function = function_entry;
      status = validate_stmt(astree_third(function));
      if (status) return status;
      current_function = NULL;
      function_entry->is_defined = 1;
    }
  }

  finalize_scope();
  return 0;
}

int make_object_entry(ASTree *object) {
  const char *ident = extract_ident(object);
  size_t ident_len = strnlen(ident, MAX_STRING_LENGTH);
  SymbolValue *exists = NULL;
  int status = 0;

  if (locate_symbol(ident, ident_len, &exists)) {
    /* TODO(Robert): duplicate declarations are fine so long as the types match,
     * so we should check that first instead of erroring straight away.
     */
    fprintf(stderr, "ERROR: symbol has already been declared: %s\n",
            extract_ident(object));
    status = -1;
  } else {
    DEBUGS('t', "Making object entry for value %s", ident);
    ASTree *type = astree_first(object);
    ASTree *identifier = astree_second(object);
    identifier->attributes |= ATTR_EXPR_LVAL;
    status = validate_type_id(object);
    if (status != 0) return status;
    if (llist_size(object->children) == 3) {
      ASTree *init_value = astree_third(object);
      status = validate_expr(init_value);
      if (status) return status;
      TypeSpec promoted_type = SPEC_EMPTY;
      status = determine_promotion(identifier, init_value, &promoted_type);
      if (status) return status;
      int compatibility =
          types_compatible(identifier, init_value, ARG1_AST | ARG2_AST);
      if (compatibility == TCHK_INCOMPATIBLE ||
          compatibility == TCHK_EXPLICIT_CAST) {
        fprintf(stderr, "ERROR: Incompatible type for object variable\n");
        status = -1;
      } else if (compatibility == TCHK_IMPLICIT_CAST) {
        insert_cast(object, 2, &promoted_type);
      }
    }

    SymbolValue *symbol = symbol_value_init(&(astree_second(object)->type),
                               &(astree_second(object)->loc));
    size_t identifier_len = strnlen(identifier->lexinfo, MAX_STRING_LENGTH);
    insert_symbol(identifier->lexinfo, identifier_len, symbol);
    if (status) {
      fprintf(stderr, "your data structure library sucks.\n");
      abort();
    }
  }
  return status;
}

int make_typedef_entry(ASTree *tipedef) {
  ASTree *type = astree_first(tipedef);
  ASTree *alias = astree_second(tipedef);
  return 0;
}

/*
 * external functions
 */

int type_checker_make_table(ASTree *root) {
  DEBUGS('t', "Making symbol table");
  create_scope(&(root->symbol_table));
  size_t i;
  for (i = 0; i < llist_size(root->children); ++i) {
    ASTree *child = llist_get(root->children, i);
    int status;
    switch (child->symbol) {
      case TOK_FUNCTION:
        DEBUGS('t', "bing");
        status = make_function_entry(child);
        if (status != 0) return status;
        break;
      case TOK_TYPE_ID:
        DEBUGS('t', "boing");
        status = make_object_entry(child);
        if (status != 0) return status;
        break;
      default:
        fprintf(stderr, "ERROR: Unexpected symbol at top level: %s\n",
                parser_get_tname(child->symbol));
        return -1;
    }
  }
  finalize_scope();
  return 0;
}
