#include "typecheck.h"

#include "astree.h"
#include "attributes.h"
#include "simplestack.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "debug.h"
#include "err.h"
#include "lyutils.h"
#include "string.h"
#include "symtable.h"

struct map *type_names;
struct map *globals;
struct map *locals;
/* the 'stack' of tables */
struct llist *tables;
struct llist *string_constants;
DECLARE_STACK(symstack, SymbolValue);
DECLARE_STACK(nrstack, size_t);
/*
 * used to store function names; required to handle nested functions and to
 * verify that the type of a return statement matches that of the function it
 * is in
 */
struct symstack function_symbols;
/*
 * used to store sequence numbers of declarations in nested blocks, including
 * compound statements, functions, if, do, while, and for
 */
struct nrstack sequence_nrs;
/*
 * used to count the number of sub-blocks in a given block, or, when at global
 * scope, the number of function, union, and struct definitions/prototypes
 *
 * the 'size' member can be used to determine the depth of blocks currently
 * being counted
 */
struct nrstack block_nrs;
/*
 * Function_symbols will have a dummy value on top representing global scope.
 * This way, all three of the above stacks will always have at least one value
 * for the duration of type checking, and will always have the same number of
 * items on the stack.
 *
 * Maybe the three values should be combined into a single structure and stack,
 * to make managing them simpler.
 */
static const size_t MAX_STRING_LENGTH = 31;
static const size_t DEFAULT_MAP_SIZE = 100;
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
enum types_compatible_arg_flags {
  ARG1_AST = 1 << 0,
  ARG1_SMV = 1 << 1,
  ARG1_TYPE = 1 << 2,
  ARG2_AST = 1 << 3,
  ARG2_SMV = 1 << 4,
  ARG2_TYPE = 1 << 5
};

/*
 * forward declarations for some internal functions
 */

/* for traversing the syntax tree */
int validate_type_id(ASTree *tid);
int validate_call(ASTree *call);
int validate_block(ASTree *block);
int validate_expr(ASTree *statement);
int validate_stmt(ASTree *statement);
int validate_assignment(ASTree *assignment);
int validate_binop(ASTree *operator);
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

/*
 * wrapper functions for use with badlib
 */
static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = strncmp(s1, s2, MAX_STRING_LENGTH);
  }
  return ret;
}

static void symbol_value_destroy_wrapper(void *unused, void *symval,
                                         size_t unused2, size_t unused3) {
  symbol_value_destroy(symval);
}

static void symbol_table_destroy(void *table, size_t unused) {
  /* cleanup symbol values */
  map_foreach(table, symbol_value_destroy_wrapper);
  /* destroy table, which frees symbol values */
  map_destroy(table);
}

/*
 * TODO(Robert): recursively setting the block number no longer works because
 * of nested scoping; instead block numbers should be set during the validation
 * of expressions, at which point it is not possible to further nest scopes.
 */
void set_blocknr(ASTree *tree, size_t nr) {
  tree->loc.blocknr = nr;
  size_t i;
  for (i = 0; i < llist_size(tree->children); ++i) {
    set_blocknr(llist_get(tree->children, i), nr);
  }
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

void insert_cast(ASTree *tree, size_t index, struct typespec *type) {
  ASTree *to_cast = llist_get(tree->children, index);
  ASTree *cast = astree_init(TOK_CAST, to_cast->loc, "_cast");
  cast->type = *type;
  llist_insert(tree->children, astree_adopt(cast, to_cast, NULL, NULL),
               index);
}

int assign_type_id(ASTree *ident, SymbolValue *struct_member_id) {
  DEBUGS('t', "Attempting to assign a type");
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strnlen(id_str, MAX_STRING_LENGTH);
  SymbolValue *local_id = map_get(locals, (char *)id_str, id_str_len);
  SymbolValue *global_id = map_get(globals, (char *)id_str, id_str_len);

  if (struct_member_id) {
    DEBUGS('t', "Assigning %s a struct member value\n", id_str);
    ident->type = struct_member_id->type;
  } else if (local_id) {
    DEBUGS('t', "Assigning %s a local value\n", id_str);
    ident->type = local_id->type;
  } else if (global_id) {
    DEBUGS('t', "Assigning %s a global value", id_str);
    ident->type = global_id->type;
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
  if (type_is_int_or_ptr(type1) && type_is_int_or_ptr(type2)) {
    return TCHK_COMPATIBLE;
  } else if (type_is_arithmetic(type1) && type_is_arithmetic(type2)) {
    return TCHK_COMPATIBLE;
  } else {
    return TCHK_INCOMPATIBLE;
  }
}

int aggregate_typespec(ASTree *type, char *spec_list) {
    strcat(spec_list, type->lexinfo);
}

int construct_typespec(ASTree *type, ASTree *ident, size_t depth) {
  /* Only the left operand may have TOK_SIGNED or TOK_UNSIGNED.
   * If the right operand is int or char, the left operand cannot be type int or
   * char.
   * If the left operand is int or char, the right operand cannot be anything
   * except null, since int or char should be the last specifier present, if
   * they are present at all.
   * To ensure that detection of misplaced tokens works, if either of the
   * operands are TOK_CHAR or TOK_INT, the token of the return value will be
   * TOK_INT.
   */

  if (depth == 0) {
    /* first token in specifier list */
    switch (type->token) {
      case TOK_SHORT:
        ident->type.base = TYPE_SIGNED;
        ident->type.width = SIZEOF_SHORT;
        ident->type.alignment = SIZEOF_SHORT;
        break;
      case TOK_INT:
      case TOK_SIGNED:
        ident->type.base = TYPE_SIGNED;
        ident->type.width = SIZEOF_INT;
        ident->type.alignment = SIZEOF_INT;
        break;
      case TOK_CHAR:
        ident->type.base = TYPE_SIGNED;
        ident->type.width = SIZEOF_CHAR;
        ident->type.alignment = SIZEOF_CHAR;
        break;
      case TOK_LONG:
        ident->type.base = TYPE_SIGNED;
        ident->type.width = SIZEOF_LONG;
        ident->type.alignment = SIZEOF_LONG;
        break;
      case TOK_UNSIGNED:
        ident->type.base = TYPE_UNSIGNED;
        ident->type.width = SIZEOF_INT;
        ident->type.alignment = SIZEOF_INT;
        break;
      default:
        break;
    }
  } else if ((type->token == TOK_INT || type->token == TOK_CHAR) &&
             astree_first(type)) {
    /* error; int and char must be last token */
  } else if ((type->token == TOK_SIGNED || type->token == TOK_UNSIGNED)
        && depth > 0) {
    /* error: no tokens may come before signed/unsigned */
  } else if ((type->token == TOK_SHORT || type->token == TOK_LONG) &&
             (ident->type.width == 2 || ident->type.width == 8)) {
    /* error; multiple short or long tokens */
  } else if ((type->token == TOK_SHORT || type->token == TOK_LONG) &&
             (ident->type.width == 1)) {
    /* error; invalid type adjectives for char */
  } else {
    /* valid combination of type specifiers */
    switch (spec2->token) {
      case TOK_SHORT:
        ident->type.width = SIZEOF_SHORT;
        ident->type.alignment = SIZEOF_SHORT;
        break;
      case TOK_LONG:
        ident->type.width = SIZEOF_LONG;
        ident->type.alignment = SIZEOF_LONG;
        break;
      case TOK_CHAR:
        ident->type.width = SIZEOF_CHAR;
        ident->type.alignment = SIZEOF_CHAR;
        break;
      default:
        ident->type.width = spec1->type.width;
        ident->type.alignment = spec1->type.alignment;
    }
  }

  return spec2;
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
  int action = CONV_COMPATIBLE;
  struct typespec *type1 = &(arg1->type);
  struct typespec *type2 = &(arg2->type);
  int args_arithmetic = type_is_arithmetic(type1) && type_is_arithmetic(type2);
  int args_int_or_ptr =
      (type_is_integer(type1) || type1->base == TYPE_POINTER) &&
      (type_is_integer(type2) || type2->base == TYPE_POINTER);

  /* determine width of out */
  if (type1->width > type2->width) {
    out->width = type1->width;
    out->alignment = type1->alignment;
  } else {
    out->width = type2->width;
    out->alignment = type2->alignment;
  }

  /* determine type of out; pointer is above all */
  if (type1->base == TYPE_POINTER || type2->base == TYPE_POINTER) {
    out->base = TYPE_POINTER;
  } else if ((type1->base == TYPE_UNSIGNED && type1->width == out->width) ||
             (type2->base == TYPE_UNSIGNED && type2->width == out->width)) {
    out->base = TYPE_UNSIGNED;
  } else {
    out->base = TYPE_SIGNED;
  }

  /* promote to signed int if the value can be represented by it */
  if (out->width < 4) {
    out->width = 4;
    out->alignment = 4;
    out->base = TYPE_SIGNED;
    action = TCHK_PROMOTE_BOTH;
  }

  return action;
}

int locate_symbol (const char* ident, size_t ident_len, SymbolValue** out) {
  size_t i;
  for (i = 0; i < llist_size(tables); ++i) {
    struct map *current_table = llist_get(tables, i);
    *out = map_get(current_table, (char*) ident, ident_len);
    if(*out) break;
  }
  /* true if the top of the stack (current scope) contains the symbol */
  return !i;
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
      fprintf(stderr, "ERROR: UNEXPECTED TOKEN IN STATEMENT: %s\n",
              statement->lexinfo);
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
    case TOK_LE:
    case TOK_GE:
    case TOK_GT:
    case TOK_LT:
    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
    /* case '|':
     * case '^':
     * case '&':
     * case TOK_SHL:
     * case TOK_SHR: */
      status = validate_binop(expression);
      break;
    case '!':
    case TOK_POS: /* promotion operator */
    case TOK_NEG:
    /* case '~': */
      status = validate_unop(expression);
      break;
    case TOK_CALL:
      expression->attributes |= ATTR_VREG;
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
    case TOK_NULLPTR:
    case TOK_CHARCON:
      /* types are set on construction and we don't need to do
       * anything else
       */
      break;
    case TOK_STRINGCON:
      /* save for intlang */
      llist_push_back(string_constants, (char *)expression->lexinfo);
      break;
    case TOK_IDENT:
      DEBUGS('t', "bonk");
      status = assign_type_id(expression, NULL);
      break;
    default:
      fprintf(stderr, "ERROR: UNEXPECTED TOKEN IN EXPRESSION: %s\n",
              expression->lexinfo);
      status = -1;
  }
  return status;
}

int construct_int_type(ASTree *type, ASTree *identifier) {

}

int validate_type_id(ASTree *tid) {
  /* TODO(Robert): arrays, structures, pointers, multiple integer widths */
  ASTree *type = astree_first(tid);
  ASTree *identifier = astree_second(tid);
  ASTree *type_node = NULL;
  SymbolValue *type_value = NULL;
  switch (type->symbol) {
    case TOK_INT:
      DEBUGS('t', "Setting attribute INT");
      identifier->type.base = TYPE_SIGNED;
      break;
    case TOK_VOID:
      DEBUGS('t', "Setting attribute VOID");
      if (identifier->type.base == TYPE_FUNCTION) {
        fprintf(stderr,
                "ERROR: variables and fields cannot be of type "
                "void!\n");
        return -1;
      } else {
        identifier->type.base = TYPE_VOID;
      }
      break;
    default:
      fprintf(stderr, "ERROR: unhandled type\n");
      return -1;
      break;
  }
  return 0;
}

int validate_block(ASTree *block) {
  size_t i;
  int status = 0;
  for (i = 0; i < llist_size(block->children); ++i) {
    ASTree *statement = llist_get(block->children, i);
    status = validate_stmt(statement);
    if (status) break;
  }
  return status;
}

int validate_call(ASTree *call) {
  const char *identifier = astree_first(call)->lexinfo;
  /* params are at the same level as the id */
  SymbolValue *function = map_get(globals, (char *)identifier,
                                  strnlen(identifier, MAX_STRING_LENGTH));
  if (function) {
    if (llist_size(call->children) - 1 != llist_size(function->type.data)) {
      fprintf(stderr, "ERROR: incorrect number of arguments %s\n", identifier);
      return -1;
    }
    DEBUGS('t', "Validating arguments for call to %s, num call->children: %d",
           identifier, llist_size(call->children) - 1);
    size_t i;
    for (i = 0; i < llist_size(function->type.data); ++i) {
      DEBUGS('t', "Validating argument %d", i);
      ASTree *param = llist_get(call->children, i + 1);
      int status = validate_expr(param);

      if (status != 0) return status;
      DEBUGS('t', "Comparing types");
      if (types_compatible(param, llist_get(function->type.data, i),
                           ARG1_AST & ARG2_SMV) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: incompatible type for argument: %s\n",
                param->lexinfo);
        return -1;
      }
    }

    call->type = function->type;
    astree_first(call)->type = function->type;
    return 0;
  } else {
    fprintf(stderr, "ERROR: Invalid call to function %s\n", identifier);
    return -1;
  }
}

int validate_assignment(ASTree *assignment) {
  int status = 0;
  status = validate_expr(astree_first(assignment));
  if (status != 0) return status;
  status = validate_expr(astree_second(assignment));
  if (status != 0) return status;
  if (types_compatible(astree_first(assignment), astree_second(assignment),
                       ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
    fprintf(stderr, "ERROR: Incompatible types for tokens: %s,%s %s,%s\n",
            parser_get_tname(astree_first(assignment)->symbol),
            astree_first(assignment)->lexinfo,
            parser_get_tname(astree_second(assignment)->symbol),
            astree_second(assignment)->lexinfo);
    status = -2;
  } else if (!(astree_first(assignment)->attributes & ATTR_LVAL)) {
    fprintf(stderr, "ERROR: Destination is not an LVAL\n");
    status = -1;
  }
  /* type is the type of the left operand */
  assignment->type = astree_second(assignment)->type;
  return status;
}

int validate_binop(ASTree *operator) {
  ASTree *left = astree_first(operator);
  ASTree *right = astree_second(operator);
  int status = 0;

  status = validate_expr(left);
  if (status != 0) return status;
  status = validate_expr(right);
  if (status != 0) return status;

  struct typespec promoted_type = {0};
  status = determine_promotion(astree_first(operator), astree_second(operator),
                               &promoted_type);
  if (status != 0) return status;

  status = types_compatible(left, &promoted_type, ARG1_AST & ARG2_TYPE);
  if (status == CONV_IMPLICIT_CAST) {
    insert_cast(operator, 0, &promoted_type);
  } else if (status == CONV_INCOMPATIBLE || status == CONV_EXPLICIT_CAST) {
    /* should not happen but I am bad at programming so it would be worth
     * checking for
     */
  }

  status = types_compatible(right, &promoted_type, ARG1_AST & ARG2_TYPE);
  if (status == CONV_IMPLICIT_CAST) {
    insert_cast(operator, 1, &promoted_type);
  } else if (status == CONV_INCOMPATIBLE || status == CONV_EXPLICIT_CAST) {
    /* should not happen but I am bad at programming so it would be worth
     * checking for
     */
  }

  return status;
}

int validate_unop(ASTree *operator) {
  int status = 0;
  status = validate_expr(astree_first(operator));
  if (status != 0) return status;
  if (astree_first(operator)->attributes != TYPE_SIGNED) {
    fprintf(stderr,
            "ERROR: '%s' argument must be of type int\n", operator->lexinfo);
    status = -1;
  }
  return status;
}

int validate_equality(ASTree *operator) {
  ASTree *dest = astree_first(operator);
  ASTree *src = astree_second(operator);
  /* types can be arbitrary here */
  int status = 0;
  status = validate_expr(dest);
  if (status != 0) return status;
  status = validate_expr(src);
  if (status != 0) return status;
  if (types_compatible(dest, src, ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
    fprintf(stderr,
            "ERROR: Incompatible types for operator: %i\n", operator->symbol);
    status = -1;
  }
  return status;
}

int validate_return(ASTree *ret) {
  int status = 0;
  SymbolValue *function = NULL;
  if (llist_size(ret->children) <= 0) {
    if (function->type.base == TYPE_FUNCTION) {
      /*
      fprintf(stderr,
              "ERROR: Return statement with value in void "
              "function: %s\n",
              function_name);
      */
      status = -1;
    }
  } else {
    validate_expr(astree_first(ret));
    status = types_compatible(astree_first(ret), function, ARG1_AST & ARG2_SMV);
    if (status == TCHK_INCOMPATIBLE) {
      fprintf(stderr, "ERROR: Incompatible return type\n");
      status = -1;
    } else {
      ret->attributes |= ATTR_VREG;
      ret->attributes |= astree_first(ret)->attributes;
    }
  }
  return status;
}

int validate_ifelse(ASTree *ifelse) {
  int status = validate_expr(astree_first(ifelse));
  if (status) return status;
  status = validate_stmt(astree_second(ifelse));
  if (status) return status;
  if (llist_size(ifelse->children) == 3)
    status = validate_stmt(astree_third(ifelse));
  if (status) return status;
  return status;
}

int validate_while(ASTree *whole) {
  int status = validate_expr(astree_first(whole));
  if (status != 0) return status;
  status = validate_stmt(astree_second(whole));
  if (status != 0) return status;
  return status;
}

int make_structure_entry(ASTree *structure) {
  const char *structure_type = extract_type(structure);
  DEBUGS('t', "ree");
  size_t structure_type_len = strnlen(structure_type, MAX_STRING_LENGTH);
  size_t dummy_sequence = 0;
  DEBUGS('t', "Defining structure type: %s", structure_type);
  SymbolValue *structure_value =
      map_get(type_names, (char *)structure_type, structure_type_len);
  if (structure_value) {
    fprintf(stderr, "ERROR: Duplicate definition of structure %s\n",
            structure_type);
    return -1;
  } else {
    structure_value = malloc(sizeof(*structure_value));
    symbol_value_init(structure_value, astree_first(structure), 0, 0);
    struct map *fields = malloc(sizeof(*fields));
    map_init(fields, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
    map_insert(type_names, (char *)structure_type, structure_type_len,
               structure_value);
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
        int status = validate_type_id(field);
        if (status != 0) return status;
        astree_second(field)->attributes |= ATTR_LVAL;
        field_value = malloc(sizeof(*field_value));
        symbol_value_init(field_value, astree_second(field), i - 1, 0);
        map_insert(fields, (char *)field_id_str, field_id_str_len, field_value);
        DEBUGS('t', "Field inserted at %s", astree_second(field)->lexinfo);
      }
    }
    structure_value->type.data = fields;
  }
  return 0;
}

int make_function_entry(ASTree *function) {
  int ret = 0;
  size_t dummy_sequence = 0;
  DEBUGS('t', "Making entry for node with symbol: %s",
         parser_get_tname(function->symbol));
  ASTree *type_id = astree_first(function);
  DEBUGS('t', "oof");
  astree_second(type_id)->type.base = TYPE_FUNCTION;
  DEBUGS('t', "oof");
  int status = validate_type_id(type_id);
  if (status != 0) return status;
  if (type_id->type.base == TYPE_ARRAY) {
    fprintf(stderr, "ERROR: Function %s has an array return type.\n",
            extract_ident(type_id));
    /* TODO(rbergero): keep going if we can */
    return -1;
  } else if (type_id->type.base == TYPE_STRUCT) {
    const char *structure_type = extract_type(type_id);
    size_t structure_type_len = strnlen(structure_type, MAX_STRING_LENGTH);
    SymbolValue *structure_value =
        map_get(type_names, (char *)structure_type, structure_type_len);
    if (!structure_value) {
      fprintf(stderr, "ERROR: Structrue has no definition: %s\n",
              structure_type);
      return -1;
    }
  }
  DEBUGS('t', "oof");

  SymbolValue *function_entry = malloc(sizeof(*function_entry));
  symbol_value_init(function_entry, astree_second(type_id), 0, 0);
  locals = malloc(sizeof(*locals));
  status = map_init(locals, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
  if (status) {
    fprintf(stderr, "fuck you\n");
    abort();
  }
  /* check and add parameters; set block */
  /* set_blocknr(astree_second(function)); */
  ASTree *params = astree_second(function);
  size_t param_sequence_nr = 0;
  size_t i;
  for (i = 0; i < llist_size(params->children); ++i) {
    ASTree *param = llist_get(params->children, i);
    const char *param_id_str = extract_ident(param);
    /*
     * TODO(Robert): implement find function(s) in badlib and search for duplicate
     * parameters
     */
    ASTree *param_identifier = astree_second(param);
    DEBUGS('t', "Defining function parameter %s", extract_ident(param));
    status = make_object_entry(param);
    if (status != 0) return status;
    size_t param_id_str_len = strnlen(param_id_str, MAX_STRING_LENGTH);
    SymbolValue *param_entry =
        map_get(locals, (char *)param_id_str, param_id_str_len);
    llist_push_back(function_entry->type.data, param_entry);
  }

  DEBUGS('t', "Inserting function entry with block id %u", nrstack_top(&block_nrs));
  const char *function_id = extract_ident(type_id);
  size_t function_id_len = strnlen(function_id, MAX_STRING_LENGTH);
  SymbolValue *prototype =
      map_get(globals, (char *)function_id, function_id_len);
  if (prototype) {
    if (types_compatible(prototype, function_entry, ARG1_SMV & ARG2_SMV) ==
        TCHK_INCOMPATIBLE) {
      fprintf(stderr, "ERROR: redefinition of function: %s\n", function_id);
      return -1;
    } else if (prototype->has_block) {
      fprintf(stderr, "ERROR: function has already been defined: %s\n",
              function_id);
      return -1;
    } else if (llist_size(function->children) == 3) {
      DEBUGS('t', "Completing entry for prototype %s", function_id);
      size_t new_sequence_nr = 0;
      status = validate_block(astree_third(function));
      if (status) return status;
      prototype->has_block = 1;
    }
  } else {
    map_insert(globals, (char *)function_id, function_id_len, function_entry);
    if (llist_size(function->children) == 3) {
      DEBUGS('t', "No protoype; defining function %s", function_id);
      size_t new_sequence_nr = 0;
      /* set_blocknr(astree_third(function), next_block); */
      status = validate_block(astree_third(function));
      if (status != 0) return status;
      function_entry->has_block = 1;
    }
  }

  llist_push_back(tables, locals);
  locals = NULL;
  nrstack_replace(&block_nrs, nrstack_top(&block_nrs) + 1);
  return 0;
}

int make_object_entry(ASTree *object) {
  const char *ident = extract_ident(object);
  size_t ident_len = strnlen(ident, MAX_STRING_LENGTH);
  SymbolValue *exists = NULL;
  int status = 0;

  if (locate_symbol (ident, ident_len, &exists)) {
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
    identifier->attributes |= ATTR_LVAL;
    status = validate_type_id(object);
    if (status != 0) return status;
    if (llist_size(object->children) == 3) {
      size_t dummy_sequence = 0;
      status = validate_expr(astree_third(object));
      if (status) return status;
      if (types_compatible(astree_third(object), astree_second(object),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible type for object variable\n");
        status = -1;
      }
    }

    SymbolValue *symbol = malloc(sizeof(*symbol));
    symbol_value_init(symbol, astree_second(object), nrstack_top(&sequence_nrs),
                      nrstack_top(&block_nrs));
    size_t identifier_len = strnlen(identifier->lexinfo, MAX_STRING_LENGTH);
    map_insert(llist_front(tables), (char *)identifier->lexinfo, identifier_len,
               symbol);
    nrstack_replace(&block_nrs, nrstack_top(&block_nrs) + 1);
  }
  return status;
}

/*
 * external functions
 */
void type_checker_init_globals() {
  /*
   * structure cleanup is done by foreach functions, while freeing of the
   * structures themselves is handled by the destructor of their container
   */

  /* linked lists */
  string_constants = malloc(sizeof(*string_constants));
  llist_init(string_constants, NULL, NULL);
  tables = malloc(sizeof(*tables));
  llist_init(tables, free, NULL);
  /* maps */
  struct map *file_scope = malloc(sizeof(*file_scope));
  map_init(file_scope, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
  llist_push_front(tables, file_scope);

  globals = malloc(sizeof(*globals));
  map_init(globals, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
  type_names = malloc(sizeof(*type_names));
  map_init(type_names, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
}

void type_checker_free_globals() {
  DEBUGS('t', "DESTROYING SYMTABLES");
  /* recursive cleanup of tables */
  llist_foreach(tables, symbol_table_destroy);
  llist_destroy(tables);
  DEBUGS('t', "  SYMTABLES DESTROYED");
}

int type_checker_make_table(ASTree *root) {
  DEBUGS('t', "Making symbol table");
  size_t i;
  for (i = 0; i < llist_size(root->children); ++i) {
    ASTree *child = llist_get(root->children, i);
    int status;
    size_t dummy_sequence = 0;
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
      case '=':
        DEBUGS('t', "bonk");
        status = make_object_entry(astree_first(child));
        if (status != 0) return status;
        status = validate_expr(astree_second(child));
        if (status != 0) return status;
        if (types_compatible(astree_first(child), astree_second(child),
                             ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
          fprintf(stderr, "ERROR: Incompatible types for global\n");
          return -1;
        } else if (!(astree_first(child)->attributes & ATTR_LVAL)) {
          fprintf(stderr,
                  "ERROR: Global assignment destination is not an "
                  "LVAL\n");
          return -1;
        }
        /* type is the type of the right operand */
        child->type = astree_second(child)->type;
        if (status != 0) return status;
        break;
      default:
        fprintf(stderr, "ERROR: Unexpected symbol at top level: %s\n",
                parser_get_tname(child->symbol));
        return -1;
    }
  }
  /* use local table list to group all tables together */
  llist_push_back(tables, globals);
  llist_push_back(tables, type_names);
  return 0;
}

void type_checker_dump_symbols(FILE *out) {
  /*
  size_t current_blocknr = 0;
  DEBUGS ('s', "Dumping structure types");
  for (khiter_t k = kh_begin (type_names); k != kh_end (type_names); ++k) {
      if (kh_exist (type_names, k)) {
          SymbolValue *structure = kh_val (type_names, k);
          fprintf (out, "%s ", kh_key (type_names, k));
          print_symbol_value (out, structure);
          fprintf (out, "\n");
          for (khiter_t k2 = kh_begin (structure->fields);
               k2 != kh_end (structure->fields);
               ++k2) {
              if (kh_exist (structure->fields, k2)) {
                  fprintf (out, "%s ", kh_key (structure->fields, k));
                  print_symbol_value (out, kh_val (structure->fields, k));
                  fprintf (out, "\n");
              }
          }
      }
  }

  DEBUGS ('s', "Dumping global declarations");
  for (khiter_t k = kh_begin (globals); k != kh_end (globals); ++k) {
      SymbolValue *top = kh_val (globals, k);
      fprintf (out, "%s ", kh_key (globals, k));
      DEBUGS ('s', "Writing global value %s", kh_key (globals, k));
      print_symbol_value (out, top);
      fprintf (out, "\n");
      if (top->attributes[ATTR_FUNCTION]) {
          DEBUGS ('s', "Dumping local declarations");
          locals = kv_A (tables, current_blocknr);
          for (khiter_t local_k = kh_begin (locals);
               local_k != kh_end (locals);
               ++local_k) {
              DEBUGS ('s', "Writing a local value");
              SymbolValue *local = kh_val (locals, local_k);
              fprintf (out, "    %s ", kh_key (locals, local_k));
              print_symbol_value (out, local);
              fprintf (out, "\n");
          }
          DEBUGS ('t', "All local values written");
          ++current_blocknr;
          fprintf (out, "\n");
      }
  }
  */
}
