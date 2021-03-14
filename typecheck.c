#include "typecheck.h"

#include "astree.h"
#include "attributes.h"
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
struct llist *tables;
struct llist *string_constants;
int next_block = 1;
const char *DUMMY_FUNCTION = "__DUMMY__";
static const size_t MAX_STRING_LENGTH = 31;
static const size_t DEFAULT_MAP_SIZE = 100;
enum type_checker_status {
  TCHK_COMPATIBLE,
  TCHK_IMPLICIT_CAST,
  TCHK_EXPLICIT_CAST,
  TCHK_INCOMPATIBLE,
  TCHK_E_NO_FLAGS
};
enum types_compatible_arg_flags {
  ARG1_AST = 1 << 0,
  ARG1_SMV = 1 << 1,
  ARG2_AST = 1 << 2,
  ARG2_SMV = 1 << 3
};

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

void set_blocknr(ASTree *tree, size_t nr) {
  tree->loc.blocknr = nr;
  for (size_t i = 0; i < llist_size(tree->children); ++i) {
    set_blocknr(llist_get(tree->children, i), nr);
  }
}

ASTree *extract_param(ASTree *function, size_t index) {
  return llist_get(astree_second(function)->children, index);
}

const char *extract_ident(ASTree *type_id) {
  return astree_second(type_id)->lexinfo;
}

const char *extract_type(ASTree *type_id) {
  return astree_first(type_id)->lexinfo;
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

int types_compatible(void *arg1, void *arg2, unsigned int flags) {
  struct typespec *type1 = NULL;
  struct typespec *type2 = NULL;

  if (flags & ARG1_AST) {
    type1 = &(((ASTree *)arg1)->type);
  } else if (flags & ARG1_SMV) {
    type1 = &(((SymbolValue *)arg1)->type);
  } else {
    fprintf(stderr, "ERROR: no flags provided for argument 1\n");
    return TCHK_E_NO_FLAGS;
  }
  if (flags & ARG2_AST) {
    type2 = &(((ASTree *)arg2)->type);
  } else if (flags & ARG2_SMV) {
    type2 = &(((SymbolValue *)arg2)->type);
  } else {
    fprintf(stderr, "ERROR: no flags provided for argument 2\n");
    return TCHK_E_NO_FLAGS;
  }

  /* TODO(Robert): check for arrays, pointers, structs, unions, and valid
   * conversions between types
   */
  if (type1->base == type2->base) {
    return TCHK_COMPATIBLE;
  } else {
    return TCHK_INCOMPATIBLE;
  }
};

int validate_type_id(ASTree *type, ASTree *identifier);
int validate_call(ASTree *call);
int validate_block(ASTree *block, const char *function_name,
                   size_t *sequence_nr);
int validate_stmt_expr(ASTree *statement, const char *function_name,
                       size_t *sequence_nr);
int make_local_entry(ASTree *local, size_t *sequence_nr);
int make_global_entry(ASTree *global);
int make_function_entry(ASTree *function);
int make_structure_entry(ASTree *structure);

int validate_type_id(ASTree *type, ASTree *identifier) {
  DEBUGS('t', "Validating typeid of symbol %s", identifier->lexinfo);
  if (type->symbol == TOK_ARRAY) {
    DEBUGS('t', "Setting attribute ARRAY");
    identifier->attributes |= ATTR_ARRAY;
    type = astree_first(type);
  }
  ASTree *type_node = NULL;
  SymbolValue *type_value = NULL;
  switch (type->symbol) {
    case TOK_INT:
      DEBUGS('t', "Setting attribute INT");
      identifier->type.base = TYPE_INT;
      break;
    case TOK_STRING:
      DEBUGS('t', "Setting attribute STRING");
      identifier->type.base = TYPE_STRING;
      break;
    case TOK_PTR:
      DEBUGS('t', "Setting attribute STRUCT");
      type_node = astree_first(type);
      type_value = map_get(type_names, (char *)type_node->lexinfo,
                           strnlen(type_node->lexinfo, MAX_STRING_LENGTH));
      if (type_value) {
        type_node->type = type_value->type;
      } else {
        fprintf(stderr, "ERROR: Type not declared: %s\n", type_node->lexinfo);
        return -1;
      }
      break;
    case TOK_VOID:
      DEBUGS('t', "Setting attribute VOID");
      if (identifier->attributes & ATTR_ARRAY) {
        fprintf(stderr, "ERROR: you may not have arrays of type void!\n");
        return -1;
      } else if (identifier->type.base == TYPE_FUNCTION) {
        fprintf(stderr,
                "ERROR: variables and fields cannot be of type "
                "void!\n");
        return -1;
      } else {
        identifier->type.base = TYPE_VOID;
      }
      break;
    case TOK_IDENT:
      DEBUGS('t', "Type is pointer; resolving");
      type_value = map_get(type_names, (char *)type->lexinfo,
                           strnlen(type->lexinfo, MAX_STRING_LENGTH));
      if (type_value) {
        identifier->type = type_value->type;
      } else {
        fprintf(stderr, "ERROR: type not declared: %s\n",
                identifier->type.identifier);
        return -1;
      }
      break;
    default:
      fprintf(stderr, "ERROR: unhandled type\n");
      return -1;
      break;
  }
  return 0;
}

int validate_block(ASTree *block, const char *function_name,
                   size_t *sequence_nr) {
  for (size_t i = 0; i < llist_size(block->children); ++i) {
    ASTree *statement = llist_get(block->children, i);
    int status = validate_stmt_expr(statement, function_name, sequence_nr);
    if (status != 0) return status;
  }
  return 0;
}

int validate_stmt(ASTree *statement, const char *function_name,
                  size_t *sequence_nr) {
  int status;
  const char *ident;
  SymbolValue *function =
      function_name == DUMMY_FUNCTION
          ? NULL
          : map_get(globals, (char *)function_name,
                    strnlen(function_name, MAX_STRING_LENGTH));
  DEBUGS('t', "Validating next statement");
  switch (statement->symbol) {
    case TOK_RETURN:
      if (llist_size(statement->children) <= 0) {
        if (function->type.base == TYPE_FUNCTION) {
          fprintf(stderr,
                  "ERROR: Return statement with value in void "
                  "function: %s\n",
                  function_name);
          return -1;
        }
      } else {
        validate_stmt_expr(astree_first(statement), function_name, sequence_nr);
        status = types_compatible(astree_first(statement), function,
                                  ARG1_AST & ARG2_SMV);
        if (status == TCHK_INCOMPATIBLE) {
          fprintf(stderr, "ERROR: Incompatible return type\n");
          return status;
        } else {
          statement->attributes |= ATTR_VREG;
          statement->attributes |= astree_first(statement)->attributes;
        }
      }
      break;
    case TOK_TYPE_ID:
      status = make_local_entry(statement, sequence_nr);
      if (status != 0) return status;
      break;
    case TOK_IF:
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (llist_size(statement->children) == 3)
        status = validate_stmt_expr(astree_third(statement), function_name,
                                    sequence_nr);
      if (status != 0) return status;
      break;
    case TOK_WHILE:
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      break;
    case TOK_BLOCK:
      status = validate_block(statement, function_name, sequence_nr);
      if (status != 0) return status;
      break;
  }
}

int validate_expr(ASTree *expression, const char *function_name,
                  size_t *sequence_nr) {
  int status;
  const char *ident;
  SymbolValue *function =
      function_name == DUMMY_FUNCTION
          ? NULL
          : map_get(globals, (char *)function_name,
                    strnlen(function_name, MAX_STRING_LENGTH));

  DEBUGS('t', "Validating next expression");
  switch (expression->symbol) {
    case '=':
      status = validate_stmt_expr(astree_first(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (types_compatible(astree_first(expression), astree_second(expression),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible types for tokens: %s,%s %s,%s\n",
                parser_get_tname(astree_first(expression)->symbol),
                astree_first(expression)->lexinfo,
                parser_get_tname(astree_second(expression)->symbol),
                astree_second(expression)->lexinfo);
        return -1;
      } else if (!(astree_first(expression)->attributes & ATTR_LVAL)) {
        fprintf(stderr, "ERROR: Destination is not an LVAL\n");
        return -1;
      }
      // type is the type of the left operand
      expression->type = astree_second(expression)->type;
      break;
    // here begins the trees made by the "expr" production
    case TOK_EQ:
    case TOK_NE:
      // types can be arbitrary here
      status = validate_stmt_expr(astree_first(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (types_compatible(astree_first(expression), astree_second(expression),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible types for operator: %s\n",
                expression->symbol);
        return -1;
      }
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
      // handle int exprs
      status = validate_stmt_expr(astree_first(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (types_compatible(astree_first(expression), astree_second(expression),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible types for operator: %s\n",
                expression->symbol);
        return -1;
      } else if (astree_first(expression)->type.base != TYPE_INT) {
        fprintf(stderr, "ERROR: Operator %s must have operands of type int\n",
                parser_get_tname(expression->symbol));
        fprintf(stderr, "Offending operands: %s %s\n",
                astree_first(expression)->lexinfo,
                astree_second(expression)->lexinfo);
        return -1;
      }
      break;
    case TOK_NOT:
    case TOK_POS:
    case TOK_NEG:
      // unary operators
      status = validate_stmt_expr(astree_first(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (astree_first(expression)->attributes != TYPE_INT) {
        fprintf(stderr, "ERROR: '%s' argument must be of type int\n",
                expression->lexinfo);
        return -1;
      }
      break;
    case TOK_ALLOC:
      status = validate_type_id(astree_first(expression), expression);
      if (status != 0) return status;
      if (llist_size(expression->children) == 2) {
        status = validate_stmt_expr(astree_second(expression), function_name,
                                    sequence_nr);
        if (status != 0) return status;
        if (astree_second(expression)->attributes != TYPE_INT ||
            (astree_second(expression)->attributes & ATTR_ARRAY)) {
          fprintf(stderr, "ERROR: alloc size argument must be of type int!");
          return -1;
        }
      }
      break;
    case TOK_CALL:
      expression->attributes |= ATTR_VREG;
      status = validate_call(expression);
      if (status != 0) return status;
      break;
    case TOK_INDEX:
      // evaluate left and right side
      // make sure left is an array and right is an int
      // set type to type of array, minus the array
      status = validate_stmt_expr(astree_first(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (astree_second(expression)->attributes == TYPE_INT) {
        if (astree_first(expression)->attributes & ATTR_ARRAY) {
          expression->type = astree_first(expression)->type;
          expression->attributes &= (~ATTR_ARRAY);
        } else if (astree_first(expression)->type.base == TYPE_STRING) {
          expression->type.base = TYPE_INT;
        } else {
          fprintf(stderr, "ERROR: only strings and arrays may be indexed\n");
        }
      } else {
        fprintf(stderr,
                "ERROR: argument to index operator must be of type "
                "int\n");
      }
      break;
    case TOK_ARROW:
      // evaluate left but not right since right
      // is always an ident
      status = validate_stmt_expr(astree_first(expression), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      const char *tid = astree_first(expression)->type.identifier;
      SymbolValue *struct_def =
          map_get(type_names, (char *)tid, strnlen(tid, MAX_STRING_LENGTH));
      if (astree_first(expression)->type.base == TYPE_STRUCT && struct_def) {
        /* make sure field is defined */
        const char *tid2 = astree_second(expression)->lexinfo;
        SymbolValue *field_def = map_get(struct_def->fields, (char *)tid2,
                                         strnlen(tid2, MAX_STRING_LENGTH));
        if (field_def) {
          assign_type_id(astree_second(expression), field_def);
          assign_type_id(expression, field_def);
        } else {
          fprintf(stderr, "ERROR: field %s is not a member of structure\n",
                  astree_second(expression)->lexinfo);
          return -1;
        }
      } else {
        fprintf(stderr, "ERROR: structure %s not defined for token %s\n",
                astree_first(expression)->type.identifier,
                astree_first(expression)->lexinfo);
        return -1;
      }
      break;
    case TOK_INTCON:
    case TOK_NULLPTR:
    case TOK_CHARCON:
      // types are set on construction and we don't need to do
      // anything else
      break;
    case TOK_STRINGCON:
      // save for intlang
      llist_push_back(string_constants, (char *)expression->lexinfo);
      break;
    case TOK_IDENT:
      DEBUGS('t', "bonk");
      status = assign_type_id(expression, NULL);
      if (status != 0) return status;
      break;
    default:
      fprintf(stderr, "ERROR: UNEXPECTED TOKEN IN STATEMENT/EXPRESSION: %s\n",
              expression->lexinfo);
      return -1;
  }
}

int validate_stmt_expr(ASTree *statement, const char *function_name,
                       size_t *sequence_nr) {
  int status;
  const char *ident;
  SymbolValue *function =
      function_name == DUMMY_FUNCTION
          ? NULL
          : map_get(globals, (char *)function_name,
                    strnlen(function_name, MAX_STRING_LENGTH));

  DEBUGS('t', "Validating next statement/expression");
  switch (statement->symbol) {
    // trees generated my the "statement" production
    case TOK_RETURN:
      if (llist_size(statement->children) <= 0) {
        if (function->type.base == TYPE_FUNCTION) {
          fprintf(stderr,
                  "ERROR: Return statement with value in void "
                  "function: %s\n",
                  function_name);
          return -1;
        }
      } else {
        validate_stmt_expr(astree_first(statement), function_name, sequence_nr);
        status = types_compatible(astree_first(statement), function,
                                  ARG1_AST & ARG2_SMV);
        if (status == TCHK_INCOMPATIBLE) {
          fprintf(stderr, "ERROR: Incompatible return type\n");
          return status;
        } else {
          statement->attributes |= ATTR_VREG;
          statement->attributes |= astree_first(statement)->attributes;
        }
      }
      break;
    case TOK_TYPE_ID:
      status = make_local_entry(statement, sequence_nr);
      if (status != 0) return status;
      break;
    case TOK_IF:
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (llist_size(statement->children) == 3)
        status = validate_stmt_expr(astree_third(statement), function_name,
                                    sequence_nr);
      if (status != 0) return status;
      break;
    case TOK_WHILE:
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      break;
    case TOK_BLOCK:
      status = validate_block(statement, function_name, sequence_nr);
      if (status != 0) return status;
      break;
    // gray area; vardecls (a statement) and assignments (an
    // expression) can both have an '=' as their root but we handle
    // them the same way
    case '=':
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (types_compatible(astree_first(statement), astree_second(statement),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible types for tokens: %s,%s %s,%s\n",
                parser_get_tname(astree_first(statement)->symbol),
                astree_first(statement)->lexinfo,
                parser_get_tname(astree_second(statement)->symbol),
                astree_second(statement)->lexinfo);
        return -1;
      } else if (!(astree_first(statement)->attributes & ATTR_LVAL)) {
        fprintf(stderr, "ERROR: Destination is not an LVAL\n");
        return -1;
      }
      // type is the type of the left operand
      statement->type = astree_second(statement)->type;
      break;
    // here begins the trees made by the "expr" production
    case TOK_EQ:
    case TOK_NE:
      // types can be arbitrary here
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (types_compatible(astree_first(statement), astree_second(statement),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible types for operator: %s\n",
                statement->symbol);
        return -1;
      }
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
      // handle int exprs
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (types_compatible(astree_first(statement), astree_second(statement),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible types for operator: %s\n",
                statement->symbol);
        return -1;
      } else if (astree_first(statement)->type.base != TYPE_INT) {
        fprintf(stderr, "ERROR: Operator %s must have operands of type int\n",
                parser_get_tname(statement->symbol));
        fprintf(stderr, "Offending operands: %s %s\n",
                astree_first(statement)->lexinfo,
                astree_second(statement)->lexinfo);
        return -1;
      }
      break;
    case TOK_NOT:
    case TOK_POS:
    case TOK_NEG:
      // unary operators
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (astree_first(statement)->attributes != TYPE_INT) {
        fprintf(stderr, "ERROR: '%s' argument must be of type int\n",
                statement->lexinfo);
        return -1;
      }
      break;
    case TOK_ALLOC:
      status = validate_type_id(astree_first(statement), statement);
      if (status != 0) return status;
      if (llist_size(statement->children) == 2) {
        status = validate_stmt_expr(astree_second(statement), function_name,
                                    sequence_nr);
        if (status != 0) return status;
        if (astree_second(statement)->attributes != TYPE_INT ||
            (astree_second(statement)->attributes & ATTR_ARRAY)) {
          fprintf(stderr, "ERROR: alloc size argument must be of type int!");
          return -1;
        }
      }
      break;
    case TOK_CALL:
      statement->attributes |= ATTR_VREG;
      status = validate_call(statement);
      if (status != 0) return status;
      break;
    case TOK_INDEX:
      // evaluate left and right side
      // make sure left is an array and right is an int
      // set type to type of array, minus the array
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      status = validate_stmt_expr(astree_second(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      if (astree_second(statement)->attributes == TYPE_INT) {
        if (astree_first(statement)->attributes & ATTR_ARRAY) {
          statement->type = astree_first(statement)->type;
          statement->attributes &= (~ATTR_ARRAY);
        } else if (astree_first(statement)->type.base == TYPE_STRING) {
          statement->type.base = TYPE_INT;
        } else {
          fprintf(stderr, "ERROR: only strings and arrays may be indexed\n");
        }
      } else {
        fprintf(stderr,
                "ERROR: argument to index operator must be of type "
                "int\n");
      }
      break;
    case TOK_ARROW:
      // evaluate left but not right since right
      // is always an ident
      status = validate_stmt_expr(astree_first(statement), function_name,
                                  sequence_nr);
      if (status != 0) return status;
      const char *tid = astree_first(statement)->type.identifier;
      SymbolValue *struct_def =
          map_get(type_names, (char *)tid, strnlen(tid, MAX_STRING_LENGTH));
      if (astree_first(statement)->type.base == TYPE_STRUCT && struct_def) {
        /* make sure field is defined */
        const char *tid2 = astree_second(statement)->lexinfo;
        SymbolValue *field_def = map_get(struct_def->fields, (char *)tid2,
                                         strnlen(tid2, MAX_STRING_LENGTH));
        if (field_def) {
          assign_type_id(astree_second(statement), field_def);
          assign_type_id(statement, field_def);
        } else {
          fprintf(stderr, "ERROR: field %s is not a member of structure\n",
                  astree_second(statement)->lexinfo);
          return -1;
        }
      } else {
        fprintf(stderr, "ERROR: structure %s not defined for token %s\n",
                astree_first(statement)->type.identifier,
                astree_first(statement)->lexinfo);
        return -1;
      }
      break;
    case TOK_INTCON:
    case TOK_NULLPTR:
    case TOK_CHARCON:
      // types are set on construction and we don't need to do
      // anything else
      break;
    case TOK_STRINGCON:
      // save for intlang
      llist_push_back(string_constants, (char *)statement->lexinfo);
      break;
    case TOK_IDENT:
      DEBUGS('t', "bonk");
      status = assign_type_id(statement, NULL);
      if (status != 0) return status;
      break;
    default:
      fprintf(stderr, "ERROR: UNEXPECTED TOKEN IN STATEMENT/EXPRESSION: %s\n",
              statement->lexinfo);
      return -1;
  }
  return 0;
}

int validate_call(ASTree *call) {
  const char *identifier = astree_first(call)->lexinfo;
  // params are at the same level as the id
  SymbolValue *function = map_get(globals, (char *)identifier,
                                  strnlen(identifier, MAX_STRING_LENGTH));
  if (function) {
    if (llist_size(call->children) - 1 != llist_size(function->parameters)) {
      fprintf(stderr, "ERROR: incorrect number of arguments %s\n", identifier);
      return -1;
    }
    DEBUGS('t', "Validating arguments for call to %s, num call->children: %d",
           identifier, llist_size(call->children) - 1);
    for (size_t i = 0; i < llist_size(function->parameters); ++i) {
      DEBUGS('t', "Validating argument %d", i);
      ASTree *param = llist_get(call->children, i + 1);
      size_t dummy_sequence = 0;
      DEBUGS('t', "Argument was not null");
      int status = validate_stmt_expr(param, DUMMY_FUNCTION, &dummy_sequence);
      if (status != 0) return status;
      DEBUGS('t', "Comparing types");
      if (types_compatible(param, llist_get(function->parameters, i),
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

// TODO: function for sorting table entries (not necessary; just makes output
// prettier for the type checker)
/*vector<symbol_entry> type_checker::sort_symtable(symbol_table* table) {
    DEBUGS('9', "Sorting symtable");
    vector<symbol_entry> sorted;
    if(table->empty()) return sorted;
    symbol_entry*  min_lloc = nullptr;
    while(sorted.size() < table->size()) {
        DEBUGS('9', "Sorting another table entry");
        for(auto & entry : *table) {
            bool in_sorted = false;
            for(symbol_entry sortee : sorted) {
                if(entry.first == sortee.first) {
                    DEBUGS('9', "Entry " << *(entry.first)
                            << " has already been sorted; continue");
                    in_sorted = true;
                }
            }
            if(in_sorted) continue;
            if(min_lloc == nullptr) min_lloc = &entry;

            if(entry.second->lloc.filenr < astree_second->
                  lloc.filenr) {
                min_lloc = &entry;
            } else if(
                    entry.second->lloc.filenr == astree_second->
                    lloc.filenr &&
                    entry.second->lloc.linenr < astree_second->
                    lloc.linenr) {
                min_lloc = &entry;
            } else if(
                    entry.second->lloc.filenr == astree_second->
                    lloc.filenr &&
                    entry.second->lloc.linenr == astree_second->
                    lloc.linenr &&
                    entry.second->lloc.offset <= astree_second->
                    lloc.offset) {
                min_lloc = &entry;
            }
        }
        DEBUGS('9', "Inserting entry " << *(astree_first));
        sorted.push_back(*min_lloc);
        min_lloc = nullptr;
    }
    return sorted;
}*/

int make_global_entry(ASTree *global) {
  const char *ident = extract_ident(global);
  size_t ident_len = strnlen(ident, MAX_STRING_LENGTH);
  SymbolValue *exists = map_get(globals, (char *)ident, ident_len);

  if (exists) {
    // error; duplicate declaration
    fprintf(stderr, "ERROR: Global var has already been declared: %s\n",
            extract_ident(global));
    return -1;
  } else {
    DEBUGS('t', "Making global entry for value %s", ident);
    ASTree *type = astree_first(global);
    ASTree *identifier = astree_second(global);
    identifier->attributes |= ATTR_LVAL;
    int status = validate_type_id(astree_first(global), astree_second(global));
    if (status != 0) return status;
    if (llist_size(global->children) == 3) {
      size_t dummy_sequence = 0;
      status = validate_stmt_expr(astree_third(global), DUMMY_FUNCTION,
                                  &dummy_sequence);
      if (status != 0) return status;
      if (types_compatible(astree_third(global), astree_second(global),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible type for global variable\n");
        return -1;
      }
    }

    SymbolValue *global_value = malloc(sizeof(*global_value));
    symbol_value_init(global_value, identifier, 0, 0);
    map_insert(globals, (char *)extract_ident(global), ident_len, global_value);
    return 0;
  }
}

int make_structure_entry(ASTree *structure) {
  const char *structure_type = extract_type(structure);
  DEBUGS('t', "ree");
  size_t structure_type_len = strnlen(structure_type, MAX_STRING_LENGTH);
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
    // start from 2nd child; 1st was type name
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
        int status =
            validate_type_id(astree_first(field), astree_second(field));
        if (status != 0) return status;
        astree_second(field)->attributes |= ATTR_LVAL;
        field_value = malloc(sizeof(*field_value));
        symbol_value_init(field_value, astree_second(field), i - 1, 0);
        map_insert(fields, (char *)field_id_str, field_id_str_len, field_value);
        DEBUGS('t', "Field inserted at %s", astree_second(field)->lexinfo);
      }
    }
    structure_value->fields = fields;
  }
  return 0;
}

int make_function_entry(ASTree *function) {
  int ret = 0;
  DEBUGS('t', "Making entry for node with symbol: %s",
         parser_get_tname(function->symbol));
  ASTree *type_id = astree_first(function);
  DEBUGS('t', "oof");
  astree_second(type_id)->type.base = TYPE_FUNCTION;
  DEBUGS('t', "oof");
  int status = validate_type_id(astree_first(type_id), astree_second(type_id));
  SymbolValue *function_entry = NULL;
  if (status != 0) return status;
  if (type_id->attributes |= ATTR_ARRAY) {
    fprintf(stderr, "ERROR: Function %s has an array return type.\n",
            extract_ident(type_id));
    // TODO(rbergero): keep going if we can
    return -1;
  } else if (type_id->type.base = TYPE_STRUCT) {
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

  function_entry = malloc(sizeof(*function_entry));
  symbol_value_init(function_entry, astree_second(type_id), 0, 0);
  locals = malloc(sizeof(*locals));
  status = map_init(locals, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
  if (status) {
    fprintf(stderr, "fuck you\n");
    abort();
  }
  // check and add parameters; set block
  set_blocknr(astree_second(function), next_block);
  ASTree *params = astree_second(function);
  size_t param_sequence_nr = 0;
  for (size_t i = 0; i < llist_size(params->children); ++i) {
    ASTree *param = llist_get(params->children, i);
    const char *param_id_str = extract_ident(param);
    /*
    int empty;
    khiter_t k = kh_get (SymbolTable, locals, param_id_str);
    if (!empty) {
        fprintf (stderr,
                 "ERROR: Duplicate declaration of parameter: %s\n",
                 param_id_str);
        return -1;
    }
    */
    ASTree *param_identifier = astree_second(param);
    DEBUGS('t', "Defining function parameter %s", extract_ident(param));
    status = make_local_entry(param, &param_sequence_nr);
    if (status != 0) return status;
    size_t param_id_str_len = strnlen(param_id_str, MAX_STRING_LENGTH);
    SymbolValue *param_entry =
        map_get(locals, (char *)param_id_str, param_id_str_len);
    llist_push_back(function_entry->parameters, param_entry);
  }

  DEBUGS('t', "Inserting function entry with block id %u", next_block);
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
      status =
          validate_block(astree_third(function), function_id, &new_sequence_nr);
      if (status != 0) return status;
      prototype->has_block = 1;
    }
  } else {
    map_insert(globals, (char *)function_id, function_id_len, function_entry);
    if (llist_size(function->children) == 3) {
      DEBUGS('t', "No protoype; defining function %s", function_id);
      size_t new_sequence_nr = 0;
      set_blocknr(astree_third(function), next_block);
      status =
          validate_block(astree_third(function), function_id, &new_sequence_nr);
      if (status != 0) return status;
      function_entry->has_block = 1;
    }
  }

  llist_push_back(tables, locals);
  locals = NULL;
  ++next_block;
  return 0;
}

int make_local_entry(ASTree *local, size_t *sequence_nr) {
  const char *local_ident = extract_ident(local);
  size_t local_ident_len = strnlen(local_ident, MAX_STRING_LENGTH);
  DEBUGS('t', "ree: %p", locals);
  SymbolValue *existing_entry =
      map_get(locals, (char *)local_ident, local_ident_len);

  DEBUGS('t', "ree");
  if (existing_entry) {
    DEBUGS('t', "Got symtable entry %p", existing_entry);
    fprintf(stderr,
            "ERROR: Duplicate declaration of variable %s at location %d\n",
            extract_ident(local), existing_entry->loc.linenr);
    return -1;
  } else {
    ASTree *type = astree_first(local);
    ASTree *identifier = astree_second(local);
    DEBUGS('t', "Making entry for local var %s", identifier->lexinfo);
    identifier->attributes |= ATTR_LVAL;
    int status = validate_type_id(astree_first(local), astree_second(local));
    if (status != 0) return status;
    DEBUGS('t', "Checking to see if var has an initial value");
    if (llist_size(local->children) == 3) {
      size_t dummy_sequence = 0;
      DEBUGS('t', "it do have value");
      status = validate_stmt_expr(astree_third(local), DUMMY_FUNCTION,
                                  &dummy_sequence);
      if (status != 0) return status;
      if (types_compatible(astree_third(local), astree_second(local),
                           ARG1_AST & ARG2_AST) == TCHK_INCOMPATIBLE) {
        fprintf(stderr, "ERROR: Incompatible type for local variable\n");
        return -1;
      }
    }

    SymbolValue *local_value = malloc(sizeof(*local_value));
    symbol_value_init(local_value, astree_second(local), *sequence_nr,
                      next_block);
    size_t identifier_len = strnlen(identifier->lexinfo, MAX_STRING_LENGTH);
    map_insert(locals, (char *)identifier->lexinfo, identifier_len,
               local_value);
    ++sequence_nr;
    return 0;
  }
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
  for (size_t i = 0; i < llist_size(root->children); ++i) {
    ASTree *child = llist_get(root->children, i);
    int status;
    /*
     * validation method requires a sequence number but expressions
     * as defined by the parser cannot have vardecls below them in
     * the tree so we don't actually need to track sequence once we
     * get down that far, even though much of the code is the same
     */
    size_t dummy_sequence = 0;
    switch (child->symbol) {
      case TOK_FUNCTION:
        DEBUGS('t', "bing");
        status = make_function_entry(child);
        if (status != 0) return status;
        break;
      case TOK_STRUCT:
        DEBUGS('t', "bong");
        status = make_structure_entry(child);
        if (status != 0) return status;
        break;
      case TOK_TYPE_ID:
        DEBUGS('t', "boing");
        status = make_global_entry(child);
        if (status != 0) return status;
        break;
      case '=':
        DEBUGS('t', "bonk");
        status = make_global_entry(astree_first(child));
        if (status != 0) return status;
        status = validate_stmt_expr(astree_second(child), DUMMY_FUNCTION,
                                    &dummy_sequence);
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
        // type is the type of the right operand
        child->type = astree_second(child)->type;
        if (status != 0) return status;
        break;
      default:
        fprintf(stderr, "ERROR: Unexpected symbol at top level: %s\n",
                parser_get_tname(child->symbol));
        return -1;
    }
  }
  // use local table list to group all tables together
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
