#include "tchk_decl.h"

#include "asmgen.h"
#include "assert.h"
#include "badalist.h"
#include "lyutils.h"
#include "state.h"
#include "stdlib.h"

/* TODO(Robert): make naming consistent */
/* TODO(Robert): Type: make sure that type information is propogated correctly,
 * meaning mostly that the type of syntax tree nodes is set to `NULL` when it
 * gets moved up the tree
 */

/* tags will need two passes to define their members: the first for inserting
 * the symbols into the symbol table, and the second to swipe the symbols from
 * the members and put them into an auxspec
 */
ASTree *validate_tag_typespec(ASTree *spec_list, ASTree *tag) {
  if (!type_is_none(spec_list->type) ||
      type_add_flags(tag->type, type_get_flags(spec_list->type)))
    return astree_create_errnode(astree_adopt(spec_list, 1, tag),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list, tag);

  int status = type_destroy(spec_list->type);
  if (status) abort();
  spec_list->type = tag->type;
  tag->type = NULL;

  return astree_adopt(spec_list, 1, tag);
}

ASTree *validate_type_id_typespec(ASTree *spec_list, ASTree *type_id) {
  const char *type_name = type_id->lexinfo;
  size_t type_name_len = strlen(type_name);
  SymbolValue *symval = NULL;
  state_get_symbol(state, type_name, type_name_len, &symval);
  if (!symval) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                                 BCC_TERR_TYPEID_NOT_FOUND, 2, spec_list,
                                 type_id);
  } else if (!type_is_typedef(symval->type)) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                                 BCC_TERR_EXPECTED_TYPEID, 2, spec_list,
                                 type_id);
  } else if (!type_is_none(spec_list->type) ||
             (type_is_declarator(symval->type) &&
              !type_is_pointer(symval->type) &&
              type_is_qualified(spec_list->type))) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                                 type_id);
  } else {
    unsigned int old_flags = type_get_flags(spec_list->type);
    int status = type_destroy(spec_list->type);
    if (status) abort();
    status = type_copy(&spec_list->type, symval->type, 1);
    if (status) abort();

    Type *stripped;
    if (type_is_declarator(spec_list->type)) {
      status = type_strip_all_declarators(&stripped, spec_list->type);
      if (status) abort();
      if (type_is_pointer(spec_list->type)) {
        /* apply qualifiers to the pointer */
        status = type_add_flags(spec_list->type, old_flags & QUAL_FLAG_MASK);
        if (status)
          return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                                       BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                                       type_id);
        old_flags &= ~QUAL_FLAG_MASK;
      }
    } else {
      stripped = spec_list->type;
    }

    status = type_add_flags(stripped, old_flags);
    if (status)
      return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                                   BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                                   type_id);
    else
      return astree_adopt(spec_list, 1, type_id);
  }
}

static unsigned int type_flag_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_VOID:
      return SPEC_FLAG_VOID;
    case TOK_CHAR:
      return SPEC_FLAG_CHAR;
    case TOK_INT:
      return SPEC_FLAG_INTEGRAL;
    case TOK_SHORT:
      return SPEC_FLAG_SHORT;
    case TOK_LONG:
      return SPEC_FLAG_LONG;
    case TOK_SIGNED:
      return SPEC_FLAG_SIGNED;
    case TOK_UNSIGNED:
      return SPEC_FLAG_UNSIGNED;
    case TOK_FLOAT:
      return SPEC_FLAG_FLOAT;
    case TOK_DOUBLE:
      return SPEC_FLAG_DOUBLE;
    case TOK_CONST:
      return QUAL_FLAG_CONST;
    case TOK_VOLATILE:
      return QUAL_FLAG_VOLATILE;
    case TOK_RESTRICT:
      return QUAL_FLAG_NONE; /* C90: no restrict */
    case TOK_TYPEDEF:
      return STOR_FLAG_TYPEDEF;
    case TOK_AUTO:
      return STOR_FLAG_AUTO;
    case TOK_REGISTER:
      return STOR_FLAG_REGISTER;
    case TOK_STATIC:
      return STOR_FLAG_STATIC;
    case TOK_EXTERN:
      return STOR_FLAG_EXTERN;
    case TOK_UNION:
      /* fallthrough */
    case TOK_STRUCT:
      /* fallthrough */
    case TOK_ENUM:
      /* fallthrough */
    case TOK_TYPEDEF_NAME:
      /* fallthrough */
    default:
      abort();
  }
}

ASTree *validate_typespec(ASTree *spec_list, ASTree *spec) {
  if (spec_list->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(spec_list, spec);
  } else if (spec->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(spec_list, spec);
  }

  assert(spec_list->type != NULL);
  assert(!type_is_declarator(spec_list->type));

  if (spec->symbol == TOK_STRUCT || spec->symbol == TOK_UNION ||
      spec->symbol == TOK_ENUM) {
    return validate_tag_typespec(spec_list, spec);
  } else if (spec->symbol == TOK_TYPEDEF_NAME) {
    return validate_type_id_typespec(spec_list, spec);
  } else if (type_add_flags(spec_list->type,
                            type_flag_from_symbol(spec->symbol))) {
    return astree_create_errnode(astree_adopt(spec_list, 1, spec),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                                 spec);
  } else {
    return astree_adopt(spec_list, 1, spec);
  }
}

ASTree *validate_typespec_list(ASTree *spec_list) {
  if (spec_list->symbol == TOK_TYPE_ERROR) return spec_list;
  int status = type_normalize(spec_list->type);
  if (status)
    return astree_create_errnode(spec_list, BCC_TERR_INCOMPLETE_TYPE, 1,
                                 spec_list);
  return spec_list;
}

static int location_is_empty(Location *loc) {
  return loc->filenr == 0 && loc->linenr == 0 && loc->offset == 0;
}

static unsigned int get_link_and_store(const Type *type) {
  unsigned int root_flags;
  if (type_is_declarator(type)) {
    Type *stripped;
    int status = type_strip_all_declarators(&stripped, type);
    if (status || stripped == NULL) abort();
    root_flags = type_get_flags(stripped);
  } else {
    root_flags = type_get_flags(type);
  }

  if (root_flags & STOR_FLAG_TYPEDEF) {
    return SYMFLAG_TYPEDEF;
  } else if (llist_size(&state->table_stack) == 1) {
    if (root_flags & STOR_FLAG_EXTERN) {
      return SYMFLAG_LINK_EXT | SYMFLAG_STORE_EXT;
    } else if (root_flags & STOR_FLAG_STATIC) {
      return SYMFLAG_LINK_INT | SYMFLAG_STORE_STAT;
    } else {
      return SYMFLAG_LINK_EXT | SYMFLAG_STORE_STAT;
    }
  } else if ((root_flags & STOR_FLAG_EXTERN) || type_is_function(type)) {
    /* validate_declaration will handle extern block symbols */
    return SYMFLAG_LINK_EXT | SYMFLAG_STORE_EXT;
  } else if (root_flags & STOR_FLAG_STATIC) {
    return SYMFLAG_LINK_NONE | SYMFLAG_STORE_STAT;
  } else {
    return SYMFLAG_LINK_NONE | SYMFLAG_STORE_AUTO;
  }
}

static int linkage_valid(unsigned int new_flags, unsigned int old_flags) {
  if (new_flags & old_flags & SYMFLAG_TYPEDEF) return 0;
  assert(new_flags & SYMFLAGS_LINK);
  assert(old_flags & SYMFLAGS_LINK);
  if ((new_flags & SYMFLAG_LINK_NONE) || (old_flags & SYMFLAG_LINK_NONE)) {
    return 0;
  } else if ((old_flags & SYMFLAG_LINK_EXT) && (new_flags & SYMFLAG_LINK_EXT)) {
    return 1;
  } else if (old_flags & SYMFLAG_LINK_INT) {
    if (new_flags & SYMFLAG_LINK_INT) {
      return 1;
    } else if ((new_flags & SYMFLAG_LINK_EXT) &&
               (new_flags & SYMFLAG_STORE_EXT)) {
      return 1;
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

static int is_array_completion(const Type *new_type, const Type *old_type) {
  if (!type_is_array(new_type) || !type_is_array(old_type))
    return 0;
  else if (new_type->array.length != 0 && old_type->array.length != 0)
    return 0;

  Type *new_elem_type, *old_elem_type;
  int status = type_strip_declarator(&new_elem_type, new_type);
  if (status) abort();
  status = type_strip_declarator(&old_elem_type, old_type);
  if (status) abort();
  return types_equivalent(new_elem_type, old_elem_type, 0, 1);
}

static const char *create_unique_name(ASTree *tree) {
  const char *node_str;
  switch (tree->symbol) {
    case TOK_TYPE_NAME:
      node_str = "type_name";
      break;
    case TOK_STRUCT:
      node_str = "struct";
      break;
    case TOK_ENUM:
      node_str = "enum";
      break;
    case TOK_UNION:
      node_str = "union";
      break;
    default:
      fprintf(stderr, "ERROR: unable to create unique name for symbol %s\n",
              parser_get_tname(tree->symbol));
      abort();
  }

  char filenr_str[32], linenr_str[32], offset_str[32];
  sprintf(filenr_str, "%lu", tree->loc.filenr);
  sprintf(linenr_str, "%lu", tree->loc.linenr);
  sprintf(offset_str, "%lu", tree->loc.offset);
  size_t name_len = strlen(filenr_str) + strlen(linenr_str) +
                    strlen(offset_str) + strlen(node_str) + 3;
  char *name = malloc(sizeof(char) * (name_len + 1));

  sprintf(name, "%s_%s_%s_%s", filenr_str, linenr_str, offset_str, node_str);
  return name;
}

/*
 * Combines type specifier and declarator information and inserts symbol value
 * into the table at the current scope. Sets source location of the declaration
 * to the source location of the first declarator, which is convenient when
 * sorting symbol values and block scopes. Returns the declarator node passed in
 * as the second argument, or an error node enclosing this node. Can be safely
 * called with error nodes as arguments.
 */
ASTree *validate_declaration(ASTree *declaration, ASTree *declarator) {
  PFDBG1('t', "Making object entry for value %s", declarator->lexinfo);
  if (location_is_empty(&UNWRAP(declaration)->loc))
    UNWRAP(declaration)->loc = UNWRAP(declarator)->loc;
  if (declaration->symbol == TOK_TYPE_ERROR ||
      declarator->symbol == TOK_TYPE_ERROR)
    return declarator;

  ASTree *spec_list = astree_get(declaration, 0);
  if (declarator->type == NULL) {
    int status = type_copy(&declarator->type, spec_list->type, 0);
    if (status) abort();
  } else {
    int status = type_append(declarator->type, spec_list->type, 1);
    if (status) abort();
  }

  /* TODO(Robert): determine if type names need to have a symbol defined for
   * them for any reason
   */
  if (declarator->symbol == TOK_TYPE_NAME) return declarator;

  const char *identifier = declarator->symbol == TOK_TYPE_NAME
                               ? create_unique_name(declarator)
                               : declarator->lexinfo;
  size_t identifier_len = strlen(identifier);
  SymbolValue *exists = NULL;
  int is_redeclaration =
      state_get_symbol(state, identifier, identifier_len, &exists);
  unsigned int symbol_flags = get_link_and_store(declarator->type);
  if (type_is_incomplete(declarator->type)) symbol_flags |= SYMFLAG_INCOMPLETE;
  if (declarator->symbol == TOK_TYPE_NAME) symbol_flags |= SYMFLAG_TYPENAME;

  if (is_redeclaration) {
    if (!linkage_valid(symbol_flags, exists->flags)) {
      return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1,
                                   declarator);
    } else if (types_equivalent(declarator->type, exists->type, 0, 1)) {
      int status = type_destroy(declarator->type);
      if (status) abort();
      declarator->type = exists->type;
      return declarator;
      /* TODO(Robert): i'm not sure what the point of this is... */
    } else if (is_array_completion(declarator->type, exists->type)) {
      int status = type_destroy(exists->type);
      if (status) abort();
      exists->type = declarator->type;
      declarator->type = exists->type;
      return declarator;
    } else if (type_is_function(exists->type) &&
               type_is_function(declarator->type)) {
      Type *exists_ret_type, *declarator_ret_type;
      assert(!type_strip_declarator(&exists_ret_type, exists->type));
      assert(!type_strip_declarator(&declarator_ret_type, declarator->type));
      if (type_is_prototyped_function(exists->type) &&
          type_is_prototyped_function(declarator->type)) {
        if (types_equivalent(exists->type, declarator->type, 0, 1)) {
          assert(!type_destroy(declarator->type));
          declarator->type = exists->type;
          return declarator;
        } else {
          return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1,
                                       declarator);
        }
      } else if (types_equivalent(exists_ret_type, declarator_ret_type, 0, 1)) {
        if (!type_is_prototyped_function(exists->type)) {
          /* give existing symbol a prototype; the existing type info may have
           * already been used elsewhere so we need to replace the contents
           * of the type info in place
           */
          /* TODO(Robert): because the type checker should consider any
           * arguments to a function without a prototype to be valid, the
           * assembly generator should use the argument list and not the
           * parameter list to marshal arguments for calls to old-style
           * functions
           */
          assert(exists->type->function.parameters == NULL);
          *(exists->type) = *(declarator->type);
          exists->type->function.next = exists_ret_type;
          exists->type->function.is_old_style = 1;
          declarator->type->function.parameters = NULL;
        }

        assert(!type_destroy(declarator->type));
        declarator->type = exists->type;
        return declarator;
      } else {
        return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1,
                                     declarator);
      }
    } else {
      return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1,
                                   declarator);
    }
  } else if (exists &&
             (symbol_flags & (SYMFLAG_STORE_EXT | SYMFLAG_LINK_EXT)) &&
             types_equivalent(declarator->type, exists->type, 0, 1)) {
    /* extern symval in block with outer symval declares nothing */
    int status = type_destroy(declarator->type);
    if (status) abort();
    declarator->type = exists->type;
    exists->flags |= SYMFLAG_INHERIT;
    return declarator;
  } else {
    SymbolValue *symbol =
        symbol_value_init(&declarator->loc, state_get_sequence(state));
    symbol->flags = symbol_flags;
    symbol->type = declarator->type;
    int status = state_insert_symbol(state, identifier, identifier_len, symbol);
    if (status) abort();
    /* typedefs and type names are not lvalues */
    if (!(symbol->flags & (SYMFLAG_TYPEDEF | SYMFLAG_TYPENAME)))
      declarator->attributes |= ATTR_EXPR_LVAL;
    return declarator;
  }
}

ASTree *finalize_declaration(ASTree *declaration) {
  if (declaration == &EMPTY_EXPR) return declaration;
  ASTree *errnode = NULL;
  if (declaration->symbol == TOK_TYPE_ERROR) {
    errnode = declaration;
    declaration = astree_get(declaration, 0);
  }

  /* free type specifier info now that declarators no longer need it */
  ASTree *spec_list = astree_get(declaration, 0);
  int status = type_destroy(spec_list->type);
  if (status) abort();
  spec_list->type = NULL;
  return errnode != NULL ? errnode : declaration;
}

ASTree *validate_array_size(ASTree *array, ASTree *expr) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(array, expr);
  }
  if (!type_is_integral(expr->type) || !(expr->attributes & ATTR_EXPR_CONST) ||
      (expr->attributes & ATTR_CONST_INIT)) {
    return astree_create_errnode(astree_adopt(array, 1, expr),
                                 BCC_TERR_EXPECTED_INTCONST, 2, array, expr);
  }
  if (expr->constant.integral.value == 0) {
    return astree_create_errnode(astree_adopt(array, 1, expr),
                                 BCC_TERR_EXPECTED_NONZERO, 2, array, expr);
  }
  return astree_adopt(array, 1, expr);
}

ASTree *validate_param_list(ASTree *param_list) {
  param_list->symbol_table = symbol_table_init(FUNCTION_TABLE);
  int status = state_push_table(state, param_list->symbol_table);
  if (status)
    return astree_create_errnode(param_list, BCC_TERR_LIBRARY_FAILURE, 0);
  else
    return param_list;
}

static void replace_param_dirdecl(ASTree *declarator) {
  assert(type_is_array(declarator->type) || type_is_function(declarator->type));
  Type *pointer_type;
  int status = type_init_pointer(&pointer_type, 0);
  if (status) abort();
  Type *stripped_type;
  status = type_strip_declarator(&stripped_type, declarator->type);
  if (status) abort();
  if (type_is_function(declarator->type))
    declarator->type->function.next = NULL;
  else
    declarator->type->array.next = NULL;
  status = type_destroy(declarator->type);
  if (status) abort();
  status = type_append(pointer_type, stripped_type, 0);
  if (status) abort();
  declarator->type = pointer_type;
}

ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator) {
  declarator = validate_declaration(declaration, declarator);
  if (param_list->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(
        param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(
        param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(
        param_list, astree_propogate_errnode(declaration, declarator));
  }

  if (type_is_function(declarator->type) || type_is_array(declarator->type)) {
    replace_param_dirdecl(declarator);
    if (declarator->symbol != TOK_TYPE_NAME) {
      SymbolValue *param_symval;
      int is_param =
          state_get_symbol(state, declarator->lexinfo,
                           strlen(declarator->lexinfo), &param_symval);
      assert(param_symval != NULL);
      assert(is_param);
      param_symval->type = declarator->type;
    }
  }

  return astree_adopt(
      param_list, 1,
      finalize_declaration(astree_adopt(declaration, 1, declarator)));
}

ASTree *finalize_param_list(ASTree *param_list, ASTree *ellipsis) {
  if (ellipsis != NULL)
    param_list->symbol == TOK_TYPE_ERROR
        ? (void)astree_propogate_errnode(param_list, ellipsis)
        : (void)astree_adopt(param_list, 1, ellipsis);
  int status = state_pop_table(state);
  if (status)
    return astree_create_errnode(param_list, BCC_TERR_LIBRARY_FAILURE, 0);
  else
    return param_list;
}

ASTree *define_params(ASTree *declarator, ASTree *param_list) {
  if (declarator->type != NULL &&
      (type_is_array(declarator->type) || type_is_function(declarator->type)))
    return astree_create_errnode(astree_adopt(declarator, 1, param_list),
                                 BCC_TERR_INCOMPATIBLE_DECL, 2, declarator,
                                 param_list);

  size_t parameters_size = astree_count(param_list);
  int is_variadic = 0, is_old_style = 0;
  Type **parameters;
  if (parameters_size == 0) {
    is_variadic = 1, is_old_style = 1, parameters = NULL;
  } else if (astree_get(param_list, 0)->symbol == TOK_VOID) {
    parameters = NULL, parameters_size = 0;
  } else {
    parameters = malloc(parameters_size * sizeof(Type *));
    if (astree_get(param_list, parameters_size - 1)->symbol == TOK_ELLIPSIS) {
      --parameters_size;
      is_variadic = 1;
    }
    size_t i;
    for (i = 0; i < parameters_size; ++i) {
      ASTree *param_declaration = astree_get(param_list, i);
      ASTree *param_declarator = astree_get(param_declaration, 1);
      parameters[i] = param_declarator->type;
    }
  }

  Type *function_type;
  int status = type_init_function(&function_type, parameters_size, parameters,
                                  is_variadic, is_old_style);
  if (status) abort();
  if (declarator->type != NULL) {
    status = type_append(declarator->type, function_type, 0);
    if (status) abort();
  } else {
    declarator->type = function_type;
  }
  return astree_adopt(declarator, 1, param_list);
}

ASTree *define_array(ASTree *declarator, ASTree *array) {
  /* no need to check for incomplete types here; we won't know if the element
   * type is incomplete until the next declarator or until all declarators are
   * handled or the declaration specifier type information is appended to the
   * declarator's type information
   */
  if (declarator->type != NULL && type_is_array(declarator->type) &&
      astree_count(array) == 0)
    return astree_create_errnode(astree_adopt(declarator, 1, array),
                                 BCC_TERR_INCOMPLETE_TYPE, 2, array,
                                 declarator->type);
  else if (declarator->type != NULL && type_is_function(declarator->type))
    return astree_create_errnode(astree_adopt(declarator, 1, array),
                                 BCC_TERR_INCOMPATIBLE_DECL, 2, declarator,
                                 array);

  Type *array_type;
  int status;
  if (astree_count(array) == 0)
    status = type_init_array(&array_type, 0, 1);
  else
    status = type_init_array(&array_type,
                             astree_get(array, 0)->constant.integral.value, 0);
  if (status) abort();
  if (declarator->type != NULL) {
    status = type_append(declarator->type, array_type, 0);
    if (status) abort();
  } else {
    declarator->type = array_type;
  }
  return astree_adopt(declarator, 1, array);
}

ASTree *define_pointer(ASTree *declarator, ASTree *pointer) {
  if (declarator->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(declarator, pointer);

  unsigned int qual_flags = QUAL_FLAG_NONE;
  size_t i;
  for (i = 0; i < astree_count(pointer); ++i) {
    ASTree *qualifier = astree_get(pointer, i);
    if (qual_flags & type_flag_from_symbol(qualifier->symbol)) {
      return astree_create_errnode(astree_adopt(declarator, 1, pointer),
                                   BCC_TERR_INCOMPATIBLE_SPEC, 2, declarator,
                                   qualifier);
    } else {
      qual_flags |= type_flag_from_symbol(qualifier->symbol);
    }
  }

  Type *pointer_type;
  int status = type_init_pointer(&pointer_type, qual_flags);
  if (status) abort();
  if (declarator->type != NULL) {
    status = type_append(declarator->type, pointer_type, 0);
    if (status) abort();
  } else {
    declarator->type = pointer_type;
  }
  return astree_adopt(declarator, 1, pointer);
}

ASTree *define_dirdecl(ASTree *declarator, ASTree *dirdecl) {
  if (declarator->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(declarator, dirdecl);

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

ASTree *define_symbol(ASTree *decl_list, ASTree *equal_sign,
                      ASTree *initializer) {
  ASTree *declarator =
      astree_remove(UNWRAP(decl_list), astree_count(decl_list) - 1);
  if (decl_list->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = decl_list;
    decl_list = astree_get(errnode, 0);
    if (initializer->symbol == TOK_TYPE_ERROR) {
      int status = type_merge_errors(errnode->type, initializer->type);
      if (status) abort();
      (void)astree_adopt(decl_list, 1,
                         astree_adopt(equal_sign, 2, declarator,
                                      astree_remove(initializer, 0)));
      status = astree_destroy(initializer);
      if (status) abort();
      return errnode;
    } else {
      (void)astree_adopt(decl_list, 1,
                         astree_adopt(equal_sign, 2, declarator, initializer));
      return errnode;
    }
  } else if (initializer->symbol == TOK_TYPE_ERROR) {
    ASTree *errnode = initializer;
    return astree_adopt(errnode, 1,
                        astree_adopt(decl_list, 1,
                                     astree_adopt(equal_sign, 2, declarator,
                                                  astree_remove(errnode, 0))));
  }
  assert(declarator->symbol == TOK_IDENT);

  SymbolValue *symval;
  (void)state_get_symbol(state, declarator->lexinfo,
                         strlen(declarator->lexinfo), &symval);
  assert(symval != NULL);
  if (symval->flags & SYMFLAG_DEFINED)
    return astree_create_errnode(
        astree_adopt(decl_list, 1,
                     astree_adopt(equal_sign, 2, declarator, initializer)),
        BCC_TERR_REDEFINITION, 1, declarator);
  symval->flags |= SYMFLAG_DEFINED;
  equal_sign->type = declarator->type;
  /* TODO(Robert): because init lists are hard to type check, all type checking
   * for initializers should occur in `init.c`, not here, and occurrs
   * simultaneously with code generation. currently, `traverse_initializer`
   * can't handle struct/union assignment when the initializer is not an
   * initializer list.
   *
   * in short, initialization is a mess and the code that performs it is all
   * over the place. it would be nice if it was all togther so that i don't
   * forget to do any type checking, like i did before
   */
  /* TODO(Robert): do not allow string literals or initializer lists to exceed
   * capacity for fixed size arrays, either here or in init.c
   */
  /* TODO(Robert): according to the standard, attempting to modify a string
   * literal is undefined behavior, and the type of a string literal is
   * `char []`. It is not const-qualified. However, the compiler marks string
   * literals as `const` and stores them in read-only memory. I am not sure if
   * this behavior is standards-compliant.
   */
  if (initializer->symbol != TOK_INIT_LIST &&
      !((type_is_char_array(declarator->type) ||
         type_is_pointer(declarator->type)) &&
        initializer->symbol == TOK_STRINGCON) &&
      !types_assignable(declarator->type, initializer->type,
                        astree_is_const_zero(initializer))) {
    return astree_create_errnode(
        astree_adopt(decl_list, 1,
                     astree_adopt(equal_sign, 2, declarator, initializer)),
        BCC_TERR_INCOMPATIBLE_TYPES, 3, equal_sign, declarator->type,
        initializer->type);
  } else {
    /* wait to emit code until `finalize_declaration` */
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
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

  SymbolValue *symval;
  int in_current_scope = state_get_symbol(state, declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symval);
  assert(in_current_scope);
  assert(symval != NULL);
  if (!type_is_function(symval->type)) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                                 BCC_TERR_EXPECTED_FUNCTION, 1, declarator);
  } else if (symval->flags & SYMFLAG_DEFINED) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                                 BCC_TERR_REDEFINITION, 1, declarator);
  }

  /* param list should be first child for properly defined functions */
  ASTree *param_list = astree_get(declarator, 0);
  assert(param_list->symbol == TOK_PARAM_LIST);
  size_t i;
  for (i = 0; i < astree_count(param_list); ++i) {
    ASTree *param = astree_get(param_list, i);
    if (param->symbol == TOK_TYPE_NAME) {
      return astree_create_errnode(
          astree_adopt(declaration, 2, declarator, body),
          BCC_TERR_EXPECTED_DECLARATOR, 2, declarator, param);
    }
  }

  /* treat body like a normal block statement, but move param table to body node
   * and define function before entering body scope */
  if (param_list->symbol_table == NULL) {
    body->symbol_table = symbol_table_init(FUNCTION_TABLE);
  } else {
    body->symbol_table = param_list->symbol_table;
    param_list->symbol_table = NULL;
  }
  int status = state_push_table(state, body->symbol_table);
  if (status) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                                 BCC_TERR_LIBRARY_FAILURE, 0);
  }

  /* TODO(Robert): set function to a dummy value even in the event of failure so
   * that return statements in the body may be processed in some way
   */
  assert(declarator->symbol == TOK_IDENT);
  status = state_set_function(state, declarator->lexinfo, symval);
  if (status)
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                                 BCC_TERR_FAILURE, 0);
  return begin_translate_fn(declaration, declarator, body);
}

ASTree *validate_fnbody_content(ASTree *function, ASTree *fnbody_content) {
  /* we can't reuse validate_block_content here because all that function does
   * is perform adoption and propogate errors, both of which are different here
   * because of the tree structure of function definitions
   */
  ASTree *fnbody = astree_get(UNWRAP(function), 2);
  if (function->symbol == TOK_TYPE_ERROR) {
    if (fnbody_content->symbol == TOK_TYPE_ERROR) {
      (void)astree_adopt(fnbody, 1, astree_remove(fnbody_content, 0));
      assert(!type_merge_errors(function->type, fnbody_content->type));
      assert(!astree_destroy(fnbody_content));
      return function;
    } else {
      (void)astree_adopt(fnbody, 1, fnbody_content);
      return function;
    }
  } else if (fnbody_content->symbol == TOK_TYPE_ERROR) {
    (void)astree_adopt(fnbody, 1, astree_remove(fnbody_content, 0));
    return astree_adopt(fnbody_content, 1, function);
  } else if (fnbody_content->symbol == TOK_DECLARATION) {
    ASTree *errnode = translate_local_declarations(fnbody, fnbody_content);
    if (errnode->symbol == TOK_TYPE_ERROR) {
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, function);
    } else {
      return function;
    }
  } else {
    (void)astree_adopt(fnbody, 1, fnbody_content);
    return function;
  }
}

ASTree *finalize_function(ASTree *function) {
  ASTree *body = astree_get(UNWRAP(function), 2);
  ASTree *ret = function;
  if (body->symbol_table->label_namespace != NULL) {
    ArrayList label_strs;
    assert(!alist_init(&label_strs,
                       map_size(body->symbol_table->label_namespace)));
    assert(!map_keys(body->symbol_table->label_namespace, &label_strs));
    size_t i;
    for (i = 0; i < alist_size(&label_strs); ++i) {
      const char *label_str = alist_get(&label_strs, i);
      size_t label_str_len = strlen(label_str);
      LabelValue *labval = state_get_label(state, label_str, label_str_len);
      if (!labval->is_defined) {
        ret = astree_create_errnode(ret, BCC_TERR_LABEL_NOT_FOUND, 1,
                                    labval->tree);
      }
    }
    assert(!alist_destroy(&label_strs, NULL));
  }
  SymbolValue *symval = state_get_function(state);
  symval->flags |= SYMFLAG_DEFINED;
  int status = state_unset_function(state);
  if (status) ret = astree_create_errnode(ret, BCC_TERR_FAILURE, 0);
  /* do not use finalize_block because it will put the error in an awkward place
   */
  status = state_pop_table(state);
  if (status) ret = astree_create_errnode(ret, BCC_TERR_LIBRARY_FAILURE, 0);
  if (ret->symbol == TOK_TYPE_ERROR) return ret;
  return end_translate_fn(function);
}

static TagType tag_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return TAG_STRUCT;
    case TOK_UNION:
      return TAG_UNION;
    case TOK_ENUM:
      return TAG_ENUM;
    default:
      abort();
  }
}

static int error_code_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return BCC_TERR_EXPECTED_STRUCT;
    case TOK_UNION:
      return BCC_TERR_EXPECTED_UNION;
    case TOK_ENUM:
      return BCC_TERR_EXPECTED_ENUM;
    default:
      abort();
  }
}

ASTree *validate_unique_tag(ASTree *tag_type_node, ASTree *left_brace) {
  const char *tag_name = create_unique_name(tag_type_node);
  const size_t tag_name_len = strlen(tag_name);
  TagType tag_type = tag_from_symbol(tag_type_node->symbol);
  TagValue *tag_value = tag_value_init(tag_type);
  int status = state_insert_tag(state, tag_name, tag_name_len, tag_value);
  if (status) abort();
  status = type_init_tag(&tag_type_node->type, QUAL_FLAG_NONE | STOR_FLAG_NONE,
                         tag_name, tag_value);
  if (status) abort();

  if (tag_type == TAG_ENUM) {
    /* remove struct/union member tables from scope stack */
    SymbolTable *top_scope = state_peek_table(state);
    while (top_scope->type == MEMBER_TABLE) {
      assert(!llist_push_back(&tag_value->data.enumerators.struct_name_spaces,
                              top_scope));
      assert(!state_pop_table(state));
      top_scope = state_peek_table(state);
    }
    tag_value->width = 4;
    tag_value->alignment = 4;
  } else {
    status = state_push_table(state, tag_value->data.members.by_name);
    if (status) abort();
  }
  return astree_adopt(tag_type_node, 1, left_brace);
}

ASTree *validate_tag_decl(ASTree *tag_type_node, ASTree *tag_name_node) {
  const char *tag_name = tag_name_node->lexinfo;
  const size_t tag_name_len = strlen(tag_name);
  TagValue *exists = NULL;
  int is_redeclaration = state_get_tag(state, tag_name, tag_name_len, &exists);
  TagType tag_type = tag_from_symbol(tag_type_node->symbol);

  if (exists) {
    if (is_redeclaration) {
      if (tag_type != exists->tag) {
        return astree_create_errnode(
            astree_adopt(tag_type_node, 1, tag_name_node),
            error_code_from_symbol(tag_type_node->symbol), 2, tag_type_node,
            tag_name_node);
      } else {
        int status =
            type_init_tag(&tag_type_node->type, QUAL_FLAG_NONE | STOR_FLAG_NONE,
                          tag_name, exists);
        if (status) abort();
        return astree_adopt(tag_type_node, 1, tag_name_node);
      }
    } else {
      /* TODO(Robert): error handling */
      TagValue *tag_value = NULL;
      if (tag_type != exists->tag) {
        tag_value = tag_value_init(tag_type);
        assert(!state_insert_tag(state, tag_name, tag_name_len, tag_value));
      } else {
        tag_value = exists;
      }

      int status =
          type_init_tag(&tag_type_node->type, QUAL_FLAG_NONE | STOR_FLAG_NONE,
                        tag_name, tag_value);
      if (status) abort();
      return astree_adopt(tag_type_node, 1, tag_name_node);
    }
  } else if (tag_type != TAG_ENUM) {
    /* TODO(Robert): error handling */
    TagValue *tag_value = tag_value_init(tag_type);
    assert(!state_insert_tag(state, tag_name, tag_name_len, tag_value));
    int status =
        type_init_tag(&tag_type_node->type, QUAL_FLAG_NONE | STOR_FLAG_NONE,
                      tag_name, tag_value);
    if (status) abort();
    return astree_adopt(tag_type_node, 1, tag_name_node);
  } else {
    /* do not insert enum tag; enum must declare their constants */
    return astree_create_errnode(astree_adopt(tag_type_node, 1, tag_name_node),
                                 BCC_TERR_TAG_NOT_FOUND, 1, tag_name_node);
  }
}

ASTree *validate_tag_def(ASTree *tag_type_node, ASTree *tag_name_node,
                         ASTree *left_brace) {
  const char *tag_name = tag_name_node->lexinfo;
  const size_t tag_name_len = strlen(tag_name);
  TagValue *exists = NULL;
  int is_redeclaration = state_get_tag(state, tag_name, tag_name_len, &exists);
  TagType tag_type = tag_from_symbol(tag_type_node->symbol);

  if (is_redeclaration && exists->is_defined) {
    return astree_create_errnode(
        astree_adopt(tag_type_node, 2, tag_name_node, left_brace),
        BCC_TERR_REDEFINITION, 1, tag_name_node);
  } else {
    /* TODO(Robert): error handling */
    TagValue *tag_value;
    if (exists && is_redeclaration) {
      tag_value = exists;
    } else {
      tag_value = tag_value_init(tag_type);
      assert(!state_insert_tag(state, tag_name, tag_name_len, tag_value));
    }

    int status =
        type_init_tag(&tag_type_node->type, QUAL_FLAG_NONE | STOR_FLAG_NONE,
                      tag_name, tag_value);
    if (status) abort();

    if (tag_type == TAG_ENUM) {
      /* remove struct/union member tables from scope stack */
      SymbolTable *top_scope = state_peek_table(state);
      while (top_scope->type == MEMBER_TABLE) {
        assert(!llist_push_back(&tag_value->data.enumerators.struct_name_spaces,
                                top_scope));
        assert(!state_pop_table(state));
        top_scope = state_peek_table(state);
      }
      tag_value->width = 4;
      tag_value->alignment = 4;
    } else {
      status = state_push_table(state, tag_value->data.members.by_name);
      if (status) abort();
    }
    return astree_adopt(tag_type_node, 2, tag_name_node, left_brace);
  }
}

ASTree *finalize_tag_def(ASTree *tag) {
  ASTree *errnode = NULL;
  if (tag->symbol == TOK_TYPE_ERROR) {
    errnode = tag;
    tag = astree_get(tag, 0);
  }

  /* TODO(Robert): Type: make sure that all type information has been processed
   * correctly by this point; before the base type and alignment were set here
   */
  TagValue *tag_value = tag->type->tag.value;
  tag_value->is_defined = 1;

  if (tag_value->tag == TAG_ENUM) {
    /* push struct/union member tables back onto the scope stack */
    while (!llist_empty(&tag_value->data.enumerators.struct_name_spaces)) {
      SymbolTable *member_scope =
          llist_pop_back(&tag_value->data.enumerators.struct_name_spaces);
      assert(member_scope != NULL);
      assert(!state_push_table(state, member_scope));
    }
  } else {
    assert(!state_pop_table(state));
    /* pad aggregate so that it can tile an array */
    if (tag_value->alignment != 0) {
      size_t padding =
          tag_value->alignment - (tag_value->width % tag_value->alignment);
      if (padding != tag_value->alignment) tag_value->width += padding;
    }
  }
  return errnode == NULL ? tag : errnode;
}

ASTree *define_enumerator(ASTree *enum_, ASTree *ident_node, ASTree *equal_sign,
                          ASTree *expr) {
  ASTree *left_brace =
      astree_get(enum_, astree_count(UNWRAP(enum_)) == 2 ? 1 : 0);
  if (enum_->symbol == TOK_TYPE_ERROR) {
    /* ASTree *real_enum = astree_get(enum_, 0); */
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return enum_;
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return enum_;
    }
  }
  const char *ident = ident_node->lexinfo;
  const size_t ident_len = strlen(ident);

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
  /* mark as enumeration consntant */
  symval->flags |= SYMFLAG_ENUM_CONST;
  /* copy type info from tag node */
  int status = type_copy(&symval->type, enum_->type, 0);
  if (status) abort();

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

  ident_node->type = symval->type;

  TagValue *tagval = symval->type->tag.value;
  if (equal_sign != NULL) {
    /* TODO?(Robert): evaluate enumeration constants */
    if (expr->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = astree_propogate_errnode(
          astree_adopt(equal_sign, 1, ident_node), expr);
      /* TODO?(Robert): have error propogation function handle more complex
       * syntax tree structures
       */
      return astree_propogate_errnode(enum_, errnode);
    }
    if (!(expr->attributes & ATTR_EXPR_CONST) ||
        (expr->attributes & ATTR_CONST_INIT)) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return astree_create_errnode(enum_, BCC_TERR_EXPECTED_ARITHCONST, 2,
                                   equal_sign, expr);
    }
    int *value = malloc(sizeof(int));
    *value = tagval->data.enumerators.last_value =
        expr->constant.integral.value;
    int status = map_insert(&tagval->data.enumerators.by_name, (char *)ident,
                            ident_len, value);
    assert(status == 0);
    astree_adopt(left_brace, 1, astree_adopt(equal_sign, 2, ident_node, expr));
    return enum_;
  } else {
    int *value = malloc(sizeof(int));
    *value = ++tagval->data.enumerators.last_value;
    int status = map_insert(&tagval->data.enumerators.by_name, (char *)ident,
                            ident_len, value);
    assert(status == 0);
    astree_adopt(left_brace, 1, ident_node);
    return enum_;
  }
}

ASTree *define_struct_member(ASTree *struct_, ASTree *member) {
  (void)finalize_declaration(member);
  ASTree *left_brace =
      astree_get(UNWRAP(struct_), astree_count(UNWRAP(struct_)) == 2 ? 1 : 0);
  if (struct_->symbol == TOK_TYPE_ERROR) {
    (void)astree_adopt(left_brace, 1, UNWRAP(member));
    if (member->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = member;
      (void)llist_extract(&member->children, 0);
      int status = type_merge_errors(struct_->type, errnode->type);
      if (status) abort();
      status = astree_destroy(errnode);
      if (status) abort();
    }
    return struct_;
  } else if (member->symbol == TOK_TYPE_ERROR) {
    (void)astree_adopt(left_brace, 1, llist_extract(&member->children, 0));
    return astree_adopt(member, 1, struct_);
  }

  TagValue *tagval = struct_->type->tag.value;
  LinkedList *member_list = &tagval->data.members.in_order;
  size_t i;
  /* skip first child, which is the typespec list */
  for (i = 1; i < astree_count(member); ++i) {
    ASTree *declarator = astree_get(member, i);
    SymbolValue *symval;
    int is_member = state_get_symbol(state, declarator->lexinfo,
                                     strlen(declarator->lexinfo), &symval);
    assert(is_member);
    assert(symval != NULL);
    size_t member_alignment = type_get_alignment(declarator->type);
    size_t member_width = type_get_width(declarator->type);
    if (tagval->alignment < member_alignment) {
      tagval->alignment = member_alignment;
    }
    if (tagval->tag == TAG_STRUCT) {
      size_t padding = member_alignment - (tagval->width % member_alignment);
      if (padding != member_alignment) tagval->width += padding;
      symval->disp = tagval->width;
      tagval->width += member_width;
    } else if (tagval->width < member_width) {
      tagval->width = member_width;
    }
    llist_push_back(member_list, symval);
  }
  astree_adopt(left_brace, 1, member);
  return struct_;
}

ASTree *declare_symbol(ASTree *declaration, ASTree *declarator) {
  ASTree *err_or_decl = validate_declaration(declaration, declarator);
  if (err_or_decl->symbol == TOK_TYPE_ERROR ||
      declaration->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(declaration, err_or_decl);
  else
    return astree_adopt(declaration, 1, declarator);
}

ASTree *validate_topdecl(ASTree *root, ASTree *topdecl) {
  if (root->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else if (topdecl->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else {
    return translate_global_declarations(root, topdecl);
  }
}
