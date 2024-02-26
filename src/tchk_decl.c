#include "tchk_decl.h"

#include "asmgen.h"
#include "assert.h"
#include "badalist.h"
#include "lyutils.h"
#include "state.h"
#include "stdlib.h"

/* TODO(Robert): make naming consistent */
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

/* tags will need two passes to define their members: the first for inserting
 * the symbols into the symbol table, and the second to swipe the symbols from
 * the members and put them into an auxspec
 */
ASTree *validate_tag_spec(ASTree *decl_specs, ASTree *tag_spec) {
  if (type_add_flags(tag_spec->type, type_get_flags(decl_specs->type)))
    return astree_create_errnode(astree_adopt(decl_specs, 1, tag_spec),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, decl_specs,
                                 tag_spec);

  type_destroy(decl_specs->type);
  decl_specs->type = tag_spec->type;
  tag_spec->type = NULL;

  return astree_adopt(decl_specs, 1, tag_spec);
}

ASTree *validate_typedef_name(ASTree *decl_specs, ASTree *typedef_name) {
  const char *type_name = typedef_name->lexinfo;
  size_t type_name_len = strlen(type_name);
  Symbol *symbol = NULL;
  state_get_symbol(state, type_name, type_name_len, &symbol);
  if (!symbol) {
    return astree_create_errnode(astree_adopt(decl_specs, 1, typedef_name),
                                 BCC_TERR_TYPEID_NOT_FOUND, 2, decl_specs,
                                 typedef_name);
  } else if (symbol->linkage != LINK_TYPEDEF) {
    return astree_create_errnode(astree_adopt(decl_specs, 1, typedef_name),
                                 BCC_TERR_EXPECTED_TYPEID, 2, decl_specs,
                                 typedef_name);
  } else if (!type_is_none(decl_specs->type) ||
             (type_is_declarator(symbol->type) &&
              !type_is_pointer(symbol->type) &&
              type_is_qualified(decl_specs->type))) {
    return astree_create_errnode(astree_adopt(decl_specs, 1, typedef_name),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, decl_specs,
                                 typedef_name);
  } else {
    unsigned int old_flags = type_get_flags(decl_specs->type);
    type_destroy(decl_specs->type);
    decl_specs->type = type_copy(symbol->type, 1);

    if (type_is_pointer(decl_specs->type)) {
      /* apply qualifiers to the pointer */
      int status = type_add_flags(decl_specs->type, old_flags & QUAL_FLAG_MASK);
      if (status)
        return astree_create_errnode(astree_adopt(decl_specs, 1, typedef_name),
                                     BCC_TERR_INCOMPATIBLE_SPEC, 2, decl_specs,
                                     typedef_name);
      old_flags &= ~QUAL_FLAG_MASK;
    }

    int status =
        type_add_flags(type_get_decl_specs(decl_specs->type), old_flags);
    if (status)
      return astree_create_errnode(astree_adopt(decl_specs, 1, typedef_name),
                                   BCC_TERR_INCOMPATIBLE_SPEC, 2, decl_specs,
                                   typedef_name);
    else
      return astree_adopt(decl_specs, 1, typedef_name);
  }
}

static unsigned int type_flag_from_tok_kind(int tok_kind) {
  switch (tok_kind) {
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

ASTree *validate_decl_spec(ASTree *decl_specs, ASTree *decl_spec) {
  if (decl_specs->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(decl_specs, decl_spec);
  } else if (decl_spec->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(decl_specs, decl_spec);
  }

  assert(decl_specs->type != NULL);
  assert(!type_is_declarator(decl_specs->type));

  if (decl_spec->tok_kind == TOK_ATTR_LIST) {
    astree_destroy(decl_spec);
    return decl_specs;
  } else if (decl_spec->tok_kind == TOK_STRUCT ||
             decl_spec->tok_kind == TOK_UNION ||
             decl_spec->tok_kind == TOK_ENUM) {
    return validate_tag_spec(decl_specs, decl_spec);
  } else if (decl_spec->tok_kind == TOK_TYPEDEF_NAME) {
    return validate_typedef_name(decl_specs, decl_spec);
  } else if (type_add_flags(decl_specs->type,
                            type_flag_from_tok_kind(decl_spec->tok_kind))) {
    return astree_create_errnode(astree_adopt(decl_specs, 1, decl_spec),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, decl_specs,
                                 decl_spec);
  } else {
    return astree_adopt(decl_specs, 1, decl_spec);
  }
}

ASTree *finalize_decl_specs(ASTree *decl_specs) {
  if (decl_specs->tok_kind == TOK_TYPE_ERROR) return decl_specs;
  int status = type_normalize(decl_specs->type);
  if (status)
    return astree_create_errnode(decl_specs, BCC_TERR_INCOMPLETE_TYPE, 1,
                                 decl_specs);
  return decl_specs;
}

static int location_is_empty(Location *loc) {
  return loc->filenr == 0 && loc->linenr == 0 && loc->offset == 0;
}

static Linkage determine_linkage(Type *type, int has_outer_symbol) {
  const SymbolTable *top_scope = state_peek_table(state);
  unsigned int decl_flags = type_get_flags(type_get_decl_specs(type));

  if (top_scope->kind == TABLE_MEMBER) {
    return LINK_MEMBER;
  } else if (top_scope->kind == TABLE_TRANS_UNIT) {
    switch (decl_flags & STOR_FLAG_MASK) {
      case STOR_FLAG_AUTO:
        /* fallthrough */
      case STOR_FLAG_REGISTER:
        return LINK_INVALID;
      case STOR_FLAG_EXTERN:
        /* fallthrough */
      case STOR_FLAG_NONE:
        return LINK_EXT;
      case STOR_FLAG_STATIC:
        return LINK_INT;
      case STOR_FLAG_TYPEDEF:
        return LINK_TYPEDEF;
      default:
        abort();
    }
  } else {
    switch (decl_flags & STOR_FLAG_MASK) {
      case STOR_FLAG_NONE:
        return type_is_function(type) ? LINK_EXT : LINK_NONE;
        /* fallthrough */
      case STOR_FLAG_STATIC:
        /* fallthrough */
      case STOR_FLAG_AUTO:
        /* fallthrough */
      case STOR_FLAG_REGISTER:
        return type_is_function(type) ? LINK_INVALID : LINK_NONE;
      case STOR_FLAG_EXTERN:
        return has_outer_symbol ? LINK_INHERIT : LINK_EXT;
      case STOR_FLAG_TYPEDEF:
        return LINK_TYPEDEF;
      default:
        abort();
    }
  }
}

static StorageClass determine_storage(Type *type, int has_outer_symbol) {
  const SymbolTable *top_scope = state_peek_table(state);
  unsigned int decl_flags = type_get_flags(type_get_decl_specs(type));

  if (top_scope->kind == TABLE_MEMBER) {
    return STORE_MEMBER;
  } else if (top_scope->kind == TABLE_TRANS_UNIT) {
    switch (decl_flags & STOR_FLAG_MASK) {
      case STOR_FLAG_AUTO:
        /* fallthrough */
      case STOR_FLAG_REGISTER:
        return STORE_INVALID;
      case STOR_FLAG_EXTERN:
        return STORE_EXT;
      case STOR_FLAG_STATIC:
        /* fallthrough */
      case STOR_FLAG_NONE:
        return STORE_STAT;
      case STOR_FLAG_TYPEDEF:
        return STORE_TYPEDEF;
      default:
        abort();
    }
  } else {
    switch (decl_flags & STOR_FLAG_MASK) {
      case STOR_FLAG_NONE:
        return type_is_function(type) ? STORE_EXT : STORE_AUTO;
      case STOR_FLAG_REGISTER:
        /* fallthrough */
      case STOR_FLAG_AUTO:
        return type_is_function(type) ? STORE_INVALID : STORE_AUTO;
      case STOR_FLAG_STATIC:
        return type_is_function(type) ? STORE_INVALID : STORE_STAT;
      case STOR_FLAG_EXTERN:
        return has_outer_symbol ? STORE_INHERIT : STORE_EXT;
      case STOR_FLAG_TYPEDEF:
        return STORE_TYPEDEF;
      default:
        abort();
    }
  }
}

static int linkage_valid(const Symbol *existing, Linkage linkage) {
  assert(existing != NULL);
  assert(existing->linkage != LINK_INVALID);
  assert(linkage != LINK_INVALID);
  if (existing->linkage == LINK_TYPEDEF || linkage == LINK_TYPEDEF) {
    /* typedef redefinition */
    return 0;
  } else if (existing->linkage == LINK_ENUM_CONST ||
             linkage == LINK_ENUM_CONST) {
    /* enumeration constant redefinition */
    return 0;
  } else if (existing->linkage == LINK_MEMBER && linkage == LINK_MEMBER) {
    /* record member redeclaration */
    return 0;
  } else if (existing->linkage == LINK_INHERIT || linkage == LINK_INHERIT) {
    /* extern symbol at block scope; other symbol must also inherit linkage and
     * storage class from an outer scope or must have external linkage
     */
    return existing->linkage == linkage || existing->linkage == LINK_EXT ||
           linkage == LINK_EXT;
  } else if (existing->linkage == LINK_NONE || linkage == LINK_NONE) {
    /* redeclaration of symbol at block scope */
    return 0;
  } else if (existing->linkage == LINK_EXT && linkage == LINK_EXT) {
    /* multiple external declarations are allowed */
    return 1;
  } else if (existing->linkage != LINK_INT) {
    /* declaration of symbol with internal linkage after previous declaration
     * with external linkage
     */
    return 0;
  } else if (linkage == LINK_INT || linkage == LINK_EXT) {
    /* multiple external declarations are allowed */
    return 1;
  } else {
    /* everything else; not sure if this is necessary */
    return 0;
  }
}

static int is_array_completion(const Type *new_type, const Type *old_type) {
  if (!type_is_array(new_type) || !type_is_array(old_type))
    return 0;
  else if (!new_type->array.deduce_length && !old_type->array.deduce_length)
    return 0;
  else if (new_type->array.length != 0 && old_type->array.length != 0)
    return 0;

  Type *new_elem_type = type_strip_declarator(new_type);
  Type *old_elem_type = type_strip_declarator(old_type);
  return types_equivalent(new_elem_type, old_elem_type, 0, 1);
}

static const char *create_unique_name(ASTree *tree) {
  const char *node_str;
  switch (tree->tok_kind) {
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
              parser_get_tname(tree->tok_kind));
      abort();
  }

  char filenr_str[32], linenr_str[32], offset_str[32];
  sprintf(filenr_str, "%lu", tree->loc.filenr);
  sprintf(linenr_str, "%lu", tree->loc.linenr);
  sprintf(offset_str, "%lu", tree->loc.offset);
  size_t name_len = strlen(filenr_str) + strlen(linenr_str) +
                    strlen(offset_str) + strlen(node_str) + 3;
  char *name = malloc(sizeof(char) * (name_len + 1));
  /* gcc doesn't like the printf below unless we make check malloc */
  if (name == NULL) abort();

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
  if (declaration->tok_kind == TOK_TYPE_ERROR ||
      declarator->tok_kind == TOK_TYPE_ERROR)
    return declarator;

  ASTree *decl_specs = astree_get(declaration, 0);
  if (declarator->type == NULL) {
    declarator->type = type_copy(decl_specs->type, 0);
  } else {
    (void)type_append(declarator->type, decl_specs->type, 1);
  }

  if (declarator->tok_kind == TOK_TYPE_NAME) return declarator;

  const char *identifier = declarator->tok_kind == TOK_TYPE_NAME
                               ? create_unique_name(declarator)
                               : declarator->lexinfo;
  size_t identifier_len = strlen(identifier);
  Symbol *exists = NULL;
  int is_redeclaration =
      state_get_symbol(state, identifier, identifier_len, &exists);
  int exists_equivalent =
      exists == NULL ? 0
                     : types_equivalent(declarator->type, exists->type, 0, 1);
  Linkage linkage = determine_linkage(declarator->type,
                                      !is_redeclaration && exists_equivalent);

  if (linkage == LINK_INVALID) {
    /* TODO(Robert): create a proper error for invalid linkage */
    return astree_create_errnode(declarator, BCC_TERR_FAILURE, 0);
  } else if (is_redeclaration) {
    if (!linkage_valid(exists, linkage)) {
      return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1,
                                   declarator);
    } else if (types_equivalent(declarator->type, exists->type, 0, 1)) {
      type_destroy(declarator->type);
      declarator->type = exists->type;
      return declarator;
    } else if (is_array_completion(declarator->type, exists->type)) {
      if (exists->type->array.length == 0 &&
          declarator->type->array.length != 0) {
        /* replace type information in place since it may have been used */
        /* TODO(Robert): add a function `type_replace` or similar that replaces
         * type information in place
         */
        exists->type->array.deduce_length =
            declarator->type->array.deduce_length;
        exists->type->array.length = declarator->type->array.length;
      }
      type_destroy(declarator->type);
      declarator->type = exists->type;
      return declarator;
    } else if (type_is_function(exists->type) &&
               type_is_function(declarator->type)) {
      Type *exists_ret_type = type_strip_declarator(exists->type);
      Type *declarator_ret_type = type_strip_declarator(declarator->type);
      if (type_is_prototyped_function(exists->type) &&
          type_is_prototyped_function(declarator->type)) {
        if (types_equivalent(exists->type, declarator->type, 0, 1)) {
          type_destroy(declarator->type);
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
          assert(exists->type->function.parameters == NULL);
          *(exists->type) = *(declarator->type);
          exists->type->function.next = exists_ret_type;
          exists->type->function.is_old_style = 1;
          declarator->type->function.parameters = NULL;
        }

        type_destroy(declarator->type);
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
  } else {
    Symbol *symbol = symbol_init(&declarator->loc);
    symbol->type = declarator->type;
    symbol->linkage = linkage;
    symbol->storage = determine_storage(declarator->type,
                                        !is_redeclaration && exists_equivalent);
    assert(symbol->storage != STORE_INVALID);
    state_insert_symbol(state, identifier, identifier_len, symbol);
    if (symbol_is_lvalue(symbol)) declarator->attributes |= ATTR_EXPR_LVAL;
    return declarator;
  }
}

ASTree *finalize_declaration(ASTree *declaration) {
  ASTree *errnode = NULL;
  if (declaration->tok_kind == TOK_TYPE_ERROR) {
    errnode = declaration;
    declaration = astree_get(declaration, 0);
  }

  /* free type specifier info now that declarators no longer need it */
  ASTree *decl_specs = astree_get(declaration, 0);
  type_destroy(decl_specs->type);
  decl_specs->type = NULL;
  return errnode != NULL ? errnode : declaration;
}

ASTree *validate_array_size(ASTree *array, ASTree *expr) {
  if (expr->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(array, expr);
  }
  if (!type_is_integral(expr->type) ||
      (expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_INT) {
    return astree_create_errnode(astree_adopt(array, 1, expr),
                                 BCC_TERR_EXPECTED_INTCONST, 2, array, expr);
  }
  if (astree_is_const_zero(expr)) {
    return astree_create_errnode(astree_adopt(array, 1, expr),
                                 BCC_TERR_EXPECTED_NONZERO, 2, array, expr);
  }
  return astree_adopt(array, 1, expr);
}

ASTree *validate_param_list(ASTree *param_list) {
  param_list->symbol_table = symbol_table_init(TABLE_FUNCTION);
  state_push_table(state, param_list->symbol_table);
  return param_list;
}

static void replace_param_dirdecl(ASTree *declarator) {
  assert(type_is_array(declarator->type) || type_is_function(declarator->type));
  Type *pointer_type = type_init_pointer(SPEC_FLAG_NONE);
  Type *stripped_type = type_strip_declarator(declarator->type);
  if (type_is_function(declarator->type))
    declarator->type->function.next = NULL;
  else
    declarator->type->array.next = NULL;
  type_destroy(declarator->type);
  (void)type_append(pointer_type, stripped_type, 0);
  declarator->type = pointer_type;
}

ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator) {
  declarator = validate_declaration(declaration, declarator);
  if (param_list->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(
        param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declaration->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(
        param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declarator->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(
        param_list, astree_propogate_errnode(declaration, declarator));
  }

  if (type_is_function(declarator->type) || type_is_array(declarator->type)) {
    replace_param_dirdecl(declarator);
    if (declarator->tok_kind != TOK_TYPE_NAME) {
      Symbol *param_symbol;
      int is_param =
          state_get_symbol(state, declarator->lexinfo,
                           strlen(declarator->lexinfo), &param_symbol);
#ifdef NDEBUG
      (void)is_param;
#endif
      assert(param_symbol != NULL);
      assert(is_param);
      param_symbol->type = declarator->type;
    }
  }

  return astree_adopt(
      param_list, 1,
      finalize_declaration(astree_adopt(declaration, 1, declarator)));
}

ASTree *finalize_param_list(ASTree *param_list, ASTree *ellipsis) {
  if (ellipsis != NULL)
    param_list->tok_kind == TOK_TYPE_ERROR
        ? (void)astree_propogate_errnode(param_list, ellipsis)
        : (void)astree_adopt(param_list, 1, ellipsis);
  state_pop_table(state);
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
  } else if (astree_get(param_list, 0)->tok_kind == TOK_VOID) {
    parameters = NULL, parameters_size = 0;
  } else {
    parameters = malloc(parameters_size * sizeof(Type *));
    if (astree_get(param_list, parameters_size - 1)->tok_kind == TOK_ELLIPSIS) {
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

  Type *function_type = type_init_function(parameters_size, parameters,
                                           is_variadic, is_old_style);
  if (declarator->type != NULL) {
    (void)type_append(declarator->type, function_type, 0);
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
  if (astree_count(array) == 0)
    array_type = type_init_array(0, 1);
  else if (type_is_unsigned(astree_get(array, 0)->type))
    array_type = type_init_array(
        astree_get(array, 0)->constant.integral.unsigned_value, 0);
  else
    array_type = type_init_array(
        astree_get(array, 0)->constant.integral.signed_value, 0);

  if (declarator->type != NULL) {
    (void)type_append(declarator->type, array_type, 0);
  } else {
    declarator->type = array_type;
  }
  return astree_adopt(declarator, 1, array);
}

ASTree *define_pointer(ASTree *declarator, ASTree *pointer) {
  if (declarator->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode(declarator, pointer);

  unsigned int qual_flags = QUAL_FLAG_NONE;
  size_t i;
  for (i = 0; i < astree_count(pointer); ++i) {
    ASTree *qualifier = astree_get(pointer, i);
    if (qual_flags & type_flag_from_tok_kind(qualifier->tok_kind)) {
      return astree_create_errnode(astree_adopt(declarator, 1, pointer),
                                   BCC_TERR_INCOMPATIBLE_SPEC, 2, declarator,
                                   qualifier);
    } else {
      qual_flags |= type_flag_from_tok_kind(qualifier->tok_kind);
    }
  }

  Type *pointer_type = type_init_pointer(qual_flags);
  if (declarator->type != NULL) {
    (void)type_append(declarator->type, pointer_type, 0);
  } else {
    declarator->type = pointer_type;
  }
  return astree_adopt(declarator, 1, pointer);
}

ASTree *define_dirdecl(ASTree *declarator, ASTree *dirdecl) {
  if (declarator->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode(declarator, dirdecl);

  switch (dirdecl->tok_kind) {
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
  if (decl_list->tok_kind == TOK_TYPE_ERROR) {
    ASTree *errnode = decl_list;
    decl_list = astree_get(errnode, 0);
    if (initializer->tok_kind == TOK_TYPE_ERROR) {
      (void)type_merge_errors(errnode->type, initializer->type);
      (void)astree_adopt(decl_list, 1,
                         astree_adopt(equal_sign, 2, declarator,
                                      astree_remove(initializer, 0)));
      astree_destroy(initializer);
      return errnode;
    } else {
      (void)astree_adopt(decl_list, 1,
                         astree_adopt(equal_sign, 2, declarator, initializer));
      return errnode;
    }
  } else if (initializer->tok_kind == TOK_TYPE_ERROR) {
    ASTree *errnode = initializer;
    return astree_adopt(errnode, 1,
                        astree_adopt(decl_list, 1,
                                     astree_adopt(equal_sign, 2, declarator,
                                                  astree_remove(errnode, 0))));
  }
  assert(declarator->tok_kind == TOK_IDENT);

  Symbol *symbol;
  (void)state_get_symbol(state, declarator->lexinfo,
                         strlen(declarator->lexinfo), &symbol);
  assert(symbol != NULL);
  if (symbol->defined)
    return astree_create_errnode(
        astree_adopt(decl_list, 1,
                     astree_adopt(equal_sign, 2, declarator, initializer)),
        BCC_TERR_REDEFINITION, 1, declarator);
  symbol->defined = 1;
  ;
  equal_sign->type = declarator->type;
  /* code is emitted by `validate_fnbody_content`, `validate_topdecl`, or
   * `validate_block_content` and type checking is performed in `init.c`
   */
  return astree_adopt(decl_list, 1,
                      astree_adopt(equal_sign, 2, declarator, initializer));
}

ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body) {
  declarator = validate_declaration(declaration, declarator);
  if (declaration->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(declaration, 2, declarator, body);
  } else if (declarator->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(declaration, 2, declarator, body);
  }

  Symbol *symbol;
  int in_current_scope = state_get_symbol(state, declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symbol);
#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(in_current_scope);
  assert(symbol != NULL);
  assert(type_is_function(symbol->type));
  if (symbol->defined)
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                                 BCC_TERR_REDEFINITION, 1, declarator);

  /* param list should be first child for properly defined functions */
  ASTree *param_list = astree_get(declarator, 0);
  assert(param_list->tok_kind == TOK_PARAM_LIST);
  size_t i;
  for (i = 0; i < astree_count(param_list); ++i) {
    ASTree *param = astree_get(param_list, i);
    if (param->tok_kind == TOK_TYPE_NAME) {
      return astree_create_errnode(
          astree_adopt(declaration, 2, declarator, body),
          BCC_TERR_EXPECTED_DECLARATOR, 2, declarator, param);
    }
  }

  /* treat body like a normal block statement, but move param table to body node
   * and define function before entering body scope */
  if (param_list->symbol_table == NULL) {
    body->symbol_table = symbol_table_init(TABLE_FUNCTION);
  } else {
    body->symbol_table = param_list->symbol_table;
    param_list->symbol_table = NULL;
  }
  state_push_table(state, body->symbol_table);

  assert(declarator->tok_kind == TOK_IDENT);
  state_set_function(state, declarator->lexinfo, symbol);
  return begin_translate_fn(declaration, declarator, body);
}

ASTree *validate_fnbody_content(ASTree *function, ASTree *fnbody_content) {
  /* we can't reuse validate_block_content here because all that function does
   * is perform adoption and propogate errors, both of which are different here
   * because of the tree structure of function definitions
   */
  ASTree *fnbody = astree_get(UNWRAP(function), 2);
  if (function->tok_kind == TOK_TYPE_ERROR) {
    if (fnbody_content->tok_kind == TOK_TYPE_ERROR) {
      (void)astree_adopt(fnbody, 1, astree_remove(fnbody_content, 0));
      (void)type_merge_errors(function->type, fnbody_content->type);
      astree_destroy(fnbody_content);
      return function;
    } else {
      (void)astree_adopt(fnbody, 1, fnbody_content);
      return function;
    }
  } else if (fnbody_content->tok_kind == TOK_TYPE_ERROR) {
    (void)astree_adopt(fnbody, 1, astree_remove(fnbody_content, 0));
    return astree_adopt(fnbody_content, 1, function);
  } else if (fnbody_content->tok_kind == TOK_DECLARATION) {
    ASTree *errnode = translate_local_declarations(fnbody, fnbody_content);
    if (errnode->tok_kind == TOK_TYPE_ERROR) {
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
    int status =
        alist_init(&label_strs, map_size(body->symbol_table->label_namespace));
    if (status) abort();
    status = map_keys(body->symbol_table->label_namespace, &label_strs);
    if (status) abort();
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
    status = alist_destroy(&label_strs, NULL);
    if (status) abort();
  }
  Symbol *symbol = state_get_function(state);
  symbol->defined = 1;
  state_unset_function(state);
  /* do not use finalize_block because it will put the error in an awkward place
   */
  state_pop_table(state);
  if (ret->tok_kind == TOK_TYPE_ERROR) return ret;
  return end_translate_fn(function);
}

static TagKind tag_from_tok_kind(int tok_kind) {
  switch (tok_kind) {
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

static int error_code_from_tok_kind(int tok_kind) {
  switch (tok_kind) {
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

ASTree *validate_unique_tag(ASTree *tag_spec, ASTree *left_brace) {
  const char *id_str = create_unique_name(tag_spec);
  const size_t id_str_len = strlen(id_str);
  TagKind kind = tag_from_tok_kind(tag_spec->tok_kind);
  Tag *tag = tag_init(kind);
  state_insert_tag(state, id_str, id_str_len, tag);
  tag_spec->type = type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, tag);

  if (kind == TAG_ENUM) {
    /* remove struct/union member tables from scope stack */
    SymbolTable *top_scope = state_peek_table(state);
    while (top_scope->kind == TABLE_MEMBER) {
      int status =
          llist_push_back(&tag->data.enumerators.struct_name_spaces, top_scope);
      if (status) abort();
      state_pop_table(state);
      top_scope = state_peek_table(state);
    }
    tag->width = 4;
    tag->alignment = 4;
  } else {
    state_push_table(state, tag->data.members.by_name);
  }
  return astree_adopt(tag_spec, 1, left_brace);
}

ASTree *validate_tag_decl(ASTree *tag_spec, ASTree *tag_id) {
  const char *id_str = tag_id->lexinfo;
  const size_t id_str_len = strlen(id_str);
  Tag *exists = NULL;
  int is_redeclaration = state_get_tag(state, id_str, id_str_len, &exists);
  TagKind kind = tag_from_tok_kind(tag_spec->tok_kind);

  if (exists) {
    if (is_redeclaration) {
      if (kind != exists->kind) {
        return astree_create_errnode(
            astree_adopt(tag_spec, 1, tag_id),
            error_code_from_tok_kind(tag_spec->tok_kind), 2, tag_spec, tag_id);
      } else {
        tag_spec->type =
            type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, exists);
        return astree_adopt(tag_spec, 1, tag_id);
      }
    } else {
      Tag *tag;
      if (kind != exists->kind) {
        tag = tag_init(kind);
        state_insert_tag(state, id_str, id_str_len, tag);
      } else {
        tag = exists;
      }

      tag_spec->type =
          type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, tag);
      return astree_adopt(tag_spec, 1, tag_id);
    }
  } else if (kind != TAG_ENUM) {
    Tag *tag = tag_init(kind);
    state_insert_tag(state, id_str, id_str_len, tag);
    tag_spec->type =
        type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, tag);
    return astree_adopt(tag_spec, 1, tag_id);
  } else {
    /* do not insert enum tag; enum must declare their constants */
    return astree_create_errnode(astree_adopt(tag_spec, 1, tag_id),
                                 BCC_TERR_TAG_NOT_FOUND, 1, tag_id);
  }
}

ASTree *validate_tag_def(ASTree *tag_spec, ASTree *tag_id, ASTree *left_brace) {
  const char *id_str = tag_id->lexinfo;
  const size_t id_str_len = strlen(id_str);
  Tag *exists = NULL;
  int is_redeclaration = state_get_tag(state, id_str, id_str_len, &exists);
  TagKind kind = tag_from_tok_kind(tag_spec->tok_kind);

  if (is_redeclaration && exists->is_defined) {
    return astree_create_errnode(astree_adopt(tag_spec, 2, tag_id, left_brace),
                                 BCC_TERR_REDEFINITION, 1, tag_id);
  } else {
    Tag *tag;
    if (exists && is_redeclaration) {
      tag = exists;
    } else {
      tag = tag_init(kind);
      if (tag == NULL) abort();
      state_insert_tag(state, id_str, id_str_len, tag);
    }

    tag_spec->type =
        type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, tag);

    if (kind == TAG_ENUM) {
      /* remove struct/union member tables from scope stack */
      SymbolTable *top_scope = state_peek_table(state);
      while (top_scope->kind == TABLE_MEMBER) {
        int status = llist_push_back(&tag->data.enumerators.struct_name_spaces,
                                     top_scope);
        if (status) abort();
        state_pop_table(state);
        top_scope = state_peek_table(state);
      }
      tag->width = 4;
      tag->alignment = 4;
    } else {
      state_push_table(state, tag->data.members.by_name);
    }
    return astree_adopt(tag_spec, 2, tag_id, left_brace);
  }
}

ASTree *finalize_tag_def(ASTree *tag_spec) {
  ASTree *errnode = NULL;
  if (tag_spec->tok_kind == TOK_TYPE_ERROR) {
    errnode = tag_spec;
    tag_spec = UNWRAP(tag_spec);
  }

  Tag *tag = tag_spec->type->tag.value;
  tag->is_defined = 1;

  if (tag->kind == TAG_ENUM) {
    /* push struct/union member tables back onto the scope stack */
    while (!llist_empty(&tag->data.enumerators.struct_name_spaces)) {
      SymbolTable *member_scope =
          llist_pop_back(&tag->data.enumerators.struct_name_spaces);
      assert(member_scope != NULL);
      state_push_table(state, member_scope);
    }
  } else {
    /* pop member scope from stack */
    if (state_peek_table(state) == tag->data.members.by_name)
      state_pop_table(state);
    /* pad aggregate so that it can tile an array */
    if (tag->alignment != 0) {
      size_t padding = tag->alignment - (tag->width % tag->alignment);
      if (padding != tag->alignment) tag->width += padding;
    }
  }
  return errnode == NULL ? tag_spec : errnode;
}

ASTree *define_enumerator(ASTree *enum_spec, ASTree *enum_id,
                          ASTree *equal_sign, ASTree *expr) {
  ASTree *left_brace = astree_get(UNWRAP(enum_spec),
                                  astree_count(UNWRAP(enum_spec)) == 2 ? 1 : 0);
  if (enum_spec->tok_kind == TOK_TYPE_ERROR) {
    /* ASTree *real_enum = astree_get(enum_spec, 0); */
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1, astree_adopt(equal_sign, 2, enum_id, expr));
      return enum_spec;
    } else {
      astree_adopt(left_brace, 1, enum_id);
      return enum_spec;
    }
  }
  const char *id_str = enum_id->lexinfo;
  const size_t id_str_len = strlen(id_str);

  Symbol *exists = NULL;
  int is_redefinition = state_get_symbol(state, id_str, id_str_len, &exists);
  if (is_redefinition) {
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1, astree_adopt(equal_sign, 2, enum_id, expr));
      return astree_create_errnode(enum_spec, BCC_TERR_REDEFINITION, 1,
                                   enum_id);
    } else {
      astree_adopt(left_brace, 1, enum_id);
      return astree_create_errnode(enum_spec, BCC_TERR_REDEFINITION, 1,
                                   enum_id);
    }
  }

  Symbol *symbol = symbol_init(&enum_id->loc);
  /* mark as enumeration consntant */
  symbol->linkage = LINK_ENUM_CONST;
  symbol->storage = STORE_ENUM_CONST;
  symbol->defined = 1;
  /* copy type info from tag node */
  symbol->type = type_copy(enum_spec->type, 0);

  state_insert_symbol(state, id_str, id_str_len, symbol);
  enum_id->type = symbol->type;

  Tag *tag = symbol->type->tag.value;
  if (equal_sign != NULL) {
    if (expr->tok_kind == TOK_TYPE_ERROR) {
      ASTree *errnode =
          astree_propogate_errnode(astree_adopt(equal_sign, 1, enum_id), expr);
      return astree_propogate_errnode(enum_spec, errnode);
    }
    if ((expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_INT) {
      astree_adopt(left_brace, 1, astree_adopt(equal_sign, 2, enum_id, expr));
      return astree_create_errnode(enum_spec, BCC_TERR_EXPECTED_ARITHCONST, 2,
                                   equal_sign, expr);
    }
    int *value = malloc(sizeof(int));
    if (type_is_unsigned(expr->type)) {
      *value = tag->data.enumerators.last_value =
          expr->constant.integral.unsigned_value;
    } else {
      *value = tag->data.enumerators.last_value =
          expr->constant.integral.signed_value;
    }
    int status = map_insert(&tag->data.enumerators.by_name, (char *)id_str,
                            id_str_len, value);
    if (status) abort();
    astree_adopt(left_brace, 1, astree_adopt(equal_sign, 2, enum_id, expr));
    return enum_spec;
  } else {
    int *value = malloc(sizeof(int));
    *value = ++tag->data.enumerators.last_value;
    int status = map_insert(&tag->data.enumerators.by_name, (char *)id_str,
                            id_str_len, value);
    if (status) abort();
    astree_adopt(left_brace, 1, enum_id);
    return enum_spec;
  }
}

/* TODO(Robert): report an error when a record with a member of function type
 * is declared and replace the check for function types here with an assert
 */
ASTree *define_record_member(ASTree *record_spec, ASTree *member) {
  (void)finalize_declaration(member);
  ASTree *left_brace = astree_get(
      UNWRAP(record_spec), astree_count(UNWRAP(record_spec)) == 2 ? 1 : 0);
  if (record_spec->tok_kind == TOK_TYPE_ERROR) {
    (void)astree_adopt(left_brace, 1, UNWRAP(member));
    if (member->tok_kind == TOK_TYPE_ERROR) {
      ASTree *errnode = member;
      (void)llist_extract(&member->children, 0);
      (void)type_merge_errors(record_spec->type, errnode->type);
      astree_destroy(errnode);
    }
    return record_spec;
  } else if (member->tok_kind == TOK_TYPE_ERROR) {
    (void)astree_adopt(left_brace, 1, llist_extract(&member->children, 0));
    return astree_adopt(member, 1, record_spec);
  }

  Tag *tag = record_spec->type->tag.value;
  LinkedList *member_list = &tag->data.members.in_order;
  size_t i;
  /* skip first child, which is the typespec list */
  for (i = 1; i < astree_count(member); ++i) {
    ASTree *declarator = astree_get(member, i);
    Symbol *symbol;
    int is_member = state_get_symbol(state, declarator->lexinfo,
                                     strlen(declarator->lexinfo), &symbol);
#ifdef NDEBUG
    (void)is_member;
#endif
    assert(is_member);
    assert(symbol != NULL);
    size_t member_alignment = type_get_alignment(declarator->type);
    size_t member_width = type_get_width(declarator->type);
    if (tag->alignment < member_alignment) {
      tag->alignment = member_alignment;
    }
    if (tag->kind == TAG_STRUCT) {
      size_t padding = member_alignment - (tag->width % member_alignment);
      if (padding != member_alignment) tag->width += padding;
      symbol->disp = tag->width;
      tag->width += member_width;
    } else if (tag->width < member_width) {
      tag->width = member_width;
    }
    llist_push_back(member_list, symbol);
  }
  astree_adopt(left_brace, 1, member);
  return record_spec;
}

ASTree *declare_symbol(ASTree *declaration, ASTree *declarator) {
  ASTree *err_or_decl = validate_declaration(declaration, declarator);
  if (err_or_decl->tok_kind == TOK_TYPE_ERROR ||
      declaration->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode(declaration, err_or_decl);
  else
    return astree_adopt(declaration, 1, declarator);
}

ASTree *validate_topdecl(ASTree *root, ASTree *topdecl) {
  if (root->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else if (topdecl->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else {
    return translate_global_declarations(root, topdecl);
  }
}
