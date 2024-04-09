#include "tchk_decl.h"

#include "asmgen.h"
#include "assert.h"
#include "badalist.h"
#include "bcc_err.h"
#include "conversions.h"
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
  if (type_add_flags(tag_spec->type, type_get_flags(decl_specs->type))) {
    (void)semerr_incompatible_spec(decl_specs, tag_spec);
    return astree_adopt(decl_specs, 1, tag_spec);
  }

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
    (void)semerr_symbol_not_found(typedef_name);
  } else if (symbol->linkage != LINK_TYPEDEF) {
    (void)semerr_expected_typedef_name(typedef_name, symbol);
  } else if (!type_is_none(decl_specs->type) ||
             (type_is_declarator(symbol->type) &&
              !type_is_pointer(symbol->type) &&
              type_is_qualified(decl_specs->type))) {
    (void)semerr_incompatible_spec(decl_specs, typedef_name);
  } else {
    unsigned int old_flags = type_get_flags(decl_specs->type);
    type_destroy(decl_specs->type);
    decl_specs->type = type_copy(symbol->type, 1);

    if (type_is_pointer(decl_specs->type)) {
      /* apply qualifiers to the pointer */
      if (type_add_flags(decl_specs->type, old_flags & QUAL_FLAG_MASK)) {
        (void)semerr_incompatible_spec(decl_specs, typedef_name);
        return astree_adopt(decl_specs, 1, typedef_name);
      } else {
        old_flags &= ~QUAL_FLAG_MASK;
      }
    }

    if (type_add_flags(type_get_decl_specs(decl_specs->type), old_flags))
      (void)semerr_incompatible_spec(decl_specs, typedef_name);
  }

  return astree_adopt(decl_specs, 1, typedef_name);
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
    (void)semerr_incompatible_spec(decl_specs, decl_spec);
    return astree_adopt(decl_specs, 1, decl_spec);
  } else {
    return astree_adopt(decl_specs, 1, decl_spec);
  }
}

ASTree *finalize_decl_specs(ASTree *decl_specs) {
  if (type_normalize(decl_specs->type)) (void)semerr_invalid_type(decl_specs);
  return decl_specs;
}

static int location_is_empty(Location *loc) {
  return loc->filenr == 0 && loc->linenr == 0 && loc->offset == 0;
}

static int set_symbol_qualifiers(const char *ident, size_t ident_len,
                                 Symbol *symbol) {
  const SymbolTable *top_scope = state_peek_table(state);
  unsigned int decl_flags = type_get_flags(type_get_decl_specs(symbol->type));

  if (top_scope->kind == TABLE_MEMBER) {
    if ((decl_flags & STOR_FLAG_MASK) != STOR_FLAG_NONE) {
      return -1;
    } else {
      symbol->linkage = LINK_MEMBER;
      symbol->storage = STORE_MEMBER;
      return 0;
    }
  } else if (top_scope->kind == TABLE_TRANS_UNIT) {
    switch (decl_flags & STOR_FLAG_MASK) {
      case STOR_FLAG_AUTO:
        /* fallthrough */
      case STOR_FLAG_REGISTER:
        return -1;
      case STOR_FLAG_EXTERN:
        symbol->linkage = LINK_EXT;
        symbol->storage = STORE_EXT;
        return 0;
      case STOR_FLAG_NONE:
        symbol->linkage = LINK_EXT;
        if (type_is_function(symbol->type))
          symbol->storage = STORE_EXT;
        else
          symbol->storage = STORE_STAT;
        return 0;
      case STOR_FLAG_STATIC:
        symbol->linkage = LINK_INT;
        symbol->storage = STORE_STAT;
        return 0;
      case STOR_FLAG_TYPEDEF:
        symbol->linkage = LINK_TYPEDEF;
        symbol->storage = STORE_TYPEDEF;
        return 0;
      default:
        abort();
    }
  } else {
    switch (decl_flags & STOR_FLAG_MASK) {
      int heritable;
      case STOR_FLAG_AUTO:
        /* fallthrough */
      case STOR_FLAG_REGISTER:
        if (type_is_function(symbol->type)) {
          return -1;
        } else {
          symbol->linkage = LINK_NONE;
          symbol->storage = STORE_AUTO;
          return 0;
        }
      case STOR_FLAG_STATIC:
        if (type_is_function(symbol->type)) {
          return -1;
        } else {
          symbol->linkage = LINK_NONE;
          symbol->storage = STORE_STAT;
          return 0;
        }
      case STOR_FLAG_NONE:
        if (!type_is_function(symbol->type)) {
          symbol->linkage = LINK_NONE;
          symbol->storage = STORE_AUTO;
          return 0;
        }
        /* fallthrough */
      case STOR_FLAG_EXTERN:
        heritable = state_inheritance_valid(state, ident, ident_len, symbol);
        if (heritable) {
          /* call above should set linkage and storage class */
          return 0;
        } else {
          return -1;
        }
      case STOR_FLAG_TYPEDEF:
        symbol->linkage = LINK_TYPEDEF;
        symbol->storage = STORE_TYPEDEF;
        return 0;
      default:
        abort();
    }
  }
}

static int linkage_valid(const Symbol *existing, Linkage linkage) {
  assert(existing != NULL);
  if (existing->linkage == LINK_TYPEDEF || linkage == LINK_TYPEDEF) {
    /* typedef redefinition */
    return 0;
  } else if (existing->linkage == LINK_ENUM_CONST ||
             linkage == LINK_ENUM_CONST) {
    /* enumeration constant redefinition */
    return 0;
  } else if (existing->linkage == LINK_MEMBER || linkage == LINK_MEMBER) {
    /* record member redeclaration */
    return 0;
  } else if (existing->linkage == LINK_NONE || linkage == LINK_NONE) {
    /* redeclaration of symbol at block scope */
    return 0;
  } else if (existing->linkage == LINK_EXT && linkage == LINK_EXT) {
    /* multiple declarations of a symbol are allowed with the same linkage */
    return 1;
  } else if (existing->linkage != LINK_INT) {
    /* declaration of symbol with internal linkage after previous declaration
     * with external linkage
     */
    return 0;
  } else if (linkage == LINK_INT || linkage == LINK_EXT) {
    /* multiple declarations of a symbol are allowed with the same linkage,
     * and a symbol initially declared with internal linkage may subsequently
     * be declared with external linkage, though the linkage will not change
     */
    return 1;
  } else {
    /* everything else; not sure if this is necessary */
    return 0;
  }
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
static Symbol *validate_declaration(ASTree *declaration, ASTree *declarator) {
  PFDBG1('t', "Making object entry for value %s", declarator->lexinfo);
  if (location_is_empty(&declaration->loc)) declaration->loc = declarator->loc;

  ASTree *decl_specs = astree_get(declaration, 0);
  if (declarator->type == NULL) {
    declarator->type = type_copy(decl_specs->type, 0);
  } else {
    (void)type_append(declarator->type, decl_specs->type, 1);
  }

  if (declarator->tok_kind == TOK_TYPE_NAME) return NULL;

  /* TODO(Robert): why don't we do this for type names? */
  if (type_validate(declarator->type)) {
    (void)semerr_invalid_type(declarator);
    type_destroy(declarator->type);
    return NULL;
  }

  const char *identifier = declarator->tok_kind == TOK_TYPE_NAME
                               ? create_unique_name(declarator)
                               : declarator->lexinfo;
  size_t identifier_len = strlen(identifier);
  Symbol *exists = NULL;
  int is_redeclaration =
      state_get_symbol(state, identifier, identifier_len, &exists);
  Symbol *symbol = symbol_init(&declarator->loc);
  symbol->type = declarator->type;

  if (set_symbol_qualifiers(identifier, identifier_len, symbol)) {
    (void)semerr_invalid_linkage(declarator, symbol);
    symbol_destroy(symbol);
    return NULL;
  } else if (!is_redeclaration) {
    state_insert_symbol(state, identifier, identifier_len, symbol);
    if (symbol_is_lvalue(symbol)) declarator->attributes |= ATTR_EXPR_LVAL;
    /* reassign type information in case it was replaced */
    if (symbol->info == SYM_INHERITOR) declarator->type = symbol->type;
    return symbol;
  } else if (!linkage_valid(exists, symbol->linkage) ||
             !(types_equivalent(exists->type, declarator->type, 0, 1) ||
               type_complete_array(exists->type, declarator->type) ||
               type_prototype_function(exists->type, declarator->type))) {
    (void)semerr_incompatible_linkage(declarator, exists, symbol);
    symbol_destroy(symbol);
    return NULL;
  } else {
    if (exists->info == SYM_HIDDEN) {
      assert(exists->linkage == LINK_EXT && symbol->linkage == LINK_EXT);
      exists->storage = symbol->storage;
      exists->info = SYM_NONE;
    }
    if (exists->storage == STORE_EXT && symbol->storage == STORE_STAT)
      exists->storage = STORE_STAT;
    symbol_destroy(symbol);
    declarator->type = exists->type;
    return exists;
  }
}

ASTree *finalize_declaration(ASTree *declaration) {
  /* free type specifier info now that declarators no longer need it */
  ASTree *decl_specs = astree_get(declaration, 0);
  type_destroy(decl_specs->type);
  decl_specs->type = NULL;
  return declaration;
}

ASTree *validate_array_size(ASTree *array, ASTree *expr) {
  if (!type_is_integral(expr->type) ||
      (expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_INT ||
      astree_is_const_zero(expr))
    (void)semerr_invalid_arr_size(array, expr);
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
  Symbol *param_symbol = validate_declaration(declaration, declarator);
  /* we cannot replace dirdecls until after `validate_declaration`, since that
   * function may introduce a function or array type through a typedef
   */
  if ((type_is_function(declarator->type) || type_is_array(declarator->type)))
    replace_param_dirdecl(declarator);

  if (param_symbol == NULL)
    return astree_adopt(
        param_list, 1,
        finalize_declaration(astree_adopt(declaration, 1, declarator)));

  /* assign in case dirdecl was replaced */
  param_symbol->type = declarator->type;
  param_symbol->info = SYM_DEFINED;

  return astree_adopt(
      param_list, 1,
      finalize_declaration(astree_adopt(declaration, 1, declarator)));
}

ASTree *finalize_param_list(ASTree *param_list, ASTree *ellipsis) {
  state_pop_table(state);
  return ellipsis == NULL ? param_list : astree_adopt(param_list, 1, ellipsis);
}

ASTree *define_params(ASTree *declarator, ASTree *param_list) {
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
  unsigned int qual_flags = QUAL_FLAG_NONE;
  size_t i;
  for (i = 0; i < astree_count(pointer); ++i) {
    ASTree *qualifier = astree_get(pointer, i);
    if (qual_flags & type_flag_from_tok_kind(qualifier->tok_kind)) {
      (void)semerr_incompatible_spec(pointer, qualifier);
      return astree_adopt(declarator, 1, pointer);
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
  switch (dirdecl->tok_kind) {
    case TOK_ARRAY:
      return define_array(declarator, dirdecl);
    case TOK_PARAM_LIST:
      return define_params(declarator, dirdecl);
    default:
      abort();
  }
}

ASTree *define_symbol(ASTree *decl_list, ASTree *equal_sign,
                      ASTree *initializer) {
  ASTree *declarator = astree_remove(decl_list, astree_count(decl_list) - 1);
  assert(declarator->tok_kind == TOK_IDENT);

  if (initializer->tok_kind != TOK_INIT_LIST &&
      (!type_is_char_array(declarator->type) ||
       initializer->tok_kind != TOK_STRINGCON))
    initializer = tchk_ptr_conv(initializer, 1);

  Symbol *symbol;
  (void)state_get_symbol(state, declarator->lexinfo,
                         strlen(declarator->lexinfo), &symbol);
  assert(symbol != NULL);
  assert(symbol->type = declarator->type);
  if (symbol->info == SYM_DEFINED) {
    (void)semerr_redefine_symbol(declarator, symbol);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (symbol->storage == STORE_EXT) {
    (void)semerr_define_extern(declarator);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (initializer->tok_kind == TOK_INIT_LIST ||
             initializer->tok_kind == TOK_STRINGCON) {
    /*
     * nothing to be done; let `init.c` type check
     */
  } else if (symbol->storage == STORE_STAT &&
             (initializer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    (void)semerr_expected_init(initializer);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (!types_assignable(declarator->type, initializer->type,
                               astree_is_const_zero(initializer))) {
    (void)semerr_incompatible_types(equal_sign, declarator->type,
                                    initializer->type);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (symbol->storage == STORE_AUTO) {
    initializer = tchk_cexpr_conv(
        tchk_scal_conv(tchk_rval_conv(initializer, NULL), declarator->type),
        NULL);
  } else {
    initializer = tchk_scal_conv(initializer, declarator->type);
  }

  assert(symbol->info == SYM_NONE);
  symbol->info = SYM_DEFINED;
  equal_sign->type = declarator->type;

  if (symbol->linkage == LINK_NONE || symbol->info == SYM_INHERITOR)
    return translate_local_init(decl_list, equal_sign, declarator, initializer);
  else
    return translate_global_init(decl_list, equal_sign, declarator,
                                 initializer);
}

ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body) {
  Symbol *symbol = validate_declaration(declaration, declarator);

  if (symbol == NULL) return astree_adopt(declaration, 2, declarator, body);

  assert(type_is_function(symbol->type));
  assert(symbol->type == declarator->type);

  if (symbol->info == SYM_DEFINED) {
    (void)semerr_redefine_symbol(declarator, symbol);
    return astree_adopt(declaration, 2, declarator, body);
  }

  symbol->storage = STORE_STAT;
  assert(symbol->info != SYM_INHERITOR);
  symbol->info = SYM_NONE;

  /* param list should be first child for properly defined functions */
  ASTree *param_list = astree_get(declarator, 0);
  assert(param_list->tok_kind == TOK_PARAM_LIST);
  size_t i;
  for (i = 0; i < astree_count(param_list); ++i) {
    ASTree *param = astree_get(param_list, i);
    if (param->tok_kind == TOK_TYPE_NAME) {
      (void)semerr_expected_ident(declarator, param);
      return astree_adopt(declaration, 2, declarator, body);
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
  if (fnbody_content->tok_kind == TOK_DECLARATION)
    end_local_decls(fnbody_content);

  ASTree *fnbody = astree_get(function, 2);
  (void)astree_adopt(fnbody, 1, fnbody_content);
  return function;
}

ASTree *finalize_function(ASTree *function) {
  ASTree *body = astree_get(function, 2);
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
      if (!labval->is_defined) (void)semerr_label_not_found(labval->tree);
    }
    status = alist_destroy(&label_strs, NULL);
    if (status) abort();
  }
  Symbol *symbol = state_get_function(state);
  assert(symbol->info == SYM_NONE);
  symbol->info = SYM_DEFINED;
  state_unset_function(state);
  /* do not use finalize_block because it will put the error in an awkward place
   */
  state_pop_table(state);
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
        (void)semerr_redefine_tag(tag_spec, tag_id, exists);
        return astree_adopt(tag_spec, 1, tag_id);
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
    (void)semerr_enum_not_found(tag_spec, tag_id);
    return astree_adopt(tag_spec, 1, tag_id);
  }
}

ASTree *validate_tag_def(ASTree *tag_spec, ASTree *tag_id, ASTree *left_brace) {
  const char *id_str = tag_id->lexinfo;
  const size_t id_str_len = strlen(id_str);
  Tag *exists = NULL;
  int is_redeclaration = state_get_tag(state, id_str, id_str_len, &exists);
  TagKind kind = tag_from_tok_kind(tag_spec->tok_kind);

  if (is_redeclaration && exists->is_defined) {
    (void)semerr_redefine_tag(tag_spec, tag_id, exists);
    return astree_adopt(tag_spec, 2, tag_id, left_brace);
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

  return tag_spec;
}

ASTree *define_enumerator(ASTree *enum_spec, ASTree *enum_id,
                          ASTree *equal_sign, ASTree *expr) {
  ASTree *left_brace =
      astree_get(enum_spec, astree_count(enum_spec) == 2 ? 1 : 0);
  const char *id_str = enum_id->lexinfo;
  const size_t id_str_len = strlen(id_str);

  Symbol *exists = NULL;
  int is_redefinition = state_get_symbol(state, id_str, id_str_len, &exists);
  if (is_redefinition) {
    (void)semerr_redefine_symbol(enum_id, exists);
    if (equal_sign != NULL) {
      return astree_adopt(left_brace, 1,
                          astree_adopt(equal_sign, 2, enum_id, expr));
    } else {
      return astree_adopt(left_brace, 1, enum_id);
    }
  }

  Symbol *symbol = symbol_init(&enum_id->loc);
  /* mark as enumeration consntant */
  symbol->linkage = LINK_ENUM_CONST;
  symbol->storage = STORE_ENUM_CONST;
  symbol->info = SYM_DEFINED;
  /* copy type info from tag node */
  symbol->type = type_copy(enum_spec->type, 0);

  state_insert_symbol(state, id_str, id_str_len, symbol);
  enum_id->type = symbol->type;

  Tag *tag = symbol->type->tag.value;
  if (equal_sign != NULL) {
    if ((expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_INT) {
      (void)semerr_expected_const(equal_sign, expr);
      return astree_adopt(left_brace, 1,
                          astree_adopt(equal_sign, 2, enum_id, expr));
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
  ASTree *left_brace =
      astree_get(record_spec, astree_count(record_spec) == 2 ? 1 : 0);

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
  Symbol *symbol = validate_declaration(declaration, declarator);
  if (symbol == NULL) return astree_adopt(declaration, 1, declarator);
  assert(symbol->type = declarator->type);

  if (symbol->linkage == LINK_NONE || symbol->info == SYM_INHERITOR)
    return translate_local_decl(declaration, declarator);
  else
    return translate_global_decl(declaration, declarator);
}

ASTree *validate_topdecl(ASTree *root, ASTree *topdecl) {
  end_global_decls(topdecl);
  return astree_adopt(root, 1, topdecl);
}
