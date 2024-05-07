#include "tchk_decl.h"

#include "asmgen.h"
#include "assert.h"
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
  Symbol *symbol = NULL;
  state_get_symbol(state, type_name, &symbol);
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

static int set_symbol_qualifiers(const char *ident, Symbol *symbol) {
  ScopeKind kind = scope_get_kind(state_peek_scope(state));
  unsigned int decl_flags = type_get_flags(type_get_decl_specs(symbol->type));

  if (kind == SCOPE_MEMBER) {
    if ((decl_flags & STOR_FLAG_MASK) != STOR_FLAG_NONE) {
      return -1;
    } else {
      symbol->linkage = LINK_MEMBER;
      symbol->storage = STORE_MEMBER;
      return 0;
    }
  } else if (kind == SCOPE_FILE) {
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
        heritable = state_inheritance_valid(state, ident, symbol);
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
  Symbol *exists = NULL;
  int is_redeclaration = scope_get_kind(state_peek_scope(state)) == SCOPE_MEMBER
                             ? state_get_member(state, identifier, &exists)
                             : state_get_symbol(state, identifier, &exists);
  Symbol *symbol = symbol_init(&declarator->loc);
  symbol->type = declarator->type;

  if (set_symbol_qualifiers(identifier, symbol)) {
    (void)semerr_invalid_linkage(declarator, symbol);
    symbol_destroy(symbol);
    return NULL;
  } else if (symbol->storage != STORE_TYPEDEF && type_is_void(symbol->type)) {
    (void)semerr_incomplete_type(declarator, symbol->type);
    symbol_destroy(symbol);
    return NULL;
  } else if (!is_redeclaration) {
    if (symbol->linkage == LINK_MEMBER)
      state_insert_member(state, identifier, symbol);
    else
      state_insert_symbol(state, identifier, symbol);
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
  param_list->scope = scope_init(SCOPE_FUNCTION);
  state_enter_prototype(state, param_list);
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

ASTree *finalize_param_list(ASTree *param_list, ASTree *optional) {
  state_leave_prototype(state);
  return optional == NULL ? param_list : astree_adopt(param_list, 1, optional);
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
  ASTree *declarator = astree_disown(decl_list);
  assert(declarator->tok_kind == TOK_IDENT);

  if (initializer->tok_kind != TOK_INIT_LIST &&
      (!type_is_char_array(declarator->type) ||
       initializer->tok_kind != TOK_STRINGCON))
    initializer = tchk_ptr_conv(initializer, 1);

  Symbol *symbol;
  (void)state_get_symbol(state, declarator->lexinfo, &symbol);
  assert(symbol != NULL);
  assert(symbol->type = declarator->type);
  assert(symbol->linkage != LINK_MEMBER);
  if (symbol->info == SYM_DEFINED) {
    (void)semerr_redefine_symbol(declarator, symbol);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (symbol->storage == STORE_EXT) {
    (void)semerr_define_extern(declarator);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (type_is_incomplete(symbol->type) &&
             !type_is_deduced_array(symbol->type)) {
    (void)semerr_not_assignable(equal_sign, symbol->type);
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
        tchk_scal_conv(tchk_rval_conv(initializer), declarator->type));
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
  assert(declarator->tok_kind == TOK_IDENT);
  Symbol *symbol = validate_declaration(declaration, declarator);

  if (symbol == NULL) return astree_adopt(declaration, 2, declarator, body);

  assert(type_is_function(symbol->type));
  assert(symbol->type == declarator->type);

  Type *return_type = type_strip_declarator(declarator->type);
  if (type_is_incomplete(return_type) && !type_is_void(return_type)) {
    (void)semerr_incomplete_type(declarator, return_type);
    return astree_adopt(declaration, 2, declarator, body);
  } else if (astree_count(declarator) == 0 ||
             astree_get(declarator, 0)->tok_kind != TOK_PARAM_LIST) {
    /* achieved function type through typedef name; not allowed */
    (void)semerr_fndef_typedef(declarator);
    return astree_adopt(declaration, 2, declarator, body);
  } else if (symbol->info == SYM_DEFINED) {
    (void)semerr_redefine_symbol(declarator, symbol);
    return astree_adopt(declaration, 2, declarator, body);
  }

  symbol->storage = STORE_STAT;
  assert(symbol->info != SYM_INHERITOR);
  symbol->info = SYM_NONE;

  ASTree *param_list = astree_get(declarator, 0);
  size_t i, param_count = type_param_count(declarator->type);
  assert(astree_count(param_list) == param_count ||
         (type_is_variadic_function(declarator->type) &&
          astree_count(param_list) - 1 == param_count) ||
         (astree_count(param_list) == 1 &&
          astree_get(param_list, 0)->tok_kind == TOK_VOID && param_count == 0));
  for (i = 0; i < param_count; ++i) {
    ASTree *param_declarator = astree_get(astree_get(param_list, i), 1);
    if (param_declarator->tok_kind == TOK_TYPE_NAME) {
      (void)semerr_expected_ident(declarator, param_declarator);
      return astree_adopt(declaration, 2, declarator, body);
    } else if (type_is_incomplete(param_declarator->type)) {
      (void)semerr_incomplete_type(declarator, param_declarator->type);
      return astree_adopt(declaration, 2, declarator, body);
    }
  }

  assert(param_list->scope != NULL);
  /* treat body like a normal block statement, but move param table to body node
   * and define function before entering body scope */
  body->scope = param_list->scope;
  param_list->scope = NULL;
  state_enter_function(state, declarator, body);
  return begin_translate_fn(declaration, declarator, body);
}

ASTree *validate_fnbody_content(ASTree *function, ASTree *content) {
  ASTree *body = astree_get(function, 2);
  /* this function perfoms adoption */
  (void)translate_block_content(body, content);
  return function;
}

ASTree *finalize_function(ASTree *function) {
  ASTree *body = astree_get(function, 2);
  assert(body->scope != NULL);
  assert(scope_get_kind(body->scope) == SCOPE_FUNCTION);

  size_t label_index, label_count = scope_label_count(body->scope);
  for (label_index = 0; label_index < label_count; ++label_index) {
    Label *label;
    scope_label_at(body->scope, label_index, NULL, &label);
    if (!label->defined) (void)semerr_label_not_found(label->tree);
  }

  Symbol *symbol = state_get_function(state);
  assert(symbol->info == SYM_NONE);
  symbol->info = SYM_DEFINED;
  state_leave_function(state);
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
  TagKind kind = tag_from_tok_kind(tag_spec->tok_kind);
  Tag *tag = tag_init(kind);
  state_insert_tag(state, id_str, tag);
  tag_spec->type = type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, tag);

  if (kind == TAG_STRUCT || kind == TAG_UNION) state_enter_record(state, tag);
  return astree_adopt(tag_spec, 1, left_brace);
}

ASTree *validate_tag_decl(ASTree *tag_spec, ASTree *tag_id) {
  const char *id_str = tag_id->lexinfo;
  Tag *exists = NULL;
  int is_redeclaration = state_get_tag(state, id_str, &exists);
  TagKind kind = tag_from_tok_kind(tag_spec->tok_kind);

  if (exists) {
    if (is_redeclaration) {
      if (kind != exists->record.kind) {
        (void)semerr_redefine_tag(tag_spec, tag_id, exists);
        return astree_adopt(tag_spec, 1, tag_id);
      } else {
        tag_spec->type =
            type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, exists);
        return astree_adopt(tag_spec, 1, tag_id);
      }
    } else {
      Tag *tag;
      if (kind != exists->record.kind) {
        tag = tag_init(kind);
        state_insert_tag(state, id_str, tag);
      } else {
        tag = exists;
      }

      tag_spec->type =
          type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, tag);
      return astree_adopt(tag_spec, 1, tag_id);
    }
  } else if (kind != TAG_ENUM) {
    Tag *tag = tag_init(kind);
    state_insert_tag(state, id_str, tag);
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
  Tag *exists = NULL;
  int is_redeclaration = state_get_tag(state, id_str, &exists);
  TagKind kind = tag_from_tok_kind(tag_spec->tok_kind);

  if (is_redeclaration && exists->record.defined) {
    (void)semerr_redefine_tag(tag_spec, tag_id, exists);
    return astree_adopt(tag_spec, 2, tag_id, left_brace);
  } else {
    Tag *tag;
    if (exists && is_redeclaration) {
      tag = exists;
    } else {
      tag = tag_init(kind);
      if (tag == NULL) abort();
      state_insert_tag(state, id_str, tag);
    }

    tag_spec->type =
        type_init_tag(QUAL_FLAG_NONE | STOR_FLAG_NONE, id_str, tag);

    if (kind == TAG_STRUCT || kind == TAG_UNION) state_enter_record(state, tag);
    return astree_adopt(tag_spec, 2, tag_id, left_brace);
  }
}

ASTree *finalize_tag_def(ASTree *tag_spec) {
  Tag *tag = tag_spec->type->tag.value;
  tag->record.defined = 1;

  if (tag->record.kind == TAG_STRUCT || tag->record.kind == TAG_UNION) {
    /* pop member scope from stack */
    if (state_peek_scope(state) == tag->record.members)
      state_leave_record(state);
    /* pad aggregate so that it can tile an array */
    if (tag->record.alignment != 0) {
      size_t padding =
          tag->record.alignment - (tag->record.width % tag->record.alignment);
      if (padding != tag->record.alignment) tag->record.width += padding;
    }
  }

  return tag_spec;
}

ASTree *define_enumerator(ASTree *enum_spec, ASTree *enum_id,
                          ASTree *equal_sign, ASTree *expr) {
  ASTree *left_brace =
      astree_get(enum_spec, astree_count(enum_spec) == 2 ? 1 : 0);
  if (equal_sign == NULL)
    (void)astree_adopt(left_brace, 1, enum_id);
  else
    (void)astree_adopt(left_brace, 1,
                       astree_adopt(equal_sign, 2, enum_id, expr));
  const char *id_str = enum_id->lexinfo;

  Symbol *exists = NULL;
  int is_redefinition = state_get_symbol(state, id_str, &exists);
  if (is_redefinition) {
    (void)semerr_redefine_symbol(enum_id, exists);
    return enum_spec;
  }

  Symbol *symbol = symbol_init(&enum_id->loc);
  /* mark as enumeration consntant */
  symbol->linkage = LINK_ENUM_CONST;
  symbol->storage = STORE_ENUM_CONST;
  symbol->info = SYM_DEFINED;
  /* copy type info from tag node */
  symbol->type = type_copy(enum_spec->type, 0);

  state_insert_symbol(state, id_str, symbol);
  enum_id->type = symbol->type;

  Tag *tag = symbol->type->tag.value;
  if (equal_sign == NULL) {
    tag_add_constant(tag, id_str, tag_last_constant(tag) + 1);
  } else if ((expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_INT) {
    (void)semerr_expected_const(equal_sign, expr);
  } else if (type_is_unsigned(expr->type)) {
    tag_add_constant(tag, id_str, expr->constant.integral.unsigned_value);
  } else {
    tag_add_constant(tag, id_str, expr->constant.integral.unsigned_value);
  }

  return enum_spec;
}

ASTree *define_record_member(ASTree *record_spec, ASTree *member) {
  (void)finalize_declaration(member);
  ASTree *left_brace =
      astree_get(record_spec, astree_count(record_spec) == 2 ? 1 : 0);

  Tag *tag = record_spec->type->tag.value;
  size_t i;
  /* skip first child, which is the typespec list */
  for (i = 1; i < astree_count(member); ++i) {
    ASTree *declarator = astree_get(member, i);
    Symbol *symbol;
    int is_member = state_get_member(state, declarator->lexinfo, &symbol);
#ifdef NDEBUG
    (void)is_member;
#endif
    assert(is_member);
    assert(symbol != NULL);

    if (type_is_incomplete(symbol->type)) {
      (void)semerr_incomplete_type(declarator, symbol->type);
      (void)astree_adopt(left_brace, 1, member);
      return record_spec;
    } else if (type_is_function(symbol->type)) {
      (void)semerr_fn_member(declarator);
      (void)astree_adopt(left_brace, 1, member);
      return record_spec;
    }

    size_t member_alignment = type_get_alignment(declarator->type);
    size_t member_width = type_get_width(declarator->type);
    if (tag->record.alignment < member_alignment) {
      tag->record.alignment = member_alignment;
    }
    if (tag->record.kind == TAG_STRUCT) {
      size_t padding =
          member_alignment - (tag->record.width % member_alignment);
      if (padding != member_alignment) tag->record.width += padding;
      symbol->disp = tag->record.width;
      tag->record.width += member_width;
    } else if (tag->record.width < member_width) {
      tag->record.width = member_width;
    }
  }
  astree_adopt(left_brace, 1, member);
  return record_spec;
}

/* insert symbol into table in case it appears in its own initializer */
ASTree *prepare_init(ASTree *declaration, ASTree *declarator) {
  Symbol *symbol = validate_declaration(declaration, declarator);
  /* emit a `nop` in case we just validated a type name */
  if (symbol == NULL)
    return scope_get_kind(state_peek_scope(state)) == SCOPE_FILE
               ? astree_adopt(declaration, 1, declarator)
               : astree_adopt(declaration, 1, translate_empty_expr(declarator));
  assert(symbol->type = declarator->type);

  return astree_adopt(declaration, 1, declarator);
}

ASTree *declare_symbol(ASTree *declaration, ASTree *declarator) {
  Symbol *symbol = validate_declaration(declaration, declarator);
  /* emit a `nop` in case we just validated a type name */
  if (symbol == NULL) {
    return scope_get_kind(state_peek_scope(state)) == SCOPE_FILE
               ? astree_adopt(declaration, 1, declarator)
               : astree_adopt(declaration, 1, translate_empty_expr(declarator));
  } else if (type_is_incomplete(symbol->type) &&
             symbol->storage != STORE_TYPEDEF && symbol->linkage == LINK_NONE) {
    /* local object declaration with incomplete type */
    (void)semerr_incomplete_type(declarator, symbol->type);
    assert(scope_get_kind(state_peek_scope(state)) != SCOPE_FILE);
    return astree_adopt(declaration, 1, translate_empty_expr(declarator));
  }
  assert(symbol->type = declarator->type);

  if (symbol->linkage == LINK_NONE || symbol->info == SYM_INHERITOR)
    return translate_local_decl(declaration, declarator);
  else
    return translate_global_decl(declaration, declarator);
}

ASTree *validate_topdecl(ASTree *unit, ASTree *topdecl) {
  return translate_topdecl(unit, topdecl);
}
