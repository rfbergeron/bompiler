#include "tchk_decl.h"
#include "yyparse.h"
#include "state.h"
#include "stdlib.h"
#include "tchk_common.h"
#include "assert.h"

int spec_list_includes_type(ASTree *spec_list) {
  size_t i;
  for (i = 0; i < astree_count(spec_list); ++i) {
    ASTree *decl_spec = astree_get(spec_list, i);
    switch (decl_spec->symbol) {
      case TOK_INT:
      case TOK_SHORT:
      case TOK_LONG:
      case TOK_CHAR:
      case TOK_STRUCT:
      case TOK_UNION:
      case TOK_ENUM:
      case TOK_SIGNED:
      case TOK_UNSIGNED:
      case TOK_VOID:
        return 1;
      default:
        continue;
    }
  }
  return 0;
}

SymbolValue *sym_from_type(TypeSpec *type) {
  SymbolValue temp;
  /* struct members have addresses increasing in order of declarations */
  ptrdiff_t offset = (char *)&temp.type - (char *)&temp;
  return (SymbolValue *)((char *)type - offset);
}

ASTree *validate_integer_typespec(ASTree *spec_list, enum typespec_index i,
                                  enum typespec_flag f, size_t bytes) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & INCOMPATIBLE_FLAGSETS[i]) {
    return astree_create_errnode(spec_list, BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                       astree_get(spec_list, astree_count(spec_list) - 1));
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
    return spec_list;
  }
}

BaseType type_from_tag(TagType tag) {
  switch (tag) {
    case TAG_STRUCT:
      return TYPE_STRUCT;
    case TAG_UNION:
      return TYPE_UNION;
    case TAG_ENUM:
      return TYPE_ENUM;
    default:
      return -1;
  }
}

AuxType aux_from_tag(TagType tag) {
  switch (tag) {
    case TAG_STRUCT:
      return AUX_STRUCT;
    case TAG_UNION:
      return AUX_UNION;
    case TAG_ENUM:
      return AUX_ENUM;
    default:
      return -1;
  }
}

unsigned int flag_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return TYPESPEC_FLAG_STRUCT;
    case TOK_UNION:
      return TYPESPEC_FLAG_UNION;
    case TOK_ENUM:
      return TYPESPEC_FLAG_ENUM;
    default:
      return TYPESPEC_FLAG_NONE;
  }
}

unsigned int index_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return TYPESPEC_INDEX_STRUCT;
    case TOK_UNION:
      return TYPESPEC_INDEX_UNION;
    case TOK_ENUM:
      return TYPESPEC_INDEX_ENUM;
    default:
      return TYPESPEC_INDEX_COUNT;
  }
}


/* tags will need two passes to define their members: the first for inserting
 * the symbols into the symbol table, and the second to swipe the symbols from
 * the members and put them into an auxspec
 */
ASTree *validate_tag_typespec(ASTree *spec_list, ASTree *tag) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & INCOMPATIBLE_FLAGSETS[index_from_symbol(tag->symbol)]) {
    return astree_create_errnode(astree_adopt(spec_list, 1, tag),
                       BCC_TERR_INCOMPLETE_SPEC, 2, spec_list, tag);
  }
  /* TODO(Robert): get unique tag name/info somehow */
  const char *tag_name = astree_get(tag, 0)->lexinfo;
  size_t tag_name_len = strlen(tag_name);
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, tag_name_len, &tagval);
  if (tagval == NULL) {
    return astree_create_errnode(astree_adopt(spec_list, 1, tag), BCC_TERR_TAG_NOT_FOUND,
                       1, tag);
  }

  out->base = type_from_tag(tagval->tag);
  out->width = tagval->width;
  out->alignment = tagval->alignment;
  out->flags |= flag_from_symbol(tag->symbol);

  if (out->auxspecs.anchor == NULL) {
    typespec_init(out);
  }

  AuxSpec *tag_aux = calloc(1, sizeof(*tag_aux));
  tag_aux->aux = aux_from_tag(tagval->tag);
  tag_aux->data.tag.name = tag_name;
  tag_aux->data.tag.val = tagval;
  llist_push_back(&out->auxspecs, tag_aux);
  return astree_adopt(spec_list, 1, tag);
}

ASTree *validate_typedef_typespec(ASTree *spec_list, ASTree *typedef_) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & TYPESPEC_FLAGS_STORAGE_CLASS) {
    return astree_create_errnode(astree_adopt(spec_list, 1, typedef_),
                       BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list, typedef_);
  } else {
    out->flags |= TYPESPEC_FLAG_TYPEDEF;
    return astree_adopt(spec_list, 1, typedef_);
  }
}

ASTree *validate_type_id_typespec(ASTree *spec_list, ASTree *type_id) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  const char *type_name = type_id->lexinfo;
  size_t type_name_len = strlen(type_name);
  SymbolValue *symval = NULL;
  state_get_symbol(state, type_name, type_name_len, &symval);
  if (!symval) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                       BCC_TERR_TYPEID_NOT_FOUND, 2, spec_list, type_id);
  } else if ((symval->type.flags & TYPESPEC_FLAG_TYPEDEF) == 0) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                       BCC_TERR_EXPECTED_TYPEID, 2, spec_list, type_id);
  } else {
    out->base = symval->type.base;
    out->width = symval->type.width;
    out->alignment = symval->type.alignment;
    out->flags |= symval->type.flags & (~TYPESPEC_FLAG_TYPEDEF);

    if (out->auxspecs.anchor == NULL) {
      typespec_init(out);
    }

    int status = typespec_append_auxspecs(out, &symval->type);
    if (status) {
      return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    }
    return astree_adopt(spec_list, 1, type_id);
  }
}

ASTree *validate_typespec(ASTree *spec_list, ASTree *spec) {
  if (spec_list->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(spec_list, spec);
  } else if (spec->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(spec_list, spec);
  }

  if (spec_list->type == NULL) {
    spec_list->type = calloc(1, sizeof(TypeSpec));
  }

  switch (spec->symbol) {
    case TOK_VOID: {
      TypeSpec *out = (TypeSpec *)spec_list->type;
      int incompatible =
          out->flags & INCOMPATIBLE_FLAGSETS[TYPESPEC_INDEX_VOID];
      if (incompatible) {
        return astree_create_errnode(astree_adopt(spec_list, 1, spec),
                           BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list, spec);
      } else {
        out->base = TYPE_VOID;
        out->flags |= TYPESPEC_FLAG_VOID;
        return astree_adopt(spec_list, 1, spec);
      }
    }
    case TOK_INT:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_INT, TYPESPEC_FLAG_INT,
                                       X64_SIZEOF_INT);
    case TOK_LONG:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_LONG, TYPESPEC_FLAG_LONG,
                                       X64_SIZEOF_LONG);
    case TOK_SHORT:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_SHORT,
                                       TYPESPEC_FLAG_SHORT, X64_SIZEOF_SHORT);
    case TOK_CHAR:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_CHAR, TYPESPEC_FLAG_CHAR,
                                       X64_SIZEOF_CHAR);
    case TOK_SIGNED:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_SIGNED,
                                       TYPESPEC_FLAG_SIGNED, 0);
    case TOK_UNSIGNED:
      return validate_integer_typespec(astree_adopt(spec_list, 1, spec),
                                       TYPESPEC_INDEX_UNSIGNED,
                                       TYPESPEC_FLAG_UNSIGNED, 0);
    case TOK_UNION:
    case TOK_STRUCT:
    case TOK_ENUM:
      return validate_tag_typespec(spec_list, spec);
    case TOK_CONST:
      /* fall through */
    case TOK_VOLATILE:
      return astree_adopt(spec_list, 1, spec);
    case TOK_TYPEDEF_NAME:
      return validate_type_id_typespec(spec_list, spec);
    case TOK_TYPEDEF:
      return validate_typedef_typespec(spec_list, spec);
    default:
      return astree_create_errnode(astree_adopt(spec_list, 1, spec), BCC_TERR_FAILURE, 0);
  }
}

ASTree *validate_typespec_list(ASTree *spec_list) {
  if (spec_list->symbol == TOK_TYPE_ERROR) return spec_list;
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if ((out->flags & (TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAG_CHAR)) &&
      !(out->flags & TYPESPEC_FLAGS_SIGNEDNESS)) {
    /* default to signed if not specified */
    out->base = TYPE_SIGNED;
    return spec_list;
  } else if ((out->flags & TYPESPEC_FLAGS_SIGNEDNESS) &&
             !(out->flags & (TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAG_CHAR))) {
    /* default to width of "int" if not specified */
    out->width = X64_SIZEOF_INT;
    out->alignment = X64_ALIGNOF_INT;
    return spec_list;
  } else if (out->base == TYPE_NONE) {
    return astree_create_errnode(spec_list, BCC_TERR_INCOMPLETE_TYPE, 1, spec_list);
  } else {
    return spec_list;
  }
}

/*
 * Combines type specifier and declarator information and inserts symbol value
 * into the table at the current scope. Returns the declarator node passed in
 * as the second argument, or an error node enclosing this node. Can be safely
 * called with error nodes as arguments.
 */
ASTree *validate_declaration(ASTree *declaration, ASTree *declarator) {
  DEBUGS('t', "Making object entry for value %s", declarator->lexinfo);
  if (declaration->symbol == TOK_TYPE_ERROR ||
      declarator->symbol == TOK_TYPE_ERROR) {
    return declarator;
  }
  ASTree *spec_list = astree_get(declaration, 0);
  const TypeSpec *decl_specs = spec_list->type;
  SymbolValue *symbol = sym_from_type((TypeSpec *)declarator->type);
  symbol->type.base = decl_specs->base;
  symbol->type.alignment = decl_specs->alignment;
  symbol->type.width = decl_specs->width;
  symbol->type.flags = decl_specs->flags;
  int status = typespec_append_auxspecs(&symbol->type, (TypeSpec *)decl_specs);
  if (status) {
    return astree_create_errnode(declarator, BCC_TERR_LIBRARY_FAILURE, 0);
  }

  const char *identifier = declarator->lexinfo;
  size_t identifier_len = strnlen(identifier, MAX_IDENT_LEN);
  SymbolValue *exists = NULL;
  int is_redefinition =
      state_get_symbol(state, identifier, identifier_len, &exists);
  if (!is_redefinition) {
    if (exists && (exists->type.flags & TYPESPEC_FLAG_TYPEDEF) &&
        !spec_list_includes_type(astree_get(declaration, 0))) {
      /* don't redeclare typedefs if type was not specified in declaration */
      symbol_value_destroy(symbol);
      declarator->type = &exists->type;
      return declarator;
    } else if (declarator->symbol == TOK_TYPE_NAME) {
      /* type name in cast or sizeof; do not insert symbol */
      /* TODO(Robert): copy and free the information in the symbol we just
       * created so that the syntax tree node can free it later, or use fancy
       * pointer math to get back the address in the heap of the SymbolValue
       * that the type information is embedded in.
       */
      return declarator;
    } else {
      /* TODO(Robert): check for incomplete type */
      int status =
          state_insert_symbol(state, identifier, identifier_len, symbol);
      if (status) {
        symbol_value_destroy(symbol);
        declarator->type = &SPEC_EMPTY;
        return astree_create_errnode(declarator, BCC_TERR_LIBRARY_FAILURE, 0);
      }
      declarator->type = &symbol->type;
      return declarator;
    }
  } else if ((typespec_is_function(&exists->type) &&
              typespec_is_function(&symbol->type)) ||
             ((exists->type.flags & TYPESPEC_FLAG_TYPEDEF) &&
              (symbol->type.flags & TYPESPEC_FLAG_TYPEDEF))) {
    int compatibility = types_compatible(&exists->type, &symbol->type);
    symbol_value_destroy(symbol);
    declarator->type = &SPEC_EMPTY;
    if (compatibility != TCHK_COMPATIBLE) {
      return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1, declarator);
    } else {
      return declarator;
    }
  } else {
    /* TODO(Robert): allow redefinition of extern symbols so long as types
     * are compatible
     */
    symbol_value_destroy(symbol);
    declarator->type = &SPEC_EMPTY;
    return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1, declarator);
  }
}

ASTree *finalize_declaration(ASTree *declaration) {
  if (declaration == &EMPTY_EXPR) return declaration;
  ASTree *errnode = NULL;
  if (declaration->symbol == TOK_TYPE_ERROR) {
    errnode = declaration;
    declaration = astree_get(declaration, 0);
  }
  ASTree *spec_list = astree_get(declaration, 0);
  if (spec_list->type != &SPEC_EMPTY) {
    int status = typespec_destroy((TypeSpec *)spec_list->type);
    free((TypeSpec *)spec_list->type);
    spec_list->type = &SPEC_EMPTY;
  }
  return errnode != NULL ? errnode : declaration;
}

ASTree *validate_array_size(ASTree *array, ASTree *expr) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(array, expr);
  }
  if ((expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST)) == 0) {
    return astree_create_errnode(astree_adopt(array, 1, expr), BCC_TERR_EXPECTED_INTCONST,
                       2, array, expr);
  }
  return astree_adopt(array, 1, expr);
}

ASTree *validate_param_list(ASTree *param_list) {
  param_list->symbol_table = symbol_table_init();
  int status = state_push_table(state, param_list->symbol_table);
  if (status)
    return astree_create_errnode(param_list, BCC_TERR_LIBRARY_FAILURE, 0);
  else
    return param_list;
}

ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator) {
  declarator = validate_declaration(declaration, declarator);
  if (param_list->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(param_list, astree_propogate_errnode(declaration, declarator));
  } else if (declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(param_list, astree_propogate_errnode(declaration, declarator));
  }

  return astree_adopt(param_list, 1, astree_adopt(declaration, 1, declarator));
}

ASTree *finalize_param_list(ASTree *param_list) {
  int status = state_pop_table(state);
  if (status)
    return astree_create_errnode(param_list, BCC_TERR_LIBRARY_FAILURE, 0);
  else
    return param_list;
}

ASTree *define_params(ASTree *declarator, ASTree *param_list) {
  AuxSpec *aux_function = calloc(1, sizeof(*aux_function));
  aux_function->aux = AUX_FUNCTION;
  aux_function->data.params = malloc(sizeof(*aux_function->data.params));
  /* type is not resposible for freeing symbol information */
  llist_init(aux_function->data.params, NULL, NULL);
  LinkedList *param_entries = aux_function->data.params;
  size_t i;
  for (i = 0; i < astree_count(param_list); ++i) {
    ASTree *declaration = astree_get(param_list, i);
    ASTree *declarator = astree_get(declaration, 1);
    SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
    int status = llist_push_back(param_entries, symval);
    if (status) {
      return astree_create_errnode(astree_adopt(declarator, 1, param_list),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    }
  }
  TypeSpec *out = (TypeSpec *)declarator->type;
  int status = llist_push_back(&out->auxspecs, aux_function);
  if (status) {
    return astree_create_errnode(astree_adopt(declarator, 1, param_list),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return astree_adopt(declarator, 1, param_list);
}

ASTree *define_array(ASTree *declarator, ASTree *array) {
  TypeSpec *spec = (TypeSpec *)declarator->type;
  if (typespec_is_incomplete(spec)) {
    return astree_create_errnode(astree_adopt(declarator, 1, array),
                       BCC_TERR_INCOMPLETE_TYPE, 2, array, spec);
  }
  AuxSpec *aux_array = calloc(1, sizeof(*aux_array));
  aux_array->aux = AUX_ARRAY;
  /* TODO(Robert): evaluate array size during three address code generation */
  /* set array size to any nonzero value, for now */
  aux_array->data.memory_loc.length = -1;
  int status = llist_push_back(&spec->auxspecs, aux_array);
  if (status) {
    free(aux_array);
    return astree_create_errnode(astree_adopt(declarator, 1, array),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  } else {
    return astree_adopt(declarator, 1, array);
  }
}

ASTree *define_pointer(ASTree *declarator, ASTree *pointer) {
  if (declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declarator, pointer);
  }
  AuxSpec *aux_pointer = calloc(1, sizeof(*aux_pointer));
  aux_pointer->aux = AUX_POINTER;
  TypeSpec *spec = (TypeSpec *)declarator->type;
  int status = llist_push_back(&spec->auxspecs, aux_pointer);
  if (status) {
    free(aux_pointer);
    return astree_create_errnode(astree_adopt(declarator, 1, pointer),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  } else {
    return astree_adopt(declarator, 1, pointer);
  }
}

ASTree *define_dirdecl(ASTree *declarator, ASTree *dirdecl) {
  /* TODO(Robert): do not allow multiple function dirdecls to occur, and do not
   * allow functions to return array types
   */
  /* TODO(Robert): possibly add external function to check validity of linked
   * list and other badlib data structures
   * TODO(Robert): initialize auxspec list in a more predictable and centralized
   * way, and rename or repurpose typespec_init to make it more clear what it
   * does
   */
  if (declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declarator, dirdecl);
  }
  TypeSpec *out = (TypeSpec *)declarator->type;
  if (out->auxspecs.anchor == NULL) {
    typespec_init(out);
  }

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

/* TODO(Robert): handle more complicated (nested list) initializers */

ASTree *typecheck_array_initializer(ASTree *equal_sign, ASTree *declarator,
                                    ASTree *init_list) {
  /* TODO(Robert): evaluate array size when generating three address code */
  const TypeSpec *array_type = declarator->type;
  TypeSpec element_type = SPEC_EMPTY;
  int status = strip_aux_type(&element_type, array_type);
  if (status) {
    return astree_create_errnode(astree_adopt(equal_sign, 2, declarator, init_list),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  ASTree *ret = astree_adopt(equal_sign, 2, declarator, init_list);
  size_t i;
  for (i = 0; i < astree_count(init_list); ++i) {
    ASTree *initializer = astree_get(init_list, i);
    ASTree *errnode = perform_pointer_conv(initializer);
    errnode = convert_type(errnode, &element_type);
    /* insert any successfully created conversion nodes into the tree, but do
     * not insert the error node, since that goes at the top of the tree */
    (void)astree_replace(init_list, i, UNWRAP(errnode));
    if (errnode->symbol == TOK_TYPE_ERROR) {
      /* remove initializer tree from errnode's list of children */
      (void)llist_extract(&errnode->children, 0);
      if (ret->symbol == TOK_TYPE_ERROR) {
        int status = typespec_append_auxspecs((TypeSpec *)ret->type,
                                              (TypeSpec *)errnode->type);
        /* TODO(Robert): check for and indicate errors */
        if (status) abort();
        status = astree_destroy(errnode);
        if (status) abort();
      } else {
        ret = astree_adopt(errnode, 1, ret);
      }
    }
  }
  return ret;
}

ASTree *typecheck_union_initializer(ASTree *equal_sign, ASTree *declarator,
                                    ASTree *init_list) {
  ASTree *identifier = declarator;
  AuxSpec *union_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = union_aux->data.tag.name;
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, strlen(tag_name), &tagval);
  /* tag was found previously when the declarator was validated, so if it is not
   * found now, there is a bug in the compiler, not the input.
   */
  assert(tagval != NULL);

  const LinkedList *members = &tagval->data.members.in_order;
  if (astree_count(init_list) > 1) {
    return astree_create_errnode(astree_adopt(equal_sign, 2, declarator, init_list),
                       BCC_TERR_EXCESS_INITIALIZERS, 1, declarator);
  } else {
    /* there should be one initializer of a type compatible with the type of the
     * first member of the union
     */
    ASTree *initializer = astree_get(init_list, 0);
    ASTree *errnode = perform_pointer_conv(initializer);
    SymbolValue *member_symbol = llist_front((LinkedList *)members);
    errnode = convert_type(errnode, &member_symbol->type);
    (void)astree_replace(init_list, 0, UNWRAP(errnode));
    if (errnode->symbol == TOK_TYPE_ERROR) {
      (void)llist_extract(&errnode->children, 0);
      return astree_adopt(errnode, 1,
                          astree_adopt(equal_sign, 2, declarator, init_list));
    } else {
      return astree_adopt(equal_sign, 2, declarator, init_list);
    }
  }
}

ASTree *typecheck_struct_initializer(ASTree *equal_sign, ASTree *declarator,
                                     ASTree *init_list) {
  ASTree *identifier = declarator;
  AuxSpec *struct_aux = llist_front((LinkedList *)&identifier->type->auxspecs);
  const char *tag_name = struct_aux->data.tag.name;
  TagValue *tagval = NULL;
  state_get_tag(state, tag_name, strlen(tag_name), &tagval);
  /* tag was found previously when the declarator was validated, so if it is not
   * found now, there is a bug in the compiler, not the input.
   */
  assert(tagval != NULL);

  const LinkedList *members = &tagval->data.members.in_order;
  if (members->size < astree_count(init_list)) {
    return astree_create_errnode(astree_adopt(equal_sign, 2, declarator, init_list),
                       BCC_TERR_EXCESS_INITIALIZERS, 1, declarator);
  } else {
    ASTree *ret = astree_adopt(equal_sign, 2, declarator, init_list);
    size_t i;
    for (i = 0; i < astree_count(init_list); ++i) {
      ASTree *initializer = astree_get(init_list, i);
      ASTree *errnode = perform_pointer_conv(initializer);
      SymbolValue *member_symbol = llist_get((LinkedList *)members, i);
      errnode = convert_type(errnode, &member_symbol->type);
      (void)astree_replace(init_list, i, UNWRAP(errnode));
      if (errnode->symbol == TOK_TYPE_ERROR) {
        /* remove initializer tree from errnode's list of children */
        (void)llist_extract(&errnode->children, 0);
        if (ret->symbol == TOK_TYPE_ERROR) {
          int status = typespec_append_auxspecs((TypeSpec *)ret->type,
                                                (TypeSpec *)errnode->type);
          /* TODO(Robert): check for and indicate errors */
          if (status) abort();
          status = astree_destroy(errnode);
          if (status) abort();
        } else {
          ret = astree_adopt(errnode, 1, ret);
        }
      }
    }
    return ret;
  }
}

ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src) {
  if (src->symbol == TOK_INIT_LIST) {
    const TypeSpec *decl_type = dest->type;
    if (typespec_is_array(decl_type)) {
      return typecheck_array_initializer(assignment, dest, src);
    } else if (decl_type->base == TYPE_UNION) {
      return typecheck_union_initializer(assignment, dest, src);
    } else if (decl_type->base == TYPE_STRUCT) {
      return typecheck_struct_initializer(assignment, dest, src);
    } else {
      return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                         BCC_TERR_UNEXPECTED_LIST, 2, dest, src);
    }
  } else {
    src = perform_pointer_conv(src);
    src = convert_type(src, dest->type);
    if (dest->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(assignment, 2, dest, src);
    } else if (src->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(assignment, 2, dest, src);
    }
    assignment->attributes |=
        src->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITHCONST);

    assignment->type = dest->type;
    return astree_adopt(assignment, 2, dest, src);
  }
}

ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer) {
  declarator = validate_declaration(declaration, declarator);
  if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(
        declaration, 1,
        astree_propogate_errnode_v(equal_sign, 2, declarator, initializer));
  }
  equal_sign = validate_assignment(equal_sign, declarator, initializer);
  if (equal_sign->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declaration, equal_sign);
  }
  return astree_adopt(declaration, 1, equal_sign);
}

int resolve_label(ASTree *ident_node) {
  const char *ident = ident_node->lexinfo;
  size_t ident_len = strnlen(ident, MAX_IDENT_LEN);
  LabelValue *labval = state_get_label(state, ident, ident_len);
  if (labval == NULL) {
    return BCC_TERR_SYM_NOT_FOUND;
  } else {
    return BCC_TERR_SUCCESS;
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

  SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
  if (symval == NULL) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_FAILURE, 0);
  } else if (!typespec_is_function(&symval->type)) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_EXPECTED_FUNCTION, 1, declarator);
  } else if (symval->flags & SYMFLAG_FUNCTION_DEFINED) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_REDEFINITION, 1, declarator);
  }

  /* param list should be first child for properly defined functions */
  ASTree *param_list = astree_get(declarator, 0);
  assert(param_list->symbol == TOK_PARAM_LIST);
  /* treat body like a normal block statement, but move param table to body node
   * and define function before entering body scope */
  body->symbol_table = param_list->symbol_table;
  param_list->symbol_table = NULL;
  int status = state_push_table(state, body->symbol_table);
  if (status) {
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }

  /* TODO(Robert): set function to a dummy value even in the event of failure so
   * that return statements in the body may be processed in some way
   */
  status = state_set_function(state, symval);
  if (status)
    return astree_create_errnode(astree_adopt(declaration, 2, declarator, body),
                       BCC_TERR_FAILURE, 0);
  return astree_adopt(declaration, 2, declarator, body);
}

ASTree *validate_fnbody_content(ASTree *function, ASTree *fnbody_content) {
  /* we can't reuse validate_block_content here because all that function does
   * is perform adoption and propogate errors, both of which are different here
   * because of the tree structure of function definitions
   */
  ASTree *fnbody = astree_get(UNWRAP(function), 2);
  (void)astree_adopt(fnbody, 1, UNWRAP(fnbody_content));
  if (fnbody_content->symbol == TOK_TYPE_ERROR) {
    (void)astree_remove(fnbody_content, 0);
    if (function->symbol == TOK_TYPE_ERROR) {
      /* TODO(Robert): copy-pasted from astree_propogate_errnode; deduplicate later */
      TypeSpec *parent_errs = (TypeSpec *)function->type;
      TypeSpec *child_errs = (TypeSpec *)fnbody_content->type;
      int status = typespec_append_auxspecs(parent_errs, child_errs);
      /* TODO(Robert): be a man */
      if (status) abort();
      status = astree_destroy(fnbody_content);
      if (status) abort();
      return function;
    } else {
      return astree_adopt(fnbody_content, 1, function);
    }
  }

  /* TODO(Robert): handle control flow statements even in the case of errors */
  int status = merge_block_controls(fnbody, fnbody_content);
  if (status) {
    return astree_create_errnode(astree_adopt(fnbody, 1, fnbody_content),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return function;
}

ASTree *finalize_function(ASTree *function) {
  ASTree *body = astree_get(UNWRAP(function), 2);
  ASTree *ret = function;
  TypeSpec *errspec = symbol_table_process_control(body->symbol_table, function->symbol);
  if (errspec != NULL) {
    if (function->symbol == TOK_TYPE_ERROR) {
      TypeSpec *function_errs = (TypeSpec *)function->type;
      int status = typespec_append_auxspecs(function_errs, errspec);
      if (status) abort();
    } else {
      ASTree *errnode = astree_init(TOK_TYPE_ERROR, function->loc, "_terr");
      errnode->type = errspec;
      ret = astree_adopt(errnode, 1, function);
    }
  }
  SymbolValue *symval = state_get_function(state);
  symval->flags |= SYMFLAG_FUNCTION_DEFINED;
  int status = state_unset_function(state);
  if (status) ret = astree_create_errnode(ret, BCC_TERR_FAILURE, 0);
  /* do not use finalize_block because it will put the error in an awkward place
   */
  status = state_pop_table(state);
  if (status) ret = astree_create_errnode(ret, BCC_TERR_LIBRARY_FAILURE, 0);
  return ret;
}

TagType tag_from_symbol(int symbol) {
  switch (symbol) {
    case TOK_STRUCT:
      return TAG_STRUCT;
    case TOK_UNION:
      return TAG_UNION;
    case TOK_ENUM:
      return TAG_ENUM;
    default:
      return -1;
  }
}

int errcode_from_tagtype(TagType tag_type) {
  switch (tag_type) {
    case TAG_STRUCT:
      return BCC_TERR_EXPECTED_STRUCT;
    case TAG_UNION:
      return BCC_TERR_EXPECTED_UNION;
    case TAG_ENUM:
      return BCC_TERR_EXPECTED_ENUM;
    default:
      return BCC_TERR_FAILURE;
  }
}

ASTree *validate_tag_def(ASTree *tag_type_node, ASTree *tag_name_node,
                         ASTree *left_brace) {
  const char *tag_name = tag_name_node->lexinfo;
  const size_t tag_name_len = strnlen(tag_name, MAX_IDENT_LEN);
  TagValue *exists = NULL;
  int is_redefinition = state_get_tag(state, tag_name, tag_name_len, &exists);
  int tag_declares_members = left_brace != NULL;
  TagType tag_type = tag_from_symbol(tag_type_node->symbol);
  if (is_redefinition) {
    if (tag_type != exists->tag) {
      return astree_create_errnode(
          astree_adopt(tag_type_node, 2, tag_name_node, left_brace),
          errcode_from_tagtype(exists->tag), 2, tag_type_node, tag_name_node);
    } else if (tag_declares_members) {
      if (exists->is_defined) {
        return astree_create_errnode(
            astree_adopt(tag_type_node, 2, tag_name_node, left_brace),
            BCC_TERR_REDEFINITION, 1, tag_name_node);
      } else {
        /* TODO(Robert): error handling */
        tag_type_node->type = calloc(1, sizeof(TypeSpec));
        int status = typespec_init((TypeSpec *)tag_type_node->type);
        AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
        tag_aux->aux = aux_from_tag(tag_type);
        tag_aux->data.tag.name = tag_name;
        tag_aux->data.tag.val = exists;
        status = llist_push_back((LinkedList *)&tag_type_node->type->auxspecs,
                                 tag_aux);
        if (tag_type != TAG_ENUM) {
          int status = state_push_table(state, exists->data.members.by_name);
        }
        return astree_adopt(tag_type_node, 2, tag_name_node, left_brace);
      }
    } else {
      return astree_adopt(tag_type_node, 1, tag_name_node);
    }
  } else if (tag_declares_members) {
    /* TODO(Robert): error handling */
    TagValue *tagval = tag_value_init(tag_type);
    int status = state_insert_tag(state, tag_name, tag_name_len, tagval);
    tag_type_node->type = calloc(1, sizeof(TypeSpec));
    status = typespec_init((TypeSpec *)tag_type_node->type);
    AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
    tag_aux->aux = aux_from_tag(tag_type);
    tag_aux->data.tag.name = tag_name;
    tag_aux->data.tag.val = tagval;
    status =
        llist_push_back((LinkedList *)&tag_type_node->type->auxspecs, tag_aux);
    if (tag_type != TAG_ENUM) {
      int status = state_push_table(state, tagval->data.members.by_name);
    }
    return astree_adopt(tag_type_node, 2, tag_name_node, left_brace);
  } else {
    return astree_adopt(tag_type_node, 1, tag_name_node);
  }
}

ASTree *finalize_tag_def(ASTree *tag) {
  ASTree *errnode = NULL;
  if (tag->symbol == TOK_TYPE_ERROR) {
    errnode = tag;
    tag = astree_get(tag, 0);
  }
  if (tag->symbol != TOK_ENUM) {
    int status = state_pop_table(state);
  }
  AuxSpec *struct_aux = llist_back(&tag->type->auxspecs);
  TagValue *tagval = struct_aux->data.tag.val;
  tagval->is_defined = 1;
  int status = typespec_destroy((TypeSpec *)tag->type);
  free((TypeSpec *)tag->type);
  tag->type = &SPEC_EMPTY;
  return errnode == NULL ? tag : errnode;
}

ASTree *define_enumerator(ASTree *enum_, ASTree *ident_node, ASTree *equal_sign,
                          ASTree *expr) {
  if (enum_->symbol == TOK_TYPE_ERROR) {
    ASTree *real_enum = astree_get(enum_, 0);
    ASTree *left_brace = astree_get(real_enum, 1);
    if (equal_sign != NULL) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return enum_;
    } else {
      astree_adopt(left_brace, 1, ident_node);
      return enum_;
    }
  }
  ASTree *left_brace = astree_get(enum_, 1);
  const char *ident = ident_node->lexinfo;
  const size_t ident_len = strnlen(ident, MAX_IDENT_LEN);

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
  int status = typespec_init(&symval->type);
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
  symval->type.alignment = X64_ALIGNOF_INT;
  symval->type.width = X64_SIZEOF_INT;
  symval->type.base = TYPE_ENUM;
  /* mark as enumeration consntant */
  symval->flags |= SYMFLAG_ENUM_CONST;

  AuxSpec *enum_aux = calloc(1, sizeof(*enum_aux));
  enum_aux->aux = AUX_ENUM;
  enum_aux->data.tag.name = (astree_get(enum_, 0))->lexinfo;
  llist_push_back(&symval->type.auxspecs, enum_aux);

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

  ident_node->type = &symval->type;

  if (equal_sign != NULL) {
    /* TODO(Robert): evaluate enumeration constants */
    if (expr->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode =
          astree_propogate_errnode(astree_adopt(equal_sign, 1, ident_node), expr);
      /* TODO(Robert): have error propogation function handle more complex
       * syntax tree structures
       */
      return astree_propogate_errnode(enum_, errnode);
    }
    if ((expr->attributes & ATTR_EXPR_ARITHCONST) == 0) {
      astree_adopt(left_brace, 1,
                   astree_adopt(equal_sign, 2, ident_node, expr));
      return astree_create_errnode(enum_, BCC_TERR_EXPECTED_ARITHCONST, 2, equal_sign,
                         expr);
    }
    astree_adopt(left_brace, 1, astree_adopt(equal_sign, 2, ident_node, expr));
    return enum_;
  } else {
    astree_adopt(left_brace, 1, ident_node);
    return enum_;
  }
}

ASTree *define_struct_member(ASTree *struct_, ASTree *member) {
  if (struct_->symbol == TOK_TYPE_ERROR) {
    ASTree *left_brace = astree_get(UNWRAP(struct_), 1);
    (void)astree_adopt(left_brace, 1, UNWRAP(member));
    if (member->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = member;
      (void)llist_extract(&member->children, 0);
      int status = typespec_append_auxspecs((TypeSpec *)struct_->type,
                                            (TypeSpec *)errnode->type);
      if (status) abort();
      status = astree_destroy(errnode);
      if (status) abort();
    }
    return struct_;
  } else if (member->symbol == TOK_TYPE_ERROR) {
    ASTree *left_brace = astree_get(struct_, 1);
    (void)astree_adopt(left_brace, 1, llist_extract(&member->children, 0));
    return astree_adopt(member, 1, struct_);
  }
  AuxSpec *struct_aux = llist_back(&struct_->type->auxspecs);
  TagValue *tagval = struct_aux->data.tag.val;
  LinkedList *member_list = &tagval->data.members.in_order;
  size_t i;
  /* skip first child, which is the typespec list */
  for (i = 1; i < astree_count(member); ++i) {
    ASTree *declarator = astree_get(member, i);
    SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
    if (tagval->alignment < symval->type.alignment) {
      tagval->alignment = symval->type.alignment;
    }
    if (tagval->tag == TAG_STRUCT) {
      sprintf(symval->obj_loc, "%%s+%zu", tagval->width);
      size_t padding =
          symval->type.alignment - (tagval->width % symval->type.alignment);
      tagval->width += padding + symval->type.width;
    } else if (tagval->width < symval->type.width) {
      tagval->width = symval->type.width;
    }
    llist_push_back(member_list, symval);
  }
  ASTree *left_brace = astree_get(struct_, 1);
  astree_adopt(left_brace, 1, member);
  return struct_;
}

ASTree *validate_declarator(ASTree *declarator) {
  SymbolValue *symval =
      symbol_value_init(&declarator->loc, state_get_sequence(state));
  declarator->type = &symval->type;
  return declarator;
}

ASTree *declare_symbol(ASTree *declaration, ASTree *declarator) {
  ASTree *err_or_decl = validate_declaration(declaration, declarator);
  if (err_or_decl->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declaration, err_or_decl);
  } else if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declaration, err_or_decl);
  } else {
    return astree_adopt(declaration, 1, declarator);
  }
}

ASTree *validate_topdecl(ASTree *root, ASTree *topdecl) {
  if (root->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else if (topdecl->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(root, topdecl);
  } else {
    return astree_adopt(root, 1, topdecl);
  }
}
