#include "tchk_decl.h"

#include "asmgen.h"
#include "assert.h"
#include "badalist.h"
#include "lyutils.h"
#include "state.h"
#include "stdlib.h"
#include "tchk_common.h"

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
    return astree_create_errnode(
        spec_list, BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
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

int combine_types(TypeSpec *dest, const TypeSpec *src) {
  dest->base = src->base;
  dest->alignment = src->alignment;
  dest->width = src->width;
  dest->flags |= src->flags;
  return typespec_append_auxspecs(dest, (TypeSpec *)src);
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

  if (out->auxspecs.anchor == NULL) {
    typespec_init(out);
  }

  assert(!combine_types((TypeSpec *)spec_list->type, tag->type));
  /* free temporary typespec */
  TypeSpec *tag_type = (TypeSpec *)tag->type;
  (void)llist_pop_back(&tag_type->auxspecs);
  assert(!typespec_destroy((TypeSpec *)tag->type));
  free(tag_type);
  tag->type = &SPEC_EMPTY;
  out->flags |= flag_from_symbol(tag->symbol);

  return astree_adopt(spec_list, 1, tag);
}

ASTree *validate_qualifier(ASTree *spec_list, ASTree *qualifier,
                           enum typespec_flag flag) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & (TYPESPEC_FLAG_CONST | TYPESPEC_FLAG_VOLATILE)) {
    return astree_create_errnode(astree_adopt(spec_list, 1, qualifier),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                                 qualifier);
  } else {
    out->flags |= flag;
    return astree_adopt(spec_list, 1, qualifier);
  }
}

ASTree *validate_storage_class(ASTree *spec_list, ASTree *storage_class,
                               enum typespec_flag flag) {
  TypeSpec *out = (TypeSpec *)spec_list->type;
  if (out->flags & TYPESPEC_FLAGS_STORAGE_CLASS) {
    return astree_create_errnode(astree_adopt(spec_list, 1, storage_class),
                                 BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                                 storage_class);
  } else {
    out->flags |= flag;
    return astree_adopt(spec_list, 1, storage_class);
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
                                 BCC_TERR_TYPEID_NOT_FOUND, 2, spec_list,
                                 type_id);
  } else if ((symval->type.flags & TYPESPEC_FLAG_TYPEDEF) == 0) {
    return astree_create_errnode(astree_adopt(spec_list, 1, type_id),
                                 BCC_TERR_EXPECTED_TYPEID, 2, spec_list,
                                 type_id);
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
                                     BCC_TERR_INCOMPATIBLE_SPEC, 2, spec_list,
                                     spec);
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
    case TOK_TYPEDEF_NAME:
      return validate_type_id_typespec(spec_list, spec);
    case TOK_CONST:
      return validate_qualifier(spec_list, spec, TYPESPEC_FLAG_CONST);
    case TOK_VOLATILE:
      return validate_qualifier(spec_list, spec, TYPESPEC_FLAG_VOLATILE);
    case TOK_TYPEDEF:
      return validate_storage_class(spec_list, spec, TYPESPEC_FLAG_TYPEDEF);
    case TOK_AUTO:
      return validate_storage_class(spec_list, spec, TYPESPEC_FLAG_AUTO);
    case TOK_REGISTER:
      return validate_storage_class(spec_list, spec, TYPESPEC_FLAG_REGISTER);
    case TOK_STATIC:
      return validate_storage_class(spec_list, spec, TYPESPEC_FLAG_STATIC);
    case TOK_EXTERN:
      return validate_storage_class(spec_list, spec, TYPESPEC_FLAG_EXTERN);
    default:
      return astree_create_errnode(astree_adopt(spec_list, 1, spec),
                                   BCC_TERR_FAILURE, 0);
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
    return astree_create_errnode(spec_list, BCC_TERR_INCOMPLETE_TYPE, 1,
                                 spec_list);
  } else {
    return spec_list;
  }
}

int location_is_empty(Location *loc) {
  return loc->filenr == 0 && loc->linenr == 0 && loc->offset == 0;
}

void set_link_and_store(SymbolValue *symval) {
  if (llist_size(&state->table_stack) == 1) {
    if (symval->type.flags & TYPESPEC_FLAG_EXTERN) {
      symval->flags |= (SYMFLAG_LINK_EXT | SYMFLAG_STORE_EXT);
    } else if (symval->type.flags & TYPESPEC_FLAG_STATIC) {
      symval->flags |= (SYMFLAG_LINK_INT | SYMFLAG_STORE_STAT);
    } else {
      symval->flags |= (SYMFLAG_LINK_EXT | SYMFLAG_STORE_STAT);
    }
  } else if (symval->type.flags & TYPESPEC_FLAG_EXTERN) {
    /* validate_declaration will handle extern block symbols */
    symval->type.flags |= (SYMFLAG_LINK_EXT | SYMFLAG_STORE_EXT);
  } else if (symval->type.flags & TYPESPEC_FLAG_STATIC) {
    symval->flags |= (SYMFLAG_LINK_NONE | SYMFLAG_STORE_STAT);
  } else {
    symval->flags |= (SYMFLAG_LINK_NONE | SYMFLAG_STORE_AUTO);
  }
}

int linkage_valid(SymbolValue *symval, SymbolValue *existing) {
  if (symval->type.flags & existing->type.flags & TYPESPEC_FLAG_TYPEDEF)
    return 0;
  assert(symval->flags & SYMFLAGS_LINK);
  assert(existing->flags & SYMFLAGS_LINK);
  if ((symval->flags & SYMFLAG_LINK_NONE) ||
      (existing->flags & SYMFLAG_LINK_NONE)) {
    return 0;
  } else if ((existing->flags & SYMFLAG_LINK_EXT) &&
             (symval->flags & SYMFLAG_LINK_EXT)) {
    return 1;
  } else if (existing->flags & SYMFLAG_LINK_INT) {
    if (symval->flags & SYMFLAG_LINK_INT) {
      return 1;
    } else if ((symval->flags & SYMFLAG_LINK_EXT) &&
               (symval->flags & SYMFLAG_STORE_EXT)) {
      return 1;
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

int is_array_completion(SymbolValue *symval, SymbolValue *exists) {
  int ret = 0;
  AuxSpec *sym_array_aux = llist_front(&symval->type.auxspecs);
  AuxSpec *exists_array_aux = llist_front(&symval->type.auxspecs);
  if (sym_array_aux->data.memory_loc.length == 0 ||
      exists_array_aux->data.memory_loc.length == 0) {
    TypeSpec sym_type_temp;
    TypeSpec exists_type_temp;
    assert(!strip_aux_type(&sym_type_temp, &symval->type));
    assert(!strip_aux_type(&exists_type_temp, &exists->type));
    if (types_equivalent(&sym_type_temp, &exists_type_temp,
                         IGNORE_STORAGE_CLASS))
      ret = 1;
    assert(!typespec_destroy(&sym_type_temp));
    assert(!typespec_destroy(&exists_type_temp));
  }
  return ret;
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
  DEBUGS('t', "Making object entry for value %s", declarator->lexinfo);
  if (location_is_empty(&UNWRAP(declaration)->loc))
    UNWRAP(declaration)->loc = UNWRAP(declarator)->loc;
  if (declaration->symbol == TOK_TYPE_ERROR ||
      declarator->symbol == TOK_TYPE_ERROR)
    return declarator;
  ASTree *spec_list = astree_get(declaration, 0);
  int status = combine_types((TypeSpec *)declarator->type, spec_list->type);
  if (status)
    return astree_create_errnode(declarator, BCC_TERR_LIBRARY_FAILURE, 0);
  if (declarator->symbol == TOK_TYPE_NAME) return declarator;
  SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
  set_link_and_store(symval);
  const char *identifier = declarator->lexinfo;
  size_t identifier_len = strlen(identifier);
  SymbolValue *exists = NULL;
  int is_redeclaration =
      state_get_symbol(state, identifier, identifier_len, &exists);
  if (is_redeclaration) {
    if (linkage_valid(symval, exists)) {
      if (types_equivalent(&symval->type, &exists->type,
                           IGNORE_STORAGE_CLASS)) {
        symbol_value_destroy(symval);
        declarator->type = &exists->type;
        return declarator;
      } else if (typespec_is_array(&symval->type) &&
                 typespec_is_array(&exists->type) &&
                 is_array_completion(symval, exists)) {
        if (typespec_is_incomplete(&exists->type)) {
          TypeSpec temp = exists->type;
          exists->type = symval->type;
          symval->type = temp;
        }
        symbol_value_destroy(symval);
        declarator->type = &exists->type;
        return declarator;
      }
    }
    return astree_create_errnode(declarator, BCC_TERR_REDEFINITION, 1,
                                 declarator);
  } else if (exists && (symval->type.flags & TYPESPEC_FLAG_EXTERN) &&
             types_equivalent(&symval->type, &exists->type,
                              IGNORE_STORAGE_CLASS)) {
    /* extern symval in block with outer symval declares nothing */
    symbol_value_destroy(symval);
    declarator->type = &exists->type;
    exists->flags |= SYMFLAG_INHERIT;
    return declarator;
  } else {
    if (typespec_is_incomplete(&symval->type)) {
      symval->flags |= SYMFLAG_INCOMPLETE;
    }
    int status = state_insert_symbol(state, identifier, identifier_len, symval);
    if (status) {
      symbol_value_destroy(symval);
      declarator->type = &SPEC_EMPTY;
      return astree_create_errnode(declarator, BCC_TERR_LIBRARY_FAILURE, 0);
    }
    /* typedefs are not lvalues */
    if (!(symval->type.flags & TYPESPEC_FLAG_TYPEDEF))
      declarator->attributes |= ATTR_EXPR_LVAL;
    declarator->type = &symval->type;
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
  if (!typespec_is_integer(expr->type) ||
      !(expr->attributes & ATTR_EXPR_CONST) ||
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
  TypeSpec *spec = (TypeSpec *)declarator->type;
  if (typespec_is_array(spec)) {
    return astree_create_errnode(astree_adopt(declarator, 1, param_list),
                                 BCC_TERR_INCOMPATIBLE_DECL, 2, declarator,
                                 param_list);
  }
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
  int status = llist_push_back(&spec->auxspecs, aux_function);
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
  } else if (typespec_is_function(spec)) {
    return astree_create_errnode(astree_adopt(declarator, 1, array),
                                 BCC_TERR_INCOMPATIBLE_DECL, 2, declarator,
                                 array);
  }
  AuxSpec *aux_array = calloc(1, sizeof(*aux_array));
  aux_array->aux = AUX_ARRAY;
  if (astree_count(array) == 0) {
    aux_array->data.memory_loc.length = 0;
  } else {
    ASTree *expr = astree_get(array, 0);
    aux_array->data.memory_loc.length = expr->constant.integral.value;
  }
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
  size_t i;
  for (i = 0; i < astree_count(pointer); ++i) {
    ASTree *qualifier = astree_get(pointer, i);
    if (qualifier->symbol == TOK_CONST) {
      if (aux_pointer->data.memory_loc.qualifiers & TYPESPEC_FLAG_CONST) {
        return astree_create_errnode(astree_adopt(declarator, 1, pointer),
                                     BCC_TERR_INCOMPATIBLE_SPEC, 2, declarator,
                                     qualifier);
      } else {
        aux_pointer->data.memory_loc.qualifiers |= TYPESPEC_FLAG_CONST;
      }
    } else if (qualifier->symbol == TOK_VOLATILE) {
      if (aux_pointer->data.memory_loc.qualifiers & TYPESPEC_FLAG_VOLATILE) {
        return astree_create_errnode(astree_adopt(declarator, 1, pointer),
                                     BCC_TERR_INCOMPATIBLE_SPEC, 2, declarator,
                                     qualifier);
      } else {
        aux_pointer->data.memory_loc.qualifiers |= TYPESPEC_FLAG_VOLATILE;
      }
    }
  }
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

size_t count_agg_members(TypeSpec *spec) {
  assert(typespec_is_array(spec) || typespec_is_union(spec) ||
         typespec_is_struct(spec));
  if (typespec_is_array(spec)) {
    AuxSpec *array_aux = llist_front(&spec->auxspecs);
    /* TODO(Robert): handle deduced array sizes, if necessary */
    return array_aux->data.memory_loc.length;
  } else if (typespec_is_struct(spec)) {
    AuxSpec *struct_aux = llist_front(&spec->auxspecs);
    TagValue *tagval = struct_aux->data.tag.val;
    return llist_size(&tagval->data.members.in_order);
  } else {
    return 1;
  }
}

TypeSpec *get_agg_member_spec(TypeSpec *spec, size_t index) {
  assert(typespec_is_array(spec) || typespec_is_union(spec) ||
         typespec_is_struct(spec));
  if (typespec_is_array(spec)) {
    TypeSpec *ret_spec = malloc(sizeof(TypeSpec));
    assert(!strip_aux_type(ret_spec, spec));
    return ret_spec;
  } else {
    assert(typespec_is_struct(spec) || index == 0);
    AuxSpec *struct_aux = llist_front(&spec->auxspecs);
    LinkedList *member_list = &struct_aux->data.tag.val->data.members.in_order;
    SymbolValue *member_symval = llist_get(member_list, index);
    TypeSpec *member_spec = &member_symval->type;
    /* must be copied so that we can call typespec_destroy/free on it */
    TypeSpec *copy_spec = malloc(sizeof(TypeSpec));
    assert(!typespec_copy(copy_spec, member_spec));
    return copy_spec;
  }
}

void cleanup_agg_member_spec(TypeSpec *spec) {
  assert(!typespec_destroy(spec));
  free(spec);
}

ASTree *validate_scalar_init(TypeSpec *dest_type, ASTree *initializer);
ASTree *validate_init_list(TypeSpec *dest_spec, ASTree *init_list) {
  assert(typespec_is_array(dest_spec) || typespec_is_union(dest_spec) ||
         typespec_is_struct(dest_spec));
  if (init_list->symbol == TOK_TYPE_ERROR) return init_list;
  struct agg_entry {
    TypeSpec *spec;
    size_t index;
  };
  size_t initializer_index = 0;
  LinkedList agg_stack;
  int status =
      llist_init(&agg_stack, (void (*)(void *))cleanup_agg_member_spec, NULL);
  if (status) abort();
  struct agg_entry *first_entry = malloc(sizeof(struct agg_entry));
  first_entry->spec = dest_spec;
  first_entry->index = 0;
  status = llist_push_front(&agg_stack, first_entry);
  if (status) abort();
  while (!llist_empty(&agg_stack)) {
    struct agg_entry *entry = llist_front(&agg_stack);
    TypeSpec *member_spec = get_agg_member_spec(entry->spec, entry->index);
    ASTree *initializer = astree_get(init_list, initializer_index);
    if (typespec_is_scalar(member_spec)) {
      ASTree *errnode = validate_scalar_init(member_spec, initializer);
      if (errnode->symbol == TOK_TYPE_ERROR) {
        llist_destroy(&agg_stack);
        (void)astree_remove(errnode, 0);
        return astree_adopt(errnode, 1, init_list);
      }
      ++initializer_index;
      ++entry->index;
      /* intentionally leak member type resources in case the member type needs
       * to be printed in the error message
       */
      cleanup_agg_member_spec(member_spec);
    } else if (initializer->symbol == TOK_INIT_LIST) {
      ASTree *errnode = validate_init_list(member_spec, initializer);
      if (errnode->symbol == TOK_TYPE_ERROR) {
        llist_destroy(&agg_stack);
        (void)astree_remove(errnode, 0);
        return astree_adopt(errnode, 1, init_list);
      }
      ++initializer_index;
      ++entry->index;
      cleanup_agg_member_spec(member_spec);
    } else {
      struct agg_entry *new_entry = malloc(sizeof(struct agg_entry));
      new_entry->index = 0;
      new_entry->spec = member_spec;
      int status = llist_push_front(&agg_stack, new_entry);
      if (status) abort();
    }
    if (entry->index >= count_agg_members(entry->spec)) {
      cleanup_agg_member_spec(llist_pop_front(&agg_stack));
    } else if (initializer_index >= astree_count(init_list)) {
      break;
    }
  }
  llist_destroy(&agg_stack);
  if (initializer_index != astree_count(init_list)) {
    return astree_create_errnode(init_list, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                 init_list);
  }
  return init_list;
}

ASTree *validate_scalar_init(TypeSpec *dest_type, ASTree *initializer) {
  if (initializer->symbol == TOK_TYPE_ERROR) return initializer;
  pointer_conversions(initializer);
  if (initializer->symbol == TOK_INIT_LIST) {
    if (astree_count(initializer) > 1) {
      return astree_create_errnode(initializer, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                   initializer);
    } else if (astree_get(initializer, 0)->symbol == TOK_INIT_LIST) {
      return astree_create_errnode(initializer, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                   initializer);
    } else {
      ASTree *real_initializer = astree_get(initializer, 0);
      if (types_assignable(dest_type, real_initializer)) {
        return initializer;
        ;
      } else {
        return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES,
                                     3, initializer, dest_type,
                                     real_initializer->type);
      }
    }
  } else if (types_assignable(dest_type, initializer)) {
    return initializer;
  } else {
    return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, dest_type, initializer->type);
  }
}

ASTree *validate_struct_init(TypeSpec *dest_type, ASTree *initializer) {
  if (initializer->symbol == TOK_TYPE_ERROR) {
    return initializer;
  } else if (initializer->symbol == TOK_INIT_LIST) {
    return validate_init_list(dest_type, initializer);
  } else if (types_assignable(dest_type, initializer)) {
    return initializer;
  } else {
    return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, dest_type, initializer->type);
  }
}

ASTree *validate_array_init(TypeSpec *dest_type, ASTree *initializer) {
  if (initializer->symbol == TOK_TYPE_ERROR) {
    return initializer;
  } else if (initializer->symbol == TOK_INIT_LIST) {
    return validate_init_list(dest_type, initializer);
  } else if (typespec_is_chararray(dest_type) &&
             initializer->symbol == TOK_STRINGCON) {
    return initializer;
  } else {
    return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, dest_type, initializer->type);
  }
}

ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer) {
  declarator = validate_declaration(declaration, declarator);
  if (declaration->symbol == TOK_TYPE_ERROR ||
      declarator->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(
        declaration, 1,
        astree_propogate_errnode_v(equal_sign, 2, declarator, initializer));
  }
  SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
  if (symval->flags & SYMFLAG_DEFINED)
    return astree_create_errnode(
        astree_adopt(declaration, 1,
                     astree_adopt(equal_sign, 2, declarator, initializer)),
        BCC_TERR_REDEFINITION, 1, declarator);
  symval->flags |= SYMFLAG_DEFINED;
  equal_sign->type = declarator->type;
  if (typespec_is_struct(declarator->type) ||
      typespec_is_union(declarator->type)) {
    initializer =
        validate_struct_init((TypeSpec *)declarator->type, initializer);
  } else if (typespec_is_array(declarator->type)) {
    initializer =
        validate_array_init((TypeSpec *)declarator->type, initializer);
  } else {
    initializer =
        validate_scalar_init((TypeSpec *)declarator->type, initializer);
  }
  if (initializer->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(
        declaration, 1,
        astree_propogate_errnode_v(equal_sign, 2, declarator, initializer));
  } else if (symval->flags & SYMFLAG_LINK_NONE) {
    maybe_load_cexpr(initializer, NULL);
    return translate_local_init(declaration, equal_sign, declarator,
                                initializer);
  } else {
    maybe_load_cexpr(initializer, NULL);
    return translate_global_init(declaration, equal_sign, declarator,
                                 initializer);
  }
}

int resolve_label(ASTree *ident_node) {
  const char *ident = ident_node->lexinfo;
  size_t ident_len = strlen(ident);
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
  status = state_set_function(state, symval);
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
  (void)astree_adopt(fnbody, 1, UNWRAP(fnbody_content));
  if (fnbody_content->symbol == TOK_TYPE_ERROR) {
    (void)astree_remove(fnbody_content, 0);
    if (function->symbol == TOK_TYPE_ERROR) {
      /* TODO(Robert): copy-pasted from astree_propogate_errnode; deduplicate
       * later */
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
  return function;
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

void fill_tag_spec(TypeSpec *spec, TagValue *tagval) {
  spec->base = type_from_tag(tagval->tag);
  spec->alignment = tagval->alignment;
  spec->width = tagval->width;
}

const char *create_unique_name(ASTree *tree) {
  /* TODO(Robert): calculate buffer size */
  char *name = malloc(64 * sizeof(char));
  const char *node_string;
  switch (tree->symbol) {
    case TOK_TYPE_NAME:
      node_string = "type_name";
      break;
    case TOK_STRUCT:
      node_string = "struct";
      break;
    case TOK_ENUM:
      node_string = "enum";
      break;
    case TOK_UNION:
      node_string = "union";
      break;
    default:
      node_string = NULL;
  }
  if (node_string == NULL) {
    fprintf(stderr, "ERROR: unable to create unqiue name for symbol %s\n",
            parser_get_tname(tree->symbol));
    abort();
  }

  sprintf(name, "%lu_%lu_%lu_%s", tree->loc.filenr, tree->loc.linenr,
          tree->loc.offset, node_string);
  return name;
}

ASTree *validate_unique_tag(ASTree *tag_type_node, ASTree *left_brace) {
  const char *tag_name = create_unique_name(tag_type_node);
  const size_t tag_name_len = strlen(tag_name);
  TagType tag_type = tag_from_symbol(tag_type_node->symbol);
  /* TODO(Robert): error handling */
  TagValue *tagval = tag_value_init(tag_type);
  tag_type_node->type = calloc(1, sizeof(TypeSpec));
  assert(!typespec_init((TypeSpec *)tag_type_node->type));
  AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
  tag_aux->aux = aux_from_tag(tag_type);
  tag_aux->data.tag.name = tag_name;
  tag_aux->data.tag.val = tagval;
  assert(
      !llist_push_back((LinkedList *)&tag_type_node->type->auxspecs, tag_aux));
  fill_tag_spec((TypeSpec *)tag_type_node->type, tagval);
  if (tag_type == TAG_ENUM) {
    /* remove struct/union member tables from scope stack */
    SymbolTable *top_scope = state_peek_table(state);
    while (top_scope->type == MEMBER_TABLE) {
      assert(!llist_push_back(&tagval->data.enumerators.struct_name_spaces,
                              top_scope));
      assert(!state_pop_table(state));
      top_scope = state_peek_table(state);
    }
    tagval->width = 4;
    tagval->alignment = 4;
  } else {
    int status = state_push_table(state, tagval->data.members.by_name);
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
            errcode_from_tagtype(exists->tag), 2, tag_type_node, tag_name_node);
      } else {
        tag_type_node->type = calloc(1, sizeof(TypeSpec));
        assert(!typespec_init((TypeSpec *)tag_type_node->type));
        fill_tag_spec((TypeSpec *)tag_type_node->type, exists);
        AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
        tag_aux->aux = aux_from_tag(tag_type);
        tag_aux->data.tag.name = tag_name;
        tag_aux->data.tag.val = exists;

        assert(!llist_push_back((LinkedList *)&tag_type_node->type->auxspecs,
                                tag_aux));
        return astree_adopt(tag_type_node, 1, tag_name_node);
      }
    } else {
      /* TODO(Robert): error handling */
      TagValue *tagval = NULL;
      if (tag_type != exists->tag) {
        tagval = tag_value_init(tag_type);
        assert(!state_insert_tag(state, tag_name, tag_name_len, tagval));
      } else {
        tagval = exists;
      }
      tag_type_node->type = calloc(1, sizeof(TypeSpec));
      assert(!typespec_init((TypeSpec *)tag_type_node->type));
      fill_tag_spec((TypeSpec *)tag_type_node->type, tagval);
      AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
      tag_aux->aux = aux_from_tag(tag_type);
      tag_aux->data.tag.name = tag_name;
      tag_aux->data.tag.val = tagval;
      assert(!llist_push_back((LinkedList *)&tag_type_node->type->auxspecs,
                              tag_aux));
      return astree_adopt(tag_type_node, 1, tag_name_node);
    }
  } else if (tag_type != TAG_ENUM) {
    /* TODO(Robert): error handling */
    TagValue *tagval = tag_value_init(tag_type);
    assert(!state_insert_tag(state, tag_name, tag_name_len, tagval));
    tag_type_node->type = calloc(1, sizeof(TypeSpec));
    assert(!typespec_init((TypeSpec *)tag_type_node->type));
    fill_tag_spec((TypeSpec *)tag_type_node->type, tagval);
    AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
    tag_aux->aux = aux_from_tag(tag_type);
    tag_aux->data.tag.name = tag_name;
    tag_aux->data.tag.val = tagval;
    assert(!llist_push_back((LinkedList *)&tag_type_node->type->auxspecs,
                            tag_aux));
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
    TagValue *tagval = tag_value_init(tag_type);
    if (exists) {
      tagval = exists;
    } else {
      tagval = tag_value_init(tag_type);
      assert(!state_insert_tag(state, tag_name, tag_name_len, tagval));
    }
    tag_type_node->type = calloc(1, sizeof(TypeSpec));
    assert(!typespec_init((TypeSpec *)tag_type_node->type));
    fill_tag_spec((TypeSpec *)tag_type_node->type, tagval);
    AuxSpec *tag_aux = calloc(1, sizeof(AuxSpec));
    tag_aux->aux = aux_from_tag(tag_type);
    tag_aux->data.tag.name = tag_name;
    tag_aux->data.tag.val = tagval;
    assert(!llist_push_back((LinkedList *)&tag_type_node->type->auxspecs,
                            tag_aux));
    if (tag_type == TAG_ENUM) {
      /* remove struct/union member tables from scope stack */
      SymbolTable *top_scope = state_peek_table(state);
      while (top_scope->type == MEMBER_TABLE) {
        assert(!llist_push_back(&tagval->data.enumerators.struct_name_spaces,
                                top_scope));
        assert(!state_pop_table(state));
        top_scope = state_peek_table(state);
      }
      tagval->width = 4;
      tagval->alignment = 4;
    } else {
      int status = state_push_table(state, tagval->data.members.by_name);
    }
    return astree_adopt(tag_type_node, 1, tag_name_node);
  }
}

ASTree *finalize_tag_def(ASTree *tag) {
  ASTree *errnode = NULL;
  if (tag->symbol == TOK_TYPE_ERROR) {
    errnode = tag;
    tag = astree_get(tag, 0);
  }
  TypeSpec *tag_type = (TypeSpec *)tag->type;
  AuxSpec *tag_aux = llist_back(&tag_type->auxspecs);
  TagValue *tagval = tag_aux->data.tag.val;
  tag_type->base = type_from_tag(tagval->tag);
  tag_type->alignment = tagval->alignment;
  tag_type->width = tagval->width;
  tagval->is_defined = 1;
  if (tagval->tag == TAG_ENUM) {
    /* push struct/union member tables back onto the scope stack */
    while (!llist_empty(&tagval->data.enumerators.struct_name_spaces)) {
      SymbolTable *member_scope =
          llist_pop_back(&tagval->data.enumerators.struct_name_spaces);
      assert(member_scope != NULL);
      assert(!state_push_table(state, member_scope));
    }
  } else {
    assert(!state_pop_table(state));
    /* pad aggregate so that it can tile an array */
    if (tagval->alignment != 0) {
      size_t padding = tagval->alignment - (tagval->width % tagval->alignment);
      if (padding != tagval->alignment) tagval->width += padding;
    }
  }
  return errnode == NULL ? tag : errnode;
}

ASTree *define_enumerator(ASTree *enum_, ASTree *ident_node, ASTree *equal_sign,
                          ASTree *expr) {
  ASTree *left_brace =
      astree_get(enum_, astree_count(UNWRAP(enum_)) == 2 ? 1 : 0);
  if (enum_->symbol == TOK_TYPE_ERROR) {
    ASTree *real_enum = astree_get(enum_, 0);
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
  assert(!auxspec_copy(enum_aux, llist_front(&enum_->type->auxspecs)));
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

  TagValue *tagval = enum_aux->data.tag.val;
  if (equal_sign != NULL) {
    /* TODO(Robert): evaluate enumeration constants */
    if (expr->symbol == TOK_TYPE_ERROR) {
      ASTree *errnode = astree_propogate_errnode(
          astree_adopt(equal_sign, 1, ident_node), expr);
      /* TODO(Robert): have error propogation function handle more complex
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
  ASTree *left_brace =
      astree_get(UNWRAP(struct_), astree_count(UNWRAP(struct_)) == 2 ? 1 : 0);
  if (struct_->symbol == TOK_TYPE_ERROR) {
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
    TypeSpec *spec = (TypeSpec *)declarator->type;
    SymbolValue *symval = sym_from_type(spec);
    size_t member_alignment = typespec_get_alignment(spec);
    size_t member_width = typespec_get_width(spec);
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
    SymbolValue *symval = sym_from_type((TypeSpec *)declarator->type);
    if (symval->flags & SYMFLAG_LINK_NONE) {
      return translate_local_decl(declaration, declarator);
    } else {
      return translate_global_decl(declaration, declarator);
    }
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
