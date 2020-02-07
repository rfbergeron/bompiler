#include "symtable.h"

#include "astree.h"
#include "auxlib.h"
#include "lyutils.h"

khash_t (symbol_table) * type_names;
khash_t (symbol_table) * globals;
khash_t (symbol_table) * locals;
kvec_t (khash_t (symbol_table)) tables;
kvec_t (char *) string_constants;
int next_block = 1;
const char *DUMMY_FUNCTION = "__DUMMY__";
int TYPE_ATTR_MASK[16];

#define types_compatible(x, y)                                                 \
    _Generic((x), \
        int *: types_compatible_attr,        \
        SymbolValue *: types_compatible_smv, \
        ASTree *: _Generic((y), \
            ASTree *: types_compatible_ast, \
            SymbolValue *: types_compatible_ast_smv) \
        ) (x,y)

/*******************************************************************************
 * helper functions
 ******************************************************************************/

FILE *print_attributes (FILE *out, const int *attributes, const char *type_id) {
    for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
        if (attributes[i]) {
            fprintf (out, " %s", attr_map[i]);
            if (i == ATTR_STRUCT) fprintf (out, "(%s)", type_id);
        }
    }
    return out;
}

void *copy_type_attrs (ASTree *parent, ASTree *child) {
    for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
        parent->attributes[i] |= (child->attributes[i] & TYPE_ATTR_MASK[i]);
    }
}

/*******************************************************************************
 * symbol_value functions
 ******************************************************************************/

SymbolValue *symbol_value_init (ASTree *tree,
                                size_t sequence_,
                                size_t block_nr_) {
    SymbolValue *ret = malloc (sizeof (SymbolValue));
    ret->attributes = tree->attributes;
    ret->loc = tree->loc;
    ret->type_id = tree->type_id;
    ret->sequence = sequence_;
    ret->block_nr = block_nr_;
    return ret;
}

void symbol_value_free (SymbolValue *symbol_value_) {
    if (symbol_value_->fields != NULL) {
        for (size_t i = 0; i < symbol_value_->fields; ++i) {
            SymbolValue *field = *(symbol_value_->fields + i);
            if (field != NULL) symbol_value_free (field);
        }
    }
    if (symbol_value_->parameters != NULL) {
        for (size_t i = 0; i < symbol_value_->parameters; ++i) {
            SymbolValue *param = *(symbol_value_->parameters + i);
            if (param != NULL) symbol_value_free (param);
        }
    }
    free (symbol_value_);
}

int symbol_value_has_attr (SymbolValue *symval, enum attr attribute) {
    return symval->attributes[(size_t) attribute];
}

void symbol_value_set_attr (SymbolValue *symval, enum attr attribute) {
    symval->attributes[(size_t) attribute] = 1;
}

FILE *print_symbol_value (FILE *out, const SymbolValue *symval) {
    fprintf (out,
             "{ %u, %u, %u } { %u }",
             symval->loc.filenr,
             symval->loc.linenr,
             symval->loc.offset,
             symval->block_nr);
    print_attributes (out, symval->attributes, symval->type_id);
    return out;
}

/*******************************************************************************
 * type checker functions
 ******************************************************************************/

struct vector *type_checker_get_tables () {
    return tables;
}

struct vector *type_checker_get_string_constants () {
    return string_constants;
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

            if(entry.second->lloc.filenr < min_lloc->second->
                  lloc.filenr) {
                min_lloc = &entry;
            } else if(
                    entry.second->lloc.filenr == min_lloc->second->
                    lloc.filenr &&
                    entry.second->lloc.linenr < min_lloc->second->
                    lloc.linenr) {
                min_lloc = &entry;
            } else if(
                    entry.second->lloc.filenr == min_lloc->second->
                    lloc.filenr &&
                    entry.second->lloc.linenr == min_lloc->second->
                    lloc.linenr &&
                    entry.second->lloc.offset <= min_lloc->second->
                    lloc.offset) {
                min_lloc = &entry;
            }
        }
        DEBUGS('9', "Inserting entry " << *(min_lloc->first));
        sorted.push_back(*min_lloc);
        min_lloc = nullptr;
    }
    return sorted;
}*/

int make_symbol_table (ASTree *root) {
    DEBUGS ('t', "Making symbol table");
    for (size_t i = 0; i < kv_size (root->children); ++i) {
        ASTree *child = kv_A (root->children, i);
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
                status = make_function_entry (child);
                if (status != 0) return status;
                break;
            case TOK_STRUCT:
                status = make_structure_entry (child);
                if (status != 0) return status;
                break;
            case TOK_TYPE_ID:
                status = make_global_entry (child);
                if (status != 0) return status;
                break;
            case '=':
                status = make_global_entry (astree_first (child));
                if (status != 0) return status;
                status = validate_stmt_expr (astree_second (child),
                                             &DUMMY_FUNCTION,
                                             dummy_sequence);
                if (status != 0) return status;
                if (!types_compatible (astree_first (child),
                                       astree_second (child))) {
                    fprintf (stderr, "ERROR: Incompatible types for global\n");
                    return -1;
                } else if (!astree - first (child)->attributes[ATTR_LVAL]) {
                    fprintf (stderr,
                             "ERROR: Global assignment destination is not an "
                             "LVAL\n");
                    return -1;
                }
                // type is the type of the right operand
                copy_type_attrs(child, astree_second(child);
                if(status != 0) return status;
                break;
            default:
                fprintf(stderr, "ERROR: Unexpected symbol at top level: %s\n",
                        parser_get_tname (child->symbol));
                return -1;
        }
    }
    // use local table list to group all tables together
    kv_push (khash_t (symbol_table), tables, globals);
    kv_push (khash_t (symbol_table), tables, type_names);
    return 0;
}

int make_global_entry (ASTree *global) {
    int empty;
    khiter_t k =
            kh_put (symbol_table, globals, extract_indent (global), &empty);
    if (!empty) {
        // error; duplicate declaration
        cerr << "ERROR: Global var has already been declared: "
             << *(extract_ident (global)) << endl;
        return -1;
    } else {
        ASTree *type = astree_first (global);
        ASTree *identifier = astree_second (global);
        identifier->attributes[ATTR_LVAL] = 1;
        identifier->attributes[ATTR_VARIABLE] = 1;
        int status = validate_type_id (global);
        if (status != 0) return status;
        if (kv_size (global->children) == 3) {
            size_t dummy_sequence = 0;
            status = validate_stmt_expr (astree_third (global),
                                         &DUMMY_FUNCTION,
                                         dummy_sequence);
            if (status != 0) return status;
            if (!types_compatible (astree_third (global),
                                   astree_second (global))) {
                cerr << "ERROR: Incompatible type for global variable" << endl;
                return -1;
            }
        }

        SymbolValue *global_value = symbol_value_init (identifier, 0, 0);
        kh_value (globals, k) = global_value;
        return 0;
    }
}

int make_structure_entry (ASTree *structure) {
    const char *structure_type = structure->first ()->lexinfo;
    DEBUGS ('t', "Defining structure type: " << *structure_type);
    int empty;
    khiter_t k = kh_put (symbol_table, type_names, structure_type, &empty);
    if (!empty) {
        cerr << "ERROR: Duplicate definition of structure " << structure_type
             << endl;
        fprintf (stderr,
                 "ERROR: Duplicate definition of structure %s\n",
                 structure_type);
        return -1;
    } else {
        SymbolValue structure_value =
                symbol_value_init (astree_first (structure), 0, 0);
        khash_t (symbol_table) *fields = kh_init (symbol_table);
        kh_val (type_names, k) = structure_value;
        // start from 2nd child; 1st was type name
        for (size_t i = 1; i < kv_size (structure->children); ++i) {
            ASTree *field = kv_A (structure->children, i);
            const char *field_id_str = extract_ident (field);
            DEBUGS ('t', "Found structure field: " << field_id_str);
            khiter_t k2 = kh_put (symbol_table, fields, field, &empty);
            if (!empty) {
                fprintf (stderr,
                         "ERROR: Duplicate declaration of field: %s\n",
                         field_id_str);
                return -1;
            } else {
                int status = validate_type_id (field);
                if (status != 0) return status;
                astree_second (field)->attributes[ATTR_FIELD] = 1;
                astree_second (field)->attributes[ATTR_LVAL] = 1;
                SymbolValue *field_value =
                        symbol_value_init (astree_second (field), i - 1, 0);
                kh_val (fields, k2) = field_value;
                DEBUGS ('t',
                        "Field inserted at %s",
                        astree_second (field)->lexinfo);
            }
        }
        structure_value->fields = fields;
        astree_first (structure)->decl_loc = structure_value->loc;
    }
    return 0;
}

int make_function_entry (ASTree *function) {
    int ret = 0;
    ASTree *type_id = astree_first (function);
    astree_second (type_id)->attributes[ATTR_FUNCTION] = 1;
    const char *function_id = extract_ident (type_id);
    int status = validate_type_id (type_id);
    if (status != 0) return status;
    if (type_id->attributes[ATTR_ARRAY]) {
        fprintf ("ERROR: Function %s has an array return type.\n", function_id);
        // TODO(rbergero): keep going if we can
        return -1;
    } else if (type_id->attributes[ATTR_STRUCT]) {
        int empty;
        khiter_t k =
                kh_put (symbol_table, type_names, function->type_id, &empty);
        if (empty) {
            fprintf (stderr,
                     "ERROR: Structrue has no definition: %s\n",
                     function->type_id);
            return -1;
        }
    }

    SymbolValue *function_entry =
            symbol_value_init (astree_second (type_id), 0, 0);
    locals = kh_init (symbol_table);
    // check and add parameters; set block
    set_block_nr (astree_second (function), next_block);
    ASTree *params = astree_second (function);
    size_t param_sequence_nr = 0;
    for (size_t i = 0; i < kv_size (params->children); ++i) {
        ASTree *param = kv_A (params->children, i);
        const char *param_id_str = extract_ident (param);
        int empty;
        khiter_t k = kh_put (symbol_value, locals, param_id_str, &empty);
        if (!empty) {
            fprintf (stderr,
                     "ERROR: Duplicate declaration of parameter: %s\n",
                     param_id_str);
            return -1;
        }
        ASTree *param_identifier = astree - second (param);
        param_identifier->attributes[ATTR_PARAM] = 1;
        status = make_local_entry (param, param_sequence_nr);
        if (status != 0) return status;
        SymbolValue *param_entry =
                symbol_value_init (astree_second (param), i, next_block);
        kh_val (locals, k) = param_entry;
        kv_push (SymbolValue *, function_entry->parameters, param_entry);
    }

    DEBUGS ('t', "Inserting function entry with block id " << next_block);
    int empty;
    khiter_t k = kh_put (symbol_table, globals, function_id, &empty);
    if (!empty) {
        SymbolValue *prototype = kv_val (globals, k);
        if (!functions_equal (prototype, function_entry)) {
            fprintf ("ERROR: redefinition of function: %s\n", function_id);
            return -1;
        } else if (prototype->has_block) {
            fprintf (stderr,
                     "ERROR: function has already been defined: %s\n",
                     function_id);
            return -1;
        } else if (kv_size (function->children) == 3) {
            DEBUGS ('t', "Completing entry for prototype %s", function_id);
            size_t sequence_nr = 0;
            status = validate_block (astree_third (function),
                                     function_id,
                                     sequence_nr);
            if (status != 0) return status;
            prototype->has_block = true;
        }
    } else {
        kh_val (globals, k) = function_entry;
        if (kv_size (function->children) == 3) {
            DEBUGS ('t', "No protoype; defining function %s", function_id);
            size_t sequence_nr = 0;
            set_block_nr (astree_third (function), next_block);
            status = validate_block (astree_third (function),
                                     function_id,
                                     sequence_nr);
            if (status != 0) return status;
            function_entry->has_block = true;
        }
    }

    kv_push (symbol_table, tables, locals);
    locals = NULL;
    ++next_block;
    return 0;
}

int make_local_entry (ASTree *local, size_t *sequence_nr) {
    if(kh_get(symbol_table, locals, extract_ident(local)) != kh_end(locals) {
        cerr << "ERROR: Duplicate declaration of variable: "
             << *extract_ident (local) << endl;
        return -1;
    } else {
        ASTree *type = local->first ();
        ASTree *identifier = local->second ();
        DEBUGS ('t', "Making entry for local var " << identifier->lexinfo);
        identifier->attributes[ATTR_LVAL] = 1;
        identifier->attributes[ATTR_VARIABLE] = 1;
        int status = validate_type_id (local);
        if (status != 0) return status;
        DEBUGS ('t', "Checking to see if var has an initial value");
        if (kv_size (local->children) == 3) {
            size_t dummy_sequence = 0;
            DEBUGS ('t', "it do have value");
            status = validate_stmt_expr (local->third (),
                                         &DUMMY_FUNCTION,
                                         dummy_sequence);
            if (status != 0) return status;
            if (!types_compatible (local->third (), local->second ())) {
                cerr << "ERROR: Incompatible type for local variable" << endl;
                return -1;
            }
        }

        SymbolValue *local_value = symbol_value_init (astree_second (local),
                                                      sequence_nr,
                                                      next_block);
        khiter_t k =
                kh_put (symbol_table, locals, identifier->lexinfo, &status);
        kh_value (locals, k) = local_value;
        locals->insert ({identifier->lexinfo, local_value});
        ++sequence_nr;
        return 0;
    }
}

int validate_block (ASTree *block,
                    const char *function_name,
                    size_t *sequence_nr) {
    for (ASTree *statement : block->children) {
        int status = validate_stmt_expr (statement, function_name, sequence_nr);
        if (status != 0) return status;
    }
    return 0;
}

int type_checker::validate_stmt_expr (ASTree *statement,
                                      const string *function_name,
                                      size_t &sequence_nr) {
    int status;
    const string *ident;
    symbol_value *function = function_name == &DUMMY_FUNCTION
                                     ? nullptr
                                     : globals->at (function_name);
    DEBUGS ('t', "Validating next statement/expression");
    switch (statement->symbol) {
        // trees generated my the "statement" production
        case TOK_RETURN:
            if (statement->children.empty ()) {
                if (!function->attributes.test ((size_t) attr::VOID)) {
                    cerr << "ERROR: Return statement with value in"
                         << " void function: " << *function_name << endl;
                    return -1;
                }
            } else {
                validate_stmt_expr (statement->first (),
                                    function_name,
                                    sequence_nr);
                status = types_compatible (statement->first (), function);
                if (status == 0) {
                    cerr << "ERROR: Incompatible return type" << endl;
                    return status;
                } else {
                    statement->set_attr (attr::VREG);
                    statement->attributes |=
                            (statement->first ()->attributes & TYPE_ATTR_MASK);
                }
            }
            break;
        case TOK_TYPE_ID:
            statement->second ()->attributes.set ((size_t) attr::LOCAL);
            status = make_local_entry (statement, sequence_nr);
            if (status != 0) return status;
            break;
        case TOK_IF:
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (statement->children.size () == 3)
                status = validate_stmt_expr (statement->third (),
                                             function_name,
                                             sequence_nr);
            if (status != 0) return status;
            break;
        case TOK_WHILE:
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            break;
        case TOK_BLOCK:
            status = validate_block (statement, function_name, sequence_nr);
            if (status != 0) return status;
            break;
        // gray area; vardecls (a statement) and assignments (an
        // expression) can both have an '=' as their root but we handle
        // them the same way
        case '=':
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!types_compatible (statement->first (), statement->second ())) {
                cerr << "ERROR: Incompatible types for tokens: "
                     << parser::get_tname (statement->first ()->symbol) << " "
                     << *(statement->first ()->lexinfo) << " "
                     << parser::get_tname (statement->second ()->symbol) << " "
                     << *(statement->second ()->lexinfo) << endl;
                return -1;
            } else if (!statement->first ()->attributes.test (
                               (size_t) attr::LVAL)) {
                cerr << "ERROR: Destination is not an LVAL" << endl;
                return -1;
            }
            // type is the type of the left operand
            statement->attributes |=
                    (statement->second ()->attributes & TYPE_ATTR_MASK);
            break;
        // here begins the trees made by the "expr" production
        case TOK_EQ:
        case TOK_NE:
            // types can be arbitrary here
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!types_compatible (statement->first (), statement->second ())) {
                cerr << "ERROR: Incompatible types for operator: "
                     << statement->symbol << endl;
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
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!types_compatible (statement->first (), statement->second ())) {
                cerr << "ERROR: Incompatible types for operator: "
                     << statement->symbol << endl;
                return -1;
            } else if (!statement->first ()->attributes.test (
                               (size_t) attr::INT)) {
                cerr << "ERROR: Operator "
                     << parser::get_tname (statement->symbol)
                     << " must have operands of type int" << endl;
                cerr << "Offending operands: "
                     << *(statement->first ()->lexinfo) << " "
                     << *(statement->second ()->lexinfo) << endl;
                return -1;
            }
            break;
        case TOK_NOT:
        case TOK_POS:
        case TOK_NEG:
            // unary operators
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!statement->first ()->attributes.test ((size_t) attr::INT)) {
                cerr << "ERROR: '" << *(statement->lexinfo)
                     << "' argument must be of type int" << endl;
                return -1;
            }
            break;
        case TOK_ALLOC:
            status = validate_type_id (statement->first (), statement);
            if (status != 0) return status;
            if (statement->children.size () == 2) {
                status = validate_stmt_expr (statement->second (),
                                             function_name,
                                             sequence_nr);
                if (status != 0) return status;
                if (!statement->second ()->has_attr (attr::INT) ||
                    statement->second ()->has_attr (attr::ARRAY)) {
                    cerr << "ERROR: alloc size argument must be of"
                         << " type int!" << endl;
                    return -1;
                }
            }
            break;
        case TOK_CALL:
            statement->attributes.set ((size_t) attr::VREG);
            status = validate_call (statement);
            if (status != 0) return status;
            break;
        case TOK_INDEX:
            // evaluate left and right side
            // make sure left is an array and right is an int
            // set type to type of array, minus the array
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (statement->second ()->has_attr (attr::INT)) {
                if (statement->first ()->has_attr (attr::ARRAY)) {
                    statement->attributes |=
                            (statement->first ()->attributes & TYPE_ATTR_MASK);
                    statement->attributes.reset ((size_t) attr::ARRAY);
                } else if (statement->first ()->has_attr (attr::STRING)) {
                    statement->set_attr (attr::INT);
                } else {
                    cerr << "ERROR: only strings and arrays may be"
                         << " indexed" << endl;
                }
            } else {
                cerr << "ERROR: argument to index operator must be"
                     << " of type int" << endl;
            }
            break;
        case TOK_ARROW:
            // evaluate left but not right since right
            // is always an ident
            status = validate_stmt_expr (statement->first (),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (statement->first ()->attributes.test ((size_t) attr::STRUCT) &&
                type_names->find (statement->first ()->type_id) !=
                        type_names->end ()) {
                // get struct def and check to make sure the field
                // is in it,
                symbol_value *struct_def =
                        type_names->at (statement->first ()->type_id);
                if (struct_def->fields->find (statement->second ()->lexinfo) !=
                    struct_def->fields->end ()) {
                    symbol_value *field_def = struct_def->fields->at (
                            statement->second ()->lexinfo);
                    assign_type (statement->second (), field_def);
                    assign_type (statement, field_def);
                } else {
                    cerr << "ERROR: field " << *(statement->second ()->lexinfo)
                         << " is not a member of structure" << endl;
                    return -1;
                }
            } else {
                cerr << "ERROR: structure " << *(statement->first ()->type_id)
                     << " not defined for token "
                     << *(statement->first ()->lexinfo) << endl;
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
            string_constants.push_back (*(statement->lexinfo));
            break;
        case TOK_IDENT:
            status = assign_type (statement);
            if (status != 0) return status;
            break;
        default:
            cerr << "ERROR: UNEXPECTED TOKEN IN STATEMENT/EXPRESSION: "
                 << *(statement->lexinfo) << endl;
            return -1;
    }
    return 0;
}

//TODO: more _Generic? or just suck it up at the call site?
/*int validate_type_id (ASTree *type_id) {
    return validate_type_id (type_id->first (), type_id->second ());
}*/

int validate_type_id (ASTree *type, ASTree *identifier) {
    DEBUGS ('t', "Validating typeid of symbol " << *(identifier->lexinfo));
    if (type->symbol == TOK_ARRAY) {
        DEBUGS ('t', "Setting attribute ARRAY");
        identifier->attributes.set ((size_t) attr::ARRAY);
        type = type->first ();
    }
    ASTree *type_node;
    symbol_value *type_value;
    switch (type->symbol) {
        case TOK_INT:
            DEBUGS ('t', "Setting attribute INT");
            identifier->attributes.set ((size_t) attr::INT);
            break;
        case TOK_STRING:
            DEBUGS ('t', "Setting attribute STRING");
            identifier->attributes.set ((size_t) attr::STRING);
            break;
        case TOK_PTR:
            DEBUGS ('t', "Setting attribute STRUCT");
            type_node = type->first ();
            if (type_names->find (type_node->lexinfo) == type_names->end ()) {
                cerr << "ERROR: Type not declared: " << *(type_node->lexinfo)
                     << endl;
                return -1;
            } else {
                type_value = type_names->at (type_node->lexinfo);
                type_node->decl_loc = type_value->lloc;
                type_node->attributes.set ((size_t) attr::TYPEID);
                identifier->attributes.set ((size_t) attr::STRUCT);
                identifier->type_id = type_node->lexinfo;
            }
            break;
        case TOK_VOID:
            DEBUGS ('t', "Setting attribute VOID");
            if (identifier->attributes.test ((size_t) attr::ARRAY)) {
                cerr << "ERROR: you may not have arrays of type void!" << endl;
                return -1;
            } else if (!identifier->attributes.test ((size_t) attr::FUNCTION)) {
                cerr << "ERROR: variables and fields cannot be of type "
                     << "void!" << endl;
                return -1;
            } else {
                identifier->attributes.set ((size_t) attr::VOID);
            }
            break;
        case TOK_IDENT:
            DEBUGS ('t', "Type is pointer; resolving");
            if (type_names->find (type->lexinfo) == type_names->end ()) {
                cerr << "ERROR: Type not declared: " << *(identifier->type_id)
                     << endl;
                return -1;
            } else {
                symbol_value *type_value = type_names->at (type->lexinfo);
                identifier->set_attr (attr::STRUCT);
                identifier->type_id = type->lexinfo;
                type->set_attr (attr::TYPEID);
                type->decl_loc = type_value->lloc;
            }
            break;
        default:
            cerr << "ERROR: Unhandled type" << endl;
            return -1;
            break;
    }
    identifier->decl_loc = identifier->loc;
    DEBUGS ('t', "id now has attributes: " << identifier->attributes);
    return 0;
}

int validate_call (ASTree *call) {
    const string *identifier = call->first ()->lexinfo;
    // params are at the same level as the id
    vector<ASTree *> params = call->children;
    if (globals->find (identifier) != globals->end ()) {
        symbol_value *function = globals->at (identifier);
        if (params.size () - 1 != function->parameters.size ()) {
            cerr << "ERROR: incorrect number of arugments: " << *identifier
                 << endl;
            return -1;
        }
        DEBUGS ('t',
                "Validating arguments for call to "
                        << *identifier
                        << " num params: " << params.size () - 1);
        for (size_t i = 0; i < function->parameters.size (); ++i) {
            DEBUGS ('t', "Validating argument " << i);
            ASTree *param = params[i + 1];
            size_t dummy_sequence = 0;
            DEBUGS ('t', "Argument was not null");
            int status =
                    validate_stmt_expr (param, &DUMMY_FUNCTION, dummy_sequence);
            if (status != 0) return status;
            DEBUGS ('t', "Comparing types");
            if (!types_compatible (param, function->parameters[i])) {
                cerr << "ERROR: incompatible type for argument: "
                     << *(param->lexinfo) << endl;
                return -1;
            }
        }
        call->attributes |= function->attributes;
        call->first ()->attributes |= function->attributes;
        call->type_id = function->type_id;
        call->first ()->type_id = function->type_id;
        call->first ()->decl_loc = function->lloc;
        return 0;
    } else {
        cerr << "ERROR: Invalid call to function: " << *identifier << endl;
        return -1;
    }
}

int assign_type (ASTree *ident) {
    DEBUGS ('t', "Attempting to assign a type");
    int locals_empty, globals_empty;
    const char *id_str = ident->lexinfo;
    khiter_t locals_k = kh_put (symbol_table, locals, id_str, &locals_empty);
    khiter_t globals_k = kh_put (symbol_table, globals, id_str, &globals_empty);
    if (!locals_empty) {
        DEBUGS ('t', "Assigning %s a local value\n", id_str);
        assign_type(ident, kh_val(locals, locals_k);
    } else if (!globals_empty) {
        DEBUGS ('t', "Assigning %s a global value", id_str);
        assign_type(ident, kh_val(globals, globals_k);
    } else {
        fprintf (stderr,
                 "ERROR: could not resolve symbol: %s %s\n",
                 (ident->lexinfo),
                 parser_get_tname (ident->symbol));
        return -1;
    }
    return 0;
}

int assign_type (ASTree *ident, symbol_value *value) {
    for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
        indent->attributes[i] = value->attributes[i];
    }
    ident->type_id = value->type_id;
    ident->decl_loc = value->loc;
    return 0;
}

int functions_equal (symbol_value *f1, symbol_value *f2) {
    if (f1->parameters.size () != f2->parameters.size ()) return false;
    for (size_t i = 0; i < f1->parameters.size (); ++i) {
        if (!types_compatible (f1->parameters[i], f2->parameters[i]))
            return false;
    }
    return true;
}

int types_compatible_smv (symbol_value *v1, symbol_value *v2) {
    return types_compatible_attr (v1->attributes, v2->attributes);
}

int types_compatible_ast (ASTree *t1, ASTree *t2) {
    if (t1->symbol == TOK_TYPE_ID) t1 = astree_second (t1);
    return types_compatible_attr (t1->attributes, t2->attributes);
}

int types_compatible_ast_smv (ASTree *tree, symbol_value *entry) {
    if (tree->symbol == TOK_TYPE_ID) tree = astree_second (tree);
    return types_compatible_attr (tree->attributes, entry->attributes);
}

int types_compatible_attr (int *a1, int *a2) {
    DEBUGS ('t', "Comparing bitsets: " << a1 << " and " << a2);
    if ((a1[ATTR_ARRAY] || a1[ATTR_STRUCT] || a1[ATTR_STRING] ||
         a1[ATTR_NULL]) &&
        a2[ATTR_NULL]) {
        return 1;
    } else if ((a2[ATTR_ARRAY] || a2[ATTR_STRUCT] || a2[ATTR_STRING] ||
                a2[ATTR_NULL]) &&
               a1[ATTR_NULL]) {
        return 1;
    } else if (a1[ATTR_ARRAY]) !=
            a2[ATTR_ARRAY])) {
        return 0;
    }
    else if (a1[ATTR_VOID] && a2[ATTR_VOID]) {
        return 1;
    } else if (a1[ATTR_STRUCT] && a2[ATTR_STRUCT]) {
        return 1;
    } else if (a1[ATTR_STRING] && a2[ATTR_STRING]) {
        return 1;
    } else if (a1[ATTR_INT] && a2[ATTR_INT]) {
        return 1;
    } else {
        DEBUGS ('t', "OOF");
        return 0;
    }
}

ASTree *extract_param (ASTree *function, size_t index) {
    return kv_A (astree_second (function)->children, index);
}

const char *extract_ident (ASTree *type_id) {
    return astree_second (type_id)->lexinfo;
}

enum attr get_type_attr (const SymbolValue *symval) {
    attr_bitset attributes = symval->attributes;
    int attributes[] = symval->attributes;
    for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
        if (attributes[i] & TYPE_ATTR_MASK[i]) return (enum attr) i
    }
    errx (1, "Symbol entry did not have any type attributes set.");
}

void set_block_nr (ASTree *tree, size_t nr) {
    tree->block_nr = nr;
    for (size_t i = 0; i < kv_size (tree->children); ++i) {
        set_block_nr (kv_A (tree->children, i), nr);
    }
}

void type_checker_dump_symbols (FILE *out) {
    size_t current_block_nr = 0;
    DEBUGS ('s', "Dumping structure types");
    for (khiter_t k = kh_begin (type_names), k != kh_end (type_name), ++k) {
        if(kh_exist(type_names, k) {
            SymbolValue *structure = kh_val (type_name, k);
            fprintf (out, "%s ", kh_key (type_names, k));
            print_symbol_value (out, structure);
            fprintf (out, "\n");
            for (khiter_t k2 = kh_begin (structure->fields);
                 k2 != kh_end (structure->fields);
                 ++k2) {
                if (kh_exist (structure->fields, k2)) {
                    fprintf (out, "%s ", kh_key (structure->fields, k));
                    print_symbol_value (out, kh_val (stricture->fields, k));
                    fprintf (out, "\n");
                }
            }
        }
    }

    DEBUGS ('s', "Dumping global declarations");
    for (khiter_t k = kh_begin (globals); k != kh_end (globals)++ k) {
        DEBUGS ('s', "Writing a global value");
        SymbolValue *top = kh_val (globals, k);
        fprintf (out, "%s ", kh_key (globals, k));
        print_symbol_value (out, top);
        fprintf (out, "\n");
        if (astree_second (top)->attributes[ATTR_FUNCTION]) {
            locals = kv_A (tables, current_block_nr);
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
            ++current_block_nr;
            fprintf (out, "\n");
        }
    }
}

void type_checker_init_globals () {
    TYPE_ATTR_MASK[ATTR_INT] = 1;
    TYPE_ATTR_MASK[ATTR_VOID] = 1;
    TYPE_ATTR_MASK[ATTR_STRING] = 1;
    TYPE_ATTR_MASK[ATTR_NULLPTR_T] = 1;
    TYPE_ATTR_MASK[ATTR_STRUCT] = 1;
    TYPE_ATTR_MASK[ATTR_ARRAY] = 1;
    kv_init (string_constants);
    kv_init (tables);
    globals = kh_init (symbol_table);
    type_names = kh_init (symbol_table);
    locals = kh_init (symbol_table);
}

void type_checker_free_globals () {
    DEBUGS ('t', "DESTROYING SYMTABLES");
    for (size_t i = 0; i < kv_size (tables); ++i) {
        khash_t (symbol_table) *locals = kv_A (tables, i);
        for (khiter_t k = kh_begin (locals); k != hk_end (locals); ++k) {
                    if (exists (locals, k))
                        symbol_value_free (kh_val (locals, k));
        }
        kv_destroy (locals);
    }
    DEBUGS ('t', "  SYMTABLES DESTROYED");
}
