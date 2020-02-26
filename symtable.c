#include "symtable.h"

#include "astree.h"
#include "auxlib.h"
#include "err.h"
#include "lyutils.h"

khash_t (SymbolTable) * type_names;
khash_t (SymbolTable) * globals;
khash_t (SymbolTable) * locals;
kvec_t (khash_t (SymbolTable) *) tables;
kvec_t (const char *) string_constants;
int next_block = 1;
const char *DUMMY_FUNCTION = "__DUMMY__";
int TYPE_ATTR_MASK[16];

int types_compatible_attr (int *a1, int *a2) {
    if ((a1[ATTR_ARRAY] || a1[ATTR_STRUCT] || a1[ATTR_STRING] ||
         a1[ATTR_NULL]) &&
        a2[ATTR_NULL]) {
        return 1;
    } else if ((a2[ATTR_ARRAY] || a2[ATTR_STRUCT] || a2[ATTR_STRING] ||
                a2[ATTR_NULL]) &&
               a1[ATTR_NULL]) {
        return 1;
    } else if (a1[ATTR_ARRAY] != a2[ATTR_ARRAY]) {
        return 0;
    } else if (a1[ATTR_VOID] && a2[ATTR_VOID]) {
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

int types_compatible_smv (SymbolValue *v1, SymbolValue *v2) {
    return types_compatible_attr (v1->attributes, v2->attributes);
}

int types_compatible_ast (ASTree *t1, ASTree *t2) {
    if (t1->symbol == TOK_TYPE_ID) t1 = astree_second (t1);
    return types_compatible_attr (t1->attributes, t2->attributes);
}

int types_compatible_ast_smv (ASTree *tree, SymbolValue *entry) {
    if (tree->symbol == TOK_TYPE_ID) tree = astree_second (tree);
    return types_compatible_attr (tree->attributes, entry->attributes);
}

#define types_compatible(x, y)                                                 \
    _Generic((x), \
        int *: types_compatible_attr,        \
        SymbolValue *: types_compatible_smv, \
        ASTree *: _Generic((y), \
            ASTree *: types_compatible_ast, \
            SymbolValue *: types_compatible_ast_smv) \
        ) (x,y)

int functions_equal (SymbolValue *f1, SymbolValue *f2) {
    if (kv_size (f1->parameters) != kv_size (f2->parameters)) return 0;
    for (size_t i = 0; i < kv_size (f1->parameters); ++i) {
        if (!types_compatible (kv_A (f1->parameters, i),
                               kv_A (f2->parameters, i)))
            return 0;
    }
    return 0;
}

enum attr get_type_attr (const SymbolValue *symval) {
    for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
        if (symval->attributes[i] & TYPE_ATTR_MASK[i]) return (enum attr) i;
    }
    errx (1, "Symbol entry did not have any type attributes set.");
}

void set_blocknr (ASTree *tree, size_t nr) {
    tree->blocknr = nr;
    for (size_t i = 0; i < kv_size (tree->children); ++i) {
        set_blocknr (kv_A (tree->children, i), nr);
    }
}

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

ASTree *extract_param (ASTree *function, size_t index) {
    return kv_A (astree_second (function)->children, index);
}

const char *extract_ident (ASTree *type_id) {
    return astree_second (type_id)->lexinfo;
}

int assign_type_id_smv (ASTree *ident, SymbolValue *value) {
    for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
        ident->attributes[i] = value->attributes[i];
    }
    ident->type_id = value->type_id;
    ident->decl_loc = value->loc;
    return 0;
}

int assign_type_id (ASTree *ident) {
    DEBUGS ('t', "Attempting to assign a type");
    int locals_empty, globals_empty;
    const char *id_str = ident->lexinfo;
    khiter_t locals_k = kh_put (SymbolTable, locals, id_str, &locals_empty);
    khiter_t globals_k = kh_put (SymbolTable, globals, id_str, &globals_empty);
    if (!locals_empty) {
        DEBUGS ('t', "Assigning %s a local value\n", id_str);
        assign_type_id_smv (ident, kh_val (locals, locals_k));
    } else if (!globals_empty) {
        DEBUGS ('t', "Assigning %s a global value", id_str);
        assign_type_id_smv (ident, kh_val (globals, globals_k));
    } else {
        fprintf (stderr,
                 "ERROR: could not resolve symbol: %s %s\n",
                 (ident->lexinfo),
                 parser_get_tname (ident->symbol));
        return -1;
    }
    return 0;
}

int validate_type_id (ASTree *type, ASTree *identifier);
int validate_call (ASTree *call);
int validate_block (ASTree *block,
                    const char *function_name,
                    size_t *sequence_nr);
int validate_stmt_expr (ASTree *statement,
                        const char *function_name,
                        size_t *sequence_nr);
int make_local_entry (ASTree *local, size_t *sequence_nr);
int make_global_entry (ASTree *global);
int make_function_entry (ASTree *function);
int make_structure_entry (ASTree *structure);

int validate_type_id (ASTree *type, ASTree *identifier) {
    DEBUGS ('t', "Validating typeid of symbol %s", identifier->lexinfo);
    if (type->symbol == TOK_ARRAY) {
        DEBUGS ('t', "Setting attribute ARRAY");
        identifier->attributes[ATTR_ARRAY] = 1;
        type = type->first (type);
    }
    ASTree *type_node = NULL;
    SymbolValue *type_value = NULL;
    khiter_t k;
    switch (type->symbol) {
        case TOK_INT:
            DEBUGS ('t', "Setting attribute INT");
            identifier->attributes[ATTR_INT] = 1;
            break;
        case TOK_STRING:
            DEBUGS ('t', "Setting attribute STRING");
            identifier->attributes[ATTR_STRING] = 1;
            break;
        case TOK_PTR:
            DEBUGS ('t', "Setting attribute STRUCT");
            type_node = type->first (type);
            k = kh_get (SymbolTable, type_names, type_node->lexinfo);
            if (k == kh_end (type_names)) {
                fprintf (stderr,
                         "ERROR: Type not declared: %s\n",
                         type_node->lexinfo);
                return -1;
            } else {
                type_value = kh_value (type_names, k);
                type_node->decl_loc = type_value->loc;
                type_node->attributes[ATTR_TYPEID] = 1;
                identifier->attributes[ATTR_STRUCT] = 1;
                identifier->type_id = type_node->lexinfo;
            }
            break;
        case TOK_VOID:
            DEBUGS ('t', "Setting attribute VOID");
            if (identifier->attributes[ATTR_ARRAY]) {
                fprintf (stderr,
                         "ERROR: you may not have arrays of type void!\n");
                return -1;
            } else if (!identifier->attributes[ATTR_FUNCTION]) {
                fprintf (stderr,
                         "ERROR: variables and fields cannot be of type "
                         "void!\n");
                return -1;
            } else {
                identifier->attributes[ATTR_VOID] = 1;
            }
            break;
        case TOK_IDENT:
            DEBUGS ('t', "Type is pointer; resolving");
            k = kh_get (SymbolTable, type_names, type->lexinfo);
            if (k == kh_end (type_names)) {
                fprintf (stderr,
                         "ERROR: type not declared: %s\n",
                         identifier->type_id);
                return -1;
            } else {
                SymbolValue *type_value = kh_value (type_names, k);
                identifier->attributes[ATTR_STRUCT] = 1;
                identifier->type_id = type->lexinfo;
                type->attributes[ATTR_TYPEID] = 1;
                type->decl_loc = type_value->loc;
            }
            break;
        default:
            fprintf (stderr, "ERROR: unhandled type\n");
            return -1;
            break;
    }
    identifier->decl_loc = identifier->loc;
    return 0;
}

int validate_block (ASTree *block,
                    const char *function_name,
                    size_t *sequence_nr) {
    for (size_t i = 0; i < kv_size (block->children); ++i) {
        ASTree *statement = kv_A (block->children, i);
        int status = validate_stmt_expr (statement, function_name, sequence_nr);
        if (status != 0) return status;
    }
    return 0;
}

int validate_stmt_expr (ASTree *statement,
                        const char *function_name,
                        size_t *sequence_nr) {
    int status;
    const char *ident;
    khiter_t fn_k = kh_put (SymbolTable, globals, function_name, &status);
    SymbolValue *function =
            function_name == DUMMY_FUNCTION ? NULL : kh_value (globals, fn_k);
    DEBUGS ('t', "Validating next statement/expression");
    switch (statement->symbol) {
        // trees generated my the "statement" production
        case TOK_RETURN:
            if (kv_size (statement->children) <= 0) {
                if (!function->attributes[ATTR_VOID]) {
                    fprintf (stderr,
                             "ERROR: Return statement with value in void "
                             "function: %s\n",
                             function_name);
                    return -1;
                }
            } else {
                validate_stmt_expr (statement->first (statement),
                                    function_name,
                                    sequence_nr);
                status = types_compatible (statement->first (statement),
                                           function);
                if (status == 0) {
                    fprintf (stderr, "ERROR: Incompatible return type\n");
                    return status;
                } else {
                    statement->attributes[ATTR_VREG] = 1;
                    copy_type_attrs (statement, statement->first (statement));
                }
            }
            break;
        case TOK_TYPE_ID:
            statement->second (statement)->attributes[ATTR_LOCAL] = 1;
            status = make_local_entry (statement, sequence_nr);
            if (status != 0) return status;
            break;
        case TOK_IF:
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (kv_size (statement->children) == 3)
                status = validate_stmt_expr (statement->third (statement),
                                             function_name,
                                             sequence_nr);
            if (status != 0) return status;
            break;
        case TOK_WHILE:
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (statement),
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
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!types_compatible (statement->first (statement),
                                   statement->second (statement))) {
                fprintf (
                        stderr,
                        "ERROR: Incompatible types for tokens: %s,%s %s,%s\n",
                        parser_get_tname (statement->first (statement)->symbol),
                        statement->first (statement)->lexinfo,
                        parser_get_tname (
                                statement->second (statement)->symbol),
                        statement->second (statement)->lexinfo);
                return -1;
            } else if (!statement->first (statement)->attributes[ATTR_LVAL]) {
                fprintf (stderr, "ERROR: Destination is not an LVAL\n");
                return -1;
            }
            // type is the type of the left operand
            copy_type_attrs (statement, statement->second (statement));
            break;
        // here begins the trees made by the "expr" production
        case TOK_EQ:
        case TOK_NE:
            // types can be arbitrary here
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!types_compatible (statement->first (statement),
                                   statement->second (statement))) {
                fprintf (stderr,
                         "ERROR: Incompatible types for operator: %s\n",
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
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!types_compatible (statement->first (statement),
                                   statement->second (statement))) {
                fprintf (stderr,
                         "ERROR: Incompatible types for operator: %s\n",
                         statement->symbol);
                return -1;
            } else if (!statement->first (statement)->attributes[ATTR_INT]) {
                fprintf (stderr,
                         "ERROR: Operator %s must have operands of type int\n",
                         parser_get_tname (statement->symbol));
                fprintf (stderr,
                         "Offending operands: %s %s\n",
                         statement->first (statement)->lexinfo,
                         statement->second (statement)->lexinfo);
                return -1;
            }
            break;
        case TOK_NOT:
        case TOK_POS:
        case TOK_NEG:
            // unary operators
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (!statement->first (statement)->attributes[ATTR_INT]) {
                fprintf (stderr,
                         "ERROR: '%s' argument must be of type int\n",
                         statement->lexinfo);
                return -1;
            }
            break;
        case TOK_ALLOC:
            status = validate_type_id (statement->first (statement), statement);
            if (status != 0) return status;
            if (kv_size (statement->children) == 2) {
                status = validate_stmt_expr (statement->second (statement),
                                             function_name,
                                             sequence_nr);
                if (status != 0) return status;
                if (!statement->second (statement)->attributes[ATTR_INT] ||
                    statement->second (statement)->attributes[ATTR_ARRAY]) {
                    fprintf (stderr,
                             "ERROR: alloc size argument must be of type int!");
                    return -1;
                }
            }
            break;
        case TOK_CALL:
            statement->attributes[ATTR_VREG] = 1;
            status = validate_call (statement);
            if (status != 0) return status;
            break;
        case TOK_INDEX:
            // evaluate left and right side
            // make sure left is an array and right is an int
            // set type to type of array, minus the array
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            status = validate_stmt_expr (statement->second (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            if (statement->second (statement)->attributes[ATTR_INT]) {
                if (statement->first (statement)->attributes[ATTR_ARRAY]) {
                    copy_type_attrs (statement, statement->first (statement));
                    statement->attributes[ATTR_ARRAY] = 0;
                } else if (statement->first (statement)
                                   ->attributes[ATTR_STRING]) {
                    statement->attributes[ATTR_INT] = 1;
                } else {
                    fprintf (stderr,
                             "ERROR: only strings and arrays may be indexed\n");
                }
            } else {
                fprintf (stderr,
                         "ERROR: argument to index operator must be of type "
                         "int\n");
            }
            break;
        case TOK_ARROW:
            // evaluate left but not right since right
            // is always an ident
            status = validate_stmt_expr (statement->first (statement),
                                         function_name,
                                         sequence_nr);
            if (status != 0) return status;
            khiter_t k = kh_get (SymbolTable,
                                 type_names,
                                 statement->first (statement)->type_id);
            if (statement->first (statement)->attributes[ATTR_STRUCT] &&
                k != kh_end (type_names)) {
                // get struct def and check to make sure the field
                // is in it,
                SymbolValue *struct_def = kh_value (type_names, k);
                khiter_t k2 = kh_get (SymbolTable,
                                      struct_def->fields,
                                      statement->second (statement)->lexinfo);
                if (k2 != kh_end (struct_def->fields)) {
                    SymbolValue *field_def = kh_val (struct_def->fields, k2);
                    assign_type_id_smv (statement->second (statement),
                                        field_def);
                    assign_type_id_smv (statement, field_def);
                } else {
                    fprintf (stderr,
                             "ERROR: field %s is not a member of structure\n",
                             statement->second (statement)->lexinfo);
                    return -1;
                }
            } else {
                fprintf (stderr,
                         "ERROR: structure %s not defined for token %s\n",
                         statement->first (statement)->type_id,
                         statement->first (statement)->lexinfo);
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
            kv_push (const char *, string_constants, statement->lexinfo);
            break;
        case TOK_IDENT:
            status = assign_type_id (statement);
            if (status != 0) return status;
            break;
        default:
            fprintf (stderr,
                     "ERROR: UNEXPECTED TOKEN IN STATEMENT/EXPRESSION: %s\n",
                     statement->lexinfo);
            return -1;
    }
    return 0;
}

int validate_call (ASTree *call) {
    const char *identifier = call->first (call)->lexinfo;
    // params are at the same level as the id
    khiter_t k = kh_get (SymbolTable, globals, identifier);
    if (k != kh_end (globals)) {
        SymbolValue *function = kh_value (globals, k);
        if (kv_size (call->children) - 1 != kv_size (function->parameters)) {
            fprintf (stderr,
                     "ERROR: incorrect number of arguments %s\n",
                     identifier);
            return -1;
        }
        DEBUGS ('t',
                "Validating arguments for call to %s, num call->children: %d",
                identifier,
                kv_size (call->children) - 1);
        for (size_t i = 0; i < kv_size (function->parameters); ++i) {
            DEBUGS ('t', "Validating argument %d", i);
            ASTree *param = kv_A (call->children, i + 1);
            size_t dummy_sequence = 0;
            DEBUGS ('t', "Argument was not null");
            int status =
                    validate_stmt_expr (param, DUMMY_FUNCTION, &dummy_sequence);
            if (status != 0) return status;
            DEBUGS ('t', "Comparing types");
            if (!types_compatible (param, kv_A (function->parameters, i))) {
                fprintf (stderr,
                         "ERROR: incompatible type for argument: %s\n",
                         param->lexinfo);
                return -1;
            }
        }

        memcpy (call->attributes, function->attributes, NUM_ATTRIBUTES);
        memcpy (call->first (call)->attributes,
                function->attributes,
                NUM_ATTRIBUTES);
        call->type_id = function->type_id;
        call->first (call)->type_id = function->type_id;
        call->first (call)->decl_loc = function->loc;
        return 0;
    } else {
        fprintf (stderr, "ERROR: Invalid call to function %s\n", identifier);
        return -1;
    }
}

/*******************************************************************************
 * symbol_value functions
 ******************************************************************************/

SymbolValue *symbol_value_init (ASTree *tree,
                                size_t sequence_,
                                size_t blocknr_) {
    SymbolValue *ret = malloc (sizeof (SymbolValue));
    memcpy (ret->attributes, tree->attributes, NUM_ATTRIBUTES);
    ret->loc = tree->loc;
    ret->type_id = tree->type_id;
    ret->sequence = sequence_;
    ret->blocknr = blocknr_;
    return ret;
}

void symbol_value_free (SymbolValue *symbol_value) {
    if (symbol_value->fields != NULL) {
        for (khiter_t i = 0; i != kh_end (symbol_value->fields); ++i) {
            if (kh_exist (symbol_value->fields, i))
                free ((SymbolValue *) kh_value (symbol_value->fields, i));
        }
    }

    while (kv_size (symbol_value->parameters) > 0)
        free (kv_pop (symbol_value->parameters));
    free (symbol_value);
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
             symval->blocknr);
    print_attributes (out, symval->attributes, symval->type_id);
    return out;
}

/*******************************************************************************
 * type checker functions
 ******************************************************************************/

/*kvec_t (khash_t (SymbolTable)) type_checker_get_tables () {
    return tables;
}

kvec_t (char *) type_checker_get_string_constants () {
    return string_constants;
}*/

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

int make_global_entry (ASTree *global) {
    int empty;
    khiter_t k = kh_put (SymbolTable, globals, extract_ident (global), &empty);
    if (!empty) {
        // error; duplicate declaration
        fprintf (stderr,
                 "ERROR: Global var has already been declared: %s\n",
                 extract_ident (global));
        return -1;
    } else {
        ASTree *type = astree_first (global);
        ASTree *identifier = astree_second (global);
        identifier->attributes[ATTR_LVAL] = 1;
        identifier->attributes[ATTR_VARIABLE] = 1;
        int status = validate_type_id (global->first (global),
                                       global->second (global));
        if (status != 0) return status;
        if (kv_size (global->children) == 3) {
            size_t dummy_sequence = 0;
            status = validate_stmt_expr (astree_third (global),
                                         DUMMY_FUNCTION,
                                         &dummy_sequence);
            if (status != 0) return status;
            if (!types_compatible (astree_third (global),
                                   astree_second (global))) {
                fprintf (stderr,
                         "ERROR: Incompatible type for global variable\n");
                return -1;
            }
        }

        SymbolValue *global_value = symbol_value_init (identifier, 0, 0);
        kh_value (globals, k) = global_value;
        return 0;
    }
}

int make_structure_entry (ASTree *structure) {
    const char *structure_type = structure->first (structure)->lexinfo;
    DEBUGS ('t', "Defining structure type: %s", *structure_type);
    int empty;
    khiter_t k = kh_put (SymbolTable, type_names, structure_type, &empty);
    if (!empty) {
        fprintf (stderr,
                 "ERROR: Duplicate definition of structure %s\n",
                 structure_type);
        return -1;
    } else {
        SymbolValue *structure_value =
                symbol_value_init (astree_first (structure), 0, 0);
        khash_t (SymbolTable) *fields = kh_init (SymbolTable);
        kh_val (type_names, k) = structure_value;
        // start from 2nd child; 1st was type name
        for (size_t i = 1; i < kv_size (structure->children); ++i) {
            ASTree *field = kv_A (structure->children, i);
            const char *field_id_str = extract_ident (field);
            DEBUGS ('t', "Found structure field: %s", field_id_str);
            khiter_t k2 = kh_put (SymbolTable, fields, field_id_str, &empty);
            if (!empty) {
                fprintf (stderr,
                         "ERROR: Duplicate declaration of field: %s\n",
                         field_id_str);
                return -1;
            } else {
                int status = validate_type_id (field->first (field),
                                               field->second (field));
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
    int status = validate_type_id (type_id->first (type_id),
                                   type_id->second (type_id));
    if (status != 0) return status;
    if (type_id->attributes[ATTR_ARRAY]) {
        fprintf (stderr,
                 "ERROR: Function %s has an array return type.\n",
                 function_id);
        // TODO(rbergero): keep going if we can
        return -1;
    } else if (type_id->attributes[ATTR_STRUCT]) {
        int empty;
        khiter_t k =
                kh_put (SymbolTable, type_names, function->type_id, &empty);
        if (empty) {
            fprintf (stderr,
                     "ERROR: Structrue has no definition: %s\n",
                     function->type_id);
            return -1;
        }
    }
    DEBUGS('t', "oof");

    SymbolValue *function_entry =
            symbol_value_init (astree_second (type_id), 0, 0);
    locals = kh_init (SymbolTable);
    // check and add parameters; set block
    set_blocknr (astree_second (function), next_block);
    ASTree *params = astree_second (function);
    size_t param_sequence_nr = 0;
    for (size_t i = 0; i < kv_size (params->children); ++i) {
        ASTree *param = kv_A (params->children, i);
        const char *param_id_str = extract_ident (param);
        int empty;
        khiter_t k = kh_put (SymbolTable, locals, param_id_str, &empty);
        if (!empty) {
            fprintf (stderr,
                     "ERROR: Duplicate declaration of parameter: %s\n",
                     param_id_str);
            return -1;
        }
        ASTree *param_identifier = astree_second (param);
        param_identifier->attributes[ATTR_PARAM] = 1;
        status = make_local_entry (param, &param_sequence_nr);
        if (status != 0) return status;
        SymbolValue *param_entry =
                symbol_value_init (astree_second (param), i, next_block);
        kh_val (locals, k) = param_entry;
        kv_push (SymbolValue *, function_entry->parameters, param_entry);
    }

    DEBUGS ('t', "Inserting function entry with block id %u", next_block);
    int empty;
    khiter_t k = kh_put (SymbolTable, globals, function_id, &empty);
    if (!empty) {
        SymbolValue *prototype = kh_val (globals, k);
        if (!functions_equal (prototype, function_entry)) {
            fprintf (stderr,
                     "ERROR: redefinition of function: %s\n",
                     function_id);
            return -1;
        } else if (prototype->has_block) {
            fprintf (stderr,
                     "ERROR: function has already been defined: %s\n",
                     function_id);
            return -1;
        } else if (kv_size (function->children) == 3) {
            DEBUGS ('t', "Completing entry for prototype %s", function_id);
            size_t new_sequence_nr = 0;
            status = validate_block (astree_third (function),
                                     function_id,
                                     &new_sequence_nr);
            if (status != 0) return status;
            prototype->has_block = 1;
        }
    } else {
        kh_val (globals, k) = function_entry;
        if (kv_size (function->children) == 3) {
            DEBUGS ('t', "No protoype; defining function %s", function_id);
            size_t new_sequence_nr = 0;
            set_blocknr (astree_third (function), next_block);
            status = validate_block (astree_third (function),
                                     function_id,
                                     &new_sequence_nr);
            if (status != 0) return status;
            function_entry->has_block = 1;
        }
    }

    kv_push (khash_t (SymbolTable) *, tables, locals);
    locals = NULL;
    ++next_block;
    return 0;
}

int make_local_entry (ASTree *local, size_t *sequence_nr) {
    if (kh_get (SymbolTable, locals, extract_ident (local)) !=
        kh_end (locals)) {
        fprintf (stderr,
                 "ERROR: Duplicate declaration of variable: %s\n",
                 extract_ident (local));
        return -1;
    } else {
        ASTree *type = local->first (local);
        ASTree *identifier = local->second (local);
        DEBUGS ('t', "Making entry for local var %s", identifier->lexinfo);
        identifier->attributes[ATTR_LVAL] = 1;
        identifier->attributes[ATTR_VARIABLE] = 1;
        int status =
                validate_type_id (local->first (local), local->second (local));
        if (status != 0) return status;
        DEBUGS ('t', "Checking to see if var has an initial value");
        if (kv_size (local->children) == 3) {
            size_t dummy_sequence = 0;
            DEBUGS ('t', "it do have value");
            status = validate_stmt_expr (local->third (local),
                                         DUMMY_FUNCTION,
                                         &dummy_sequence);
            if (status != 0) return status;
            if (!types_compatible (local->third (local),
                                   local->second (local))) {
                fprintf (stderr,
                         "ERROR: Incompatible type for local variable\n");
                return -1;
            }
        }

        SymbolValue *local_value = symbol_value_init (astree_second (local),
                                                      *sequence_nr,
                                                      next_block);
        khiter_t k = kh_put (SymbolTable, locals, identifier->lexinfo, &status);
        kh_value (locals, k) = local_value;
        ++sequence_nr;
        return 0;
    }
}

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
                                             DUMMY_FUNCTION,
                                             &dummy_sequence);
                if (status != 0) return status;
                if (!types_compatible (astree_first (child),
                                       astree_second (child))) {
                    fprintf (stderr, "ERROR: Incompatible types for global\n");
                    return -1;
                } else if (!astree_first (child)->attributes[ATTR_LVAL]) {
                    fprintf (stderr,
                             "ERROR: Global assignment destination is not an "
                             "LVAL\n");
                    return -1;
                }
                // type is the type of the right operand
                copy_type_attrs (child, astree_second (child));
                if (status != 0) return status;
                break;
            default:
                fprintf (stderr,
                         "ERROR: Unexpected symbol at top level: %s\n",
                         parser_get_tname (child->symbol));
                return -1;
        }
    }
    // use local table list to group all tables together
    kv_push (khash_t (SymbolTable) *, tables, globals);
    kv_push (khash_t (SymbolTable) *, tables, type_names);
    return 0;
}

void type_checker_dump_symbols (FILE *out) {
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
        DEBUGS ('s', "Writing a global value");
        SymbolValue *top = kh_val (globals, k);
        fprintf (out, "%s ", kh_key (globals, k));
        print_symbol_value (out, top);
        fprintf (out, "\n");
        if (top->attributes[ATTR_FUNCTION]) {
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
}

void type_checker_init_globals () {
    TYPE_ATTR_MASK[ATTR_INT] = 1;
    TYPE_ATTR_MASK[ATTR_VOID] = 1;
    TYPE_ATTR_MASK[ATTR_STRING] = 1;
    TYPE_ATTR_MASK[ATTR_NULL] = 1;
    TYPE_ATTR_MASK[ATTR_STRUCT] = 1;
    TYPE_ATTR_MASK[ATTR_ARRAY] = 1;
    kv_init (string_constants);
    kv_init (tables);
    globals = kh_init (SymbolTable);
    type_names = kh_init (SymbolTable);
    locals = kh_init (SymbolTable);
}

void type_checker_free_globals () {
    DEBUGS ('t', "DESTROYING SYMTABLES");
    for (size_t i = 0; i < kv_size (tables); ++i) {
        khash_t (SymbolTable) *locals = kv_A (tables, i);
        for (khiter_t k = kh_begin (locals); k != kh_end (locals); ++k) {
            if (kh_exist (locals, k)) symbol_value_free (kh_val (locals, k));
        }
        kh_destroy (SymbolTable, locals);
    }
    DEBUGS ('t', "  SYMTABLES DESTROYED");
}
