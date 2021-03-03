#include "symtable.h"

#include "astree.h"
#include "debug.h"
#include "attributes.h"
#include "err.h"
#include "lyutils.h"
#include "string.h"
#include "badlib/badllist.h"

struct map * type_names;
struct map * globals;
struct map * locals;
struct llist * tables;
struct llist * string_constants;
int next_block = 1;
const char *DUMMY_FUNCTION = "__DUMMY__";
int TYPE_ATTR_MASK[16];
static const size_t MAX_STRING_LENGTH = 31;
static const size_t DEFAULT_MAP_SIZE = 100;

static int strncmp_wrapper (void *s1, void *s2) {
    return strncmp(s1, s2, MAX_STRING_LENGTH);
}

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

int functions_equal (SymbolValue *f1, SymbolValue *f2) {
    if (llist_size (f1->parameters) != llist_size (f2->parameters)) return 0;
    for (size_t i = 0; i < llist_size (f1->parameters); ++i) {
        if (!types_compatible_smv (llist_get (f1->parameters, i),
                               llist_get (f2->parameters, i)))
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
    for (size_t i = 0; i < llist_size (tree->children); ++i) {
        set_blocknr (llist_get (tree->children, i), nr);
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
    return llist_get (astree_second (function)->children, index);
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
    size_t id_str_len = strnlen(id_str, MAX_STRING_LENGTH);
    SymbolValue * local_id = map_get(locals, (char*)id_str, id_str_len);
    SymbolValue * global_id = map_get(globals, (char*)id_str, id_str_len);

    if (!locals_empty) {
        DEBUGS ('t', "Assigning %s a local value\n", id_str);
        assign_type_id_smv (ident, local_id);
    } else if (!globals_empty) {
        DEBUGS ('t', "Assigning %s a global value", id_str);
        assign_type_id_smv (ident, global_id);
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
            type_value = map_get(type_names, (char*)type_node->lexinfo,
                    strnlen(type_node->lexinfo, MAX_STRING_LENGTH));
            if (type_value) {
                type_node->decl_loc = type_value->loc;
                type_node->attributes[ATTR_TYPEID] = 1;
                identifier->attributes[ATTR_STRUCT] = 1;
                identifier->type_id = type_node->lexinfo;
            } else {
                fprintf (stderr,
                         "ERROR: Type not declared: %s\n",
                         type_node->lexinfo);
                return -1;
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
            type_value = map_get(type_names, (char*)type->lexinfo,
                    strnlen(type->lexinfo, MAX_STRING_LENGTH));
            if (type_value) {
                identifier->attributes[ATTR_STRUCT] = 1;
                identifier->type_id = type->lexinfo;
                type->attributes[ATTR_TYPEID] = 1;
                type->decl_loc = type_value->loc;
            } else {
                fprintf (stderr,
                         "ERROR: type not declared: %s\n",
                         identifier->type_id);
                return -1;
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
    for (size_t i = 0; i < llist_size (block->children); ++i) {
        ASTree *statement = llist_get (block->children, i);
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
    SymbolValue *function =
            function_name == DUMMY_FUNCTION ? NULL :
            map_get(globals, (char*)function_name, strnlen(function_name, MAX_STRING_LENGTH));

    DEBUGS ('t', "Validating next statement/expression");
    switch (statement->symbol) {
        // trees generated my the "statement" production
        case TOK_RETURN:
            if (llist_size (statement->children) <= 0) {
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
                status = types_compatible_ast_smv (statement->first (statement),
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
            if (llist_size (statement->children) == 3)
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
            if (!types_compatible_ast (statement->first (statement),
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
            if (!types_compatible_ast (statement->first (statement),
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
            if (!types_compatible_ast (statement->first (statement),
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
            if (llist_size (statement->children) == 2) {
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
            const char *tid = statement->first (statement)->type_id;
            SymbolValue *struct_def = map_get(type_names, (char*)tid,
                    strnlen(tid, MAX_STRING_LENGTH));
            if (statement->first (statement)->attributes[ATTR_STRUCT] &&
               struct_def) {
                /* make sure field is defined */
                const char *tid2 = statement->second(statement)->lexinfo;
                SymbolValue *field_def = map_get(struct_def->fields,
                        (char*)tid2, strnlen(tid2, MAX_STRING_LENGTH));
                if (field_def) {
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
            llist_push_front (string_constants, (char*)statement->lexinfo);
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
    SymbolValue *function = map_get (globals, (char*)identifier, strnlen(identifier, MAX_STRING_LENGTH));
    if (function) {
        if (llist_size (call->children) - 1 != llist_size (function->parameters)) {
            fprintf (stderr,
                     "ERROR: incorrect number of arguments %s\n",
                     identifier);
            return -1;
        }
        DEBUGS ('t',
                "Validating arguments for call to %s, num call->children: %d",
                identifier,
                llist_size (call->children) - 1);
        for (size_t i = 0; i < llist_size (function->parameters); ++i) {
            DEBUGS ('t', "Validating argument %d", i);
            ASTree *param = llist_get (call->children, i + 1);
            size_t dummy_sequence = 0;
            DEBUGS ('t', "Argument was not null");
            int status =
                    validate_stmt_expr (param, DUMMY_FUNCTION, &dummy_sequence);
            if (status != 0) return status;
            DEBUGS ('t', "Comparing types");
            if (!types_compatible_ast_smv (param, llist_get (function->parameters, i))) {
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
        map_destroy(symbol_value->fields);
    }

    llist_destroy(symbol_value->parameters);
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
    size_t ident_len = strnlen(extract_ident(global), MAX_STRING_LENGTH);
    SymbolValue *exists = map_get(globals, (char*)extract_ident (global), ident_len);
    if (exists) {
        // error; duplicate declaration
        fprintf (stderr,
                 "ERROR: Global var has already been declared: %s\n",
                 extract_ident (global));
        return -1;
    } else {
        DEBUGS('t', "Making global entry for value %s", extract_ident(global));
        ASTree *type = astree_first (global);
        ASTree *identifier = astree_second (global);
        identifier->attributes[ATTR_LVAL] = 1;
        identifier->attributes[ATTR_VARIABLE] = 1;
        int status = validate_type_id (global->first (global),
                                       global->second (global));
        if (status != 0) return status;
        if (llist_size (global->children) == 3) {
            size_t dummy_sequence = 0;
            status = validate_stmt_expr (astree_third (global),
                                         DUMMY_FUNCTION,
                                         &dummy_sequence);
            if (status != 0) return status;
            if (!types_compatible_ast (astree_third (global),
                                   astree_second (global))) {
                fprintf (stderr,
                         "ERROR: Incompatible type for global variable\n");
                return -1;
            }
        }

        SymbolValue *global_value = symbol_value_init (identifier, 0, 0);
        map_insert(globals, (char*)extract_ident(global), ident_len, global_value);
        return 0;
    }
}

int make_structure_entry (ASTree *structure) {
    const char *structure_type = structure->first (structure)->lexinfo;
    size_t structure_type_len = strnlen(structure_type, MAX_STRING_LENGTH);
    DEBUGS ('t', "Defining structure type: %s", *structure_type);
    SymbolValue *structure_value = map_get(type_names, (char*)structure_type, structure_type_len);
    if (structure_value) {
        fprintf (stderr,
                 "ERROR: Duplicate definition of structure %s\n",
                 structure_type);
        return -1;
    } else {
        structure_value = symbol_value_init (astree_first (structure), 0, 0);
        struct map *fields = malloc(sizeof(*fields));
        map_init(fields, DEFAULT_MAP_SIZE, NULL, (void(*)(void*))symbol_value_free, strncmp_wrapper);
        map_insert(type_names, (char*)structure_type, structure_type_len,
                structure_value);
        // start from 2nd child; 1st was type name
        size_t i;
        for (i = 1; i < llist_size (structure->children); ++i) {
            ASTree *field = llist_get (structure->children, i);
            const char *field_id_str = extract_ident (field);
            size_t field_id_str_len = strnlen(field_id_str, MAX_STRING_LENGTH);
            DEBUGS ('t', "Found structure field: %s", field_id_str);
            SymbolValue *field_value = map_get (fields, (char*)field_id_str, field_id_str_len);
            if (field_value) {
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
                field_value = symbol_value_init (astree_second (field), i - 1, 0);
                map_insert(fields, (char*)field_id_str, field_id_str_len, field_value);
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
    size_t fn_tid_ten = strnlen(function->type_id, MAX_STRING_LENGTH);
    int status = validate_type_id (type_id->first (type_id),
                                   type_id->second (type_id));
    SymbolValue *function_entry = NULL;
    if (status != 0) return status;
    if (type_id->attributes[ATTR_ARRAY]) {
        fprintf (stderr,
                 "ERROR: Function %s has an array return type.\n",
                 function_id);
        // TODO(rbergero): keep going if we can
        return -1;
    } else if (type_id->attributes[ATTR_STRUCT]) {
        int empty;
        function_entry = map_get(type_names, (char*)function->type_id, fn_tid_ten);
        if (empty) {
            fprintf (stderr,
                     "ERROR: Structrue has no definition: %s\n",
                     function->type_id);
            return -1;
        }
    }
    DEBUGS('t', "oof");

    function_entry = symbol_value_init (astree_second (type_id), 0, 0);
    map_init(locals, DEFAULT_MAP_SIZE, NULL, (void(*)(void*))symbol_value_free, strncmp_wrapper);
    // check and add parameters; set block
    set_blocknr (astree_second (function), next_block);
    ASTree *params = astree_second (function);
    size_t param_sequence_nr = 0;
    for (size_t i = 0; i < llist_size (params->children); ++i) {
        ASTree *param = llist_get (params->children, i);
        const char *param_id_str = extract_ident (param);
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
        ASTree *param_identifier = astree_second (param);
        param_identifier->attributes[ATTR_PARAM] = 1;
        DEBUGS('t', "Defining function parameter %s", extract_ident (param));
        status = make_local_entry (param, &param_sequence_nr);
        if (status != 0) return status;
        size_t param_id_str_len = strnlen(param_id_str, MAX_STRING_LENGTH);
        SymbolValue *param_entry = map_get(locals, (char*)param_id_str, param_id_str_len);
        llist_push_front(function_entry->parameters, param_entry);
    }

    DEBUGS ('t', "Inserting function entry with block id %u", next_block);
    size_t function_id_len = strnlen(function_id, MAX_STRING_LENGTH);
    SymbolValue *prototype = map_get(globals, (char*)function_id, function_id_len);
    if (prototype) {
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
        } else if (llist_size (function->children) == 3) {
            DEBUGS ('t', "Completing entry for prototype %s", function_id);
            size_t new_sequence_nr = 0;
            status = validate_block (astree_third (function),
                                     function_id,
                                     &new_sequence_nr);
            if (status != 0) return status;
            prototype->has_block = 1;
        }
    } else {
        map_insert (globals, (char*)function_id, function_id_len, function_entry);
        if (llist_size (function->children) == 3) {
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

    llist_push_front (tables, locals);
    locals = NULL;
    ++next_block;
    return 0;
}

int make_local_entry (ASTree *local, size_t *sequence_nr) {
    const char *local_ident = extract_ident(local);
    size_t local_ident_len = strnlen(local_ident, MAX_STRING_LENGTH);
    SymbolValue *existing_entry = map_get(locals, (char*)local_ident, local_ident_len);

    if (existing_entry) {
        DEBUGS('t', "Got symtable entry %p", existing_entry);
        fprintf (stderr,
                 "ERROR: Duplicate declaration of variable %s at location %d\n",
                 extract_ident (local), existing_entry->loc.linenr);
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
        if (llist_size (local->children) == 3) {
            size_t dummy_sequence = 0;
            DEBUGS ('t', "it do have value");
            status = validate_stmt_expr (local->third (local),
                                         DUMMY_FUNCTION,
                                         &dummy_sequence);
            if (status != 0) return status;
            if (!types_compatible_ast (local->third (local),
                                   local->second (local))) {
                fprintf (stderr,
                         "ERROR: Incompatible type for local variable\n");
                return -1;
            }
        }

        SymbolValue *local_value = symbol_value_init (astree_second (local),
                                                      *sequence_nr,
                                                      next_block);
        size_t identifier_len = strnlen(identifier->lexinfo, MAX_STRING_LENGTH);
        map_insert(locals, (char*)identifier->lexinfo, identifier_len, local_value);
        ++sequence_nr;
        return 0;
    }
}

int make_symbol_table (ASTree *root) {
    DEBUGS ('t', "Making symbol table");
    for (size_t i = 0; i < llist_size (root->children); ++i) {
        ASTree *child = llist_get (root->children, i);
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
                if (!types_compatible_ast (astree_first (child),
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
    llist_push_front(tables, globals);
    llist_push_front(tables, type_names);
    return 0;
}

void type_checker_dump_symbols (FILE *out) {
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

void type_checker_init_globals () {
    TYPE_ATTR_MASK[ATTR_INT] = 1;
    TYPE_ATTR_MASK[ATTR_VOID] = 1;
    TYPE_ATTR_MASK[ATTR_STRING] = 1;
    TYPE_ATTR_MASK[ATTR_NULL] = 1;
    TYPE_ATTR_MASK[ATTR_STRUCT] = 1;
    TYPE_ATTR_MASK[ATTR_ARRAY] = 1;
    string_constants = malloc (sizeof(*string_constants));
    llist_init(string_constants, NULL, NULL);
    tables = malloc (sizeof(*tables));
    llist_init(tables, (void(*)(void*))symbol_value_free, NULL);
    globals = malloc (sizeof(*globals));
    map_init(globals, DEFAULT_MAP_SIZE, NULL, (void(*)(void*))symbol_value_free,
            strncmp_wrapper);
    type_names = malloc (sizeof(*type_names));
    map_init(type_names, DEFAULT_MAP_SIZE, NULL, (void(*)(void*))symbol_value_free,
            strncmp_wrapper);
    locals = malloc (sizeof(*locals));
    map_init(locals, DEFAULT_MAP_SIZE, NULL, (void(*)(void*))symbol_value_free,
            strncmp_wrapper);
}

void type_checker_free_globals () {
    DEBUGS ('t', "DESTROYING SYMTABLES");
    /* the destructor for table values was set so we should be able to just free
     * the list, which will call the destructor for every inserted table
     */
    llist_destroy(tables);
    DEBUGS ('t', "  SYMTABLES DESTROYED");
}
