#include <utility>
#include <string>
#include <vector>
#include <unordered_map>
#include <iomanip>

#include "astree.h"
#include "lyutils.h"
#include "symtable.h"
#include "auxlib.h"
#include "intlang.h"

using namespace std;
using symbol_table = unordered_map<const string*,symbol_value*>;
using symbol_entry = symbol_table::value_type;

size_t generator::branch_count = 0;
size_t generator::vreg_count = 0;
ostream* generator::out = &cout;
const string* generator::NO_LABEL = new string(" ");
const string* generator::FUN_LABEL = new string(".function ");
const string* generator::STC_LABEL = new string(".struct ");
const string* generator::GLO_LABEL = new string(".global ");
const string* generator::LOC_LABEL = new string(".local ");
const string* generator::STR_LABEL = new string(".string ");
const string* generator::FLD_LABEL = new string(".field ");
vector<string> generator::string_constants;

void generator::set_out(ostream* out_) {
    out = out_;
}

int generator::write_int_lang(astree* root,
        vector<symbol_table*> tables,
        vector<string> string_constants_) {
    DEBUGH('l', "Writing structure definitions");
    string_constants = string_constants_;
    vector<symbol_entry> sorted_structures =
            type_checker::sort_symtable(tables[tables.size()-1]);
    tables.pop_back();
    for(auto&& itor = sorted_structures.cbegin();
            itor != sorted_structures.end(); ++itor) {
        write_struct_decl(*itor);
    }

    DEBUGH('l', "Writing string constant definitions");
    for(size_t i = 0; i < string_constants.size(); ++i) {
        write_string_decl(string_constants[i], i);
    }
    *out << endl;

    DEBUGH('l', "Writing global definitions");
    vector<symbol_entry> sorted_globals =
            type_checker::sort_symtable(tables[tables.size()-1]);
    tables.pop_back();
    // probably unnecessary but since we're going over globals
    vector<symbol_entry> sorted_functions;
    for(auto&& itor = sorted_globals.cbegin();
            itor != sorted_globals.end(); ++itor) {
        if(itor->second->has_attr(attr::FUNCTION))
            sorted_functions.push_back(*itor);
        else
            write_var_decl(*itor);
    }
    *out << endl;

    vector<astree*> statements = root->children;
    for(auto&& itor = statements.cbegin();
            itor != statements.cend(); ++itor) {
        if((*itor)->symbol == TOK_FUNCTION) {
            DEBUGH('l', "Writing a function definition");
            size_t fn_block = (*itor)->second()->block_nr - 1;
            write_function_decl(*itor, tables[fn_block]);
        }
    }
    return 0;
}

int generator::write_var_decl(symbol_entry pair) {
    const string* id = pair.first;
    symbol_value* value = pair.second;
    if(value->has_attr(attr::PARAM)) {
        DEBUGH('l', "Omitting parameter form local declarotions");
    } else if(value->has_attr(attr::LOCAL)) {
        IDLABEL(*LOC_LABEL, write_type(value) << *(id));
    } else if(value->has_attr(attr::FIELD)) {
        IDLABEL(*FLD_LABEL, write_type(value) << *(id));
    } else {
        WRLABEL(*GLO_LABEL, write_type(value) << *(id));
    }
    return 0;
}

int generator::write_string_decl(string s, size_t strcount) {
    WRLABEL(*STR_LABEL, ".s" << strcount << " " << s);
    return 0;
}

int generator::write_struct_decl(symbol_entry pair) {
    const string* id = pair.first;
    symbol_value* value = pair.second;
    symbol_table* fields = value->fields;
    WRLABEL(*STC_LABEL, *(id));
    for(size_t bucket = 0; bucket < fields->bucket_count(); ++bucket) {
        for(auto&& itor = fields->cbegin(bucket);
                itor != fields->cend(bucket); ++itor) {
            write_var_decl(*itor);
        }
    }
    *out << ".end" << endl << endl;
    return 0;
}

int generator::write_function_decl(astree* fun, symbol_table* locals) {
    // write function header (type, name, args, etc)
    astree* fun_name_node = fun->first()->second();
    *out << setw(10) << left << *FUN_LABEL << write_type(fun_name_node)
         << *(fun_name_node->lexinfo) << " (";
    vector<astree*> params = fun->second()->children;
    for(size_t i = 0; i < params.size(); ++i) {
        if(i > 0) *out << ", ";
        astree* param_name_node = params[i]->second();
        *out << write_type(param_name_node)
             << *(param_name_node->lexinfo);
    }
    *out << ")" << endl;

    // write local declarations
    DEBUGH('l', "Emitting local declarations");
    vector<symbol_entry> sorted_locals =
            type_checker::sort_symtable(locals);
    for(auto&& itor = sorted_locals.cbegin(); itor !=
            sorted_locals.cend(); ++itor) {
        write_var_decl(*itor);
    }

    // write block
    if(fun->children.size() == 3) {
        branch_count = 9;
        vreg_count = 0;
        vector<astree*> block_statements = fun->third()->children;
        DEBUGH('l', "Writing block");
        for(auto && itor = block_statements.begin();
                itor != block_statements.end(); ++itor) {
            const string* tmp_label = NO_LABEL;
            write_stmt_expr(*itor, tmp_label);
        }
    }

    // terminate function declaration
    INDENT("return");
    *out << ".end" << endl << endl;
    return 0;
}

string generator::write_stmt_expr(astree* expr, const string*& label,
        bool return_compound) {
    DEBUGH('l', "Writing expression");
    string left_operand;
    string right_operand;
    string source;
    string destination;
    string ret;
    string structure;
    string size;
    string array;
    string index;
    string branch_label;
    string block_label;
    string skip_label;
    string else_label;
    size_t current_branch;
    bool do_compound;
    vector<string> param_strings;
    switch(expr->symbol) {
        case TOK_EQ:
        case TOK_NE:
        case TOK_LE:
        case TOK_GE:
        case TOK_GT:
        case TOK_LT:
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
            DEBUGH('l', "Expression is integer operator");
            left_operand = write_stmt_expr(expr->first(), label);
            right_operand = write_stmt_expr(expr->second(), label);
            ret = left_operand + " " + *(expr->lexinfo) + " "
                + right_operand;

            if(return_compound) {
                return ret;
            } else {
                destination = "$t" + to_string(vreg_count++);
                destination += ":i";
                WRLABEL(*label, destination << " = " << ret);
                if(label != NO_LABEL) label = NO_LABEL;
                return destination;
            }
            break;
        case TOK_NEG:
        case TOK_POS:
        case TOK_NOT:
            DEBUGH('l', "Expression is unary integer operator");
            left_operand = write_stmt_expr(expr->first(), label);
            if(expr->symbol == TOK_NOT)
                ret += "! ";
            else if(expr->symbol == TOK_NEG)
                ret += "-";
            else if(expr->symbol == TOK_POS)
                ret += "+";
            else
                ret += "ERR";
            ret += left_operand;

            if(return_compound) {
                return ret;
            } else {
                destination += "$t" + to_string(vreg_count++);
                destination += ":i";
                WRLABEL(*label, destination << " = " << ret);
                if(label != NO_LABEL) label = NO_LABEL;
                return destination;
            }
            break;
        case '=':
            /* We cannot put the destination returned by the index
             * and arrow operators into a register and then assign
             * values to that destination; it must be the immediate
             * value on the left hand side of an assignment operator.
             * So, if the lhs is an arrow or index, the rhs must be
             * a simple expression (const value or register).
             *
             * If the rhs is also an index or arrow, that is fine;
             * extract the value, put it in a register, and assign.
             */
            DEBUGH('l', "Expression is assignment");
            do_compound = (expr->first()->symbol == TOK_INDEX) ||
                          (expr->first()->symbol == TOK_ARROW);
            destination = write_stmt_expr(expr->first(), label,
                    do_compound);
            source = write_stmt_expr(expr->second(), label,
                    not do_compound);

            WRLABEL(*label, destination << " = " << source);
            if(label != NO_LABEL) label = NO_LABEL;
            return source;
            break;
        case TOK_TYPE_ID:
            DEBUGH('l', "Expression is declaration");
            if(expr->children.size() == 3) {
                DEBUGH('l', "Assignment occurs at declaration");

                source = write_stmt_expr(expr->third(), label, true);

                WRLABEL(*label, *(expr->second()->lexinfo) << " = "
                        << source);
                if(label != NO_LABEL) label = NO_LABEL;
                return *(expr->second()->lexinfo);
            }
            return "";
            break;
        case TOK_ALLOC:
            DEBUGH('l', "Expression is alloc");
            ret = "malloc(";
            if(expr->has_attr(attr::ARRAY)) {
                right_operand = write_stmt_expr(expr->second(), label);
                size = "$t" + to_string(vreg_count++) + ":i";

                if(expr->has_attr(attr::STRUCT)) {
                    left_operand = "sizeof struct ";
                    left_operand += *(expr->type_id);
                } else if(expr->has_attr(attr::INT)) {
                    left_operand = "sizeof int";
                } else if(expr->has_attr(attr::STRING)) {
                    left_operand = "sizeof void*";
                } else {
                    left_operand = "ERR";
                }

                WRLABEL(*label, size << " = " << right_operand
                        << " * " << left_operand);
                if(label != NO_LABEL) label = NO_LABEL;
            } else if(expr->has_attr(attr::STRING)) {
                size = write_stmt_expr(expr->second(), label);
            } else if(expr->has_attr(attr::STRUCT)) {
                size = "$t" + to_string(vreg_count++) + ":i";
                right_operand = "sizeof struct ";
                right_operand += *(expr->first()->lexinfo);
                WRLABEL(*label, size << " = " << right_operand);
                if(label != NO_LABEL) label = NO_LABEL;
            } else {
                size = "ERR";
            }
            ret += size + ")";

            if(return_compound) {
                return ret;
            } else {
                destination = "$t" + to_string(vreg_count++);
                destination += ":p";
                WRLABEL(*label, destination << " = " << ret);
                if(label != NO_LABEL) label = NO_LABEL;
                return destination;
            }
            break;
        case TOK_ARROW:
            DEBUGH('l', "Expression is arrow");
            structure = write_stmt_expr(expr->first(), label);
            if(return_compound) {
                return structure + "->" + *(expr->first()->type_id)
                    + "::" + *(expr->second()->lexinfo);
            } else {
                destination = "$t" + to_string(vreg_count++);
                destination += write_reg_type(expr->second());
                WRLABEL(*label, destination << " = " << structure
                        << "->" << *(expr->first()->type_id) << "::"
                        << *(expr->second()->lexinfo));
                if(label != NO_LABEL) label = NO_LABEL;
                return destination;
            }
            break;
        case TOK_INDEX:
            DEBUGH('l', "Expression is index");
            array = write_stmt_expr(expr->first(), label);
            index = write_stmt_expr(expr->second(), label);
            // typesize is size of returned value, not of array itself
            ret += array + "[" + index + " * " +
                write_stride(expr, expr->first()) + "]";
            if(return_compound) {
                return ret;
            } else {
                destination = "$t" + to_string(vreg_count++);
                destination += write_reg_type(expr);
                WRLABEL(*label, destination << " = " << ret);
                if(label != NO_LABEL) label = NO_LABEL;
                return destination;
            }
            break;
        case TOK_RETURN:
            DEBUGH('l', "Expression is return");
            if(expr->children.size() == 1) {
                ret += " ";
                ret +=  write_stmt_expr(expr->first(), label);
            }
            WRLABEL(*label, "return" << ret);
            if(label != NO_LABEL) label = NO_LABEL;
            return "";
            break;
        case TOK_CALL:
            DEBUGH('l', "Expression is a function call");
            for(size_t i = 1; i < expr->children.size(); ++i) {
                param_strings.push_back(write_stmt_expr(
                            expr->children[i], label));
            }
            ret += *(expr->first()->lexinfo) + "(";
            for(size_t i = 0; i < param_strings.size(); ++i) {
                if(i > 0) ret += ", ";
                ret += param_strings[i];
            }
            ret += ")";
            if(expr->has_attr(attr::VOID)) {
                WRLABEL(*label, ret);
                if(label != NO_LABEL) label = NO_LABEL;
                return "";
            } else if(return_compound) {
                return ret;
            } else {
                destination += "$t" + to_string(vreg_count++);
                destination += write_reg_type(expr->first());
                WRLABEL(*label, destination << " = " << ret);
                if(label != NO_LABEL) label = NO_LABEL;
                return destination;
            }
            break;
        case TOK_WHILE:
            DEBUGH('l', "Expression is a while loop");
            // branch label is written by the children?
            current_branch = branch_count++;
            branch_label = ".wh" + to_string(current_branch) + ":";
            block_label = ".do" + to_string(current_branch) + ":";
            skip_label = ".od" + to_string(current_branch) + ":";

            /* write out label for any previous branches that go
             * immediately to another branch
             */
            if(label != NO_LABEL) WRLABEL(*label, "");
            label = &branch_label;
            // children deal with labelling
            left_operand = write_stmt_expr(expr->first(), label);
            WRLABEL(*label, "goto "
                    << skip_label.substr(0, skip_label.length() - 1)
                    << " if ! " << left_operand);
            // whatever this thing is, it should print itself out
            label = &block_label;
            write_stmt_expr(expr->second(), label);
            WRLABEL(*label, "goto "
                    << branch_label.substr(0, branch_label.length() - 1));
            *out << skip_label << endl;
            return "";
        case TOK_IF:
            DEBUGH('l', "Expression is an if statement");
            current_branch = branch_count++;
            branch_label = ".if" + to_string(current_branch) + ":";
            block_label = ".th" + to_string(current_branch) + ":";
            skip_label = ".fi" + to_string(current_branch) + ":";
            else_label = ".el" + to_string(current_branch) + ":";

            /* write out label for any previous branches that go
             * immediately to another branch
             */
            if(label != NO_LABEL) WRLABEL(*label, "");
            label = &branch_label;
            left_operand = write_stmt_expr(expr->first(), label);
            if(expr->children.size() == 3) {
                WRLABEL(*label, "goto "
                        << else_label.substr(0, else_label.length() - 1)
                        << " if ! " << left_operand);
            } else {
                WRLABEL(*label, "goto "
                        << skip_label.substr(0, skip_label.length() - 1)
                        << " if ! " << left_operand);
            }

            label = &block_label;
            write_stmt_expr(expr->second(), label);
            if(expr->children.size() == 3) {
                WRLABEL(*label, "goto "
                        << skip_label.substr(0, skip_label.length() - 1));
                // label is dealt with in children
                label = &else_label;
                write_stmt_expr(expr->third(), label);
            }
            *out << skip_label << endl;
            return "";
            break;
        case TOK_BLOCK:
            DEBUGH('l', "Expression is a block");
            for(size_t i = 0; i < expr->children.size(); ++i) {
                astree* statement = expr->children[i];
                // just in case this is an if, else, or while
                // block, we need to pass the label to the first child
                //
                // actually, with the current scheme, since the label
                // is shared between all calls, we should just be able
                // to pass it around and have it get set back to a blank
                // by the end of the first call?
                if(i == 0) {
                    write_stmt_expr(statement, label);
                } else {
                    const string * tmp_label = NO_LABEL;
                    write_stmt_expr(statement, tmp_label);
                }
            }
            return "";
            break;
        case TOK_NULLPTR:
            return "0:p";
            break;
        case TOK_IDENT:
        case TOK_INTCON:
        case TOK_CHARCON:
            return *(expr->lexinfo);
            break;
        case TOK_STRINGCON:
            for(size_t i = 0; i < string_constants.size(); ++i) {
                if(string_constants[i] == *(expr->lexinfo))
                    return ".s" + to_string(i);
            }
            return "ERR";
            break;        
        default:
            cerr << "UNIMPLEMENTED: " << parser::get_tname(expr->symbol)
                << " " << *(expr->lexinfo) << endl;
            return "ERR";
    }
}

string generator::write_stride(astree* index, astree* memblock) {
    if(index->has_attr(attr::STRUCT)) {
        return ":p";
    } else if(memblock->has_attr(attr::STRING)) {
        return ":c";
    } else if(index->has_attr(attr::INT)) {
        return ":i";
    } else {
        return "ERR";
    }
}

string generator::write_reg_type(astree* tree) {
    if(tree->has_attr(attr::ARRAY) ||
            tree->has_attr(attr::STRUCT) ||
            tree->has_attr(attr::STRING)) {
        return ":p";
    } else if(tree->has_attr(attr::INT)) {
        return ":i";
    } else {
        return "ERR";
    }
}


string generator::write_type(astree* tree) {
    // i think this is the type for all arrays?
    if(tree->has_attr(attr::ARRAY)) {
        return "void* ";
    } else if(tree->has_attr(attr::INT)) {
        return "int ";
    } else if(tree->has_attr(attr::VOID)) {
        return "void ";
    } else if(tree->has_attr(attr::STRUCT)) {
        //return "struct " + *(tree->type_id) + " ";
        return "void* ";
    } else if(tree->has_attr(attr::STRING)) {
        return "void* ";
    } else {
        return "ERR";
    }
}

string generator::write_type(symbol_value* value) {
    // i think this is the type for all arrays?
    if(value->has_attr(attr::ARRAY)) {
        return "void* ";
    } else if(value->has_attr(attr::INT)) {
        return "int ";
    } else if(value->has_attr(attr::VOID)) {
        return "void ";
    } else if(value->has_attr(attr::STRUCT)) {
        // return "struct " + *(value->type_id) + " ";
        return "void* ";
    } else if(value->has_attr(attr::STRING)) {
        return "void* ";
    } else {
        return "ERR";
    }
}
