#ifndef __INTLANG_H__
#define __INTLANG_H__

#include <string>

#include "auxlib.h"
#include "astree.h"
#include "lyutils.h"

using namespace std;

class generator {
    using symbol_table = unordered_map<const string*,symbol_value*>;
    using symbol_entry = symbol_table::value_type;
    private:
        static size_t branch_count;
        static size_t vreg_count;
        static const string* NO_LABEL;
        static const string* FUN_LABEL;
        static const string* STC_LABEL;
        static const string* GLO_LABEL;
        static const string* LOC_LABEL;
        static const string* STR_LABEL;
        static const string* FLD_LABEL;
        static vector<string> string_constants;
        static ostream* out;
    public:
        static void set_out(ostream* out_);
        static int write_int_lang(astree* root,
                vector<symbol_table*> tables,
                vector<string> string_constants);
        static int write_var_decl(symbol_entry);
        static int write_string_decl(string, size_t);
        static int write_struct_decl(symbol_entry);
        static int write_function_decl(astree*,symbol_table*);
        static string write_stmt_expr(astree* expr,
                const string*& label,
                bool return_compound = false);
        static string write_stride(astree* index, astree* memblock);
        static string write_reg_type(astree*);
        static string write_type(symbol_value*);
        static string write_type(astree*);
};

#define WRLABEL(LABEL,CODE) { \
            *out << setw(10) << left << LABEL << CODE << endl; \
        }
#define IDLABEL(LABEL,CODE) { \
            *out << setw(10) << *NO_LABEL << LABEL \
                 << CODE << endl; \
        }
#define INDENT(CODE) { \
            *out << setw(10) << *NO_LABEL << CODE << endl; \
        }
#endif
