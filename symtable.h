#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include <stdint.h>
#include <string.h>

#include "astree.h"
#include "auxlib.h"
#include "klib/khash.h"
#include "lyutils.h"

typedef struct SymbolValue SymbolValue;
KHASH_MAP_INIT_STR (SymbolTable, SymbolValue *);

// attributes correspond to array indices in the order they are listed here
enum attr {
    ATTR_VOID,
    ATTR_INT,
    ATTR_NULL,
    ATTR_STRING,
    ATTR_STRUCT,
    ATTR_ARRAY,
    ATTR_FUNCTION,
    ATTR_VARIABLE,
    ATTR_FIELD,
    ATTR_TYPEID,
    ATTR_PARAM,
    ATTR_LOCAL,
    ATTR_LVAL,
    ATTR_CONST,
    ATTR_VREG,
    ATTR_VADDR,
    NUM_ATTRIBUTES
};

char attr_map[][32] = {"void",
                       "int",
                       "null",
                       "string",
                       "struct",
                       "array",
                       "function",
                       "variable",
                       "field",
                       "typeid",
                       "parameter",
                       "local",
                       "lval",
                       "const",
                       "vreg",
                       "vaddr"};

struct SymbolValue {
    int attributes[(size_t) NUM_ATTRIBUTES];
    size_t sequence;
    khash_t (SymbolTable) * fields;
    Location loc;
    size_t blocknr;
    kvec_t (SymbolValue *) parameters;
    const char *type_id;
    int has_block;
};

SymbolValue *symbol_value_init (ASTree *tree,
                                size_t sequence_,
                                size_t blocknr_);
void symbol_value_free (SymbolValue *symbol_value_);
void symbol_value_print (SymbolValue *symbol_value_, FILE *out);
void type_checker_init_globals ();
void type_checker_free_globals ();

// may need functions for printing attributes and attribute sets

// where the magic happens
int check_types (ASTree *root);
#endif
