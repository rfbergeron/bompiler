#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include <stdint.h>
#include <string.h>

#include "astree.h"
#include "auxlib.h"
#include "lyutils.h"
#include "map.h"
#include "vector.h"

// circular dependency with astree; forward declare
struct astree;
struct location;

// attributes correspond to array indices in the order they are listed here
enum attr {
    ATTR_VOID,
    ATTR_INT,
    ATTR_NULL,
    ATTR_STRING,
    ATTR_STRUCT,
    ATTR_ARRAY,
    ATTR_FUNC,
    ATTR_VAR,
    ATTR_FIELD,
    ATTR_TYPEID,
    ATTR_PARAM,
    ATTR_LOCAL,
    ATTR_LVAL,
    ATTR_CONST,
    ATTR_VREG,
    ATTR_VADDR
};

struct symbol_value {
    int attributes[16];
    size_t sequence;
    struct map *fields;
    struct location loc;
    size_t block_nr;
    struct vector *parameters;
    const char **type_id;
    int has_block;
};

struct symbol_value *symbol_value_init (struct astree *tree,
                                        size_t sequence_,
                                        size_t block_nr_);
void symbol_value_free (struct symbol_value *symbol_value_);
void symbol_value_print (struct symbol_value *symbol_value_, FILE *out);

// may need functions for printing attributes and attribute sets

// where the magic happens
int check_types (struct astree *root);
#endif
