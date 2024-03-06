#ifndef __ASTREE_H__
#define __ASTREE_H__

#include "badllist.h"
#include "badmap.h"
#include "bcc_types.h"
#include "debug.h"
#include "stdint.h"
#include "symtable.h"

enum attribute {
  ATTR_NONE = 0,              /* no attributes set */
  ATTR_EXPR_LVAL = 1 << 0,    /* refers to an assignable location */
  ATTR_STMT_DEFAULT = 1 << 1, /* switch statement has a default label */
  ATTR_CONST_NONE = 0,        /* not a constant expression */
  ATTR_CONST_MAYBE = 1 << 2,  /* can become constant expression */
  ATTR_CONST_INIT = 1 << 3,   /* valid initializer constexpr */
  ATTR_CONST_INT = 3 << 2,    /* valid integral constexpr */
  NUM_ATTRIBUTES = 4,         /* number of attribute flags */
  ATTR_MASK_CONST = ATTR_CONST_INT
};

typedef struct astree {
  Type *type;                /* type info */
  const char *lexinfo;       /* lexical information */
  SymbolTable *symbol_table; /* symbol table for scope, if applicable */
  ListIter *first_instr;
  ListIter *last_instr;
  struct {
    union {
      signed long signed_value;
      unsigned long unsigned_value;
    } integral;
    const char *label;
    Symbol *symbol;
  } constant;
  Location loc; /* source location */
  size_t jump_id;
  size_t case_id;
  size_t spill_eightbytes;
  LinkedList children;     /* children of this n-way node */
  int tok_kind;            /* token code */
  unsigned int attributes; /* node-specific attributes */
} ASTree;

#define UNWRAP(node) \
  (node->tok_kind == TOK_TYPE_ERROR ? astree_get(node, 0) : node)

void astree_init_globals(void);
void astree_destroy_globals(void);
ASTree *astree_init(int tok_kind, const Location location, const char *lexinfo);
void astree_destroy(ASTree *tree);
ASTree *astree_adopt(ASTree *parent, const size_t count, ...);
ASTree *astree_replace(ASTree *parent, const size_t index, ASTree *child);
ASTree *astree_get(ASTree *parent, const size_t index);
ASTree *astree_remove(ASTree *parent, const size_t index);
ASTree *astree_create_errnode(ASTree *child, ErrorCode code, size_t info_count,
                              ...);
ASTree *astree_propogate_errnode(ASTree *parent, ASTree *child);
ASTree *astree_propogate_errnode_v(ASTree *parent, size_t count, ...);
ASTree *astree_propogate_errnode_a(ASTree *parent, size_t count,
                                   ASTree **children);
size_t astree_count(ASTree *parent);
int astree_is_const_zero(const ASTree *tree);
int astree_to_string(ASTree *astree, char *buffer);
int astree_print_tree(ASTree *tree, FILE *out, int depth);
int astree_print_symbols(ASTree *tree, FILE *out, int depth);
#endif
