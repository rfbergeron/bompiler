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
  ATTR_EXPR_CONST = 1 << 2,   /* node refers to a valid constant expression */
  ATTR_CONST_INIT = 1 << 3,   /* constant expression is only for initializers */
  ATTR_CONST_ADDR = 1 << 4,   /* constant expression includes an address */
  NUM_ATTRIBUTES = 5,         /* number of attribute flags */
  ATTR_MASK_CONST = ATTR_EXPR_CONST | ATTR_CONST_INIT | ATTR_CONST_ADDR
};

typedef struct astree {
  Type *type;                /* type info */
  const char *lexinfo;       /* lexical information */
  SymbolTable *symbol_table; /* symbol table for scope, if applicable */
  ListIter *first_instr;
  ListIter *last_instr;
  union {
    struct {
      long disp;
      const char *label;
      SymbolValue *symval;
    } address;
    struct {
      unsigned long value;
    } integral;
  } constant;
  Location loc; /* source location */
  size_t jump_id;
  size_t case_id;
  size_t spill_eightbytes;
  LinkedList children;     /* children of this n-way node */
  int symbol;              /* token code */
  unsigned int attributes; /* node-specific attributes */
} ASTree;

#define EMPTY_EXPR_VALUE                                                   \
  {                                                                        \
    NULL, ";", NULL, NULL, NULL, {{0L, NULL, NULL}}, LOC_EMPTY_VALUE, 0UL, \
        0UL, 0UL, BLIB_LLIST_EMPTY, ';', ATTR_NONE                         \
  }
#define UNWRAP(node) \
  (node->symbol == TOK_TYPE_ERROR ? astree_get(node, 0) : node)
extern ASTree EMPTY_EXPR;

ASTree *astree_init(int symbol, const Location location, const char *lexinfo);
int astree_destroy(ASTree *tree);
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
