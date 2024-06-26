#ifndef __ASTREE_H__
#define __ASTREE_H__

#include "arrlist.h"
#include "bcc_types.h"
#include "debug.h"
#include "instr.h"
#include "scope.h"
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
  Type *type;          /* type info */
  const char *lexinfo; /* lexical information */
  Scope *scope;        /* symbol table for scope, if applicable */
  Instruction *instructions;
  struct {
    union {
      signed long signed_value;
      unsigned long unsigned_value;
    } integral;
    const char *label;
  } constant;
  Location loc; /* source location */
  size_t jump_id;
  size_t case_id;
  size_t spill_eightbytes;
  ARR_DECL(struct astree *, children);
  int tok_kind;            /* token code */
  unsigned int attributes; /* node-specific attributes */
} ASTree;

void astree_init_globals(void);
void astree_destroy_globals(void);
ASTree *astree_init(int tok_kind, const Location location, const char *lexinfo);
void astree_destroy(ASTree *tree);
ASTree *astree_adopt(ASTree *parent, const size_t count, ...);
ASTree *astree_get(const ASTree *parent, const size_t index);
ASTree *astree_disown(ASTree *parent);
size_t astree_count(const ASTree *parent);
int astree_is_const_zero(const ASTree *tree);
int astree_to_string(const ASTree *astree, char *buffer);
int astree_print_tree(ASTree *tree, FILE *out, int depth);
int astree_print_symbols(ASTree *tree, FILE *out, int depth);
#endif
