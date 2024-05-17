#ifndef __ASTREE_H__
#define __ASTREE_H__

#include "arrlist.h"
#include "bcc_types.h"
#include "debug.h"
#include "instr.h"
#include "scope.h"
#include "stdint.h"
#include "symtable.h"

typedef enum cexpr_kind {
  CEXPR_FALSE, /* not a constant expression */
  CEXPR_MAYBE, /* can become constant expression */
  CEXPR_INIT,  /* valid initializer constexpr */
  CEXPR_INT    /* valid integral constexpr */
} CExprKind;

typedef enum lval_kind { LVAL_FALSE, LVAL_TRUE, LVAL_MODABLE } LValKind;

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
  int tok_kind;         /* token code */
  CExprKind cexpr_kind; /* kind of constant expression, if applicable */
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
LValKind astree_is_lvalue(const ASTree *tree);
int astree_to_string(const ASTree *astree, char *buffer);
int astree_print_tree(ASTree *tree, FILE *out, int depth);
int astree_print_symbols(ASTree *tree, FILE *out, int depth);
#endif
