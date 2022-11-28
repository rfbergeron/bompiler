#ifndef __TCHK_COMMON_H__
#define __TCHK_COMMON_H__

#include "astree.h"

enum types_equivalent_flags {
  IGNORE_QUALIFIERS = 1 << 0,
  IGNORE_STORAGE_CLASS = 1 << 1
};

int params_equivalent(const AuxSpec *aux1, const AuxSpec *aux2);
int members_equivalent(const AuxSpec *aux1, const AuxSpec *aux2);
int aux_equivalent(const AuxSpec *aux1, const AuxSpec *aux2,
                   unsigned int flags);
int types_equivalent(const TypeSpec *type1, const TypeSpec *type2,
                     unsigned int flags);
int is_const_zero(ASTree *tree);
int types_assignable(const TypeSpec *dest_type, ASTree *src);
const TypeSpec *arithmetic_conversions(const TypeSpec *type1,
                                       const TypeSpec *type2);
void pointer_conversions(ASTree *expr);
#endif
