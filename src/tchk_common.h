#ifndef __TCHK_COMMON_H__
#define __TCHK_COMMON_H__

#include "astree.h"

enum type_checker_action {
  TCHK_COMPATIBLE,
  TCHK_IMPLICIT_CAST,
  TCHK_EXPLICIT_CAST,
  TCHK_INCOMPATIBLE,
  TCHK_E_NO_FLAGS
};

ASTree *perform_pointer_conv(ASTree *expr);
ASTree *convert_type(ASTree *expr, const TypeSpec *type);
int compare_params(LinkedList *dests, LinkedList *srcs);
int compare_members(LinkedList *dests, LinkedList *srcs);
int compare_declspecs(const TypeSpec *dest, const TypeSpec *src);
int compare_auxspecs(const LinkedList *dests, const LinkedList *srcs);
int types_compatible(const TypeSpec *type1, const TypeSpec *type2);
int determine_conversion(const TypeSpec *type1, const TypeSpec *type2,
                         const TypeSpec **out);
int merge_block_controls(ASTree *block, ASTree *stmt);
#endif
