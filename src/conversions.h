#include "astree.h"

#define TCHK_STD_CONV(expr, conv_arr, where) \
  tchk_ptr_conv(tchk_rval_conv((expr), (where)), (conv_arr))
ASTree *tchk_scal_conv(ASTree *expr, Type *type);
ASTree *tchk_disp_conv(ASTree *expr, const Type *pointer_type, ListIter *where);
ASTree *tchk_ptr_conv(ASTree *expr, int conv_arr);
ASTree *tchk_rval_conv(ASTree *expr, ListIter *where);
ASTree *tchk_cexpr_conv(ASTree *expr, ListIter *where);
