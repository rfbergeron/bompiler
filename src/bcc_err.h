#ifndef __BCC_ERR_H__
#define __BCC_ERR_H__
#include "attributes.h"
#include "stdarg.h"
AuxSpec *create_erraux(int errcode, size_t info_count, ...);
AuxSpec *create_erraux_v(int errcode, size_t info_count, va_list info_ptrs);
int print_errs(TypeSpec *errspec, FILE *out);
#endif
