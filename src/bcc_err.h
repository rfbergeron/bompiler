#ifndef __BCC_ERR_H__
#define __BCC_ERR_H__
#include "stdarg.h"
#include "stddef.h"
#include "stdio.h"

typedef enum error_code {
  BCC_TERR_FAILURE = -1,
  BCC_TERR_SUCCESS = 0,
  BCC_TERR_LIBRARY_FAILURE,
  BCC_TERR_INCOMPLETE_TYPE,
  BCC_TERR_INCOMPLETE_SPEC,
  BCC_TERR_INCOMPATIBLE_TYPES,
  BCC_TERR_INCOMPATIBLE_SPEC,
  BCC_TERR_INCOMPATIBLE_DECL,
  BCC_TERR_EXPECTED_TAG,
  BCC_TERR_EXPECTED_TAG_PTR,
  BCC_TERR_EXPECTED_STRUCT,
  BCC_TERR_EXPECTED_UNION,
  BCC_TERR_EXPECTED_ENUM,
  BCC_TERR_EXPECTED_FUNCTION,
  BCC_TERR_EXPECTED_FN_PTR,
  BCC_TERR_EXPECTED_TYPEID,
  BCC_TERR_EXPECTED_DECLARATOR,
  BCC_TERR_EXPECTED_CONST,
  BCC_TERR_EXPECTED_INTEGER,
  BCC_TERR_EXPECTED_INTCONST,
  BCC_TERR_EXPECTED_ARITHMETIC,
  BCC_TERR_EXPECTED_ARITHCONST,
  BCC_TERR_EXPECTED_SCALAR,
  BCC_TERR_EXPECTED_SCALCONST,
  BCC_TERR_EXPECTED_POINTER,
  BCC_TERR_EXPECTED_RETURN,
  BCC_TERR_EXPECTED_RETVAL,
  BCC_TERR_EXPECTED_LVAL,
  BCC_TERR_EXPECTED_NONZERO,
  BCC_TERR_UNEXPECTED_LIST,
  BCC_TERR_UNEXPECTED_TOKEN,
  BCC_TERR_UNEXPECTED_BODY,
  BCC_TERR_UNEXPECTED_RETURN,
  BCC_TERR_EXCESS_INITIALIZERS,
  BCC_TERR_EXCESS_PARAMS,
  BCC_TERR_INSUFF_PARAMS,
  BCC_TERR_SYM_NOT_FOUND,
  BCC_TERR_TYPEID_NOT_FOUND,
  BCC_TERR_TAG_NOT_FOUND,
  BCC_TERR_LABEL_NOT_FOUND,
  BCC_TERR_REDEFINITION,
  BCC_TERR_CONST_TOO_SMALL,
  BCC_TERR_CONST_TOO_LARGE
} ErrorCode;

typedef struct compile_error CompileError;
struct compile_error {
  ErrorCode code;
  void **info;
  size_t info_size;
};

/* forward declare type to prevent circular dependency */
union type;

CompileError *compile_error_init(ErrorCode code, size_t info_size, ...);
CompileError *compile_error_init_v(ErrorCode code, size_t info_size,
                                   va_list info_ptrs);
void compile_error_destroy(CompileError *error);
int print_errors(const union type *type, FILE *out);
int semerr_const_too_large(const char *intstr, const union type *type);
#endif
