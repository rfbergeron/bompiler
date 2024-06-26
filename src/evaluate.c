#include "evaluate.h"

#include <limits.h>

#include "asmgen.h"
#include "assert.h"
#include "bcc_err.h"
#include "conversions.h"
#include "ctype.h"
#include "errno.h"
#include "lyutils.h"
#include "state.h"
#include "stdlib.h"
#include "yyparse.h"

#define MIN(x, y) ((x) > (y) ? (y) : (x))
#define BINARY_SELECT(optext, dest_value, common_type, left_value,            \
                      right_value)                                            \
  do {                                                                        \
    if (type_get_width(common_type) == X64_SIZEOF_LONG) {                     \
      if (type_is_unsigned(common_type)) {                                    \
        (dest_value) =                                                        \
            (unsigned long)(left_value)optext(unsigned long)(right_value);    \
      } else {                                                                \
        (dest_value) =                                                        \
            (signed long)(left_value)optext(signed long)(right_value);        \
      }                                                                       \
    } else if (type_get_width(common_type) == X64_SIZEOF_INT &&               \
               type_is_unsigned(common_type)) {                               \
      (dest_value) =                                                          \
          (unsigned int)(left_value)optext(unsigned int)(right_value);        \
    } else {                                                                  \
      (dest_value) = (signed int)(left_value)optext(signed int)(right_value); \
    }                                                                         \
  } while (0)
#define BINARY_EVAL(optext, dest_value, common_type, left, right)              \
  do {                                                                         \
    assert(type_is_arithmetic(common_type) &&                                  \
           type_is_arithmetic(left->type) && type_is_arithmetic(right->type)); \
    if (type_is_unsigned(left->type)) {                                        \
      if (type_is_unsigned(right->type)) {                                     \
        BINARY_SELECT(optext, dest_value, common_type,                         \
                      left->constant.integral.unsigned_value,                  \
                      right->constant.integral.unsigned_value);                \
      } else {                                                                 \
        BINARY_SELECT(optext, dest_value, common_type,                         \
                      left->constant.integral.unsigned_value,                  \
                      right->constant.integral.signed_value);                  \
      }                                                                        \
    } else if (type_is_unsigned(right->type)) {                                \
      BINARY_SELECT(optext, dest_value, common_type,                           \
                    left->constant.integral.signed_value,                      \
                    right->constant.integral.unsigned_value);                  \
    } else {                                                                   \
      BINARY_SELECT(optext, dest_value, common_type,                           \
                    left->constant.integral.signed_value,                      \
                    right->constant.integral.signed_value);                    \
    }                                                                          \
  } while (0)
#define COMPARATOR_SELECT(optext, dest_value, common_type, left_value,        \
                          right_value)                                        \
  do {                                                                        \
    if (type_get_width(common_type) == X64_SIZEOF_LONG) {                     \
      if (type_is_unsigned(common_type)) {                                    \
        (dest_value) =                                                        \
            (unsigned long)(left_value)optext(unsigned long)(right_value);    \
      } else {                                                                \
        (dest_value) =                                                        \
            (signed long)(left_value)optext(signed long)(right_value);        \
      }                                                                       \
    } else if (type_get_width(common_type) == X64_SIZEOF_INT &&               \
               type_is_unsigned(common_type)) {                               \
      (dest_value) =                                                          \
          (unsigned int)(left_value)optext(unsigned int)(right_value);        \
    } else {                                                                  \
      (dest_value) = (signed int)(left_value)optext(signed int)(right_value); \
    }                                                                         \
  } while (0)
#define COMPARATOR_EVAL(optext, comparator, left, right)                       \
  do {                                                                         \
    assert(type_is_arithmetic(left->type) && type_is_arithmetic(right->type)); \
    Type *common_type =                                                        \
        (type_is_pointer(left->type) || type_is_pointer(right->type))          \
            ? (Type *)TYPE_LONG                                                \
            : type_arithmetic_conversions(left->type, right->type);            \
    if (left->constant.label != NULL) {                                        \
      comparator->constant.integral.signed_value =                             \
          left->constant.integral.signed_value optext                          \
              right->constant.integral.signed_value;                           \
    } else if (type_is_unsigned(left->type)) {                                 \
      if (type_is_unsigned(right->type)) {                                     \
        COMPARATOR_SELECT(optext, comparator->constant.integral.signed_value,  \
                          common_type, left->constant.integral.unsigned_value, \
                          right->constant.integral.unsigned_value);            \
      } else {                                                                 \
        COMPARATOR_SELECT(optext, comparator->constant.integral.signed_value,  \
                          common_type, left->constant.integral.unsigned_value, \
                          right->constant.integral.signed_value);              \
      }                                                                        \
    } else if (type_is_unsigned(right->type)) {                                \
      COMPARATOR_SELECT(optext, comparator->constant.integral.signed_value,    \
                        common_type, left->constant.integral.signed_value,     \
                        right->constant.integral.unsigned_value);              \
    } else {                                                                   \
      COMPARATOR_SELECT(optext, comparator->constant.integral.signed_value,    \
                        common_type, left->constant.integral.signed_value,     \
                        right->constant.integral.signed_value);                \
    }                                                                          \
  } while (0)
#define SHIFT_SELECT(optext, shift, left_value, right_value)    \
  do {                                                          \
    if (type_get_width(shift->type) == X64_SIZEOF_LONG) {       \
      if (type_is_unsigned(shift->type)) {                      \
        shift->constant.integral.unsigned_value =               \
            (unsigned long)(left_value)optext(right_value);     \
      } else {                                                  \
        shift->constant.integral.signed_value =                 \
            (signed long)(left_value)optext(right_value);       \
      }                                                         \
    } else if (type_get_width(shift->type) == X64_SIZEOF_INT && \
               type_is_unsigned(shift->type)) {                 \
      shift->constant.integral.unsigned_value =                 \
          (unsigned int)(left_value)optext(right_value);        \
    } else {                                                    \
      shift->constant.integral.signed_value =                   \
          (signed int)(left_value)optext(right_value);          \
    }                                                           \
  } while (0)
#define SHIFT_EVAL(optext, shift, left, right)                                 \
  do {                                                                         \
    assert(type_is_arithmetic(shift->type) &&                                  \
           type_is_arithmetic(left->type) && type_is_arithmetic(right->type)); \
    if (type_is_unsigned(left->type)) {                                        \
      if (type_is_unsigned(right->type)) {                                     \
        SHIFT_SELECT(optext, shift, left->constant.integral.unsigned_value,    \
                     right->constant.integral.unsigned_value);                 \
      } else {                                                                 \
        SHIFT_SELECT(optext, shift, left->constant.integral.unsigned_value,    \
                     right->constant.integral.signed_value);                   \
      }                                                                        \
    } else if (type_is_unsigned(right->type)) {                                \
      SHIFT_SELECT(optext, shift, left->constant.integral.signed_value,        \
                   right->constant.integral.unsigned_value);                   \
    } else {                                                                   \
      SHIFT_SELECT(optext, shift, left->constant.integral.signed_value,        \
                   right->constant.integral.signed_value);                     \
    }                                                                          \
  } while (0)
#define UNARY_SELECT(optext, operator, operand_value)        \
  do {                                                       \
    if (type_get_width(operator->type) == X64_SIZEOF_LONG) { \
      if (type_is_unsigned(operator->type)) {                \
        operator->constant.integral.unsigned_value =         \
            optext(unsigned long)(operand_value);            \
      } else {                                               \
        operator->constant.integral.signed_value =           \
            optext(signed long)(operand_value);              \
      }                                                      \
    } else if (type_is_unsigned(operator->type)) {           \
      operator->constant.integral.unsigned_value =           \
          optext(unsigned int)(operand_value);               \
    } else {                                                 \
      operator->constant.integral.signed_value =             \
          optext(signed int)(operand_value);                 \
    }                                                        \
  } while (0)
#define UNARY_EVAL(optext, operator, operand)                                  \
  do {                                                                         \
    assert(type_is_arithmetic(operator->type) &&                               \
           type_is_arithmetic(operand->type));                                 \
    if (type_is_unsigned(operand->type)) {                                     \
      UNARY_SELECT(optext, operator,                                           \
                   operand->constant.integral.unsigned_value);                 \
    } else {                                                                   \
      UNARY_SELECT(optext, operator, operand->constant.integral.signed_value); \
    }                                                                          \
  } while (0)
#define BINOP_CASE(opchar, optext, optrans)                                   \
  case opchar:                                                                \
    if ((left->attributes & ATTR_MASK_CONST) == ATTR_CONST_INT &&             \
        (right->attributes & ATTR_MASK_CONST) == ATTR_CONST_INT) {            \
      operator->attributes |= ATTR_CONST_INT;                                 \
      if (type_is_unsigned(operator->type)) {                                 \
        BINARY_EVAL(optext, operator->constant.integral.unsigned_value,       \
                            operator->type, left, right);                     \
      } else {                                                                \
        BINARY_EVAL(                                                          \
            optext, operator->constant.integral.signed_value, operator->type, \
            left, right);                                                     \
      }                                                                       \
      return astree_adopt(operator, 2, left, right);                          \
    } else {                                                                  \
      right = tchk_cexpr_conv(right);                                         \
      left = tchk_cexpr_conv(left);                                           \
      return optrans(operator, left, right);                                  \
    }
#define UNOP_CASE(opchar, optext, optrans)                           \
  case opchar:                                                       \
    if ((operand->attributes & ATTR_MASK_CONST) == ATTR_CONST_INT) { \
      operator->attributes |= ATTR_CONST_INT;                        \
      UNARY_EVAL(optext, operator, operand);                         \
      return astree_adopt(operator, 1, operand);                     \
    } else {                                                         \
      operand = tchk_cexpr_conv(operand);                            \
      return optrans(operator, operand);                             \
    }

/* NOTE: the lexer only parses numbers as unsigned values; if it attempted to
 * parse explicitly signed values it would swallow up addition and subtraction
 * operators
 */
ASTree *evaluate_intcon(ASTree *intcon) {
  intcon->attributes |= ATTR_CONST_INT;
  errno = 0;
  char *endptr = NULL;
  unsigned long value = strtoul(intcon->lexinfo, &endptr, 0);
  if (errno == EINVAL || endptr == intcon->lexinfo) {
    /* no characters parsed; lexer should make this impossible... */
    abort();
  } else if (errno == ERANGE) {
    (void)semerr_const_too_large(intcon, TYPE_UNSIGNED_LONG);
    return intcon;
  }

  int is_signed = 1, is_long = 0;
  switch (endptr[0]) {
    case '\0':
      break;
    case 'u':
      /* fallthrough */
    case 'U':
      is_signed = 0;
      if (endptr[1] == 'l' || endptr[1] == 'L') is_long = 1;
      break;
    case 'l':
      /* fallthrough */
    case 'L':
      is_long = 1;
      if (endptr[1] == 'u' || endptr[1] == 'U') is_signed = 0;
      break;
    default:
      abort();
  }

  if ((!is_signed && is_long) || value > LONG_MAX) {
    intcon->type = (Type *)TYPE_UNSIGNED_LONG;
    intcon->constant.integral.unsigned_value = value;
  } else if (is_long || value > UINT_MAX) {
    intcon->type = (Type *)TYPE_LONG;
    intcon->constant.integral.signed_value = value;
  } else if (!is_signed || value > INT_MAX) {
    intcon->type = (Type *)TYPE_UNSIGNED_INT;
    intcon->constant.integral.unsigned_value = value;
  } else {
    intcon->type = (Type *)TYPE_INT;
    intcon->constant.integral.signed_value = value;
  }

  return intcon;
}

ASTree *evaluate_charcon(ASTree *charcon) {
  charcon->attributes |= ATTR_CONST_INT;
  if (charcon->lexinfo[1] != '\\') {
    charcon->constant.integral.signed_value = charcon->lexinfo[1];
    return charcon;
  }

  char escaped_char = charcon->lexinfo[2];
  if (escaped_char == 'x') {
    /* hex number */
    errno = 0;
    char *endptr = NULL;
    charcon->constant.integral.signed_value =
        (char)strtol(&charcon->lexinfo[3], &endptr, 16);
    if (errno == EINVAL || endptr == &charcon->lexinfo[3]) {
      /* no characters parsed; lexer should make this impossible... */
      abort();
    } else if (errno == ERANGE ||
               charcon->constant.integral.signed_value > CHAR_MAX) {
      (void)semerr_const_too_large(charcon, TYPE_CHAR);
      return charcon;
    }
  } else if (isdigit(escaped_char)) {
    /* octal number */
    charcon->constant.integral.signed_value =
        (char)strtol(&charcon->lexinfo[2], NULL, 8);
  } else {
    /* ASCII control sequence, \?, \", \', or \\ */
    switch (escaped_char) {
      case 'n':
        charcon->constant.integral.signed_value = '\n';
        break;
      case 't':
        charcon->constant.integral.signed_value = '\t';
        break;
      case 'v':
        charcon->constant.integral.signed_value = '\v';
        break;
      case 'b':
        charcon->constant.integral.signed_value = '\b';
        break;
      case 'r':
        charcon->constant.integral.signed_value = '\r';
        break;
      case 'f':
        charcon->constant.integral.signed_value = '\f';
        break;
      case 'a':
        charcon->constant.integral.signed_value = '\a';
        break;
      case '\\':
        charcon->constant.integral.signed_value = '\\';
        break;
      case '\?':
        charcon->constant.integral.signed_value = '\?';
        break;
      case '\'':
        charcon->constant.integral.signed_value = '\'';
        break;
      case '"':
        charcon->constant.integral.signed_value = '"';
        break;
      default:
        charcon->constant.integral.signed_value = '\0';
    }
  }

  return charcon;
}

ASTree *evaluate_stringcon(ASTree *stringcon) {
  /* this function will emit the necessary directives for the literal */
  const char *stringcon_label = asmgen_literal_label(stringcon->lexinfo);
  assert(stringcon_label != NULL);
  stringcon->attributes |= ATTR_CONST_INIT;
  stringcon->constant.label = stringcon_label;
  stringcon->constant.integral.signed_value = 0;
  return stringcon;
}

ASTree *evaluate_ident(ASTree *ident) {
  const char *id_str = ident->lexinfo;
  Symbol *symbol = NULL;
  (void)state_get_symbol(state, id_str, &symbol);
  if (symbol->storage == STORE_EXT || symbol->storage == STORE_STAT) {
    ident->attributes |= ATTR_CONST_MAYBE;
    ident->constant.integral.signed_value = 0;
    if (symbol->linkage == LINK_NONE)
      ident->constant.label =
          mk_static_label(ident->lexinfo, symbol->static_id);
    else
      ident->constant.label = ident->lexinfo;
    return ident;
  } else if (symbol->storage == STORE_ENUM_CONST) {
    ident->attributes |= ATTR_CONST_INT;
    ident->constant.integral.signed_value =
        tag_get_constant(ident->type->tag.value, ident->lexinfo);
    return ident;
  } else {
    return translate_ident(ident);
  }
}

ASTree *evaluate_addition(ASTree *addition, ASTree *left, ASTree *right) {
  if ((left->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (right->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (left->constant.label != NULL && right->constant.label != NULL) ||
      ((left->constant.label != NULL) && type_is_pointer(right->type)) ||
      ((right->constant.label != NULL) && type_is_pointer(left->type))) {
    right = tchk_cexpr_conv(right);
    left = tchk_cexpr_conv(left);
    return translate_addition(addition, left, right);
  } else if (type_is_pointer(left->type)) {
    addition->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                                right->attributes & ATTR_MASK_CONST);
    addition->constant = left->constant;
    if (type_is_signed(right->type) || type_is_enum(right->type))
      addition->constant.integral.signed_value +=
          right->constant.integral.signed_value;
    else
      addition->constant.integral.signed_value +=
          right->constant.integral.unsigned_value;
  } else if (type_is_pointer(right->type)) {
    addition->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                                right->attributes & ATTR_MASK_CONST);
    addition->constant = right->constant;
    if (type_is_signed(left->type) || type_is_enum(left->type))
      addition->constant.integral.signed_value +=
          left->constant.integral.signed_value;
    else
      addition->constant.integral.signed_value +=
          left->constant.integral.unsigned_value;
  } else {
    addition->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                                right->attributes & ATTR_MASK_CONST);
    if (type_is_unsigned(addition->type)) {
      BINARY_EVAL(+, addition->constant.integral.unsigned_value, addition->type,
                  left, right);
    } else {
      BINARY_EVAL(+, addition->constant.integral.signed_value, addition->type,
                  left, right);
    }
  }
  return astree_adopt(addition, 2, left, right);
}

/* we can rely on the fact that pointer subtraction is only valid if both
 * operands have pointer type or only the left has pointer type to simplify
 * this logic
 *
 * constexpr evaluation would fail if:
 * 1. both operands have pointer type, but only the right operand has an
 * address component
 * 2. both operands have pointer type and both operands have an address
 * component, but the address is different
 * 3. only the left operand has pointer type, but the right operand has an
 * address component
 * 4. neither operand has pointer type and both operands have an address
 * component, but the address is different
 * 5. neither operand has pointer type, and both operands have an address
 * component, but the address is different
 *
 * simplified:
 * 1. if both operands have an address, they must be the same
 * 2. if the right operand has an address, the left shall also have one
 *
 * further simplified:
 * the right operand must have no address or the same address as the left
 * operand
 *
 * when performing integer subtraction, there are 3 possibilities:
 * 1. neither operand has an address component
 * 2. both operands have an address component
 * 3. only the left operand has an address component
 */
ASTree *evaluate_subtraction(ASTree *subtraction, ASTree *left, ASTree *right) {
  if ((left->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (right->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (right->constant.label != NULL &&
       right->constant.label != left->constant.label)) {
    right = tchk_cexpr_conv(right);
    left = tchk_cexpr_conv(left);
    return translate_addition(subtraction, left, right);
  } else if (type_is_pointer(right->type)) {
    subtraction->attributes |= ATTR_CONST_INIT;
    subtraction->constant.integral.signed_value =
        (left->constant.integral.signed_value -
         right->constant.integral.signed_value);
  } else if (type_is_pointer(left->type)) {
    subtraction->attributes |= ATTR_CONST_INIT;
    subtraction->constant = left->constant;
    if (type_is_signed(right->type) || type_is_enum(right->type))
      subtraction->constant.integral.signed_value -=
          right->constant.integral.signed_value;
    else
      subtraction->constant.integral.signed_value -=
          right->constant.integral.unsigned_value;
  } else {
    subtraction->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                                   right->attributes & ATTR_MASK_CONST);
    if (left->constant.label != right->constant.label) {
      subtraction->constant.label = left->constant.label;
    }
    if (type_is_unsigned(subtraction->type)) {
      BINARY_EVAL(-, subtraction->constant.integral.unsigned_value,
                  subtraction->type, left, right);
    } else {
      BINARY_EVAL(-, subtraction->constant.integral.signed_value,
                  subtraction->type, left, right);
    }
  }
  return astree_adopt(subtraction, 2, left, right);
}

ASTree *evaluate_shift(ASTree *shift, ASTree *left, ASTree *right) {
  if ((left->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT &&
      (right->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT &&
      left->constant.label == NULL && right->constant.label == NULL) {
    shift->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                             right->attributes & ATTR_MASK_CONST);
    if (shift->tok_kind == TOK_SHL) {
      if (type_is_unsigned(shift->type)) {
        SHIFT_EVAL(<<, shift, left, right);
      } else {
        SHIFT_EVAL(<<, shift, left, right);
      }
    } else {
      if (type_is_unsigned(shift->type)) {
        SHIFT_EVAL(>>, shift, left, right);
      } else {
        SHIFT_EVAL(>>, shift, left, right);
      }
    }
    return astree_adopt(shift, 2, left, right);
  } else {
    right = tchk_cexpr_conv(right);
    left = tchk_cexpr_conv(left);
    return translate_shift(shift, left, right);
  }
}

ASTree *evaluate_relational(ASTree *relational, ASTree *left, ASTree *right) {
  if ((left->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (right->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      left->constant.label != right->constant.label) {
    right = tchk_cexpr_conv(right);
    left = tchk_cexpr_conv(left);
    return translate_comparison(relational, left, right);
  }

  relational->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                                right->attributes & ATTR_MASK_CONST);

  switch (relational->tok_kind) {
    case '<':
      COMPARATOR_EVAL(<, relational, left, right);
      break;
    case TOK_LE:
      COMPARATOR_EVAL(<=, relational, left, right);
      break;
    case '>':
      COMPARATOR_EVAL(>, relational, left, right);
      break;
    case TOK_GE:
      COMPARATOR_EVAL(>=, relational, left, right);
      break;
    default:
      abort();
  }

  return astree_adopt(relational, 2, left, right);
}

ASTree *evaluate_equality(ASTree *equality, ASTree *left, ASTree *right) {
  if ((left->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (right->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (left->constant.label != right->constant.label &&
       (left->constant.integral.signed_value != 0 ||
        right->constant.integral.signed_value != 0))) {
    right = tchk_cexpr_conv(right);
    left = tchk_cexpr_conv(left);
    return translate_comparison(equality, left, right);
  }

  equality->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                              right->attributes & ATTR_MASK_CONST);

  if (left->constant.label != right->constant.label) {
    equality->constant.integral.signed_value = equality->tok_kind == TOK_NE;
  } else if (equality->tok_kind == TOK_NE) {
    COMPARATOR_EVAL(!=, equality, left, right);
  } else {
    COMPARATOR_EVAL(==, equality, left, right);
  }

  return astree_adopt(equality, 2, left, right);
}

ASTree *evaluate_logical(ASTree *logical, ASTree *left, ASTree *right) {
  if ((left->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (right->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    right = tchk_cexpr_conv(right);
    left = tchk_cexpr_conv(left);
    return translate_logical(logical, left, right);
  }

  logical->attributes |= MIN(left->attributes & ATTR_MASK_CONST,
                             right->attributes & ATTR_MASK_CONST);
  logical->constant.integral.signed_value =
      logical->tok_kind == TOK_OR
          ? (left->constant.label != NULL ||
             left->constant.integral.unsigned_value != 0 ||
             right->constant.label != NULL ||
             right->constant.integral.unsigned_value != 0)
          : ((left->constant.label != NULL ||
              left->constant.integral.unsigned_value != 0) &&
             (right->constant.label != NULL ||
              right->constant.integral.unsigned_value != 0));
  return astree_adopt(logical, 2, left, right);
}

ASTree *evaluate_cast(ASTree *cast, ASTree *expr) {
  if ((expr->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    expr = tchk_cexpr_conv(expr);
    return translate_scal_conv(cast, expr);
  }

  cast->attributes |= !type_is_integral(cast->type)
                          ? ATTR_CONST_INIT
                          : (expr->attributes & ATTR_MASK_CONST);

  if (expr->constant.label != NULL || !type_is_arithmetic(cast->type)) {
    cast->constant = expr->constant;
  } else {
    UNARY_EVAL(+, cast, expr);
  }

  return astree_adopt(cast, 1, expr);
}

ASTree *evaluate_ptr_conv(ASTree *ptr_conv, ASTree *expr) {
  if ((expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_NONE) {
    assert(expr->constant.label != NULL);
    ptr_conv->attributes |= ATTR_CONST_INIT;
    ptr_conv->constant = expr->constant;
    return astree_adopt(ptr_conv, 1, expr);
  } else {
    assert(!instr_empty(expr->instructions));
    return translate_ptr_conv(ptr_conv, expr);
  }
}

ASTree *evaluate_scal_conv(ASTree *scal_conv, ASTree *expr) {
  assert(!(expr->attributes & ATTR_EXPR_LVAL));
  if ((expr->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    assert((type_is_arithmetic(scal_conv->type) &&
            type_is_arithmetic(expr->type)) ||
           (type_is_pointer(scal_conv->type) && type_is_pointer(expr->type)));
    assert(!instr_empty(expr->instructions));
    return translate_scal_conv(scal_conv, expr);
  } else if (type_is_pointer(scal_conv->type)) {
    assert(type_is_pointer(expr->type) || astree_is_const_zero(expr));
    scal_conv->constant = expr->constant;
    scal_conv->attributes = expr->attributes;
    return astree_adopt(scal_conv, 1, expr);
  } else {
    assert(type_is_arithmetic(expr->type));
    assert(type_is_arithmetic(scal_conv->type));
    scal_conv->attributes |= expr->attributes & ATTR_MASK_CONST;
    UNARY_EVAL(+, scal_conv, expr);
    return astree_adopt(scal_conv, 1, expr);
  }
}

ASTree *evaluate_disp_conv(ASTree *disp_conv, ASTree *expr,
                           const Type *pointer_type) {
  assert(disp_conv->type == TYPE_LONG);
  assert(type_is_integral(expr->type));
  assert(!(expr->attributes & ATTR_EXPR_LVAL));
  if ((expr->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT &&
      expr->constant.label == NULL) {
    ptrdiff_t stride = type_elem_width(pointer_type);
    disp_conv->attributes = expr->attributes;
    if (type_is_unsigned(expr->type))
      disp_conv->constant.integral.signed_value =
          expr->constant.integral.unsigned_value;
    else
      disp_conv->constant.integral.signed_value =
          expr->constant.integral.signed_value;
    disp_conv->constant.integral.signed_value *= stride;
    return astree_adopt(disp_conv, 1, expr);
  } else {
    expr = tchk_cexpr_conv(expr);
    return translate_disp_conv(disp_conv, expr, pointer_type);
  }
}

ASTree *evaluate_diff_conv(ASTree *diff_conv, ASTree *expr,
                           const Type *pointer_type) {
  if ((expr->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT &&
      expr->constant.label == NULL) {
    ptrdiff_t stride = type_elem_width(pointer_type);
    diff_conv->attributes = expr->attributes;
    diff_conv->constant.integral.signed_value =
        expr->constant.integral.signed_value / stride;
    return astree_adopt(diff_conv, 1, expr);
  } else {
    expr = tchk_cexpr_conv(expr);
    return translate_diff_conv(diff_conv, expr, pointer_type);
  }
}

ASTree *evaluate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr) {
  if ((condition->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (true_expr->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (false_expr->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    false_expr = tchk_cexpr_conv(false_expr);
    true_expr = tchk_cexpr_conv(true_expr);
    condition = tchk_cexpr_conv(condition);
    return translate_conditional(qmark, condition, true_expr, false_expr);
  }

  ASTree *selected_expr =
      condition->constant.label != NULL ||
              condition->constant.integral.unsigned_value != 0
          ? true_expr
          : false_expr;
  qmark->attributes |= MIN(condition->attributes & ATTR_MASK_CONST,
                           MIN(true_expr->attributes & ATTR_MASK_CONST,
                               false_expr->attributes & ATTR_MASK_CONST));
  if (selected_expr->constant.label == NULL) {
    UNARY_EVAL(+, qmark, selected_expr);
  } else {
    qmark->constant = selected_expr->constant;
  }
  return astree_adopt(qmark, 3, condition, true_expr, false_expr);
}

ASTree *evaluate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  if ((pointer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      (index->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT ||
      index->constant.label != NULL) {
    index = tchk_cexpr_conv(index);
    pointer = tchk_cexpr_conv(pointer);
    return translate_subscript(subscript, pointer, index);
  }

  subscript->attributes |= ATTR_CONST_MAYBE;
  subscript->constant = pointer->constant;
  subscript->constant.integral.signed_value +=
      index->constant.integral.signed_value;

  return astree_adopt(subscript, 2, pointer, index);
}

ASTree *evaluate_addrof(ASTree *addrof, ASTree *operand) {
  if ((operand->attributes & ATTR_MASK_CONST) != ATTR_CONST_MAYBE) {
    operand = tchk_cexpr_conv(operand);
    return translate_addrof(addrof, operand);
  } else {
    addrof->attributes |= ATTR_CONST_INIT;
    addrof->constant = operand->constant;
    return astree_adopt(addrof, 1, operand);
  }
}

ASTree *evaluate_reference(ASTree *reference, ASTree *struct_, ASTree *member) {
  if ((struct_->attributes & ATTR_MASK_CONST) < ATTR_CONST_MAYBE) {
    struct_ = tchk_cexpr_conv(struct_);
    return translate_reference(reference, struct_, member);
  } else {
    Type *tag_type = reference->tok_kind == TOK_ARROW
                         ? type_strip_declarator(struct_->type)
                         : struct_->type;

    Symbol *symbol = type_member_name(tag_type, member->lexinfo);
    assert(symbol);
    reference->constant.integral.signed_value =
        struct_->constant.integral.signed_value + symbol->disp;
    reference->constant.label = struct_->constant.label;
    reference->attributes |= ATTR_CONST_MAYBE;

    return astree_adopt(reference, 2, struct_, member);
  }
}

ASTree *evaluate_comma(ASTree *comma, ASTree *left, ASTree *right) {
  comma->attributes |= right->attributes & ATTR_EXPR_LVAL;
  if ((right->attributes & ATTR_MASK_CONST) != ATTR_CONST_NONE &&
      (left->attributes & ATTR_MASK_CONST) != ATTR_CONST_NONE) {
    comma->attributes |= (right->attributes & ATTR_MASK_CONST) == ATTR_CONST_INT
                             ? ATTR_CONST_INIT
                             : (right->attributes & ATTR_MASK_CONST);
    comma->constant = right->constant;
  } else {
    right = tchk_cexpr_conv(right);
    left = tchk_cexpr_conv(left);
  }
  return translate_comma(comma, left, right);
}

ASTree *evaluate_binop(ASTree *operator, ASTree * left, ASTree *right) {
  switch (operator->tok_kind) {
    BINOP_CASE('&', &, translate_binop);
    BINOP_CASE('|', |, translate_binop);
    BINOP_CASE('^', ^, translate_binop);
    BINOP_CASE('/', /, translate_multiplication);
    BINOP_CASE('%', %, translate_multiplication);
    BINOP_CASE('*', *, translate_multiplication);
    case TOK_AND:
    case TOK_OR:
      return evaluate_logical(operator, left, right);
    case TOK_EQ:
    case TOK_NE:
      return evaluate_equality(operator, left, right);
    case '<':
    case TOK_LE:
    case '>':
    case TOK_GE:
      return evaluate_relational(operator, left, right);
    case TOK_SHL:
      return evaluate_shift(operator, left, right);
    case TOK_SHR:
      return evaluate_shift(operator, left, right);
    case '+':
      return evaluate_addition(operator, left, right);
    case '-':
      return evaluate_subtraction(operator, left, right);
    default:
      fprintf(stderr,
              "FATAL: attempted to evaluate constant expression with "
              "unknown binary operation %s\n",
              parser_get_tname(operator->tok_kind));
      abort();
  }
}

ASTree *evaluate_unop(ASTree *operator, ASTree * operand) {
  switch (operator->tok_kind) {
    UNOP_CASE(TOK_NEG, -, translate_unop);
    UNOP_CASE('~', ~, translate_unop);
    /* treat like a cast */
    UNOP_CASE(TOK_POS, +, translate_scal_conv);
    case '!':
      if ((operator->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT) {
        operator->attributes |= operand->attributes & ATTR_MASK_CONST;
        operator->constant.integral.signed_value =(
            type_is_unsigned(operand->type) &&
            operand->constant.integral.unsigned_value != 0) ||
            operand->constant.integral.signed_value != 0 ||
            operand->constant.label != NULL;
        return astree_adopt(operator, 1, operand);
      } else {
        operand = tchk_cexpr_conv(operand);
        return translate_logical_not(operator, operand);
      }
    case TOK_SIZEOF:
      operator->attributes |= ATTR_CONST_INT;
      operator->constant.integral.unsigned_value = type_get_width(
          operand->tok_kind == TOK_DECLARATION ? astree_get(operand, 1)->type
                                               : operand->type);
      return astree_adopt(operator, 1, operand);
    default:
      fprintf(stderr,
              "FATAL: attempted to evaluate constant expression with "
              "unknown unary operation %s\n",
              parser_get_tname(operator->tok_kind));
      abort();
  }
}
