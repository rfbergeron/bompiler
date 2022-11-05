#include "evaluate.h"

#include "asmgen.h"
#include "ctype.h"
#include "errno.h"
#include "lyutils.h"
#include "state.h"
#include "stdlib.h"
#include "tchk_common.h"
#include "yyparse.h"

#define SELECT_CAST(expr, width, is_signed)                                        \
  (is_signed                   ? width == X64_SIZEOF_CHAR    ? (signed char)(expr) \
                                 : width == X64_SIZEOF_SHORT ? (short)(expr)       \
                                 : width == X64_SIZEOF_INT   ? (int)(expr)         \
                                                             : (long)(expr)        \
                     : width == X64_SIZEOF_CHAR ? (unsigned char)(expr)            \
   : width == X64_SIZEOF_SHORT ? (unsigned short)(expr)                            \
   : width == X64_SIZEOF_INT   ? (unsigned)(expr)                                  \
                               : (unsigned long)(expr))
#define CAST_UNARY(operator, opnode, operand)                              \
  do {                                                                     \
    size_t width = typespec_get_width(opnode->type);                       \
    int is_signed = typespec_is_signed(opnode->type);                      \
    opnode->constant.integral.value =                                      \
        SELECT_CAST(operator SELECT_CAST(operand->constant.integral.value, \
                                         width, is_signed),                \
                    width, is_signed);                                     \
  } while (0)
#define CAST_BINARY(operator, opnode, left, right)                   \
  do {                                                               \
    size_t width = typespec_get_width(opnode->type);                 \
    int is_signed = typespec_is_signed(opnode->type);                \
    opnode->constant.integral.value = SELECT_CAST(                   \
        SELECT_CAST(left->constant.integral.value, width, is_signed) \
        operator SELECT_CAST(right->constant.integral.value, width,  \
                             is_signed),                             \
        width, is_signed);                                           \
  } while (0)
#define BINOP_CASE(opchar, optext, optrans)                            \
  case opchar:                                                         \
    if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&    \
        !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) { \
      operator->attributes |= ATTR_EXPR_CONST |(                       \
          (left->attributes | right->attributes) & ATTR_CONST_INIT);   \
      CAST_BINARY(optext, operator, left, right);                      \
      return astree_adopt(operator, 2, left, right);                   \
    } else {                                                           \
      maybe_load_cexpr(left);                                          \
      maybe_load_cexpr(right);                                         \
      return optrans(operator, left, right);                           \
    }
#define UNOP_CASE(opchar, optext, optrans)           \
  case opchar:                                       \
    if ((operator->attributes & ATTR_EXPR_CONST) &&  \
        !(operator->attributes & ATTR_CONST_ADDR)) { \
      operator->attributes = operand->attributes;    \
      CAST_UNARY(optext, operator, operand);         \
      return astree_adopt(operator, 1, operand);     \
    } else {                                         \
      maybe_load_cexpr(operand);                     \
      return optrans(operator, operand);             \
    }

ASTree *evaluate_intcon(ASTree *intcon) {
  intcon->attributes |= ATTR_EXPR_CONST;
  errno = 0;
  unsigned long unsigned_value = 0;
  long signed_value = strtol(intcon->lexinfo, NULL, 0);
  if (errno == ERANGE) {
    if (signed_value == LONG_MIN) {
      return astree_create_errnode(intcon, BCC_TERR_CONST_TOO_LARGE, 1, intcon);
    } else {
      errno = 0;
      unsigned_value = strtoul(intcon->lexinfo, NULL, 0);
      if (errno == ERANGE) {
        return astree_create_errnode(intcon, BCC_TERR_CONST_TOO_SMALL, 1,
                                     intcon);
      } else {
        intcon->constant.integral.value = unsigned_value;
      }
    }
  } else {
    intcon->constant.integral.value = signed_value;
  }

  size_t lexinfo_len = strlen(intcon->lexinfo);
  /* TODO(Robert): This is... disgusting. Perhaps there is a better way. */
  if (lexinfo_len == 1) {
    intcon->type = &SPEC_INT;
  } else if (intcon->lexinfo[lexinfo_len - 1] == 'u' ||
             intcon->lexinfo[lexinfo_len - 1] == 'U') {
    if (intcon->lexinfo[lexinfo_len - 2] == 'l' ||
        intcon->lexinfo[lexinfo_len - 2] == 'L') {
      intcon->type == &SPEC_ULONG;
    } else if (signed_value >= INT_MIN && signed_value <= INT_MAX) {
      intcon->type = &SPEC_UINT;
    } else {
      intcon->type = &SPEC_ULONG;
    }
  } else if (intcon->lexinfo[lexinfo_len - 1] == 'l' ||
             intcon->lexinfo[lexinfo_len - 1] == 'L') {
    if (intcon->lexinfo[lexinfo_len - 2] == 'u' ||
        intcon->lexinfo[lexinfo_len - 2] == 'U') {
      intcon->type == &SPEC_ULONG;
    } else if (unsigned_value == 0) {
      intcon->type = &SPEC_LONG;
    } else {
      intcon->type = &SPEC_ULONG;
    }
  } else if (intcon->lexinfo[0] == '0') {
    if (unsigned_value != 0) {
      intcon->type = &SPEC_ULONG;
    } else if (signed_value >= INT_MIN && signed_value <= INT_MAX) {
      intcon->type = &SPEC_INT;
    } else if (signed_value >= 0 && signed_value <= UINT_MAX) {
      intcon->type = &SPEC_UINT;
    } else {
      intcon->type = &SPEC_LONG;
    }
  } else if (unsigned_value != 0) {
    intcon->type = &SPEC_ULONG;
  } else if (signed_value <= INT_MAX && signed_value >= INT_MIN) {
    intcon->type = &SPEC_INT;
  } else {
    intcon->type = &SPEC_LONG;
  }
  return intcon;
}

ASTree *evaluate_charcon(ASTree *charcon) {
  charcon->attributes |= ATTR_EXPR_CONST;
  const char *const_str = charcon->lexinfo + 1;
  size_t const_str_len = strlen(const_str) - 1;
  if (const_str[0] == '\\') {
    if (const_str[1] == 'x') {
      /* hex number */
      charcon->constant.integral.value = strtol(&const_str[2], NULL, 16);
    } else if (isdigit(const_str[1])) {
      /* octal number */
      charcon->constant.integral.value = strtol(&const_str[1], NULL, 8);
    } else {
      /* ASCII control sequence, \?, \", \', or \\ */
      switch (const_str[1]) {
        case 'n':
          charcon->constant.integral.value = '\n';
          break;
        case 't':
          charcon->constant.integral.value = '\t';
          break;
        case 'v':
          charcon->constant.integral.value = '\v';
          break;
        case 'b':
          charcon->constant.integral.value = '\b';
          break;
        case 'r':
          charcon->constant.integral.value = '\r';
          break;
        case 'f':
          charcon->constant.integral.value = '\f';
          break;
        case 'a':
          charcon->constant.integral.value = '\a';
          break;
        case '\\':
          charcon->constant.integral.value = '\\';
          break;
        case '\?':
          charcon->constant.integral.value = '\?';
          break;
        case '\'':
          charcon->constant.integral.value = '\'';
          break;
        case '"':
          charcon->constant.integral.value = '"';
          break;
        default:
          charcon->constant.integral.value = '\0';
      }
    }
  } else {
    charcon->constant.integral.value = const_str[0];
  }
  return charcon;
}

ASTree *evaluate_ident(ASTree *ident) {
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strlen(id_str);
  SymbolValue *symval = NULL;
  (void)state_get_symbol(state, id_str, id_str_len, &symval);
  if (symval->flags & (SYMFLAG_STORE_EXT | SYMFLAG_STORE_STAT)) {
    ident->attributes |= ATTR_EXPR_CONST;
    ident->attributes |= ATTR_CONST_INIT;
    ident->attributes |= ATTR_CONST_ADDR;
    ident->constant.address.label = ident->lexinfo;
    ident->constant.address.offset = 0;
  } else if (symval->flags & SYMFLAG_ENUM_CONST) {
    ident->attributes |= ATTR_EXPR_CONST;
    AuxSpec *enum_aux = llist_back(&ident->type->auxspecs);
    TagValue *tagval = enum_aux->data.tag.val;
    int *value = map_get(&tagval->data.enumerators.by_name,
                         (char *)ident->lexinfo, strlen(ident->lexinfo));
    ident->constant.integral.value = *value;
  }
  return ident;
}

ASTree *evaluate_addition(ASTree *addition, ASTree *left, ASTree *right) {
  if (!(left->attributes & right->attributes & ATTR_EXPR_CONST) ||
      (left->attributes & right->attributes & ATTR_CONST_ADDR)) {
    maybe_load_cexpr(left);
    maybe_load_cexpr(right);
    return translate_addition(addition, left, right);
  } else if ((left->attributes & ATTR_CONST_ADDR)) {
    addition->attributes |= left->attributes & ATTR_MASK_CONST;
    addition->constant.address.label = left->constant.address.label;
    addition->constant.address.offset = (long)left->constant.address.offset +
                                        (long)right->constant.integral.value;
  } else if ((right->attributes & ATTR_CONST_ADDR)) {
    addition->attributes |= right->attributes & ATTR_MASK_CONST;
    addition->constant.address.label = right->constant.address.label;
    addition->constant.address.offset = (long)left->constant.integral.value +
                                        (long)right->constant.address.offset;
  } else {
    addition->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    CAST_BINARY(+, addition, left, right);
  }
  return astree_adopt(addition, 2, left, right);
}

ASTree *evaluate_subtraction(ASTree *subtraction, ASTree *left, ASTree *right) {
  if ((left->attributes & right->attributes & ATTR_CONST_ADDR) &&
      strcmp(left->constant.address.label, right->constant.address.label) ==
          0) {
    subtraction->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    subtraction->constant.integral.value = (long)left->constant.address.offset -
                                           (long)right->constant.address.offset;
  } else if ((left->attributes & ATTR_CONST_ADDR) &&
             (right->attributes & ATTR_EXPR_CONST) &&
             !(right->attributes & ATTR_CONST_ADDR)) {
    subtraction->attributes |= left->attributes & ATTR_MASK_CONST;
    subtraction->constant.address.label = left->constant.address.label;
    subtraction->constant.address.offset = (long)left->constant.address.offset -
                                           (long)right->constant.integral.value;
  } else if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&
             !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {
    subtraction->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    CAST_BINARY(-, subtraction, left, right);
  } else {
    maybe_load_cexpr(left);
    maybe_load_cexpr(right);
    return translate_addition(subtraction, left, right);
  }
  return astree_adopt(subtraction, 2, left, right);
}

ASTree *evaluate_shiftl(ASTree *shiftl, ASTree *left, ASTree *right) {
  if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&
      !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {
    shiftl->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    size_t width = typespec_get_width(shiftl->type);
    int is_signed = typespec_is_signed(shiftl->type);
    shiftl->constant.integral.value =
        SELECT_CAST(SELECT_CAST(left->constant.integral.value, width, is_signed)
                        << right->constant.integral.value,
                    width, is_signed);
    return astree_adopt(shiftl, 2, left, right);
  } else {
    maybe_load_cexpr(left);
    maybe_load_cexpr(right);
    return translate_binop(shiftl, left, right);
  }
}

ASTree *evaluate_shiftr(ASTree *shiftr, ASTree *left, ASTree *right) {
  if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&
      !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {
    shiftr->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    size_t width = typespec_get_width(shiftr->type);
    int is_signed = typespec_is_signed(shiftr->type);
    shiftr->constant.integral.value =
        SELECT_CAST(SELECT_CAST(left->constant.integral.value, width, is_signed)
                        << right->constant.integral.value,
                    width, is_signed);
    return astree_adopt(shiftr, 2, left, right);
  } else {
    maybe_load_cexpr(left);
    maybe_load_cexpr(right);
    return translate_binop(shiftr, left, right);
  }
}

ASTree *evaluate_relational(ASTree *relational, ASTree *left, ASTree *right) {
  int pointer_relation =
      (left->attributes & right->attributes & ATTR_CONST_ADDR) &&
      strcmp(left->constant.address.label, right->constant.address.label) == 0;
  int arithmetic_relation =
      (left->attributes & right->attributes & ATTR_EXPR_CONST) &&
      !((left->attributes | right->attributes) & ATTR_CONST_ADDR);
  if (pointer_relation || arithmetic_relation) {
    relational->attributes |=
        ATTR_EXPR_CONST |
        ((left->attributes | right->attributes) & ATTR_CONST_INIT);
    const TypeSpec *common_type =
        typespec_is_pointer(left->type) || typespec_is_pointer(right->type)
            ? &SPEC_LONG
            : arithmetic_conversions(left->type, right->type);
    size_t width = typespec_get_width(common_type);
    int is_signed = typespec_is_signed(common_type);
    switch (relational->symbol) {
      case '<':
        relational->constant.integral.value =
            SELECT_CAST(left->constant.integral.value, width, is_signed) <
            SELECT_CAST(left->constant.integral.value, width, is_signed);
        break;
      case TOK_LE:
        relational->constant.integral.value =
            SELECT_CAST(left->constant.integral.value, width, is_signed) <=
            SELECT_CAST(left->constant.integral.value, width, is_signed);
        break;
      case '>':
        relational->constant.integral.value =
            SELECT_CAST(left->constant.integral.value, width, is_signed) >
            SELECT_CAST(left->constant.integral.value, width, is_signed);
        break;
      case TOK_GE:
        relational->constant.integral.value =
            SELECT_CAST(left->constant.integral.value, width, is_signed) >=
            SELECT_CAST(left->constant.integral.value, width, is_signed);
        break;
      default:
        abort();
    }
    return astree_adopt(relational, 2, left, right);
  } else {
    maybe_load_cexpr(left);
    maybe_load_cexpr(right);
    return translate_comparison(relational, left, right);
  }
}

ASTree *evaluate_equality(ASTree *equality, ASTree *left, ASTree *right) {
  if (left->attributes & right->attributes & ATTR_CONST_ADDR) {
    equality->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    equality->constant.integral.value =
        strcmp(left->constant.address.label, right->constant.address.label) ==
            0 &&
        (left->constant.address.offset == right->constant.address.offset);
  } else if ((left->attributes & ATTR_CONST_ADDR) &&
             (right->attributes & ATTR_EXPR_CONST) &&
             right->constant.integral.value == 0) {
    equality->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    equality->constant.integral.value = equality->symbol == TOK_NE;
  } else if ((right->attributes & ATTR_CONST_ADDR) &&
             (left->attributes & ATTR_EXPR_CONST) &&
             left->constant.integral.value == 0) {
    equality->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    equality->constant.integral.value = equality->symbol == TOK_NE;
  } else if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&
             !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {
    equality->attributes |= ATTR_EXPR_CONST;
    const TypeSpec *promoted_type =
        arithmetic_conversions(left->type, right->type);
    size_t width = typespec_get_width(promoted_type);
    int is_signed = typespec_is_signed(promoted_type);
    if (equality->symbol == TOK_EQ) {
      equality->constant.integral.value =
          SELECT_CAST(left->constant.integral.value, width, is_signed) ==
          SELECT_CAST(right->constant.integral.value, width, is_signed);
    } else {
      equality->constant.integral.value =
          SELECT_CAST(left->constant.integral.value, width, is_signed) !=
          SELECT_CAST(right->constant.integral.value, width, is_signed);
    }
  } else {
    maybe_load_cexpr(left);
    maybe_load_cexpr(right);
    return translate_comparison(equality, left, right);
  }
  return astree_adopt(equality, 2, left, right);
}

ASTree *evaluate_logical(ASTree *logical, ASTree *left, ASTree *right) {
  if (left->attributes & right->attributes & ATTR_EXPR_CONST) {
    logical->attributes =
        ATTR_EXPR_CONST |
        ((left->attributes | right->attributes) & ATTR_CONST_INIT);
    if (logical->symbol == TOK_OR) {
      logical->constant.integral.value =
          ((left->attributes & ATTR_CONST_ADDR) ||
           left->constant.integral.value) ||
          ((right->attributes & ATTR_CONST_ADDR) ||
           right->constant.integral.value);
    } else {
      logical->constant.integral.value =
          ((left->attributes & ATTR_CONST_ADDR) ||
           left->constant.integral.value) &&
          ((right->attributes & ATTR_CONST_ADDR) ||
           right->constant.integral.value);
    }
    return astree_adopt(logical, 2, left, right);
  } else {
    maybe_load_cexpr(left);
    maybe_load_cexpr(right);
    return translate_logical(logical, left, right);
  }
}

ASTree *evaluate_cast(ASTree *cast, ASTree *expr) {
  if (expr->attributes & ATTR_EXPR_CONST) {
    cast->attributes |= expr->attributes & ATTR_MASK_CONST;
    if (expr->attributes & ATTR_CONST_ADDR) {
      cast->constant = expr->constant;
    } else {
      if (typespec_is_pointer(cast->type)) cast->attributes |= ATTR_CONST_INIT;
      size_t width = typespec_get_width(cast->type);
      /* is_signed can be used on pointers, and returns true */
      int is_signed = typespec_is_signed(cast->type);
      cast->constant.integral.value =
          SELECT_CAST(expr->constant.integral.value, width, is_signed);
    }
    return astree_adopt(cast, 1, expr);
  } else {
    maybe_load_cexpr(expr);
    return translate_cast(cast, expr);
  }
}

ASTree *evaluate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr) {
  if (condition->attributes & true_expr->attributes & false_expr->attributes &
      ATTR_EXPR_CONST) {
    ASTree *selected_expr = (condition->attributes & ATTR_CONST_ADDR) ||
                                    condition->constant.integral.value
                                ? true_expr
                                : false_expr;
    qmark->constant = selected_expr->constant;
    qmark->attributes |= (selected_expr->attributes & ATTR_MASK_CONST) |
                         (condition->attributes & ATTR_CONST_INIT);
    if (typespec_is_arithmetic(qmark->type) &&
        !(selected_expr->attributes & ATTR_CONST_ADDR)) {
      size_t width = typespec_get_width(qmark->type);
      int is_signed = typespec_is_signed(qmark->type);
      qmark->constant.integral.value =
          SELECT_CAST(selected_expr->constant.integral.value, width, is_signed);
    } else if (typespec_is_pointer(qmark->type) &&
               !(selected_expr->attributes & ATTR_CONST_ADDR)) {
      qmark->constant.integral.value = SELECT_CAST(
          selected_expr->constant.integral.value, X64_SIZEOF_LONG, TYPE_SIGNED);
    } else {
      qmark->constant = selected_expr->constant;
    }
    return astree_adopt(qmark, 3, condition, true_expr, false_expr);
  } else {
    maybe_load_cexpr(condition);
    maybe_load_cexpr(true_expr);
    maybe_load_cexpr(false_expr);
    return translate_conditional(qmark, condition, true_expr, false_expr);
  }
}

ASTree *evaluate_binop(ASTree *operator, ASTree * left, ASTree *right) {
  switch (operator->symbol) {
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
      return evaluate_shiftl(operator, left, right);
    case TOK_SHR:
      return evaluate_shiftr(operator, left, right);
    case '+':
      return evaluate_addition(operator, left, right);
    case '-':
      return evaluate_subtraction(operator, left, right);
    default:
      fprintf(stderr,
              "FATAL: attempted to evaluate constant expression with "
              "unknown binary operation %s\n",
              parser_get_tname(operator->symbol));
      abort();
  }
}

ASTree *evaluate_unop(ASTree *operator, ASTree * operand) {
  switch (operator->symbol) {
    UNOP_CASE(TOK_NEG, -, translate_unop);
    UNOP_CASE('~', ~, translate_unop);
    case TOK_POS:
      if ((operator->attributes & ATTR_EXPR_CONST) &&
          !(operator->attributes & ATTR_CONST_ADDR)) {
        /* most of this is probably unnecessary since everything gets cast up
         * to unsigned long when it is stored on the tree, but i have no idea
         */
        size_t width = typespec_get_width(operand->type);
        int is_signed = typespec_is_signed(operand->type);
        operator->attributes = operand->attributes;
        operator->constant.integral.value = +
            SELECT_CAST(operand->constant.integral.value, width, is_signed);
        return astree_adopt(operator, 1, operand);
      } else {
        /* treat like a cast */
        maybe_load_cexpr(operand);
        return translate_cast(operator, operand);
      }
    case '!':
      if ((operator->attributes & ATTR_EXPR_CONST) &&
          !(operator->attributes & ATTR_CONST_ADDR)) {
        operator->attributes = operand->attributes;
        size_t width = typespec_get_width(operator->type);
        int is_signed = typespec_is_signed(operator->type);
        operator->constant.integral.value = !SELECT_CAST(
            operand->constant.integral.value, width, is_signed);
        return astree_adopt(operator, 1, operand);
      } else {
        maybe_load_cexpr(operand);
        return translate_logical_not(operator, operand);
      }
    case TOK_SIZEOF:
      operator->attributes |= ATTR_EXPR_CONST;
      operator->constant.integral.value = typespec_get_width(operand->type);
      return astree_adopt(operator, 1, operand);
    default:
      fprintf(stderr,
              "FATAL: attempted to evaluate constant expression with "
              "unknown unary operation %s\n",
              parser_get_tname(operator->symbol));
      abort();
  }
}
