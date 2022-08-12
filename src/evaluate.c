#include "evaluate.h"

#include "ctype.h"
#include "errno.h"
#include "lyutils.h"
#include "stdlib.h"
#include "yyparse.h"

#define GEN_CASE(symbol, operator)                                     \
  case symbol:                                                         \
    binop->attributes |=                                               \
        left_op->attributes & right_op->attributes & ATTR_EXPR_CONST2; \
    if ((binop->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)    \
      return binop;                                                    \
    binop->constval = left_op->constval operator right_op->constval;   \
    return binop;
#define GEN_CASE_SIGNED(symbol, operator)                              \
  case symbol:                                                         \
    binop->attributes |=                                               \
        left_op->attributes & right_op->attributes & ATTR_EXPR_CONST2; \
    if ((binop->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)    \
      return binop;                                                    \
    if (left_op->type->base == TYPE_SIGNED) {                          \
      binop->constval =                                                \
          (long)left_op->constval operator(long) right_op->constval;   \
    } else {                                                           \
      binop->constval = left_op->constval operator right_op->constval; \
    }                                                                  \
    return binop;

ASTree *evaluate_intcon(ASTree *intcon) {
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
        intcon->constval = unsigned_value;
      }
    }
  } else {
    intcon->constval = signed_value;
  }

  intcon->attributes |= ATTR_EXPR_CONST2;
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
  charcon->attributes |= ATTR_EXPR_CONST2;
  const char *const_str = charcon->lexinfo + 1;
  size_t const_str_len = strlen(const_str) - 1;
  if (const_str[0] == '\\') {
    if (const_str[1] == 'x') {
      /* hex number */
      charcon->constval = strtol(&const_str[2], NULL, 16);
    } else if (isdigit(const_str[1])) {
      /* octal number */
      charcon->constval = strtol(&const_str[1], NULL, 8);
    } else {
      /* ASCII control sequence, \?, \", \', or \\ */
      switch (const_str[1]) {
        case 'n':
          charcon->constval = '\n';
          break;
        case 't':
          charcon->constval = '\t';
          break;
        case 'v':
          charcon->constval = '\v';
          break;
        case 'b':
          charcon->constval = '\b';
          break;
        case 'r':
          charcon->constval = '\r';
          break;
        case 'f':
          charcon->constval = '\f';
          break;
        case 'a':
          charcon->constval = '\a';
          break;
        case '\\':
          charcon->constval = '\\';
          break;
        case '\?':
          charcon->constval = '\?';
          break;
        case '\'':
          charcon->constval = '\'';
          break;
        case '"':
          charcon->constval = '"';
          break;
        default:
          charcon->constval = '\0';
      }
    }
  } else {
    charcon->constval = const_str[0];
  }
  return charcon;
}

ASTree *evaluate_ident(ASTree *ident) {
  if (ident->attributes & ATTR_EXPR_CONST2) {
    AuxSpec *enum_aux = llist_back(&ident->type->auxspecs);
    TagValue *tagval = enum_aux->data.tag.val;
    int *value = map_get(&tagval->data.enumerators.by_name,
                         (char *)ident->lexinfo, strlen(ident->lexinfo));
    ident->constval = *value;
  }
  return ident;
}

ASTree *evaluate_addition(ASTree *addition) {
  ASTree *left_op = astree_get(addition, 0);
  ASTree *right_op = astree_get(addition, 1);
  addition->attributes |=
      left_op->attributes & right_op->attributes & ATTR_EXPR_CONST2;
  if ((addition->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
    return addition;
  if (left_op->type->base == TYPE_SIGNED) {
    addition->constval = (long)left_op->constval + (long)right_op->constval;
  } else {
    addition->constval = left_op->constval + right_op->constval;
  }
  return addition;
}

ASTree *evaluate_subtraction(ASTree *subtraction) {
  ASTree *left_op = astree_get(subtraction, 0);
  ASTree *right_op = astree_get(subtraction, 1);
  subtraction->attributes |=
      left_op->attributes & right_op->attributes & ATTR_EXPR_CONST2;
  if ((subtraction->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
    return subtraction;
  if (left_op->type->base == TYPE_SIGNED) {
    subtraction->constval = (long)left_op->constval - (long)right_op->constval;
  } else {
    subtraction->constval = left_op->constval - right_op->constval;
  }
  return subtraction;
}

ASTree *evaluate_shiftl(ASTree *shiftl) {
  ASTree *left_op = astree_get(shiftl, 0);
  ASTree *right_op = astree_get(shiftl, 1);
  shiftl->attributes |=
      left_op->attributes & right_op->attributes & ATTR_EXPR_CONST2;
  if ((shiftl->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
    return shiftl;
  if (right_op->type->base == TYPE_SIGNED) {
    shiftl->constval = left_op->constval << (long)right_op->constval;
  } else {
    shiftl->constval = left_op->constval << right_op->constval;
  }
  return shiftl;
}

ASTree *evaluate_shiftr(ASTree *shiftr) {
  ASTree *left_op = astree_get(shiftr, 0);
  ASTree *right_op = astree_get(shiftr, 1);
  shiftr->attributes |=
      left_op->attributes & right_op->attributes & ATTR_EXPR_CONST2;
  if ((shiftr->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
    return shiftr;
  if (left_op->type->base == TYPE_SIGNED) {
    if (right_op->type->base == TYPE_SIGNED) {
      shiftr->constval = (long)left_op->constval >> (long)right_op->constval;
    } else {
      shiftr->constval = (long)left_op->constval >> right_op->constval;
    }
  } else if (right_op->type->base == TYPE_UNSIGNED) {
    shiftr->constval = left_op->constval >> (long)right_op->constval;
  } else {
    shiftr->constval = left_op->constval >> right_op->constval;
  }
  return shiftr;
}

ASTree *evaluate_cast(ASTree *cast) {
  ASTree *expr = astree_get(cast, astree_count(cast) == 1 ? 0 : 1);
  if (typespec_is_arithmetic(cast->type) &&
      (expr->attributes & ATTR_EXPR_CONST2)) {
    cast->attributes |= ATTR_EXPR_CONST2;
    cast->constval = expr->constval;
  }
  return cast;
}

ASTree *evaluate_conditional(ASTree *conditional) {
  ASTree *condition = astree_get(conditional, 0);
  ASTree *true_expr = astree_get(conditional, 1);
  ASTree *false_expr = astree_get(conditional, 2);

  conditional->attributes |= true_expr->attributes & false_expr->attributes &
                             condition->attributes & ATTR_EXPR_CONST2;
  if ((conditional->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
    return conditional;
  if (condition->constval) {
    conditional->constval = true_expr->constval;
  } else {
    conditional->constval = false_expr->constval;
  }
  return conditional;
}

ASTree *evaluate_binop(ASTree *binop) {
  ASTree *left_op = astree_get(binop, 0);
  ASTree *right_op = astree_get(binop, 1);
  switch (binop->symbol) {
    GEN_CASE('*', *);
    GEN_CASE('&', &);
    GEN_CASE('|', |);
    GEN_CASE('^', ^);
    GEN_CASE(TOK_AND, &&);
    GEN_CASE(TOK_OR, ||);
    GEN_CASE_SIGNED('/', /);
    GEN_CASE_SIGNED('%', %);
    GEN_CASE_SIGNED(TOK_EQ, ==);
    GEN_CASE_SIGNED(TOK_NE, !=);
    GEN_CASE_SIGNED(TOK_GE, >=);
    GEN_CASE_SIGNED(TOK_LE, <=);
    GEN_CASE_SIGNED('>', >);
    GEN_CASE_SIGNED('<', <);
    case TOK_SHL:
      return evaluate_shiftl(binop);
    case TOK_SHR:
      return evaluate_shiftr(binop);
    case '+':
      return evaluate_addition(binop);
    case '-':
      return evaluate_subtraction(binop);
    default:
      fprintf(stderr,
              "FATAL: attempted to evaluate constant expression with "
              "unknown binary operation %s\n",
              parser_get_tname(binop->symbol));
      abort();
  }
}

ASTree *evaluate_unop(ASTree *unop) {
  ASTree *operand = astree_get(unop, 0);
  switch (unop->symbol) {
    case TOK_NEG:
      unop->attributes = operand->attributes & ATTR_EXPR_CONST2;
      if ((unop->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
        return unop;
      unop->constval = -operand->constval;
      return unop;
    case TOK_POS:
      unop->attributes = operand->attributes & ATTR_EXPR_CONST2;
      if ((unop->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
        return unop;
      unop->constval = operand->constval;
      return unop;
    case '~':
      unop->attributes = operand->attributes & ATTR_EXPR_CONST2;
      if ((unop->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
        return unop;
      unop->constval = ~operand->constval;
      return unop;
    case '!':
      unop->attributes = operand->attributes & ATTR_EXPR_CONST2;
      if ((unop->attributes & ATTR_EXPR_CONST2) != ATTR_EXPR_CONST2)
        return unop;
      unop->constval = !operand->constval;
      return unop;
    case TOK_SIZEOF:
      unop->attributes |= ATTR_EXPR_CONST2;
      unop->constval = typespec_get_width((TypeSpec *)operand->type);
      return unop;
    case TOK_POST_INC:
    case TOK_POST_DEC:
    case TOK_INC:
    case TOK_DEC:
      return unop;
    default:
      fprintf(stderr,
              "FATAL: attempted to evaluate constant expression with "
              "unknown unary operation %s\n",
              parser_get_tname(unop->symbol));
      abort();
  }
}
