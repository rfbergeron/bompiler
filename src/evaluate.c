#include "evaluate.h"

#include "ctype.h"
#include "errno.h"
#include "lyutils.h"
#include "state.h"
#include "stdlib.h"
#include "yyparse.h"

#define GEN_CASE(symbol, operator)                                            \
  case symbol:                                                                \
    if (!(binop->attributes & ATTR_EXPR_CONST) ||                             \
        (binop->attributes & ATTR_CONST_ADDR))                                \
      return binop;                                                           \
    binop->constant.integral.value =                                          \
        left_op->constant.integral.value operator right_op->constant.integral \
            .value;                                                           \
    return binop;
#define GEN_CASE_SIGNED(symbol, operator)                              \
  case symbol:                                                         \
    if (!(binop->attributes & ATTR_EXPR_CONST) ||                      \
        (binop->attributes & ATTR_CONST_ADDR))                         \
      return binop;                                                    \
    if (left_op->type->base == TYPE_SIGNED) {                          \
      binop->constant.integral.value =                                 \
          (long)left_op->constant.integral.value operator(long)        \
              right_op->constant.integral.value;                       \
    } else {                                                           \
      binop->constant.integral.value =                                 \
          left_op->constant.integral.value operator right_op->constant \
              .integral.value;                                         \
    }                                                                  \
    return binop;

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

ASTree *evaluate_addition(ASTree *addition) {
  ASTree *left_op = astree_get(addition, 0);
  ASTree *right_op = astree_get(addition, 1);
  if (left_op->attributes & right_op->attributes & ATTR_CONST_ADDR) {
    return addition;
  } else if ((left_op->attributes & ATTR_CONST_ADDR) &&
             (right_op->attributes & ATTR_EXPR_CONST)) {
    addition->attributes |= ATTR_EXPR_CONST | ATTR_CONST_ADDR | ATTR_CONST_INIT;
    addition->constant.address.label = left_op->constant.address.label;
    addition->constant.address.offset =
        left_op->constant.address.offset + right_op->constant.integral.value;
    return addition;
  } else if ((left_op->attributes & ATTR_EXPR_CONST) &&
             (right_op->attributes & ATTR_CONST_ADDR)) {
    addition->attributes |= ATTR_EXPR_CONST | ATTR_CONST_ADDR | ATTR_CONST_INIT;
    addition->constant.address.label = right_op->constant.address.label;
    addition->constant.address.offset =
        left_op->constant.integral.value + right_op->constant.address.offset;
    return addition;
  } else if (left_op->attributes & right_op->attributes & ATTR_EXPR_CONST) {
    addition->attributes |=
        ATTR_EXPR_CONST |
        ((left_op->attributes | right_op->attributes) & ATTR_CONST_INIT);
    addition->constant.integral.value =
        left_op->constant.integral.value + right_op->constant.integral.value;
    return addition;
  } else {
    return addition;
  }
}

ASTree *evaluate_subtraction(ASTree *subtraction) {
  ASTree *left_op = astree_get(subtraction, 0);
  ASTree *right_op = astree_get(subtraction, 1);
  if ((left_op->attributes & right_op->attributes & ATTR_CONST_ADDR) &&
      (left_op->constant.address.label == right_op->constant.address.label)) {
    subtraction->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    subtraction->constant.integral.value =
        right_op->constant.address.offset - left_op->constant.address.offset;
  } else if ((left_op->attributes & ATTR_CONST_ADDR) &&
             (right_op->attributes & ATTR_EXPR_CONST) &&
             !(right_op->attributes & ATTR_CONST_ADDR)) {
    subtraction->attributes |=
        ATTR_EXPR_CONST | ATTR_CONST_ADDR | ATTR_CONST_INIT;
    subtraction->constant.address.label = left_op->constant.address.label;
    subtraction->constant.address.offset =
        left_op->constant.address.offset - right_op->constant.integral.value;
  } else if ((left_op->attributes & right_op->attributes & ATTR_EXPR_CONST) &&
             !((left_op->attributes | right_op->attributes) &
               ATTR_CONST_ADDR)) {
    subtraction->attributes |=
        ATTR_EXPR_CONST |
        ((left_op->attributes | right_op->attributes) & ATTR_CONST_INIT);
    subtraction->constant.integral.value =
        left_op->constant.integral.value - right_op->constant.integral.value;
  }
  return subtraction;
}

ASTree *evaluate_shiftl(ASTree *shiftl) {
  ASTree *left_op = astree_get(shiftl, 0);
  ASTree *right_op = astree_get(shiftl, 1);
  if ((left_op->attributes & right_op->attributes & ATTR_EXPR_CONST) &&
      !((left_op->attributes | right_op->attributes) & ATTR_CONST_ADDR)) {
    shiftl->attributes |=
        ATTR_EXPR_CONST |
        ((left_op->attributes | right_op->attributes) & ATTR_CONST_INIT);
    if (right_op->type->base == TYPE_SIGNED) {
      shiftl->constant.integral.value =
          left_op->constant.integral.value
          << (long)right_op->constant.integral.value;
    } else {
      shiftl->constant.integral.value = left_op->constant.integral.value
                                        << right_op->constant.integral.value;
    }
  }
  return shiftl;
}

ASTree *evaluate_shiftr(ASTree *shiftr) {
  ASTree *left_op = astree_get(shiftr, 0);
  ASTree *right_op = astree_get(shiftr, 1);
  if ((left_op->attributes & right_op->attributes & ATTR_EXPR_CONST) &&
      !((left_op->attributes | right_op->attributes) & ATTR_CONST_ADDR)) {
    shiftr->attributes |=
        ATTR_EXPR_CONST |
        ((left_op->attributes | right_op->attributes) & ATTR_CONST_INIT);
    if (left_op->type->base == TYPE_SIGNED) {
      if (right_op->type->base == TYPE_SIGNED) {
        shiftr->constant.integral.value =
            (long)left_op->constant.integral.value >>
            (long)right_op->constant.integral.value;
      } else {
        shiftr->constant.integral.value =
            (long)left_op->constant.integral.value >>
            right_op->constant.integral.value;
      }
    } else if (right_op->type->base == TYPE_UNSIGNED) {
      shiftr->constant.integral.value = left_op->constant.integral.value >>
                                        (long)right_op->constant.integral.value;
    } else {
      shiftr->constant.integral.value =
          left_op->constant.integral.value >> right_op->constant.integral.value;
    }
  }
  return shiftr;
}

ASTree *evaluate_relational(ASTree *relational) {
  ASTree *left_op = astree_get(relational, 0);
  ASTree *right_op = astree_get(relational, 1);
  if ((left_op->attributes & right_op->attributes & ATTR_CONST_ADDR) &&
      left_op->constant.address.label == right_op->constant.address.label) {
    relational->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    switch (relational->symbol) {
      case '<':
        relational->constant.integral.value =
            (long)left_op->constant.address.offset <
            (long)right_op->constant.address.offset;
        break;
      case TOK_GE:
        relational->constant.integral.value =
            (long)left_op->constant.address.offset >=
            (long)right_op->constant.address.offset;
        break;
      case '>':
        relational->constant.integral.value =
            (long)left_op->constant.address.offset >
            (long)right_op->constant.address.offset;
        break;
      case TOK_LE:
        relational->constant.integral.value =
            (long)left_op->constant.address.offset <=
            (long)right_op->constant.address.offset;
        break;
      default:
        abort();
    }
  } else if ((left_op->attributes & right_op->attributes & ATTR_EXPR_CONST) &&
             !((left_op->attributes | right_op->attributes) &
               ATTR_CONST_ADDR)) {
    relational->attributes |=
        ATTR_EXPR_CONST |
        ((right_op->attributes | left_op->attributes) & ATTR_CONST_INIT);
    if (left_op->type->base == TYPE_SIGNED ||
        typespec_is_pointer(left_op->type)) {
      switch (relational->symbol) {
        case '<':
          relational->constant.integral.value =
              (long)left_op->constant.integral.value <
              (long)right_op->constant.integral.value;
          break;
        case TOK_GE:
          relational->constant.integral.value =
              (long)left_op->constant.integral.value >=
              (long)right_op->constant.integral.value;
          break;
        case '>':
          relational->constant.integral.value =
              (long)left_op->constant.integral.value >
              (long)right_op->constant.integral.value;
          break;
        case TOK_LE:
          relational->constant.integral.value =
              (long)left_op->constant.integral.value <=
              (long)right_op->constant.integral.value;
          break;
        default:
          abort();
      }
    } else {
      switch (relational->symbol) {
        case '<':
          relational->constant.integral.value =
              left_op->constant.integral.value <
              right_op->constant.integral.value;
          break;
        case TOK_GE:
          relational->constant.integral.value =
              left_op->constant.integral.value >=
              right_op->constant.integral.value;
          break;
        case '>':
          relational->constant.integral.value =
              left_op->constant.integral.value >
              right_op->constant.integral.value;
          break;
        case TOK_LE:
          relational->constant.integral.value =
              left_op->constant.integral.value <=
              right_op->constant.integral.value;
          break;
        default:
          abort();
      }
    }
  }
  return relational;
}

ASTree *evaluate_equality(ASTree *equality) {
  ASTree *left_op = astree_get(equality, 0);
  ASTree *right_op = astree_get(equality, 1);
  if (left_op->attributes & right_op->attributes & ATTR_CONST_ADDR) {
    equality->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    equality->constant.integral.value =
        (left_op->constant.address.label == right_op->constant.address.label) &&
        (left_op->constant.address.offset == right_op->constant.address.offset);
  } else if ((left_op->attributes & ATTR_CONST_ADDR) &&
             (right_op->attributes & ATTR_EXPR_CONST) &&
             right_op->constant.integral.value == 0) {
    equality->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    equality->constant.integral.value = equality->symbol == TOK_NE;
  } else if ((right_op->attributes & ATTR_CONST_ADDR) &&
             (left_op->attributes & ATTR_EXPR_CONST) &&
             left_op->constant.integral.value == 0) {
    equality->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    equality->constant.integral.value = equality->symbol == TOK_NE;
  } else if ((left_op->attributes & right_op->attributes & ATTR_EXPR_CONST) &&
             !((left_op->attributes | right_op->attributes) &
               ATTR_CONST_ADDR)) {
    equality->attributes |= ATTR_EXPR_CONST;
    if (equality->symbol == TOK_EQ) {
      equality->constant.integral.value =
          left_op->constant.integral.value == right_op->constant.integral.value;
    } else {
      equality->constant.integral.value =
          left_op->constant.integral.value != right_op->constant.integral.value;
    }
  }
  return equality;
}

ASTree *evaluate_cast(ASTree *cast) {
  ASTree *expr = astree_get(cast, astree_count(cast) == 1 ? 0 : 1);
  if (expr->attributes & ATTR_EXPR_CONST) {
    cast->attributes |= expr->attributes &
                        (ATTR_EXPR_CONST | ATTR_CONST_ADDR | ATTR_CONST_INIT);
    if (typespec_is_pointer(cast->type)) cast->attributes |= ATTR_CONST_INIT;
    cast->constant = expr->constant;
  }
  return cast;
}

ASTree *evaluate_conditional(ASTree *conditional) {
  ASTree *condition = astree_get(conditional, 0);
  ASTree *true_expr = astree_get(conditional, 1);
  ASTree *false_expr = astree_get(conditional, 2);

  if (condition->attributes & true_expr->attributes & false_expr->attributes &
      ATTR_EXPR_CONST) {
    if ((condition->attributes & ATTR_CONST_ADDR) ||
        condition->constant.integral.value) {
      conditional->constant = true_expr->constant;
      conditional->attributes |=
          (true_expr->attributes &
           (ATTR_EXPR_CONST | ATTR_CONST_ADDR | ATTR_CONST_INIT)) |
          (condition->attributes & ATTR_CONST_INIT);
    } else {
      conditional->constant = false_expr->constant;
      conditional->attributes |=
          (false_expr->attributes &
           (ATTR_EXPR_CONST | ATTR_CONST_ADDR | ATTR_CONST_INIT)) |
          (condition->attributes & ATTR_CONST_INIT);
    }
  }
  return conditional;
}

ASTree *evaluate_binop(ASTree *binop) {
  ASTree *left_op = astree_get(binop, 0);
  ASTree *right_op = astree_get(binop, 1);
  switch (binop->symbol) {
    GEN_CASE('&', &);
    GEN_CASE('|', |);
    GEN_CASE('^', ^);
    GEN_CASE(TOK_AND, &&);
    GEN_CASE(TOK_OR, ||);
    GEN_CASE_SIGNED('/', /);
    GEN_CASE_SIGNED('%', %);
    GEN_CASE_SIGNED('*', *);
    case TOK_EQ:
    case TOK_NE:
      return evaluate_equality(binop);
    case '<':
    case TOK_LE:
    case '>':
    case TOK_GE:
      return evaluate_relational(binop);
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
      if (!(unop->attributes & ATTR_EXPR_CONST) ||
          (unop->attributes & ATTR_CONST_ADDR))
        return unop;
      unop->constant.integral.value = -operand->constant.integral.value;
      return unop;
    case TOK_POS:
      if (!(unop->attributes & ATTR_EXPR_CONST) ||
          (unop->attributes & ATTR_CONST_ADDR))
        return unop;
      unop->constant.integral.value = +operand->constant.integral.value;
      return unop;
    case '~':
      if (!(unop->attributes & ATTR_EXPR_CONST) ||
          (unop->attributes & ATTR_CONST_ADDR))
        return unop;
      unop->constant.integral.value = ~operand->constant.integral.value;
      return unop;
    case '!':
      if (!(unop->attributes & ATTR_EXPR_CONST) ||
          (unop->attributes & ATTR_CONST_ADDR))
        return unop;
      unop->constant.integral.value = !operand->constant.integral.value;
      return unop;
    case TOK_SIZEOF:
      unop->constant.integral.value =
          typespec_get_width((TypeSpec *)operand->type);
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
