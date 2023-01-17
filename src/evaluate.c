#include "evaluate.h"

#include "asmgen.h"
#include "assert.h"
#include "ctype.h"
#include "errno.h"
#include "lyutils.h"
#include "state.h"
#include "stdlib.h"
#include "tchk_common.h"
#include "yyparse.h"

#define SHCA(optext, type, dest, left_value, right_value)       \
  do {                                                          \
    size_t selector = typespec_get_width(type)                  \
                      << 4 * typespec_is_signed(type);          \
    switch (selector) {                                         \
      case X64_SIZEOF_CHAR:                                     \
        dest = (unsigned char)(left_value)optext(right_value);  \
        break;                                                  \
      case X64_SIZEOF_SHORT:                                    \
        dest = (unsigned short)(left_value)optext(right_value); \
        break;                                                  \
      case X64_SIZEOF_INT:                                      \
        dest = (unsigned int)(left_value)optext(right_value);   \
        break;                                                  \
      case X64_SIZEOF_LONG:                                     \
        dest = (unsigned long)(left_value)optext(right_value);  \
        break;                                                  \
      case X64_SIZEOF_CHAR << 4:                                \
        dest = (signed char)(left_value)optext(right_value);    \
        break;                                                  \
      case X64_SIZEOF_SHORT << 4:                               \
        dest = (short)(left_value)optext(right_value);          \
        break;                                                  \
      case X64_SIZEOF_INT << 4:                                 \
        dest = (int)(left_value)optext(right_value);            \
        break;                                                  \
      case X64_SIZEOF_LONG << 4:                                \
        dest = (long)(left_value)optext(right_value);           \
        break;                                                  \
    }                                                           \
  } while (0)
#define BINCA(optext, type, dest, left_value, right_value)                    \
  do {                                                                        \
    size_t selector = typespec_get_width(type)                                \
                      << 4 * typespec_is_signed(type);                        \
    switch (selector) {                                                       \
      case X64_SIZEOF_CHAR:                                                   \
        dest = (unsigned char)(left_value)optext(unsigned char)(right_value); \
        break;                                                                \
      case X64_SIZEOF_SHORT:                                                  \
        dest =                                                                \
            (unsigned short)(left_value)optext(unsigned short)(right_value);  \
        break;                                                                \
      case X64_SIZEOF_INT:                                                    \
        dest = (unsigned int)(left_value)optext(unsigned int)(right_value);   \
        break;                                                                \
      case X64_SIZEOF_LONG:                                                   \
        dest = (unsigned long)(left_value)optext(unsigned long)(right_value); \
        break;                                                                \
      case X64_SIZEOF_CHAR << 4:                                              \
        dest = (signed char)(left_value)optext(signed char)(right_value);     \
        break;                                                                \
      case X64_SIZEOF_SHORT << 4:                                             \
        dest = (short)(left_value)optext(short)(right_value);                 \
        break;                                                                \
      case X64_SIZEOF_INT << 4:                                               \
        dest = (int)(left_value)optext(int)(right_value);                     \
        break;                                                                \
      case X64_SIZEOF_LONG << 4:                                              \
        dest = (long)(left_value)optext(long)(right_value);                   \
        break;                                                                \
    }                                                                         \
  } while (0)
#define UNCA(optext, type, dest, value)                \
  do {                                                 \
    size_t selector = typespec_get_width(type)         \
                      << 4 * typespec_is_signed(type); \
    switch (selector) {                                \
      case X64_SIZEOF_CHAR:                            \
        dest = optext(unsigned char)(value);           \
        break;                                         \
      case X64_SIZEOF_SHORT:                           \
        dest = optext(unsigned short)(value);          \
        break;                                         \
      case X64_SIZEOF_INT:                             \
        dest = optext(unsigned int)(value);            \
        break;                                         \
      case X64_SIZEOF_LONG:                            \
        dest = optext(unsigned long)(value);           \
        break;                                         \
      case X64_SIZEOF_CHAR << 4:                       \
        dest = optext(signed char)(value);             \
        break;                                         \
      case X64_SIZEOF_SHORT << 4:                      \
        dest = optext(short)(value);                   \
        break;                                         \
      case X64_SIZEOF_INT << 4:                        \
        dest = optext(int)(value);                     \
        break;                                         \
      case X64_SIZEOF_LONG << 4:                       \
        dest = optext(long)(value);                    \
        break;                                         \
    }                                                  \
  } while (0)
#define BINOP_CASE(opchar, optext, optrans)                                 \
  case opchar:                                                              \
    if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&         \
        !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {      \
      operator->attributes |=(left->attributes | right->attributes) &       \
          ATTR_MASK_CONST;                                                  \
      BINCA(optext, operator->type, operator->constant.integral.value,      \
            left->constant.integral.value, right->constant.integral.value); \
      return astree_adopt(operator, 2, left, right);                        \
    } else {                                                                \
      maybe_load_cexpr(right, NULL);                                        \
      maybe_load_cexpr(left, right->first_instr);                           \
      return optrans(operator, left, right);                                \
    }
#define UNOP_CASE(opchar, optext, optrans)                            \
  case opchar:                                                        \
    if ((operator->attributes & ATTR_EXPR_CONST) &&                   \
        !(operator->attributes & ATTR_CONST_ADDR)) {                  \
      operator->attributes |= operand->attributes & ATTR_MASK_CONST;  \
      UNCA(optext, operator->type, operator->constant.integral.value, \
           operand->constant.integral.value);                         \
      return astree_adopt(operator, 1, operand);                      \
    } else {                                                          \
      maybe_load_cexpr(operand, NULL);                                \
      return optrans(operator, operand);                              \
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

ASTree *evaluate_stringcon(ASTree *stringcon) {
  const char *stringcon_label;
  (void)asmgen_literal_label(stringcon->lexinfo, &stringcon_label);
  assert(stringcon_label != NULL);
  stringcon->attributes |= ATTR_CONST_INIT | ATTR_CONST_ADDR | ATTR_EXPR_CONST;
  stringcon->constant.address.label = stringcon_label;
  stringcon->constant.address.disp = 0;
  (void)state_get_symbol(state, stringcon_label, strlen(stringcon_label),
                         &stringcon->constant.address.symval);
  assert(stringcon->constant.address.symval != NULL);
  return stringcon;
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
    ident->constant.address.symval = symval;
    ident->constant.address.disp = 0;
    if (typespec_is_function(&symval->type))
      ident->constant.address.label = mk_fnptr_text(ident->lexinfo);
    else if (symval->flags & SYMFLAG_LINK_NONE)
      ident->constant.address.label =
          mk_static_label(ident->lexinfo, symval->static_id);
    else
      ident->constant.address.label = ident->lexinfo;
    return ident;
  } else if (symval->flags & SYMFLAG_ENUM_CONST) {
    ident->attributes |= ATTR_EXPR_CONST;
    AuxSpec *enum_aux = llist_back(&ident->type->auxspecs);
    TagValue *tagval = enum_aux->data.tag.val;
    int *value = map_get(&tagval->data.enumerators.by_name,
                         (char *)ident->lexinfo, strlen(ident->lexinfo));
    ident->constant.integral.value = *value;
    return ident;
  } else {
    return translate_ident(ident);
  }
}

ASTree *evaluate_addition(ASTree *addition, ASTree *left, ASTree *right) {
  if (!(left->attributes & right->attributes & ATTR_EXPR_CONST) ||
      (left->attributes & right->attributes & ATTR_CONST_ADDR) ||
      ((left->attributes & ATTR_CONST_ADDR) &&
       typespec_is_pointer(right->type)) ||
      ((right->attributes & ATTR_CONST_ADDR) &&
       typespec_is_pointer(left->type))) {
    maybe_load_cexpr(right, NULL);
    maybe_load_cexpr(left, right->first_instr);
    return translate_addition(addition, left, right);
  } else if ((left->attributes & ATTR_CONST_ADDR)) {
    size_t stride =
        typespec_is_pointer(left->type) ? typespec_elem_width(left->type) : 1;
    addition->attributes |= left->attributes & ATTR_MASK_CONST;
    addition->constant = left->constant;
    addition->constant.address.disp +=
        (long)(right->constant.integral.value * stride);
  } else if ((right->attributes & ATTR_CONST_ADDR)) {
    size_t stride =
        typespec_is_pointer(right->type) ? typespec_elem_width(right->type) : 1;
    addition->attributes |= right->attributes & ATTR_MASK_CONST;
    addition->constant = right->constant;
    addition->constant.address.disp +=
        (long)(left->constant.integral.value * stride);
  } else {
    size_t left_stride =
        typespec_is_pointer(right->type) ? typespec_elem_width(right->type) : 1;
    size_t right_stride =
        typespec_is_pointer(left->type) ? typespec_elem_width(left->type) : 1;
    addition->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    BINCA(+, addition->type, addition->constant.integral.value,
          left->constant.integral.value * left_stride,
          right->constant.integral.value * right_stride);
  }
  return astree_adopt(addition, 2, left, right);
}

ASTree *evaluate_subtraction(ASTree *subtraction, ASTree *left, ASTree *right) {
  if ((left->attributes & right->attributes & ATTR_CONST_ADDR) &&
      left->constant.address.symval == right->constant.address.symval &&
      (typespec_is_pointer(right->type) || !typespec_is_pointer(left->type))) {
    size_t stride =
        typespec_is_pointer(left->type) ? typespec_elem_width(left->type) : 1;
    subtraction->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    subtraction->constant.integral.value =
        (left->constant.address.disp - right->constant.address.disp) /
        (long)stride;
  } else if ((left->attributes & ATTR_CONST_ADDR) &&
             (right->attributes & ATTR_EXPR_CONST) &&
             !(right->attributes & ATTR_CONST_ADDR)) {
    size_t stride =
        typespec_is_pointer(left->type) ? typespec_elem_width(left->type) : 1;
    subtraction->attributes |= left->attributes & ATTR_MASK_CONST;
    subtraction->constant = left->constant;
    subtraction->constant.address.disp -=
        (long)(right->constant.integral.value * stride);
  } else if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&
             !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {
    size_t left_stride =
        typespec_is_pointer(right->type) ? typespec_elem_width(right->type) : 1;
    size_t right_stride =
        typespec_is_pointer(left->type) ? typespec_elem_width(left->type) : 1;
    subtraction->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    BINCA(-, subtraction->type, subtraction->constant.integral.value,
          left->constant.integral.value * left_stride,
          right->constant.integral.value * right_stride);
  } else {
    maybe_load_cexpr(right, NULL);
    maybe_load_cexpr(left, right->first_instr);
    return translate_addition(subtraction, left, right);
  }
  return astree_adopt(subtraction, 2, left, right);
}

ASTree *evaluate_shiftl(ASTree *shiftl, ASTree *left, ASTree *right) {
  if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&
      !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {
    shiftl->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    SHCA(<<, shiftl->type, shiftl->constant.integral.value,
         left->constant.integral.value, right->constant.integral.value);
    return astree_adopt(shiftl, 2, left, right);
  } else {
    maybe_load_cexpr(right, NULL);
    maybe_load_cexpr(left, right->first_instr);
    return translate_binop(shiftl, left, right);
  }
}

ASTree *evaluate_shiftr(ASTree *shiftr, ASTree *left, ASTree *right) {
  if ((left->attributes & right->attributes & ATTR_EXPR_CONST) &&
      !((left->attributes | right->attributes) & ATTR_CONST_ADDR)) {
    shiftr->attributes |=
        (left->attributes | right->attributes) & ATTR_MASK_CONST;
    SHCA(>>, shiftr->type, shiftr->constant.integral.value,
         left->constant.integral.value, right->constant.integral.value);
    return astree_adopt(shiftr, 2, left, right);
  } else {
    maybe_load_cexpr(right, NULL);
    maybe_load_cexpr(left, right->first_instr);
    return translate_binop(shiftr, left, right);
  }
}

ASTree *evaluate_relational(ASTree *relational, ASTree *left, ASTree *right) {
  int pointer_relation =
      (left->attributes & right->attributes & ATTR_CONST_ADDR) &&
      left->constant.address.symval == right->constant.address.symval;
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
    switch (relational->symbol) {
      case '<':
        BINCA(<, common_type, relational->constant.integral.value,
              left->constant.integral.value, right->constant.integral.value);
        break;
      case TOK_LE:
        BINCA(<=, common_type, relational->constant.integral.value,
              left->constant.integral.value, right->constant.integral.value);
        break;
      case '>':
        BINCA(>, common_type, relational->constant.integral.value,
              left->constant.integral.value, right->constant.integral.value);
        break;
      case TOK_GE:
        BINCA(>=, common_type, relational->constant.integral.value,
              left->constant.integral.value, right->constant.integral.value);
        break;
      default:
        abort();
    }
    return astree_adopt(relational, 2, left, right);
  } else {
    maybe_load_cexpr(right, NULL);
    maybe_load_cexpr(left, right->first_instr);
    return translate_comparison(relational, left, right);
  }
}

ASTree *evaluate_equality(ASTree *equality, ASTree *left, ASTree *right) {
  if (left->attributes & right->attributes & ATTR_CONST_ADDR) {
    equality->attributes |= ATTR_EXPR_CONST | ATTR_CONST_INIT;
    equality->constant.integral.value =
        left->constant.address.symval == right->constant.address.symval &&
        left->constant.address.disp == right->constant.address.disp;
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
    BINCA(==, promoted_type, equality->constant.integral.value,
          left->constant.integral.value, right->constant.integral.value);
    equality->constant.integral.value ^= equality->symbol == TOK_NE;
  } else {
    maybe_load_cexpr(right, NULL);
    maybe_load_cexpr(left, right->first_instr);
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
    maybe_load_cexpr(right, NULL);
    maybe_load_cexpr(left, right->first_instr);
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
      /* is_signed can be used on pointers, and returns true */
      UNCA(+, cast->type, cast->constant.integral.value,
           expr->constant.integral.value);
    }
    return astree_adopt(cast, 1, expr);
  } else {
    maybe_load_cexpr(expr, NULL);
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
    if (!(selected_expr->attributes & ATTR_CONST_ADDR)) {
      UNCA(+, qmark->type, qmark->constant.integral.value,
           selected_expr->constant.integral.value);
    } else {
      qmark->constant = selected_expr->constant;
    }
    return astree_adopt(qmark, 3, condition, true_expr, false_expr);
  } else {
    maybe_load_cexpr(false_expr, NULL);
    maybe_load_cexpr(true_expr, false_expr->first_instr);
    maybe_load_cexpr(condition, true_expr->first_instr);
    return translate_conditional(qmark, condition, true_expr, false_expr);
  }
}

ASTree *evaluate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  if ((pointer->attributes & ATTR_CONST_ADDR) &&
      !(index->attributes & ATTR_CONST_ADDR) &&
      (index->attributes & ATTR_EXPR_CONST)) {
    subscript->attributes |= pointer->attributes & ATTR_MASK_CONST;
    subscript->constant.address.label = pointer->constant.address.label;
    subscript->constant.address.disp =
        pointer->constant.address.disp +
        (long)(index->constant.integral.value *
               typespec_get_width(subscript->type));
  } else if (!((pointer->attributes | index->attributes) & ATTR_CONST_ADDR) &&
             (pointer->attributes & index->attributes & ATTR_EXPR_CONST)) {
    subscript->attributes |= pointer->attributes & ATTR_MASK_CONST;
    subscript->constant.integral.value =
        (long)pointer->constant.integral.value +
        (long)(index->constant.integral.value *
               typespec_get_width(subscript->type));
  } else {
    maybe_load_cexpr(index, NULL);
    maybe_load_cexpr(pointer, index->first_instr);
    return translate_subscript(subscript, pointer, index);
  }
  return astree_adopt(subscript, 2, pointer, index);
}

ASTree *evaluate_addrof(ASTree *addrof, ASTree *operand) {
  if (operand->attributes & ATTR_EXPR_CONST) {
    addrof->attributes |= operand->attributes & ATTR_MASK_CONST;
    addrof->constant = operand->constant;
    return astree_adopt(addrof, 1, operand);
  } else {
    maybe_load_cexpr(operand, NULL);
    return translate_addrof(addrof, operand);
  }
}

ASTree *evaluate_reference(ASTree *reference, ASTree *struct_, ASTree *member) {
  if (struct_->attributes & ATTR_CONST_ADDR) {
    reference->attributes |= struct_->attributes & ATTR_MASK_CONST;
    SymbolValue *symval = typespec_member_name(struct_->type, member->lexinfo);
    assert(symval);
    reference->constant.address.label = struct_->constant.address.label;
    reference->constant.address.disp =
        struct_->constant.address.disp + (long)symval->disp;
    return astree_adopt(reference, 2, struct_, member);
  } else {
    /* scalars should not be castable to aggregates */
    assert(!(struct_->attributes & ATTR_EXPR_CONST) ||
           reference->symbol == TOK_ARROW);
    maybe_load_cexpr(struct_, NULL);
    return translate_reference(reference, struct_, member);
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
    /* treat like a cast */
    UNOP_CASE(TOK_POS, +, translate_cast);
    case '!':
      if ((operator->attributes & ATTR_EXPR_CONST) &&
          !(operator->attributes & ATTR_CONST_ADDR)) {
        operator->attributes |= operand->attributes & ATTR_MASK_CONST;
        /* cast to operand type, not operator type */
        UNCA(!, operand->type, operator->constant.integral.value,
             operand->constant.integral.value);
        return astree_adopt(operator, 1, operand);
      } else {
        maybe_load_cexpr(operand, NULL);
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
