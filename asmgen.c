#include "asmgen.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badalist.h"
#include "debug.h"
#include "lyutils.h"
#include "symtable.h"

#define MAX_INSTRUCTION_LENGTH 8
#define MAX_OP_LENGTH 16
#define MAX_LABEL_LENGTH 32
/* TODO(Robert):
 * write epilog function
 * generalize return, prolog, epilog to use vregs instead of explicit x64
 * registers
 * make function calls respect error codes
 */

/* macros used to generate string constants for instructions */
#define FOREACH_INSTRUCTION(GENERATOR) \
  /* arithmetic */                     \
  GENERATOR(ADD)                       \
  GENERATOR(SUB)                       \
  GENERATOR(MUL)                       \
  GENERATOR(DIV)                       \
  GENERATOR(INC)                       \
  GENERATOR(DEC)                       \
  GENERATOR(NEG)                       \
  GENERATOR(IMUL)                      \
  GENERATOR(IDIV)                      \
  /* compare and test */               \
  GENERATOR(TEST)                      \
  GENERATOR(CMP)                       \
  /* logical and bitwise */            \
  GENERATOR(NOT)                       \
  GENERATOR(OR)                        \
  GENERATOR(AND)                       \
  GENERATOR(LEA)                       \
  GENERATOR(XOR)                       \
  /* shifts */                         \
  GENERATOR(SHL)                       \
  GENERATOR(SHR)                       \
  GENERATOR(SAR)                       \
  /* code movement */                  \
  GENERATOR(MOV)                       \
  GENERATOR(MOVZX)                     \
  GENERATOR(MOVSX)                     \
  GENERATOR(PUSH)                      \
  GENERATOR(POP)                       \
  GENERATOR(CALL)                      \
  GENERATOR(LEAVE)                     \
  GENERATOR(RET)                       \
  GENERATOR(NOP)                       \
  /* data section definitions */       \
  GENERATOR(DB)                        \
  GENERATOR(DW)                        \
  GENERATOR(DD)                        \
  GENERATOR(DQ)                        \
  /* bss section definitions */        \
  GENERATOR(RESB)                      \
  GENERATOR(RESW)                      \
  GENERATOR(RESD)                      \
  GENERATOR(RESQ)

#define GENERATE_ENUM(ENUM) INSTR_##ENUM,
#define GENERATE_STRING(STRING) #STRING,

typedef struct vreg_entry {
  char *object_name;
  TypeSpec *object_type;
  size_t vreg_num;
  struct llist assigned_vregs;
} VregEntry;

typedef struct instruction_data {
  const char *instruction;
  char label[MAX_LABEL_LENGTH];
  char dest_operand[MAX_OP_LENGTH];
  char src_operand[MAX_OP_LENGTH];
} InstructionData;

typedef enum instruction_enum {
  FOREACH_INSTRUCTION(GENERATE_ENUM)
} InstructionEnum;

/* rbx, rbp, r12-r15 are preserved across function calls */
typedef enum instruction_flags {
  SOURCE_OPERAND = 1 << 0, /* result does not need to be in a register */
  DEST_OPERAND = 1 << 1,   /* result does need to be in a register */
  WANT_OBJ_VADDR = 1 << 2, /* result should be object address, not value */
  MOV_TO_MEM = 1 << 3,     /* parent instruction is MOV */
  RETURN_REG = 1 << 4,     /* result is to be place in a return register */
} InstructionFlags;

const char instructions[][MAX_INSTRUCTION_LENGTH] = {
    FOREACH_INSTRUCTION(GENERATE_STRING)};

/* Base and index are registers; scale is limited to {1, 2, 4, 8}, and offset
 * is a signed 32-bit integer. Having unnecessary offsets and scales shouldn't
 * affect the validity of the code.
 *
 * stars indicate that field width is an argument
 * TODO(Robert): make field width an argument
 */
static const char INDEX_FMT[] = "[%s+%s*%hhu+%i]";
static const char OFFSET_FMT[] = "[%s+%i]";
static const char VREG_FMT[] = "vr%zu%c";
static const char BINOP_FMT[] = "%8s%8s%8s,%s\n";
static const char UNOP_FMT[] = "%8s%8s%8s\n";
static const char NULLOP_FMT[] = "%8s%8s\n";
static const char LABEL_FMT[] = "%s\n";
static const char COND_FMT[] = ".C%u:\n";
static const char END_FMT[] = ".E%u:\n";
static const char LOOP_FMT[] = ".L%u:\n";
static const char SECTION_FMT[] = ".section %s\n";
static const char EMPTY_FMT[] = "";

/* not sure if field width is an (unsigned) int or a size_t */
static size_t branch_count = 0;
static size_t vreg_count = 0;
static size_t stack_window = 0;
static char current_label[MAX_LABEL_LENGTH];
static SymbolValue *current_function;

static LinkedList *text_section;
static LinkedList *data_section;
static LinkedList *bss_section;

/* the function that mallocs the InstructionData is also responsible
 * for pushing the InstructionData onto its respective stack
 *
 * if the function called to populate the InstructionData fails, it should not
 * be pushed onto the stack
 */
static char *translate_stride(ASTree *index, ASTree *memblock);
static char *translate_reg_type(ASTree *);
static char *translate_type(void *type, int flags);
static int translate_stmt(ASTree *stmt);
static int translate_block(ASTree *block);
static int translate_expr(ASTree *tree, InstructionData *data,
                          unsigned int flags);

/* TODO(Robert): turn into C if necessary
#define WRLABEL(LABEL,CODE) { \
            *out << setw(10) << left << LABEL << CODE << endl; \
        }
#define IDLABEL(LABEL,CODE) { \
            *out << setw(10) << *NO_LABEL << LABEL \
                 << CODE << endl; \
        }
#define INDENT(CODE) { \
            *out << setw(10) << *NO_LABEL << CODE << endl; \
        }
*/

/* TODO(Robert): it may be a good idea to generalize this so that an object's
 * location can be an offset from any register or label, rather than just the
 * stack pointer
 */
int assign_space(ASTree *tree) {
  SymbolValue *symval = NULL;
  locate_symbol((void *)tree->lexinfo, strlen(tree->lexinfo), &symval);
  if (symval == NULL) {
    fprintf(stderr, "ERROR: unable to resolve symbol %s\n", tree->lexinfo);
    return -1;
  }
  size_t required_padding =
      tree->type.alignment - (stack_window % tree->type.alignment);
  stack_window += required_padding;
  symval->stack_offset = stack_window;
  stack_window += tree->type.width;
  sprintf(symval->obj_loc, "rsp%+i", symval->stack_offset);
  return 0;
}

int assign_vreg(TypeSpec *type, InstructionData *data, const size_t vreg_num,
                unsigned int opflags) {
  char *dest =
      opflags & SOURCE_OPERAND ? data->src_operand : data->dest_operand;
  char reg_width = 0;

  if (opflags & WANT_OBJ_VADDR) {
    reg_width = 'q';
  } else {
    /* TODO(Robert): use fancy bit things to get rid of the switch statement */
    switch (type->width) {
      case X64_SIZEOF_LONG:
        reg_width = 'q';
        break;
      case X64_SIZEOF_INT:
        reg_width = 'd';
        break;
      case X64_SIZEOF_SHORT:
        reg_width = 'w';
        break;
      case X64_SIZEOF_CHAR:
        reg_width = 'b';
        break;
      default:
        fprintf(stderr, "ERROR: unable to assign vreg of width %zu\n",
                type->width);
        return -1;
        break;
    }
  }
  sprintf(dest, VREG_FMT, vreg_num, reg_width);
  return 0;
}

int resolve_object(ASTree *ident, InstructionData *out, unsigned int flags) {
  char *dest = flags & SOURCE_OPERAND ? out->src_operand : out->dest_operand;
  SymbolValue *symval = NULL;
  locate_symbol((void *)ident->lexinfo, strlen(ident->lexinfo), &symval);
  if (symval == NULL) {
    fprintf(stderr, "ERROR: unable to resolve symbol %s\n", ident->lexinfo);
    return -1;
  }

  if (flags & WANT_OBJ_VADDR) {
    sprintf(dest, "%s", symval->obj_loc);
  } else {
    sprintf(dest, "[%s]", symval->obj_loc);
  }
  return 0;
}

int translate_return(ASTree *ret, InstructionData *data) {
  DEBUGS('g', "Translating return statement");
  data->instruction = instructions[INSTR_RET];

  InstructionData *value_data = calloc(1, sizeof(*value_data));
  int status = translate_expr(astree_first(ret), value_data, SOURCE_OPERAND);
  if (status) return status;
  llist_push_back(text_section, value_data);

  InstructionData *mov_data = calloc(1, sizeof(*mov_data));
  mov_data->instruction = instructions[INSTR_MOV];
  strcpy(mov_data->src_operand, value_data->dest_operand);

  /* choose between rax, eax, ax, and al */
  /* TODO(Robert): have assign_vreg handle this */
  switch (current_function->type.nested->width) {
    case X64_SIZEOF_LONG:
      sprintf(mov_data->dest_operand, "rax");
      break;
    case X64_SIZEOF_INT:
      sprintf(mov_data->dest_operand, "eax");
      break;
    case X64_SIZEOF_SHORT:
      sprintf(mov_data->dest_operand, "ax");
      break;
    case X64_SIZEOF_CHAR:
      sprintf(mov_data->dest_operand, "al");
      break;
    default:
      fprintf(stderr, "ERROR: unable to assign vreg of width %zu\n",
              current_function->type.nested->width);
      return -1;
  }

  llist_push_back(text_section, mov_data);

  return 0;
}

int translate_ident(ASTree *ident, InstructionData *data, unsigned int flags) {
  if (flags & WANT_OBJ_VADDR) {
    data->instruction = instructions[INSTR_LEA];
    int status = resolve_object(ident, data, SOURCE_OPERAND & WANT_OBJ_VADDR);
    if (status) return status;
    status = assign_vreg(&(ident->type), data, vreg_count++,
                         DEST_OPERAND & WANT_OBJ_VADDR);
    if (status) return status;
  } else {
    data->instruction = instructions[INSTR_MOV];
    int status = resolve_object(ident, data, SOURCE_OPERAND);
    if (status) return status;
    status = assign_vreg(&(ident->type), data, vreg_count++, DEST_OPERAND);
    if (status) return status;
  }

  return 0;
}

int translate_conversion(ASTree *operator, InstructionData * data,
                         unsigned int flags) {
  /* NOTE: on x64, most operations that write to the lower 32 bits of a
   * register will zero the upper 32 bits. However, I will be zero- and
   * sign-extensions whenever the conversion requires it, just to be safe.
   */
  /* any signed int -> any wider unsigned int: movzx
   * any signed int -> any wider signed int: movsx
   * any unsigned int -> any wider int: movzx
   * any int -> any narrower int: simple mov
   * any int -> any int of same width: nop
   */
  DEBUGS('g', "Translating conversion");

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  ASTree *converted_expr = NULL;
  if (operator->children->size == 1)
    converted_expr = astree_first(operator);
  else
    converted_expr = astree_second(operator);
  int status = translate_expr(converted_expr, src_data, SOURCE_OPERAND);
  if (status) return status;
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);
  status = assign_vreg(&(operator->type), data, vreg_count++, DEST_OPERAND);
  if (status) return status;

  TypeSpec target_type = operator->type;
  TypeSpec source_type = converted_expr->type;

  if (source_type.width > target_type.width) {
    data->instruction = instructions[INSTR_MOV];
  }
  if (source_type.width == target_type.width) {
    data->instruction = instructions[INSTR_NOP];
  }
  if (source_type.base == TYPE_SIGNED) {
    if (target_type.base == TYPE_SIGNED) {
      data->instruction = instructions[INSTR_MOVSX];
    } else if (target_type.base == TYPE_UNSIGNED) {
      data->instruction = instructions[INSTR_MOVZX];
    }
  } else if (source_type.base == TYPE_UNSIGNED) {
    data->instruction = instructions[INSTR_MOVZX];
  } else {
    fprintf(stderr, "ERROR: unable to determine conversion\n");
    return -1;
  }

  return 0;
}

int translate_intcon(ASTree *constant, InstructionData *data,
                     unsigned int flags) {
  DEBUGS('g', "Translating integer constant");
  if (flags & DEST_OPERAND) {
    /* result will be destination register of parent operand; mov constant
     * into a register first
     */
    data->instruction = instructions[INSTR_MOV];
    int status =
        assign_vreg(&(constant->type), data, vreg_count++, DEST_OPERAND);
    if (status) return status;
    strcpy(data->src_operand, constant->lexinfo);
  } else {
    /* result does not need to be in a register */
    strcpy(data->dest_operand, constant->lexinfo);
  }
  return 0;
}

int translate_unop(ASTree *operator, InstructionData * data,
                   InstructionEnum num, unsigned int flags) {
  DEBUGS('g', "Translating unary operation: %s", instructions[num]);
  data->instruction = instructions[num];
  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  int status = translate_expr(astree_first(operator), dest_data, DEST_OPERAND);
  if (status) return status;
  llist_push_back(text_section, dest_data);
  strcpy(data->dest_operand, dest_data->dest_operand);
  return 0;
}

int translate_binop(ASTree *operator, InstructionData * data,
                    InstructionEnum num, unsigned int flags) {
  DEBUGS('g', "Translating binary operation: %s", instructions[num]);
  data->instruction = instructions[num];
  /* TODO(Robert): make sure that the order (left or right op first) is
   * correct
   */
  InstructionData *src_data = calloc(1, sizeof(*src_data));
  int status =
      translate_expr(astree_second(operator), src_data, SOURCE_OPERAND);
  if (status) return status;
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);

  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  status = translate_expr(astree_first(operator), dest_data, DEST_OPERAND);
  if (status) return status;
  llist_push_back(text_section, dest_data);
  strcpy(data->dest_operand, dest_data->dest_operand);

  return 0;
}

int translate_cast(ASTree *cast, InstructionData *data) {
  ASTree *casted_expr = NULL;
  if (cast->children->size == 1)
    casted_expr = astree_first(cast);
  else
    casted_expr = astree_second(cast);
  return 0;
}

int translate_assignment(ASTree *assignment, InstructionData *data,
                         unsigned int flags) {
  DEBUGS('g', "Translating assignment");
  data->instruction = instructions[INSTR_MOV];

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  int status = translate_expr(astree_second(assignment), src_data,
                              SOURCE_OPERAND & MOV_TO_MEM);
  if (status) return status;
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);

  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  status = translate_expr(astree_first(assignment), dest_data,
                          DEST_OPERAND & MOV_TO_MEM);
  if (status) return status;
  llist_push_back(text_section, dest_data);
  strcpy(data->dest_operand, dest_data->dest_operand);

  return 0;
}

int translate_param(ASTree *param, InstructionData *data) {
  DEBUGS('g', "Translating parameter");
  data->instruction = instructions[INSTR_MOV];
  ASTree *param_ident = astree_second(param);
  int status =
      assign_vreg(&(param_ident->type), data, vreg_count++, SOURCE_OPERAND);
  if (status) return status;
  status = assign_space(param_ident);
  if (status) return status;
  resolve_object(param_ident, data, DEST_OPERAND & MOV_TO_MEM);
  return 0;
}

int translate_local_decl(ASTree *type_id, InstructionData *data) {
  /* figure out how much stack space to allocate, assign address to be the
   * location for this object and, if the value is initialized upon
   * declaration, write out the instructions which assign the value
   */
  DEBUGS('g', "Translating local declaration");
  ASTree *ident = astree_second(type_id);
  int status = assign_space(ident);
  if (status) return status;

  if (type_id->children->size == 3) {
    data->instruction = instructions[INSTR_MOV];
    int status = resolve_object(ident, data, DEST_OPERAND & MOV_TO_MEM);
    if (status) return status;
    InstructionData *value_data = calloc(1, sizeof(*value_data));
    status = translate_expr(astree_third(type_id), value_data,
                            SOURCE_OPERAND & MOV_TO_MEM);
    if (status) return status;
    llist_push_back(text_section, value_data);
    strcpy(data->src_operand, value_data->dest_operand);
  }
  return 0;
}

int translate_global_decl(ASTree *type_id, InstructionData *data) {
  DEBUGS('g', "Translating global declaration");
  ASTree *ident = astree_second(type_id);
  sprintf(data->label, "%s:", ident->lexinfo);
  /* TODO(Robert): indicate somehow in the tree or symbol table that this
   * is a global variable and should be referenced by its name, as
   * opposed to a stack offset
   */
  /* TODO(Robert): figure out how to initialize data for a struct or any
   * type wider than a quadword
   */
  if (type_id->children->size == 3) {
    /* put in data section */
    /* TODO(Robert): have the compiler evaluate compile-time constants
     */
    ASTree *init_value = astree_third(type_id);
    strcpy(data->dest_operand, "COMPILE-TIME CONSTANT");

    switch (ident->type.width) {
      case X64_SIZEOF_LONG:
        data->instruction = instructions[INSTR_DQ];
        break;
      case X64_SIZEOF_INT:
        data->instruction = instructions[INSTR_DD];
        break;
      case X64_SIZEOF_SHORT:
        data->instruction = instructions[INSTR_DW];
        break;
      case X64_SIZEOF_CHAR:
        data->instruction = instructions[INSTR_DB];
        break;
      default:
        fprintf(stderr,
                "ERROR: unable to determine instruction for initialized"
                " data of width %zu\n",
                ident->type.width);
        return -1;
        break;
    }
  } else {
    /* put in bss/uninitialized data section */
    data->dest_operand[0] = '1';
    data->dest_operand[1] = 0;
    switch (ident->type.width) {
      case X64_SIZEOF_LONG:
        data->instruction = instructions[INSTR_RESQ];
        break;
      case X64_SIZEOF_INT:
        data->instruction = instructions[INSTR_RESD];
        break;
      case X64_SIZEOF_SHORT:
        data->instruction = instructions[INSTR_RESW];
        break;
      case X64_SIZEOF_CHAR:
        data->instruction = instructions[INSTR_RESB];
        break;
      default:
        fprintf(stderr,
                "ERROR: unable to determine instruction for uninitialized"
                " data of width %zu\n",
                ident->type.width);
        return -1;
        break;
    }
  }
  return 0;
}

static int translate_expr(ASTree *tree, InstructionData *out,
                          unsigned int flags) {
  int status = 0;

  switch (tree->symbol) {
    case '+':
      status = translate_binop(tree, out, INSTR_ADD, flags);
      break;
    case '-':
      status = translate_binop(tree, out, INSTR_SUB, flags);
      break;
    case '*':
      status = translate_binop(tree, out, INSTR_MUL, flags);
      break;
    case '/':
      status = translate_binop(tree, out, INSTR_DIV, flags);
      break;
    case TOK_IDENT:
      status = translate_ident(tree, out, flags);
      break;
    case '=':
      status = translate_assignment(tree, out, flags);
      break;
    case TOK_CAST:
      status = translate_conversion(tree, out, flags);
      break;
    case TOK_INTCON:
      status = translate_intcon(tree, out, flags);
      break;
    case TOK_CHARCON:
      break;
    default:
      fprintf(stderr, "ERROR: Unimplemented token: %s, lexinfo: %s\n",
              parser_get_tname(tree->symbol), tree->lexinfo);
      status = -1;
      break;
  }
  return status;
}

static int translate_stmt(ASTree *stmt) {
  InstructionData *data;
  int status = 0;

  if (stmt->symbol_table) enter_scope(stmt->symbol_table);
  switch (stmt->symbol) {
    case TOK_BLOCK:
      status = translate_block(stmt);
      break;
    case TOK_RETURN:
      data = calloc(1, sizeof(*data));
      status = translate_return(stmt, data);
      if (status) break;
      llist_push_back(text_section, data);
      break;
    case TOK_WHILE:
      fprintf(stderr, "ERROR: while constructs not implemented\n");
      status = -1;
      break;
    /*
    case TOK_DO:
      fprintf(stderr, "ERROR: do-while constructs not implemented\n");
      status = -1;
        break;
    */
    case TOK_IF:
      fprintf(stderr, "ERROR: if-else constructs not implemented\n");
      status = -1;
      break;
    case TOK_TYPE_ID:
      data = calloc(1, sizeof(*data));
      status = translate_local_decl(stmt, data);
      if (status) break;
      llist_push_back(text_section, data);
      break;
    default:
      data = calloc(1, sizeof(*data));
      status = translate_expr(stmt, data, 0);
      if (status) break;
      llist_push_back(text_section, data);
      break;
  }

  if (stmt->symbol_table) leave_scope();
  return status;
}

static int translate_block(ASTree *block) {
  DEBUGS('g', "Translating compound statement");
  LinkedList *stmts = block->children;
  size_t i;
  for (i = 0; i < stmts->size; ++i) {
    ASTree *stmt = llist_get(stmts, i);
    int status = translate_stmt(stmt);
    if (status) return status;
  }
  return 0;
}

/* TODO(Robert): consider writing a more generic form of the function prolog,
 * by reserving N saved registers (including general purpose ones and stack,
 * base, and frame pointers) and handling those before anything else.
 */
int translate_prolog(ASTree *function) {
  InstructionData *push_rbp_data = calloc(1, sizeof(InstructionData));
  push_rbp_data->instruction = instructions[INSTR_PUSH];
  sprintf(push_rbp_data->src_operand, "rbp");
  llist_push_back(text_section, push_rbp_data);

  InstructionData *mov_rbp_data = calloc(1, sizeof(InstructionData));
  mov_rbp_data->instruction = instructions[INSTR_MOV];
  sprintf(mov_rbp_data->src_operand, "rsp");
  sprintf(mov_rbp_data->dest_operand, "rbp");
  llist_push_back(text_section, mov_rbp_data);

  InstructionData *push_r12_data = calloc(1, sizeof(InstructionData));
  push_r12_data->instruction = instructions[INSTR_PUSH];
  sprintf(push_r12_data->src_operand, "r12");
  llist_push_back(text_section, push_r12_data);

  InstructionData *push_r13_data = calloc(1, sizeof(InstructionData));
  push_r13_data->instruction = instructions[INSTR_PUSH];
  sprintf(push_r13_data->src_operand, "r13");
  llist_push_back(text_section, push_r13_data);

  InstructionData *push_r14_data = calloc(1, sizeof(InstructionData));
  push_r14_data->instruction = instructions[INSTR_PUSH];
  sprintf(push_r14_data->src_operand, "r14");
  llist_push_back(text_section, push_r14_data);

  InstructionData *push_r15_data = calloc(1, sizeof(InstructionData));
  push_r15_data->instruction = instructions[INSTR_PUSH];
  sprintf(push_r15_data->src_operand, "r15");
  llist_push_back(text_section, push_r15_data);

  return 0;
}

int translate_epilog(ASTree *function) {
  InstructionData *pop_r15_data = calloc(1, sizeof(InstructionData));
  pop_r15_data->instruction = instructions[INSTR_POP];
  sprintf(pop_r15_data->src_operand, "r15");
  llist_push_back(text_section, pop_r15_data);

  InstructionData *pop_r14_data = calloc(1, sizeof(InstructionData));
  pop_r14_data->instruction = instructions[INSTR_POP];
  sprintf(pop_r14_data->src_operand, "r14");
  llist_push_back(text_section, pop_r14_data);

  InstructionData *pop_r13_data = calloc(1, sizeof(InstructionData));
  pop_r13_data->instruction = instructions[INSTR_POP];
  sprintf(pop_r13_data->src_operand, "r13");
  llist_push_back(text_section, pop_r13_data);

  InstructionData *pop_r12_data = calloc(1, sizeof(InstructionData));
  pop_r12_data->instruction = instructions[INSTR_POP];
  sprintf(pop_r12_data->src_operand, "r12");
  llist_push_back(text_section, pop_r12_data);

  InstructionData *mov_rsp_data = calloc(1, sizeof(InstructionData));
  mov_rsp_data->instruction = instructions[INSTR_MOV];
  sprintf(mov_rsp_data->src_operand, "rbp");
  sprintf(mov_rsp_data->dest_operand, "rsp");
  llist_push_back(text_section, mov_rsp_data);

  InstructionData *push_rbp_data = calloc(1, sizeof(InstructionData));
  push_rbp_data->instruction = instructions[INSTR_POP];
  sprintf(push_rbp_data->src_operand, "rbp");
  llist_push_back(text_section, push_rbp_data);

  return 0;
}

int translate_function(ASTree *function, InstructionData *data) {
  DEBUGS('g', "Translating function definition");
  ASTree *name_node = astree_second(astree_first(function));
  locate_symbol(name_node->lexinfo, strlen(name_node->lexinfo),
                &current_function);
  if (current_function == NULL) {
    fprintf(stderr, "ERROR: unable to resolve symbol %s\n", name_node->lexinfo);
    return -1;
  }
  sprintf(data->label, "%s:", name_node->lexinfo);
  translate_prolog(function);

  /* enter function parameter/body scope briefly to handle parameters, then exit
   * again
   */
  size_t i;
  ASTree *params = astree_second(function);
  enter_scope(params->symbol_table);
  for (i = 0; i < params->children->size; ++i) {
    ASTree *param = llist_get(params->children, i);
    InstructionData *param_data = calloc(1, sizeof(*param_data));
    int status = translate_param(param, param_data);
    if (status) return status;
    llist_push_back(text_section, param_data);
  }
  leave_scope();

  int status = translate_stmt(astree_third(function));
  if (status) return status;
  translate_epilog(function);
  return 0;
}

int translate_file(ASTree *root) {
  size_t i;
  enter_scope(root->symbol_table);
  for (i = 0; i < llist_size(root->children); ++i) {
    ASTree *topdecl = llist_get(root->children, i);
    InstructionData *topdecl_data = calloc(1, sizeof(*topdecl_data));
    int status = 0;
    switch (topdecl->symbol) {
      case TOK_TYPE_ID:
        status = translate_global_decl(topdecl, topdecl_data);
        llist_push_back(data_section, topdecl_data);
        break;
      case TOK_FUNCTION:
        /* do nothing if this is just a prototype */
        if (topdecl->children->size > 2) {
          /* data will hold the function label, which should appear before
           * the function body
           */
          llist_push_back(text_section, topdecl_data);
          status = translate_function(topdecl, topdecl_data);
        }
        break;
      /*
      case TOK_STRUCT:
      case TOK_UNION:
      case TOK_TYPEDEF:
      */
      default:
        fprintf(stderr, "ERROR: unrecognized symbol at top level\n");
        status = -1;
    }

    if (status) return status;
  }

  leave_scope();
  return 0;
}

int write_instruction(InstructionData *data, FILE *out) {
  /* all fields except for the instruction are character arrays, not pointers,
   * which are always legal to pass to printf and co., so we only need to
   * check to see whether or not the instruction is NULL before printing
   */
  /* we want to defer as much of the formatting as possible until we get to this
   * point so that we have as much information as possible to align fields in
   * the generated assembly
   */

  if (data->instruction == NULL && strlen(data->label) > 0) {
    /* only a label */
    fprintf(out, LABEL_FMT, data->label);
  } else if (data->instruction != NULL) {
    /* instruction, possible with a label */
    int instruction_type =
        (!!data->dest_operand[0]) | (!!data->src_operand[0] << 1);
    switch (instruction_type) {
      case 0:
        /* nullary */
        fprintf(out, NULLOP_FMT, data->label, data->instruction);
        break;
      case 1:
        /* unary, destination */
        fprintf(out, UNOP_FMT, data->label, data->instruction,
                data->dest_operand);
        break;
      case 2:
        /* unary, source */
        fprintf(out, UNOP_FMT, data->label, data->instruction,
                data->src_operand);
        break;
      case 3:
        /* binary operation */
        fprintf(out, BINOP_FMT, data->label, data->instruction,
                data->dest_operand, data->src_operand);
        break;
      default:
        break;
    }
  }

  return 0;
}

int write_text_section(FILE *out) {
  size_t i;
  for (i = 0; i < text_section->size; ++i) {
    write_instruction(llist_get(text_section, i), out);
  }
  return 0;
}

int write_data_section(FILE *out) {
  size_t i;
  for (i = 0; i < data_section->size; ++i) {
    write_instruction(llist_get(data_section, i), out);
  }
  return 0;
}

int write_bss_section(FILE *out) {
  size_t i;
  for (i = 0; i < bss_section->size; ++i) {
    write_instruction(llist_get(bss_section, i), out);
  }
  return 0;
}

int write_asm(FILE *out) {
  /* TODO(Robert): write assembler directives */
  write_text_section(out);
  write_data_section(out);
  write_bss_section(out);
  return 0;
}

void asmgen_init_globals() {
  text_section = malloc(sizeof(*text_section));
  llist_init(text_section, free, NULL);
  data_section = malloc(sizeof(*data_section));
  llist_init(data_section, free, NULL);
  bss_section = malloc(sizeof(*bss_section));
  llist_init(bss_section, free, NULL);
  memset(current_label, 0, MAX_LABEL_LENGTH);
}

void asmgen_free_globals() {
  llist_destroy(text_section);
  llist_destroy(data_section);
  llist_destroy(bss_section);
}
