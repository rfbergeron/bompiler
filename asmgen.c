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
} InstructionFlags;

const char instructions[][MAX_INSTRUCTION_LENGTH] = {
    FOREACH_INSTRUCTION(GENERATE_STRING)};

/* Base and index are registers; scale is limited to {1, 2, 4, 8}, and offset
 * is a signed 32-bit integer. Having unnecessary offsets and scales shouldn't
 * affect the validity of the code.
 *
 * stars indicate that field width is an argument
 */
static const char INDEX_FMT[] = "[%s+%s*%hhu+%i]";
static const char OFFSET_FMT[] = "[%s+%i]";
static const char SRC_LINE_FMT[] = "%*s:%*s %*s %s\n";
static const char VREG_FMT[] = "vr%zu%c";
static const char UNOP_FMT[] = "%s %s";
static const char BINOP_FMT[] = "%s %s,%s";
static const char LABEL_FMT[] = "%s: ";
static const char COND_FMT[] = ".C%u\n";
static const char END_FMT[] = ".E%u\n";
static const char LOOP_FMT[] = ".L%u\n";
static const char SECTION_FMT[] = ".section %s\n";
static const char FUNCTION_PROLOG[] =
    "PUSH rbp\n"
    "MOV rbp, rsp\n"
    "PUSH r12\n"
    "PUSH r13\n"
    "PUSH r14\n"
    "PUSH r15\n";
static const char FUNCTION_EPILOG[] =
    "POP r15\n"
    "POP r14\n"
    "POP r13\n"
    "POP r12\n"
    "MOV rsp, rbp\n"
    "POP rbp\n"
    "RET\n\n";

/* not sure if field width is an (unsigned) int or a size_t */
static size_t branch_count = 0;
static size_t vreg_count = 0;
static size_t stack_window = 0;
static char current_label[MAX_LABEL_LENGTH];
static SymbolValue *current_function;
static Map *current_symtable;

static LinkedList *text_section;
static LinkedList *data_section;
static LinkedList *bss_section;

extern FILE *oilfile;

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
  SymbolValue *symval =
      map_get(current_symtable, (void *)tree->lexinfo, strlen(tree->lexinfo));
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
    }
  }
  sprintf(dest, VREG_FMT, vreg_num, reg_width);
  return 0;
}

int resolve_object(ASTree *ident, InstructionData *out, unsigned int flags) {
  char *dest = flags & SOURCE_OPERAND ? out->src_operand : out->dest_operand;
  SymbolValue *symval =
      map_get(current_symtable, (void *)ident->lexinfo, strlen(ident->lexinfo));
  if (flags & WANT_OBJ_VADDR) {
    sprintf(dest, "%s", symval->obj_loc);
  } else {
    sprintf(dest, "[%s]", symval->obj_loc);
  }
  return 0;
}

int translate_return(ASTree *ret, InstructionData *data) {
  data->instruction = instructions[INSTR_RET];

  InstructionData *value_data = malloc(sizeof(*value_data));
  translate_expr(astree_first(ret), value_data, SOURCE_OPERAND);
  llist_push_back(text_section, value_data);

  InstructionData *mov_data = malloc(sizeof(*mov_data));
  mov_data->instruction = instructions[INSTR_MOV];
  strcpy(mov_data->src_operand, value_data->dest_operand);

  /* choose between rax, eax, ax, and al */
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
  }

  llist_push_back(text_section, mov_data);

  return 0;
}

int translate_ident(ASTree *ident, InstructionData *data, unsigned int flags) {
  if (flags & WANT_OBJ_VADDR) {
    data->instruction = instructions[INSTR_LEA];
    resolve_object(ident, data, SOURCE_OPERAND & WANT_OBJ_VADDR);
    assign_vreg(&(ident->type), data, vreg_count++,
                DEST_OPERAND & WANT_OBJ_VADDR);
  } else {
    data->instruction = instructions[INSTR_MOV];
    resolve_object(ident, data, SOURCE_OPERAND);
    assign_vreg(&(ident->type), data, vreg_count++, DEST_OPERAND);
  }

  return 0;
}

int translate_conversion(ASTree *operator, InstructionData * data) {
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

  InstructionData *src_data = malloc(sizeof(*src_data));
  translate_expr(astree_first(operator), src_data, SOURCE_OPERAND);
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);
  assign_vreg(&(operator->type), data, vreg_count++, DEST_OPERAND);

  TypeSpec target_type = operator->type;
  TypeSpec source_type = astree_first(operator)->type;

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
    return -1;
  }

  return 0;
}

int translate_unop(ASTree *operator, InstructionData * data,
                   InstructionEnum num, unsigned int flags) {
  data->instruction = instructions[num];
  InstructionData *dest_data = malloc(sizeof(*dest_data));
  translate_expr(astree_first(operator), dest_data, DEST_OPERAND);
  llist_push_back(text_section, dest_data);
  strcpy(data->dest_operand, dest_data->dest_operand);
  return 0;
}

int translate_binop(ASTree *operator, InstructionData * data,
                    InstructionEnum num, unsigned int flags) {
  data->instruction = instructions[num];
  /* TODO(Robert): make sure that the order (left or right op first) is
   * correct
   */
  InstructionData *src_data = malloc(sizeof(*src_data));
  translate_expr(astree_second(operator), src_data, SOURCE_OPERAND);
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);

  InstructionData *dest_data = malloc(sizeof(*dest_data));
  translate_expr(astree_first(operator), dest_data, DEST_OPERAND);
  llist_push_back(text_section, dest_data);
  strcpy(data->dest_operand, dest_data->dest_operand);

  return 0;
}

int translate_assignment(ASTree *assignment, InstructionData *data,
                         unsigned int flags) {
  data->instruction = instructions[INSTR_MOV];

  InstructionData *src_data = malloc(sizeof(*src_data));
  translate_expr(astree_second(assignment), src_data,
                 SOURCE_OPERAND & MOV_TO_MEM);
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);

  InstructionData *dest_data = malloc(sizeof(*dest_data));
  translate_expr(astree_first(assignment), dest_data,
                 DEST_OPERAND & MOV_TO_MEM);
  llist_push_back(text_section, dest_data);
  strcpy(data->dest_operand, dest_data->dest_operand);

  return 0;
}

int translate_param(ASTree *param, InstructionData *data) {
  data->instruction = instructions[INSTR_MOV];
  assign_vreg(&(param->type), data, vreg_count++, SOURCE_OPERAND);
  assign_space(param);
  resolve_object(param, data, DEST_OPERAND & MOV_TO_MEM);
  return 0;
}

int translate_local_decl(ASTree *type_id, InstructionData *data) {
  /* figure out how much stack space to allocate, assign address to be the
   * location for this object and, if the value is initialized upon
   * declaration, write out the instructions which assign the value
   */
  ASTree *ident = astree_second(type_id);
  assign_space(ident);

  if (type_id->children->size == 3) {
    data->instruction = instructions[INSTR_MOV];
    resolve_object(ident, data, DEST_OPERAND & MOV_TO_MEM);
    InstructionData *value_data = malloc(sizeof(*value_data));
    translate_expr(astree_third(type_id), value_data,
                   SOURCE_OPERAND & MOV_TO_MEM);
    llist_push_back(text_section, value_data);
    strcpy(data->src_operand, value_data->dest_operand);
  }
  return 0;
}

int translate_global_decl(ASTree *type_id, InstructionData *data) {
  ASTree *ident = astree_second(type_id);
  strcpy(data->label, ident->lexinfo);
  /* TODO(Robert): indicate somehow in the tree or symbol table that this
   * is a global variable and should be referenced by its name, as
   * opposed to a stack offset
   */
  /* TODO(Robert): figure out how to initialize data for a struct or any
   * type wider than a quadword
   */
  if (type_id->children->size == 3) {
    /* put in data section */
    /* TODO(Robert): this is very unsafe and the complier also needs to
     * optimize away constant expressions so they can be used to initialize
     * global variables. None of the expressions in the subtree should
     * resolve to assembly instructions, since they should collapse down to
     * a single constant that can be evaluated at compile time.
     */
    ASTree *init_value = astree_third(type_id);
    strcpy(data->dest_operand, init_value->lexinfo);

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
    }
  }
  return 0;
}

static int translate_expr(ASTree *tree, InstructionData *out,
                          unsigned int flags) {
  int status = 0;

  switch (tree->symbol) {
    case '+':
      translate_binop(tree, out, INSTR_ADD, flags);
      break;
    case '-':
      translate_binop(tree, out, INSTR_SUB, flags);
      break;
    case '*':
      translate_binop(tree, out, INSTR_MUL, flags);
      break;
    case '/':
      translate_binop(tree, out, INSTR_DIV, flags);
      break;
    case TOK_IDENT:
      translate_ident(tree, out, flags);
      break;
    case '=':
      translate_assignment(tree, out, flags);
      break;
    default:
      break;
  }
  return 0;
}

static int translate_stmt(ASTree *stmt) {
  InstructionData *data;

  if (stmt->symbol_table) enter_scope(stmt->symbol_table);
  switch (stmt->symbol) {
    case TOK_BLOCK:
      translate_block(stmt);
      break;
    case TOK_RETURN:
      data = malloc(sizeof(*data));
      translate_return(stmt, data);
      llist_push_back(text_section, data);
      break;
    case TOK_WHILE:
      break;
    case TOK_IF:
      break;
    case TOK_TYPE_ID:
      data = malloc(sizeof(*data));
      translate_local_decl(stmt, data);
      llist_push_back(text_section, data);
      break;
    default:
      data = malloc(sizeof(*data));
      translate_expr(stmt, data, 0);
      llist_push_back(text_section, data);
      break;
  }

  if (stmt->symbol_table) leave_scope();
  return 0;
}

static int translate_block(ASTree *block) {
  LinkedList *stmts = block->children;
  size_t i;
  for (i = 0; i < stmts->size; ++i) {
    ASTree *stmt = llist_get(stmts, i);
    translate_stmt(stmt);
  }
  return 0;
}

int translate_function_decl(ASTree *function, InstructionData *data) {
  /* do nothing if this is just a prototype */
  if (function->children->size < 3) return 0;
  /* write function header (type, name, args, etc) */
  ASTree *name_node = astree_second(astree_first(function));
  /* assign function label */
  strcpy(data->label, name_node->lexinfo);
  /* TODO: write function prolog - push base pointer and all preserved registers
   * onto the stack
   */
  size_t i;
  LinkedList *params = astree_second(function)->children;
  for (i = 0; i < params->size; ++i) {
    ASTree *param = llist_get(params, i);
    InstructionData *param_data = malloc(sizeof(*param_data));
    translate_param(param, param_data);
    llist_push_back(text_section, param_data);
  }

  translate_stmt(astree_third(function));
  /* TODO: write function epilog */
  return 0;
}

int translate_file(ASTree *root) {
  size_t i;
  enter_scope(root->symbol_table);
  for (i = 0; i < llist_size(root->children); ++i) {
    ASTree *topdecl = llist_get(root->children, i);
    InstructionData *topdecl_data = malloc(sizeof(*topdecl_data));
    switch (topdecl->symbol) {
      case TOK_TYPE_ID:
        translate_global_decl(topdecl, topdecl_data);
        llist_push_back(data_section, topdecl_data);
        break;
      case TOK_FUNCTION:
        /* data will hold the function label, which should appear before
         * the function body
         */
        llist_push_back(text_section, topdecl_data);
        translate_function_decl(topdecl, topdecl_data);
        break;
      /*
      case TOK_STRUCT:
      case TOK_UNION:
      case TOK_TYPEDEF:
      */
      default:
        fprintf(stderr, "ERROR: unrecognized symbol at top level\n");
        return -1;
    }
  }

  leave_scope();
  return 0;
}

int write_text_section() { return 0; }

int write_data_section(struct llist *global_var_stack) { return 0; }

int write_bss_section(struct llist *uninitialized_data_stack) { return 0; }
