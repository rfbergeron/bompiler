#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
/* get rid of inline in cmocka */
#define inline
#include <cmocka.h>
#include <stdlib.h>
#include "astree.h"
#include "symtable.h"
#include "state.h"
#include "yyparse.h"

/* duplicate globals... regret... */
int skip_type_check = 0;
const TypeSpec SPEC_EMPTY = {0, 0, BLIB_LLIST_EMPTY, TYPESPEC_FLAG_NONE,
                             TYPE_NONE};
const size_t MAX_IDENT_LEN = 31;
CompilerState *state = NULL;

LabelValue *__wrap_state_get_label(CompilerState *state, const char *ident,
        const size_t ident_len) {
  check_expected_ptr(ident);
  return mock_ptr_type(LabelValue *);
}

int __wrap_typespec_destroy(TypeSpec *spec) {
  return mock_type(int);
}

int __wrap_typespec_init(TypeSpec *spec) {
  return mock_type(int);
}

void process_control_bad_symbol(void **state) {
  SymbolTable *table = symbol_table_init();
  expect_assert_failure(symbol_table_process_control(table, TOK_CALL));
  symbol_table_destroy(table);
}

void process_control_none(void **state) {
  SymbolTable *table = symbol_table_init();
  symbol_table_process_control(table, TOK_FOR);
  symbol_table_destroy(table);
}

void process_control_bad_label(void **state) {
  SymbolTable *table = symbol_table_init();

  ControlValue *good_ctrl = test_malloc(sizeof(ControlValue));
  ASTree good_node = EMPTY_EXPR_VALUE;
  good_node.lexinfo = "good";
  good_ctrl->tree = &good_node;
  good_ctrl->type = CTRL_GOTO;
  symbol_table_add_control(table, good_ctrl);

  /* Unfortunately, this memory will be freed using a function pointer called
   * by badlib. The _test_free function cannot be used in its place since it
   * has too many parameters.
   *
   * It is possible, but it will require changes to badlib.
   */
  ControlValue *bad_ctrl = malloc(sizeof(ControlValue));
  ASTree bad_node = EMPTY_EXPR_VALUE;
  bad_node.lexinfo = "bad";
  bad_ctrl->tree = &bad_node;
  bad_ctrl->type = CTRL_GOTO;
  symbol_table_add_control(table, bad_ctrl);

  expect_string(__wrap_state_get_label, ident, "bad");
  will_return(__wrap_state_get_label, NULL);
  expect_string(__wrap_state_get_label, ident, "good");
  LabelValue good_labval = {0};
  will_return(__wrap_state_get_label, &good_labval);

  symbol_table_process_control(table, TOK_DECLARATION);
  assert_int_equal(symbol_table_count_control(table), 1);
  symbol_table_destroy(table);
}

void process_control_switch(void **state) {
  SymbolTable *table = symbol_table_init();

  ControlValue *goto_ctrl = malloc(sizeof(ControlValue));
  goto_ctrl->type = CTRL_GOTO;
  symbol_table_add_control(table, goto_ctrl);

  ControlValue *break_ctrl = test_malloc(sizeof(ControlValue));
  break_ctrl->type = CTRL_BREAK;
  symbol_table_add_control(table, break_ctrl);

  ControlValue *continue_ctrl = malloc(sizeof(ControlValue));
  continue_ctrl->type = CTRL_CONTINUE;
  symbol_table_add_control(table, continue_ctrl);

  ControlValue *default_ctrl = test_malloc(sizeof(ControlValue));
  default_ctrl->type = CTRL_DEFAULT;
  symbol_table_add_control(table, default_ctrl);

  ControlValue *case_ctrl = test_malloc(sizeof(ControlValue));
  case_ctrl->type = CTRL_CASE;
  symbol_table_add_control(table, case_ctrl);

  symbol_table_process_control(table, TOK_SWITCH);
  assert_int_equal(symbol_table_count_control(table), 2);
  symbol_table_destroy(table);
}

void process_control_while(void **state) {
  SymbolTable *table = symbol_table_init();

  ControlValue *goto_ctrl = malloc(sizeof(ControlValue));
  goto_ctrl->type = CTRL_GOTO;
  symbol_table_add_control(table, goto_ctrl);

  ControlValue *break_ctrl = test_malloc(sizeof(ControlValue));
  break_ctrl->type = CTRL_BREAK;
  symbol_table_add_control(table, break_ctrl);

  ControlValue *continue_ctrl = test_malloc(sizeof(ControlValue));
  continue_ctrl->type = CTRL_CONTINUE;
  symbol_table_add_control(table, continue_ctrl);

  ControlValue *default_ctrl = malloc(sizeof(ControlValue));
  default_ctrl->type = CTRL_DEFAULT;
  symbol_table_add_control(table, default_ctrl);

  ControlValue *case_ctrl = malloc(sizeof(ControlValue));
  case_ctrl->type = CTRL_CASE;
  symbol_table_add_control(table, case_ctrl);

  symbol_table_process_control(table, TOK_WHILE);
  assert_int_equal(symbol_table_count_control(table), 3);
  symbol_table_destroy(table);
}

void process_control_function(void **state) {
  SymbolTable *table = symbol_table_init();

  ControlValue *goto_ctrl = test_malloc(sizeof(ControlValue));
  ASTree goto_node = EMPTY_EXPR_VALUE;
  goto_node.lexinfo = "goto";
  goto_ctrl->tree = &goto_node;
  goto_ctrl->type = CTRL_GOTO;
  symbol_table_add_control(table, goto_ctrl);

  ControlValue *break_ctrl = malloc(sizeof(ControlValue));
  break_ctrl->type = CTRL_BREAK;
  symbol_table_add_control(table, break_ctrl);

  ControlValue *continue_ctrl = malloc(sizeof(ControlValue));
  continue_ctrl->type = CTRL_CONTINUE;
  symbol_table_add_control(table, continue_ctrl);

  ControlValue *default_ctrl = malloc(sizeof(ControlValue));
  default_ctrl->type = CTRL_DEFAULT;
  symbol_table_add_control(table, default_ctrl);

  ControlValue *case_ctrl = malloc(sizeof(ControlValue));
  case_ctrl->type = CTRL_CASE;
  symbol_table_add_control(table, case_ctrl);

  expect_string(__wrap_state_get_label, ident, "goto");
  LabelValue goto_labval = {0};
  will_return(__wrap_state_get_label, &goto_labval);

  symbol_table_process_control(table, TOK_DECLARATION);
  assert_int_equal(symbol_table_count_control(table), 4);
  symbol_table_destroy(table);
}

int main(int argc, char **argv) {
    struct CMUnitTest tests[] = {
      cmocka_unit_test(process_control_bad_symbol),
      cmocka_unit_test(process_control_none),
      cmocka_unit_test(process_control_bad_label),
      cmocka_unit_test(process_control_switch),
      cmocka_unit_test(process_control_while),
      cmocka_unit_test(process_control_function)
    };

    return cmocka_run_group_tests(tests, NULL, NULL);
}
