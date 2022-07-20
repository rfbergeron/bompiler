#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
/* get rid of inlinie in cmocka */
#define inline
#include <cmocka.h>
#include <stdlib.h>

#include "astree.h"
#include "yyparse.h"

/* fake global... regret... */
int skip_type_check = 0;
/* duplicate global... regret... */
const TypeSpec SPEC_EMPTY = {0, 0, BLIB_LLIST_EMPTY, TYPESPEC_FLAG_NONE,
                             TYPE_NONE};

/* badllist mocks */
#include "wrap_badllist.c"

const char *__wrap_string_set_intern(const char *string) {
  return mock_ptr_type(const char *);
}

const char *__wrap_parser_get_tname(int symbol) {
  return mock_ptr_type(const char *);
}

int __wrap_symbol_table_destroy(SymbolTable *table) { return mock_type(int); }

int __wrap_auxspec_destroy(AuxSpec *aux) {
  check_expected_ptr(aux);
  return mock_type(int);
}

int __wrap_typespec_destroy(TypeSpec *spec) { return mock_type(int); }

void destroy_null(void **state) {
  int status = astree_destroy(NULL);
  assert_true(status);
}

void destroy_empty(void **state) {
  ASTree *tree = &EMPTY_EXPR;
  int status = astree_destroy(tree);
  assert_false(status);
  assert_ptr_equal(tree, &EMPTY_EXPR);
  assert_memory_equal(tree, &EMPTY_EXPR, sizeof(ASTree));
}

void destroy_addrof(void **state) {
  ASTree *addrof = test_malloc(sizeof(ASTree));
  *addrof = (ASTree)EMPTY_EXPR_VALUE;
  addrof->symbol = TOK_ADDROF;
  TypeSpec *addrof_spec = test_malloc(sizeof(TypeSpec));
  addrof->type = addrof_spec;
  AuxSpec expected_ptr_aux = {0};

  will_return_always(__wrap_typespec_destroy, 0);
  will_return_always(__wrap_auxspec_destroy, 0);
  will_return_always(__wrap_llist_destroy, 0);
  will_return(__wrap_llist_pop_front, &expected_ptr_aux);
  expect_value(__wrap_auxspec_destroy, aux, &expected_ptr_aux);

  int status = astree_destroy(addrof);
  assert_false(status);
}

void adopt_too_many(void **state) {
  ASTree errnode = EMPTY_EXPR_VALUE;
  errnode.symbol = TOK_TYPE_ERROR;
  ASTree child = EMPTY_EXPR_VALUE;

  will_return(__wrap_llist_size, 1);
  expect_assert_failure(astree_adopt(&errnode, 1, &child));
}

void adopt_again(void **state) {
  ASTree parent = EMPTY_EXPR_VALUE;
  ASTree child = EMPTY_EXPR_VALUE;

  will_return(__wrap_llist_find, 1);
  expect_assert_failure(astree_adopt(&parent, 1, &child));
}

void adopt_empty_again(void **state) {
  ASTree parent = EMPTY_EXPR_VALUE;
  will_return(__wrap_llist_push_back, 0);
  astree_adopt(&parent, 1, &EMPTY_EXPR);
}

void adopt_null_child(void **state) {
  ASTree parent = EMPTY_EXPR_VALUE;
  expect_assert_failure(astree_adopt(&parent, 1, NULL));
}

void replace_out_of_bounds(void **state) {
  ASTree parent = EMPTY_EXPR_VALUE;
  will_return(__wrap_llist_size, 1);
  assert_null(astree_replace(&parent, 1, &EMPTY_EXPR));
}

void replace_null_child(void **state) {
  ASTree parent = EMPTY_EXPR_VALUE;
  will_return(__wrap_llist_size, 1);
  expect_assert_failure(astree_replace(&parent, 0, NULL));
}

int main(int argc, char **argv) {
  struct CMUnitTest tests[] = {cmocka_unit_test(destroy_null),
                               cmocka_unit_test(destroy_empty),
                               cmocka_unit_test(destroy_addrof),
                               cmocka_unit_test(adopt_too_many),
                               cmocka_unit_test(adopt_again),
                               cmocka_unit_test(adopt_empty_again),
                               cmocka_unit_test(adopt_null_child),
                               cmocka_unit_test(replace_out_of_bounds),
                               cmocka_unit_test(replace_null_child)};

  return cmocka_run_group_tests(tests, NULL, NULL);
}
