#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
/* get rid of inline in cmocka */
#define inline
#undef UNIT_TESTING
#include <cmocka.h>
#include <stdlib.h>

#include "astree.h"
#include "yyparse.h"

/* fake global... regret... */
int skip_type_check = 0;
/* duplicate global... regret... */
const TypeSpec SPEC_EMPTY = {0, 0, BLIB_LLIST_EMPTY, TYPESPEC_FLAG_NONE,
                             TYPE_NONE};
const Location LOC_EMPTY = LOC_EMPTY_VALUE;
const AuxSpec AUXSPEC_PTR = {{{0, 0, 0}}, AUX_POINTER};
const AuxSpec AUXSPEC_CONST_PTR = {{{0, TYPESPEC_FLAG_CONST, 0}}, AUX_POINTER};
const AuxSpec AUXSPEC_VOLATILE_PTR = {{{0, TYPESPEC_FLAG_VOLATILE, 0}},
                                      AUX_POINTER};
const AuxSpec AUXSPEC_CONST_VOLATILE_PTR = {
    {{0, TYPESPEC_FLAG_CONST | TYPESPEC_FLAG_VOLATILE, 0}}, AUX_POINTER};

const char *__wrap_string_set_intern(const char *string) {
  check_expected_ptr(string);
  return mock_ptr_type(const char *);
}

const char *__wrap_parser_get_tname(int symbol) {
  check_expected(symbol);
  return mock_ptr_type(const char *);
}

int __wrap_symbol_table_destroy(SymbolTable *table) {
  check_expected_ptr(table);
  return mock_type(int);
}

int __wrap_auxspec_destroy(AuxSpec *aux) {
  check_expected_ptr(aux);
  return mock_type(int);
}

int __wrap_typespec_destroy(TypeSpec *type) {
  check_expected_ptr(type);
  return mock_type(int);
}

int __wrap_typespec_append_auxspecs(TypeSpec *dest, const TypeSpec *src) {
  check_expected_ptr(dest);
  check_expected_ptr(src);
  return mock_type(int);
}

int __wrap_typespec_append_aux(TypeSpec *type, AuxSpec *aux) {
  check_expected_ptr(type);
  check_expected_ptr(aux);
  return mock_type(int);
}

int __wrap_typespec_prepend_aux(TypeSpec *type, AuxSpec *aux) {
  check_expected_ptr(type);
  check_expected_ptr(aux);
  return mock_type(int);
}

AuxSpec *__wrap_typespec_get_aux(TypeSpec *type) {
  check_expected_ptr(type);
  return mock_ptr_type(AuxSpec *);
}

AuxSpec *__wrap_create_erraux_v(int errcode, size_t info_count,
                                va_list info_ptrs) {
  check_expected(errcode);
  check_expected(info_count);
  return mock_ptr_type(AuxSpec *);
}

int __wrap_typespec_init(TypeSpec *type) {
  check_expected_ptr(type);
  return mock_type(int);
}

void destroy_null(void **state) {
  int status = astree_destroy(NULL);
  assert_false(status);
}

void destroy_empty(void **state) {
  ASTree *tree = &EMPTY_EXPR;
  int status = astree_destroy(tree);
  assert_false(status);
  assert_ptr_equal(tree, &EMPTY_EXPR);
  assert_memory_equal(tree, &EMPTY_EXPR, sizeof(ASTree));
}

void destroy_addrof(void **state) {
  expect_any(__wrap_string_set_intern, string);
  will_return(__wrap_string_set_intern, "&");

  ASTree *addrof = astree_init(TOK_ADDROF, LOC_EMPTY, NULL);
  TypeSpec *addrof_spec = test_malloc(sizeof(TypeSpec));
  AuxSpec expected_ptr_aux = {0};
  addrof->type = addrof_spec;

  expect_value(__wrap_typespec_destroy, type, addrof_spec);
  will_return(__wrap_typespec_destroy, 0);
  expect_value(__wrap_symbol_table_destroy, table, NULL);
  will_return(__wrap_symbol_table_destroy, 0);

  int status = astree_destroy(addrof);
  assert_false(status);
}

void adopt_too_many(void **state) {
  expect_any_count(__wrap_string_set_intern, string, 3);
  will_return(__wrap_string_set_intern, "_terr");
  will_return_count(__wrap_string_set_intern, ";", 2);
  expect_value_count(__wrap_symbol_table_destroy, table, NULL, 3);
  will_return_count(__wrap_symbol_table_destroy, 0, 3);

  ASTree *errnode = astree_init(TOK_TYPE_ERROR, LOC_EMPTY, NULL);
  ASTree *child1 = astree_init(';', LOC_EMPTY, NULL);
  ASTree *child2 = astree_init(';', LOC_EMPTY, NULL);

  expect_assert_failure(astree_adopt(errnode, 2, child1, child2));
  assert_false(astree_destroy(errnode));
  assert_false(astree_destroy(child1));
  assert_false(astree_destroy(child2));
}

void adopt_again(void **state) {
  expect_any_count(__wrap_string_set_intern, string, 2);
  will_return_count(__wrap_string_set_intern, ";", 2);
  expect_value_count(__wrap_symbol_table_destroy, table, NULL, 2);
  will_return_count(__wrap_symbol_table_destroy, 0, 2);
  ASTree *parent = astree_init(';', LOC_EMPTY, NULL);
  ASTree *child = astree_init(';', LOC_EMPTY, NULL);

  expect_assert_failure(astree_adopt(parent, 2, child, child));
  assert_false(astree_destroy(parent));
}

void adopt_empty_again(void **state) {
  expect_any(__wrap_string_set_intern, string);
  will_return(__wrap_string_set_intern, ";");
  expect_value(__wrap_symbol_table_destroy, table, NULL);
  will_return(__wrap_symbol_table_destroy, 0);
  ASTree *parent = astree_init(';', LOC_EMPTY, NULL);
  (void)astree_adopt(parent, 2, &EMPTY_EXPR, &EMPTY_EXPR);
  assert_false(astree_destroy(parent));
}

void adopt_null_child(void **state) {
  expect_any(__wrap_string_set_intern, string);
  will_return(__wrap_string_set_intern, ";");
  expect_value(__wrap_symbol_table_destroy, table, NULL);
  will_return(__wrap_symbol_table_destroy, 0);
  ASTree *parent = astree_init(';', LOC_EMPTY, NULL);

  expect_assert_failure(astree_adopt(parent, 1, NULL));
  assert_false(astree_destroy(parent));
}

void replace_out_of_bounds(void **state) {
  expect_any_count(__wrap_string_set_intern, string, 2);
  will_return_count(__wrap_string_set_intern, ";", 2);
  expect_value_count(__wrap_symbol_table_destroy, table, NULL, 2);
  will_return_count(__wrap_symbol_table_destroy, 0, 2);

  ASTree *parent = astree_init(';', LOC_EMPTY, NULL);
  assert_null(astree_replace(parent, 1, &EMPTY_EXPR));

  (void)astree_adopt(parent, 3, &EMPTY_EXPR, &EMPTY_EXPR, &EMPTY_EXPR);
  ASTree *child = astree_init(';', LOC_EMPTY, NULL);
  assert_null(astree_replace(parent, 4, child));

  assert_ptr_equal(astree_replace(parent, 1, child), &EMPTY_EXPR);
  assert_ptr_equal(astree_get(parent, 1), child);

  assert_false(astree_destroy(parent));
}

void replace_null_child(void **state) {
  ASTree parent = EMPTY_EXPR_VALUE;
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
