#include <cmocka.h>
#include "badllist.h"

int __wrap_llist_init(LinkedList *list, BlibDestroyer dest, BlibComparator comp) {
    return mock_type(int);
}
int __wrap_llist_destroy(LinkedList *list) {
    return mock_type(int);
}
/* int __wrap_llist_copy(LinkedList *dest, const LinkedList *src); */
int __wrap_llist_push_front(LinkedList *list, void *element) {
    return mock_type(int);
}
void *__wrap_llist_pop_front(LinkedList *list) {
    return mock_ptr_type(void*);
}
void *__wrap_llist_front(const LinkedList *list) {
    return mock_ptr_type(void*);
}

int __wrap_llist_push_back(LinkedList *list, void *element) {
    return mock_type(int);
}
void *__wrap_llist_pop_back(LinkedList *list) {
    return mock_ptr_type(void*);
}
void *__wrap_llist_back(const LinkedList *list) {
    return mock_ptr_type(void*);
}
/* int __wrap_llist_rotate_forwards(LinkedList *list); */
/* int __wrap_llist_rotate_backwards(LinkedList *list); */
void *__wrap_llist_get(const LinkedList *list, size_t index) {
    return mock_ptr_type(void*);
}
int __wrap_llist_insert(LinkedList *list, void *element, size_t index) {
    return mock_type(int);
}
int __wrap_llist_delete(LinkedList *list, size_t index) {
    return mock_type(int);
}
void *__wrap_llist_extract(LinkedList *list, size_t index) {
    return mock_ptr_type(void*);
}
size_t __wrap_llist_find(const LinkedList *list, void *target) {
    return mock_type(size_t);
}
size_t __wrap_llist_rfind(const LinkedList *list, void *target) {
    return mock_type(size_t);
}
/* void __wrap_llist_foreach(LinkedList *list, void (*fn)(void *)); */
/* int __wrap_llist_sort(LinkedList *list, int (*compare)(void *, void *)); */
size_t __wrap_llist_size(const LinkedList *list) {
    return mock_type(size_t);
}
int __wrap_llist_empty(const LinkedList *list) {
    return mock_type(int);
}
int __wrap_llist_status(const LinkedList *list) {
    return mock_type(int);
}
