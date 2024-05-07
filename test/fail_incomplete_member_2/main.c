#include <stddef.h>

struct foo {
    size_t count;
    int bar[];
};

int main(void) {
    return 0;
}
