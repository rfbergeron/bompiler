#include <stddef.h>

struct foo;

struct bar {
    size_t count;
    struct foo *foos;
};

struct baz {
    struct foo foo;
};

int main(void) {
    return 0;
}
