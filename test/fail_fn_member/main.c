#include <stddef.h>

struct foo {
    size_t count;
    void (*fns)(void);
};

struct bar {
    void baz(void);
};

int main(void) {
    return 0;
}
