typedef void foo;

foo fn(void);

int main(void) {
    typedef int bar[];
    extern foo fn2(bar);
    bar baz = {0, 1, 2};
    return 0;
}
