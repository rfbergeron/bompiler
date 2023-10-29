void do_nothing (void) {
    return (void)(2 + 4 * 10);
}

void silly_inc (int *i) {
    return (void)++*i;
}

int main(void) {
    int i = 0;
    silly_inc(&i);
    do_nothing();
    return 0;
}
