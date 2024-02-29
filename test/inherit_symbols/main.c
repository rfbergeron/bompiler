int foo[];

void bar(int i) {
    extern int foo[6];
}

int foo[];
int foo[6];

struct bing { void *ptr; unsigned long size; } bong[4];
void baz(void) {
    extern void baz(void);
    extern void bar();
    extern struct bing bong[];
}

static struct yeet { char *ptr; unsigned long size; unsigned long cap; } yomp[];
void yoink(void) {
    extern struct yeet yomp[8];
}
static struct yeet yomp[8];

static int fee();

static void fie(void) {
    extern int fee(void *, unsigned long);
}

static int fee(void *, unsigned long);

unsigned long foe(char *);
