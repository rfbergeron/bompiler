struct typespec {
  enum base_type {
    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_FUNCTION,
    TYPE_INTEGRAL,
    TYPE_FLOATING,
    TYPE_UNION,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_VOID,
    TYPE_CHAR
  } base;
  unsigned int flags;
  unsigned long alignment;
  unsigned long width;
  struct typespec *next;
};

union astree {
  struct expr {
    int symbol;
    char *lexinfo;
    struct llist *children;
    unsigned int attributes;
    struct typespec *expr_type;
    unsigned long constval;
  } expr;
  struct stmt {
    int symbol;
    char *lexinfo;
    struct llist *children;
    unsigned int attributes;
    unsigned long jump_id;
    unsigned long case_id;
  } stmt;
  struct compound {
    int symbol;
    char *lexinfo;
    struct llist *children;
    enum scope_type {
      SCOPE_TRANS_UNIT,
      SCOPE_BLOCK,
      SCOPE_FUNCTION
    } scope_type;
  } compound;
};

int main(int argc, char **argv) {
  static char balls[69];
  static struct typespec tspec;
  (void)argc, (void)argv;
  static union astree foo = {{2, balls, ((void*)0), 0, &tspec, 0 }};
  (void)foo;
  return 0;
}
