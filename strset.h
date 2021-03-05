#ifndef __STRING_SET__
#define __STRING_SET__

#include <stdio.h>
#include <string.h>

void string_set_init_globals();
void string_set_free_globals();
const char *string_set_intern(const char *string);
void string_set_dump(FILE *out);

#endif
