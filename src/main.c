#include <assert.h>
#include <err.h>
#include <getopt.h>
#include <libgen.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "asmgen.h"
#include "astree.h"
#include "bcc_err.h"
#include "debug.h"
#include "lyutils.h"
#include "state.h"
#include "strset.h"
#include "symtable.h"

#define LINESIZE 1024
const char *SRC_EXT = ".c";
const char *STR_EXT = ".str";
const char *TOK_EXT = ".tok";
const char *AST_EXT = ".ast";
const char *SYM_EXT = ".sym";
const char *IL_EXT = ".il";
const char *ASM_EXT = ".asm";
const char *ERR_EXT = ".err";

/* "builtin" preprocessor arguments */
const char *const CPP_BUILTIN_ARGS[] = {
    "cpp",
    "-std=c90",
    "-D__STRICT_ANSI__", /* suppress gcc specific code in system headers */
    "-U__GNUC__",
    "-U__GNUC_MINOR__",
    "-U__GNUC_PATCHLEVEL__",
    "-D_UINTPTR_T" /* suppress cmocka `uintptr_t`; its a redefinition */
};

char name[LINESIZE];
char strname[LINESIZE];
char tokname[LINESIZE];
char astname[LINESIZE];
char symname[LINESIZE];
char ilname[LINESIZE];
char asmname[LINESIZE];
char errname[LINESIZE];

FILE *strfile;
FILE *tokfile;
FILE *astfile;
FILE *symfile;
FILE *ilfile;
FILE *asmfile;
FILE *errfile;

static char **cpp_args;
static size_t cpp_args_cap = 1;
static size_t cpp_args_size =
    sizeof(CPP_BUILTIN_ARGS) / sizeof(*CPP_BUILTIN_ARGS);

int skip_asm = 0;
int skip_allocator = 0;
int skip_liveness = 0;
int skip_diagnostics = 0;
int semantic_error = 0;
int stdin_tmp_fileno;

void destroy_cpp_args(void) {
  size_t i;
  for (i = 0; i < cpp_args_size; ++i) free(cpp_args[i]);
  free(cpp_args);
}

void scan_options(int argc, char **argv) {
  opterr = 0;
  for (;;) {
    int option = getopt(argc, argv, "@:D:I:lycaALd");

    if (option == EOF) break;
    switch (option) {
      case '@':
        debug_set_flags(optarg);
        break;
      case 'D':
        /* fallthrough */
      case 'I':
        PFDBG2('c', "Preprocessor argument: -%c%s", option, optarg);
        /* leave two slots empty for file name and NULL pointer */
        if (cpp_args_size >= cpp_args_cap - 2)
          cpp_args = realloc(cpp_args, sizeof(*cpp_args) * (cpp_args_cap *= 2));
        /* add one for nul terminator and two for dash and option */
        cpp_args[cpp_args_size] =
            malloc(sizeof(**cpp_args) * (strlen(optarg) + 3));
        cpp_args[cpp_args_size][0] = '-';
        cpp_args[cpp_args_size][1] = option;
        cpp_args[cpp_args_size][2] = '\0';
        strcat(cpp_args[cpp_args_size++], optarg);
        break;
      case 'l':
        PFDBG0('c', "flex debug output enabled");
        yy_flex_debug = 1;
        break;
      case 'y':
        PFDBG0('c', "bison debug output enabled");
        yydebug = 1;
        break;
      /*
      case 'c':
        skip_type_check = 1;
        break;
      */
      case 'a':
        skip_asm = 1;
        break;
      case 'A':
        skip_allocator = 1;
        break;
      case 'L':
        skip_liveness = 1;
        break;
      case 'd':
        skip_diagnostics = 1;
        break;
      default:
        fprintf(stderr, "-%c: invalid option\n", (char)optopt);
        break;
    }
  }
}

int main(int argc, char **argv) {
  yydebug = 0;
  yy_flex_debug = 0;

  while (cpp_args_cap < cpp_args_size + 2) cpp_args_cap *= 2;
  cpp_args = malloc(cpp_args_cap * sizeof(*cpp_args));

  size_t i;
  for (i = 0; i < cpp_args_size; ++i) {
    cpp_args[i] = malloc((strlen(CPP_BUILTIN_ARGS[i]) + 1) * sizeof(char));
    strcpy(cpp_args[i], CPP_BUILTIN_ARGS[i]);
  }

  scan_options(argc, argv);

  /* allocate space so that srcpath can be freed in `destroy_cpp_args` */
  char *srcpath = malloc(strlen(argv[optind]) + 1);
  strcpy(srcpath, argv[optind]);
  char *srcname = basename(srcpath);
  char *ext_ptr = strstr(srcname, SRC_EXT);
  ptrdiff_t name_len =
      ext_ptr == NULL ? (ptrdiff_t)strlen(srcname) : ext_ptr - srcname;

  assert(name_len < LINESIZE);

  if (ext_ptr == NULL || name_len <= 0) {
    errx(1, "%s: Not a valid source file name.", srcname);
  }

  struct stat statbuf;
  int status = stat(srcpath, &statbuf);

  if (status == -1) {
    err(1, "%s", srcname);
  }

  /* chop off the source extension, if there is one */
  strcpy(name, srcname);
  name[name_len] = 0;

  PFDBG0('m', "Creating names of output files and cpp exec line");
  strcpy(strname, name);
  strcat(strname, STR_EXT);
  strcpy(tokname, name);
  strcat(tokname, TOK_EXT);
  strcpy(astname, name);
  strcat(astname, AST_EXT);
  strcpy(symname, name);
  strcat(symname, SYM_EXT);
  strcpy(ilname, name);
  strcat(ilname, IL_EXT);
  strcpy(asmname, name);
  strcat(asmname, ASM_EXT);
  strcpy(errname, name);
  strcat(errname, ERR_EXT);

  /* add path argument and NULL element to exec line */
  cpp_args[cpp_args_size++] = srcpath;
  cpp_args[cpp_args_size++] = NULL;

  PFDBG0('m', "Opening preprocessor pipe");
  /* this is way more complicated than it would normally be since -ansi does
   * not allow the use of the popen() function, so we have to do all the
   * heavy lifting ourselves
   */
  int pipedes[2];
  status = pipe(pipedes);
  if (status) {
    err(EXIT_FAILURE, NULL);
  }

  pid_t parent = fork();

  if (parent) {
    /* parent process procedures */
    /* save stdin for later */
    stdin_tmp_fileno = dup(STDIN_FILENO);
    /* duplicate the read end of the pipe to stdin */
    dup2(pipedes[0], STDIN_FILENO);
    /* close extra fds */
    close(pipedes[0]);
    close(pipedes[1]);
    /* set yyin to standard input, which should be the read end of the pipe */
    yyin = stdin;
    PFDBG1('m', "value of yyin: %p", yyin);
    if (yyin == NULL) {
      err(EXIT_FAILURE, "Parent failed to lay pipe");
    }
  } else {
    /* child process procedures*/
    /* duplicate the write end of the pipe to fd 1, or stdout */
    dup2(pipedes[1], STDOUT_FILENO);
    /* close extra fds */
    close(pipedes[0]);
    close(pipedes[1]);
    /* execute the preprocessor; if successful execution should stop here */
    execvp("cpp", cpp_args);
    /* error if the preprocessor was unable to run */
    err(EXIT_FAILURE, "Child failed to lay pipe");
  }

  PFDBG0('m', "Opening output files");
  strfile = fopen(strname, "w");
  if (strfile == NULL) {
    err(1, "%s", strname);
  }
  tokfile = fopen(tokname, "w");
  if (tokfile == NULL) {
    err(1, "%s", tokname);
  }
  astfile = fopen(astname, "w");
  if (astfile == NULL) {
    err(1, "%s", astname);
  }
  symfile = fopen(symname, "w");
  if (symfile == NULL) {
    err(1, "%s", symname);
  }
  ilfile = fopen(ilname, "w");
  if (ilfile == NULL) {
    err(1, "%s", ilname);
  }
  asmfile = fopen(asmname, "w");
  if (asmfile == NULL) {
    err(1, "%s", asmname);
  }
  errfile = fopen(errname, "w");
  if (errfile == NULL) {
    err(1, "%s", ilname);
  }

  /* remember to initialize certain things, like the string table */
  type_init_globals();
  string_set_init_globals();
  lexer_init_globals();
  state = state_init();
  astree_init_globals();
  asmgen_init_globals(srcname);

  int syntax_error = yyparse();
  if (syntax_error) {
    warnx("Parsing failed with status %d.", syntax_error);
    goto cleanup;
  }

  if (skip_diagnostics) goto skip_diagnostics;
  string_set_print(strfile);
  astree_print_symbols(parser_root, symfile, 0);
  astree_print_tree(parser_root, astfile, 0);
  generator_debug_il(ilfile);
skip_diagnostics:
  if (skip_asm) goto skip_asm;
  generator_print_il(asmfile);
skip_asm:

cleanup:
  PFDBG0('m', "Execution finished; wrapping up.");

  /* restore stdin */
  dup2(stdin_tmp_fileno, STDIN_FILENO);
  close(stdin_tmp_fileno);

  fclose(strfile);
  fclose(tokfile);
  fclose(astfile);
  fclose(symfile);
  fclose(ilfile);
  fclose(asmfile);
  fclose(errfile);

  destroy_cpp_args();
  PFDBG0('m', "syntax tree cleanup");
  astree_destroy_globals();
  PFDBG0('m', "global state cleanup");
  state_destroy(state);
  PFDBG0('m', "parser cleanup");
  parser_destroy_globals();
  PFDBG0('m', "assembly generator cleanup");
  asmgen_free_globals();
  PFDBG0('m', "string set cleanup");
  string_set_free_globals();
  PFDBG0('m', "lexing/parsing helper cleanup");
  lexer_free_globals();

  if (semantic_error || syntax_error)
    return EXIT_FAILURE;
  else
    return EXIT_SUCCESS;
}
