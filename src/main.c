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
const char *CPP = "/usr/bin/cpp -nostdinc ";
const char *SRC_EXT = ".c";
const char *STR_EXT = ".str";
const char *TOK_EXT = ".tok";
const char *AST_EXT = ".ast";
const char *SYM_EXT = ".sym";
const char *IL_EXT = ".il";
const char *ERR_EXT = ".err";
char cppcmd[LINESIZE];
char name[LINESIZE];
char strname[LINESIZE];
char tokname[LINESIZE];
char astname[LINESIZE];
char symname[LINESIZE];
char ilname[LINESIZE];
char errname[LINESIZE];

/* cpp options go here */
FILE *strfile;
FILE *tokfile;
FILE *astfile;
FILE *symfile;
FILE *ilfile;
FILE *errfile;

int skip_type_check = 0;
int skip_asm = 0;
int stdin_tmp_fileno;

void scan_options(int argc, char **argv) {
  opterr = 0;
  for (;;) {
    int option = getopt(argc, argv, "@:D:lyca");

    if (option == EOF) break;
    switch (option) {
      case '@':
        debug_set_flags(optarg);
        break;
      case 'D':
        /* cpp args
           DEBUGH('c', "     cpp option: " << optarg);
           cpp_opts.append(" -D ").append(optarg); */
        break;
      case 'l':
        /* DEBUGH('c', "     yy_flex_debug set to 1"); */
        yy_flex_debug = 1;
        break;
      case 'y':
        /* DEBUGH('c', "     yydebug set to 1"); */
        yydebug = 1;
        break;
      case 'c':
        skip_type_check = 1;
        break;
      case 'a':
        skip_asm = 1;
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
  scan_options(argc, argv);

  char *srcpath = argv[optind];
  char *srcname = basename(srcpath);
  char *ext_ptr = strstr(srcname, SRC_EXT);
  ptrdiff_t name_len = ext_ptr - srcname;

  assert(name_len < LINESIZE);

  if (ext_ptr == NULL || name_len <= 0) {
    errx(1, "%s: Not a valid source file name.", srcname);
  }

  struct stat statbuf;
  int status = stat(srcpath, &statbuf);

  if (status == -1) {
    err(1, "%s", srcname);
  }

  /* chop off the source extension */
  strcpy(name, srcname);
  name[name_len] = 0;

  DEBUGS('m', "Creating names of output files and cpp exec line");
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
  strcpy(errname, name);
  strcat(errname, ERR_EXT);
  strcpy(cppcmd, CPP);
  strcat(cppcmd, srcname);

  DEBUGS('m', "Opening preprocessor pipe");
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
    DEBUGS('m', "value of yyin: %p", yyin);
    if (yyin == NULL) {
      err(1, "%s", cppcmd);
    }
  } else {
    /* child process procedures*/
    /* duplicate the write end of the pipe to fd 1, or stdout */
    dup2(pipedes[1], STDOUT_FILENO);
    /* close extra fds */
    close(pipedes[0]);
    close(pipedes[1]);
    /* execute the preprocessor; if successful execution should stop here */
    execlp("/usr/bin/cpp", "cpp", srcpath, NULL);
    /* error if the preprocessor was unable to run */
    err(EXIT_FAILURE, "Failed to lay pipe\n");
  }

  DEBUGS('m', "Opening output files");
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
  errfile = fopen(errname, "w");
  if (errfile == NULL) {
    err(1, "%s", ilname);
  }

  /* remember to initialize certain things, like the string table */
  string_set_init_globals();
  lexer_init_globals();
  state = state_init();
  asmgen_init_globals();

  status = yyparse();
  if (status) {
    warnx("Parsing failed with status %d.", status);
    goto cleanup;
  }

  string_set_print(strfile);
  astree_print_symbols(UNWRAP(parser_root), symfile);
  astree_print_tree(UNWRAP(parser_root), astfile, 0);

  if (skip_asm || skip_type_check) goto cleanup;
  status = translate_file(parser_root);
  if (status) {
    warnx("Assembly translation failed.");
    goto cleanup;
  }

  status = write_asm(ilfile);
  if (status) {
    warnx("Failed to emit assembly instructions.");
    goto cleanup;
  }

cleanup:
  if (parser_root->symbol == TOK_TYPE_ERROR) {
    int status = print_errs((TypeSpec *)parser_root->type, errfile);
    if (status) {
      warnx("Failed to print program errors.");
    }
  }
  DEBUGS('m', "Execution finished; wrapping up.");

  /* restore stdin */
  dup2(stdin_tmp_fileno, STDIN_FILENO);
  close(stdin_tmp_fileno);

  fclose(strfile);
  fclose(tokfile);
  fclose(astfile);
  fclose(symfile);
  fclose(ilfile);
  fclose(errfile);

  DEBUGS('m', "global state cleanup");
  state_destroy(state);
  DEBUGS('m', "syntax tree cleanup");
  astree_destroy(parser_root);
  DEBUGS('m', "string set cleanup");
  string_set_free_globals();
  DEBUGS('m', "lexing/parsing helper cleanup");
  lexer_free_globals();
  DEBUGS('m', "assembly generator cleanup");
  asmgen_free_globals();
  return EXIT_SUCCESS;
}
