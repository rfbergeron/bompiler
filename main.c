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

#include "astree.h"
#include "debug.h"
#include "lyutils.h"
#include "strset.h"
#include "typecheck.h"

#define LINESIZE 1024
const char *CPP = "/usr/bin/cpp -nostdinc ";
const char *SRC_EXT = ".oc";
const char *STR_EXT = ".str";
const char *TOK_EXT = ".tok";
const char *AST_EXT = ".ast";
const char *SYM_EXT = ".sym";
const char *OIL_EXT = ".oil";
char cppcmd[LINESIZE];
char strname[LINESIZE];
char tokname[LINESIZE];
char astname[LINESIZE];
char symname[LINESIZE];
char oilname[LINESIZE];

/* cpp options go here */
FILE *strfile;
FILE *tokfile;
FILE *astfile;
FILE *symfile;
FILE *oilfile;

int do_type_check = 1;

void scan_options(int argc, char **argv) {
  opterr = 0;
  for (;;) {
    int option = getopt(argc, argv, "@:D:lyc");

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
        do_type_check = 0;
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

  char *oc_name = basename(argv[optind]);
  char *ext_ptr = strstr(oc_name, SRC_EXT);
  ptrdiff_t name_len = ext_ptr - oc_name;

  assert(name_len < LINESIZE);

  if (ext_ptr == NULL || name_len <= 0) {
    errx(1, "%s: Not an oc file.", oc_name);
  }

  struct stat statbuf;
  int status = stat(oc_name, &statbuf);

  if (status == -1) {
    err(1, "%s", oc_name);
  }

  DEBUGS('m', "Creating names of output files and cpp exec line");
  memcpy(strname, oc_name, name_len);
  memcpy(strname + name_len, STR_EXT, strlen(STR_EXT));
  memcpy(tokname, oc_name, name_len);
  memcpy(tokname + name_len, TOK_EXT, strlen(TOK_EXT));
  memcpy(astname, oc_name, name_len);
  memcpy(astname + name_len, AST_EXT, strlen(AST_EXT));
  memcpy(symname, oc_name, name_len);
  memcpy(symname + name_len, SYM_EXT, strlen(SYM_EXT));
  memcpy(cppcmd, CPP, strlen(CPP));
  memcpy(cppcmd + strlen(CPP), oc_name, strlen(oc_name));

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
    execlp("/usr/bin/cpp", "cpp", oc_name, NULL);
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

  /* remember to initialize certain things, like the string table */
  string_set_init_globals();
  lexer_init_globals();
  type_checker_init_globals();

  DEBUGS('m', "Parsing");
  int parse_status = yyparse();

  if (parse_status != 0) {
    warnx("Parsing failed with status %d.", parse_status);
  } else {
    DEBUGS('m', "Parse successful; dumping strings.");

    if (do_type_check) type_checker_make_table(parser_root);
    string_set_dump(strfile);
    astree_print_tree(parser_root, astfile, 0);
    /* type_checker_dump_symbols (symfile); */
  }

  DEBUGS('m', "Execution finished; wrapping up.");
  int pclose_status = pclose(yyin);

  fclose(strfile);
  fclose(tokfile);
  fclose(astfile);
  DEBUGS('m', "string set cleanup");
  string_set_free_globals();
  DEBUGS('m', "lexing/parsing helper cleanup");
  lexer_free_globals();
  DEBUGS('m', "type checker and symbol table cleanup");
  type_checker_free_globals();
  return EXIT_SUCCESS;
}
