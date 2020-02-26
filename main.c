#include <assert.h>
#include <err.h>
#include <getopt.h>
#include <libgen.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "astree.h"
#include "auxlib.h"
#include "symtable.h"
#include "lyutils.h"
#include "strset.h"

#define LINESIZE 1024
const char *CPP = "/usr/bin/cpp -nostdinc ";
const char *OC_EXT = ".oc";
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

// cpp options go here
FILE *strfile;
FILE *tokfile;
FILE *astfile;
FILE *symfile;
FILE *oilfile;

void scan_options (int argc, char **argv) {
    opterr = 0;
    for (;;) {
        int option = getopt (argc, argv, "@:D:ly");

        if (option == EOF) break;
        switch (option) {
            case '@':
                debug_set_flags (optarg);
                break;
            case 'D':
                // cpp args
                // DEBUGH('c', "     cpp option: " << optarg);
                // cpp_opts.append(" -D ").append(optarg);
                break;
            case 'l':
                // DEBUGH('c', "     yy_flex_debug set to 1");
                yy_flex_debug = 1;
                break;
            case 'y':
                // DEBUGH('c', "     yydebug set to 1");
                yydebug = 1;
                break;
            default:
                fprintf (stderr, "-%c: invalid option\n", (char) optopt);
                break;
        }
    }
}

int main (int argc, char **argv) {
    yydebug = 0;
    yy_flex_debug = 0;
    scan_options (argc, argv);

    char *oc_name = basename (argv[optind]);
    char *ext_ptr = strstr (oc_name, OC_EXT);
    ptrdiff_t name_len = ext_ptr - oc_name;

    assert (name_len < LINESIZE);

    if (ext_ptr == NULL || name_len <= 0) {
        errx (1, "%s: Not an oc file.", oc_name);
    }

    struct stat statbuf;
    int status = stat (oc_name, &statbuf);

    if (status == -1) {
        err (1, "%s", oc_name);
    }

    DEBUGS ('m', "Creating names of output files and cpp exec line");
    memcpy (strname, oc_name, name_len);
    memcpy (strname + name_len, STR_EXT, strlen (STR_EXT));
    memcpy (tokname, oc_name, name_len);
    memcpy (tokname + name_len, TOK_EXT, strlen (TOK_EXT));
    memcpy (astname, oc_name, name_len);
    memcpy (astname + name_len, AST_EXT, strlen (AST_EXT));
    memcpy (cppcmd, CPP, strlen (CPP));
    memcpy (cppcmd + strlen (CPP), oc_name, strlen (oc_name));

    DEBUGS ('m', "Opening files and pipes");
    yyin = popen (cppcmd, "r");
    if (yyin == NULL) {
        err (1, "%s", cppcmd);
    }
    strfile = fopen (strname, "w");
    if (strfile == NULL) {
        err (1, "%s", strname);
    }
    tokfile = fopen (tokname, "w");
    if (tokfile == NULL) {
        err (1, "%s", tokname);
    }
    astfile = fopen (astname, "w");
    if (astfile == NULL) {
        err (1, "%s", astname);
    }
    // remember to initialize certain things, like the string table
    string_set_init_globals ();
    lexer_init_globals ();
    type_checker_init_globals ();

    DEBUGS ('m', "Parsing");
    int parse_status = yyparse ();

    if (parse_status != 0) {
        warnx ("Parsing failed with status %d.", parse_status);
    } else {
        DEBUGS ('m', "Parse successful; dumping strings.");
        string_set_dump (strfile);
        astree_print_tree (parser_root, astfile, 0);
        make_symbol_table (parser_root);
    }

    DEBUGS ('m', "Execution finished; wrapping up.");
    int pclose_status = pclose (yyin);

    fclose (strfile);
    fclose (tokfile);
    fclose (astfile);
    string_set_free_globals ();
    lexer_free_globals ();
    type_checker_free_globals ();
    return EXIT_SUCCESS;
}
