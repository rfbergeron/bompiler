#include <iostream>
#include <cstdlib>
#include <string>
#include <cstring>
#include <iomanip>
#include <fstream>
#include <vector>

#include <libgen.h>
#include <unistd.h>

#include "auxlib.h"
#include "string_set.h"
#include "lyutils.h"
#include "astree.h"
#include "symtable.h"
#include "yyparse.h"
#include "intlang.h"

const string CPP = "/usr/bin/cpp";
const string OC_EXT = ".oc";
const string STR_EXT = ".str";
const string TOK_EXT = ".tok";
const string AST_EXT = ".ast";
const string SYM_EXT = ".sym";
const string OIL_EXT = ".oil";
constexpr size_t LINESIZE = 1024;
string cpp_opts = " -nostdinc";
ofstream strfile;
ofstream tokfile;
ofstream astfile;
ofstream symfile;
ofstream oilfile;

// Print the meaning of a signal.
static string sig_text (int signal) {
   string sig_nr = to_string (signal);
   const char* sigstr = strsignal (signal);
   if (sigstr == nullptr) return sig_nr;
                     else return sig_nr + " (" + sigstr + ")";
}

// Print the status returned from a subprocess.
void cerr_status (const char* command, int status) {
   if (status == 0) return; 
   cerr << command << ": " << status << setw(4) << setfill('0')
        << hex << status << setfill(' ') << dec;
   if (WIFEXITED (status)) {
      cerr << ", exit " << WEXITSTATUS (status);
   }
   if (WIFSIGNALED (status)) {
      cerr << ", Terminated signal " << sig_text (WTERMSIG (status));
      #ifdef WCOREDUMP
      if (WCOREDUMP (status)) cerr << ", core dumped";
      #endif
   }
   if (WIFSTOPPED (status)) {
      cerr << ", Stopped signal ", sig_text (WSTOPSIG (status));
   }
   if (WIFCONTINUED (status)) {
      cerr << ", Continued";
   }
   cerr << endl;
}

void scan_options(int argc, char** argv) { 
   opterr = 0;
   for (;;) {
      int option = getopt (argc, argv, "@:D:ly");
      if (option == EOF) break;
      switch (option) {
         case '@':
            debug::setflags (optarg);
            break;
         case 'D':
            // cpp args
            DEBUGH('c', "     cpp option: " << optarg);
            cpp_opts.append(" -D ").append(optarg);
            break;
         case 'l':
            DEBUGH('c', "     yy_flex_debug set to 1");
            yy_flex_debug = 1;
            break;
         case 'y':
            DEBUGH('c', "     yydebug set to 1");
            yydebug = 1;
            break;
         default:
            cerr << "-" << char (optopt) << ": invalid option"
               << endl;
            break;
      }
   }
}

int main(int argc, char** argv) {
   yydebug = 0;
   yy_flex_debug = 0;
   scan_options(argc, argv);
   ios_base::sync_with_stdio(true);
   // calls to getopt increment optind, so after scan_options returns
   // we should be at the name of the target file.
   DEBUGH('a', "     oc program infile_path: " << argv[optind]);
   DEBUGH('a', "     CPP exec: " << CPP << cpp_opts);

   string oc_name { basename(argv[optind]) };
   size_t ext_index = oc_name.rfind(OC_EXT);
   if(ext_index == string::npos ||
         OC_EXT.compare(oc_name.substr(ext_index))) {
      cerr << "not an .oc file: " << oc_name << endl;
      return EXIT_FAILURE;
   }

   string base_name = oc_name.substr(0, ext_index);
   string str_name = base_name + STR_EXT;
   string tok_name = base_name + TOK_EXT;
   string ast_name = base_name + AST_EXT;
   string sym_name = base_name + SYM_EXT;
   string oil_name = base_name + OIL_EXT;

   strfile.open(str_name.c_str());
   if((strfile.rdstate() & ofstream::failbit) != 0) {
      cerr << "failed to open file for writing: "
           << str_name << endl;
      return EXIT_FAILURE;
   }

   tokfile.open(tok_name.c_str());
   if((tokfile.rdstate() & ofstream::failbit) != 0) {
      cerr << "failed to open file for writing: "
           << tok_name << endl;
      return EXIT_FAILURE;
   }

   astfile.open(ast_name.c_str());
   if((astfile.rdstate() & ofstream::failbit) != 0) {
      cerr << "failed to open file for writing: "
           << ast_name << endl;
      return EXIT_FAILURE;
   }

   symfile.open(sym_name.c_str());
   if((symfile.rdstate() & ofstream::failbit) != 0) {
      cerr << "failed to open file for writing: "
           << sym_name << endl;
      return EXIT_FAILURE;
   }

   oilfile.open(oil_name.c_str());
   if((symfile.rdstate() & ofstream::failbit) != 0) {
       cerr << "Failed to pen file for writing: "
            << sym_name << endl;
       return EXIT_FAILURE;
    }

   string command = CPP + cpp_opts + " " + oc_name;
   yyin = popen(command.c_str(), "r");
   int exit_status = EXIT_SUCCESS;
   if(yyin == nullptr ) {
      cerr << "failed to open pipe for command: " << command << endl;
      return EXIT_FAILURE;
   } else {
      DEBUGH('y', "  activating bison");
      int parse_status = yyparse();

      if(parse_status == 0) {
         DEBUGH('y', "  bison parse was successful");
         // do type checking
         int typecheck_status = type_checker::
              make_symbol_table(parser::root);
         if(typecheck_status == 0) {
            string_set::dump(strfile);
            astree::print(astfile, parser::root, 0); 
            type_checker::dump_symbols(symfile);
            generator::set_out(&oilfile);
            generator::write_int_lang(parser::root,
                    type_checker::get_tables(),
                    type_checker::get_string_constants());
            DEBUGH('y', "  all outputs dumped");
            type_checker::destroy_tables();
         } else {
            cerr << "Type checking failed" << endl;
            exit_status = EXIT_FAILURE;
         }
      }
      else {
        cerr << "Parsing failed with status " << parse_status << endl;  
        exit_status = EXIT_FAILURE;
      }

      int pclose_status = pclose(yyin);
      cerr_status (command.c_str(), pclose_status);
      strfile.close();
      tokfile.close();
      astfile.close();
      symfile.close();
      DEBUGH('a', "  exiting");
      return exit_status;
   }
}
