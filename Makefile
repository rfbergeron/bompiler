# Makefile, Author: Robert Bergeron

WARN = -Wall -Wextra -Werror -Wpedantic -Wshadow
SANITIZE = -fsanitize=address
#DEBUG = -ggdb -pg
CFLAGS = -O0 -ansi -Ibadlib -Ibadlib/murmur3 -ggdb -pg
C = gcc
CYY = ${C} -Wno-sign-compare
CDEP = ${C} -ansi -MMD ${WARN}
MAKEFILE = Makefile
DEPFILE = .depend
# name of the compiled executable
EXECNAME = bompiler
# .c source files
SRC = debug.c attributes.c strset.c lyutils.c astree.c symtable.c typecheck.c asmgen.c main.c
# .h header files
HDR = debug.h attributes.h strset.h lyutils.h astree.h symtable.h typecheck.h asmgen.h simplestack.h
# files generated by flex and bison
GENSRC = yyparse.c yylex.c
GENHDR = yyparse.h
# library files
BADLIB = badmap.c badllist.c badalist.c murmur3/murmur3.c
LIBSRC = $(foreach file, ${BADLIB}, badlib/${file})
LIBHDR = ${BADLIB:.c=.h} badlib.h
# convenient groups
ALLSRC = ${SRC} ${GENSRC} ${LIBSRC}
ALLHDR = ${HDR} ${GENHDR} ${LIBHDR}
# .o object files
OBJ = $(notdir ${ALLSRC:.c=.o})
# .gch header gens
GCH = ${ALLHDR:=.gch}
# files generated by GNU Indent
FMT = ${SRC:=~} ${HDR:=~} parser.y~
# set VPATH for includes
vpath %.c = badlib:badlib/murmur3
vpath %.h = badlib:badlib/murmur3

# Target-specific variables
sanitize: CFLAGS += ${SANITIZE}
debug: CFLAGS += ${DEBUG}
warn: CFLAGS += ${WARN}
paranoid: CFLAGS += ${SANITIZE} ${WARN} ${DEBUG}

all : ${DEPFILE} ${EXECNAME}

sanitize : ${DEPFILE} ${EXECNAME}

warn : ${DEPFILE} ${EXECNAME}

debug : ${DEPFILE} ${EXECNAME}

paranoid : ${DEPFILE} ${EXECNAME}

${EXECNAME} : ${OBJ}
	${C} ${CFLAGS} -o $@ $^

yylex.o : yylex.c
	${CYY} ${CFLAGS} -c $^

yyparse.o : yyparse.c yyparse.h
	${CYY} ${CFLAGS} -c $^

%.o : %.c ${ALLHDR}
	${C} ${CFLAGS} -c $^

yylex.c : scanner.l
	flex --outfile=yylex.c scanner.l

yyparse.h yyparse.c : parser.y
	bison -Wall -v --defines=yyparse.h --output=yyparse.c parser.y

clean:
	rm -f ${OBJ} ${GENSRC} ${GENHDR} ${DEPFILE} ${GCH}

spotless: clean
	rm -f ${EXECNAME} ${FMT} *.output *.out

cleantest:
	rm -f *.str *.tok *.ast *.sym *.oil

ci: 
	git add ${HDR} ${SRC} ${MAKEFILE} README.md DESIGN.md TODO.md DESIGN2.md \
	IDEAS.md .gitignore parser.y scanner.l

${DEPFILE}: ${ALLSRC}
	@ touch $@
	makedepend -f$@ -Y -- ${CFLAGS} -- ${ALLSRC}

depend: ${DEPFILE}

indent:
	indent -l80 -nfca -hnl -ncdb -bad -bap -nbc -bbo -nbfda -npsl -br -brs \
	-brf -ce -cdw -lps -saf -sai -saw -pcs -cs -bs -nut -ts4 -i4 -pi4 -ci4 \
	-cli4 -ppi4 -cbi0 -bli0 ${SRC} ${HDR} parser.y

format:
	clang-format --style=Google -i ${SRC} ${HDR} parser.y

sinclude ${DEPFILE}
