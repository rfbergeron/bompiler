# Makefile, Author: Robert Bergeron

WARN = -Wall -Wextra -Werror -Wpedantic -Wshadow
# full gcc exec command
C = gcc -g -O0 -std=gnu11
CWARN = ${C} ${WARN} -fdiagnostics-color=never
CYY = ${C} -Wno-sign-compare
CDEP = gcc -std=gnu11 -MM ${WARN}
CDBG = ${C} -pg -fsanitize=address
MKFILE = Makefile
DEPFILE = ${MKFILE}.dep
GMAKE = ${MAKE} --no-print-directory
# name of the compiled executable
EXECNAME = bompiler
# name of the testing executable
TSTEXECNAME = test
# main source file
MAIN = main.c
# test source file
TEST = test.c
# .c source files
SRC = map.c string_set.c lyutils.c astree.c vector.c auxlib.c
# .h header files
HDR = map.h string_set.h vector.h lyutils.h auxlib.h astree.h symtable.h
# .o object files
OBJ = ${SRC:.c=.o} ${GENSRC:.c=.o}
# .gch header somethings?
GCH = ${HDR:=.gch} ${GENHDR:=.gch}
# files generated by flex and bison
GENSRC = yylex.c yyparse.c
GENHDR = yyparse.h
# files generated by the formatter
FMT = ${SRC:=~} ${HDR:=~} ${TEST:=~} parser.y~

all : ${DEPFILE} ${EXECNAME}

${EXECNAME} : ${OBJ} ${MAIN:.c=.o}
	${C} -o bompiler ${OBJ} ${MAIN:.c=.o}

${TSTEXECNAME} : ${OBJ} ${TEST:.c=.o}
	${C} -o test ${OBJ} ${TEST:.c=.o}

debug : ${OBJ} ${MAIN:.c=.o}
	${CDBG} -o bompiler ${OBJ} ${MAIN:.c=.o}

yylex.o : yylex.c
	${CYY} -c $< ${HDR} ${GENHDR}

yyparse.o : yyparse.c yyparse.h
	${CYY} -c $< ${HDR} ${GENHDR}

%.o : %.c ${GENHDR}
	${C} -c $< ${HDR} ${GENHDR}

yylex.c : scanner.l
	flex --outfile=yylex.c scanner.l

yyparse.h yyparse.c : parser.y
	bison -Wall -v --defines=yyparse.h --output=yyparse.c parser.y

clean:
	rm -f ${OBJ} ${TEST:.c=.o} ${MAIN:.c=.o} ${GENSRC} ${GENHDR} ${DEPFILE} \
	${GCH}

spotless: clean
	rm -f ${EXECNAME} ${TSTEXECNAME} ${FMT} *.output *.out

ci: 
	git add ${HDR} ${SRC} ${MKFILE} ${TEST} ${MAIN} README.md DESIGN.md \
	.gitignore parser.y scanner.l

dep: ${SRC} ${GENSRC} ${GENHDR}
	${CDEP} ${SRC} ${HDR} ${TEST} ${MAIN} ${GENSRC} ${GENHDR} \
	>> ${DEPFILE}

indent:
	indent -l80 -nfca -hnl -ncdb -bad -bap -nbc -bbo -nbfda -npsl -br -brs \
	-brf -ce -cdw -lps -saf -sai -saw -pcs -cs -bs -nut -ts4 -i4 -pi4 -ci4 \
	-cli4 -ppi4 -cbi0 -bli0 ${SRC} ${HDR} ${TEST} ${MAIN} parser.y

format:
	clang-format --style=file -i ${SRC} ${HDR} ${TEST} ${MAIN} parser.y


${DEPFILE} :
	@ touch ${DEPFILE}
	${GMAKE} dep
