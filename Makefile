CC ?= gcc
CWARN ?= -Wall -Wextra -Wpedantic -Wshadow -Wno-declaration-after-statement
CFLAGS ?= -Isrc -Ibuild -Ibadlib -Ibadlib/murmur3 -ansi
MKFILE = Makefile
EXE = build/bompiler
SRC = debug.c bcc_err.c attributes.c strset.c lyutils.c astree.c symtable.c typecheck.c asmgen.c state.c main.c
HDR = debug.h bcc_err.h attributes.h strset.h lyutils.h astree.h symtable.h typecheck.h asmgen.h state.h simplestack.h
OBJ = ${SRC:.c=.o}
# files generated by flex and bison
GENSRC = yyparse.c yylex.c
GENHDR = yyparse.h
GENOBJ = ${GENSRC:.c=.o}
# library files
LIBHDR = badlib/badmap.h badlib/badllist.h badlib/badalist.h
LIBOBJ = badmap.o badllist.o badalist.o murmur3.o
# convenient groups
HDRFILES = ${HDR:%=src/%} ${GENHDR:%=build/%} ${LIBHDR}
OBJFILES = ${OBJ:%=build/%} ${GENOBJ:%=build/%} ${LIBOBJ:%=build/%}

.PHONY: all badlib build debug

all: CFLAGS += -O1
all: ${EXE}

debug: CFLAGS += -Og -pg -ggdb -fsanitize=address
debug: LIB_TARGET = debug
debug: ${EXE}

${EXE}: ${OBJFILES}
	${CC} ${CFLAGS} ${CWARN} -o $@ $^

build:
	[ -d build ] || mkdir build

badlib:
	git submodule update --init --recursive badlib

build/yylex.o: build/yylex.c
	${CC} ${CFLAGS} ${CWARN} -c -o $@ $^

build/yyparse.o: build/yyparse.c build/yyparse.h
	${CC} ${CFLAGS} ${CWARN} -c -o $@ $<

${LIBOBJ:%=build/%}: badlib build
	make -C badlib/ ${LIB_TARGET}
	cp ${@:build/%=badlib/%} ./build/

${LIBHDR}: badlib

build/%.o: src/%.c ${HDRFILES} build
	${CC} ${CFLAGS} ${CWARN} -c $< -o $@

build/yylex.c: src/scanner.l build
	flex --outfile=$@ $<

build/yyparse.h build/yyparse.c&: src/parser.y build
	bison -Wall -v --defines=build/yyparse.h --output=build/yyparse.c $<

clean:
	make -C badlib/ clean
	rm -f ${OBJFILES} ${GENSRC:%=build/%} ${GENHDR:%=build/%} build/yyparse.output

ci: 
	git add ${HDR:%=src/%} ${SRC:%=src/%} ${MKFILE} README.md doc/*.md .gitignore .gitmodules \
		src/parser.y src/scanner.l badlib

format:
	clang-format --style=Google -i ${SRC} ${HDR} parser.y
