CC ?= gcc
CWARN ?= -Wall -Wextra -Wpedantic -Wshadow -Wno-declaration-after-statement
CFLAGS ?= -Isrc -Ibuild -Ibadlib -Ibadlib/murmur3 -ansi
WRAP_ATTRS = -Wl,--wrap=auxspec_destroy,--wrap=typespec_destroy
WRAP_LIB = -Wl,--wrap=llist_destroy,--wrap=llist_pop_front,--wrap=llist_find,--wrap=llist_extract,--wrap=llist_get,--wrap=llist_size,--wrap=llist_insert,--wrap=llist_push_back,--wrap=llist_init
WRAP_LYUTILS = -Wl,--wrap=parser_get_tname
WRAP_STRSET = -Wl,--wrap=string_set_intern
WRAP_SYMTABLE = -Wl,--wrap=symbol_table_destroy
WRAP_ALL = ${WRAP_ATTRS} ${WRAP_LIB} ${WRAP_LYUTILS} ${WRAP_STRSET} ${WRAP_SYMTABLE}
MKFILE = Makefile
EXE = build/bompiler
TESTS = build/test_astree
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

.PHONY: all badlib build debug test clean ci format

all: CFLAGS += -O1
all: ${EXE}

debug: CFLAGS += -Og -pg -ggdb -fsanitize=address
debug: LIB_TARGET = debug
debug: ${EXE}

test: CFLAGS += -Itest -Og -pg -ggdb
test: ${TESTS}

${EXE}: ${OBJFILES}
	${CC} ${CFLAGS} ${CWARN} $^ -o $@

build/test_symtable: CFLAGS += -Itest -Og -pg -ggdb
# hack: relies on that fact that we don't issue warning on test code
build/test_symtable: CWARN += -DUNIT_TESTING -DNDEBUG
build/test_symtable: build/symtable.o ${LIBOBJ:%=build/%}
	${CC} ${CFLAGS} -c ${@:build/%=test/%.c} -o ${@:%=%.o}
	${CC} ${CFLAGS} -lcmocka -Wl,--wrap=typespec_destroy,--wrap=typespec_init,--wrap=state_get_label ${@:%=%.o} $^ -o $@

build/test_%: build/test_%.o build/debug.o
	${CC} ${CFLAGS} ${CWARN} -DUNIT_TESTING -c ${@:build/test_%=src/%.c} -o ${@:build/test_%=build/%.o}
	${CC} ${CFLAGS} ${WRAP_ALL} -lcmocka ${@:build/test_%=build/%.o} $^ -o $@

build:
	[ -d build ] || mkdir build

badlib:
	git submodule update --init --recursive badlib

build/yylex.o: build/yylex.c
	${CC} ${CFLAGS} ${CWARN} -c $^ -o $@

build/yyparse.o: build/yyparse.c build/yyparse.h
	${CC} ${CFLAGS} ${CWARN} -c $< -o $@

${LIBOBJ:%=build/%}: badlib build
	make -C badlib/ ${LIB_TARGET}
	cp ${@:build/%=badlib/%} ./build/

${LIBHDR}: badlib

build/test_%.o: test/test_%.c ${HDRFILES} build
	${CC} ${CFLAGS} -c $< -o $@

build/%.o: src/%.c ${HDRFILES} build
	${CC} ${CFLAGS} ${CWARN} -c $< -o $@

build/yylex.c: src/scanner.l build
	flex --outfile=$@ $<

build/yyparse.h build/yyparse.c&: src/parser.y build
	bison -Wall -v --defines=build/yyparse.h --output=build/yyparse.c $<

clean:
	make -C badlib/ clean
	rm -f ${OBJFILES} ${OBJFILES:build/%.o=build/test_%.o} ${GENSRC:%=build/%} ${GENHDR:%=build/%} build/yyparse.output

ci: 
	git add ${HDR:%=src/%} ${SRC:%=src/%} test/wrap_badllist.c test/test_astree.c test/test_symtable.c ${MKFILE} README.md doc/*.md .gitignore .gitmodules \
		src/parser.y src/scanner.l badlib

format:
	clang-format --style=Google -i ${SRC} ${HDR} parser.y
