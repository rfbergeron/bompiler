export CWARN ?= -Wall -Wextra -Wpedantic -Wno-shadow -Wno-declaration-after-statement -Wno-unused -Wno-variadic-macros -Wno-cast-function-type
export CFLAGS ?= -Isrc -Ibuild -Ibadlib -Ibadlib/murmur3 -ansi
WRAP_ATTRS := -Wl,--wrap=auxspec_destroy,--wrap=typespec_destroy,--wrap=create_erraux_v,--wrap=typespec_append_auxspecs,--wrap=typespec_append_aux,--wrap=typespec_prepend_aux,--wrap=typespec_init,--wrap=typespec_get_aux
WRAP_LYUTILS := -Wl,--wrap=parser_get_tname
WRAP_STRSET := -Wl,--wrap=string_set_intern
WRAP_SYMTABLE := -Wl,--wrap=symbol_table_destroy
WRAP_STATE := -WL,--wrap=state_get_label
MKFILE := Makefile
EXE := build/bompiler
TESTS := build/test_astree
SRC := debug.c bcc_err.c attributes.c strset.c lyutils.c astree.c symtable.c tchk_common.c tchk_expr.c tchk_stmt.c tchk_decl.c asmgen.c state.c evaluate.c main.c
HDR := debug.h bcc_err.h attributes.h strset.h lyutils.h astree.h symtable.h tchk_common.h tchk_expr.h tchk_stmt.h tchk_decl.h asmgen.h state.h evaluate.h simplestack.h
OBJ := ${SRC:.c=.o}
# files generated by flex and bison
GENSRC := yyparse.c yylex.c
GENHDR := yyparse.h
GENOBJ := ${GENSRC:.c=.o}
# library files
LIBHDR := badlib/badmap.h badlib/badllist.h badlib/badalist.h
LIBOBJ := badmap.o badllist.o badalist.o murmur3.o
# convenient groups
HDRFILES := ${HDR:%=src/%} ${GENHDR:%=build/%} ${LIBHDR}
OBJFILES := ${OBJ:%=build/%} ${GENOBJ:%=build/%} ${LIBOBJ:%=build/%}

.PHONY: all badlib build debug test clean ci format

all: CFLAGS += -O1
all: ${EXE}

debug: CFLAGS += -Og -pg -ggdb
debug: ${EXE}

test: CFLAGS += -Itest -Og -pg -ggdb
test: LDFLAGS += -lcmocka
test: export CPPFLAGS = -DUNIT_TESTING
test: ${TESTS}

${EXE}: ${OBJFILES}
	${CC} ${CFLAGS} ${CWARN} $^ -o $@

build/test_astree: LDFLAGS += ${WRAP_ATTRS} ${WRAP_SYMTABLE} ${WRAP_STRSET} ${WRAP_LYUTILS}
build/test_%: build/%.o build/test_%.o build/debug.o ${LIBOBJ:%=build/%}
	${CC} ${CFLAGS} ${LDFLAGS} $^ -o $@

build:
	[ -d build ] || mkdir build

badlib:
	git submodule update --init --recursive badlib
	make -C badlib/ murmur3

build/yylex.o: build/yylex.c
	${CC} ${CFLAGS} -c $^ -o $@

build/yyparse.o: build/yyparse.c build/yyparse.h
	${CC} ${CFLAGS} -c $< -o $@

${LIBOBJ:%=build/%}: build
	make -e -C badlib/ ${@:build/%=%}
	mv ${@:build/%=badlib/%} ./build/

build/test_%.o: test/test_%.c ${HDRFILES} build
	${CC} ${CFLAGS} ${CPPFLAGS} ${CWARN} -c $< -o $@

build/%.o: src/%.c ${HDRFILES} build
	${CC} ${CFLAGS} ${CPPFLAGS} ${CWARN} -c $< -o $@

build/yylex.c: src/scanner.l build
	flex --outfile=$@ $<

build/yyparse.h build/yyparse.c&: src/parser.y build
	bison -Wall -v --defines=build/yyparse.h --output=build/yyparse.c $<

clean:
	make -C badlib/ clean
	rm -f ${OBJFILES} ${OBJFILES:build/%.o=build/test_%.o} ${GENSRC:%=build/%} ${GENHDR:%=build/%} build/yyparse.output

ci: 
	git add ${HDR:%=src/%} ${SRC:%=src/%} test/test_astree.c ${MKFILE} README.md doc/*.md .gitignore .gitmodules \
		src/parser.y src/scanner.l badlib

format:
	clang-format --style=Google -i ${SRC:%=src/%} ${HDR:%=src/%} test/test_astree.c src/parser.y
