export CWARN ?= -Wall -Wextra -Wpedantic -Wno-shadow -Wno-declaration-after-statement -Wno-cast-function-type -Wbad-function-cast -Wc90-c99-compat
export CFLAGS ?= -Isrc -Ibuild -Ibadlib -Ibadlib/murmur3 -ansi
WRAP_ATTRS := -Wl,--wrap=auxspec_destroy,--wrap=typespec_destroy,--wrap=create_erraux_v,--wrap=typespec_append_auxspecs,--wrap=typespec_append_aux,--wrap=typespec_prepend_aux,--wrap=typespec_init,--wrap=typespec_get_aux
WRAP_LYUTILS := -Wl,--wrap=parser_get_tname
WRAP_STRSET := -Wl,--wrap=string_set_intern
WRAP_SYMTABLE := -Wl,--wrap=symbol_table_destroy
WRAP_STATE := -WL,--wrap=state_get_label
MKFILE := Makefile
EXE := build/bompiler
SRC := debug.c bcc_err.c bcc_types.c strset.c lyutils.c astree.c symtable.c tchk_expr.c tchk_stmt.c tchk_decl.c asmgen.c state.c evaluate.c init.c regalloc.c instr.c bblock.c conversions.c main.c
HDR := debug.h bcc_err.h bcc_types.h strset.h lyutils.h astree.h symtable.h tchk_expr.h tchk_stmt.h tchk_decl.h asmgen.h state.h evaluate.h init.h regalloc.h instr.h bblock.h conversions.h simplestack.h
OBJ := ${SRC:.c=.o}
# unit test files
UNITS := build/astree_test
UNITSRC := ${UNITS:build/%=%.c}
UNITOBJ := ${UNITSRC:.c=.o}
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

.PHONY: all debug sanitize units badlib test clean test_clean ci format units spotless selftest selftest_clean

all: CFLAGS += -O1
all: ${EXE}

release: CFLAGS += -O2 -DNDEBUG -fsanitize=address,undefined -fstack-protector-all
release: ${EXE}

debug: CFLAGS += -O0 -pg -g -fsanitize=undefined -fstack-protector-all
debug: LDFLAGS += -Wl,-Map=./output.map
debug: ${EXE}

sanitize: CFLAGS += -O0 -pg -g -fsanitize=address -fsanitize=undefined -fstack-protector-all
sanitize: ${EXE}

units: CFLAGS += -O0 -pg -g
units: LDFLAGS += -lcmocka
units: export CPPFLAGS = -DUNIT_TESTING
units: ${UNITS}

${EXE}: ${OBJFILES}
	${CC} ${CFLAGS} ${LDFLAGS} ${CWARN} $^ -o $@

build/astree_test: LDFLAGS += ${WRAP_ATTRS} ${WRAP_SYMTABLE} ${WRAP_STRSET} ${WRAP_LYUTILS}
build/%_test: build/%.o build/%_test.o build/debug.o ${LIBOBJ:%=build/%}
	${CC} ${CFLAGS} ${CWARN} ${LDFLAGS} $^ -o $@

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

build/%.o: src/%.c ${HDRFILES} build
	${CC} ${CFLAGS} ${CPPFLAGS} ${CWARN} -c $< -o $@

build/yylex.c: src/lexer.l build
	flex --outfile=$@ $<

build/yyparse.h build/yyparse.c&: src/parser.y build
	bison -Wall -v --defines=build/yyparse.h --output=build/yyparse.c $<

format:
	clang-format --style=Google -i ${SRC:%=src/%} ${HDR:%=src/%} \
	${UNITSRC:%=src/%} src/parser.y src/parser.c

check:
	clang-format --style=Google --dry-run --Werror ${SRC:%=src/%} \
        ${HDR:%=src/%} ${UNITSRC:%=src/%} src/parser.y src/parser.c

ci: check
	git add ${HDR:%=src/%} ${SRC:%=src/%} ${UNITSRC:%=src/%} ${MKFILE} \
	README.md doc/*.md .gitignore .gitmodules \
	src/parser.y src/parser.c src/lexer.l badlib
	make -C test ci

test:
	@if [ -x "${EXE}" ]; then \
		echo "make -C test"; \
		make -C test; \
	else \
		echo "Please build an executable before running tests."; \
		false; \
	fi

selftest:
	@if [ -x "${EXE}" ]; then \
		echo "make -C selftest"; \
                env CC="$(readlink -f ./build/bompiler)" \
		make -C selftest; \
	else \
		echo "Please build an executable before running tests."; \
		false; \
	fi

clean:
	make -C badlib/ clean
	rm -f ${OBJFILES} ${UNITOBJ:%=build/%} ${GENSRC:%=build/%} \
	${GENHDR:%=build/%} build/yyparse.output

test_clean:
	make -C test/ clean

selftest_clean:
	make -C selftest/ clean

spotless: clean test_clean selftest_clean
	rm -f ${EXE} ${UNITS}
