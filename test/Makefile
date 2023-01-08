SRC := $(shell ls | grep -e '.*\.c')
ASM = ${SRC:.c=.asm}

.PHONY: all build clean build_clean ci build_ci

all:
	./run_tests.sh build

build: ${ASM}

%.asm : %.c FORCE
	@${CC} $< 1>build.out 2>error.out

clean:
	./run_tests.sh build_clean

build_clean:
	@rm -f *.str *.ast *.tok *.il *.err *.sym *.asm *.out

ci:
	git add run_tests.sh Makefile
	./run_tests.sh build_ci

build_ci:
	@git add ${SRC}

FORCE:
