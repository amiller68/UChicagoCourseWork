CCBIN=/usr/bin/gcc
CC=$(CCBIN) -Wall -Wextra -std=c99 -pedantic -g

default: cyclic

cyclic: cyclic.h cyclic.c cyclic_test.c
	$(CC) -O3 -g -o cyclic cyclic.c cyclic_test.c -lm

puzzles: puzzles.h puzzles.c puzzles_test.c
	$(CC) -O3 -o puzzles puzzles.c puzzles_test.c -lm

.PHONY: clean
clean:
	rm -Rf *.o cyclic puzzles *.dSYM

.PHONY: package
package:
	tar -cvzf lab0-handin.tgz cyclic.c cyclic.h cyclic_test.c puzzles.c puzzles.h puzzles_test.c
