CCBIN=/usr/bin/gcc
CC=$(CCBIN) -Wall -Wextra -std=c99 -pedantic -g

default: cquence

dna: dna.h dna.c dna_test.c
	$(CC) -O3 -o dna dna.c dna_test.c

cquence: dna.h dna.c cquence.c cquence_test.c
	$(CC) -O3 -o cquence dna.c cquence.c cquence_test.c

recursion: list.h list.c recursion.c recursion_test.c
	$(CC) -O3 -o recursion list.c recursion.c recursion_test.c

.PHONY: clean
clean:
	rm -Rf *.o dna cquence recursion *.dSYM

.PHONY: package
package:
	tar -cvzf lab1-handin.tgz dna.h dna.c cquence.c list.h list.c recursion.c
