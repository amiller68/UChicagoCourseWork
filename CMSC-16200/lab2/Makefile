CCBIN=/usr/bin/gcc
CC=$(CCBIN) -Wall -Wextra -std=c99 -pedantic -g
C_LIBS=lib/array_util.c lib/string_util.c

default: validity

duplicates: $(C_LIBS) lib/*.h duplicates.c duplicates_test.c
	$(CC) -O3 -o duplicates $(C_LIBS) duplicates.c duplicates_test.c

trending: $(C_LIBS) lib/*.h trending.c trending_test.c
	$(CC) -O3 -o trending $(C_LIBS) trending.c trending_test.c

validity: $(C_LIBS) lib/*.h validity.c validity_test.c
	$(CC) -O3 -o validity $(C_LIBS) validity.c validity_test.c

.PHONY: clean
clean:
	rm -Rf *.o lib/*.o duplicates trending validity *.dSYM

.PHONY: package
package:
	tar -cvzf lab2-handin.tgz duplicates.c trending.c validity.c
