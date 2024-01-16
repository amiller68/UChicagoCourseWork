CCBIN=/usr/bin/gcc
CC=$(CCBIN) -Wall -Wextra -std=c99 -pedantic -g

default: vigenere

vigenere:  lib/*.h english.c hashtable.c encrypt.c decrypt.c vigenere_test.c
	$(CC) -O3 -o vigenere  english.c hashtable.c encrypt.c decrypt.c vigenere_test.c

hashtable:  lib/*.h hashtable.c hashtable_test.c english.c
	$(CC) -O3 -o hashtable  hashtable.c hashtable_test.c english.c

.PHONY: clean
clean:
	rm -Rf *.o lib/*.o vigenere *.dSYM hashtable

.PHONY: package
package:
	tar -cvzf lab3-handin.tgz *
