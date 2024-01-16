CCBIN=/usr/bin/gcc
CC=$(CCBIN) -Wall -Wextra -std=c99 -pedantic -g -fsanitize=address
C_LIBS=lib/image_util.c lib/colors.c
#C_PNG_LIBS=-I/usr/local/include -L/usr/local/lib
C_PNG_LIBS=
LINK_LIBS= -lpng

default: segment

checklib: $(C_LIBS) lib/*.h pixel.c libpng_check.c
	$(CC) -O3 -o checklib $(C_PNG_LIBS) $(C_LIBS) pixel.c libpng_check.c $(LINK_LIBS)

unionfind: $(C_LIBS) lib/*.h pixel.c graph.c unionfind.c unionfind_test.c
	$(CC) -O3 -o unionfind $(C_PNG_LIBS) $(C_LIBS) pixel.c graph.c unionfind.c unionfind_test.c $(LINK_LIBS)

segment: $(C_LIBS) lib/*.h pixel.c graph.c unionfind.c segment.c segment_test.c
	$(CC) -O3 -o segment $(C_PNG_LIBS) $(C_LIBS) pixel.c graph.c unionfind.c segment.c segment_test.c $(LINK_LIBS)

.PHONY: clean
clean:
	rm -Rf *.o lib/*.o img/*.html checklib unionfind segment *.dSYM

.PHONY: package
package:
	tar -cvzf lab4-handin.tgz *
