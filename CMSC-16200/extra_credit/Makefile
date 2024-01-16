CCBIN=/usr/bin/gcc
CC=$(CCBIN) -Wall -Wextra -std=c99 -pedantic -g
C_LIBS=lib/legal_position.h lib/climb.h
#C_PNG_LIBS=-I/usr/local/include -L/usr/local/lib

default: climb

legal_position: lib/*.h legal_position.c legal_position_test.c
	$(CC) -O3 -g -o legal_position legal_position.c legal_position_test.c -lm


climb: $(C_LIBS) climb.c wall.c legal_position.c
	$(CC) -O3 -g -o climb climb.c wall.c legal_position.c -lm


.PHONY: clean
clean:
	 rm -Rf *.o climb legal_position *.dSYM

.PHONY: package
package:
	tar -cvzf Climber-handin.tgz legal_position.c legal_position.h legal_position_test.c climb.c climb.h climb_test.c

