/* CMSC 16200 - lab 3
 * File: hashtable_test.c 
 * Test of hash table
 */

#include "lib/hashtable.h"
#include <stdlib.h>
#include <stdio.h>


int main(void) {
	hashtable* ht = allocate(2);
  	print_table(ht);
	
	printf("Adding new entries\n\n");
	keyType key = "hello";
  	keyType key1 = "uhh";
	keyType key2 = "I";
	keyType key3 = "am";
	keyType key4 = "stupid";
	keyType key5 = "dorf";

	valType value1 = 1;
	valType value2 = 2;
	valType value3 = 3;

  	put(ht, key, value1);
	put(ht, key4, value3);
	put(ht, key2, value2);
	put(ht, key5, value2);
	put(ht, key1, value1);
	put(ht, key3, value3);
	
	printf("The Table:\n");
	print_table(ht);
	printf("\n");
	
	printf("Looking up %s value\n", key);
  	valType result = get(ht, key);
  	printf("The value of %s is %d\n\n", key, result);
	
	keyType wrong = "hehe";
	printf("Looking up %s value\n", wrong);
	(get(ht, wrong) < 0) ? printf("%s not in table\n\n", wrong) : printf("Something is wrong\n\n");

	printf("Erasing %s from table\n", key4);
	(erase(ht, key4) > 0) ?  printf("Success!\n\n") : printf("Something is wrong");

	printf("The Table: \n");
	print_table(ht);
	printf("\n");

	printf("Erasing %s from table\n", wrong);
  	(erase(ht, wrong) < 0) ?  printf("It failed but it should fail\n\n") : printf("Somehting is wrong\n\n");

	printf("The Table: \n");
	print_table(ht);

  	deallocate(ht);
  	printf("All tests have been successfully passed.\n");
  	return 0;
}

