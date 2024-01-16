#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "lib/duplicates.h"

int main(void) {
    // Testing number_distinct
    printf("%s\n", "Testing number_distinct ...");
    const char **a = calloc(3, sizeof(char*)); // create an array of strings on heap
    a[0] = "hello";
    a[1] = "world";
    a[2] = "world";
	const char **b = calloc(9, sizeof(char*));
	b[0] = "apple";
	b[1] = "apple";
	b[2] = "beets";
	b[3] = "candy";
	b[4] = "hungry";
	b[5] = "hungry";
	b[6] = "mmm";
	b[7] = "mmm";
	b[8] = "egg";
	assert(6 == number_distinct(b, 9));
	assert(5 == number_distinct(b, 8));
	assert(4 == number_distinct(b, 5));
    assert(2 == number_distinct(a, 3));
    free(a);
    
    // Add your own tests here.

    printf("%s\n", "Passed all tests!");
	

    // Testing indices_distinct
    printf("%s\n", "Testing indices_distinct ...");
    // Add your own tests here.
 
	int *result = malloc(number_distinct(b, 9)*sizeof(int));
	indices_distinct(b, result, 9);
	printf("Array: { "); for(int i=0; i<9; i++) printf("%s ", b[i]); printf("}\n");
	printf("Dsitinct Indicies: { "); for(int i=0; i<6; i++) printf("%d ", result[i]); printf("}\n");
    
	printf("Testing sub array");
	int *result2 = malloc(number_distinct(b, 6)*sizeof(int));
    indices_distinct(b, result2, 6);
    printf("Array: { "); for(int i=0; i<6; i++) printf("%s ", b[i]); printf("}\n");
    printf("Dsitinct Indicies: { "); for(int i=0; i<number_distinct(b, 6); i++) printf("%d ", result2[i]); printf("}\n");
	printf("%s\n\n", "Passed all tests!");


    // Testing remove_duplicates
    printf("%s\n", "Testing remove_duplicates ...");
    printf("Array: { "); for(int i=0; i<9; i++) printf("%s ", b[i]); printf("}\n");
	char **remove = remove_duplicates(b, 9);
    printf("Unique Entries: { "); for(int i = 0; i<number_distinct(b, 9); i++) printf("%s ", remove[i]); printf("}\n");	
    printf("%s\n", "Passed all tests!");
    return 0;
}
