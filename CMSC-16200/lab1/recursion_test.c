#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "list.h"
#include "recursion.h"

int main(void) {
    printf("%lf\n", power(2, 7)); 
    printf("%lf\n", power(2, -7));
	linked_list *L = singleton(-7);
    cons(1,L);
    cons(-1,L);
    cons(-7,L);
    cons(9,L);
	cons(2, L);
    print_list(L);
	printf("%d\n", pop(L));
	print_list(L);
    prefixSum(L);
    print_list(L);
    assert(subsetSum(L, 4) == true);
	assert(subsetSum(L, 11) == true);
	assert(subsetSum(L, -3) == true);
	assert(subsetSum(L, 1) == true);
	assert(subsetSum(L, 5) == true);
	assert(subsetSum(L, 109) == false);
    free_list(L);
    printf("Passed all tests!\n");
    return 0;
}
