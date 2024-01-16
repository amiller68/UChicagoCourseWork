#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "dna.h"
#include "cquence.h"

int main(void) {
    strand *a = string2strand("CGAT");
    print_seq(a);
    strand *b = string2strand("AGC");
    print_seq(b);
    strand *c = join(a,b);
    print_seq(c);
    strand *d = interleave(a,b);
    print_seq(d);
 	inject(a,b,0);
    print_seq(a);
	inject(a,b,5);	
    print_seq(a);
	assert(isPrefix(a,b));
	assert(!(isPrefix(a,c)));
	printf("prefix works\n");
    printf("%d\n",find(a,b));
	strand *test = string2strand("CCG");
	printf("%d\n",find(a, test));
	free_strand(test);
	strand *test1 = string2strand("AGCCGAGCAT");
	printf("%d\n", find(a, test1));
	strand *test2 = string2strand("AGCCGAGCATA");
    printf("%d\n", find(a, test2));
	strand *eh = string2strand("TA");
	printf("%d\n", find(a, eh));
	strand *test3 = string2strand("GC");
	printf("%d\n", find(a, test3));
    snip(a,b);
    print_seq(a);
    printf("%d\n",find(a,b));
    strand *test4 = string2strand("AG");
	printf("%d\n", snip(a, test4));
	print_seq(a);
	strand *test5=string2strand("T");
	snip(a,test5);
	print_seq(a);
	free_strand(a);
    free_strand(b);
    free_strand(c);
    free_strand(d);
    printf("Passed all tests!\n");
	return 0;
}
