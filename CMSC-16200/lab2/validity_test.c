#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "lib/validity.h"


int main(void) {
    // Testing sentence2hashtag
    printf("%s\n", "Testing sentence2hashtag ...");
	char hash[] = "I'm stupiD";
	printf("Original sentence: %s\n", hash);
	sentence2hashtag(hash);
	printf("Resulting Hash tag: #%s\n", hash);
    printf("%s\n", "Passed all tests!");
	printf("\n");    

    // Testing is_valid_hashtag
    printf("%s\n", "Testing is_valid_hashtag ...");
	char test[] = "iamverystupid";
	char empty[] = "";
	char negative[] = "iamiamverycoolblarghskqfwrdsnfkladnflksnfdjfhklafhkldslflkajadkshfsbsfwssdfsdfsdasfaatbjkdz";
	assert(is_valid_hashtag(empty));
	assert(is_valid_hashtag(test));
	assert(!(is_valid_hashtag(negative)));
    printf("%s\n", "PASSED ALL TESTS!");
    return 0;
}
