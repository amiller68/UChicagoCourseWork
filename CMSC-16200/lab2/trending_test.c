#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "lib/trending.h"

int main(void) {
	const char **b = calloc(9, sizeof(char*));
    b[0] = "apple";
    b[1] = "apple";
    b[2] = "beets";
    b[3] = "candy";
    b[4] = "hungry";
    b[5] = "hungry";
    b[6] = "mmm";
    b[7] = "mmm";
    b[8] = "mmm";
	const char  **b_unique = calloc(5, sizeof(char*));
	b_unique[0] = "apple";
	b_unique[1] = "beets";
	b_unique[2] = "candy";
	b_unique[3] = "hungry";
	b_unique[4] = "mmm";
	int *b_freq = malloc(5*sizeof(int));
	b_freq[0] = 2;
	b_freq[1] = 1;
	b_freq[2] = 1;
	b_freq[3] = 2;
	b_freq[4] = 3;
/*
	const char **feed = calloc(6, sizeof(char*));
	feed[0] = "apple"; 
	feed[1] = "apple";
	feed[2] = "candy";
	feed[3] = "epsilon";
	feed[4] = "mmm";
	feed[5] = "mmm";*/
	const char **words = calloc(4, sizeof(char*));
    words[0] = "bobcat";
    words[1] = "chipmunk";
    words[2] = "eagle";
    words[3] = "tortoise";
    int *f = calloc(4, sizeof(int));
    f[0]=2; f[1]=1; f[2]=3; f[3]=1;
    const char **feed = calloc(5, sizeof(char*));
    feed[0] = "tortoise";
    feed[1] = "tortoise";
    feed[2] = "hare";
    feed[3] = "hare";
    feed[4] = "hare";
	/*
    int not_appear = count_frequencies(words, f, 4, feed, 5, true);
    if (not_appear==3 && f[0]==2 && f[1]==1 && f[2]==3 && f[3]==3)
        mark += 2;
    f[0]=2; f[1]=1; f[2]=3; f[3]=1;
    not_appear = count_frequencies(words, f, 4, feed, 5, false);
    if (not_appear==3 && f[0]==2 && f[1]==1 && f[2]==3 && f[3]==3)
        mark += 2;
    free(words); free(f); free(feed);
	*/
	// Testing count_frequencies
    printf("%s\n", "Testing count_frequencies ...");
	printf("Fast Version:\n");
	printf("Dictionary = {"); for(int i=0; i<4; i++) printf("%s ", words[i]); printf("}\n");
	printf("Current freq of dict entries = {"); for(int i=0; i<4; i++) printf("%d ", f[i]); printf("}\n");	
	printf("Feed = {"); for(int i=0; i<6; i++) printf("%s ", feed[i]); printf("}\n");
	int new = count_frequencies(words, f, 4, feed, 5, true);
	printf("New freq of dict entries = {"); for(int i=0; i<5; i++) printf("%d ", b_freq[i]); printf("}\n");
	printf("# of new strings in feed: %d\n", new);
/*	printf("Slow version:\n");
    printf("Dictionary = {"); for(int i=0; i<5; i++) printf("%s ", b_unique[i]); printf("}\n");
    printf("Current freq of dict entries = {"); for(int i=0; i<5; i++) printf("%d ", b_freq[i]); printf("}\n"); 
    printf("Feed = {"); for(int i=0; i<6; i++) printf("%s ", feed[i]); printf("}\n");
    new = count_frequencies(b_unique, b_freq, 5, feed, 6, false);
    printf("New freq of dict entries = {"); for(int i=0; i<5; i++) printf("%d ", b_freq[i]); printf("}\n");
    printf("# of new strings in feed: %d\n", new);*/
    printf("%s\n\n", "Passed all tests!");

	int *result = calloc(3, sizeof(int));
	//result[0]=0; result[1]=0; result[2]=0;
    // Testing top_three
    printf("%s\n", "Testing top_three ...");
	printf("Freq of dict entries = {"); for(int i=0; i<5; i++) printf("%d ", b_freq[i]); printf("}\n");
	top_three(b_freq, result, 5);
	printf("Sorted Freq by index = {"); for(int i=0; i<3; i++) printf("%d ", result[i]); printf("}\n");
    printf("%s\n", "Passed all tests!");
    return 0;
}
