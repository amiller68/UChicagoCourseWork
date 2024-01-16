/* CMSC 16200 - Lab 2
 * File: trending.c
 * 
 * Name: (YOUR NAME HERE)
 * CNet: (YOUR CNET ID HERE)
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "lib/trending.h"
#include "lib/array_util.h"

int fast_protocol(const char *word, const char **feed, int feedlength){
    int index = binary_search(word, feed, feedlength);
    if(index == -1) return 0;
    int upper = index;
    int lower = index;
    while(lower>0 && feed[lower-1] == feed[index]) lower--;
    while(upper<feedlength-1 && feed[upper+1] == feed[index]) upper++;
    return 1+(upper-lower);
}

int slow_protocol(const char *word, const char **feed, int feedlength){
	int index = linear_search(word, feed, feedlength);
	if(index == -1) return 0;
	int count = 1;
	while(index+count<feedlength && feed[index+count]==feed[index]) count++;
	return count;
}

int count_frequencies(const char **words, int *frequencies, int n, 
                      const char **feed, int feedlength, bool fast) {
 	int total = 0;
	for(int i=0; i<n; i++){
		int increment =
		(fast) ? fast_protocol(words[i], feed, feedlength) : slow_protocol(words[i], feed, feedlength);
		frequencies[i] += increment;
		total += increment;
	}
	return feedlength-total; 
}

void top_three(int *frequencies, int *result, int n) {
	for(int i=0; i<3; i++) result[i] = -1;
	for(int i=0; i<n; i++){
		for(int j=0; j<3; j++){
			if(result[j]==-1){result[j]=i; break;}
			else if(frequencies[i]>frequencies[result[j]]){
				for(int k=2; k>=j+1; k--) result[k] = result[k-1];
				result[j] = i;
				break;
			}
		}
	}
	return;
}
