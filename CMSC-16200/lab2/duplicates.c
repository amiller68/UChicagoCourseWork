/* CMSC 16200 - Lab 2
 * File: duplicates.c
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib/duplicates.h"

int number_distinct(const char **words, int n) {
	int count = 1;
	int i = 0;
	while(i<n-1){
		if(words[i] != words[i+1]) count++; 
		i++;
	}
	return count; 
}

void indices_distinct(const char **words, int *result, int n) {
    int index = 0;
	for(int i=0; i<n; i++){
		result[index]=i;
		while(words[i] == words[i+1]  && i<n-1) i++;
		index++;
	}
	return; 
}

char **remove_duplicates(const char **words, int n) {
    int len = number_distinct(words, n);
	int *index = malloc(len*sizeof(int));
	if(index==NULL){ printf("Memory Allocation Failure"); return NULL; }
	indices_distinct(words, index, n);
	char **result = calloc(len, sizeof(char*));
	if(result==NULL){ printf("Memory Allocation Failure"); return NULL; }
	for(int i=0; i<len; i++) result[i] = (char *) words[index[i]];
	return result;
}

