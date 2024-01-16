/* CMSC 16200 - Lab 2
 * File: validity.c
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include "lib/validity.h"
#include "lib/string_util.h"

void remove_char(char *sentence, int i){
	while(sentence[i]!='\0'){ 
		char hold = sentence[i+1];
		sentence[i]=hold;
		i++;
	}
	return;
}

void sentence2hashtag(char *sentence){
	int i = 0;
	while (sentence[i]!='\0'){
		if(isalpha(sentence[i])>0){
			if(isupper(sentence[i])>0) sentence[i]=tolower(sentence[i]);
			else i++;
		}
		else remove_char(sentence, i);
	}
	return;
}


bool is_valid_hashtag(char *hashtag) {
  	int len = strlen(hashtag);
	if(len==0) return true;
	bool result = false;
	for(int i=1; i<=len; i++){
		char *front = malloc((1+i)*sizeof(char));
		if(front==NULL) {printf("Memory Allocation Failure"); return false;}
		front = substring(front, hashtag, 0, i);
		char *back = malloc((1+len-i)*sizeof(char));
		if(back==NULL) {printf("Memory Allocation Failure"); return false;}
		back = substring(back, hashtag, i, len-i);
		if(is_word(front)) result = result || is_valid_hashtag(back);
		free(front); free(back);
	}
	return result; 
}
