/* CMSC 16200 - Lab 1
 * File: dna.c
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dna.h"

void print_seq(strand *L) {
    if (L == NULL) return;
    if (L->head == NULL || L->length == 0) {
        printf("DNA seq empty.\n");
        return;
    }
    seq *iterator = NULL;
    printf("DNA seq (len = %d): < ", L->length);
    for (iterator = L->head; iterator != NULL; iterator = iterator->next) {
        printf("%c ", iterator->data);
    }
    printf(">\n");
}


//does not account for empty strands
//helper function that returns pntr of seq @ pos; indexed from 0
seq* traverse(strand *S, int pos){
    seq* pntr = S->head;
    for(int i=0; i<pos; i++) pntr=pntr->next;
    return (pntr);
}

strand *empty_strand() {
	strand *S = malloc(sizeof(strand));
	if(S==NULL){printf("Allocation Failure"); return NULL;}
	S -> length = 0;
	S -> head = NULL;
	return S;
}



void cons_strand(char data, strand *S) {
	seq *new  = malloc(sizeof(seq));
	if(new==NULL){printf("Allocation Failure"); return;}
	new->data = data;
	new->next=S->head;
	S->head=new;
	S->length++;
	return;
}

strand *string2strand(char *s) {
	strand *S = empty_strand();
	int len = strlen(s);
    for(int i=len-1; i>=0; i--) cons_strand(s[i], S);
	return S;
}

void free_strand(strand *S) {
	seq* pntr = S->head;
	free(S);
	while(pntr!=NULL){
		seq* ref = pntr;
		pntr=pntr->next;
		free(ref);
	}
	return;
}
