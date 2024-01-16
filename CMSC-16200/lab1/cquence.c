/* CMSC 16200 - Lab 1
 * File: cquence.c
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>
#include <assert.h>
#include "dna.h"
#include "cquence.h"
#define MIN(a,b) (((a)<(b))?(a):(b))


//copies a block of nodes into memory and deletes route strand
seq* copy(strand *template){
	strand *copy = empty_strand();
	int len = template->length;
	for(int i=len-1; i>=0; i--) cons_strand(traverse(template, i)->data, copy);
	seq* head = copy->head;
	free(copy);
	return head;
}

strand* copy_strand(strand *template){
    strand *copy = empty_strand();
    int len = template->length;
    for(int i=len-1; i>=0; i--) cons_strand(traverse(template, i)->data, copy);
    return copy;
}



strand *join(strand *S1, strand *S2) {
	if(S1==NULL & S2==NULL) return NULL;
	else if(S1==NULL) return copy_strand(S2);
	else if(S2==NULL) return copy_strand(S1);
	strand* base = copy_strand(S1);
	seq* tail = copy(S2);
	seq* connect = traverse(base, base->length-1);
	connect->next = tail;
	return base;
}

void cross(seq *first, seq* second){
	if(first->next==NULL){
		first->next = second;
		return;
	}
	else if(second->next==NULL){
		seq* hold = first->next;
		first->next = second;
		second->next = hold;
		return;	
	}
	seq* hold = first->next;
	seq* carry = second->next;
	first->next=second;
	second->next=hold;
	cross(hold, carry);
}

strand *interleave(strand *S1, strand *S2) {
	if(S1==NULL & S2==NULL) return NULL;
    else if(S1==NULL) return copy_strand(S2);
    else if(S2==NULL) return copy_strand(S1);
	strand *New = empty_strand();
	seq* cp1 = copy(S1);
	seq* cp2 = copy(S2);
	cross(cp1, cp2);
	New->head=cp1;
	New->length=S1->length+S2->length;
	return New;
}

int inject(strand *S, strand *Sub, int pos) {
	if(S==NULL || pos<0 || pos>S->length) return 1;
    else if(Sub==NULL||Sub->head==NULL) return 0;
	seq* start = copy(Sub);
	seq* end = start;
	while(end->next!=NULL) end = end->next;
	if(pos==0){
		end->next = S->head;
		S->head = start;
   }
	else{
		end->next = traverse(S, pos);
		traverse(S,pos-1)->next = start;
	}
	S->length+=Sub->length;
	return 0;
}

bool isPrefix(strand *S, strand *Sub) {
	int dom = S->length;
	int sub = Sub->length;
	if(dom<sub||S==NULL||Sub==NULL) return false;
	bool result=true;
	for(int i=0; i<sub; i++){
		char dom_char = traverse(S,i)->data;
		char sub_char = traverse(Sub,i)->data;
		if(dom_char!=sub_char){
			result=false;
			break;
		}
	}
    return result;
}

int find(strand *S, strand *Sub) {
    int dom = S->length;
    int sub = Sub->length;
	if(Sub->head==NULL) return 0;
    if(dom<sub||S==NULL||Sub==NULL) return -1;
	bool climax;
	int i;
	for(i=0; i<=dom-sub; i++){
		climax = true;
		for(int j=0; j<sub; j++){
			char dom_char=traverse(S,i+j)->data;
			char sub_char=traverse(Sub,j)->data;
			if(dom_char!=sub_char){
				climax=false;	
				break;
			}	
		}
		if(climax) break;
	}


    if(climax) return i;
	return -1;
}

//I was told I would get extra credit for circumcision jokes in my code
int snip(strand *S, strand *Sub) {
    int kvatter = find(S,Sub);
	int mazel_tov = 0;
	int goy = 1;
	if(kvatter<0) return goy;
	if(Sub->head==NULL) return mazel_tov;
	seq* mohel;
	if(kvatter==0){
		mohel = S->head;
		S->head = traverse(S,Sub->length);
	}
	else{
		seq* start = traverse(S, kvatter-1);
		mohel = traverse(S, kvatter-1)->next;
		start->next = traverse(S, kvatter+Sub->length);
	}
	for(int i=0; i<Sub->length; i++){
		seq* bris = mohel;
		mohel = mohel->next;
		free(bris);
	}	
	S->length-=Sub->length;
	return mazel_tov;
}
