/* CMSC 16200 - Lab 1
 * File: recursion.c
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "list.h"
#include "recursion.h"

double power(int x, int y) {
	double hold = x;
	if(y==0) return 1.0;
    else if(y>0) return (hold * power(x, y-1));
	else return (power(x,y+1)/hold);
}

void sum(list *l, int carry){
	if(l==NULL) return;
	l->data+=carry;
	sum(l->next, l->data);
}


void prefixSum(linked_list *L) {
	if(L==NULL) {printf("Invalid List\n"); return;}
	sum(L->head, 0);    
	return;
}

bool match(list* l, int target){
	if(target==0) return true;
	if(l==NULL) return false;
	return match(l->next, target) || match(l->next, target-l->data);
}
bool subsetSum(linked_list *S, int n) {
	return match(S->head, n);
}

