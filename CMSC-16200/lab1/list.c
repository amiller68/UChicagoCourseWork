/* CMSC 16200 - Lab 1
 * File: list.c 
 * Implementation of linked list and its operations 
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdio.h>
#include <stdlib.h>
#include "list.h"

void print_list(linked_list *L) {
    if (L == NULL) return;
    if (L->head == NULL || L->length == 0) {
        printf("Linked list empty.\n");
        return;
    }
    list *iterator = NULL;
    printf("Linked list (len = %d): < ", L->length);
    for (iterator = L->head; iterator != NULL; iterator = iterator->next) {
        printf("%d ", iterator->data);
    }
    printf(">\n");
}

list* traverse(linked_list *S, int pos){
    list* pntr = S->head;
    for(int i=0; i<pos; i++) pntr=pntr->next;
    return (pntr);
}

linked_list *empty() {
	linked_list *L=malloc(sizeof(linked_list));
	if(L==NULL){printf("Allocation Failure"); return NULL;}
	L->length=0;
	L->head=NULL;
	return L;
}

linked_list *singleton(int data) {
    linked_list *L = empty();
	list *l = malloc(sizeof(list));
	if(l==NULL){printf("Allocation Failure"); return NULL;}
	l->data=data;
	l->next=NULL;
	L->length=1;
	L->head = l;
	return L;
}

void append(linked_list *L, int data) {
	if(L==NULL){printf("Invlaid list\n"); return;}
	list *new = malloc(sizeof(list));
	if(new==NULL){printf("Allocation Failure"); return;}
	new->next = NULL;
	new->data = data;
	if(L->length==0) L->head=new;
	else traverse(L,L->length-1)->next = new;
   	L->length++;
	return;
}

void cons(int data, linked_list *L) {
    if(L==NULL){printf("Invlaid list\n"); return;}
	list *new = malloc(sizeof(list));
    if(new==NULL){printf("Allocation Failure"); return;}
	new->data=data;
	new->next=L->head;
	L->head=new;
	L->length++;
	return;
}

int pop(linked_list *L) {
	if(L==NULL||L->head==NULL){
		printf("Invalid or empty list.\n");
		return 1;
	}
    list* ref = L->head;
	L->head = ref->next;
	int val = ref->data;
	free(ref);
	return val;
}

void free_list(linked_list *L) {
	if(L==NULL){printf("Invlaid list\n"); return;}
	list* pntr=L->head;
	while(pntr!=NULL){
		list* dlt=pntr;
		pntr=pntr->next;
		free(dlt);
	}
	free(L);
    return;
}
