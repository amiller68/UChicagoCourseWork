/* CMSC 16200 - Lab 1
 * File: list.h 
 * Interface for linked list and its operations 
 */

#ifndef _LIST_H_
#define _LIST_H_

struct node {
    int data;
    struct node* next;
};
typedef struct node list;

struct header {
    int length;
    list* head;
};
typedef struct header linked_list;

void print_list(linked_list *L);

linked_list *empty();

linked_list *singleton(int data);

void append(linked_list *L, int data);

void cons(int data, linked_list *L);

int pop(linked_list *L);

void free_list(linked_list *L);

#endif
