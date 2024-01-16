/* CMSC 16200 - Lab 1
 * File: dna.h
 */

#ifndef _DNA_H_
#define _DNA_H_

typedef struct dna_header strand;

typedef struct base_node seq;

void print_seq(strand *L);

seq* traverse(strand *S, int pos);

strand *empty_strand();

void cons_strand(char data, strand *S);

strand *string2strand(char *s);

void free_strand(strand *S);

struct base_node {
    char data;
    struct base_node* next;
};

struct dna_header {
    int length;
    seq* head;
};

#endif
