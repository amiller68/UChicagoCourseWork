/* CMSC 16200 - lab 3
 * File: hashtable.h
 * Header of hash table 
 */

#include "english.h"

#ifndef HASH_TABLE
#define HASH_TABLE  

typedef char* keyType;
typedef int valType;

typedef struct entry {
	keyType key;
	valType value;
	struct entry *next;
} entry;

typedef struct hashtable {
	int size;
	entry **table;
} hashtable;

hashtable* allocate(int size);
int put(hashtable* ht, keyType key, valType value);
valType get(hashtable* ht, keyType key);
valType erase(hashtable* ht, keyType key);
int deallocate(hashtable* ht);
int hash(keyType key, int m);
void print_table(hashtable *ht);
#endif
