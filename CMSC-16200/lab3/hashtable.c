/* CMSC 16200 - lab 3
 * File: hashtable.c 
 * Implementation of hash table
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include "lib/hashtable.h"
#include "lib/english.h"
#include <stdlib.h>
#include <stdio.h>

//work damnit
hashtable* allocate(int size){
	if(size<=0) return NULL;
	hashtable *ht = malloc(sizeof(hashtable));
	if(ht==NULL){ printf("Malloc Failure"); return NULL; }
	ht->size  = size;
	ht->table = malloc(size*sizeof(entry*));
	if(ht->table==NULL){ printf("Malloc Failure"); return NULL; }
	for(int i=0; i<size; i++) (ht->table)[i] = NULL;
	return ht;
}

int cons_entry(hashtable* ht, int index, keyType key, valType value){
	entry* hold = (ht->table)[index];
	entry *new = malloc(sizeof(entry));
	if(new == NULL){ printf("Malloc Fail"); return -1; }
	new->key = key;
	new->value = value;
	new->next = hold;
	(ht->table)[index] = new;
	return 0;
} 

int put(hashtable* ht, keyType key, valType value){
	int index = hash(key, ht->size);
	entry* ptr = (ht->table)[index];
	while(ptr != NULL){
		if(word_cmp(ptr->key, key) == 0){
			ptr->value += value;
			return 0;
		}
		ptr = ptr->next;
	}
	return cons_entry(ht, index, key, value);
}

valType get(hashtable* ht, keyType key){
	int index = hash(key, ht->size);  
	entry* ptr = (ht->table)[index];
	while(ptr != NULL){
		if(word_cmp(ptr->key, key) == 0) return ptr->value;
		ptr = ptr->next;
	}    
	return -1;
}

valType erase(hashtable* ht, keyType key){
    int index = hash(key, ht->size);
	entry* ptr = (ht->table)[index];
	if(ptr == NULL) return -1;
	entry* next = ptr->next; 
	if(word_cmp(ptr->key, key) == 0){
		(ht->table)[index] = next;
		valType value = ptr->value;
		free(ptr);
		return value; 
	}
	while(next != NULL){
		if(word_cmp(next->key, key) == 0){
			ptr->next = next->next;
			valType value = next->value;
			free(next);
			return value;
		}	
		ptr = next;
		next = ptr->next;
	}	
	return -1;
}

void free_list(entry* list){
	while(list!=NULL){
		entry* hold = list;
		list = list->next;
		free(hold);
	}
}

int deallocate(hashtable* ht){
    for(int i=0; i<ht->size; i++){
		free_list((ht->table)[i]);
	}
	return 0;
}

int hash(keyType key, int m){
    int len = strlen(key);
	int hash = 0;
	for(int i=0; i<len; i++){
		hash += 1;
	}
	return hash%m;
}

void print_table(hashtable* ht){
	if(ht == NULL){ printf("Null Table\n"); return; }
	for(int i=0; i < (ht->size); i++){
		printf("{ ");
		entry* ptr = (ht->table)[i];
		if(ptr == NULL) printf(" NULL");
		else while(ptr != NULL){ printf("(K: %s | V: %d),  ", ptr->key, (int) ptr->value); ptr=ptr->next; }
		printf(" }\n");
	}
}
