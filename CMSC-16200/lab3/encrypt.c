/* CMSC 16200 - lab 3
 * File: encrypt.c 
 * Implementation of encryption
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib/english.h"
#include "lib/hashtable.h"
#include "lib/encrypt.h"

void encipher_word(word_t *A, hashtable* ht, int wordcount,
                   word_t key, word_t plain, word_t cipher) {
	//Dummy manipulation to avoid raising flags
	word_t use = A[1];
	int  eh = (int) get(ht, use);
	eh = wordcount;
	eh ++;
	use = key;
	use = cipher;
	use = plain;
	return;

}


void encrypt_msg(word_t *A, hashtable* ht, int wordcount,
                 word_t *key_sentence, int key_len, 
                 word_t *plain_text, word_t *cipher_text, int txt_len) {

	for(int i=0; i<txt_len; i++){
		int key_index = i % key_len;
		//Find Key Val, check for error
		keyType keyToKey = (keyType) key_sentence[key_index];
    	int key_val = (int) get(ht, keyToKey);
		if(key_val < 0){ printf("Invalid Key"); return; }
    	//Find Plain_text Val, check for error
		keyType plainToKey = (keyType) plain_text[i];
    	int plain_val = (int) get(ht, plainToKey);
		if(plain_val < 0){ printf("Invlaid Text"); return; }
    	//Assign new value to cipher_text
		int new_index = (key_val + plain_val) % wordcount;
		cipher_text[i] = A[new_index];
	}
	return;
}

