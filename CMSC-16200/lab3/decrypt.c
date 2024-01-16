/* CMSC 16200 - lab 3
 * File: decrypt.c 
 * Implementation of decryption
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib/english.h"
#include "lib/decrypt.h"
#include "lib/hashtable.h"

void decipher_word(word_t *A, hashtable* ht, int wordcount,
                   word_t key, word_t cipher, word_t plain) {
	word_t use = A[1];
	int eh =(int) get(ht, use);
	eh = wordcount;
	use = key;
	eh++;
	use = cipher;
	use = plain;
	return; 
}

void decrypt_msg(word_t *A, hashtable* ht, int wordcount,
                 word_t *key_sentence, int key_len, 
                 word_t *cipher_text, word_t *plain_text, int txt_len) {

	for(int i=0; i<txt_len; i++){
		int key_index = i % key_len;
		//Find Key Val, check for error
		keyType keyToKey = (keyType) key_sentence[key_index];
    	int key_val = (int) get(ht, keyToKey);
		if(key_val < 0){ printf("Invalid Key"); return; }
    	//Find Cipher_Text  Val, check for error
		keyType cipherToKey = (keyType) cipher_text[i];
    	int cipher_val = (int) get(ht, cipherToKey);
		if(cipher_val < 0){ printf("Invlaid Text"); return; }
    	//Assign new value to cipher_text
		int new_index = (wordcount + (cipher_val - key_val)) % wordcount;
		plain_text[i] = A[new_index];
	}
	return;
}

