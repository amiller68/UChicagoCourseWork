/* CMSC 16200 - lab 3
 * File: vigenere_test.c 
 * Test of Vigenere cipher
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "lib/english.h"
#include "lib/hashtable.h"
#include "lib/encrypt.h"
#include "lib/decrypt.h"

int init_word_array(word_t **words_array_ptr) {
    // Initialize Array.
    int WORDMAXLINES = 109585;
    int WORDBUFSIZ = 50;
    int WORDCNT = 0;
    int i = 0;
    FILE *fp = fopen("lib/words", "r");
    if (fp == 0) {
        fprintf(stderr, "failed to open words.\n");
        exit(1);
    }
    *words_array_ptr = NULL;
    *words_array_ptr = malloc(WORDMAXLINES * sizeof(word_t));
    if (*words_array_ptr == NULL){
        fprintf(stderr, "cannot allocate word array.\n");
        exit(1);
    } else{
        for (int j = 0; j < WORDMAXLINES; ++j) {
            (*words_array_ptr)[j] = (word_t)malloc(WORDBUFSIZ+1);
        }

        while (i < WORDMAXLINES && fgets((*words_array_ptr)[i], 50, fp)) {
            (*words_array_ptr)[i][strlen((*words_array_ptr)[i])-1] = '\0';
            i = i + 1;
            WORDCNT = WORDCNT + 1;
        }

        for (int k = WORDCNT; k < WORDMAXLINES; ++k) {
            free((*words_array_ptr)[k]);
        }
    }
    fclose(fp);
    return WORDCNT;
}

void free_word_array(word_t *words_array, int n) {
    // Free Array.
    for (int j = 0; j < n; ++j) {
        free(words_array[j]);
    }
    free(words_array);
}

hashtable *init_word_ht(word_t *words_array, int wordcount) {

    // Initialize Hash Table.
    if (words_array == NULL || wordcount == 0){
        fprintf(stderr, "cannot allocate word hash table.\n");
        exit(1);
    }
    int HTCAPACITY = wordcount;
    hashtable* ht = NULL;
    ht = allocate((size_t)HTCAPACITY);
    if (ht == NULL) {
        fprintf(stderr, "cannot allocate word hash table.\n");
        exit(1);
    }
    for (int i = 0; i < wordcount; ++i) {
        put(ht, words_array[i], i);
    }
    return ht;
}

void free_word_ht(hashtable* words_ht) {
    // Free Hash Table.
    deallocate(words_ht);
}


int main(void) {

    /*******************************************
     * Build your words_array and hash_table here!
     *******************************************/

    word_t *words_array;
    int wordcount = init_word_array(&words_array);
    hashtable *words_ht = init_word_ht(words_array, wordcount);

	word_t test_array[] = {(word_t)"1", (word_t)"02", (word_t)"003", (word_t)"0004", (word_t)"00005", (word_t)"000006"};
	int test_count = 6;
	hashtable* test_ht = allocate(6); 
	for(int i=0; i<test_count; i++) put(test_ht, test_array[i], i);
	print_table(test_ht);
    /*******************************************
     * Define your Vigenere key-sentence here!
     *******************************************/
   
	int key_length = 3;
    word_t key_sentence[key_length];
    key_sentence[0] = (word_t)"hello";
    key_sentence[1] = (word_t)"beautiful";
    key_sentence[2] = (word_t)"world";
/*
	int testKey_len = 3;
	word_t testKey_sentence[testKey_len];
	testKey_sentence[0] = (word_t)"00005";
	testKey_sentence[1] = (word_t)"1";
	testKey_sentence[2] = (word_t)"0004";
*/
    /*******************************************
     * Test your encryption and decryption below!
     *******************************************/
    /* Begin sample test */
    
	int msg_length = 5;
    word_t plain_msg[msg_length];
    plain_msg[0] = (word_t)"hi";
    plain_msg[1] = (word_t)"my";
    plain_msg[2] = (word_t)"password";
    plain_msg[3] = (word_t)"is";
    plain_msg[4] = (word_t)"no";
    word_t *cipher_msg = malloc(msg_length * sizeof(word_t));
    for (int i = 0; i < msg_length; i++) {
        cipher_msg[i] = (word_t)malloc(50 * sizeof(char));
    }
	/*
	int msg_length = 5;
    word_t plain_msg[msg_length];
    plain_msg[0] = (word_t)"02";
    plain_msg[1] = (word_t)"1";
    plain_msg[2] = (word_t)"000006";
    plain_msg[3] = (word_t)"003";
    plain_msg[4] = (word_t)"0004";
    word_t *cipher_msg = malloc(msg_length * sizeof(word_t));
    for (int i = 0; i < msg_length; i++) {
        cipher_msg[i] = (word_t)malloc(50 * sizeof(char));
    }
	*/
    encrypt_msg(words_array, words_ht, wordcount, key_sentence, key_length, plain_msg, cipher_msg, msg_length);
//  encrypt_msg(test_array, test_ht, test_count, testKey_sentence, testKey_len, plain_msg, cipher_msg, msg_length);
	printf("\nKey sentence: \n");
    print_sentence(key_sentence, key_length);
	printf("Plain message: \n");
    print_sentence(plain_msg, msg_length);
    printf("Encrypted message: \n");
    print_sentence(cipher_msg, msg_length);

    word_t *new_plain_msg = malloc(msg_length * sizeof(word_t));
    for (int i = 0; i < msg_length; i++) {
        new_plain_msg[i] = (word_t)malloc(50 * sizeof(char));
    }

    decrypt_msg(words_array, words_ht, wordcount, key_sentence, key_length, cipher_msg, new_plain_msg, msg_length);
    
//    decrypt_msg(test_array, test_ht, test_count, testKey_sentence, testKey_len, cipher_msg, new_plain_msg, msg_length);
    
		
	printf("\nKey sentence: \n");
	print_sentence(key_sentence, key_length);
	printf("Encrypted  message: \n");
    print_sentence(cipher_msg, msg_length);
	printf("Decrypted message: \n");
    print_sentence(new_plain_msg, msg_length);
	
	free_word_array(new_plain_msg, msg_length);
    free_word_array(cipher_msg, msg_length);
    /* End sample test */

    /* Begin your own tests*/

    /* End your own tests*/

    /*******************************************
     * Free your words_array and hash_table here!
     *******************************************/
    free_word_array(words_array, wordcount);
    free_word_ht(words_ht);
    return 0;
}
