/* CMSC 16200 - lab 3
 * File: decrypt.h
 * Header file for decryption
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "english.h"
#include "hashtable.h"

#ifndef _DECRYPT_H_
#define _DECRYPT_H_


void decipher_word(word_t *A, hashtable* ht, int wordcount,
                   word_t key, word_t cipher, word_t plain);
// (optional)

void decrypt_msg(word_t *A, hashtable* ht, int wordcount,
                 word_t *key_sentence, int key_len, 
                 word_t *cipher_text, word_t *plain_text, int txt_len) ;

#endif
