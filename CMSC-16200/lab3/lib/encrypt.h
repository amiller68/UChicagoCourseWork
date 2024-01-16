/* CMSC 16200 - lab 3
 * File: encrypt.h
 * Header file for encryption
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "english.h"
#include "hashtable.h"

#ifndef _ENCRYPT_H_
#define _ENCRYPT_H_


void encipher_word(word_t *A, hashtable* ht, int wordcount,
                   word_t key, word_t plain, word_t cipher);
// (optional)

void encrypt_msg(word_t *A, hashtable* ht, int wordcount,
                 word_t *key_sentence, int key_len, 
                 word_t *plain_text, word_t *cipher_text, int txt_len);

#endif
