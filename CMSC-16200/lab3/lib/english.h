/* CMSC 16200 - lab 3
 * File: english.h 
 * Header file for string library functions
 */

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#ifndef _ENGLISH_H_
#define _ENGLISH_H_

typedef char * word_t;

int word_cmp(word_t w1, word_t w2);

void print_sentence(word_t *text, int length);

#endif
