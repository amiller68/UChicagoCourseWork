/* CMSC 16200 - lab 3
 * File: english.c 
 * More library functions for strings
 */

#include "lib/english.h"
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int word_cmp(word_t w1, word_t w2){
  return strcmp((char *)w1, (char *)w2);
}

void print_sentence(word_t *text, int length) {
  if (text == NULL) {
    fprintf(stderr, "cannot print sentence.\n");
    exit(1);
  }
  for (int i = 0; i < length; i++) {
    if (text[i] == NULL) {
      fprintf(stderr, "cannot print sentence.\n");
      exit(1);
    }
    printf("%s ", text[i]);
  }
  printf("\n");
  return;
}
