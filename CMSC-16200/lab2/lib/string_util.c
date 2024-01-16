/* CMSC 16200 - Lab 2
 * File: string_util.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "array_util.h"

// Look up dictionary
bool is_word(char *s){
    int WORDMAXLINES = 109585;
    int WORDBUFSIZ = 50;
    int i = 0;

    FILE *fp = fopen("dictionary", "r");
    if (fp == 0) {
        fp = fopen("lib/dictionary", "r");
        if (fp == 0) {
            fprintf(stderr, "failed to open dictionary\n");
            exit(1);
        }
    }

    char **lines = malloc(WORDMAXLINES * sizeof(char*));
    for (int j = 0; j < WORDMAXLINES; ++j) {
        lines[j] = (char *)malloc(WORDBUFSIZ+1);
    }

    while (i < WORDMAXLINES && fgets(lines[i], 50, fp))
    {
        lines[i][strlen(lines[i])-1] = '\0';
        i = i + 1;
    }
    fclose(fp);

    int r = binary_search(s, (const char **)lines, i);
    // printf("isWord: %d\n", r);
    for (int j = 0; j < WORDMAXLINES; ++j) {
        free(lines[j]);
    }
    free(lines);
    return (-1 != r);
}

// Extract substring
char *substring(char *dest, char *src, int i, int len) {
    memcpy( dest, &src[i], len );
    dest[len] = '\0';
    return dest;
}

// int main(void) {
//  char *s = "hello";
//  bool r = is_word(s);
//  printf(r ? "true\n" : "false\n");
//  return 42;
// }
