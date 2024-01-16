/* CMSC 16200 - Lab 2
 * File: string_util.h
 */

#ifndef _STRING_UTIL_H_
#define _STRING_UTIL_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "array_util.h"

// Look up dictionary
bool is_word(char *s);

// Extract substring
char *substring(char *dest, char *src, int i, int len);

#endif
