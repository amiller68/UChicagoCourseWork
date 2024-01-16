/* CMSC 16200 - Lab 2
 * File: duplicates.h
 */

#ifndef _DUPLICATES_H_
#define _DUPLICATES_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int number_distinct(const char **words, int n);

void indices_distinct(const char **words, int *result, int n);

char **remove_duplicates(const char **words, int n);

#endif
