/* CMSC 16200 - Lab 2
 * File: array_util.h
 */

#ifndef _ARRAY_UTIL_H_
#define _ARRAY_UTIL_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Linear search
int linear_search(const char *s, const char **A, int n);

// Binary search
int binary_search(const char *s, const char **A, int n);

#endif
