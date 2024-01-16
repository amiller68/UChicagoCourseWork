/* CMSC 16200 - Lab 2
 * File: trending.h
 */

#ifndef _TRENDING_H_
#define _TRENDING_H_

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

int count_frequencies(const char **words, int *frequencies, int n, 
                      const char **feed, int feedlength, bool fast);

void top_three(int *frequencies, int *result, int n);

#endif
