/* CMSC 16200 - Lab 1
 * File: cquence.h
 */

#ifndef _CQUENCE_H_
#define _CQUENCE_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "dna.h"

strand *join(strand *S1, strand *S2);

strand *interleave(strand *S1, strand *S2);

int inject(strand *S, strand *Sub, int pos);

bool isPrefix(strand *S, strand *Sub);

int find(strand *S, strand *Sub);

int snip(strand *S, strand *Sub);

#endif
