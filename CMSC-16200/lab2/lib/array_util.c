/* CMSC 16200 - Lab 2
 * File: array_util.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


// Linear search
int linear_search(const char *s, const char **A, int n) {
    // Assume A is sorted
    for (int i = 0; i < n; i++) {
        int cmp = strcmp(A[i], s);
        if (cmp == 0) {
            return i;
        } else if (cmp > 0) {
            //A[i] > s
            return -1;
        }
        //A[i] < s
    }
    return -1;
}

// Binary search
int binary_search(const char *s, const char **A, int n){
    int lower = 0;
    int upper = n;
    while (lower < upper) { 
        int mid = lower + (upper-lower)/2;//(lower + upper)/2 could overflow
        // printf("mid:%d, %s\n", mid, A[mid]);
        int cmp = strcmp(A[mid], s);
        if (cmp == 0) {
            return mid;
        } else if (cmp > 0) { 
          //A[mid] > s
          upper = mid;
        } else {
          //A[mid] < s
          lower = mid + 1;
        }
    }
    return -1;
}

