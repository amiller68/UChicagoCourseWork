/* CMSC 16200 - Lab 0
 * File: puzzles.c
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>
#include <assert.h>
#include "puzzles.h"

/* Begin of Puzzle 1 */
/* VOL computes the product of its three arguments */
#define VOL(X,Y,Z) X*Y*Z

int volume() {
    int v = VOL((1+1),(2+2),(3+3));
    printf("volume = %d\n", v);
    return v;
}

int puzzle1() {
    volume();
    return 0;
}
/* End of Puzzle 1 */

/* Begin of Puzzle 2 */
/* swap function swaps the values of a and b */
void swap(int *x, int *y) {
    int temp;
    temp = *x;
    *x = *y;
    *y =  temp;
	return;
}

int puzzle2() {
    int a = 2;
    int b = 3;
    printf("Before: a = %d, b = %d\n", a, b);
    swap(&a, &b);
    printf("After : a = %d, b = %d\n", a, b);
    return 0;
}
/* End of Puzzle 2 */

/* Begin of Puzzle 3 */
//the add1 function can add one to the integer stored in any p without an error
void add1(int *p) {
   if(*p<INT_MAX){
	 printf("Before: %d\n", *p);
   	 *p = *p + 1;
   	 printf("After : %d\n", *p);
	}
   else printf("Int already at max size\n");
}

int puzzle3() {
    int* p = malloc(sizeof(int));
	add1(p);
    free(p);
    return 0;
}
/* End of Puzzle 3 */

/* Begin of Puzzle 4 */ 
/* test_safe_add function can add any two integers without error */
bool test_safe_add(int a, int b) {
    if (a > 0 & (b > INT_MAX - a)) {
        return false;
    }
    if (a < 0 & (b < INT_MIN - a)) {
        return false;
    }
    return true;
}

int puzzle4() {
    int a = INT_MIN;
    int b = 5;
    if (!test_safe_add(a,b)) {
        printf("Unsafe addition detected: %d + %d\n", a, b);
        return 1;
    } else {
        printf("%d + %d = %d\n", a, b, a + b);
        return 0;
    }
}
/* End of Puzzle 4 */

/* Begin of Puzzle 5 */
/* test_safe_div function divides any two integers without error */
bool test_safe_div(int a, int b) {
    return b != 0;
}

int puzzle5() {
    int a = 42;
    int b = 0;
    if (!test_safe_div(a,b)) {
        printf("Unsafe division detected: %d / %d\n", a, b);
        return 1;
    } else {
        printf("%d / %d = %d\n", a, b, a / b);
        return 0;
    }
}
/* End of Puzzle 5 */

/* Begin of Puzzle 6 */
/* set_vector function fills a length-n array with integer v */
void set_vector(int *vec, int n, int value) {
    for (int i = 0; i < n; i++) {
        vec[i] = value;
    }
    return;
}

int puzzle6() {
    int* vec_i = malloc(3*sizeof(int));
    int value_i = 1;
    if (vec_i == NULL) {
        printf("malloc failed.\n");
        return 1;
    } else {
        set_vector(vec_i, 3, value_i);
        printf("A vector of integers:\n");
		for(int i=0; i<3; i++){printf("%d\n", vec_i[i]);}
        free(vec_i);
    }
    return 0;
}
/* End of Puzzle 6 */

