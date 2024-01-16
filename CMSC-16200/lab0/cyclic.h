/* CMSC 16200 - Lab 0
 * File: cyclic.h
 */

#ifndef _CYCLIC_H_
#define _CYCLIC_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* countDigits:
 * counts the number of digits in a number
 */
int countDigits(int num);

/* isFullyRepeated:
 * determines if the number consists of fully repeated pattern
 */
bool isFullyRepeated(int num);

/* rightRotateDigit:
 * rotates the digits in num by d places to the right
 */
int rightRotateDigit(int num, int d);

/* leftRotateDigit:
 * rotates the digits in num by d places to the left
 */
int leftRotateDigit(int num, int d);

/* isCyclicBad:
 * naive implementation for determining if a number is a cyclic number
 */
bool isCyclicBad(int num);

/* isPrime:
 * determines if a number is a prime
 */
bool isPrime(int num);

/* kthCyclic:
 * finds the kth cyclic number using Fermat's quotient form
 */
int kthCyclic(int k, int flag);
void shiftArray(unsigned int *digits, int size);
bool numToList(unsigned long long int num, unsigned int *digits, int size);
bool numToPerms(unsigned long long int num, unsigned int *digits, int size);
bool isCyclic(unsigned long long int num);
#endif
