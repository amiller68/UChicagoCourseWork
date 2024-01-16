/* CMSC 16200 - Lab 0
 * File: cyclic.c
 * 
 * Name: (Alex Miller)
 * CNet: (amiller68)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <math.h>
#include "cyclic.h"

/* countDigits:
 *  counts the number of digits in a number
 */
int countDigits(int num) {
	int count=0;
	while (num!=0){
		count++;
		num/=10;
	}    
    return count;
}
int countLongDigits(unsigned long long int num) {
    int count=0;
    while (num!=0){
        count++;
        num/=10;
    }
    return count;
}
/* isFullyRepeated:
 * determines if the number consists of fully repeated pattern
 */
bool isFullyRepeated(int num) {
	bool result = false;
    int len = countDigits(num);
    if(len==1) return true;
    for(int i=1; i<len; i++){
        int power=pow(10,i);
        if(len%i==0){
            int j = div(len, i).quot;
            int temp = num;
            int ptrn = num%power;
            for(int x=1; x<=j; x++){
                if(ptrn==temp%power){result = true; temp/=power;}
                else {result = false; break;}
            }
        }
        if(result == true){break;}
    }
    return result;
}

/* rightRotateDigit:
 * rotates the digits in num by d places to the right
 */
int rightRotateDigit(int num, int d) {
  int temp = num;
  int len = countDigits(num);
  int power=pow(10,len-1);
  for(int i=0; i<d; i++){
    int mvd = temp%10;
    temp/=10;
    temp+=mvd*power;

  }
  return temp;
}

/* leftRotateDigit:
 * rotates the digits in num by d places to the left
 */
int leftRotateDigit(int num, int d) {
  int temp = num;
  int len = countDigits(num);
  int power=pow(10,len-1);
  for(int i=0; i<d; i++){
    int mvd = temp/power;
    temp%=power;
    temp*=10;
    temp+=mvd;

  }
  return temp;
}
/* isCyclicBad:
 * naive implementation for determining if a number is a cyclic number
 */
bool isCyclicBad(int num) {
  int len = countDigits(num);
  bool result=false;
  for(int i=1; i<len; i++){
    for(int j=1; j<len; j++){
      int test = rightRotateDigit(num, j);
      if(test == num*(i+1)) {result = true; break;}
    }
  if(result) break;
  result = false;
  }
  return result;
}

//Shifts an array cyclically by 1 index
void shiftArray(unsigned int *digits, int size){
    int len = size;
	unsigned int temp = digits[0];
    for(int i=0; i<len-1; i++){
        digits[i] = digits[i+1];
    }
    digits[len-1]=temp;
}

//Checks if a number matches an array representation of that number
bool numToList(unsigned long long int num, unsigned int *digits, int size){
    bool result=true;
   // int len = countLongDigits(num);
    for(int i=0; i<size; i++){
		unsigned int test = num%10;
		num /= 10;
	//	unsigned int digit=digits[i];
		if(digits[i]!=test){result = false; break;}
    }
    return result;
}

//Checks is a number matches a single cyclic permutation of an array of digits
bool numToPerms(unsigned long long int num,unsigned int *digits, int size){
    bool result = false;
    for(int i=0; i<size; i++){
        if(numToList(num, digits, size)) {result=true; break;}
		shiftArray(digits, size);
    }
    return result;
}

//global variable to track presence of leading 0 in cyclic numbers
bool lead0 = false;

//Less naive implementation of testing whether a number is cyclic
//Bounded to numbers that fall within the range of unsigned long long ints
//(2^63 -1)
bool isCyclic(unsigned long long int num){
	int len = countLongDigits(num);
	unsigned int *digits = malloc((len)*sizeof(unsigned int));
	//populate array with digits of num
	unsigned long long int temp=num;
	for(int i=0; i<len; i++){digits[i] = temp%10; temp/=10;}
	bool result = false;
	for(int i=2; i<len; i++){//for loop to check consecutive mutliples
		unsigned long long int mult=num*i;
		//Check if a number is cyclic
		if(!(numToPerms(mult, digits, len))){result = false; break;}
		result = true; 
	}
	if(result) {free(digits); lead0=false; return result;}
	//repeats above process but accouting for leading 0
	else{
		free(digits);
		unsigned int *digits = malloc((len+1)*sizeof(unsigned int));
		unsigned long long int temp=num;
		for(int i=0; i<len; i++){
			digits[i] = temp%10;
			temp/=10;
		}
		digits[len] = 0;
		bool result = false;
		for(int i=2; i<len; i++){
			unsigned long long int mult = num*i;
			if(!(numToPerms(mult, digits, len+1))){result = false; break;}
			result = true;
		}
		if(result){lead0=true;}
		return result;
	}
}



/* isPrime:
 * determines if a number is a prime
 */
bool isPrime(int num) {
  bool result = true;
  if(num<=1) return false;
  else{
    int end = num;
    for(int i=2; i<end; i++){
      if(num%i==0) {result=false; break;}
    }
    return result;
  }
}
/* kthCyclic:
 * finds the kth cyclic number using Fermat's quotient form
*/ 
int kthCyclic(int k, int flag){
	int ind = 1;
	unsigned long long int hold;
	int count = 2;
	while(ind<=k){
    	if(isPrime(count)){
	 		unsigned long long int num = pow(10, count-1) - 1;
      		unsigned long long int den =  count;
       		unsigned long long int test = num/den;
			if(isCyclic(test)) {hold = num/den; ind++;} 
	 	}
        count ++;
	}
	if(flag==1){
		if(lead0)printf("0%llu\n", hold);
		else printf("%llu\n", hold);
	}
	lead0=false;
	return count-1;
}
