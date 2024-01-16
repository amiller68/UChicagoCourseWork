#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "cyclic.h"

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ./cyclic <k>\n");
        return 1;
    }
    // Extract information from command line
    int k = atoi(argv[1]);
    // Begin of tests
    assert(countDigits(123) == 3);
    assert(isFullyRepeated(123123) == true);
    assert(isFullyRepeated(12345123) == false);
    assert(isFullyRepeated(333) == true);
    assert(rightRotateDigit(12345, 2) == 45123);
    assert(leftRotateDigit(12345, 2) == 34512);
    assert(isCyclicBad(142857) == true);
    assert(isPrime(2) == true);
    assert(isPrime(15) == false);
    assert(kthCyclic(1, 0) == 7); // for 142857
    assert(kthCyclic(2, 0) == 17); // for 0588235294117647
    // End of tests
    kthCyclic(k, 1); // Print cyclic number to stdout
    return 0;
}

