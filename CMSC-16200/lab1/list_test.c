/* CMSC 16200 - Lab 1
 * File: list_test.c 
 */

#include <stdio.h>
#include <stdlib.h>
#include "list.h"

int main(void) {
    linked_list *a = empty();
    print_list(a);
    cons(20, a);
    print_list(a);
    cons(30, a);
    print_list(a);
    append(a, 10);
    print_list(a);
    pop(a);
    print_list(a);
    pop(a);
    print_list(a);
    pop(a);
    print_list(a);
    free_list(a);
    linked_list *b = singleton(42);
    print_list(b);
    free_list(b);
    printf("Passed all tests!\n");
    return 0;
}
