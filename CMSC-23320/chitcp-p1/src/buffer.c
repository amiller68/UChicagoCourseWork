#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <string.h>
#include "buffer.h"
#include "log.h"

void print_buffer(buffer_t *buffer)
{
    for (int i = 0; i < BUF_SIZE / 2; i+= 2) {
        chilog(TRACE, "buffer[%d] = %c or %d, buffer[%d] = %c or %d",
               i, buffer->data[i], buffer->data[i], i+1, buffer->data[i+1], buffer->data[i+1]);
    }
    chilog(TRACE, "nbytes = %d, buffer = %p, next_msg = %p",
           buffer->nbytes, buffer,buffer->next_msg);
}

void print_char(char *buffer)
{
    for (int i = 0; i < 200; i++) {
        chilog(TRACE, "buffer[%d] = %c", i, buffer[i]);
    }
}

buffer_t *init_buffer()
{
    buffer_t *buff = calloc(1, sizeof(buffer_t));
    buff->data = calloc(BUF_SIZE * 2, sizeof(char));
    buff->next_msg = buff->trail = buff->data;
    return buff;
}

int clean_buffer(buffer_t *buff, int nbytes)
{
    int difference = buff->next_msg - buff->data;
    memmove(buff->next_msg - difference, buff->next_msg, BUF_SIZE - nbytes);
    buff->next_msg = buff->data + (buff->trail - buff->next_msg);
    for (buff->trail = buff->data; *buff->trail != '\0'; buff->trail++);
    memset(buff->trail,'\0', difference);
    buff->nbytes -= difference;
    return 0;
}

int find_in_buffer(buffer_t *buff, char *delim, bool delete)
{
    int len = strlen(delim);
    if (len > BUF_SIZE || len < 0) {
        perror("find_in_buffer was passed non-string data");
    }
    char *start_buff = buff->data;
    char *start_delim = delim;
    int i;
    for (i = 0; i < BUF_SIZE - len; i++) {
        if (*(start_buff + i) == *delim) {
            int j = 0;
            while (*(start_buff + i + j) == *(delim + j)) {
                j++;
                if (j == len) {
                    if(delete) {
                        for (int k = 0; k < j; k++) {
                            *(start_buff + i + k) = '\0';
                        }
                    }
                    buff->next_msg = start_buff + i + j;
                    return i;
                }
            }
        }
    }
    return 0;
}

int pop_from_buffer(buffer_t *buff, char *delim, char *out, bool delete)
{
    int nbytes = find_in_buffer(buff, delim, delete);
    if (nbytes == 0) {
        return nbytes;
    }

    memmove(out, buff->data, nbytes + 1);
    clean_buffer(buff, nbytes);
    return nbytes;
}
