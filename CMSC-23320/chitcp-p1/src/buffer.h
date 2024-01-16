#include <stdbool.h>

#ifndef BUFFER_H_
#define BUFFER_H_

#define BUF_SIZE 512

typedef struct {
    char *data;          /* data */
    int nbytes;          /* nbytes are in the buffer */
    char *next_msg;      /* the next msg in the buffer */
    char *trail;         /* the end of relevant info in the buffer */
} buffer_t;

void print_char(char *buffer);

/*
 * init_buffer - Creates a buffer in memory and returns a pointer to it.
 *
 * returns - a pointer to the new buffer
*/
buffer_t *init_buffer();

/*
 * clean_buffer - Expels the buffer of used data. If all data is still unused,
 * this has no effect. Data is expelled from the front of the buffer, as data gets removed. The destination
 * for more data is maintained internally.
 *
 * buff - a pointer to the buffer to clean
 * nbytes - n bytes will be cleared out of the front of the buffer.
 * returns - the number of bytes successfully cleared from the buffer
*/
int clean_buffer(buffer_t *buff, int nbytes);

/*
 * find_in_buffer - Searches a buffer for a delimeter and returns an index of the beginning of
 * the delimiter within the buffer.
 *
 * buff - a pointer to the buffer to search
 * delim - a pointer to the string to search in the buffer
 * delete - If true, the delimiter is replaced with '\0' characters. No change if false.
 * returns - an index of the delimiter inside the buffer
*/
int find_in_buffer(buffer_t *buff, char *delim, bool delete);

/*
 * pop_from_buffer - Finds a message inside the buffer defined by a delim string,
 * and writes it into out. Returns number of bytes written.
 *
 * buff - a pointer to the buffer to search
 * delim - a pointer to the string to search in the buffer
 * out - a buffer destination for the bytes being removed from buff.
 * delete - If true, the delimiter is replaced with '\0' characters. No change if false.
 * returns - number of bytes successfully removed from buff / sent to out
*/
int pop_from_buffer(buffer_t *buff, char *delim, char *out, bool delete);

/*
 * is_empty - Returns true if the buffer has no contents (is all '\0's), and false otherwise.
 *
 * buff - a pointer to the buffer to search
 * returns - true if empty, false if containing any bytes
*/
bool is_empty(buffer_t *buff);

#endif