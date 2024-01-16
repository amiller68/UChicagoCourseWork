/* CMSC 16200 - lab 4
 * File: graph.h 
 * The strict pixel graph and its interface.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>
#include "image_util.h"
#include "pixel.h"

#ifndef _GRAPH_H_
#define _GRAPH_H_ 

struct pixel_edges {
	bool up;
	bool right;
};
typedef struct pixel_edges edge;

struct root_hash_entry {
	pixelID idx;
	int count;
};
typedef struct root_hash_entry root;

struct pixel_graph_header {
    unsigned int image_width;
    unsigned int image_height;
	unsigned int image_size;
	int tree_num;
    pixel pixels[ROWS][COLS];
	root *roots;
	int table_size;
	edge **edges;
    // Feel free to define more graph data below as you like.
};
typedef struct pixel_graph_header* graph;

// Allocate enough space for the graph, and intialize its required fields.
graph pixel_graph_new(unsigned int img_width, 
                      unsigned int img_height, 
                      pixel pixels[ROWS][COLS]);

int put_root_color(graph G, pixelID idx, int count);

int get_root_color(graph G, pixelID idx);

// Free up the memory used by graph G.
void pixel_graph_free(graph G);

#endif /* _GRAPH_H_ */
