/* CMSC 16200 - lab 4
 * File: segment.c 
 * Implementations for operations on connected component.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "lib/segment.h"
#include "lib/unionfind.h"
#include "lib/colors.h"
#include "lib/graph.h"
#include "lib/pixel.h"

/* Counting the connected components in the image. */
int count_connected_components(graph G, pixelID parentID[ROWS][COLS]) {
	unsigned int w = G->image_width;
	unsigned int h = G->image_height;
	
	G->table_size = G->tree_num;
	root *roots = malloc((G->table_size)*sizeof(root));
    if(roots==NULL){ printf("Malloc Failure\n"); return -1; }
    //fill table with default vals
    for(int i=0; i<G->table_size; i++){
        roots[i].idx = -1;
        roots[i].count = -1;
    }
	G->roots = roots;
	
	int counter = 0;
	for(unsigned int j=0; j<h; j++){
		for(unsigned int i=0; i<w; i++){
			pixelID p = get_pixel_id(i,j,w);
			pixelID parent = up_trees_find(parentID, w, p);
			if(p == parent){
				if(put_root_color(G, p, counter) != 0){ printf("Put Failure"); return -1; }
				counter++;
			}
		}
	}

	printf("The number of connected components in this graph is: %d\n", counter);
	return counter;
}

/* Labeling the connected components in the image. */
void label_connected_components(graph G, pixelID parentID[ROWS][COLS]) {
	unsigned int w = G->image_width;
	unsigned int h = G->image_height;
	char *filename = "output.png";
	for(unsigned int j=0; j<h; j++){
		for(unsigned int i=0; i<w; i++){
			pixelID p = get_pixel_id(i,j,w);
			pixelID root = up_trees_find(parentID, w, p);
			int colorIdx = get_root_color(G, root);
			pixel color = get_color(colorIdx);
			(G->pixels)[j][i] = color;
		}
	}

	provided_write_png(filename, G->pixels, w, h);	
	return;
}
