/* CMSC 16200 - lab 4
 * File: graph.c 
 * The strict pixel graph and its implementation.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib/graph.h"
#include "lib/pixel.h"



/*
 * Depending on your implementation, 
 * You might want to define helper functions, such as:
 *  - bool is_vertex(graph G, pixelID v);
 *  - bool is_pixel_graph(struct pixel_graph_header *G);
 *  - bool pixel_graph_isedge(graph G, pixelID v, pixelID w); 
 *  - etc.
 * for testing your graph implementation.
 */

// Allocate enough space for the graph, and initialize its required fields.
graph pixel_graph_new(unsigned int img_width, 
                      unsigned int img_height, 
                      pixel pixels[ROWS][COLS]) {

	unsigned int img_size = img_width * img_height;
	int tree_num = 0;
	
	graph G = malloc(sizeof(struct pixel_graph_header));
	if(G==NULL){ printf("Malloc Failure\n"); return NULL; }
	//malloc space for all the edges	
	edge **edges;
	edges = malloc(img_height * sizeof(edge*));
	if(edges==NULL){ printf("Malloc Failue\n"); return NULL; }
	//populate pixel array
	for(unsigned int j=0; j<img_height; j++){
		edges[j] = malloc(img_width * sizeof(edge));
		if(edges[j]==NULL){ printf("Malloc Failure"); return NULL; }
		for(unsigned int i=0; i<img_width; i++){
			memcpy(&(G->pixels)[j][i], &pixels[j][i], 4);
			tree_num++;
		}
	}

	//populate edge array
	for(unsigned int i=0; i<img_width; i++){
		for(unsigned int j=0; j<img_height; j++){
			if(j<img_height-1 && (G->pixels)[j][i]==(G->pixels)[j+1][i]) edges[j][i].up = true;
			else edges[j][i].up = false;
			if(i<img_width-1 && (G->pixels)[j][i]==(G->pixels)[j][i+1]) edges[j][i].right = true;
			else edges[j][i].right = false;
		}
	}

	G->image_width = img_width;
	G->image_height = img_height;
	G->image_size = img_size;
	G->tree_num = tree_num;
	G->edges = edges; 
	G->table_size = 0;
	return G;	
}

bool is_vertex(graph G, pixelID v){
	if ((unsigned int) v > G->image_size) return false;
	return true;
}

int root_hash(pixelID root, int m){
	return root%m;
}

int put_root_color(graph G, pixelID root, int count){
	if(G->roots==NULL){ printf("No table allocated\n"); return -1; }
	int size = G->table_size;
	int i = root_hash(root, size);
	while((G->roots)[i].idx >= 0) i = (i+1) % size;
	(G->roots)[i].idx = root;
	(G->roots)[i].count = count;
	return 0;
} 

int get_root_color(graph G, pixelID root){
	if(G->roots==NULL){ printf("No table allocate\n"); return 0; }
	int size = G->table_size;
	int i = root_hash(root, size);
	while((G->roots)[i].idx != root) i = (i+1) % size;
	int count = (G->roots)[i].count;
	return count;
}



// Free up the memory used by graph G.
void pixel_graph_free(graph G) {
	for(unsigned int i=0; i<G->image_width; i++) free((G->edges)[i]);
	free(G->edges);
	free(G->roots);
	free(G);
	return;
}

