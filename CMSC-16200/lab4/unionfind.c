/* CMSC 16200 - lab 4
 * File: unionfind.c 
  * The Union Find implementation using up-trees.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "lib/unionfind.h"

pixelID get_parent(pixelID parentID[ROWS][COLS], unsigned int w, pixelID idx){
	unsigned int x = get_x_coord(idx, w);
	unsigned int y = get_y_coord(idx, w);
	pixelID parent = parentID[y][x];
	return parent;
}

void change_parent(pixelID parentID[ROWS][COLS], unsigned int w, pixelID idx, pixelID new){
	unsigned int x = get_x_coord(idx, w);
	unsigned int y = get_y_coord(idx, w);
	parentID[y][x] = new;
	return;
}

/* Find and return the index of the root of the pixel with pixelID idx. */
pixelID up_trees_find(pixelID parentID[ROWS][COLS], unsigned int w, pixelID idx) {
  	pixelID parent = get_parent(parentID, w, idx);
  	while(parent >= 0){
  		idx = parent;
  		parent = get_parent(parentID, w, idx);
  	}
	return idx;
}

//returns array with the the containgin the root pixelID and the size the tree
//array needs to be freed after use
pixelID* root_and_size(pixelID parentID[ROWS][COLS], unsigned int w, pixelID p){
	pixelID *result = malloc(2*sizeof(pixelID));
	pixelID parent = get_parent(parentID, w, p);
	if(parent>=0){
		result[0] = up_trees_find(parentID, w, p);
		result[1] = get_parent(parentID, w, result[0]);
	}
	else{
		result[0] = p;
		result[1] = parent;
	}	
	return result;
}


/* Merge the two groups to which pixel p1 and pixel p2 belong. */
void up_trees_union(pixelID parentID[ROWS][COLS], unsigned int w, pixelID p1, pixelID p2) {
	//find the idx where the roots are
	pixelID root1, root2, size1, size2;
	pixelID* r_s = root_and_size(parentID, w, p1);
	root1 = r_s[0]; size1 = r_s[1];
	free(r_s);
	r_s = root_and_size(parentID, w, p2);
	root2 = r_s[0]; size2 = r_s[1];
	free(r_s);
	//if the first tree is bigger than the second (lower val)
	if (size1 <= size2 && root1 != root2){
		change_parent(parentID, w, root1, size1 + size2);
		change_parent(parentID, w, root2, root1);
	}
	else{
		change_parent(parentID, w, root2, size1 + size2);
		change_parent(parentID, w, root1, root2);
	}
	return;
}

/* Store forest of up-trees in the array parentID, given the graph G. */
void up_trees_new(graph G, pixelID parentID[ROWS][COLS]) {
	unsigned int w = G->image_width;
	unsigned int h = G->image_height;
	unsigned int i;
	unsigned int j;
	for(i=0; i<w; i++){
		for(j=0; j<h; j++){
			parentID[j][i] = -1;
		}
	}
	return;
}

bool same_parent(pixelID parentID[ROWS][COLS], unsigned int w, pixelID p1, pixelID p2){
	return (up_trees_find(parentID, w, p1) == up_trees_find(parentID, w, p2));
}

/* 
 * Run UNION-FIND, and store the final forest of up-trees in array parentID,
 * where count is a boolean flag indicating whether to print out the count. 
 */
void run_union_find(graph G, pixelID parentID[ROWS][COLS], bool count) {
	unsigned int w = G->image_width;
	unsigned int h = G->image_height;
	unsigned int i;
	unsigned int j;
	int counter = 0;
	for(j=0; j<h; j++){
		for(i=0; i<w; i++){
			if((G->edges)[j][i].up){
				pixelID branch = get_pixel_id(i,j,w);
				pixelID reach = get_pixel_id(i, j+1, w);
				if(!same_parent(parentID, w, branch, reach)){
					up_trees_union(parentID, w, branch, reach);
					counter++;				
				}
			}
			if((G->edges)[j][i].right){
				pixelID branch = get_pixel_id(i,j,w);
				pixelID reach = get_pixel_id(i+1, j, w);
				if(!same_parent(parentID, w, branch, reach)){
					up_trees_union(parentID, w, branch, reach);				
					counter++;
				}
			}
		}
	}
	G->tree_num = G->tree_num - counter;
	if(count) printf("The number of times union was called for this image is: %d\n", counter);
	return;
}




