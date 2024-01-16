#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lib/legal_position.h"
#include <assert.h>

int main(int argc, char* argv[]){
	if(argc != 3){ printf("Invlaid Input\n"); return 0; }
	char const* const sizeFile = argv[1];
	char const* const positionFile = argv[2];
    FILE* size = fopen(sizeFile, "r"); if(size == NULL){ printf("Invlaid Input\n"); return 0; }
	FILE* position = fopen(positionFile, "r"); if(position == NULL){ printf("Invlaid Input\n"); return 0; }
	int *coords = malloc(8*sizeof(int));
	int *dimensions = malloc(3*sizeof(int));
	if(coords == NULL || dimensions == NULL){ printf("Malloc Failure\n"); return 0; }
	int r; 
	r = fscanf(position, "%d %d %d %d %d %d %d %d", &coords[0], &coords[1], &coords[2], &coords[3], &coords[4], &coords[5], &coords[6], &coords[7]);
	if(r==0){ printf("Failed to read position file\n"); fclose(size); fclose(position); free(coords); free(dimensions); return 0; }	
	int i=0;
	while(1){
		r = fscanf(size, "%d\n", &dimensions[i]);
		if( r == 0 ){ printf("Failed to read size file\n"); fclose(size); fclose(position); free(coords); free(dimensions); return 0; }
		if( r == EOF )break;
		i++;
	}

    fclose(size); fclose(position);
    int A = dimensions[0];
	int T = dimensions[1];
	int L = dimensions[2];
	r = legal_position(A,T,L, coords);
	printf("%d\n", r);
	free(coords); free(dimensions);
	return 1;
}
