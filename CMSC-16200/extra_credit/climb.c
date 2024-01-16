#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "lib/wall.h"
#include "lib/legal_position.h"

int main(int argc, char* argv[]){
	if(argc != 3){ printf("Invlaid Input\n"); return 0; }
	char const* const sizetxt = argv[1];
	char const* const walltxt = argv[2];
	FILE* sizeFile = fopen(sizetxt, "r"); if(sizeFile == NULL){ printf("Invlaid Input\n"); return 0; }
 	FILE* wallFile = fopen(walltxt, "r"); if(wallFile == NULL){ printf("Invlaid Input\n"); return 0; }
    //Find number of holds
	char line[10];
    int counter = 0; /*Number of lines*/
    while(fgets(line, sizeof(line), wallFile) != NULL){
        counter++;
    }
	fclose(wallFile);

 	wallFile = fopen(walltxt, "r"); if(wallFile == NULL){ printf("Invlaid Input\n"); return 0; }

	hold *holds = malloc(counter*sizeof(hold));
	if(holds == NULL){ printf("Malloc Failure\n"); return 0; }
    int *dimensions = malloc(3*sizeof(int));
	if(dimensions == NULL){ printf("Malloc Failure\n"); return 0; }

    if(holds == NULL || dimensions == NULL){ printf("Malloc Failure\n"); return 0; }
	//get the dimsensions from size.txt
	int i=0;
	int r;
    while(1){
        r = fscanf(sizeFile, "%d\n", &dimensions[i]);
        if( r == 0 ){ printf("Failed to read size file\n"); fclose(sizeFile); fclose(wallFile); free(holds); free(dimensions); return 0; }
        if( r == EOF )break;
        i++;
	}
	i = 0;	
	while(fgets(line, sizeof(line), wallFile) != NULL){
        if(!sscanf(line, "%d %d", &(holds[i].x), &(holds[i].y))){ 
			printf("Failed to read wall file\n"); fclose(sizeFile); fclose(wallFile); free(holds); free(dimensions); return 0; }
        i++;
    }
	fclose(sizeFile); fclose(wallFile);

//	for(i = 0; i<=26; i+=2){for(int j=0; j<=26; j+=2){printf("%d %d\n", i, j);}}
	
/*======================================================================*/
//Now the Real coding starts
	
	int A = dimensions[0]; int T = dimensions[1]; int L = dimensions[2];

	//Don't need this now bruh
	free(dimensions);
	
	//create space for storing temporary coords
	int *temp = malloc(8*sizeof(int));
	if(temp==NULL){ printf("Malloc Failure\n"); return 0; }
	//Create a wall for claude to climb!
	wall* W = create_wall(A, T, L, holds, temp, counter);
	//generate start positions
	start_positions(W);
	
	entry *ptr = (W->starts)->head;
	while(ptr!=NULL){
		build_path(W, ptr->pos);
//		printf("----Path----\n");
		ptr = ptr->next;
	}
	
//	printf("Shortest possible num of moves: %d\n", W->shortest);

	move_list *M = generate_moves(W);
	print_moves(M, temp);






	free_wall(W);
	free_move_list(M);
	free(M);
	//removed temp free from free_wall, must be done here
	free(temp);

	return 1;	
}
