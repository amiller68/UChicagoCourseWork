#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct hold{
	int x;
	int y;
} hold;

struct pos_list;
struct entry;
struct position;

typedef struct position{
	int *coords;
	int moves;
	bool end;
	//leads back to the source position
	struct position *path;
	struct pos_list *edges;
} position;

typedef struct entry{
	position* pos;
	struct entry *next;
} entry;

typedef struct pos_list{
	entry* head;
	int size;

} pos_list;


typedef struct wall{
	int A;
	int T;
	int L;
	int size;
	int shortest;
	int *temp;
	pos_list* known;
	pos_list* starts;
	pos_list* delete_q;
	hold *holds;
	hold start;
	hold end;
} wall;

typedef struct move_list{
	entry* head;
} move_list;

void free_move_list(move_list *M);

void start_positions(wall *W);

void build_path(wall *W, position *source);

wall *create_wall(int A, int T, int L, hold *holds, int *temp, int size);

move_list *generate_moves(wall *W);

void print_moves(move_list *M, int *temp);

void free_wall();

