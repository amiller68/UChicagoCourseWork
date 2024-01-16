#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <limits.h>
#include "lib/wall.h"
#include "lib/legal_position.h"

#define spam( a ) printf( a )
//sorry for messy code claude i am tired
//might make a dif ference depending on waht I qaulify as "Same Position"
//either all the holds are the exact same or hand holds can be swtiched 
//current implementation: all holds not eact the same 
bool same_position(position *pos1, position *pos2){
	bool result = true;
	for(int i=0; i<8; i++) result = result && ((pos1->coords)[i]==(pos2->coords)[i]);
	return result;
}

bool same_coords(int *c1, int *c2){
	bool result = true;
	for(int i=0; i<8; i++) result = result && (c1[i]==c2[i]);
	return result;
}

void add_delete(wall *W, position *p){
	entry *head = (W->delete_q)->head;
	entry *new_entry = malloc(sizeof(entry));
	if(new_entry==NULL){ printf("Malloc Failure"); return; }
	new_entry->pos = p;
	new_entry->next = head;
	(W->delete_q)->head = new_entry;
	return; 
}


void skel_free_list(pos_list *pl){
	entry *ptr = pl->head;
	entry *hold = NULL;
	while(ptr != NULL){
		hold = ptr;
		ptr = ptr->next;
		free(hold);
	}
	free(pl);
	return;
}

void free_position(position *p){
	free(p->coords);
	if(p->edges != NULL){
		skel_free_list(p->edges);
	}
	free(p);
}



void free_pos_list(pos_list *pl){
	entry *ptr = pl->head;
	entry *hold = NULL;
	while(ptr != NULL){
		hold = ptr;
		ptr = ptr->next;
		free_position(hold->pos);
	}
	return;
}


void free_move_list(move_list *pl){
	entry *ptr = pl->head;
	entry *hold = NULL;
	while(ptr != NULL){
		hold = ptr;
		ptr = ptr->next;
		free(hold);
	}
	return;
}



bool is_end(wall* W, position *end){
	int endx = (W->end).x;
	int endy = (W->end).y;
//	printf("Testing End:\n");
//	printf("End hold: (%d, %d)\n", endx, endy);
//	printf("LH: (%d, %d)\n", (end->coords)[0], (end->coords)[1]);
//	printf("RH: (%d, %d)\n", (end->coords)[2], (end->coords)[3]);
	return (( (end->coords)[0]==endx && (end->coords)[1]==endy )
	    ||  ( (end->coords)[2]==endx && (end->coords)[3]==endy )); 
}


void update_moves(wall *W, position *p, int dec){
	p->moves += dec;
	pos_list *E = p->edges;
	if(E==NULL) return;
	entry *ptr = E->head;
	while(ptr!=NULL){
		if(!(ptr->pos)->end) update_moves(W, ptr->pos, dec);
		else if ((ptr->pos)->moves < W->shortest){ W->shortest = (ptr->pos)->moves; }
		ptr = ptr->next;
	}
	return;
}


bool position_known(wall *W, position *p){
//	printf("Cheching pos ["); for(int j=0; j<8; j++) printf("%d ", (p->coords)[j]); printf("]\n");
	pos_list *K = W->known;
	entry* ptr = K->head;
	while(ptr!=NULL){
		if(same_position(p, ptr->pos)){
			if(p->moves < (ptr->pos)->moves){
				int dec = p->moves - (ptr->pos)->moves;
				update_moves(W, (ptr->pos), dec);
				(ptr->pos)->path = p->path;
			}
			return true;
		}
		ptr = ptr->next;
	}
	return false;
}

bool start_known(wall *W, position *p){
	pos_list *S = W->starts;
	entry* ptr = S->head;
	while(ptr!=NULL){
		if(same_position(p, ptr->pos)){
			return true;
		}
		ptr = ptr->next;
	}
	return false;
}

void add_known(wall *W, position *p){
	pos_list *K = W->known;
	entry *new_entry = malloc(sizeof(entry));
	if(new_entry == NULL){ printf("Malloc Failure\n"); return; }
	new_entry->pos = p;
	new_entry->next = K->head;
	K->head = new_entry;
	K->size++;
	return;
}

void add_start(wall *W, position *p){
	pos_list *S = W->starts;
	entry *new_entry = malloc(sizeof(entry));
	if(new_entry == NULL){ printf("Malloc Failure\n"); return; }
	new_entry->pos = p;
	new_entry->next = S->head;
	S->head = new_entry;
	S->size++;
	return;
}

void start_positions(wall *W){
	int *coords = W->temp;
	int A = W->A;
	int T = W->T;
	int L = W->L;
	position *start;
	int *pos;	
	for(int i=0; i<W->size; i++){
		coords[0] = (W->holds)[i].x; coords[1] = (W->holds)[i].y;
		coords[2] = (W->holds)[i].x; coords[3] = (W->holds)[i].y;
		coords[4] = (W->start).x; coords[5] = (W->start).y;
		coords[6] = (W->start).x; coords[7] = (W->start).y;
		if(legal_position(A, T, L, coords)==1){
			start = malloc(sizeof(position));
			pos = malloc(8*sizeof(int));
			if (start == NULL || pos == NULL){ printf("Malloc Failure\n"); return; }
			memcpy(pos, coords, 8*sizeof(int));
			start->coords = pos;
			start->moves = 0;
			start->end = false;
			start->path = NULL;
			start->edges = NULL;
			if(!start_known(W, start)){ add_start(W, start); add_delete(W, start); }
			else{ free(start); free(pos); } //No paths to trace, just free the memory
		}
		coords[6] = (W->holds)[i].x; coords[7] = (W->holds)[i].y;
		coords[4] = (W->holds)[i].x; coords[5] = (W->holds)[i].y;
		coords[2] = (W->start).x; coords[3] = (W->start).y;
		coords[0] = (W->start).x; coords[1] = (W->start).y;
		if(legal_position(A, T, L, coords)==1){
			start = malloc(sizeof(position));
			pos = malloc(8*sizeof(int));
			if (start == NULL || pos == NULL){ printf("Malloc Failure\n"); return; }
			memcpy(pos, coords, 8*sizeof(int));
			start->coords = pos;
			start->moves = 0;
			start->end = false;
			start->path = NULL;
			start->edges = NULL;
			if(!start_known(W, start)){ add_start(W, start);  add_delete(W, start); }
			else{ free(start); free(pos); }
		}
	}
	return;
}



void generate_edges(wall *W, position *p){
	int A = W->A; int T = W->T; int L = W->L;
	

	pos_list *E = malloc(sizeof(pos_list));
	if(E==NULL){ printf("Malloc Failure\n"); return; }
	E->head = NULL;
	E->size = 0;
	int *coords = W->temp;
	int *pos = p->coords;
	int i, j, k;
	position *e;
	entry *new_entry;
	int edge_count=0;
	for(i=0; i<W->size; i++){
		for(j=0; j<7; j+=2){
			for(k=0; k<8; k++) coords[k] = pos[k]; //reset coords
			coords[j] = (W->holds)[i].x; coords[j+1] = (W->holds)[i].y; //test a hold with each limb
			if(!same_coords(coords, pos) && legal_position(A,T,L,coords)==1){
				edge_count++;

				new_entry = malloc(sizeof(entry));
				e = malloc(sizeof(position));
				if(e==NULL || new_entry==NULL){ printf("Malloc Failure\n"); return; }
					
				int *new_coords = malloc(8*sizeof(int));
				if(new_coords==NULL){ printf("Malloc Failure\n"); return; }
				memcpy(new_coords, coords, 8*sizeof(int));
					
				e->coords = new_coords;
				e->path = p;
				e->end =false;
				e->moves = p->moves + 1;
				e->edges = NULL;
				add_delete(W, e);

				new_entry->pos = e;
				new_entry->next = E->head;
				E->head = new_entry;
				E->size++;
			}
		}
	}
	if(edge_count == 0) { free(E); return; }
	p->edges = E;
	return;
}


void build_path(wall *W, position *source){
	if(position_known(W, source)) return;
	add_known(W, source);
	if(is_end(W, source)){ //update shortest path
		source->end = true;
		if(source->moves < W->shortest){ W->shortest = source->moves;  }
		return;
	}
	generate_edges(W, source);
	if(source->edges==NULL){ return; }
	entry *ptr = (source->edges)->head;
	while(ptr != NULL){
		build_path(W, ptr->pos);
		ptr = ptr->next;
	}
	return;
}


wall *create_wall(int A, int T, int L, hold *holds, int *temp, int size){
	wall *W = malloc(sizeof(wall));
	if(W==NULL){ printf("Malloc failure\n"); return NULL; }
	pos_list *K = malloc(sizeof(pos_list));
	if(K==NULL){ printf("Malloc failure\n"); return NULL; }
	K->head = NULL; K->size=0;
	pos_list *S = malloc(sizeof(pos_list));
	if(S==NULL){ printf("Malloc failure\n"); return NULL; }
	S->head = NULL; S->size=0;	
	pos_list *D = malloc(sizeof(pos_list));
	if(D==NULL){ printf("Malloc failure\n"); return NULL; }
	D->head = NULL; D->size=0;
	W->A = A;
	W->T = T;
	W->L = L;
	W->size = size;
	W->shortest = INT_MAX;
	W->temp = temp;
	W->known = K;
	W->starts = S;
	W->delete_q = D;
	W->holds = holds;
	W->start = holds[0];
	W->end = holds[size-1];
	return W;
}


//referencing visited pointers so should only account for skel on free
move_list *generate_moves(wall *W){
	int shortest = W->shortest;

	move_list *M = malloc(sizeof(move_list));
	if(M==NULL){ printf("Malloc Failure\n"); return NULL; }
	M->head = NULL;
	pos_list *known  =  W->known;
	entry *ptr = known->head;
	position *target = NULL;
	

	while(ptr != NULL){
		if((ptr->pos)->end){
			if((ptr->pos)->moves == shortest){
				target = ptr->pos;
				break;
			}
		}
		ptr = ptr->next;
	}

	if(target == NULL){ printf("Could not find a path\n"); return M; }


	entry *end = malloc(sizeof(entry));
	if(end==NULL){ printf("Malloc Failure\n"); return NULL; }
	end->pos = target;
	end->next = NULL;

	M->head = end;
	entry *new_entry;
	position *path = target->path;
	while(path != NULL){
		new_entry = malloc(sizeof(entry));
		if(new_entry==NULL){ printf("Malloc Failure"); return NULL; }
		new_entry->pos = path;
		new_entry->next = M->head;
		M->head = new_entry;
		path = (new_entry->pos)->path;
	}
	return M;
}

void print_move(position *b, position *e){
	for(int i=0; i<8; i++){
		if(b->coords[i] != e->coords[i]){
			if(i==0) printf("LH ");
			if(i==2) printf("RH ");
			if(i==4) printf("LF ");
			if(i==6) printf("RF ");
			printf("%d %d\n", (e->coords)[i], (e->coords)[i+1] ); return;

		}

	}
	return;
}


void print_moves(move_list *M, int *temp){
	if(M->head==NULL){ printf("No paths found\n"); return; }
	int *coords = temp;
	entry *ptr = M->head;
	//fill coords with start coords
	
	for(int j=0; j<8; j++) coords[j] = ((ptr->pos)->coords)[j];
	printf("LH %d %d\n", coords[0], coords[1]);
	printf("RH %d %d\n", coords[2], coords[3]);
	printf("LF %d %d\n", coords[4], coords[5]);
	printf("RF %d %d\n", coords[6], coords[7]);
	entry* step = ptr->next;
	while(step != NULL){
		print_move(ptr->pos, step->pos);
		ptr = ptr->next;
		step=step->next;
	}
	return;
}


void free_wall(wall *W){
	free_pos_list(W->delete_q);
	skel_free_list(W->known);
	skel_free_list(W->delete_q);
	skel_free_list(W->starts);
	free(W->holds);
	free(W);
}

