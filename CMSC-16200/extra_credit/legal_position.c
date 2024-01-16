#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "lib/legal_position.h"


int distsq(int x1, int y1, int x2, int y2){
	int dx = x1-x2;
	int dy = y1-y2;
	return dx*dx + dy*dy;
} 

int left_edge(int A, int lhx, int rhx){
	if(lhx<=rhx) return rhx-A;
	else return lhx-A;
}

int right_edge(int A, int lhx, int rhx){
	if(lhx<=rhx) return lhx+A; 
	else return rhx+A;
}

int upper_edge(int A, int lhy, int rhy){
	if(lhy<=rhy) return lhy+A;
	else return rhy+A;
}

int lower_edge(int A, int lhy, int rhy){
	if(lhy<=rhy) return rhy-A;
	else return lhy-A;
}

int legal_position(int A, int T, int L, int *coords){
	int lhx = coords[0]; int lhy = coords[1];
	int rhx = coords[2]; int rhy = coords[3];
	int lfx = coords[4]; int lfy = coords[5];
	int rfx = coords[6]; int rfy = coords[7];
	int upper = upper_edge(A, lhy,rhy);
	int lower = lower_edge(A, lhy,rhy);
	int left = left_edge(A, lhx, rhx);
	int right = right_edge(A, lhx, rhx);
	int topy, boty;
	for(int x=left; x<=right; x++){
		for(int y=lower; y<=upper; y++){
			topy = y + T;
			boty = y - T;
			if(distsq(x, y, rhx, rhy)<=A*A && distsq(x, y, lhx, lhy)<=A*A){
				if(distsq(x, topy, rfx, rfy)<=L*L && distsq(x, topy, lfx, lfy)<=L*L) return 1;
				if(distsq(x, boty, rfx, rfy)<=L*L && distsq(x, boty, lfx, lfy)<=L*L) return 1;
			}
		}
	} 
	return 0;
}
















