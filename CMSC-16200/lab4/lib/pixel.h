/* CMSC 16200 - lab 4
 * File: pixel.h 
 * The pixel type and an interface to pixels 
 */

#include <stdio.h>
#include <stdlib.h>

#ifndef _PIXEL_H_
#define _PIXEL_H_ 

typedef int pixel;
typedef int pixelID; 

// Returns its index to 1-D pixel array, given the coords of input pixel.
pixelID get_pixel_id(unsigned int x, unsigned int y, unsigned int w);

// Returns the x coordinate value of the pixel, 
// given its index and the width of the image.
unsigned int get_x_coord(pixelID idx, unsigned int w);

// Returns the y coordinate value of the pixel 
// given its index and the width of the image.
unsigned int get_y_coord(pixelID idx, unsigned int w);

// Returns the red component of the pixel p, between 0 and 255, inclusive.
int get_red(pixel p);

// Returns the green component of the pixel p, between 0 and 255, inclusive.
int get_green(pixel p);

// Returns the blue component of the pixel p, between 0 and 255, inclusive.
int get_blue(pixel p);

// Returns the alpha component of the pixel p, between 0 and 255, inclusive.
int get_alpha(pixel p);

// Returns an int representing an RGB pixel consisting of the given
// alpha, red, green and blue intensity values.
// All intensity values must be between 0 and 255,
// inclusive.
pixel make_pixel(int alpha, int red, int green, int blue);

#endif /* _PIXEL_H_ */
