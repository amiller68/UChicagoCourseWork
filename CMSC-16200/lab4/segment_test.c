#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "lib/segment.h"
#include "lib/colors.h"

int main() {
    // Declare variables
    pixel pixels[ROWS][COLS]; 
    unsigned int width;
    unsigned int height;
    pixelID parentID[ROWS][COLS]; // Array implementation of up trees
    graph G;
    int counter;

    // Read in ppm file
    provided_read_png("img/uchicago.png", pixels, &width, &height);

    // Allocate strict pixel graph
    G = pixel_graph_new(width, height, pixels);

    // Initialize 2-D array parentID, 
    // i.e. create a forest of up trees in ParentID
    up_trees_new(G, parentID);

    // Apply your UNION-FIND implementation
    // i.e. store final forest of up trees in ParentID
    run_union_find(G, parentID, true);

    /************************************
     * Counting the Connected Components.
     ************************************/
    printf("Counting the Connected Components...\n");
    // I.e. Scan through each pixel in search for roots in parentID
    counter = 0;
    counter = count_connected_components(G, parentID);
    printf("The number of connected components in this image is: %d\n", counter);

    /************************************
     * Labeling the Connected Components.
     ************************************/
    printf("Labeling the Connected Components...\n");
    // I.e. Scan through each pixel and then 
    //      assign same new color for pixels in each component.
    label_connected_components(G, parentID);

    // Free strict pixel graph
    pixel_graph_free(G);

    return 0;
}
