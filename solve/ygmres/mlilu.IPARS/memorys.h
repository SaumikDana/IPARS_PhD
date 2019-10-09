#define _MEM_

#include "library.h"

/* DEFINE THE ARRAYS NECESSARY FOR THE GRAPH CONSTRUCTION AND INITIALISE STARTING DIMENSIONS*/


/***************************************************************************/

void Init_memory(int *size, int pack, int *level ,int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble **lens[3], int *init);


/***************************************************************************/

void reset_memory(int **tab[3], spec_noeud **spec_tab, ensemble **lens[3],int size);
