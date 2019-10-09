#define _REC_T_
#include "library.h"



void reccursive_graph(typ ***matrix, int ***index, typ ***mat_init, int ***index_init, double sigma, int **size_tab, int *(*tab[3]), spec_noeud **spec_tab, ensemble *(*lens[3]), int *size, int pack, int *level, int *init, double delta);

void update_matrix( typ ***matrix, int ***index, int *Nodes, int size_nodes, int size_mat, int pack);

void update_matrix2( typ ***matrix, int ***index, int *Cnodes, int sizeC, int *Fnodes, int sizeF, int pack, double delta,spec_noeud **spec_tab, int old_F,ensemble *P,int lvi, int *level);

void reorganize(typ ***matrix, int ***index, int *nodes, int size, int pack);


void transpose(typ ***matrix, int ***index, typ ***matrixt, int ***indext,int size);

extern "C" void transpose2(typ ***matrix, int ***index, typ ***matrixt, int ***indext,int size);
void reverse(int **Cnodes,int sizec);

void reorganize_transpose(typ ***matrix, int ***index, int *nodes, int size, typ ***matrixt, int ***indext,ensemble *P,int size_F, int *Fnodes);
