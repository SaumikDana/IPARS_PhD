#define _LABEL_SPEC_

#include "library.h"

/* Specification file */

/* R. E. Bank, C. Wagner 2 dimensionnal Milu decomposition. Solving Ku=f*/

/* Includes the templated class element dealing with scalar or matricial elements whether it is a system or not
*/

/*****************************************************/


/****************************************************/


/* LABELLING */

/*********************************************************/

/* Check for the optimal parents of node i */


void Mark_opt_par(int i, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]));

/*********************************************************/

/* Check for the next node to be eliminated */

int Next_node(int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), nlamb **lamb, int *taille_tab,int* Nodes_val);

/*******************************************************/

/* Look if the considerated node belongs to the F-nodes set*/

bool  Check_if_F_node(int i, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]));

/********************************************************/

/* Look up whether the node is a F- or a C- node */

bool Check_if_F_or_C(int i, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab),ensemble *(*lens[3]), int *taille_table, nlamb **lamb, int **Nodes_val);



/*******************************************************/

/* Free parents for the next stage */

void Free_parents(int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]));

/********************************************************/

void reorganize_sm(typ **fi, typ *vinit, int size);


void initial_order(typ **Ui,int size);
 
