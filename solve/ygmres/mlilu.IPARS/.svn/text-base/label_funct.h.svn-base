#define _LABEL_FUNC_

#include "library.h"

/**************************************************************************
   									  
           USEFUL FUNCTIONS FOR THE GRAPH CONSTRUCTION 

*************************************************************************/


// Evaluates maximum of two numbers

double maxi(double  e1, double  e2);

/*********************************************************************/

// Return in Neighbours[] the list of the neighbours

void look_4_neighbours(int i, int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]));


/********************************************************************/

// Check if j is a neighbour of node i

bool Is_neighbour(int i, int j, int **size_tab, ensemble *(*lens[3]));


void new_vois(int node,int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]));


/*********************************************************************/

// Function evaluates the Lambda value related to the node numbre i

int lambda_i(int i, int **size_tab, int **tab[3], spec_noeud **spec_tab);

/*********************************************************************/

// Function evaluates the value chi for node i

int chi(int r, int s, ensemble *(*lens[3]));

int chi(int r, int s, spec_noeud **spec_tab);

/********************************************************************/

// Function evaluates the number of parents

int np(int i, ensemble *(*lens[3]));

/*******************************************************************/

// Define the number of common neighbours

int ncn(int i, int j, ensemble *(*lens[3]));

/*******************************************************************/

/*  Function evaluates if there exists an edge between i and j and the position in the list */

void check_edge(int i, int j, spec_noeud **spec_tab,int *val[2]);


/*******************************************************************/

// Evaluates the number of neighbours of node i

int nb(int i, ensemble *(*lens[3]));

/*******************************************************************/

// Check if node j is a parent of node i

int check_parents(int i, int j, ensemble *(*lens[3]));

/******************************************************************/

// Evaluates the maximum number of new edges in the next virtual graph

int max_new_edges(int i, spec_noeud **spec_tab, ensemble *(*lens[3]));

int max_new_edges2(int i, int j, spec_noeud **spec_tab, ensemble *(*lens[3]));


/******************************************************************/

// Evaluates value eta for nodes i and j

int eta( int i, int j, int **size_tab,spec_noeud **spec_tab, ensemble *(*lens[3]));

int eta( int i, int j,int k,  int **size_tab,spec_noeud **spec_tab, ensemble *(*lens[3]));

/*****************************************************************/

// Evaluates function nu in respect of node i towards nodes r and s

int nui( int i, int r,  int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]), int ncn);

int nui( int i, int r, int s, int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]), int ncn);

int nui( int i, int r, int s, int t, int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble *(*lens[3]), int ncn);


void update_neigh(int r, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]));
