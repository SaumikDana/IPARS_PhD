#define _LABEL_T_
#include "library.h"

/*
class compc {
  public :
  bool operator() ( const nlamb& x, const nlamb& y) const {
    if (x.val < y.val) return true;
    else return false;}
};
 */


void Init_Graph(typ **matrix, int **index, double sigma, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]));

void Mark_Nodes(typ **matrix, int **index, double sigma, int **size_tab, int *(*tab[3]), spec_noeud *(*spec_tab), ensemble *(*lens[3]));


int unique (int *t, int taille);
int unique (noeud *t, int taille);

bool compc2(nlamb x, nlamb y);

int comp( const void * x, const void * y) ;

int recomp( const void * x, const void * y) ;
