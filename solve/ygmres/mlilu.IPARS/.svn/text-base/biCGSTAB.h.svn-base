#define _BICG_
#include "library.h"


void bicgstab(typ **matrix, int **index, typ *X, typ *b, int dim, int maxit, typ epsilon, int which, bool verbose);

typ pscal(typ *u, typ *v, int dim);

extern "C" void precond(typ **matrix, int ** index, typ *vres, typ *vinit,  int dim, int which_prec);

typ norm(typ *u, int dim);

void matvec(typ **matrix, int **index,typ *vres,typ *vinit,int dim);

void bicgstab2(typ **, int **, typ *, typ *, int , int , typ , int , bool );

extern "C" void initialise(typ ***mat_init, int ***index_init,typ ***matt, int ***indext, int dim);

void copy2(typ ***Ki, typ ***Ki1, int ***index, int ***ind1, int dim);
