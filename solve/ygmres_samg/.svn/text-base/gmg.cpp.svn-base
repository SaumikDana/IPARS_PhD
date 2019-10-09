#include <iostream>
#include <math.h>
#include "gmgmod.hpp"

gmg<double> precgmg;
sparse_matrix<double> A;
graph<int> gr;
vector<double> x, f;

extern "C" {

void pinit(int rows, int *ia, int *ja, double *a, int *ig, int *jg, int mindim)
{
  A.init(rows,rows,ia,ja,a);
  gr.init(rows,ig,jg);
  precgmg.init(A,gr,mindim);
}

void preinit(int *ia, int *ja, double *a)
{
  int rows=A.getrows();
  A.del();
  A.init(rows,rows,ia,ja,a);
  precgmg.reinit(A);
}

void psolve(double *dest, double *src)
{
  x.init(A.getrows(),dest);
  f.init(A.getrows(),src);
  x.clear();
  precgmg.solve(A,x,f,0.0,1);
}

}
