#define _MILU_
//typedef double typ;
#include "library.h"

  
  void direct_solve( typ **Ui, typ **fi, typ **Ki,int **index, int size_begin, int size_end, int dim, int dim_init);
  void direct_solve2( typ **Ui, typ **fi, typ **Ki,int **index, int size_begin, int size_end, int dim, int dim_init);
  void smooth1( typ **Ui, typ **fi, typ **Ki ,int **index, int dim, int dim_init, int size_begin, int size_end);
   void smooth2( typ **Ui, typ **fi, typ **Ki ,int **index, int dim, int dim_init, int size_begin, int size_end);
  
  
  
  //void smooth( typ **Ui, typ **fi, typ **Ki ,int dim, int dim_init, int size_begin, int size_end);
  
  typ eval_aij( int i, int j,ensemble *P, typ **Ki , int **index, int dim, int dim_init,  double delta, int size_begin, int size_end, int *Fnodes);
  
  typ eval_bij( int i, int j, ensemble *P, typ **Ki, int **index, int dim, int dim_init, double delta, int size_begin, int size_end, int *Fnodes);
  
  void matvec_add( typ **Ki, int **index, typ **Ui, typ **fi, typ **di, int dim, int dim_init, int size_begin, int size_end,int level);
  
  void invert_L(typ **LU, int **indexLU,typ **di, int size_begin, int size_end, int dim, int dim_init,int level, typ *di_1, typ *vi_1);
  
  void invert_U(typ **LU, int **indexLU, typ **ui, typ **di, int size_begin, int size_end, int dim, int dim_init, int level, typ *vi_1);
    
  typ get_val(int i, int j, typ ***tab, int ***index);
  
  void create(int i, int j, typ ***tab, int ***index, typ val, typ ***tabt, int ***indext);

  void create(int i, int j, typ ***tab, int ***index, typ val);

  void createLU(int i,int oldF, typ **tab, int **index, typ **tabt, int **indext);
  
   void create2(int i, int j, typ ***tab, int ***index, typ val);

  void MLILU(typ **Ui, typ **fi,  int level, typ ***Ki,  int ***index, int dim,int dim_init, int init_lev, int gamma, double delta);
  
  void plot(typ **Ki, int **index,int size, char *files);
  
  void copy(typ ***Ki, typ ***Ki1, int ***index, int ***ind1, int dim_init);
  
  void plot(typ **Ki, int **index,int size);

