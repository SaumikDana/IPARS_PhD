//#include "library.h"

//#define _MILU_

#include "library.h"

/*
#ifndef _DEF_
#include "definitions.h"
#endif
*/

#include <fstream.h>

#define _umd2so   _F_NAME_(UMD2SO,umd2so)
#define _umd2fa   _F_NAME_(UMD2FA,umd2fa)
#define _umd2rf   _F_NAME_(UMD2RF,umd2rf)
#define _umd2in   _F_NAME_(UMD2IN,umd2in)

_F_EXTERN_(void) _umd2so(_F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER*,  _F_REAL_8 *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *, _F_REAL_8 *, _F_REAL_8 *, _F_REAL_8 *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *);
_F_EXTERN_(void) _umd2fa(_F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *,_F_INTEGER *, _F_INTEGER *, _F_REAL_8 *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *);
_F_EXTERN_(void) _umd2rf(_F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER *, _F_INTEGER , _F_REAL_8 *,
            _F_INTEGER *,_F_INTEGER *, _F_REAL_8 *, _F_INTEGER *, _F_INTEGER *, _F_REAL_8 *);
_F_EXTERN_(void) _umd2in(_F_INTEGER *, _F_REAL_8 *, _F_INTEGER *);

//typedef double typ;

/* Implementation file */

/* R. E. Bank, C. Wagner 2 dimensional Milu decomprecomosition. Solving Ku=f*/

/* Includes the templated class element dealing with scalar or matricial elements whether it is a system or not
*/

/*********************************************************/

/*Apply MLILU to the resulting matrices. Starts with MLILU(1,u,f)*/


/* REMARKS : We choose  the test vector equal to the sum of all basis element 
             so as to avoid additional storage
*/


 extern int **Res_Nodes;
extern int *Res_size;
extern  int **Res_Parents;
extern double delta;
extern typ ***matKi;
extern typ ***LU;
extern int ***indKi;
extern int ***indexLU;





//template<class typ>
void direct_solve( typ **Ui, typ **fi, typ **Ki, int **index,  int size_begin, int size_end, int dim,int  dim_init, int iterations)
{
  // Direct solve is so far a simple Gauss-Seidel solver
  
  int i,j,j0 ,k;
 /*
   
      for (  i =size_begin; i< dim_init; i++)
      {
      cout << " D solve " << (*fi)[i-size_begin] << "\n";
      }
  */

  // ASSUME THAT THE LAST SYSTEM IS SMALL AND FULL
  
 // cout << " TAILLE DU SYSTEME RESIDUEL " << (dim_init-size_begin) << "\n";

  typ *matrix = (typ*) calloc((dim_init-size_begin)*(dim_init-size_begin),sizeof(typ));
  typ *sol = (typ*) calloc((dim_init-size_begin),sizeof(typ));
  int dims  = (dim_init-size_begin);
  
  for ( i=0; i<dims; i++)
    {
    sol[i] = (*fi)[i];
    matrix[i+i*dims] = Ki[i][0];
    for ( j=1; j< index[i][0]; j++)
      {
	j0 = index[i][j];
	matrix[i*dims+j0] = Ki[i][j];
      }
    }
  /*
    fstream pipo("mat.dat",ios::out);
    fstream pipo2("sm.dat",ios::out);
  */

  /*
  for ( i=0; i< dims; i++)
    {
      for ( j=0; j< dims; j++)
	{
	 cout << matrix[i*dims+j] << " ";
	}
      cout<< "\n";
      //pipo2 << (*fi)[i] << "\n";
    }*/
  /*
  exit(1);
  
*/  
  


  for ( i=1; i<dims; i++)
    {
    for ( k=0; k<= i-1; k++)
      {
	matrix[i*dims+k] = matrix[i*dims+k]/matrix[k+k*dims];
	for ( j=k+1; j< dims; j++)
	  {
	    matrix[i*dims+j] = matrix[i*dims+j] -matrix[i*dims+k]*matrix[k*dims+j];
	  }
      }
    }
  
  //FORWARD
/*
  for ( i=0; i< dims; i++)
    {
      for ( j=0; j< dims; j++)
	{
	  printf("%.15g    ",matrix[i+j*dim]);
	}
      cout << "\n";
      //     pipo2 << (*fi)[i] << "\n";
    }
  exit(1);
*/ 


  for ( i=0; i< dims; i++)
    {
      for ( j=0; j< i; j++)
	{
	  sol[i] = sol[i] - sol[j]*matrix[i*dims+j]; 
	}//printf("%.15g\n",sol[i]);
    }

  // BACKWARD

  for ( i=dims-1; i>= 0; i--)
    {
      for ( j=dims-1; j> i; j--)
	{
	  sol[i] = sol[i] - sol[j]*matrix[i*dims+j];
	}
      sol[i] = sol[i]/matrix[i+i*dims];
     
    }
  
  /*
  for ( i=0; i<dims; i++)
    {
    matrix[i+i*dims] = Ki[i+size_begin][0];
    for ( j=1; j< index[i+size_begin][0]; j++)
      {
	j0 = index[i+size_begin][j];
	if ( j0 >= size_begin)
	  {
	    matrix[i+(j0-size_begin)*dims] = Ki[i+size_begin][j];
	  }
      }
    }

  */


//  typ res = 0;

//  typ *sm = (typ *) calloc( dims, sizeof(typ));

  /*
    for ( i=0; i< dims; i++)
    {
    sm[i] =0;
    for ( j=0; j< dims; j++)
    {
    sm[i] = sm[i] + sol[j]*matrix[i+j*dims];
    }
    printf("%.15g\n",sol[i]);
    res = res + sm[i] - (*fi)[i];
    }
    
    cout << "RSE " << res; exit(1);
  */




  /*
    typ *sol = (typ*) malloc(sizeof(typ)*(dim_init-size_begin));
    typ *sm = (typ*) malloc(sizeof(typ)*(dim_init-size_begin));
    typ **mattemp = (typ**) malloc(sizeof(typ*)*(dim_init-size_begin));
    int **indtemp = (int**) malloc(sizeof(int*)*(dim_init-size_begin));
    
    
    
    
    for ( i=0; i<dim_init - size_begin; i++)
    {
    sol[i] =0;
    sm[i] = (*fi)[i];
    mattemp[i] = (typ*) malloc(sizeof(typ));
    indtemp[i] = (int*) malloc(sizeof(int));
    mattemp[i][0] = Ki[i+size_begin][0];
    indtemp[i][0] = 1;
    for ( j=1; j< index[i+size_begin][0]; j++)
    {
    j0 = index[i+size_begin][j];
    if ( j0 >= size_begin)
    {
    mattemp[i] = (typ*) realloc(mattemp[i],sizeof(typ)*(indtemp[i][0]+1));
    indtemp[i] = (int*) realloc(indtemp[i],sizeof(typ)*(indtemp[i][0]+1));
    mattemp[i][indtemp[i][0]] = Ki[i+size_begin][j];
    indtemp[i][indtemp[i][0]++] = j0-size_begin;
    }
    }
    }
    //plot(mattemp,indtemp,dim_init-size_begin);
    
    bicgstab2(mattemp,indtemp,sol,sm,dim_init-size_begin, maxit,epsilon,which_prec,verbose);
    
  */
  
  for ( i=0; i< dim_init-size_begin; i++)
    {
      (*Ui)[i] = sol[i];
//    cout << "sol " << sol[i] <<"\n";
    }
  
  /*
  free(sol);
  free(sm);
  free(mattemp);
  free(indtemp);
*/

  free(matrix);
  free(sol);


  // exit(1);
  
  /*
    for ( int inc =0; inc < iterations; inc++)
    {
    for ( int i=size_begin; i< dim_init; i++)
    {
    (*Ui)[i-size_begin] = 0;
    for ( int j = 1; j< index[i][0]; j++)
    {
    j0 = index[i][j];
    if ( (j0 > i) && ( j0 < dim_init))
    {
    (*Ui)[i-size_begin] = (*Ui)[i-size_begin] - (Ki)[i][j]*(*Ui)[j0-size_begin];
    }
    }
    for (j = 1; j< index[i][0] ; j++)
    {
    j0 = index[i][j];
    if  ((j0 < i) && (j0 >= size_begin))
    {
    
    (*Ui)[i-size_begin] = (*Ui)[i-size_begin] - (Ki)[i][j]*(*Ui)[j0-size_begin];
    //		  cout << "J0" <<  j0-size_begin << "\n";
    }
    }
    (*Ui)[i-size_begin] = ((*fi)[i-size_begin]+(*Ui)[i-size_begin])/ (Ki)[i][0];
    }
      
    }
  */
  
  //  cout << "SB " << size_begin << "\n";
  //  cout << "SE " << size_end << "\n";
  
  /*
    for (  i =size_begin; i< size_end; i++)
    {
    cout << " D solve " << (*Ui)[i-size_begin] << "\n";
    }
    //  exit(1);  
  */
}


void direct_solve2( typ **Ui, typ **fi, typ **Ki, int **index,  int size_begin, int size_end, int dim,int  dim_init, int iterations)
{

  extern int factor_for_int;
  extern int factor_for_real;
  extern int *worki;
  extern double *workr;
 
  int i,j;
  int dims = dim_init-size_begin;
  _F_REAL_8  * PR = workr;
  _F_REAL_8  * W  = PR + factor_for_real*dims;
  _F_REAL_8  * XX = W + 2*dims;
  _F_REAL_8  * YY = XX + dims;
  _F_REAL_8  * Cntl  = YY + dims;
  _F_REAL_8  * Rinfo = Cntl + 10;
  _F_INTEGER * PI  = worki;
  _F_INTEGER * Keep= PI + factor_for_int *dims;
  _F_INTEGER * Icntl= Keep + 20;
  _F_INTEGER * InfoUMS = Icntl + 20;
   _F_INTEGER ldw = factor_for_real*dims;
   _F_INTEGER liw = factor_for_int *dims;
   _F_INTEGER ZERO = 0 ;
   _F_INTEGER nel,ii;
   
 

   // RESOLUTION
  
  for (j=0;j<dims;++j) XX[j]= (*fi)[j];  
  _umd2so(&dims,&ZERO,&ldw,&liw,PR,PI,Keep,
	  XX, (*Ui),W,Cntl,Icntl,InfoUMS,Rinfo);
  if (InfoUMS[0] < 0) printf("\nError in UMS2SO\n");


}




// Gauss-Seidel smoother

//template<class typ>
void smooth1( typ **Ui, typ **fi, typ **Ki ,int **index, int dim, int dim_init, int size_begin, int size_end, int iterations)
{

//  typ tmp;
  int j0,j,ii;
  for ( int inc =0; inc < iterations; inc++)
    {
      for ( int i=0; i<  dim; i++)
	{
//	  tmp = (*Ui)[i];
	  (*Ui)[i] = 0;
	  for ( j = 1; j< index[i][0]; j++)
	    {
	      j0 = index[i][j];
	      (*Ui)[i] = (*Ui)[i] - (Ki)[i][j]*(*Ui)[j0];
	    }
	  (*Ui)[i] = ((*fi)[i]+(*Ui)[i])/ (Ki)[i][0];
	}
      
    }
  
}

void smooth2( typ **Ui, typ **fi, typ **Ki ,int **index, int dim, int dim_init, int size_begin, int size_end, int iterations)
{
  int i,inc;
  typ omega = 0.95;
  typ tmp;

//  cout.flush() << " SIZE_BEG   " << size_begin << "\n";


  int j0,j;
  for ( inc =0; inc < iterations; inc++)
    {
      for (  i= 0; i<  dim_init-size_begin; i++)
	{
	  tmp = (*Ui)[i];
	  (*Ui)[i] = 0;
	  for ( j = 1; j< index[i][0]; j++)
	    {
	      j0 = index[i][j];
	      (*Ui)[i] = (*Ui)[i] - omega*(Ki)[i][j]*(*Ui)[j0];
	    }
	  
	  (*Ui)[i] = (omega*(*fi)[i]+(*Ui)[i] + ( 1.0-omega)*Ki[i][0]*tmp)/ (Ki)[i][0];
	}
/*      
      for (  i=dim_init-1-size_begin; i >=  0; i--)
	{
	  tmp = (*Ui)[i];
	  (*Ui)[i] = 0;
	  for ( j = 1; j< index[i][0]; j++)
	    {
	      j0 = index[i][j];
	      
	      (*Ui)[i] = (*Ui)[i] - omega*(Ki)[i][j]*(*Ui)[j0];
	    }
	  (*Ui)[i] = (omega*(*fi)[i]+(*Ui)[i] + ( 1.0-omega)*Ki[i][0]*tmp)/ (Ki)[i][0];
	}
      */
      
    }
}






// Functions evaluating aij and bij

// Admit that we only calaculates the values of nodes j belonging to the parent//     s of i ( zero otherwise)

//template<class typ>
typ eval_aij( int i, int j, ensemble *P, typ **Ki , int **index, int dim , int dim_init, double delta, int size_begin, int size_end, int *Fnodes)
{
  int j0,k;
  typ aij=0;
  int nbre_par = (P[Fnodes[i]]).nbre;
  typ sum_abs = 0;
  typ sum = 0;

  typ val = 0;
/*
  for ( int z = 0; z < nbre_par; z++)
    {
      j0 = ((P)[Fnodes[i]]).liste[z];
      for ( k=1; k< index[i][0]; k++)
	{
	  if ( index[i][k] == j0)
	    {
	      sum_abs = sum_abs + fabs((Ki)[i][k]);
	      break;
	    }
	}
    }
*/

  for ( int zz = 1; zz< index[i][0] ; zz++)
    {
      if ( index[i][zz] > i)
	{
	  sum = sum + (Ki)[i][zz];
	  if ( index[i][zz]== j) val = fabs((Ki)[i][zz]);
	}
      for ( int z = 0; z < nbre_par; z++)
	{
	  j0 = ((P)[Fnodes[i]]).liste[z];
	  if ( index[i][zz] == j0)
	    {
	      sum_abs = sum_abs + fabs((Ki)[i][zz]);
	      break;
	    }
	}
    }

      
  
  if ( sum_abs >= delta)
    {
      
      aij = val*sum / sum_abs;
      
    }
  else
    {
      aij = sum / nbre_par;
    }
  
  
  return aij;
  
}

//template<class typ>
typ eval_bij( int i, int j, ensemble *P, typ **Ki, int **index, int dim, int dim_init, double delta, int size_begin, int size_end, int *Fnodes)
  
{
  typ bij = 0;
  int nbre_par = P[Fnodes[i]].nbre;
  typ sum_abs = 0;
  typ sum = 0;
  int j0, k;
  typ val = 0;

/*  
  for ( int z = 0; z < nbre_par; z++)
    {
      j0 = (P[Fnodes[i]]).liste[z];
      for ( k=1; k < index[i][0]; k++)
	{
	  
	  if ( index[i][k] == j0)
	    {
	      sum_abs = sum_abs + fabs((Ki)[i][k]);
	      break;
	    }
	} 
    }
*/
  for ( int zz = 1; zz< index[i][0] ; zz++)
    {
      if ( index[i][zz] > i)
	{
	  sum = sum + (Ki)[i][zz];
	  if ( index[i][zz]== j) val = fabs((Ki)[i][zz]);
	}
      for ( int z = 0; z < nbre_par; z++)
	{
	  j0 = (P[Fnodes[i]]).liste[z];
	  if ( index[i][zz] == j0)
	    {
	      sum_abs = sum_abs + fabs((Ki)[i][zz]);
	      break;
	    }
	}
    }
  
  if ( sum_abs >=  delta)
    {
      bij =val*sum / sum_abs;
    }
  else
    {
      bij = sum / nbre_par;
    }
  return bij;
} 


//template<class typ>
void matvec_add( typ **Ki, int **index, typ **Ui, typ **fi, typ **di, int dim, int dim_init,int size_begin, int size_end, int level)
{
  
  int i0,j0;
  extern typ *vd;
  extern int **indices;
  
  int *indlev = indices[level];

  for (int i=0; i< dim; i++)
    {
      i0 = i ;
      (*di)[i0 ] = 0;
      for (int j=1; j< index[i][0]; j++)
	{
	      j0 = index[i][j] ;
	      (*di)[i0] = (*di)[i0] + ((Ki)[i][j])*(*Ui)[j0];
//	  (*di)[i0] = (*fi)[i0] - (*di)[i0]- (*Ui)[i0] * (Ki)[i][0];
	}
      (*di)[i0] = (*fi)[i0] - (*di)[i0]- (*Ui)[i0] * (Ki)[i][0];
      vd[i] = (*di)[i];
    }
  return;
}


//template<class typ>
void invert_L(typ **LU1,int **indexLU1, typ **di, int size_begin, int size_end, int dim, int dim_init,int level, typ *di_1, typ *vi_1)
{
  int ind,i,i0,j0,k;
  extern typ *vd;
  extern int **indices;
  
  int *indlev = indices[level];
  


for ( i =size_begin; i< dim_init; i++)
  {
    i0 = i - size_begin;
    (*di)[i0] = vd[indlev[i0]];
    for ( k = 1; (k < indexLU1[i][0]); k++)
      {
	ind = indexLU1[i][k];
	if (  (ind < size_end) && (ind < i) && (ind >= size_begin  ))
	  {
	    j0 = indexLU1[i][k] - size_begin;

	    (*di)[i0] = (*di)[i0]- (*di)[j0]* (LU1)[i][k];
	    }
	}
    if ( i>= size_end)
      {
	vi_1[i-size_end] = 0.;
	di_1[i-size_end] = (*di)[i-size_begin];
      }
  }
 
}



//template<class typ>
void invert_U(typ **LU1, int **indexLU1, typ **ui, typ **di, int size_begin, int size_end, int dim, int dim_init, int level, typ *vi_1)
{

  int i,j,j0,i0;
  extern int **indices;
  int *indlev = indices[level];
 /*
  for ( i=0; i< dim_init-size_begin; i++)
    {
      cout << indlev[i] << ",";
    }
  */
  typ *tmp =(typ*) malloc(sizeof(typ)*(dim_init - size_begin)); 

  for (  i = dim_init-1; i >= size_begin; i--)
    {
      i0 = i - size_begin;
 //    cout << "VECTEUR di " << (*di)[i0] << "\n";
   //cout << tmp[i0] << " DDSDSDS \n";

      if ( i >= size_end ) 
	{
	  tmp[i0] = (*di)[i0] = vi_1[i-size_end];
	 
	  //  (*ui)[i0]  = (*ui)[i0] +tmp[i0];
	
	}
      else
	{
	  tmp[i0] = 0;
//	  cout << "I:  " << i << "   "; 
	  for ( j=1;j< indexLU1[i][0]; j++)
	    {
	      if ( indexLU1[i][j] >i )
		{
		  j0 = indexLU1[i][j] - size_begin;
		  tmp[i0] = tmp[i0]+ (tmp)[j0]* (LU1)[i][j];
//		  cout << "DI " <<  (*ui)[j0] << "     LU1 " << (LU1)[i][j] << ",      ";
		}
	    }
	  tmp[i0] = 1/LU1[i][0]*(-tmp[i0] +(*di)[i0]);
	
	  // (*ui)[i0]  = (*ui)[i0] +tmp[i0];


	  //	  cout << " UI " << (*ui)[i0] << "\n";
	}//cout << "\n";
      (*ui)[indlev[i0]]  = (*ui)[indlev[i0]] +tmp[i0];
    }
/*
  for ( i=0;i<dim_init-size_begin;i++)
    {
      (*ui)[indlev[i]] = (*ui)[indlev[i]]+tmp[i];
    }
*/

  free(tmp);
}




void create(int i, int j, typ ***tab, int ***index, typ val,typ ***tabt, int ***indext)
{
  int k;
  bool trouve,trouvet;

  if ( val == 0 ) return;
  if ( i == j)
    {
      (*tab)[i][0] = (*tab)[i][0]+val;
      (*tabt)[j][0] = (*tabt)[j][0]+val;
      return;
    }
  else
    {
      trouve = false;
      trouvet= false;
      for ( k = 1; k < (*index)[i][0]; k++)
	{
	  if ( (*index)[i][k] == j)
	    {
	      (*tab)[i][k] = (*tab)[i][k]+val;
	      trouve = true;
	      break;
	    }
	}
      for ( k = 1; k < (*indext)[j][0]; k++)
	{
	  if ( (*indext)[j][k] == i)
	    {
	      (*tabt)[j][k] = (*tabt)[j][k]+val;
	      trouvet = true;
	      break;
	    }
	}
    }
  int size = (*index)[i][0]+1;
  int size2 = (*indext)[j][0]+1;
  if ( !trouve )
    {
      if ( (size-1)%7 == 0 ) 
	{
	  //  cout.flush() << " SIZE -1 " << size-1 << "\n";
	  (*index)[i] = (int*) realloc((*index)[i],sizeof(int)*(size+6));
	  (*tab)[i] = (typ*) realloc((*tab)[i],sizeof(typ)*(size+6));
	}
      (*index)[i][size-1] = j;
      (*index)[i][0]=size;
      (*tab)[i][size-1] = val;
    }
  if ( !trouvet )
    {
      if ( (size2-1)%7 == 0 ) 
	{
	  (*indext)[j] = (int*) realloc((*indext)[j],sizeof(int)*(size2+6));
	  (*tabt)[j] = (typ*) realloc((*tabt)[j],sizeof(typ)*(size2+6));
	}
      (*indext)[j][size2-1] = i;
      (*indext)[j][0]=size2;
      (*tabt)[j][size2-1] = val;
    }
  return;
}


void create(int i, int j, typ ***tab, int ***index, typ val)
{
  int k;


  if ( val == 0 ) return;
  if ( i == j)
    {
      (*tab)[i][0] = (*tab)[i][0]+val;
      return;
    }
  else
    {
  
      for ( k = 1; k < (*index)[i][0]; k++)
	{
	  if ( (*index)[i][k] == j)
	    {
	      (*tab)[i][k] = (*tab)[i][k]+val;
	      return;
	    }
	}
      
    }
  int size = (*index)[i][0]+1;

      if ( (size-1)%7 == 0 ) 
	{
	  //  cout.flush() << " SIZE -1 " << size-1 << "\n";
	  (*index)[i] = (int*) realloc((*index)[i],sizeof(int)*(size+6));
	  (*tab)[i] = (typ*) realloc((*tab)[i],sizeof(typ)*(size+6));
	}
      (*index)[i][size-1] = j;
      (*index)[i][0]=size;
      (*tab)[i][size-1] = val;
  return;
}










void create2(int i, int j, typ ***tab, int ***index, typ val)
{
  int k;
  
  if ( val == 0 ) return;
  if ( i == j)
    {
      (*tab)[i][0] = (*tab)[i][0]+val;
      return;
    }
  else
    {
      for ( k = 1; k < (*index)[i][0]; k++)
	{
	  if ( (*index)[i][k] == j)
	    {
	      (*tab)[i][k] = (*tab)[i][k]+val;
	      return;
	    }
	}
    }
  int size;
  size = (*index)[i][0]+1;
 // printf("A %d \n",size);
 // if ( size >= 2*4*7*10 ) { cout << "TAILLE INSUFFISANTE ";exit(1);}
 // (*index)[i] = (int*) realloc((*index)[i],sizeof(int)*size);
 // (*tab)[i] = (typ*) realloc((*tab)[i],sizeof(typ)*size);
  (*index)[i][size-1] = j;
  (*index)[i][0]=size;
  (*tab)[i][size-1] = val;
  return;
}


void createLU(int i,int oldF, typ **matrix, int **index, typ **matrixt, int **indext)
{
  int k,kk,ii;
  extern typ ***LU;
  extern int ***indexLU;
  
  ii = i+oldF;

      (*indexLU)[ii] = (int*) realloc((*indexLU)[ii],sizeof(int)*((* indexLU)[ii][0]+(index)[i][0]));
      (*LU)[ii] = (typ*) realloc((*LU)[ii],sizeof(typ)*((*indexLU)[ii][0]+(index)[i][0]));
      for ( k=1; k< index[i][0]; k++)
	{
	  if ( index[i][k] > i )
	    {
	       kk = index[i][k] + oldF;
	      (*LU)[ii][(*indexLU)[ii][0]] = matrix[i][k];
	      (*indexLU)[ii][(*indexLU)[ii][0]++] =  kk;
	    }
	}
      
       for ( k=1; k< indext[i][0]; k++)
	{
	  if ( indext[i][k] > i )
	    {
	      kk = indext[i][k]+oldF;
	      if ( ((*indexLU)[kk][0]+1)%7 == 0 )
		{
		  (*indexLU)[kk] = (int*) realloc((*indexLU)[kk],sizeof(int)*((*indexLU)[kk][0]+7));
		  (*LU)[kk] = (typ*) realloc((*LU)[kk],sizeof(typ)*((*indexLU)[kk][0]+7));
		}
	      (*LU)[kk][(*indexLU)[kk][0]] = matrixt[i][k]/matrix[i][0];
	      (*indexLU)[kk][(*indexLU)[kk][0]++] =  ii;
	    }
	}
      
  
  return;
}





typ get_val(int i, int j, typ ***tab, int ***index)
{
  int k;
  if ( i==j) return (*tab)[i][0];
  for ( k = 1; k < (*index)[i][0]; k++)
    {
      if ( (*index)[i][k] == j)
	{
	  return (*tab)[i][k];
	}
    }
  return 0;
}











//template<class typ>
void MLILU(typ **Ui, typ **fi,  int level, typ ***Ki,  int ***index,  int dim,int dim_init, int init_lev, int gama, double delta)
{

  typ *di = NULL;
  typ *di_1 = NULL;
  typ *vi_1 = NULL;

  extern int **indices;
  extern double *vdi;

  int *indlev = indices[level];


  int size_begin, size_end, szb, sze,it2, it1, its;
  extern int *levels;
  
  its = 50;
  it1 = 0;
  it2 = 1;
  
  size_begin = szb = levels[level];
  size_end = sze = levels[level+1]; 

/*  cout.flush() << "SZB   " << szb <<";";
  cin >> stop;
  */

  
  di = vdi + size_begin;//(typ*) malloc(sizeof(typ)*(dim_init-size_begin));
 
  dim =dim_init - size_begin;
 
  if ( level == init_lev ) // C_nodes
    {

      direct_solve2(Ui,fi, *Ki, *index,size_begin, size_end, dim, dim_init, its);
 
    }
  else
    {
      typ **Ki0 = (*Ki);
      int **index0 = (*index);
 
      smooth1(Ui,fi,*Ki,*index,dim, dim_init,size_begin, size_end, it1);
      matvec_add(*Ki,*index,Ui,fi,&di,dim,dim_init, size_begin, size_end,level);

    
      *Ki = matKi[level];
      *index  = indKi[level];
      
    
     (di_1) = (typ*) malloc(sizeof(typ)*(dim_init-size_end));
      vi_1 = (typ *) malloc((dim_init-size_end)*sizeof(typ));

    
      invert_L(*LU, *indexLU,&di, size_begin, size_end, dim, dim_init,level, di_1,vi_1);
  

      for (int z=0; z< gama; z++) 
	{
	  MLILU(&vi_1, &di_1,level+1, Ki, index,  dim, dim_init,init_lev,gama,delta);
	}
     

      invert_U(*LU, *indexLU,Ui, &di,szb, sze, dim, dim_init,level,vi_1);
   
      smooth1(Ui,fi,Ki0,index0,dim,dim_init, szb, sze, it2);
 
      free(di_1);
      free(vi_1);

    }
  
}
	

/*********************************************************/



// DEBUGGING PROCEDURE PLOATING FULL MATRICES

void copy(typ ***Ki, typ ***Ki1, int ***index, int ***ind1, int dim_init)
{
  int i,j;
  
  (*Ki1) = (typ**) malloc(sizeof(typ*)*dim_init);
  *ind1 = (int**) malloc(sizeof(int*)*dim_init);
  for ( i = 0; i < dim_init; i++)
    {
      (*ind1)[i] = (int*) malloc(sizeof(int)*(*index)[i][0]);
      (*Ki1)[i] = (typ*) malloc(sizeof(typ)*(*index)[i][0]);
      for ( j = 0; j < (*index)[i][0]; j++)
	{
	  (*ind1)[i][j] = (*index)[i][j];
	  (*Ki1)[i][j] = (*Ki)[i][j];
	}
    }
}

void plot(typ **Ki, int **index,int size, char *files)
{
  int i,j;   ;
  FILE *fic;
  typ **matrix = (typ**) calloc(size,sizeof(typ*));
  fic = fopen(files,"w");
 
  fprintf(fic,"%i \n\n",size); 
  for ( i=0; i<size; i++)
    {
      matrix[i] = (typ* ) calloc(size,sizeof(typ));
      for ( j = 1; j < index[i][0]; j++)
	{
	  matrix[i][(index[i][j])] = Ki[i][j];
	}
      matrix[i][i] = Ki[i][0];
    }
  for ( i=0; i<size; i++)
    {
      for ( j=0; j<size; j++)
	{
	  fprintf(fic," %f  ",matrix[i][j]);
	}
      fprintf(fic,"\n");
    }
  free(matrix);
}

void plot(typ **Ki, int **index,int size)
{
  return;
  int i,j;   
  typ **matrix = (typ**) calloc(size,sizeof(typ*));
//  ofstream out(file);
  
  printf("\n  Matrice de l'operateur \n\n"); 
  
//  cout << size << "\n";
  for ( i=0; i<size; i++)
    {
      matrix[i] = (typ* ) calloc(size,sizeof(typ));
      for ( j = 1; j < index[i][0]; j++)
	{
	  matrix[i][(index[i][j])] = Ki[i][j];
	}
      matrix[i][i] = Ki[i][0];
    }
  for ( i=0; i<size; i++)
    {
      for ( j=0; j<size; j++)
	{
	  printf(" %f", matrix[i][j]);
	}
      printf("\n");
    }
  free(matrix);
}
