//#define _REC_T_
#include "library.h"

//typedef typ  double;

#include <iostream.h>

//template<class typ>
void reccursive_graph(typ ***matrix, int ***index,typ ***mat_init, int ***index_init, double sigma, int **size_tab, int *(*tab[3]), spec_noeud **spec_tab, ensemble *(*lens[3]), int *size, int pack, int *level, int *init,double delta)
{
    

  int i, lvi;
  //  int size_mat;

//  element **mat_temp = (element**) new element*;
  ensemble *P = NULL;
 extern int *levels;
   extern int sizeFtot;

  if (*level == 0) 
    { 
      if (*init) 
	{
	  Init_memory(size,pack,level,size_tab,tab,spec_tab,lens,init);
	  return;
	}
      else {
	/* MODIFICATION OF THE LABELLING */
	*level = (*size_tab)[10];
	return;
	      }
    }
  if (*init) 
    {
      levels[0] = 0;
      levels[24]=*size;
      sizeFtot=0;
    }
  
  Init_memory(size,pack,level,size_tab,tab,spec_tab,lens,init);
  
 
  lvi = (*size_tab)[10];

  
 
  Mark_Nodes(*matrix,*index,sigma,size_tab,tab,spec_tab,lens);


   //  free(C_nodes);
    
      
    //  cout << "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSS " << size_F;
  int sizeF = (*size_tab)[1] ;
  int sizeC = (*size_tab)[2];
  int *cur_C = (*tab)[2];
  int *cur_F = (*tab)[1];
  int j;
  /*
  cout << "\n Noeuds F: \n";
  
  for (j=0; j<sizeF; j++)
    {
      cout << cur_F[j] << " ,";
    }
  
  cout << "\n\n  Noeuds C: \n";
  for (j=0; j<sizeC; j++)
    {
      cout << cur_C[j] << " ,";
    }
      
  P = (*lens)[1];
  cout << "\n\nListe des parents: \n";
  for (j=0; j< (sizeF+ sizeC); j++)
    {
      cout << "Noeud  " << j << " : ";
      int nb = P[j].nbre;
      for (int k=0; k< nb; k++)
	{
	  cout << (P[j]).liste[k]                  << ", ";
	}
      cout << "\n";
    }
  cout << "\n\n";
  */
  P = (*lens)[1];


  extern int **indices;
  extern int *temporaire;
  
  indices[lvi-((*level))] = (int*) malloc(sizeof(int)*( (*size_tab)[1]+ (*size_tab)[2]));
 
  for ( i=0; i< (*size_tab)[1]; i++)
    {
      indices[lvi-((*level))][i] = cur_F[i] ;
      temporaire[cur_F[i]] = i;
    }
   for ( i=0; i< (*size_tab)[2]; i++)
    {
      indices[lvi-((*level))][i+ (*size_tab)[1]] =  cur_C[(*size_tab)[2]-1-i] ;
      temporaire[cur_C[(*size_tab)[2]-1-i]] = i+(*size_tab)[1];
    }

// CASE OF NO FINE NODES FOUND
  
  if ( (*size_tab)[2] == (*size_tab)[11] )
    {
      printf(" Reordering was not effective : No Fine nodes \n \n");
      exit(1);
    }
  

// CASE OF NO MORE COARSE NODES BUT LASTING LEVELS TO BE PROCEEDED

  extern int size_min; 
  if ( ( ( (*size_tab)[2] < size_min )|| ( (*size_tab)[1]   == 0) ||  ((*size_tab)[2] == (*size_tab)[11] )) && (!(*init)) && (*level != 1))
    {
   //   printf(" \n Too many levels entered. Maximum level reached:  %d  \n",lvi - *level +1);
      (*size_tab)[10] = lvi - *level +1;
      *level = 1;
      lvi = (*size_tab)[10];
      levels[lvi+1] = *size;
    }

  //  update_matrix(matrix,index,cur_C, (*size_tab)[2], size_mat, pack);

  extern int ***indKi;
  extern typ ***matKi;
  extern int *sauvtaille;

  if ( *level  != 0)
    {
      update_matrix2(matrix,index,cur_C,sizeC,cur_F,sizeF,pack,delta,spec_tab,sizeFtot,P,lvi,level);
      sauvtaille[lvi-(*level)] = (*size_tab)[2];
    }

  if (*init) 
    {
      
      *init = 0;
    } 
  else 
    { 
    } 
  int olds = *size;
  sizeFtot = sizeFtot + sizeF;
  *size = sizeC;
 // cout.flush() << " LEVELE  " << lvi-(*level)+1 << " \n ";
  levels[lvi-(--(*level))] = sizeFtot;
  reset_memory(tab,spec_tab,lens,olds);
  reccursive_graph(matrix,index, mat_init,index_init,sigma,size_tab,tab,spec_tab,lens,size,pack,level, init,delta);
}

//template<class typ>


/*****************************************************************/

/* CREATE DESCENDING MATRICES */

void update_matrix2( typ ***matrix, int ***index, int *Cnodes, int sizeC, int *Fnodes, int sizeF, int pack, double delta, spec_noeud **spec_tab , int old_F,ensemble *P,int lvi, int *level)
{
  int sizet = sizeC+sizeF;
  int ll,ii,i,j,i0,j0,ii0,jj0,size;
  typ *aij, *bij;
  typ val, res;
  
  
 typ **matrixt = (typ**) malloc(sizeof(typ*)*sizet);
  int **indext = (int**) malloc(sizeof(int*)*sizet);

  extern int *temporaire;

   reorganize_transpose(matrix,index,temporaire,sizet,&matrixt,&indext,P,sizeF,Fnodes);
 // redistribute_parents(P, sizeF, Fnodes,temporaire);
 
  extern typ ***LU;
  extern int ***indexLU;
 
  aij = (typ*) malloc(sizeof(typ)*20);
  bij = (typ*) malloc(sizeof(typ)*20);
  for ( i = 0; i< sizeF; i++)
    {
      ll = i+old_F;
     createLU(i,old_F,*matrix,*index,matrixt,indext);
   
      (*LU)[ll][0] = (*matrix)[i][0];
      size = P[Fnodes[i]].nbre;
      for ( j = 0; j < size; j++)
	{
	  j0 = (P[Fnodes[i]]).liste[j];
//	  cout << " PARENTS "  << ((*Res_Parents)[i])[j] << "\n";
	  aij[j] =eval_aij(i,j0,P,*matrix, *index,sizeF ,sizeF+sizeC, delta, 0,sizeF,Fnodes);
	  bij[j] = eval_bij(i,j0,P,matrixt, indext, sizeF , sizeF+sizeC,delta,0,sizeF,Fnodes);
//	  cout << "i : " << i << " j: " << j0 << "     aij " << aij[j] << "    bij " << bij[j] << "\n";
	}
       for ( j=1; j< (*index)[i][0]; j++)
	{
	  j0 = (*index)[i][j];
	  if ( j0 > i)
	    {
	      val = (*matrix)[i][j]; 
	      for ( i0= 0; i0< size; i0++)
		{
		  jj0 =(P[Fnodes[i]]).liste[i0];
		  res = - (1/(*matrix)[i][0]) * ( bij[i0] * val);
		  create(jj0 ,j0,matrix,index,res,&matrixt,&indext);
		}
	    }
	}
       for ( j=1; j< (indext)[i][0]; j++)
	{
	  j0 = (indext)[i][j];
	  if ( j0 > i)
	    {
	      val = (matrixt)[i][j]; 
	      for ( i0= 0; i0< size; i0++)
		{
		  jj0 = (P[Fnodes[i]]).liste[i0];
		  res = - (1/(*matrix)[i][0]) * ( aij[i0] * val);
		  create(j0,jj0,matrix,index,res,&matrixt,&indext);
		}
	    }
	}
       for ( i0= 0; i0< size; i0++)
	 {
	   jj0 = (P[Fnodes[i]]).liste[i0];
	   for ( j = 0; j< size; j++)
	     {
	       ii0 = (P[Fnodes[i]]).liste[j];
	       res = (1/(*matrix)[i][0]) * ( aij[i0] * bij[j]);
	       create(ii0,jj0,matrix,index,res);
	     }
	 }
    }
  free(aij);
  free(bij);

  /***************************************************************/
  
 
  extern int ***indKi;
  extern typ ***matKi;
  extern int *sauvtaille;
  int intmax,cpt;
 
 (matKi[lvi-(*level)]) = (typ**) malloc(sizeof(typ*)*(sizeC));
  (indKi[lvi -(*level)]) = (int**) malloc(sizeof(int*)*(sizeC));

  for ( i=sizeF;  i < sizet; i++)
    { 
      cpt = 1;
      i0 = i - sizeF;
      if ( i0 < sizeF )
	{
	  free(matrixt[i0]);
	  free(indext[i0]);
	}
      free(matrixt[i]);
      free(indext[i]);
      if ( (*index)[i0][0] < (*index)[i][0])
	{
	  intmax = (*index)[i][0]/7;
	  intmax = (intmax+1)*7;
	  (*index)[i0] = (int*) realloc((*index)[i0],sizeof(int)*intmax);
	  (*matrix)[i0] = (typ*) realloc((*matrix)[i0],sizeof(typ)*intmax);
	}
      (matKi[lvi-(*level)])[i0] =  (typ*) malloc(sizeof(typ) *(*index)[i][0]);
      (indKi[lvi -(*level)])[i0] =  (int*) malloc(sizeof(int)*(*index)[i][0]);
      
      for ( j=1; j < (*index)[i][0]; j++)
	{    
	  if ( ((*index)[i][j] >= sizeF) && ( (*matrix)[i][j] != 0 ))
	    {
	      (*index)[i0][cpt] = (*index)[i][j]-sizeF;
	      (*matrix)[i0][cpt] = (*matrix)[i][j];
	      (matKi[lvi-(*level)])[i0][cpt] = (*matrix)[i][j];
	      (indKi[lvi -(*level)][i0])[cpt++] = (*index)[i][j]-sizeF;
	    }
	}
      (*matrix)[i0][0] = (*matrix)[i][0];
      (*index)[i0][0] = cpt;
      (matKi[lvi-(*level)])[i0][0] = (*matrix)[i][0];
      (indKi[lvi -(*level)])[i0][0] = cpt;
      (matKi[lvi-(*level)])[i0] =  (typ*) realloc((matKi[lvi-(*level)])[i0],sizeof(typ)*cpt);
      (indKi[lvi -(*level)])[i0] =  (int*) realloc( (indKi[lvi -(*level)])[i0],sizeof(int)*cpt);
    }

  free(matrixt);
  free(indext);

}


/*************************************************************/


//template<class typ>
void reorganize(typ ***matrix, int ***index, int *nodes, int size, int pack)
{
  int ii,j,i;
  typ **mat_temp = NULL;
  int **index_tmp = NULL;
  index_tmp = (int**) malloc(size*sizeof(int*));
  mat_temp = (typ**) malloc(size*sizeof(typ*));


  
  
  for ( i=0; i < size; i++)
    {
      ii = nodes[i];
      mat_temp[ii] = ( typ*) malloc((*index)[i][0]*sizeof(typ));
      index_tmp[ii] = ( int*) malloc(sizeof(int)*(*index)[i][0]);
      (mat_temp[ii])[0] = (*matrix)[i][0];
      (index_tmp[ii])[0] = ((*index)[i])[0];
      for ( j=1; j< (index_tmp[ii])[0]; j++)
	{
	  (index_tmp)[ii][j] = nodes[(*index)[i][j]];
	  (mat_temp)[ii][j] = (*matrix)[i][j];
	}
    } 
    
  int intmax;
  for ( i=0; i < size; i++)
    {
//      cout.flush() <<  " SIZE  " << (index_tmp)[i][0] << " ET  INIT "  << (*index)[i][0]  << "\n";
      if ( (index_tmp)[i][0] >  (*index)[i][0] )
	{
	 
	  intmax = int((index_tmp)[i][0]/ 7);
	//   cout.flush() << " CORRIGE A " << (intmax+1)*7 << "\n";
	  (*index)[i] = (int*) realloc((*index)[i],(intmax+1)*7*sizeof(int));
	  (*matrix)[i] = (typ*) realloc((*matrix)[i],(intmax+1)*7*sizeof(typ));
	}
   //   cout.flush() << " LES INDICES SONT " ;
      for ( j=0; j < (index_tmp)[i][0]; j++)
	{	  
//	  cout.flush() << j << ",  ";
	  (*index)[i][j] = (index_tmp)[i][j];
	  (*matrix)[i][j] = mat_temp[i][j];
	}   
//      cout.flush() << "\n";
    }
  
  for (i=0;i<size;i++)
    {
      free(mat_temp[i]);
      free(index_tmp[i]);
    }
  free( mat_temp );
  free( index_tmp );
  

}

void transpose(typ ***matrix, int ***index, typ ***matrixt, int ***indext,int size)
{
  int i,j,ii;
  

  for (i=0; i< size; i++)
    {
      (*matrixt)[i] = (typ*) malloc(7*sizeof(typ));
      (*indext)[i] = (int*) malloc(7*sizeof(int));
      (*indext)[i][0] = 1;
      (*matrixt)[i][0] = (*matrix)[i][0];
    }
  
  for (i=0; i< size; i++)
    {
      
      for ( j=1; j< (*index)[i][0];j++)
	{
	  ii = (*index)[i][j];
	  if ((( (*indext)[ii][0])%7) == 0 )
	    {
	      (*indext)[ii] = (int*) realloc((*indext)[ii],sizeof(int)*((*indext)[ii][0]+7));
	      (*matrixt)[ii] = (typ*) realloc((*matrixt)[ii], sizeof(typ)*((*indext)[ii][0]+7));
	    }
	  (*indext)[ii][(*indext)[ii][0]] = i;
	  ((*indext)[ii][0])= (*indext)[ii][0]+1;
	  (*matrixt)[ii][(*indext)[ii][0]-1] = (*matrix)[i][j];
	  
	}
    }
}

void transpose2(typ ***matrix, int ***index, typ ***matrixt, int ***indext,int size)
{
  int i,j,ii;
  
 
 // (*matrixt) = (typ**) realloc((*matrixt),sizeof(typ*)*size);
 // (*indext) = (int**) realloc((*indext),sizeof(int*)*size);
  

  for (i=0; i< size; i++)
    {
   //   (*matrixt)[i] = (typ*) malloc(7*2*30*sizeof(typ));
    //  (*indext)[i] = (int*) malloc(7*2*30*sizeof(int));
      (*indext)[i][0] = 1;
      (*matrixt)[i][0] = (*matrix)[i][0];
    }
  
  for (i=0; i< size; i++)
    {
      
      for ( j=1; j< (*index)[i][0];j++)
	{
	  ii = (*index)[i][j];
	 // (*indext)[ii] = (int*) realloc((*indext)[ii],sizeof(int)*((*indext)[ii][0]+1));
	 // (*matrixt)[ii] = (typ*) realloc((*matrixt)[ii], sizeof(typ)*((*indext)[ii][0]+1));
	  (*indext)[ii][(*indext)[ii][0]] = i;
	  ((*indext)[ii][0])= (*indext)[ii][0]+1;
	  (*matrixt)[ii][(*indext)[ii][0]-1] = (*matrix)[i][j];
	  
	}
    }
}

void reverse(int **Cnodes,int sizec)
{
 
  int i,temp;

  for (i=0; i< int(sizec/2);i++)
    {
      temp = (*Cnodes)[i];
      (*Cnodes)[i] = (*Cnodes)[sizec-1-i];
      (*Cnodes)[sizec-1-i] = temp;
    }
}


void reorganize_transpose(typ ***matrix, int ***index, int *nodes, int size, typ ***matrixt, int ***indext, ensemble *P,int size_F, int *Fnodes)
{
  int ii,j,i;
  typ **mat_temp = NULL;
  int **index_tmp = NULL;
  index_tmp = (int**) malloc(size*sizeof(int*));
  mat_temp = (typ**) malloc(size*sizeof(typ*));
 
  for ( i=0; i < size; i++)
    {
      if (i < size_F)
	{
	  int size_par = P[Fnodes[i]].nbre;
	  for ( j = 0; j < size_par; j++)
	    {
	      (P[Fnodes[i]]).liste[j] = nodes[(P[Fnodes[i]]).liste[j]];
	    }
	}
      ii = nodes[i];
      (*matrixt)[i] = (typ*) malloc(7*sizeof(typ));
      (*indext)[i] = (int*) malloc(7*sizeof(int));
      (*indext)[i][0] = 1;
      mat_temp[ii] = ( typ*) malloc((*index)[i][0]*sizeof(typ));
      index_tmp[ii] = ( int*) malloc(sizeof(int)*(*index)[i][0]);
      (mat_temp[ii])[0] = (*matrix)[i][0];
      (index_tmp[ii])[0] = ((*index)[i])[0];
      for ( j=1; j< (index_tmp[ii])[0]; j++)
	{
	  (index_tmp)[ii][j] = nodes[(*index)[i][j]];
	  (mat_temp)[ii][j] = (*matrix)[i][j];
	}
    } 
    
  int intmax;
  for ( i=0; i < size; i++)
    {
//      cout.flush() <<  " SIZE  " << (index_tmp)[i][0] << " ET  INIT "  << (*index)[i][0]  << "\n";
      if ( (index_tmp)[i][0] >  (*index)[i][0] )
	{
	 
	  intmax = int((index_tmp)[i][0]/ 7);
	//   cout.flush() << " CORRIGE A " << (intmax+1)*7 << "\n";
	  (*index)[i] = (int*) realloc((*index)[i],(intmax+1)*7*sizeof(int));
	  (*matrix)[i] = (typ*) realloc((*matrix)[i],(intmax+1)*7*sizeof(typ));
	}
   //   cout.flush() << " LES INDICES SONT " ;
      for ( j=1; j < (index_tmp)[i][0]; j++)
	{	  
//	  cout.flush() << j << ",  ";
	  (*index)[i][j] = (index_tmp)[i][j];
	  (*matrix)[i][j] = mat_temp[i][j];
	  ii = (*index)[i][j];
	  if ((( (*indext)[ii][0])%7) == 0 )
	    {
	      (*indext)[ii] = (int*) realloc((*indext)[ii],sizeof(int)*((*indext)[ii][0]+7));
	      (*matrixt)[ii] = (typ*) realloc((*matrixt)[ii], sizeof(typ)*((*indext)[ii][0]+7));
	    }
	  (*indext)[ii][(*indext)[ii][0]] = i;
	  ((*indext)[ii][0])= (*indext)[ii][0]+1;
	  (*matrixt)[ii][(*indext)[ii][0]-1] = (*matrix)[i][j];
	  
	}
      (*index)[i][0] = (index_tmp)[i][0];
      (*matrix)[i][0] = mat_temp[i][0];
      (*matrixt)[i][0] = (*matrix)[i][0];
      free(mat_temp[i]);
      free(index_tmp[i]);

   
//      cout.flush() << "\n";
    }
  
  free( mat_temp );
  free( index_tmp );
  

}
	 
