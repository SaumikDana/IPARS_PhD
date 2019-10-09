#include "library.h"

void redistribute_parents( ensemble *P,int size_F, int *Fnodes, int *nodes)
{
  int i,j;
 
  for ( i = 0; i< size_F; i++)
    {
      int size_par = P[Fnodes[i]].nbre;
      for ( j = 0; j < size_par; j++)
	{
	  (P[Fnodes[i]]).liste[j] = nodes[(P[Fnodes[i]]).liste[j]];
	}
      
    }
}
