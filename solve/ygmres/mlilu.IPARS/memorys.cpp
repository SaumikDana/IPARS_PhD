#include "library.h"


/* MEMORY INITIALISATION */

void Init_memory(int *size, int pack, int *level, int **size_tab, int **tab[3], spec_noeud **spec_tab, ensemble **lens[3], int *init)
{
  (*size_tab)[0] = *size;  // initial size
 ( *size_tab)[1] = 0;     // size of F nodes
  (*size_tab)[2] = 0;     // size of C nodes
  (*size_tab)[3] = 0;     // size of Wi
  (*size_tab)[4] = 0;  // size of Qi
  (*size_tab)[5] = *size;  // size of Neighbours
  (*size_tab)[6] = *size;  // size of parents
  (*size_tab)[7] = *size;  // size of optimal parents
  (*size_tab)[8] = *size;// size of resting Nodes
  (*size_tab)[9] = pack;
  if (*init)
    {
      (*size_tab)[10] = *level;
      (*size_tab)[11] = *size;
    }


  (*tab)[0] = (int*) malloc(sizeof(int)*(*size)); // Resting nodes
  (*tab)[1] = NULL; //(int*) malloc(sizeof(int)); // F-nodes 
  (*tab)[2] = NULL; //(int*) malloc(sizeof(int)); //C-nodes
//  tab[3] = new int; // Wi
//  tab[4] = new int; // Qi

 /* (*spec_tab)[0] =  (spec_noeud*) malloc(sizeof(spec_noeud)*(*size)); // Neighbours
  (*spec_tab)[1] = (spec_noeud*) malloc(sizeof(spec_noeud)*(*size)); // Parents
  (*spec_tab)[2] = (spec_noeud*) malloc(sizeof(spec_noeud)*(*size)); // Optimal parents*/

(*spec_tab) = (spec_noeud*) malloc(sizeof(spec_noeud)*(*size));
 (*lens)[0] =  (ensemble*) malloc(sizeof(ensemble)*(*size)); // Neighbours
  (*lens)[1] = (ensemble*) malloc(sizeof(ensemble)*(*size)); // Parents
  (*lens)[2] = (ensemble*) malloc(sizeof(ensemble)*(*size)); // Optimal parents*/*(*size));// Initial nodes

}

void reset_memory(int **tab[3], spec_noeud **spec_tab,  ensemble *(*lens[3]), int size)
{
  int i;
  /*
  (*tab)[0] = NULL; // Resting nodes
  (*tab)[1] = NULL; // F-nodes 
  (*tab)[2] = NULL; //C-nodes
  (*spec_tab)[0] =NULL; // Neighbours
  (*spec_tab)[1] =NULL; // Parents
  (*spec_tab)[2] =NULL; // Optimal parents
  (*spec_tab)[3] =NULL;// Initial nodes
  */
  
  

  free((*tab)[0]);
  free((*tab)[1]);
  free((*tab)[2]);
  for (i=0; i< size; i++)
    {
      free((*spec_tab)[i].liste_noeud);
      if ( ((*lens)[1])[i].liste != NULL)
	{
	  free(((*lens)[1])[i].liste);
	}
      free(((*lens)[0])[i].liste);
      if  (((*lens)[2])[i].liste != NULL )
	{
	  free(((*lens)[2])[i].liste);
	}
    }
  free((*spec_tab));
  free((*lens)[0]);
  free((*lens)[1]);
  free((*lens)[2]);
  return;
}
