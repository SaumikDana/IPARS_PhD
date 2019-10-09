/* USE TEMPLATED MEMORY ALLOCATION */

#define _MEMT_

#include "library.h"

/*********************************************************************/

// Templated pointeur destructor

template <class type>
void kill(type *ptr) {  ptr =NULL; }//delete[] ptr;

/* Unreferenced pointers */

template <class type>
void kill2(type *ptr) { delete[] ptr;}

/* Unreferenced C pointers */

template <class type>
void killC(type *ptr) {  free(ptr);}


/********************************************************************/

// Templated pointor allocation 

template<class type>
void allocate(int pack, int size_pt, type **ptr)
{

  if (size_pt != 0)
    {
      *ptr = (type*) realloc(*ptr, (size_pt + pack)*sizeof(type));
    }
  else
    *ptr = (type*) malloc((size_pt + pack)*sizeof(type));
}

/********************************************************************/

// Function checks size of the pointor and reallocate if necessary

template<class type>
void check_size( type **ptr,int size_pt, int pack) 
{
//  cout << "Test allocation2 \n";
  if  ((size_pt)%pack == 0) allocate(pack,size_pt,ptr);
} 
    
/*********************************************************************/

template <class type>
void update_ptr( type **ptr, type **new_ptr ) { *ptr = *new_ptr; } 
