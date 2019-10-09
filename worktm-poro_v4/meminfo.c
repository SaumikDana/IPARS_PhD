#include "memory.h"
#include "cfsimple.h"

/* ------------------------------------------------------------------ */

#define _nblkary    _F_NAME_(NBLKARY,nblkary)
#define _blklocal   _F_NAME_(BLKLOCAL,blklocal)
#define _aryinfo    _F_NAME_(ARYINFO,aryinfo)
#define _aryptr     _F_NAME_(ARYPTR,aryptr)
#define _layers     _F_NAME_(LAYERS,layers)
#define _ismyblk    _F_NAME_(IS_MYBLK,is_myblk)
#define _get_arydat _F_NAME_(GET_ARYDAT,get_arydat)
#define _get_maxanam _F_NAME_(GET_MAXANAM,get_maxanam)
		

_F_EXTERN_(void) _nblkary(
           _F_INTEGER * NUMBLK , _F_INTEGER * NUMARY
);

_F_EXTERN_(void) _blklocal(
           _F_INTEGER * NB , _F_INTEGER * IDIM , _F_INTEGER * JDIM ,
           _F_INTEGER * KDIM , _F_INTEGER * IOFF , _F_INTEGER * JOFF ,
           _F_INTEGER * KOFF , _F_INTEGER * IERR
);

_F_EXTERN_(void) _aryinfo(
           _F_INTEGER * NA , _F_INTEGER * KIND , _F_INTEGER * NDIM4 ,
           _F_INTEGER * IERR
 );

_F_EXTERN_(void) _aryptr(
           _F_INTEGER * NB , _F_INTEGER * NA ,
           void ** ptr , _F_INTEGER * IERR
);

_F_EXTERN_(void) _layers(
           _F_INTEGER * lay0 , _F_INTEGER * lay1, _F_INTEGER * lay2
);

_F_EXTERN_(_F_INTEGER) _IsMyBlk(_F_INTEGER *numblk);
_F_EXTERN_(void) _get_arydat(
	    _F_INTEGER *kind,
	    _F_INTEGER * ndim4, _F_INTEGER *arynum,_F_INTEGER *err,
	    char *varynam, _F_INTEGER varlen
);
_F_EXTERN_(void) _get_maxanam (
		   _F_INTEGER *val
);

/* ------------------------------------------------------------------ */

void _nblkary( _F_INTEGER * NUMBLK , _F_INTEGER * NUMARY )
{
  *NUMBLK = numblks ;
  *NUMARY = numarys ;
}

/* ------------------------------------------------------------------ */

void _blklocal(
  _F_INTEGER * NB ,
  _F_INTEGER * IDIM , _F_INTEGER * JDIM , _F_INTEGER * KDIM ,
  _F_INTEGER * IOFF , _F_INTEGER * JOFF , _F_INTEGER * KOFF ,
  _F_INTEGER * IERR )
{
  if ( 0 < *NB && *NB <= numblks ) {
    *IDIM = idim[ *NB - 1 ];
    *JDIM = jdim[ *NB - 1 ];
    *KDIM = kdim[ *NB - 1 ];
    *IOFF = iofflg[ *NB - 1 ];
    *JOFF = jofflg[ *NB - 1 ];
    *KOFF = kofflg[ *NB - 1 ];
    *IERR = 0 ;
  }
  else {
    *IERR = 1 ; /* IPARS: Invalid block */
  }
}

/* ------------------------------------------------------------------ */

void _aryinfo(
  _F_INTEGER * NA , _F_INTEGER * KIND , _F_INTEGER * NDIM4 ,
  _F_INTEGER * IERR )
{
  if ( 0 <= *NA && *NA < numarys ) {
    *KIND  = aryfrm[*NA] ;
    *NDIM4 = dimext[*NA] ;
    *IERR  = 0 ;
  }
  else {
    *IERR = 468 ; /* IPARS: invalid array */
  }
}

/* ------------------------------------------------------------------ */

void _aryptr( _F_INTEGER * NB , _F_INTEGER * NA ,
  void ** ptr , _F_INTEGER * IERR )
{
  if ( 0 < *NB && *NB <= numblks ) {
    if ( 0 <= *NA && *NA < numarys ) {
      *ptr = aryadd[*NB-1][*NA] ;
      *IERR = 0 ;
    }
    else {
      *IERR = 468 ; /* IPARS: invalid array */
    }
  }
  else {
    *IERR = 1 ; /* IPARS: invalid block */
  }
}

/* ------------------------------------------------------------------ */
void _layers( _F_INTEGER * lay0 , _F_INTEGER * lay1, _F_INTEGER * lay2 )
{
  *lay0 = layer[0];*lay1=layer[1];*lay2=layer[2];
}

/* ------------------------------------------------------------------ */
_F_INTEGER _ismyblk(_F_INTEGER *numblk)
  // returns 1 if this block is active on the current processor
  // 0 otherwise
{
const int nblk=*numblk;
const int retval = myelem[nblk];
if (retval >0)
  return 1; else return 0;
}


/* ------------------------------------------------------------------ */
void _get_arydat (
		  _F_INTEGER *kind,
		  _F_INTEGER * ndim4, _F_INTEGER *arynum,_F_INTEGER *NERR,
		  char *varynam, _F_INTEGER varlen
)
{
// *******************************************************************

// Returns info for an existing array in the memory management system

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters) (input)

// kind = Data type of the array (output)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim4 = Product of the 4th and higher dimensions (output)
//       = 1 ==> no higher dimensions

// arynum = Array number (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 466 ==> invalid name

// *******************************************************************
int na, i;
char auxname[MAXANAM+1],*cstring;

if(varlen > MAXANAM) {*NERR=-1;return;}

auxname[0]='\0';strncat(auxname, varynam, varlen);
auxname[varlen+1]='\0';
cstring=varynam;

//printf("\nParameter <%s> of len=%d\n",cstring,varlen);

for (na = 0; na < numarys; na++)
   {
   for (i = 0; i < MAXANAM; i++)
      {
      if (((cstring[i] == ' ') || (cstring[i] == '[')) &&
         (arynam[na][i] == ' ')) goto aok;
      if (cstring[i] != arynam[na][i]) break;
      if (i == (MAXANAM - 1)) goto aok;
      }
   }
*NERR = 466;
return;

aok:
//printf("\nParameters: %s of len=%d ? %s\n",cstring,varlen,arynam[na][i]);

*kind = aryfrm[na];
*ndim4 = dimext[na];
*arynum = na;
*NERR = 0;
return;

}

/* ------------------------------------------------------------------ */
void _get_maxanam (
		   _F_INTEGER *val
		   )
{
  *val = MAXANAM;
}


/* ------------------------------------------------------------------ */
void _get_arynum (
		  _F_INTEGER * ndim4, _F_INTEGER *arynum,_F_INTEGER *NERR,
		  char *varynam, _F_INTEGER varlen
)
{
// *******************************************************************
// Essential for visulization routines for Multi Model
// Returns info for an existing array in the memory management system
// works almost exactly like get_arydat except that it also checks
// the model for which the array was allocated:

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters) (input)

// ndim4 = Product of the 4th and higher dimensions (output)
//       = 1 ==> no higher dimensions

// arynum = Array number (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 466 ==> invalid name

// *******************************************************************
  int na, i;
  char auxname[MAXANAM+1],*cstring;
  int found = 0, good = 0;

  if(varlen > MAXANAM) {*NERR=-1;return;}

  auxname[0]='\0';strncat(auxname, varynam, varlen);
  auxname[varlen+1]='\0';cstring=varynam;

  // printf("\nParameter <%s> of len=%d\n",cstring,varlen);

  na = 0;
  while( (good ==0) && (na < numarys) ){
    i = 0;
    while ( ( found == 0) && (i < MAXANAM) ) {
      if (((cstring[i] == ' ') || (cstring[i] == '[')) &&
	  (arynam[na][i] == ' ')) found =1;
      if (cstring[i] != arynam[na][i]) break;
      if (i == (MAXANAM - 1)) found =1;
      i++;
    }

    if (found==1) { // verify if the model is consistent
      if (
	  ( (arymodel[na] == 0) || (arymodel[na] == *modact)
        ||  (arymodel[na] == CurrentModel) //sgt
          ) && ( aryfrm[na] == 2 )
	  ) good =1;
      else
	found = 0; // keep looking for the grid array
    }
    if (found ==0) na ++;
    }
  if (good ==0) {
    *NERR = 466; return;
  } else {
    *ndim4 = dimext[na];   *arynum = na;    *NERR = 0;
    return;
  }
}









