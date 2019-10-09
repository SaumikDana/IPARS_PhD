//====================================================================
// file : memman3.c
// contains : routines necessary for the Multi Model extension
// of the IPARS framework
//
// code history:
// MPeszynska, 9-10/98,    initial version
// SGT,        ----/07-09  mods for trchem, etc..
//

#include <stdlib.h>
#include <stdarg.h>

#include "memory.h"
#include "cfsimple.h"

_F_EXTERN_(void) _get_maxanam (
		   _F_INTEGER *val
);

/* ------------------------------------------------------------------ */
#define _c_setblks     _F_NAME_(C_SETBLKS,c_setblks)
_F_EXTERN_(void) _c_setblks(_F_INTEGER *model_array,_F_INTEGER *nproc);
_F_EXTERN_(void) _c_setblks(_F_INTEGER *model_array, _F_INTEGER *nproc)
{
  int i;

  for (i=0;i< MAXBLK;i++) blkmodel_c[i]=0;

  for (i=0;i< MAXBLK;i++) blkmodel_c [i] = model_array[i];

}

#define _c_setmodel    _F_NAME_(C_SETMODEL,c_setmodel)
_F_EXTERN_(void) _c_setmodel(_F_INTEGER *nmodel);
/* ------------------------------------------------------------------ */
_F_EXTERN_(void) _c_setmodel(_F_INTEGER *nmodel)
{
  CurrentModel = *nmodel;
}

#define _get_arymodel     _F_NAME_(GET_ARYMODEL,get_arymodel)
int _get_arymodel(int narr) {
  if ( (narr >=0) && (narr <=MAXARY ) )
    return arymodel[narr];
  else return -1;
}

#define _chg_aname       _F_NAME_(CHG_ANAME,chg_aname)
_F_EXTERN_(void)  _chg_aname (
                              _F_INTEGER *arynum,
                              char *varynam, _F_INTEGER varlen
                              );

//--------------------------------------------------
void _chg_aname (_F_INTEGER *arynum,char *varynam, _F_INTEGER varlen) {
  //--------------------------------------------------
  // changes name of an array of grid array number *arynum
  // to string *varynam
  // routine useful for arrays with 4'th dimension
  // --------------------------------------------------
  const _F_INTEGER na = *arynum;
  int i;

  for (i = 0; i < MAXANAM; i++)
    {
      const char c = varynam[i];
      arynam[na][i] = ' ';
      if ((c == ' ') || (c == '[')) break;
      arynam[na][i] = c;
    }

  if (MAXANAM > 0) arynam[na][MAXANAM]='\0';

  // printf("Chaging name for %d <%s> \n",na,arynam[na]);
}

#define _Alcga_Unpack       _F_NAME_(ALCGA_UNPACK,alcga_unpack)
_F_EXTERN_(void)  _Alcga_Unpack (
             _F_INTEGER * arynum, _F_INTEGER *arynumarr, _F_INTEGER *err);

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
void _Alcga_Unpack(_F_INTEGER * arynum,
                   _F_INTEGER *arynumarr, _F_INTEGER *err)
{
// *******************************************************************
// arynum = Array number (output) for the whole array
// arynumarr = Array numbers associated with the
//              individual components of the arrays 1.. ndim4

// err = Error number (output)
//     = 0 ==> no error
//     = 470 ==> invalid call
//     = 471 ==> too many grid arrays
//     = 472 ==>
// *******************************************************************
  const _F_INTEGER na = *arynum;
  const _F_INTEGER ncomp = dimext[na];
  _F_INTEGER nb, nc, nna;

  //------------------------------
  // check if the call makes sense
  if ( (ncomp<1) || (na <1) || (na >= MAXARY) )
    {
      *err = 470;
      return;
    }

  // -----------------------------
  // find the first available index of grid array that can be used

  for (nna = 0; (nna < MAXARY) && (arytyp[nna] != 0); nna++);

  nna--;
  // now nna is the last used grid array number

  // need 1+ncomp indices to record whole array
  // and all individual components

  if ((nna <0) || (nna + ncomp >= MAXARY))
    {
      *err = 471;
      return;
    }

  // =======================================================
  // mpesz: postprocess allocation by alcgea
  // in order to make available pointers to individual components
  // these pointers are symbolically numbered na+1, na+2 ... na+ncomp
  // no memory is allocated but pointers to previously allocated memory
  // are used.
  // each component has  memory of size mysize =
  //               idim[nb] * jdim[nb] * kdim[nb] * varlen[na]
  // the pointers to the memory are at
  //                aryadd[nb][na] + mysize*(ncomp-1)


  for(nc=1;nc<=ncomp;nc++) {
    const int myna= nna + nc ;

    //--------------------------
    // most of information aboud grid array is copied verbatim
    aryfrm[myna]=aryfrm[na];
    varlen[myna]=varlen[na];
    arytyp[myna]=arytyp[na];
    arymodel[myna]=arymodel[na];

    // ndim4 is 1 because we only
    dimext[myna]=1;

    // find pointers to memory on each block
    for (nb = 0; nb < numblks; nb++)
      if ( (*modact==0) || (modblk[nb] == *modact)
      || (fmodblk[nb] == *modact)
      && (myelem[nb] > 0) && (aryadd[nb][na] != NULL))     {
        const int mysize = idim[nb] * jdim[nb] * kdim[nb] * varlen[na] ;
        aryadd[nb][myna] = (void *)(
                                    ((char *)aryadd[nb][na] + mysize *(nc-1))
                                    );
      } else {
        aryadd[nb][myna] = aryadd[nb][na];
      }
    arynumarr [nc-1] = myna;

    // find and copy name (the name can be changed later)
    {char *s=strncpy((char *)arynam[myna],(char *)arynam[na],MAXANAM);}
  }
  numarys+=ncomp;
  // =======================================================
  *err = 0;
  return;
}


#define _get_arynum    _F_NAME_(GET_ARYNUM,get_arynum)
/* ------------------------------------------------------------------ */
// bag8: add no optimize attribute for this function
void __attribute__((optimize("O0"))) _get_arynum (
                  _F_INTEGER * ndim4, _F_INTEGER *arynum,_F_INTEGER *NERR,
                  char *varynam, _F_INTEGER * varlen
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

  if(*varlen > MAXANAM) {*NERR=-1;return;}

  auxname[0]='\0';strncat(auxname, varynam, *varlen);
  auxname[*varlen+1]='\0';cstring=auxname;

 // printf("\nParameter <%s> of len=%d\n",auxname,varlen);

  na = 0;
  while( (good ==0) && (na < numarys) ){
    i = 0;
    while ( ( found == 0) && (i < MAXANAM) ) {
      if (((cstring[i] == ' ') || (cstring[i] == '[')) &&
          (arynam[na][i] == ' ')) found =1;
      if (cstring[i] == '\0') break;
      if (cstring[i] != arynam[na][i]) break;
      if (i == (MAXANAM - 1)) found =1;
      i++;
    }

    if (found==1) { // verify if the model is consistent
      if (
          ( (arypmod[na] == 0) || (arypmod[na] == CurrentModel)
         || (arypmod[na] == *modact )
//          ) && ( aryfrm[na] == 2 )       // bag8
          )
          ) good =1;
      else
        found = 0; // keep looking for the grid array

//      printf("\nFound %d good =%d\n",na,good);

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


#define _get_currentmodel _F_NAME_(GET_CURRENTMODEL,get_currentmodel)
_F_EXTERN_(_F_INTEGER) _get_currentmodel();
/* ------------------------------------------------------------------ */
int _get_currentmodel() {
  return CurrentModel;
}
