// External variable routines for COMP package

// HISTORY:

// JOHN WHEELER       2/4/95    ORIGINAL BETA CODE
// JOHN WHEELER      6/10/00    MOVED MACHINE TYPE DEFINITIONS TO ctypes.h
// SUNIL G THOMAS    --/--/--   MODS FOR 32-64 BIT CONVERSION

#include "ctypes.h"

// ROUTINES IN THIS MODULE:

FORTSUB extdef_ (PADD vext, PINT4 vtyp, PCHAR vnam, PINT4 vdim1,
        PINT4 vdim2, PINT4 vdim3, PINT4 err);
FORTSUB extnum_ (PCHAR vnam, PINT4 vnum, PINT4 vind1, PINT4 vind2,
        PINT4 vind3);
FORTSUB extget_ (PREAL8 varb, PINT4 vnum, PINT4 vind1, PINT4 vind2,
        PINT4 vind3, PINT4 err);
FORTSUB extput_ (PREAL8 varb, PINT4 vnum, PINT4 vind1, PINT4 vind2,
        PINT4 vind3, PINT4 err);
FORTSUB extdel_ (PINT4 vnum1, PINT4 vnum2);
FORTSUB extnam_ (PINT4 vnum, PCHAR vnam);
FORTSUB clearev_ ();

// NOTES:

//   1) Compile with large memory model on IBM PC

//   2) Many fortran compilers use a stack for local variables.  Memory for
//      such variables is freed when the routine is exited.  Some compilers
//      even reuse a local variable's memory location after the last
//      statement inwhich the variable is referenced; this is consistant with
//      FORTRAN standards but not the aliasing implied by this code module.
//      To avoid problems do one of the following:
//      1)  Place the variable in a COMMOM area.
//      2)  Declare the variable to be static in the defining routine.
//      3)  Initialize the variable in a DATA statement.

// MODULE DATA:

#define MAXEXT  50     // Maximum number of external variables
#define MAXNAM    6         // Maximum external variable name length

int   numdef = 0;           // Number of external variables defined
CHAR  nam[MAXEXT][MAXNAM];  // External variable names
void  *pnt[MAXEXT];         // External variable base addresses
int  typ[MAXEXT];           // External variable data types
int  dim[MAXEXT][3];        // External variable dimensions

// *******************************************************************
FORTSUB extdef_ (PADD vext, PINT4 vtyp, PCHAR vnam, PINT4 vdim1,
        PINT4 vdim2, PINT4 vdim3, PINT4 err)
{
// *******************************************************************

//  Records location and definition of an external variable

//  vext  = Base address of the variable (input, void)

//  vtyp  = Fortran variable type (input, INTEGER)
//        = 1 ==> REAL*8
//        = 2 ==> REAL*4
//        = 3 ==> INTEGER
//        = 4 ==> INTEGER
//        = 5 ==> LOGICAL
//        = 6 ==> LOGICAL

//  vname = variable name (input, CHARACTER*MAXNAM)

//  vdim1 = array dimensions (base 1) (input, INTEGER)
//  vdim2   0 for scalars or unused dimensions
//  vdim3

//  err   = error number (output, INTEGER)
//        = 0 ==> no error
//        = 1 ==> max number of external variables exceeded

// *******************************************************************
int i;

if (numdef == (MAXEXT - 1))
   {
   *err = 1;
   return;
   }
pnt[numdef] = vext;
typ[numdef] = *vtyp;
for (i = 0; i < MAXNAM; i++) nam[numdef][i] = vnam[i];
dim[numdef][0] = *vdim1;
dim[numdef][1] = *vdim2;
dim[numdef][2] = *vdim3;
numdef++;
*err = 0;
return;
}

// *******************************************************************
FORTSUB extnum_ (PCHAR vnam, PINT4 vnum, PINT4 vdim1, PINT4 vdim2,
        PINT4 vdim3)
{
// *******************************************************************

//  Given an external variable name, returns the variable number and
//  dimensions

//  vnam  = variable name (input, CHARACTER*MAXNAM)

//  vnum  = variable number (output, INTEGER)
//        = -1 if the name is not found

//  vdim1 = array dimensions (base 1) (input, INTEGER)
//  vdim2   0 for scalars or unused dimensions
//  vdim3

// *******************************************************************
int i, ii;

for (i = 0; i < numdef; i++)
   {
   for (ii = 0; ii < MAXNAM; ii++)
       if (vnam[ii] != nam[i][ii]) goto nomat;
   *vnum = i;
   *vdim1 = dim[i][0];
   *vdim2 = dim[i][1];
   *vdim3 = dim[i][2];
   return;
nomat:;
   }
*vnum = -1;
return;
}

// *******************************************************************
FORTSUB extget_ (PREAL8 varb, PINT4 vnum, PINT4 vind1, PINT4 vind2,
        PINT4 vind3, PINT4 err)
{
// *******************************************************************

// Set a REAL*8 equal to an external variable

// varb  = variable to be set (output, REAL*8)

// vnum  = external variable number (base 0) (input, INTEGER)

//  vind1 = external vairable indexs (base 1) (input, INTEGER)
//  vind2   0 for scalars or unused indexes
//  vind3

// err  = error flag
//       = 0 ==> no error
//       = 1 ==> vind1 index out of range
//       = 2 ==> vind2 index out of range
//       = 3 ==> vind3 index out of range
//       = 4 ==> wrong number of indexes

// *******************************************************************
//long loff; //sgt
int loff; //sgt

if (dim[*vnum][0] == 0)
   {
   if (*vind1 != 0)
      {
      *err = 4;
      return;
      }
   loff = 0;
   goto doev;
   }
else
   {
   if ((*vind1 < 1) || (*vind1 > dim[*vnum][0]))
      {
      *err = 1;
      return;
      }
   loff = *vind1 - 1;
   }

if (dim[*vnum][1] == 0)
   {
   if (*vind2 != 0)
      {
      *err = 4;
      return;
      }
   goto doev;
   }
else
   {
   if ((*vind2 < 1) || (*vind2 > dim[*vnum][1]))
      {
      *err = 2;
      return;
      }
   loff += dim[*vnum][0] * (*vind2 - 1);
   }

if (dim[*vnum][2] == 0)
   {
   if (*vind3 != 0)
      {
      *err = 4;
      return;
      }
   }
else
   {
   if ((*vind3 < 1) || (*vind3 > dim[*vnum][2]))
      {
      *err = 3;
      return;
      }
   loff += dim[*vnum][0] * dim[*vnum][1] * (*vind3 - 1);
   }

doev:
switch(typ[*vnum])
   {
   case 1:
      *varb = *(((PREAL8) pnt[*vnum]) + loff);
      break;
   case 2:
      *varb = *(((PREAL4) pnt[*vnum]) + loff);
      break;
   case 3:
      *varb = *(((PINT4) pnt[*vnum]) + loff);
      break;
   case 4:
      *varb = *(((PINT2) pnt[*vnum]) + loff);
      break;
   case 5:
      if (! *(((PLOG4) pnt[*vnum]) + loff))
         *varb = 0.;
      else
         *varb = -1.;
      break;
   case 6:
      if (! *(((PLOG2) pnt[*vnum]) + loff))
         *varb = 0.;
      else
         *varb = -1.;
      break;
   }
*err = 0;
return;
}

// *******************************************************************
FORTSUB extput_ (PREAL8 varb, PINT4 vnum, PINT4 vind1, PINT4 vind2,
        PINT4 vind3, PINT4 err)
{
// *******************************************************************

// Set an external variable equal to a REAL*8

// varb  = variable to be copied (input, REAL*8)

// vnum  = external variable number (base 0) (input, INTEGER)

//  vind1 = external vairable indexs (base 1) (input, INTEGER)
//  vind2   0 for scalars or unused indexes
//  vind3

// err  = error flag
//       = 0 ==> no error
//       = 1 ==> vind1 index out of range
//       = 2 ==> vind2 index out of range
//       = 3 ==> vind3 index out of range
//       = 4 ==> wrong number of indexes

// *******************************************************************
//long loff; //sgt
int loff; //sgt

if (dim[*vnum][0] == 0)
   {
   if (*vind1 != 0)
      {
      *err = 4;
      return;
      }
   loff = 0;
   goto doev;
   }
else
   {
   if ((*vind1 < 1) || (*vind1 > dim[*vnum][0]))
      {
      *err = 1;
      return;
      }
   loff = *vind1 - 1;
   }

if (dim[*vnum][1] == 0)
   {
   if (*vind2 != 0)
      {
      *err = 4;
      return;
      }
   goto doev;
   }
else
   {
   if ((*vind2 < 1) || (*vind2 > dim[*vnum][1]))
      {
      *err = 2;
      return;
      }
   loff = loff + dim[*vnum][0] * (*vind2 - 1);
   }

if (dim[*vnum][2] == 0)
   {
   if (*vind3 != 0)
      {
      *err = 4;
      return;
      }
   }
else
   {
   if ((*vind3 < 1) || (*vind3 > dim[*vnum][2]))
      {
      *err = 3;
      return;
      }
   loff = loff + dim[*vnum][0] * dim[*vnum][1] * (*vind3 - 1);
   }

doev:
switch(typ[*vnum])
   {
   case 1:
      *(((PREAL8) pnt[*vnum]) + loff) = *varb;
      break;
   case 2:
      *(((PREAL4) pnt[*vnum]) + loff) = (float) *varb;
      break;
   case 3:
      //*(((PINT4) pnt[*vnum]) + loff) = (long) *varb;
      *(((PINT4) pnt[*vnum]) + loff) = (int) *varb; //sgt
      break;
   case 4:
      //*(((PINT2) pnt[*vnum]) + loff) = (long) (*varb + .5); //sgt
      *(((PINT2) pnt[*vnum]) + loff) = (int) (*varb + .5); //sgt
      break;
   case 5:
      if (*varb == 0.)
         *(((PLOG4) pnt[*vnum]) + loff) = 0;
      else
         *(((PLOG4) pnt[*vnum]) + loff) = -1;
      break;
   case 6:
      if (*varb == 0.)
         *(((PLOG2) pnt[*vnum]) + loff) = 0;
      else
         *(((PLOG2) pnt[*vnum]) + loff) = -1;
      break;
   }
*err = 0;
return;
}

// *******************************************************************
FORTSUB extdel_ (PINT4 vnum1, PINT4 vnum2)
{
// *******************************************************************

// Remove a range of external variable definitions

// vnum1 = first external variable number (base 0) (input, INTEGER)

// vnum2 = last external variable number (base 0) (input, INTEGER)

// *******************************************************************
int i, i1, id, ii;

i1 = (int) *vnum1;
id = (int) (*vnum2 - *vnum1 + 1);
numdef -= id;
for (i = i1; i < numdef; i++)
   {
   pnt[i] = pnt[i + id];
   typ[i] = typ[i + id];
   for (ii = 0; ii < 3; ii++) dim[i][ii] = dim[i + id][ii];
   for (ii = 0; ii < MAXNAM; ii++) nam[i][ii] = nam[i + id][ii];
   }
return;
}

// *******************************************************************
FORTSUB extnam_ (PINT4 vnum, PCHAR vnam)
{
// *******************************************************************

//  Given an external variable number, return the variable name

//  vnum  = variable number (input, INTEGER)
//          no action if variable number is out of range

//  vnam  = variable name (output, CHARACTER*MAXNAM)

// *******************************************************************
int i;

if ((*vnum < 0) || (*vnum >= numdef)) return;
for (i = 0; i < MAXNAM; i++) vnam[i] = nam[*vnum][i];
return;
}

// *******************************************************************
FORTSUB clearev_ ()
{
// *******************************************************************

//  Deletes all external variables

// *******************************************************************
numdef = 0;
return;
}
