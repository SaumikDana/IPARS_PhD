// memman1.c - Memory Management Routines in C, part 1 of 2

// HISTORY:

// JOHN WHEELER       9/10/95   ORIGINAL BETA CODE
// JOHN WHEELER       6/ 1/99   MODIFY FOR MULTIMODEL
// RICK DEAN         11/29/01   ADDED pntvarr4 and pntvarr8
// SUNIL G THOMAS    --/--/--   MODS FOR 32-64 BIT CONVERSION, TRCHEM,
//                              VTK VISUALIZATION.
// GURPREET SINGH    08/12/14   MODIFICATIONS FOR MFMFE

// NOTES:

//   1)  ERROR NUMBERS 461-500 ARE RESERVED FOR MEMORY MANAGEMENT

// Once only declaration of global variables

#define  MEM_GLOBALS
#include "memory.h"

int numblks = 0;
int numprcs = 1;
int numarys = 0;

// ROUTINE DECLARATIONS

FORTSUB  defblk_  (PINT4 vnxg, PINT4 vnyg, PINT4 vnzg, PCHAR vblknam,
             PINT4 blknum, PINT4 numprc, PINT4 err);
FORTSUB  defgrd_  (PINT4 myprc, PINT4 nxc, PINT4 nyc, PINT4 nzc,
             PINT4 vdimr, PINT4 vn0map, PINT4 vnymap, PINT2 vprcmap, PINT4 err);
FORTSUB  blkdim_  (PINT4 blknum, PINT4 vnxg, PINT4 vnyg, PINT4 vnzg,
             PINT4 err);
FORTSUB  blkoff_  (PINT4 blknum, PINT4 ioff, PINT4 joff, PINT4 koff,
             PINT4 err);
FORTSUB  blkname_ (PINT4 blknum, PCHAR vblknam, PINT4 err);
FORTSUB  alcgea_  (PCHAR varynam, PINT4 kind, PINT4 ndim4,
             PINT4 arynum, PINT4 err);
FORTSUB  alcfea_  (PCHAR varynam, PINT4 kind, PINT4 ndim4,
             PINT4 arynum, PINT4 err);
FORTSUB alcmpfagea_ (PCHAR varynam, PINT4 kind, PINT4 ndim4,
             PINT4 arynum, PINT4 err);
FORTSUB alcmpfagea1_ (PCHAR varynam, PINT4 kind, PINT4 ndim4,
             PINT4 arynum, PINT4 ev_prcblk, PINT4 err);
FORTSUB alcmpfagea2_ (PCHAR varynam, PINT4 kind, PINT4 ndim4,
             PINT4 arynum, PINT4 ev_prcblk, PINT4 err);
FORTSUB  pntvar_  (PADD varb, PINT4 arynum, PINT4 err);
FORTSUB  pntvarr8_  (PADD varb, PINT4 arynum, PINT4 err);
FORTSUB  pntvarr4_  (PADD varb, PINT4 arynum, PINT4 err);
FORTSUB  arydat_  (PCHAR varynam, PINT4 kind, PINT4 ndim4,
             PINT4 arynum, PINT4 err);
FORTSUB  arytype_ (PINT4 arynum, PINT4 kind, PINT4 ndim4,
             PINT4 arypmoda, PINT4 err);
FORTSUB  geaout_  (PINT4 arynum, PINT4 iex, PINT4 kcen);
FORTSUB  allblocks_ ();
FORTSUB  getanam_ (PINT4 arynum, PCHAR varynam, PINT4 err);
FORTSUB  pntmmodmb_ (PCHAR mb, PINT4 porohexvar,
         PINT4 flowvisvar1, PINT4 flowvisvar2);
         /* Added by Saumik and Ben Ganis */
FORTSUB  pntmmod_ (PINT4 modacta, PINT4 flomoda, PINT4 modblka);
FORTSUB  freary_  (PINT4 arynum);
FORTSUB  freeall_ ();

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB defblk_ (PINT4 vnxg, PINT4 vnyg, PINT4 vnzg, PCHAR vblknam,
          PINT4 blknum, PINT4 numprc, PINT4 err)
{
// *******************************************************************

// Registers a block of grid elements with the data management system.
// This routine must be called for all blocks before any other
// data-management routine is called.

// vnxg,vnyg,vnzg = Number of grid elements in the x, y, and z
// directions respectively for the block (input).

// vblknam = Name of the block for output (input, CHARACTER*60)

// blknum = Block number (modulo 1) (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 461 ==> max number of blocks exceeded

// *******************************************************************
int i, nb, na;
char *cstring;

numprcs = *numprc;
if (numblks == MAXBLK)
   {
   *err = 461;
   return;
   }
cstring = vblknam;
nxg[numblks] = max (1, *vnxg);
nyg[numblks] = max (1, *vnyg);
nzg[numblks] = max (1, *vnzg);
for (i = 0; i < MAXBNAM; i++) blknams[numblks][i] = cstring[i];

//  Initialize some memory management data not associated with a block

if (numblks == 0)
   {
   for (na = 0; na < MAXARY; na++) arytyp[na] = 0;
   for (na = 0; na < MAXARY; na++) arypmod[na] = 0;
   for (nb = 0; nb < MAXBLK; nb++)
      {
      jloc1[nb] = NULL;
      jloc2[nb] = NULL;
      keyout[nb] = NULL;
      nbcw = -1;
      allblk = 1;
      for (na = 0; na < MAXARY; na++) aryadd[nb][na] = NULL;
      }
   }

// arymodel is initialized to ALL =0
 for (na = 0; na < MAXARY; na++) arymodel[na] = 0;

*blknum = ++numblks;
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB defgrd_ (PINT4 myprc, PINT4 nxc, PINT4 nyc, PINT4 nzc,
       PINT4 vdimr, PINT4 vn0map, PINT4 vnymap, PINT2 vprcmap, PINT4 err)
{
// *******************************************************************

//  Defines the grid scheme to be used.  This routine must be called
//  after DIVIDE and before any call to ALCGEA.  The routine
//  allocates the KEYOUT array and initializes it with no grid
//  refinement, no keyed-out elements, and layer columns outside the
//  block marked as nonexistant.

//  myprc = Active processor number (mod 0) (input)

//  nxc,nyc,nzc = Communications layers provided in the three grid directions
//  for multiprocessor simulations.  Set these values to 0 for single
//  processor machines.  Since DIVIDE currently keeps columns of elements
//  together on the same processor, set nxc = 0. (input)

//  vdimr = Initial 1st dimension of grid-refinement arrays

//  vn0map, vnymap, vprcmap =  Pointers to N0MAP(), NYMAP(), and PRCMAP()

// err = Error number (output)
//     = 0 ==> No error
//     = 462 ==> Insufficient memory available

// *******************************************************************

int nb, n, ne, nk, i, j, k, j1, j2, k1, k2, j2g, k2g, nm, jg, kg, pid;
int jlg, klg, m, nint, i1, i2;

layer[0] = *nxc;
layer[1] = *nyc;
layer[2] = *nzc;
dimr = *vdimr;
mynod = *myprc;
n0map = vn0map;
nymap = vnymap;
prcmap = vprcmap;

//  Set flag for all processors to check if proc has elements on a block.
//  Currently done separately to improve readability and reduce interference
//  with pre-existing code. This is a one-time task and not very expensive.
for (pid = 0; pid < numprcs; pid++)
{
   for (nb = 0; nb < numblks; nb++)
   {
      ne = 0;
      prcblk[nb][pid] = -1;
      j2g = nyg[nb];
      k2g= nzg[nb];
      for (k = 0; k < k2g; k++)
      {
         n = n0map[nb] + nymap[nb] * (k + 1);
         for (j = 0; j < j2g; j++) if (prcmap[n++] == pid) ne++;
      }
      if (ne > 0) prcblk[nb][pid] = ne;
   }
}

//  Identify grid elements for the current processor in each grid block.

for (nb = 0; nb < numblks; nb++)
   {
   ne = 0;
   j1 = j2g = nyg[nb];
   k1 = k2g= nzg[nb];
   j2 = k2 = 0;
   for (k = 0; k < k2g; k++)
      {
      n = n0map[nb] + nymap[nb] * (k + 1);
      for (j = 0; j < j2g; j++)
         {
         if (prcmap[n++] == mynod)
            {
            ne++;
            if (j < j1) j1 = j;
            if (j > j2) j2 = j;
            if (k < k1) k1 = k;
            if (k > k2) k2 = k;
            }
         }
      }
   myelem[nb] = ne;

   if (ne > 0)
      {
      idim[nb] = nxg[nb] + max(1, 2 * layer[0]);
      jdim[nb] = j2 - j1 + 1 + max(1, 2 * layer[1]);
      kdim[nb] = k2 - k1 + 1 + max(1, 2 * layer[2]);
      iloc1[nb] = layer[0] + 1;
      iloc2[nb] = idim[nb] - max(1, layer[0]);
      kloc1[nb] = layer[2] + 1;
      kloc2[nb] = kdim[nb] - max(1,layer[2]);
      iofflg[nb] = -layer[0];
      jofflg[nb] = j1 - layer[1];
      kofflg[nb] = k1 - layer[2];

      nm = kdim[nb] * sizeof(int);
      if ((jloc1[nb] = (PINT4) malloc(nm)) == NULL) goto errex;
      if ((jloc2[nb] = (PINT4) malloc(nm)) == NULL) goto errex;
      nint = idim[nb] * jdim[nb] * kdim[nb];
      if ((aryadd[nb][0] = keyout[nb] = (PINT4) malloc(nint * sizeof(int)))
          == NULL) goto errex;

      for (k = 0; k < nint; k++) *(keyout[nb] + k) = 0;

      jlg = jofflg[nb] + 1;
      klg = kofflg[nb] + 1;
      i1 = iloc1[nb] - 1;
      i2 = iloc2[nb] - 1;
      for (k = 0, kg = klg; k < kdim[nb]; k++, kg++)
         {
         if ((kg > 0) && (kg <= k2g))
            {
            nk = n0map[nb] + nymap[nb] * kg - 1;
            j1 = jdim[nb];
            j2 = 0;
            for (j = 0, jg = jlg; j < jdim[nb]; j++, jg++)
               {
               if ((jg > 0) && (jg <= j2g))
                  {
                  if (prcmap[nk + jg] == mynod)
                     {
                     m = 1;
                     if (j < j1) j1 = j;
                     if (j > j2) j2 = j;
                     }
                  else
                     {
                     if (prcmap[nk + jg] < 0)
                        m = 0;
                     else
//bw                        m = -1L;
                        m = -2L;
                     }
                  nm = idim[nb] * (j + jdim[nb] * k);
                  for (i = i1; i <= i2; i++)
                     *(keyout[nb] + nm + i) = m;
                  }
               }
            *(jloc1[nb] + k) = j1 + 1;
            *(jloc2[nb] + k) = j2 + 1;
            }
         }
         //bw setup jloc1[], jloc2[] in k-direction ghost layers
      for (k=0L; k<(kloc1[nb]-1);k++)
          {
          *(jloc1[nb]+k)=*(jloc1[nb]+kloc1[nb]-1);
          *(jloc2[nb]+k)=*(jloc2[nb]+kloc1[nb]-1);
          }
      for (k=kloc2[nb]; k<(kdim[nb]);k++)
          {
          *(jloc1[nb]+k)=*(jloc1[nb]+kloc2[nb]-1);
          *(jloc2[nb]+k)=*(jloc2[nb]+kloc2[nb]-1);
          }
      }
   }

// Complete duplicate data for keyout array to satisfy i/o needs

aryfrm[0] = 4;
varlen[0] = sizeof(int);
dimext[0] = 1;
arytyp[0] = 2;
arypmod[0] = 0;
arynam[0][0] = 'K';
arynam[0][1] = 'E';
arynam[0][2] = 'Y';
arynam[0][3] = 'O';
arynam[0][4] = 'U';
arynam[0][5] = 'T';
arynam[0][6] = ' ';
numarys = 1;

// Exits

*err = 0;
return;
errex: *err = 462;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB blkdim_ (PINT4 blknum, PINT4 vnxg, PINT4 vnyg, PINT4 vnzg, PINT4 err)
{
// *******************************************************************

// Returns grid-block global dimensions
// DEFGRD must be called before this routine.

// blknum = block number (modulo 1) (input)

// vnxg,vnyg,vnzg = Number of grid elements in the x, y, and z
// directions respectively for the block (global) (output).

// err = Error number (output)
//     = 0 ==> no error
//     = 463 ==> invalid block number

// *******************************************************************
int i;

i = (*blknum) - 1;
if ((i < 0) || (i >= numblks))
   {
   *err = 463;
   return;
   }
*vnxg = nxg[i];
*vnyg = nyg[i];
*vnzg = nzg[i];
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB blkoff_ (PINT4 blknum, PINT4 ioff, PINT4 joff, PINT4 koff, PINT4 err)
{
// *******************************************************************

// Returns grid-block local to global offsets.
// DIVIDE and DEFGRD must be called before this routine.

// blknum = block number (modulo 1) (input)

// ioff,joff,koff = Offsets between global and local indexes for the
// grid block. (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 1 ==> invalid block number

// *******************************************************************
int i;

i = (*blknum) - 1;
if ((i < 0) || (i >= numblks))
   {
   *err = 463;
   return;
   }
*ioff = iofflg[i];
*joff = jofflg[i];
*koff = kofflg[i];
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB blkname_ (PINT4 blknum, PCHAR vblknam, PINT4 err)
{
// *******************************************************************

// Returns grid-block name

// blknum = block number (modulo 1) (input)

// vblknam = Name of the block (output, CHARACTER*60)

// err = Error number (output)
//     = 0 ==> no error
//     = 1 ==> invalid block number

// *******************************************************************
int i, j;
char *cstring;

i = (*blknum) - 1;
if ((i < 0) || (i >= numblks))
   {
   *err = 463;
   return;
   }
cstring = vblknam;
for (j = 0; j < MAXBNAM; j++) cstring[j] = blknams[i][j];
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB alcgea_ (PCHAR varynam, PINT4 kind, PINT4 ndim4, PINT4 arynum,
        PINT4 err)
{
// *******************************************************************

// Grid-element array creation.  Call this routine after DIVIDE but
// before any other reference to the array.

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters (input)

// kind = Data type of the array (input)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim4 = Product of the 4th and higher dimensions (input)
//       = 0 ==> no higher dimensions

// arynum = Array number (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays
//     = 462 ==> insufficient memory
//     = 465 ==> invalid data type

// *******************************************************************
int i, k, na, nb;
long long int nm;    // bag8 - 64-bit integer
char *cstring;

cstring = varynam;
for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
if (na == MAXARY)
   {
   *err = 464;
   return;
   }

aryfrm[na] = *kind;
switch(*kind)
   {
   case 1:
      varlen[na] = sizeof(float);
      break;
   case 2:
      varlen[na] = sizeof(double);
      break;
   case 3:
      varlen[na] = sizeof(int);
      break;
   case 4:
      varlen[na] = sizeof(int);
      break;
   case 5:
      varlen[na] = sizeof(int);
      break;
   case 6:
      varlen[na] = sizeof(int);
      break;
   default:
      *err = 465;
      return;
   }

dimext[na] = max (1, *ndim4);
arytyp[na] = 2;
arypmod[na] = *modact;
for (i = 0; i < MAXANAM; i++)
   {
   arynam[na][i] = ' ';
   if ((cstring[i] == ' ') || (cstring[i] == '[')) break;
   arynam[na][i] = cstring[i];
   }

// mpesz: terminate the string with "\0" so that it can be handled
// from inside of the C code: strings allocated with MAXANAM+1 length

 if (MAXANAM > 0) arynam[na][MAXANAM]='\0';
 else { *err = 465;return;     }

 // set the aux. model informnation
 arymodel[na]=CurrentModel;

for (nb = 0; nb < numblks; nb++)
   {
   if ((myelem[nb] > 0) &&
      (((*modact) == 0) || ((*modact) == modblk[nb])
    || ((*modact) == (fmodblk[nb])))
      )
      {
      // bag8 - 64-bit integer
      nm = (long long int)idim[nb] * (long long int)jdim[nb] *
           (long long int)kdim[nb] * (long long int)dimext[na] *
           (long long int)varlen[na];

      //printf("In alcgea, arynam=%s, n=%lld\n",cstring,nm);

      if (nm < 0) { *err = 462; return; }

      if ((aryadd[nb][na] = (PADD) malloc(nm)) == NULL)
         { *err = 462; return; }

      memset((void*)(aryadd[nb][na]), (int) 0, (size_t) nm );
      }
   }

if (numarys <= na) numarys++;
*arynum = na;
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB alcfea_ (PCHAR varynam, PINT4 kind, PINT4 ndim,
                           PINT4 arynum, PINT4 err)
{
// *******************************************************************

// Fracture grid-element array creation.  Call this routine after DIVIDE but
// before any other reference to the array.

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters (input)

// kind = Data type of the array (input)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim = size of the array (input)
//       = 0 ==> no higher dimensions

// arynum = Array number (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays
//     = 462 ==> insufficient memory
//     = 465 ==> invalid data type

// *******************************************************************
int i, k, na, nb;
//long nm; //sgt
int nm; //sgt
char *cstring;

cstring = varynam;
for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
if (na == MAXARY)
   {
   *err = 464L;
   return;
   }

aryfrm[na] = *kind;
switch(*kind)
   {
   case 1:
      varlen[na] = sizeof(float);
      break;
   case 2:
      varlen[na] = sizeof(double);
      break;
   case 3:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 4:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 5:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 6:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   default:
      *err = 465L;
      return;
   }

dimext[na] = max (1L, *ndim);
arytyp[na] = 2;
arypmod[na] = *modact;
for (i = 0; i < MAXANAM; i++)
   {
   arynam[na][i] = ' ';
   if ((cstring[i] == ' ') || (cstring[i] == '[')) break;
   arynam[na][i] = cstring[i];
   }

// mpesz: terminate the string with "\0" so that it can be handled
// from inside of the C code: strings allocated with MAXANAM+1 length

 if (MAXANAM > 0) arynam[na][MAXANAM]='\0';
 else { *err = 465L;return;     }

 // set the aux. model informnation
 arymodel[na]=CurrentModel;

for (nb = 0; nb < numblks; nb++)
   {
   if ((myelem[nb] > 0) &&
      (((*modact) == 0L) || ((*modact) == modblk[nb]))
      || ((*modact) == (fmodblk[nb])))
      {
      nm = dimext[na] * varlen[na];
      if ((aryadd[nb][na] = (PADD) malloc(nm)) == NULL)
         {
         *err = 462;
         return;
         }
      memset((void*)(aryadd[nb][na]), (int) 0, (size_t) nm );
      }
   }

if (numarys <= na) numarys++;
*arynum = na;
*err = 0L;

return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB alcmpfagea_ (PCHAR varynam, PINT4 kind, PINT4 ndim4,
        PINT4 arynum, PINT4 err)
{
// *******************************************************************

// Grid-element array creation.  Call this routine after DIVIDE but
// before any other reference to the array.

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters (input)

// kind = Data type of the array (input)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim4 = Product of the 4th and higher dimensions (input)
//       = 0 ==> no higher dimensions
// arynum = Array number (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays
//     = 462 ==> insufficient memory
//     = 465 ==> invalid data type

// *******************************************************************
int i, k, na, nb;
//long nm; //sgt
int nm; //sgt
char *cstring;

cstring = varynam;
for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
if (na == MAXARY)
   {
   *err = 464L;
   return;
   }

aryfrm[na] = *kind;
switch(*kind)
   {
   case 1:
      varlen[na] = sizeof(float);
      break;
   case 2:
      varlen[na] = sizeof(double);
      break;
   case 3:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 4:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 5:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 6:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   default:
      *err = 465L;
      return;
   }

dimext[na] = max (1L, *ndim4);
arytyp[na] = 2;
arypmod[na] = *modact;
for (i = 0; i < MAXANAM; i++)
   {
   arynam[na][i] = ' ';
   if ((cstring[i] == ' ') || (cstring[i] == '[')) break;
   arynam[na][i] = cstring[i];
   }

// mpesz: terminate the string with "\0" so that it can be handled
// from inside of the C code: strings allocated with MAXANAM+1 length

 if (MAXANAM > 0) arynam[na][MAXANAM]='\0';
 else { *err = 465L;return;     }

 // set the aux. model informnation
 arymodel[na]=CurrentModel;

for (nb = 0; nb < numblks; nb++)
   {
   if ((myelem[nb] > 0) &&
      (((*modact) == 0L) || ((*modact) == modblk[nb]))
      || ((*modact) == (fmodblk[nb])))
      {
      nm = (idim[nb]+1)*(jdim[nb]+1)*(kdim[nb]+1)*dimext[na]*varlen[na];
      if ((aryadd[nb][na] = (PADD) malloc(nm)) == NULL)
         {
         *err = 462;
         return;
         }
      memset((void*)(aryadd[nb][na]), (int) 0, (size_t) nm );
      }
   }

if (numarys <= na) numarys++;
*arynum = na;
*err = 0L;
return;
}

// bag8 - subroutine to allocate primary fault block when using
//        EV_PRCBLK input option
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB alcmpfagea1_ (PCHAR varynam, PINT4 kind, PINT4 ndim4,
        PINT4 arynum, PINT4 ev_prcblk, PINT4 err)
{
// *******************************************************************

// Grid-element array creation.  Call this routine after DIVIDE but
// before any other reference to the array.

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters (input)

// kind = Data type of the array (input)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim4 = Product of the 4th and higher dimensions (input)
//       = 0 ==> no higher dimensions
// arynum = Array number (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays
//     = 462 ==> insufficient memory
//     = 465 ==> invalid data type

// *******************************************************************
int i, k, na, nb;
//long nm; //sgt
int nm; //sgt
char *cstring;

cstring = varynam;
for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
if (na == MAXARY)
   {
   *err = 464L;
   return;
   }

aryfrm[na] = *kind;
switch(*kind)
   {
   case 1:
      varlen[na] = sizeof(float);
      break;
   case 2:
      varlen[na] = sizeof(double);
      break;
   case 3:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 4:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 5:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   case 6:
      //varlen[na] = sizeof(long); //sgt
      varlen[na] = sizeof(int); //sgt
      break;
   default:
      *err = 465L;
      return;
   }

dimext[na] = max (1L, *ndim4);
arytyp[na] = 2;
arypmod[na] = *modact;
for (i = 0; i < MAXANAM; i++)
   {
   arynam[na][i] = ' ';
   if ((cstring[i] == ' ') || (cstring[i] == '[')) break;
   arynam[na][i] = cstring[i];
   }

// mpesz: terminate the string with "\0" so that it can be handled
// from inside of the C code: strings allocated with MAXANAM+1 length

 if (MAXANAM > 0) arynam[na][MAXANAM]='\0';
 else { *err = 465L;return;     }

 // set the aux. model informnation
 arymodel[na]=CurrentModel;

for (nb = 0; nb < numblks; nb++)
{
   if (nb <= (*ev_prcblk-1))
   {
   if ((myelem[nb] > 0) &&
      (((*modact) == 0L) || ((*modact) == modblk[nb]))
      || ((*modact) == (fmodblk[nb])))
      {
      nm = (idim[nb]+1)*(jdim[nb]+1)*(kdim[nb]+1)*dimext[na]*varlen[na];
      if ((aryadd[nb][na] = (PADD) malloc(nm)) == NULL)
         {
         *err = 462;
         return;
         }
      memset((void*)(aryadd[nb][na]), (int) 0, (size_t) nm );
      }
   }
}

if (numarys <= na) numarys++;
*arynum = na;
*err = 0L;
return;
}

// bag8 - subroutine to allocate remaining fault blocks when using
//        EV_PRCBLK input option
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB alcmpfagea2_ (PCHAR varynam, PINT4 kind, PINT4 ndim4,
        PINT4 arynum, PINT4 ev_prcblk, PINT4 err)
{
// *******************************************************************

// Grid-element array creation.  Call this routine after DIVIDE but
// before any other reference to the array.

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters (input)

// kind = Data type of the array (input)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim4 = Product of the 4th and higher dimensions (input)
//       = 0 ==> no higher dimensions
// arynum = Array number (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays
//     = 462 ==> insufficient memory
//     = 465 ==> invalid data type

// *******************************************************************
int i, k, na, nb;
//long nm; //sgt
int nm; //sgt

na = *arynum;

for (nb = 0; nb < numblks; nb++)
{
   if (nb > (*ev_prcblk-1))
   {
   if ((myelem[nb] > 0) &&
      (((*modact) == 0L) || ((*modact) == modblk[nb]))
      || ((*modact) == (fmodblk[nb])))
      {
      nm = (idim[nb]+1)*(jdim[nb]+1)*(kdim[nb]+1)*dimext[na]*varlen[na];
      if ((aryadd[nb][na] = (PADD) malloc(nm)) == NULL)
         {
         *err = 462;
         return;
         }
      memset((void*)(aryadd[nb][na]), (int) 0, (size_t) nm );
      }
   }
}

if (numarys <= na) numarys++;
*err = 0L;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB alcbea_ (PCHAR varynam, PINT4 kind, PINT4 ndim4,
        PINT4 arynum, PINT4 markface, PINT4 err)
{
// *******************************************************************

// Boundary-element array creation and initialization to zero.
// Call this routine after DIVIDE but before any other reference to the array.

// varynam = Array name (20 charcters max) (must be terminated
// with a blank or [ if longer than 20 charcters (input)

// kind = Data type of the array (input)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim4 = Product of the 4th and higher dimensions (input)
//       = 0 ==> no higher dimensions

// arynum = Array numbers (for x, y ,z directions respectively) (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays
//     = 462 ==> insufficient memory
//     = 465 ==> invalid data type

// *******************************************************************
int i, j, na, nb, size1, size2;
//long nm; //sgt
int nm;
char *cstring;

cstring = varynam;

for (j = 0; j < 3; j++){


   for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
   if (na == MAXARY)
      {
      *err = 464L;
      return;
      }
   aryfrm[na] = *kind;
   switch(*kind)
      {
      case 1:
         varlen[na] = sizeof(float);
         break;
      case 2:
         varlen[na] = sizeof(double);
         break;
      case 3:
         //varlen[na] = sizeof(long);  //sgt
         varlen[na] = sizeof(int);
         break;
      case 4:
         //varlen[na] = sizeof(long);   //sgt
         varlen[na] = sizeof(int);
         break;
      case 5:
         //varlen[na] = sizeof(long);   //sgt
         varlen[na] = sizeof(int);
         break;
      case 6:
         //varlen[na] = sizeof(long);    //sgt
         varlen[na] = sizeof(int);
         break;
      default:
         *err = 465L;
         return;
      }

   dimext[na] = max (1L, *ndim4);
   arytyp[na] = 4;
   for (i = 0; i < MAXANAM; i++)
      {
      arynam[na][i] = ' ';
      if ((cstring[i] == ' ') || (cstring[i] == '[')) break;
      arynam[na][i] = cstring[i];
      }
   if (j == 0) arynam[na][i] = 'X';
   if (j == 1) arynam[na][i] = 'Y';
   if (j == 2) arynam[na][i] = 'Z';

   // mpesz: terminate the string with "\0" so that it can be handled
   // from inside of the C code: strings allocated with MAXANAM+1 length

   if (MAXANAM > 0) arynam[na][MAXANAM]='\0';
   else { *err = 465L;return;   }
   // set the aux. model informnation
   arymodel[na]=CurrentModel;

   for (nb = 0; nb < numblks; nb++) {
     int if_allocate = markface[nb*6+j*2] + markface[nb*6+j*2+1];

       // if this array is model independent or, it needs to be allocated
       // for the given physical model which is active on this block
       if ((((*modact) == 0L) || ((*modact) == modblk[nb])))
       {

       // then allocate it for this block

       if (myelem[nb] > 0) {
         if (if_allocate == 0) {nm=1;}
         else {
           switch(j){
           case 0:
             size1 = jdim[nb] - 2 * layer[1];
             size2 = kdim[nb] - 2 * layer[2];
             break;
           case 1:
             size1 = idim[nb] - 2 * layer[0];
             size2 = kdim[nb] - 2 * layer[2];
             break;
           case 2:
             size1 = idim[nb] - 2 * layer[0];
             size2 = jdim[nb] - 2 * layer[1];
             break;
           default:
             break;
           }
           nm = size1 * size2 * 2 * dimext[na] * varlen[na];
         }
         if ((aryadd[nb][na] = (PADD) malloc(nm)) == NULL){
           *err = 462;
           return;
         }
         /*
           HCE 1/29/98
           Experienced some floating point exceptions due to 'NAN' in an
           allocated grid element array.  The most durable solution is
           to insure that the allocated memory is initialized to zero.
         */

         memset((void*)(aryadd[nb][na]), (int) 0, (size_t) nm );
       }
     } else  {                                   // set the pointer to zero
       aryadd[nb][na] = NULL;


     }
   }
   if (numarys <= na) numarys++;
   arynum[j] = na;
   }

*err = 0L;
return;
}




//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB pntvar_ (PADD varb, PINT4 arynum, PINT4 err)
{
// *******************************************************************

// Variable registration.  Call this routine to identify a normal
// variable (not a grid-element or grid-refinement array) for use in
// the argument sequence of a work routine.  If the call to the work
// routine does not occur in the same or a subordinate routine, put
// VARB in a COMMON area so that the compiler will not assign it a
// temporary memory location.

// varb = Variable (input, any data type)

// arynum = Variable number (output).

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays and variables

// *******************************************************************
int na, nb;

for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
if (na >= MAXARY)
   {
   *err = 464;
   return;
   }

arytyp[na] = 1;
arypmod[na] = *modact;

for (nb = 0; nb < numblks; nb++) aryadd[nb][na] = varb;

if (numarys == na) numarys++;
*arynum = na;
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB pntvarr8_ (PADD varb, PINT4 arynum, PINT4 err)
{
// *******************************************************************

// Variable registration.  Call this routine to identify a normal
// variable (not a grid-element or grid-refinement array) for use in
// the argument sequence of a work routine.  If the call to the work
// routine does not occur in the same or a subordinate routine, put
// VARB in a COMMON area so that the compiler will not assign it a
// temporary memory location.

// varb = Variable (input, any data type)

// arynum = Variable number (output).

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays and variables

// *******************************************************************
int na, nb;

for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
if (na >= MAXARY)
   {
   *err = 464;
   return;
   }

arytyp[na] = 1;
arypmod[na] = *modact;

for (nb = 0; nb < numblks; nb++) aryadd[nb][na] = varb;

if (numarys == na) numarys++;
*arynum = na;
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB pntvarr4_ (PADD varb, PINT4 arynum, PINT4 err)
{
// *******************************************************************

// Variable registration.  Call this routine to identify a normal
// variable (not a grid-element or grid-refinement array) for use in
// the argument sequence of a work routine.  If the call to the work
// routine does not occur in the same or a subordinate routine, put
// VARB in a COMMON area so that the compiler will not assign it a
// temporary memory location.

// varb = Variable (input, any data type)

// arynum = Variable number (output).

// err = Error number (output)
//     = 0 ==> no error
//     = 464 ==> too many arrays and variables

// *******************************************************************
int na, nb;

for (na = 0; (na < MAXARY) && (arytyp[na] != 0); na++);
if (na >= MAXARY)
   {
   *err = 464;
   return;
   }

arytyp[na] = 1;
arypmod[na] = *modact;

for (nb = 0; nb < numblks; nb++) aryadd[nb][na] = varb;

if (numarys == na) numarys++;
*arynum = na;
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB arydat_ (PCHAR varynam, PINT4 kind, PINT4 ndim4, PINT4 arynum,
        PINT4 err)
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
int i, na;
char *cstring;

cstring = varynam;
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
*err = 466;
return;

aok:
*kind = aryfrm[na];
*ndim4 = dimext[na];
*arynum = na;
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB arytype_ (PINT4 arynum, PINT4 kind, PINT4 ndim4,
   PINT4 arypmoda, PINT4 err)
{
// *******************************************************************

// Returns info for an existing array in the memory management system

// arynum = Array number (input)

// kind = Data type of the array (output)
//      = 1 ==> REAL*4
//      = 2 ==> REAL*8
//      = 3 ==> INTEGER
//      = 4 ==> INTEGER
//      = 5 ==> LOGICAL
//      = 6 ==> LOGICAL

// ndim4 = Product of the 4th and higher dimensions (output)
//       = 1 ==> no higher dimensions

// arypmoda = Physical model that created the array (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 468 ==> invalid array number

// *******************************************************************
int na;

na = *arynum;
if ((na < 0) || (na > numarys))
   {
   *err = 468;
   return;
   }

*kind = aryfrm[na];
*arypmoda = arypmod[na];
*ndim4 = dimext[na];
*err = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB geaout_ (PINT4 arynum, PINT4 iex, PINT4 kcen)
{
// *******************************************************************

// Directs printing of a grid-element array

// arynum = Array number (input)

// iex  = Value of the fourth index (input)
//        Use 1 if there is no forth index

// kcen = Array key (input)
//      = 1 ==> Block center array
//      = 2 ==> Block corner array

// Note: the table title is transfered thru TITU in COMMON /BLKARY/

// *******************************************************************
int nbp,fm;
extern FORTSUB geaprt_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 il1,
       PINT4 il2,PINT4 jv1,PINT4 jv2,PINT4 kl1,PINT4 kl2,PINT4 ko,PINT4 nb,
       PADD an,PINT4 mel,PINT4 afrm,PINT4 dex,PINT4 kc);

fm = aryfrm[*arynum];
for (nbcw = 0; nbcw < numblks; nbcw++)
   {
   if (((*modact) == 0) || ((*modact) == modblk[nbcw])
    || ((*modact) == (fmodblk[nbcw]))
      )
      {
      nbp=nbcw + 1;
      geaprt_ (&(idim[nbcw]),&(jdim[nbcw]),&(kdim[nbcw]),&dimr,
         &(iloc1[nbcw]),&(iloc2[nbcw]),jloc1[nbcw],jloc2[nbcw],
         &(kloc1[nbcw]),&(kloc2[nbcw]),keyout[nbcw],&nbp,
         aryadd[nbcw][*arynum],&(myelem[nbcw]),&fm,iex,kcen);
      }
   }
nbcw = -1;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB allblocks_ ()
{
// *******************************************************************

// Causes the next only call to a work routine to be executed by each
// processor for all fault blocks, not just those for which the processor
// has assigned elements.  Great care should be used in invoking this option
// since null addresses may be passed to a work routine.

allblk = 0;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB getanam_ (PINT4 arynum, PCHAR varynam, PINT4 err)
{
// *******************************************************************

// Returns the name of an existing grid-element array

// arynum = Array number (input)

// varynam = Array name (20 charcters) (output)

// err = Error number (output)
//     = 0 ==> no error
//     = 466 ==> invalid array number

// *******************************************************************
int i,na;
char *cstring;

na = *arynum;
if ((na < 0) || (na > numarys))
   {
   *err = 466;
   return;
   }

cstring = varynam;

for (i = 0; i < MAXANAM; i++) cstring[i] = arynam[na][i];

*err = 0;
return;
}
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB  pntmmodmb_ (PCHAR mb, PINT4 porohexvar,
         PINT4 flowvisvar1, PINT4 flowvisvar2)
{
// *******************************************************************

// *******************************************************************

mblock=mb;
porohexvisvar=porohexvar;
simfmfevisvar=flowvisvar1;
cmfmfevisvar=flowvisvar2;
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB  pntmmod_ (PINT4 modacta, PINT4 flomoda, PINT4 modblka)
{
// *******************************************************************

// Sets c pointers (memory.h) to data in control.h

// modacta = Number of physical model currently active (input)

// flowmoda = Number of flowing model currently active (input)
//            assumed just one fixed number for every block to be
//            later changed when multimodel is implemented.

// modblka[nb] = Number of physical model in fault block nb (input)

// *******************************************************************

modact=modacta;
fmodblk=flomoda;
modblk=modblka;

}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB freeall_ ()
{
// *******************************************************************

// Free all grid-element arrays

// *******************************************************************
int nb, na;

for (na = 0; na < numarys; na++)
   {
   if (arytyp[na] > 1)
      {
      for (nb = 0; nb < numblks; nb++)
         {
         if ((nb==0) && (aryadd[nb][na] != NULL)) free(aryadd[nb][na]);
         aryadd[nb][na] = NULL;
         }
      }
   arytyp[na] = 0;
   arypmod[na] = 0;
   numblks = 0;
   }
return;
}

