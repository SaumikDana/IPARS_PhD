// update.c - Parallel Memory Management Routines in C

// HISTORY:

// JOHN WHEELER        8/1/95   ORIGINAL BETA CODE
// SUNIL G THOMAS    --/--/--   MODS FOR 32-64 BIT CONVERSION

#include "memory.h"

// ROUTINE DECLARATIONS

FORTSUB update_ (PINT4 arynum, PINT4 ktmp);

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB update_ (PINT4 arynum, PINT4 ktmp)
{
// *******************************************************************

// Updates the communication layer for 1 array.  Must be called only
// from executive routines.

// arynum = Array number (input).

// ktmp   = Template number (input).

// *******************************************************************
int na,nb,nb1,nb2;
int nbp;
int one;
extern FORTSUB r4update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB r8update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB i2update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB i4update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB l2update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB l4update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB noupdate_(PINT4 bn,PINT4 iv1,PINT4 iv2);

na = *arynum;
one = 1;

for (nb = 0; nb < numblks; nb++)
   {
   if (((*modact) == 0L) || ((*modact) == modblk[nb]) ||
       ((*modact) == fmodblk[nb]))
      {
      nbp = nb + 1;
      if (myelem[nb] > 0)
         {
         switch(aryfrm[na])
            {
            case 1:
               r4update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),
                  &(dimext[na]),&one,&(dimext[na]),&nbp,ktmp,aryadd[nb][na]);
               break;

            case 2:
               r8update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),
                  &(dimext[na]),&one,&(dimext[na]),&nbp,ktmp,aryadd[nb][na]);
               break;

            case 3:
               i2update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),
                  &(dimext[na]),&one,&(dimext[na]),&nbp,ktmp,aryadd[nb][na]);
               break;

            case 4:
//             Trigger special processing for keyout update
               if (na == 0) one = 0;
               i4update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),
                  &(dimext[na]),&one,&(dimext[na]),&nbp,ktmp,aryadd[nb][na]);
               break;

            case 5:
               l2update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),
                  &(dimext[na]),&one,&(dimext[na]),&nbp,ktmp,aryadd[nb][na]);
               break;

            case 6:
               l4update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),
                  &(dimext[na]),&one,&(dimext[na]),&nbp,ktmp,aryadd[nb][na]);
               break;

            default:
               return;
            }
         }
      else
         noupdate_ (&nbp,&one,&(dimext[na]));
      }
   }
return;
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FORTSUB updater_ (PINT4 arynum, PINT4 iv1, PINT4 iv2, PINT4 ktmp)
{
// *******************************************************************

// Updates the communication layer for 1 array.  Must be called from an
// executive routine.  All fault blocks are updated.

// arynum = Array number (input).

// iv1,iv2 = Range of the fourth index to be updated (input).

// ktmp   = Template number (input).

// *******************************************************************
int na,nb,nb1,nb2;
int nbp;
extern FORTSUB r4update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB r8update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB i2update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB i4update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB l2update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB l4update_(PINT4 id,PINT4 jd,PINT4 kd,PINT4 ld,PINT4 iv1,
                        PINT4 iv2,PINT4 bn,PINT4 kt,PADD an);
extern FORTSUB noupdate_(PINT4 bn,PINT4 iv1,PINT4 iv2);
na = *arynum;
if ((na < 0) || (na >= numarys)) return;

if (nbcw >= 0)
   {
   nb1 = nbcw;
   nb2 = nbcw + 1;
   }
else
   {
   nb1 = 0;
   nb2 = numblks;
   }

for (nb = nb1; nb < nb2; nb++)
   {
   nbp = nb + 1;
   if (myelem[nb] > 0)
      {
      switch(aryfrm[na])
         {
         case 1:
            r4update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),&(dimext[na]),
               iv1,iv2,&nbp,ktmp,aryadd[nb][na]);
            break;

         case 2:
            r8update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),&(dimext[na]),
               iv1,iv2,&nbp,ktmp,aryadd[nb][na]);
            break;

         case 3:
            i2update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),&(dimext[na]),
               iv1,iv2,&nbp,ktmp,aryadd[nb][na]);
            break;

         case 4:
            i4update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),&(dimext[na]),
               iv1,iv2,&nbp,ktmp,aryadd[nb][na]);
            break;

         case 5:
            l2update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),&(dimext[na]),
               iv1,iv2,&nbp,ktmp,aryadd[nb][na]);
            break;

         case 6:
            l4update_ (&(idim[nb]),&(jdim[nb]),&(kdim[nb]),&(dimext[na]),
               iv1,iv2,&nbp,ktmp,aryadd[nb][na]);
            break;

         default:
            return;
         }
      }
  else
      noupdate_ (&nbp,iv1,iv2);
   }
return;
}
