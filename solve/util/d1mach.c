/* source from http://www.netlib.org/blas/d1mach.f  */
/* ANSI C source for D1MACH -- remove the * in column 1 */
/** part of the DCG package for IPARS **/
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>

#include "cfsimple.h"

#define _d1mach _F_NAME_(D1MACH,d1mach)
/*double _d1mach(long *i) */ //sgt
double _d1mach(int *i)
{
       switch(*i){
         case 1: return DBL_MIN;
         case 2: return DBL_MAX;
         case 3: return DBL_EPSILON/FLT_RADIX;
         case 4: return DBL_EPSILON;
         case 5: return log10(FLT_RADIX);
         }
       /*fprintf(stderr, "invalid argument: d1mach(%ld)\n", *i); */ //sgt
       fprintf(stderr, "invalid argument: d1mach(%d)\n", *i);
       exit(1); return 0; 
/* for compilers that complain of missing return values */
}
