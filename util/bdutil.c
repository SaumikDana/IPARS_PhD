// -----------------------------------------------------------------
// file: bdutil.c
//
// utility routines for bdary condition processing and for 
// MBlock geometry
// MPeszynska, 1/01
//-----------------------------------------------------------------

#include <stdio.h>
#include "cfsimple.h"

#define _find_segment     _F_NAME_(FIND_SEGMENT,find_segment)
_F_EXTERN_(void) _find_segment(
			       _F_REAL_4 *aa,_F_REAL_4 *bb, 
			       _F_REAL_4 *cc, _F_REAL_4 *dd,
			       _F_INTEGER *check, 
			       _F_REAL_4 *res1, _F_REAL_4 *res2);

void _find_segment (_F_REAL_4 *aa,_F_REAL_4 *bb, _F_REAL_4 *cc, _F_REAL_4 *dd,
                   _F_INTEGER *check, _F_REAL_4 *res1, _F_REAL_4 *res2)
{
  const _F_REAL_4 a=*aa,b=*bb,c=*cc,d=*dd;

  if ( (a>b) || (c>d) ) {
    //printf("\nSegment %g %g %g %g incorrect \n",a,b,c,d);
    *check = 0; return;
      }

  if ( (c>=b)  ) { // abcd
    *check = 0; return;
  }

  if ( (d<=a)  ) { // cdab
    *check = 0; return;
  }

  if (  (a<=c) && (c<=b) && (a<=d) && (d<=b) ) { // acdb
    *check = 1; *res1=c;*res2=d;return;
  }
  if (  (a<=c) && (c<=b) && (b<=d) ) { // acbd
    *check = 1; *res1=c;*res2=b;return;
  }

  if (  (c<=a) && (b<=d) ) { // cabd
    *check = 1; *res1=a;*res2=b;return;
  }

  if (  (c<=a) && (d<=b) ) { // cadb
    *check = 1; *res1=a;*res2=d;return;
  }
    

}

