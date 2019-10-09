
#ifndef _R8_LAPACK_INTERFACE_
#define _R8_LAPACK_INTERFACE_

#include "r8blas.h"

/*--------------------------------------------------------------------*/

#define _r8getrf  _R8_NAME_(GETRF,getrf)
#define _r8getrs  _R8_NAME_(GETRS,getrs)
#define _r8gbtrf  _R8_NAME_(GBTRF,gbtrf)
#define _r8gbtrs  _R8_NAME_(GBTRS,gbtrs)
#define _r8laswp  _R8_NAME_(LASWP,laswp)
#define _r8getri  _R8_NAME_(GETRI,getri)

/*--------------------------------------------------------------------*/

_F_EXTERN_(void) _r8laswp(
  const _F_INTEGER * N ,
        _F_REAL_8  * A ,    const _F_INTEGER * LDA ,
  const _F_INTEGER * K1 ,   const _F_INTEGER * K2 ,
  const _F_INTEGER * IPIV , const _F_INTEGER * INC );

#define  r8laswp(N,A,LDA,K1,K2,IPIV,INCX) \
  _r8laswp(N,A,LDA,K1,K2,IPIV,INCX)

/*--------------------------------------------------------------------*/

_F_EXTERN_(void) _r8getrf(
  const _F_INTEGER * M ,   const _F_INTEGER * N ,
        _F_REAL_8  * A ,   const _F_INTEGER * LDA ,
        _F_INTEGER * IPIV , 
        _F_INTEGER * INFO );

/*--------------------------------------------------------------------*/

#define  r8getrf( M, N, A, LDA, IPIV, INFO ) \
  _r8getrf( M, N, A, LDA, IPIV, INFO )

_F_EXTERN_(void) _r8gbtrf(
  const _F_INTEGER * M ,   const _F_INTEGER * N ,
  const _F_INTEGER * KL ,  const _F_INTEGER * KU ,
        _F_REAL_8  * AB ,  const _F_INTEGER * LDAB ,
        _F_INTEGER * IPIV , 
        _F_INTEGER * INFO );

#define  r8gbtrf( M, N, KL, KU, AB, LDAB, IPIV, INFO ) \
  _r8gbtrf( M, N, KL, KU, AB, LDAB, IPIV, INFO )

/*--------------------------------------------------------------------*/

_F_EXTERN_(void) _r8getri( 
  const _F_INTEGER * N , 
        _F_REAL_8  * A ,   const _F_INTEGER * LDA ,
        _F_INTEGER * IPIV , 
        _F_REAL_8  * WORK , _F_INTEGER * LWORK ,
        _F_INTEGER * INFO );

/*--------------------------------------------------------------------*/

#define  r8getri( N, A, LDA, IPIV, WORK, LWORK, INFO ) \
  _r8getri( N, A, LDA, IPIV, WORK, LWORK, INFO )

/*--------------------------------------------------------------------*/

#if _F_STRING_OPTION_ == _F_STRING_HIDE_TRAILING_INT_

_F_EXTERN_(void) _r8getrs(
  const char       * TRANS ,
  const _F_INTEGER * N ,
  const _F_INTEGER * NRHS ,
  const _F_REAL_8  * A ,    const _F_INTEGER * LDA ,
  const _F_INTEGER * IPIV ,
        _F_REAL_8  * B ,    const _F_INTEGER * LDB ,
        _F_INTEGER * INFO 
  /* HIDDEN */ , _F_INTEGER LEN_TRANS );

#define  r8getrs( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO ) \
  _r8getrs( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO, _F_INT_ONE )

_F_EXTERN_(void) _r8gbtrs(
  const char       * TRANS ,
  const _F_INTEGER * N1 ,
  const _F_INTEGER * HBW1,  const _F_INTEGER * HBW2 ,
  const _F_INTEGER * NRHS ,
  const _F_REAL_8  * AB ,   const _F_INTEGER * LDAB ,
  const _F_INTEGER * IPIV ,
        _F_REAL_8  * Y ,
  const _F_INTEGER * N2 ,
        _F_INTEGER * INFO 
  /* HIDDEN */ , _F_INTEGER LEN_TRANS );

#define  r8gbtrs( TRANS, N1, HBW1, HBW2, IONE, AB, LDAB, IPIV, Y, N2, INFO ) \
   _r8gbtrs(TRANS,N1,HBW1,HBW2,IONE,AB,LDAB,IPIV,Y,N2,INFO,_F_INT_ONE)

/*--------------------------------------------------------------------*/

#elif  _F_STRING_OPTION_ == _F_STRING_HIDE_ADJACENT_INT_

_F_EXTERN_(void) _r8getrs(
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const _F_INTEGER * N ,
  const _F_INTEGER * NRHS ,
  const _F_REAL_8  * A ,    const _F_INTEGER * LDA ,
  const _F_INTEGER * IPIV ,
        _F_REAL_8  * B ,    const _F_INTEGER * LDB ,
        _F_INTEGER * INFO );

#define  r8getrs( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO ) \
  _r8getrs( TRANS, _F_INT_ONE, N, NRHS, A, LDA, IPIV, B, LDB, INFO )


_F_EXTERN_(void) _r8gbtrs(
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const _F_INTEGER * N1 ,
  const _F_INTEGER * HBW1,  const _F_INTEGER * HBW2 ,
  const _F_INTEGER * NRHS ,
  const _F_REAL_8  * AB ,   const _F_INTEGER * LDAB ,
  const _F_INTEGER * IPIV ,
        _F_REAL_8  * Y ,
  const _F_INTEGER * N2 ,
        _F_INTEGER * INFO );

#define  r8gbtrs( TRANS, N1, HBW1, HBW2, IONE, AB, LDAB, IPIV, Y, N2, INFO ) \
   _r8gbtrs(TRANS,_F_INT_ONE,N1,HBW1,HBW2,IONE,AB,LDAB,IPIV,Y,N2,INFO)

/*--------------------------------------------------------------------*/

#endif

/*--------------------------------------------------------------------*/

#endif

