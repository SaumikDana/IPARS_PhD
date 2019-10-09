
#ifndef _R8_BLAS_INTERFACE_
#define _R8_BLAS_INTERFACE_

#include "cfsimple.h"

/*--------------------------------------------------------------------*/ 
/* CRAY, as usual, has to be different */

#if defined(CRAY)
#define _R8_NAME_(UN,LN)	_F_NAME_( _PASTE_(S,UN) , _PASTE_(s,LN) )
#else
#define _R8_NAME_(UN,LN)	_F_NAME_( _PASTE_(D,UN) , _PASTE_(d,LN) )
#endif

/*--------------------------------------------------------------------*/ 
/* Only the first character of the string is significant */

#define _F_INT_ONE	((_F_INTEGER)1)

/*--------------------------------------------------------------------*/

#define r8rotg _R8_NAME_(ROTG,rotg)
#define r8rot  _R8_NAME_(ROT,rot)
#define r8swap _R8_NAME_(SWAP,swap)
#define r8scal _R8_NAME_(SCAL,scal)
#define r8copy _R8_NAME_(COPY,copy)
#define r8axpy _R8_NAME_(AXPY,axpy)
#define r8dot  _R8_NAME_(DOT,dot)

#define _r8gemv _R8_NAME_(GEMV,gemv)
#define _r8trmv _R8_NAME_(TRMV,trmv)
#define _r8tbmv _R8_NAME_(TBMV,tbmv)
#define _r8trsv _R8_NAME_(TRSV,trsv)
#define _r8trmm _R8_NAME_(TRMM,trmm)
#define _r8trsm _R8_NAME_(TRSM,trsm)

/*--------------------------------------------------------------------*/ 

_F_EXTERN_(void) r8rotg(
  const _F_REAL_8 * A ,
  const _F_REAL_8 * B ,
        _F_REAL_8 * C ,
        _F_REAL_8 * S );

_F_EXTERN_(void) r8rot(
  const _F_INTEGER * N ,
        _F_REAL_8  * X , const _F_INTEGER * INCX ,
        _F_REAL_8  * Y , const _F_INTEGER * INCY ,
  const _F_REAL_8  * C ,
  const _F_REAL_8  * S );

_F_EXTERN_(void) r8swap(
  const _F_INTEGER * N ,
        _F_REAL_8  * X , const _F_INTEGER * INCX ,
        _F_REAL_8  * Y , const _F_INTEGER * INCY );

_F_EXTERN_(void) r8copy(
  const _F_INTEGER * N ,
  const _F_REAL_8  * X , const _F_INTEGER * INCX ,
        _F_REAL_8  * Y , const _F_INTEGER * INCY );

_F_EXTERN_(void) r8scal(
  const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
        _F_REAL_8  * X , const _F_INTEGER * INCX );

_F_EXTERN_(void) r8axpy(
  const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
  const _F_REAL_8  * X , const _F_INTEGER * INCX ,
        _F_REAL_8  * Y , const _F_INTEGER * INCY );

_F_EXTERN_(_F_REAL_8) r8dot(
  const _F_INTEGER * N ,
  const _F_REAL_8  * X , const _F_INTEGER * INCX ,
  const _F_REAL_8  * Y , const _F_INTEGER * INCY );

/*--------------------------------------------------------------------*/

#if _F_STRING_OPTION_ == _F_STRING_HIDE_TRAILING_INT_

_F_EXTERN_(void) _r8gemv(
  const char       * TRANS ,
  const _F_INTEGER * M ,
  const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
  const _F_REAL_8  * A ,     const _F_INTEGER * LDA ,
  const _F_REAL_8  * X ,     const _F_INTEGER * INCX ,
  const _F_REAL_8  * BETA ,
        _F_REAL_8  * Y ,     const _F_INTEGER * INCY 
  /* HIDDEN */ , _F_INTEGER LEN_TRANS );

#define r8gemv(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY) \
  _r8gemv(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY,_F_INT_ONE)

_F_EXTERN_(void) _r8trmv(
  const char       * UPLO ,
  const char       * TRANS ,
  const char       * DIAG ,
  const _F_INTEGER * N ,
  const _F_REAL_8  * A , const _F_INTEGER * LDA ,
        _F_REAL_8  * X , const _F_INTEGER * INCX
  /* HIDDEN */ , _F_INTEGER LEN_UPLO
  /* HIDDEN */ , _F_INTEGER LEN_TRANS
  /* HIDDEN */ , _F_INTEGER LEN_DIAG );

#define r8trmv(UPLO,TRANS,DIAG,N,A,LDA,X,INCX) \
  _r8trmv(UPLO,TRANS,DIAG,N,A,LDA,X,INCX, _F_INT_ONE,_F_INT_ONE,_F_INT_ONE)

_F_EXTERN_(void) _r8tbmv(
  const char       * UPLO ,
  const char       * TRANS ,
  const char       * DIAG ,
  const _F_INTEGER * N ,
  const _F_INTEGER * K ,
  const _F_REAL_8  * A , const _F_INTEGER * LDA ,
        _F_REAL_8  * X , const _F_INTEGER * INCX
  /* HIDDEN */ , _F_INTEGER LEN_UPLO
  /* HIDDEN */ , _F_INTEGER LEN_TRANS
  /* HIDDEN */ , _F_INTEGER LEN_DIAG );

#define r8tbmv(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX) \
  _r8tbmv(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX, _F_INT_ONE,_F_INT_ONE,_F_INT_ONE)

_F_EXTERN_(void) _r8trsv(
  const char       * UPLO ,
  const char       * TRANS ,
  const char       * DIAG ,
  const _F_INTEGER * N ,
  const _F_REAL_8  * A , const _F_INTEGER * LDA ,
        _F_REAL_8  * X , const _F_INTEGER * INCX
  /* HIDDEN */ , _F_INTEGER LEN_UPLO
  /* HIDDEN */ , _F_INTEGER LEN_TRANS
  /* HIDDEN */ , _F_INTEGER LEN_DIAG );

#define r8trsv(UPLO,TRANS,DIAG,N,A,LDA,X,INCX) \
  _r8trsv(UPLO,TRANS,DIAG,N,A,LDA,X,INCX, _F_INT_ONE,_F_INT_ONE,_F_INT_ONE)


_F_EXTERN_(void) _r8trmm(
  const char       * SIDE ,
  const char       * UPLO ,
  const char       * TRANS ,
  const char       * DIAG ,
  const _F_INTEGER * M ,    const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
  const _F_REAL_8  * A ,    const _F_INTEGER * LDA ,
        _F_REAL_8  * B ,    const _F_INTEGER * LDB 
  /* HIDDEN */ , _F_INTEGER LEN_SIDE
  /* HIDDEN */ , _F_INTEGER LEN_UPLO 
  /* HIDDEN */ , _F_INTEGER LEN_TRANS
  /* HIDDEN */ , _F_INTEGER LEN_DIAG );

#define r8trmm(SIDE,UPLO,TRANS,DIAG,M,N,ALPHA,A,LDA,B,LDB) \
  _r8trmm(SIDE,UPLO,TRANS,DIAG,M,N,ALPHA,A,LDA,B,LDB, \
          _F_INT_ONE,_F_INT_ONE,_F_INT_ONE,_F_INT_ONE)

_F_EXTERN_(void) _r8trsm(
  const char       * SIDE ,
  const char       * UPLO ,
  const char       * TRANS ,
  const char       * DIAG ,
  const _F_INTEGER * M ,    const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
  const _F_REAL_8  * A ,    const _F_INTEGER * LDA ,
        _F_REAL_8  * B ,    const _F_INTEGER * LDB
  /* HIDDEN */ , _F_INTEGER LEN_SIDE
  /* HIDDEN */ , _F_INTEGER LEN_UPLO
  /* HIDDEN */ , _F_INTEGER LEN_TRANS
  /* HIDDEN */ , _F_INTEGER LEN_DIAG );

#define r8trsm(SIDE,UPLO,TRANS,DIAG,M,N,ALPHA,A,LDA,B,LDB) \
  _r8trsm(SIDE,UPLO,TRANS,DIAG,M,N,ALPHA,A,LDA,B,LDB, \
          _F_INT_ONE,_F_INT_ONE,_F_INT_ONE,_F_INT_ONE)

/*--------------------------------------------------------------------*/

#elif _F_STRING_OPTION_ == _F_STRING_HIDE_ADJACENT_INT_

_F_EXTERN_(void) _r8gemv(
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const _F_INTEGER * M ,     const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
  const _F_REAL_8  * A ,     const _F_INTEGER * LDA ,
  const _F_REAL_8  * X ,     const _F_INTEGER * INCX ,
  const _F_REAL_8  * BETA ,
        _F_REAL_8  * Y ,     const _F_INTEGER * INCY );

#define r8gemv(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY) \
  _r8gemv(TRANS,_F_INT_ONE,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)

_F_EXTERN_(void) _r8trmv(
  const char       * UPLO ,  /* HIDDEN */ _F_INTEGER LEN_UPLO ,
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const char       * DIAG ,  /* HIDDEN */ _F_INTEGER LEN_DIAG ,
  const _F_INTEGER * N ,
  const _F_REAL_8  * A , const _F_INTEGER * LDA ,
        _F_REAL_8  * X , const _F_INTEGER * INCX );

#define r8trmv(UPLO,TRANS,DIAG,N,A,LDA,X,INCX) \
  _r8trmv(UPLO,_F_INT_ONE,TRANS,_F_INT_ONE,DIAG,_F_INT_ONE,N,A,LDA,X,INCX)

_F_EXTERN_(void) _r8tbmv(
  const char       * UPLO ,  /* HIDDEN */ _F_INTEGER LEN_UPLO ,
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const char       * DIAG ,  /* HIDDEN */ _F_INTEGER LEN_DIAG ,
  const _F_INTEGER * N ,
  const _F_INTEGER * K ,
  const _F_REAL_8  * A , const _F_INTEGER * LDA ,
        _F_REAL_8  * X , const _F_INTEGER * INCX );

#define r8tbmv(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX) \
  _r8tbmv(UPLO,_F_INT_ONE,TRANS,_F_INT_ONE,DIAG,_F_INT_ONE,N,K,A,LDA,X,INCX)

_F_EXTERN_(void) _r8trsv(
  const char       * UPLO ,  /* HIDDEN */ _F_INTEGER LEN_UPLO ,
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const char       * DIAG ,  /* HIDDEN */ _F_INTEGER LEN_DIAG ,
  const _F_INTEGER * N ,
  const _F_REAL_8  * A , const _F_INTEGER * LDA ,
        _F_REAL_8  * X , const _F_INTEGER * INCX );

#define r8trsv(UPLO,TRANS,DIAG,N,A,LDA,X,INCX) \
  _r8trsv(UPLO,_F_INT_ONE,TRANS,_F_INT_ONE,DIAG,_F_INT_ONE,N,A,LDA,X,INCX)

_F_EXTERN_(void) _r8trmm(
  const char       * SIDE ,  /* HIDDEN */ _F_INTEGER LEN_SIDE ,
  const char       * UPLO ,  /* HIDDEN */ _F_INTEGER LEN_UPLO ,
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const char       * DIAG ,  /* HIDDEN */ _F_INTEGER LEN_DIAG ,
  const _F_INTEGER * M ,     const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
  const _F_REAL_8  * A ,     const _F_INTEGER * LDA ,
        _F_REAL_8  * B ,     const _F_INTEGER * LDB );

#define r8trmm(SIDE,UPLO,TRANS,DIAG,M,N,ALPHA,A,LDA,B,LDB) \
  _r8trmm( SIDE,_F_INT_ONE,UPLO,_F_INT_ONE,TRANS,_F_INT_ONE,DIAG,_F_INT_ONE, \
           M,N,ALPHA,A,LDA,B,LDB)

_F_EXTERN_(void) _r8trsm(
  const char       * SIDE ,  /* HIDDEN */ _F_INTEGER LEN_SIDE ,
  const char       * UPLO ,  /* HIDDEN */ _F_INTEGER LEN_UPLO ,
  const char       * TRANS , /* HIDDEN */ _F_INTEGER LEN_TRANS ,
  const char       * DIAG ,  /* HIDDEN */ _F_INTEGER LEN_DIAG ,
  const _F_INTEGER * M ,     const _F_INTEGER * N ,
  const _F_REAL_8  * ALPHA ,
  const _F_REAL_8  * A ,     const _F_INTEGER * LDA ,
        _F_REAL_8  * B ,     const _F_INTEGER * LDB );

#define r8trsm(SIDE,UPLO,TRANS,DIAG,N,A,LDA,B,LDB) \
  _r8trsm( SIDE,_F_INT_ONE,UPLO,_F_INT_ONE,TRANS,_F_INT_ONE,DIAG,_F_INT_ONE, \
           M,N,ALPHA,A,LDA,B,LDB)

#endif

#endif

