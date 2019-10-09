/*----------------------------------------------------------------------*/
/*   Direct solver for a single grid block                              */
/*----------------------------------------------------------------------*/

/*
#define DEBUGGING
#define DEBUGGING_JAC
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "cfsimple.h"
#include "r8blas.h"
#include "r8lapack.h"

/*----------------------------------------------------------------------*/
/*  Routines with FORTRAN interface (FORTRAN or 'C' called by FORTRAN)  */

#define _callwork  _F_NAME_(CALLWORK,callwork)
#define _sl1dir    _F_NAME_(SL1DIR,sl1dir)
#define _sli1dir   _F_NAME_(SLI1DIR,sli1dir)

/* Routines written in FORTRAN */

_F_EXTERN_(void) _callwork();
_F_EXTERN_(void) _sl1dir();
_F_EXTERN_(void) _sli1dir();

/* Routines written to be called by FORTRAN */

_F_EXTERN_(void) _sliblk();
_F_EXTERN_(void) _sldir();

/*----------------------------------------------------------------------*/
/* Overall IPARS interface */

typedef struct solve_all_type {

  _F_INTEGER   ns   ;   /* Maximum stencil size */
  _F_INTEGER   nev  ;   /* Number of equations/variabls */
  _F_INTEGER   nblk ;   /* Number of blocks */
  _F_INTEGER   nloc ;   /* Number of locally owned grid cells, all blocks */
  _F_INTEGER   ldgrid ; /* Leading dimension of grid-element array */
  _F_INTEGER * jmap ;   /* Grid and Jacobian mapping */
  _F_INTEGER * gmap ;   /* Grid to vector mapping */

  _F_INTEGER * jsmap ;  /* Stencil mapping */

  _F_REAL_4    * gjac ; /* IPARS' Jacobian grid-element array */
  _F_REAL_8    * gres ; /* IPARS' Residual grid-element array */
  _F_REAL_8    * gsol ; /* IPARS' Solution grid-element array */

  _F_INTEGER  idgjac ; /* IPARS' identifier for Jacobian */
  _F_INTEGER  idgres ; /* IPARS' identifier for Residual */
  _F_INTEGER  idgsol ; /* IPARS' identifier for Solution */
  _F_INTEGER  idktmp ; /* IPARS' identifier for stencil type */

  _F_REAL_8  * sol ;      /* Solution vector */
  _F_REAL_8  * res ;      /* Residual vector */
  _F_REAL_8  * mat ;      /* Matrix */
  _F_INTEGER * ipiv ;

} solve_all_type ;

/*--------------------------------------------------------------------*/

static solve_all_type * solve_data = NULL ;

/*----------------------------------------------------------------------*/

static _F_REAL_8 * fr8_alloc( size_t N )
{
  _F_REAL_8 * ptr = (_F_REAL_8 *) malloc( sizeof(_F_REAL_8) * N );

  if ( ptr == (_F_REAL_8 *) NULL ) {
    fprintf(stderr,"solver : malloc failed\n");
    exit(-1);
  }

  return ptr ;
}

static _F_INTEGER * fi_alloc( size_t N )
{
  _F_INTEGER * ptr = (_F_INTEGER *) malloc( sizeof(_F_INTEGER) * N );

  if ( ptr == (_F_INTEGER *) NULL ) {
    fprintf(stderr,"solver : malloc failed\n");
    exit(-1);
  }

  return ptr ;
}

static _F_INTEGER * fi4_alloc( size_t N )
{
  _F_INTEGER * ptr = (_F_INTEGER *) malloc( sizeof(_F_INTEGER) * N );

  if ( ptr == (_F_INTEGER *) NULL ) {
    fprintf(stderr,"solver : malloc failed\n");
    exit(-1);
  }

  return ptr ;
}

/*----------------------------------------------------------------------*/

static int comp_int_array( const void * const I1 , const void * const I2 )
{
  const int N = 3 ;
  register int i ;

  for ( i = 0 ; i < N &&
     ((const unsigned *)I1)[i] == ((const unsigned *)I2)[i] ; ++i );

  return ( i == N ) ? 0 : (
         ( ((const unsigned *)I1)[i] < ((const unsigned *)I2)[i] ) ? -1 : 1 );
}

/*----------------------------------------------------------------------*/
/* IPARS work routine to fill in the block information
   - Leading dimension of block
   - Number of local/active cells of block
   - Grid and jacobian mapping for the block
 */

static void sldirwork(
  const _F_INTEGER * const IDIM,
  const _F_INTEGER * const JDIM,
  const _F_INTEGER * const KDIM,
  const _F_INTEGER * const LDIM,
  const _F_INTEGER * const IL1,
  const _F_INTEGER * const IL2,
  const _F_INTEGER * const JLV1,
  const _F_INTEGER * const JLV2,
  const _F_INTEGER * const KL1,
  const _F_INTEGER * const KL2,
  const _F_INTEGER * const KEYOUT,
  const _F_INTEGER * const NBLK )
{
  const int idim   = *IDIM ;
  const int jdim   = *JDIM ;
  const int kdim   = *KDIM ;
  const int ldgrid = idim * jdim * kdim ;
  const int kinc   = idim * jdim ;
  const int jinc   = idim ;
  const int kbeg   = (*KL1) - 1 ;
  const int kend   = (*KL2) ;
  const int iblk   = *NBLK - 1 ;
  const int ns     = solve_data->ns ;
  const int ldjmap = 2 * ( ns + 1 );

  _F_INTEGER * jmap = NULL ;
  _F_INTEGER * gmap = NULL ;

  int nloc ;
  int k , koff , ic ;

  if ( iblk ) {
    fprintf(stderr,"Direct solver is for a single grid block only\n");
    exit(-1);
  }

  /*------------------------------------------------------------------*/
  /* Loop over 'KEYOUT' to determine number of locally active cells   */

  solve_data->gmap = gmap = fi_alloc( ldgrid );
  { int i ; for ( i = 0 ; i < ldgrid ; ++i ) gmap[i] = -1 ; }

  nloc = 0 ;
  for ( k = kbeg, koff = kbeg * kinc ; k < kend ; ++k, koff += kinc ) {
    const int jbeg = koff + jinc * ( JLV1[k] - 1 );
    const int jend = koff + jinc * ( JLV2[k] );
    int joff ;
    for ( joff = jbeg ; joff < jend ; joff += jinc ) {
      const int ibeg = joff + *IL1 - 1 ;
      const int iend = joff + *IL2 ;
      int ioff ;
      for ( ioff = ibeg ; ioff < iend ; ++ioff ) {
        if ( KEYOUT[ioff] == 1 ) {
          gmap[ioff] = nloc++ ;
        }
      }
    }
  }

  /*------------------------------------------------------------------*/
  /* Update block information */

  solve_data->ldgrid = ldgrid ;
  solve_data->nloc   = nloc ;
  solve_data->jmap   = jmap = fi_alloc( ldjmap * nloc );

  {
    const int iend = ldjmap * nloc ;
    int i ;
    for ( i = 0 ; i < iend ; ++i ) jmap[i] = 0 ;
  }

  /*------------------------------------------------------------------*/
  /* Loop over KEYOUT to determine the grid and jacobian mapping      */

  ic = 0 ;
  for ( k = kbeg, koff = kbeg * kinc ; k < kend ; ++k, koff += kinc ) {
    const int jbeg = JLV1[k] - 1 ;
    const int jend = JLV2[k] ;
    int j , joff ;
    for (j = jbeg, joff = koff + jbeg * jinc ; j < jend ; ++j, joff += jinc){
      const int ibeg = *IL1 - 1 ;
      const int iend = *IL2 ;
      int i , ioff ;
      for ( i = ibeg , ioff = joff + ibeg ; i < iend ; ++i, ++ioff ) {
        if ( KEYOUT[ioff] == 1 ) {
          _F_INTEGER * const map = jmap + ldjmap * ic++ ;
          int is ;

          for ( is = 0 ; is < ldjmap ; ++is ) map[is] = 0 ;

          /* Grid offset for the active cell */

          map[0] = ioff ; /* Grid offset of this cell */
          map[1] = 1 ;    /* Number of coefficients   */

          /* Coefficient for the local cell */

          map[2] = 0 ;          /* Coefficient offset         */
          map[3] = gmap[ioff] ; /* Vector offset of coefficient */

          /* Determine remainder of Jacobian's entries */

          for ( is = 1 ; is < ns ; ++is ) {

            const _F_INTEGER * const jsmap = solve_data->jsmap + 3 * is ;

            const int ig = i + jsmap[0] ;
            const int jg = j + jsmap[1] ;
            const int kg = k + jsmap[2] ;

            if ( 0 <= ig && ig < *IDIM &&
                 0 <= jg && jg < *JDIM &&
                 0 <= kg && kg < *KDIM ) {
              const int igoff = ig + jg * jinc + kg * kinc ;

              if ( gmap[igoff] != -1 ) {
                ++map[1] ;
                map[  2*map[1]] = is ;
                map[1+2*map[1]] = gmap[ igoff ];
              }
            }
          } /* End loop: is */
        } /* End if KEYOUT[ioff] */
      }   /* End loop: i */
    }     /* End loop: j */
  }       /* End loop: k */
}

/*----------------------------------------------------------------------*/
/*  Allocations for solver and mappings for grid interface */

void _sli1dir(
  /* Arguments regarding the grid and jacobian */

  _F_INTEGER * NEV,   /* input  Number of equations/variables */
  _F_INTEGER * NS,    /* input  Stencil size: 7,19, or 27     */
  _F_INTEGER * JSMAP, /* input  Stencil mapping               */
  _F_INTEGER * KTMP,  /* input  IPARS Type of stencil         */
  _F_INTEGER * NBLK ) /* input  Number of blocks              */
{
  const size_t nblk = *NBLK ;
  const size_t nev  = *NEV ;
  const size_t ns   = *NS ;

  size_t nloc = 0 ; /* Number of local unknowns for all grids */
  size_t nall = 0 ; /* nloc * nev */
  int i ;

  /*------------------------------------------------------------------*/
  /* Verify 'JSMAP' */

  if ( *NS < 1 || JSMAP[0] != 0 || JSMAP[1] != 0 || JSMAP[2] != 0 ) {
    fprintf(stderr,"Jacobian stencil mapping (JSMAP) leading entry is non-zero\n");
    exit(-1);
  }

  /*------------------------------------------------------------------*/
  /* Allocate base data */

  if ( solve_data ) {
    /* Free maps, solvers, and preconditioners */

    if ( solve_data->jmap ) free( solve_data->jmap );
    if ( solve_data->gmap ) free( solve_data->gmap );
    if ( solve_data->sol )  free( solve_data->sol );
    if ( solve_data->res )  free( solve_data->res );
    if ( solve_data->mat )  free( solve_data->mat );
    if ( solve_data->ipiv ) free( solve_data->ipiv );

    free( solve_data );

    solve_data = NULL ;
  }

  /* Allocation / zero fill: 'solve_data', 'jsmap', and 'blk' */

  {
    size_t sizealloc =
      sizeof(solve_all_type) +         /* solve_data                   */
      sizeof(_F_INTEGER *) * 3 * ns ;  /* solve_data->jsmap            */

    char * alloc = (char *) calloc( sizealloc , (size_t) 1 );

    if ( alloc == NULL ) {
      fprintf(stderr,"calloc failed\n");
      exit(-1);
    }

    solve_data = (solve_all_type *) alloc ;
      alloc += sizeof(solve_all_type);

    solve_data->jsmap = (_F_INTEGER *) alloc ;
      alloc += sizeof(_F_INTEGER) * 3 * ns ;
  }

  solve_data->nblk   = nblk ;
  solve_data->nev    = nev ;
  solve_data->ns     = ns ;
  solve_data->nloc   = 0 ;
  solve_data->idktmp = *KTMP ;
  solve_data->idgjac = -1 ;
  solve_data->idgres = -1 ;
  solve_data->idgsol = -1 ;

  for ( i = 0 ; i < 3 * ns ; ++i ) solve_data->jsmap[i] = JSMAP[i] ;

  /*------------------------------------------------------------------*/
  /* Sizes and maps of local grid blocks */

  {
    _F_INTEGER callworkdata = 0 ;
    callwork( sldirwork, &callworkdata );
  }

  nall = nev * ( nloc = solve_data->nloc );

  /*------------------------------------------------------------------*/
  /* Local variables dependent upon 'nloc' */

  solve_data->sol = fr8_alloc( nall );
  solve_data->res = fr8_alloc( nall );
  solve_data->mat = fr8_alloc( nall * nall );
  solve_data->ipiv = fi_alloc( nall );
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Copy grid to vector with scaling */

static void slgrid2vec(
  const _F_REAL_8 * const GS ,
        _F_REAL_8 * V ,
  const _F_REAL_8  ALPHA ,
  const int igbeg ,
  const int igend )
{
  const int ldvec  = solve_data->nloc ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );
  const int ldgrid = solve_data->ldgrid ;

  const _F_INTEGER *       jmap = solve_data->jmap ;
  const _F_INTEGER * const jend = jmap + ldvec * ldjmap ;

  int i, ig ;

  if ( ALPHA == 1.0 ) {
    for ( ig = igbeg ; ig < igend ; ++ig ) {
      const _F_REAL_8  * const G = GS + ldgrid * ig ;
      for ( jmap = solve_data->jmap ; jmap < jend ; jmap += ldjmap )
        *V++ = G[*jmap];
    }
  }
  else {
    for ( ig = igbeg ; ig < igend ; ++ig ) {
      const _F_REAL_8  * const G = GS + ldgrid * ig ;
      for ( jmap = solve_data->jmap ; jmap < jend ; jmap += ldjmap )
        *V++ = ALPHA * G[*jmap];
    }
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Copy vector to grid with scaling */

static void slvec2grid(
  _F_REAL_8 * const GS ,
  const _F_REAL_8 * V ,
  const _F_REAL_8  ALPHA ,
  const int igbeg ,
  const int igend )
{
  const _F_INTEGER IONE = 1 ;
  const _F_INTEGER IZERO = 0 ;
  const _F_REAL_8  ZERO  = 0.0 ;
  const _F_INTEGER LDG   = solve_data->ldgrid ;
  const int ldvec  = solve_data->nloc ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );
  const int ldgrid = solve_data->ldgrid ;

  int i, ig ;
  const _F_INTEGER *       jmap = solve_data->jmap ;
  const _F_INTEGER * const jend = jmap + ldvec * ldjmap ;

  if ( ALPHA == 1.0 ) {
    for ( ig = igbeg ; ig < igend ; ++ig ) {
      _F_REAL_8 * const G = GS + LDG * ig ;
      r8copy( &LDG, &ZERO, &IZERO, G, &IONE );
      for ( jmap = solve_data->jmap ; jmap < jend ; jmap += ldjmap )
        G[*jmap] = *V++ ;
    }
  }
  else {
    for ( ig = igbeg ; ig < igend ; ++ig ) {
      _F_REAL_8 * const G = GS + LDG * ig ;
      r8copy( &LDG, &ZERO, &IZERO, G, &IONE );
      for ( jmap = solve_data->jmap ; jmap < jend ; jmap += ldjmap )
        G[*jmap] = ALPHA * *V++ ;
    }
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

static void sljxv(
  const _F_REAL_4 * const AJS ,
  const _F_REAL_8 * const VS ,
        _F_REAL_8 * const VBASE , 
  const int iebeg ,
  const int ieend ,
  const int ivbeg ,
  const int ivend )
{
  const int ldvec  = solve_data->nloc ;
  const int ns     = solve_data->ns ;
  const int nev    = solve_data->nev ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );
  const int ldgrid = solve_data->ldgrid ;

  int i, iv, ie, is ;
  const _F_INTEGER * const jbeg = solve_data->jmap ;
  const _F_INTEGER * const jend = jbeg + ldvec * ldjmap ;
  const _F_INTEGER *       jmap = NULL ;
        _F_REAL_8  * V ;

  for ( iv = ivbeg ; iv < ivend ; ++iv ) {
    const int ivoff = iv * nev ;

    V = VBASE ;

    for ( ie = iebeg ; ie < ieend ; ++ie ) {
      const int ieoff = ns * ( ie + ivoff );

      const _F_REAL_8 * const VI   = VS  + ldvec  * iv ;
      const _F_REAL_4 * const AJGI = AJS + ldgrid * ieoff ;

      for ( jmap = jbeg ; jmap < jend ; jmap += ldjmap , ++V ) {
        const _F_REAL_4 * const A = AJGI + jmap[0] ;
        const int isend = 2 * jmap[1] ;

        for ( is = 2 ; is <= isend ; is += 2 ) {
          *V += A[ jmap[is] * ldgrid ] * VI[ jmap[1+is] ];
        }
      }
    }
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Matrix-vector multiply:  y = alpha * A * x + beta * y */

static void slmatvec(
  _F_INTEGER * DATA ,  /* Input [0:3] = IE1, IE2, IV1, IV2 */
  _F_REAL_8  * ALPHA , /* Input  scaling  */
  _F_REAL_8  * XVEC ,  /* Input  vector   */
  _F_REAL_8  * BETA ,  /* Input  scaling  */
  _F_REAL_8  * YVEC )  /* In/out vector   */
{
  const _F_INTEGER IONE = 1 ;
  const _F_INTEGER IE1  = DATA[0] ;
  const _F_INTEGER IE2  = DATA[1] ;
  const _F_INTEGER IV1  = DATA[2] ;
  const _F_INTEGER IV2  = DATA[3] ;
  const _F_INTEGER IE   = IE1 - 1 ;
  const _F_INTEGER IV   = IV1 - 1 ;
  const _F_INTEGER NE   = IE2 - IE ;
  const _F_INTEGER NV   = IV2 - IV ;

  /* Scale the output vector */

  {
    const _F_INTEGER N = solve_data->nloc * NE ;
    if ( 0.0 == *BETA ) {
      int i ; for ( i = 0 ; i < N ; ++i ) YVEC[i] = 0.0 ;
    }
    else if ( 1.0 != *BETA ) {
      r8scal( &N, BETA, YVEC , &IONE );
    }
  }
  {
    const _F_INTEGER N = solve_data->nloc * NV ;
    if ( *ALPHA == 0.0 ) {
      _F_REAL_8 * const X = solve_data->sol ;
      int i ;
      for ( i = 0 ; i < N ; ++i ) X[i] = 0.0 ;
    }
    else {
      r8copy( &N , XVEC, &IONE, solve_data->sol, &IONE );
      r8scal( &N, ALPHA, solve_data->sol , &IONE );
    }
  }

  /* Apply Jacobian */

  sljxv( solve_data->gjac, solve_data->sol, YVEC, IE, IE2, IV, IV2 );
}

/*----------------------------------------------------------------------*/
/* Load and factor the matrix */

static void sldirfac()
{
  const int ns     = solve_data->ns ;
  const int nev    = solve_data->nev ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  const _F_INTEGER         IONE = 1 ;
  const _F_INTEGER         NALL = solve_data->nloc * nev ;
        _F_INTEGER         INFO = 0 ;
        _F_INTEGER * const IP   = solve_data->ipiv ;
        _F_REAL_8  * const MAT  = solve_data->mat ;
  const _F_REAL_4  * const AJ   = solve_data->gjac ;

  const int                    ldg  = solve_data->ldgrid ;
  const int                    nloc = solve_data->nloc ;
  const int                    nloc2 = nloc * nloc ;
  const int                    ldgns = ldg * ns ;
  const _F_INTEGER     * const jbeg = solve_data->jmap ;
  const _F_INTEGER     * const jend = jbeg + nloc * ldjmap ;

  int i, j, iv, ie, ib, is, ieq ;
  const _F_INTEGER * jmap ;

  /* Fill the matrix */

  {
    const int iend = NALL * NALL ;
    for ( i = 0 ; i < iend ; ++i ) MAT[i] = 0.0 ;
  }

  for ( iv = 0 ; iv < nev ; ++iv ) {
    const ivoff = iv * nev ;

    _F_REAL_8 * M = MAT + iv * nloc * NALL ;

    for ( ie = 0 ; ie < nev ; ++ie ) {
      const ieoff = ns * ( ie + ivoff );

      const _F_REAL_4 * const AJGI = AJ + ldg * ieoff ;

      for ( j = 0, jmap = jbeg ; jmap < jend ; jmap += ldjmap , ++j , ++M ) {
        const _F_REAL_4 * const A = AJGI + jmap[0] ;
        const int isend = 2 * jmap[1] ;

        for ( is = 2 ; is <= isend ; is += 2 ) {
          M[ jmap[1+is] * NALL ] = A[ jmap[is] * ldg ] ;
        }
      }
    }
  }

  r8getrf(&NALL,&NALL,MAT,&NALL,IP,&INFO);
}

/*----------------------------------------------------------------------*/
/* Grab pointers to IPARS' grid element arrays */

static void slptrwork(
  const _F_INTEGER * const IDIM ,
  const _F_INTEGER * const JDIM ,
  const _F_INTEGER * const KDIM ,
  const _F_INTEGER * const LDIM ,
  const _F_INTEGER * const IL1 ,
  const _F_INTEGER * const IL2 ,
  const _F_INTEGER * const JLV1 ,
  const _F_INTEGER * const JLV2 ,
  const _F_INTEGER * const KL1 ,
  const _F_INTEGER * const KL2 ,
  const _F_INTEGER * const KEYOUT ,
  const _F_INTEGER * const NBLK ,
        _F_REAL_4    * const GJAC,  /* IPARS' Jacobian grid */
        _F_REAL_8    * const GRES,  /* IPARS' Residual grid */
        _F_REAL_8    * const GDU )  /* IPARS' Solution grid */
{
  solve_data->gjac = GJAC ;
  solve_data->gres = GRES ;
  solve_data->gsol = GDU ;

#ifdef DEBUGGING_JAC

  {
    _F_INTEGER NS = solve_data->ns ;
    _F_INTEGER NEV = solve_data->nev ;

    _sljacdebug(
      IDIM , JDIM , KDIM , LDIM ,
      IL1 , IL2 , JLV1 , JLV2 , KL1 , KL2 , KEYOUT , NBLK ,
      &NS, &NEV, GJAC );
  }

#endif
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

void _sl1dir(
  _F_INTEGER * IP_GJAC,  /* input:  IPARS id of Jacobian      */
  _F_INTEGER * IP_GRES,  /* input:  IPARS id of Residual      */
  _F_INTEGER * IP_GSOL,  /* in/out: IPARS id of 'GXDELTA'     */
  _F_INTEGER * INFO4)
{
  const _F_INTEGER IZERO = 0 ;
  const _F_INTEGER IONE  = 1 ;
  const _F_REAL_8  ZERO  = 0.0 ;
  const _F_REAL_8  ONE   = 1.0 ;

  const _F_INTEGER NBLK = solve_data->nblk ;
  const _F_INTEGER NEV  = solve_data->nev ;
  const _F_INTEGER NS   = solve_data->ns ;
  const _F_INTEGER NLOC = solve_data->nloc ;
  const _F_INTEGER NALL = NLOC * NEV ;

  _F_REAL_8 * const VSOL = solve_data->sol ;
  _F_REAL_8 * const VRES = solve_data->res ;
  _F_REAL_8 * const MAT  = solve_data->mat ;
  _F_INTEGER * const IPIV = solve_data->ipiv ;

  _F_INTEGER INFO ;
  _F_REAL_8  RNORM ;
  int i , j ;

  /*------------------------------------------------------------------*/
  /* Obtain pointers to IPARS arrays */

  solve_data->idgjac = *IP_GJAC ;
  solve_data->idgres = *IP_GRES ;
  solve_data->idgsol = *IP_GSOL ;

  solve_data->gjac = NULL ;
  solve_data->gres = NULL ;
  solve_data->gsol = NULL ;

  {
    _F_INTEGER callworkdata[4] ;

    callworkdata[0] = 3 ;
    callworkdata[1] = *IP_GJAC ;
    callworkdata[2] = *IP_GRES ;
    callworkdata[3] = *IP_GSOL ;
    _callwork( slptrwork , callworkdata );
  }

  /*------------------------------------------------------------------*/
  /* Copy residual to a more compact vector */

  slgrid2vec( solve_data->gres , VRES , ONE , IZERO , NEV );

  printf("  Input residual norm = %g\n",
    sqrt( r8dot(&NALL,VRES,&IONE,VRES,&IONE) ) );

  /*------------------------------------------------------------------*/

#ifdef DEBUGGING

  /*------------------------------------------------------------------*/
  /* DEBUGGING: Test grid-2-vector */

  slvec2grid( solve_data->gsol , VRES , ONE , IZERO , NEV );
  slgrid2vec( solve_data->gsol , VSOL , ONE , IZERO , NEV );

  for ( i = 0 ; i < NALL ; ++i ) {
    if ( VSOL[i] != VRES[i] ) {
      fprintf(stderr,"grid2vec->vec2grid failed\n");
    }
  }

  /*------------------------------------------------------------------*/
  /* DEBUGGING: replace RHS with JAC * RHS */

  {
    _F_INTEGER imatvec[4] ;
    imatvec[0] = 1 ;
    imatvec[1] = NEV ;
    imatvec[2] = 1 ;
    imatvec[3] = NEV ;

    r8copy( &NALL, VRES, &IONE, VSOL, &IONE );
    slmatvec( imatvec , &ONE, VSOL, &ZERO, VRES );
  }

  fprintf(stderr,"  Debugging: replaced RHS with JAC * RHS");
#endif

  /* END DEBUGGING */
  /*------------------------------------------------------------------*/
  /*  Explicit factorization and solution */

  sldirfac();

  r8copy(&NALL, VRES, &IONE, VSOL, &IONE );

  r8getrs("N",&NALL,&IONE,MAT,&NALL,IPIV,VSOL,&NALL,&INFO);
  *INFO4 = INFO ;

  slvec2grid( solve_data->gsol, VSOL, ONE, IZERO, NEV );

#ifdef DEBUGGING

  /* Compare it to the cooked solution, the original residual */

  slgrid2vec( solve_data->gres , VRES , ONE , IZERO , NEV );

  {
    const _F_INTEGER ITWO = 2 ;
    _F_REAL_8 tmp[2] ;
    tmp[0] = tmp[1] = 0.0 ;
    for ( j = 0 ; j < NALL ; ++j ) {
      const double mag = VRES[j] ;
      const double err = VSOL[j] - VRES[j] ;
      tmp[0] += mag * mag ;
      tmp[1] += err * err ;
    }
    tmp[0] = sqrt( tmp[0] );
    tmp[1] = sqrt( tmp[1] );
    fprintf(stderr,"solution mag = %g, error = %g\n",tmp[0],tmp[1]);
  }
#endif

  return ;
}

