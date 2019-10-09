/*----------------------------------------------------------------------*/
/*   Iterative solver for IPARS' multicomponent Jacobian                */
/*----------------------------------------------------------------------*/


/*
#define DEBUGGING
#define DEBUGGING_SOL
#define DEBUGGING_JAC
//#define DEBUGGING_PQ_JAC
#define DEBUGGING_PQ_LSOR
*/




#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "cfsimple.h"
#include "r8blas.h"
#include "r8lapack.h"

/*----------------------------------------------------------------------*/

#define SL_PREC_DIAG 		1
#define SL_PREC_LINEJAC		2
#define SL_PREC_LINEGS		3
#define SL_PREC_LINESOR		4

#define SL_SOLVE_GMRES		1

/*----------------------------------------------------------------------*/
/*  Routines with FORTRAN interface (FORTRAN or 'C' called by FORTRAN)  */
#define myblkoff   _F_NAME_(MYBLKOFF,myblkoff)
#define myblkdim   _F_NAME_(MYBLKDIM,myblkdim)
#define mycallwork _F_NAME_(MYCALLWORK,mycallwork)

#define _blkdim    _F_NAME_(BLKDIM,blkdim)
#define _blkoff    _F_NAME_(BLKOFF,blkoff)
#define _callwork  _F_NAME_(CALLWORK,callwork)
#define _slupdate  _F_NAME_(SLUPDATE,slupdate)
#define _gr8sum    _F_NAME_(GR8SUM,gr8sum)
#define _slblk     _F_NAME_(SLBLK,slblk)
#define _sliblk    _F_NAME_(SLIBLK,sliblk)
#define _slgmres   _F_NAME_(SLGMRES,slgmres)
#define _slmsg     _F_NAME_(SLMSG,slmsg)
#define _slmsgunit _F_NAME_(SLMSGUNIT,slmsgunit)
#define _sldie     _F_NAME_(SLDIE,sldie)

#define _sljacdebug   _F_NAME_(SLJACDEBUG,sljacdebug)
#define _sljacnorm    _F_NAME_(SLJACNORM,sljacnorm)

#define _set_gs_step    _F_NAME_(SET_GS_STEP,set_gs_step)

/* Routines written in FORTRAN */
_F_EXTERN_(void) myblkoff();
_F_EXTERN_(void) myblkdim();
_F_EXTERN_(void) mycallwork();

_F_EXTERN_(void) _blkdim();
_F_EXTERN_(void) _blkoff();
_F_EXTERN_(void) _callwork();
_F_EXTERN_(void) _gr8sum();
_F_EXTERN_(void) _slgmres();
_F_EXTERN_(void) _sljacdebug();
_F_EXTERN_(void) _slmsg();
_F_EXTERN_(void) _slmsgunit();
_F_EXTERN_(void) _sldie();
_F_EXTERN_(void) _set_gs_step();

/* Routines written to be called by FORTRAN */

_F_EXTERN_(void) _sliblk(
  _F_INTEGER * NEV,   /* input  Number of equations/variables */
  _F_INTEGER * NS,    /* input  Stencil size: 7,19, or 27     */
  _F_INTEGER * JSMAP, /* input  Stencil mapping               */
  _F_INTEGER * KTMP,  /* input  IPARS Type of stencil         */
  _F_INTEGER * NBLK,  /* input  Number of blocks              */
  _F_INTEGER * SPEC ); /* Input: solver & preconditioner spec's */

_F_EXTERN_(void) _slblk(
  _F_INTEGER * IP_GJAC,  /* input:  IPARS id of Jacobian      */
  _F_INTEGER * IP_GRES,  /* input:  IPARS id of Residual      */
  _F_INTEGER * IP_GSOL,  /* in/out: IPARS id of 'GXDELTA'     */
  _F_REAL_4  * RES,      /* in/out: Convergence/actual residual ratio */
  _F_REAL_4  * ARES,     /* in/out: Absolute Convergence tolerance */
  _F_INTEGER * ITER,     /* in/out: Max/Actual iterations             */
  _F_INTEGER * INFO );   /* output: Status of overall solve           */


/*--------------------------------------------------------------------*/

#if _F_STRING_OPTION_ == _F_STRING_HIDE_TRAILING_INT_ || \
    _F_STRING_OPTION_ == _F_STRING_HIDE_ADJACENT_INT_

#define SLMSG(MSG) _slmsg( MSG , strlen(MSG) )
#define SLDIE(MSG) _sldie( MSG , strlen(MSG) )

#else

#error "UNKNOWN FORTRAN STRING OPTION"

#endif

const static char strline[] =
"------------------------------------------------------------------------" ;

#define SLMSGLINE  SLMSG(strline)

/*----------------------------------------------------------------------*/
/* IPARS' grid block specifications are compressed and mapped           */

typedef struct solve_blk_type {

  _F_INTEGER   ldgrid ; /* Leading dimension of grid-element array */
  _F_INTEGER   nloc ;   /* Number of local cells in the grid-element array */
  _F_INTEGER   disp ;   /* Displacement of local cells */
  _F_INTEGER   nblk ;   /* Number of block in line preconditioners */
  _F_INTEGER * jblk ;   /* Blocking for line preconditioners */
  char       * jcolor ; /* Color of line */
  _F_INTEGER * jmap ;   /* Grid and Jacobian mapping */
  _F_INTEGER * mapj ;   /* Map grid -> vector */

  _F_REAL_4  * gjac ;  /* IPARS' Jacobian grid-element array */
  _F_REAL_8  * gres ;  /* IPARS' Residual grid-element array */
  _F_REAL_8  * gsol ;  /* IPARS' Solution grid-element array */

} solve_blk_type ;

/*
  jmap[0]     = Grid element of this vector entry (vector -> grid)
  jmap[1]     = Number of coefficients in the Jacobian's row
  jmap[i*2+0] = Jacobian's offset for the coefficient
  jmap[i*2+1] = Grid element associated with coefficient
*/

/*--------------------------------------------------------------------*/
/* Overall IPARS interface */

#define MAX_SPEC_DATA	2

typedef struct solve_all_type {

  _F_INTEGER ns   ;  /* Maximum stencil size */
  _F_INTEGER nev  ;  /* Number of equations/variabls */
  _F_INTEGER nblk ;  /* Number of blocks */
  _F_INTEGER nloc ;  /* Number of locally owned grid cells, all blocks */
  _F_INTEGER nwork ; /* Size of work space pool */
  _F_INTEGER sspec[ MAX_SPEC_DATA ]; /* Solver spec's */
  _F_INTEGER pspec[ MAX_SPEC_DATA ]; /* Preconditioner spec's */

  solve_blk_type * blk ;    /* Grid block data */
  _F_INTEGER   * jsmap ;  /* Stencil mapping */
  _F_REAL_4      * rnorm ;  /* Residual norms per equation */

  _F_INTEGER   idgjac ; /* IPARS' identifier for Jacobian */
  _F_INTEGER   idgres ; /* IPARS' identifier for Residual */
  _F_INTEGER   idgsol ; /* IPARS' identifier for Solution */
  _F_INTEGER   idktmp ; /* IPARS' identifier for stencil type */

  void      * pwork ;  /* Pressure equation's workspace */

  _F_REAL_8 * sol ;      /* Solution vector */
  _F_REAL_8 * res ;      /* Residual vector */
  _F_REAL_8 * work ;     /* Solver space pool */

} solve_all_type ;

/*--------------------------------------------------------------------*/

static solve_all_type * solve_data = NULL ;

#ifdef DEBUGGING

static solve_all_type * solve_data_backup = NULL ;
static unsigned         solve_data_size   = 0 ;

#endif

/*----------------------------------------------------------------------*/

static _F_REAL_4 * fr4_alloc( size_t N )
{
  _F_REAL_4 * ptr = (_F_REAL_4 *) malloc( sizeof(_F_REAL_4) * N );

  if ( ptr == (_F_REAL_4 *) NULL ) {
    char buf[512] ;
    sprintf(buf,"solver : malloc( %d ) failed",sizeof(_F_REAL_4) * N);
    SLDIE(buf);
  }

  return ptr ;
}

static _F_REAL_8 * fr8_alloc( size_t N )
{
  _F_REAL_8 * ptr = (_F_REAL_8 *) malloc( sizeof(_F_REAL_8) * N );

  if ( ptr == (_F_REAL_8 *) NULL ) {
    char buf[512] ;
    sprintf(buf,"solver : malloc( %d ) failed",sizeof(_F_REAL_8) * N);
    SLDIE(buf);
  }

  return ptr ;
}

static _F_INTEGER * fi_alloc( size_t N )
{
  _F_INTEGER * ptr = (_F_INTEGER *) malloc( sizeof(_F_INTEGER) * N );

  if ( ptr == (_F_INTEGER *) NULL ) {
    char buf[512] ;
    sprintf(buf,"solver : malloc( %d ) failed",sizeof(_F_INTEGER) * N);
    SLDIE(buf);
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

static void slblkwork(
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
  const int nev    = solve_data->nev ;
  const int ldjmap = 2 * ( ns + 1 );

  char       * jcolor = NULL ;
  _F_INTEGER * jblk = NULL ;
  _F_INTEGER * jmap = NULL ;
  _F_INTEGER * mapj = NULL ;

  int k , koff , n , na , ic , ica ;

  _F_INTEGER IGOFF , JGOFF , KGOFF , NERR ;

  /*------------------------------------------------------------------*/
  /* Obtain global offsets for the grid block */

  myblkoff( NBLK , &IGOFF , &JGOFF , &KGOFF , &NERR );

  /*------------------------------------------------------------------*/
  /* Loop over 'KEYOUT' to determine number of locally active cells   */

  n = 0 ;
  na = 0 ;
  for ( n = 0, k = kbeg, koff = kbeg * kinc ; k < kend ; ++k, koff += kinc ) {
    const int jbeg = koff + jinc * ( JLV1[k] - 1 );
    const int jend = koff + jinc * ( JLV2[k] );
    int joff ;
    for ( joff = jbeg ; joff < jend ; joff += jinc ) {
      const int ioffbeg = joff + *IL1 - 1 ;
      const int ioffend = joff + *IL2 ;
      const int nold = n ;
      int ioff ;
      for ( ioff = ioffbeg ; ioff < ioffend ; ++ioff ) {
        if ( KEYOUT[ioff] == 1 ) ++n ;
      }
      if ( nold < n ) ++na ;
    }
  }

  /*------------------------------------------------------------------*/
  /* Update block information */

  solve_data->blk[iblk].ldgrid = ldgrid ;
  solve_data->blk[iblk].nloc   = n ;
  solve_data->blk[iblk].nblk   = na ;
  solve_data->blk[iblk].jblk   = jblk   = fi_alloc( na );
  solve_data->blk[iblk].jcolor = jcolor = malloc( na );
  solve_data->blk[iblk].jmap   = jmap   = fi_alloc( ldjmap * n );
  solve_data->blk[iblk].mapj   = mapj   = fi_alloc( ldgrid );

  {
    int iend = ldjmap * n ;
    int i ;
    for ( i = 0 ; i < na ; ++i ) jcolor[i] = 0 ;
    for ( i = 0 ; i < na ; ++i ) jblk[i] = 0 ;
    for ( i = 0 ; i < iend ; ++i ) jmap[i] = 0 ;
    for ( i = 0 ; i < ldgrid ; ++i ) mapj[i] = -1 ;
  }

  /*------------------------------------------------------------------*/
  /* Determine SFC mapping */

  ic = 0 ;
  for ( k = kbeg, koff = kbeg * kinc ; k < kend ; ++k, koff += kinc ) {
    const int jbeg = JLV1[k] - 1 ;
    const int jend = JLV2[k] ;
    int j , joff ;
    for (j = jbeg, joff = koff + jbeg * jinc ; j < jend ; ++j, joff += jinc){
      const int ibeg = *IL1 - 1 ;
      const int iend = *IL2 ;
      int i , ioff ;
      const unsigned ITWO = 2 ;
      double sfc[2] ;
      unsigned isfc[2] ;

      /* SFC horizontal coordinate for the cell */

      sfc[0] = ((double)(k+1)) / ((double)(kdim+2));
      sfc[1] = ((double)(j+1)) / ((double)(jdim+2));
      fhsfc2d( sfc, &ITWO, isfc );

      for ( i = ibeg , ioff = joff + ibeg ; i < iend ; ++i, ++ioff ) {
        if ( KEYOUT[ioff] == 1 ) {
          _F_INTEGER * const map = jmap + ldjmap * ic++ ;

          /* Ordering (sorting) data */

          map[0] = ((_F_INTEGER *) isfc)[0] ;
          map[1] = ((_F_INTEGER *) isfc)[1] ;
          map[2] = i ;    /* Local Grid 'i' (vertical) */

          /* Other important grid data */

          map[3] = j ;    /* Local Grid 'j' */
          map[4] = k ;    /* Local Grid 'k' */
          map[5] = ioff ; /* Local Grid offset */
        }
      }
    }
  }

  
  qsort( jmap , n , sizeof(int) * ldjmap , comp_int_array );
  

  /*------------------------------------------------------------------*/
  /* Loop over active cells to determine:
     1) the grid and jacobian mapping,
     2) vertical line blocking,
     3) binary (red/black) coloring.
  */

#ifdef DEBUGGING
  SLMSGLINE ;
  SLMSG("Ordering of grid colums, begin");
#endif

  ica = 0 ;
  for ( ic = 0 ; ic < n ; ++ic ) {
    _F_INTEGER * const map = jmap + ldjmap * ic ;

    /* Extract grid information for the current cell */

    const int ig    = map[2] ; /* Local Grid 'i' */
    const int jg    = map[3] ; /* Local Grid 'j' */
    const int kg    = map[4] ; /* Local Grid 'k' */
    const int igoff = map[5] ; /* Local Grid offset */

    int is ;

    /* Blocking specifications */

    if ( jblk[ica] == 0 ) { /* New block */
      jcolor[ica] = ( jg + JGOFF + kg + KGOFF ) % 2 ;

#ifdef DEBUGGING
      {
        char buf[128] ;
        sprintf(buf,"  loc[%7d](%3d,%3d,%3d) <=> glo(%3d,%3d,%3d)",
          igoff+1,ig+1,jg+1,kg+1,ig+IGOFF+1,jg+JGOFF+1,kg+KGOFF+1);
        SLMSG(buf);
      }
#endif

    }

    ++jblk[ica] ; /* Block count */

    if ( ic + 1 < n && ( jg != map[3+ldjmap] || kg != map[4+ldjmap] ) ) {
      ++ica ;
    }

    /* Form the Grid & Jacobian mapping */

    for ( is = 0 ; is < ldjmap ; ++is ) map[is] = 0 ;

    /* Grid offset for the active cell */

    mapj[igoff] = ic ; /* Grid mapping */

    map[0] = igoff ; /* Grid offset of this cell */
    map[1] = 1 ;     /* Number of coefficients   */

    /* Coefficient for the local cell */

    map[2] = 0 ;     /* Coefficient offset         */
    map[3] = igoff ; /* Grid offset of coefficient */

    /* Determine remainder of Jacobian's entries */

    for ( is = 1 ; is < ns ; ++is ) {

      const _F_INTEGER * const jsmap = solve_data->jsmap + 3 * is ;

      const int ign = ig + jsmap[0] ;
      const int jgn = jg + jsmap[1] ;
      const int kgn = kg + jsmap[2] ;

      if ( 0 <= ign && ign < *IDIM &&
           0 <= jgn && jgn < *JDIM &&
           0 <= kgn && kgn < *KDIM ) {
        const int ignoff = ign + jgn * jinc + kgn * kinc ;

        if ( abs( KEYOUT[ignoff] ) == 1 ) {
          ++map[1] ;
          map[  2*map[1]] = is ;
          map[1+2*map[1]] = ignoff ;
        }
      }
    } /* End loop: is */
  }

#ifdef DEBUGGING
  SLMSG("Ordering of grid colums, end");
  SLMSGLINE ;
#endif

}

/*----------------------------------------------------------------------*/
/*  Allocations for solver and mappings for grid interface */

void _sliblk(
  /* Arguments regarding the grid and jacobian */

  _F_INTEGER * NEV,   /* input  Number of equations/variables */
  _F_INTEGER * NS,    /* input  Stencil size: 7,19, or 27     */
  _F_INTEGER * JSMAP, /* input  Stencil mapping               */
  _F_INTEGER * KTMP,  /* input  IPARS Type of stencil         */
  _F_INTEGER * NBLK,  /* input  Number of blocks              */
  _F_INTEGER * SPEC ) /* Input: solver & preconditioner spec's */
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
    SLDIE("Jacobian stencil mapping (JSMAP) leading entry is non-zero");
  }

  /*------------------------------------------------------------------*/
  /* Allocate base data */

  if ( solve_data ) {
    /* Free maps, solvers, and preconditioners */

    for ( i = 0 ; i < solve_data->nblk ; ++i ) {
      if ( solve_data->blk[i].jcolor) free( solve_data->blk[i].jcolor );
      if ( solve_data->blk[i].jblk )  free( solve_data->blk[i].jblk );
      if ( solve_data->blk[i].jmap )  free( solve_data->blk[i].jmap );
      if ( solve_data->blk[i].mapj )  free( solve_data->blk[i].mapj );
    }

    if ( solve_data->pwork )  free( solve_data->pwork );
    if ( solve_data->sol )    free( solve_data->sol );
    if ( solve_data->res )    free( solve_data->res );
    if ( solve_data->work )   free( solve_data->work );

    free( solve_data );
    solve_data = NULL ;
#ifdef DEBUGGING
    free( solve_data_backup );
    solve_data_backup = NULL ;
#endif
  }

  /* Allocation / zero fill: 'solve_data', 'jsmap', 'eqn', and 'blk' */

  {
    size_t sizealloc =
      sizeof(solve_all_type) +         /* solve_data         */
      sizeof(solve_blk_type) * nblk +  /* solve_data->blk    */
      sizeof(_F_INTEGER *) * 3 * ns +  /* solve_data->jsmap  */
      sizeof(_F_REAL_4) * nev ;        /* solve_data->rnorm  */

    char * alloc = (char *) calloc( sizealloc , (size_t) 1 );

#ifdef DEBUGGING
     solve_data_size = sizealloc ;
#endif

    if ( alloc == NULL ) { SLDIE("calloc failed"); }

    solve_data = (solve_all_type *) alloc ;
      alloc += sizeof(solve_all_type);

    solve_data->blk = (solve_blk_type *) alloc ;
      alloc += sizeof(solve_blk_type) * nblk ;

    solve_data->jsmap = (_F_INTEGER *) alloc ;
      alloc += sizeof(_F_INTEGER) * 3 * ns ;

    solve_data->rnorm = (_F_REAL_4 *) alloc ;
      alloc += sizeof(_F_REAL_4 *) * nev ;
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
    mycallwork( slblkwork, &callworkdata );
  }

  for ( i = 0 ; i < nblk - 1 ; ++i ) {
    solve_data->blk[i+1].disp =
      solve_data->blk[i].disp + solve_data->blk[i].nloc ;
  }

  nloc = solve_data->nloc =
    solve_data->blk[i].disp + solve_data->blk[i].nloc ;

  nall = nloc * nev ;

  /*------------------------------------------------------------------*/
  /* Local variables dependent upon 'nloc' */

  solve_data->sol = fr8_alloc( nall );
  solve_data->res = fr8_alloc( nall );

  /*------------------------------------------------------------------*/
  /* Solver */

  {
    size_t max_w = 2 * nall ; /* Vector workspace */

    switch( solve_data->sspec[0] = SPEC[0] ) {
    case SL_SOLVE_GMRES:
      {
        const size_t restart = solve_data->sspec[1] = SPEC[1] ;
        const size_t nwork   = restart + 3 ; /* Number of vectors */
        const size_t mh      = restart + 1 ; /* 'H' Rows    */
        const size_t nh      = restart + 6 ; /* 'H' Columns */
        const size_t num_w   = nall * nwork + mh * nh ;

        if ( max_w < num_w ) max_w = num_w ;
      }
      break ;

    default:
      {
        char buf[512] ;
        sprintf(buf,"Unknown solver specification (1) = %d",SPEC[0]);
        SLDIE(buf);
      }
    }

    solve_data->nwork = max_w ;
    solve_data->work  = fr8_alloc( max_w );
  }

  /* Preconditioner for pressure equation */

  switch( solve_data->pspec[0] = SPEC[2] ) {
  case SL_PREC_DIAG:
    solve_data->pwork = NULL ;
    break ;

/*
  case SL_PREC_BAND:
    {
      const size_t hbw  = solve_data->pspec[1] = SPEC[3] ;
      const size_t ld   = 3 * hbw + 1 ;
      const size_t nloci =
        ( sizeof(_F_INTEGER) * nloc + sizeof(_F_REAL_8) - 1 )
        / sizeof(_F_REAL_8) ;

      solve_data->pwork = (void *) fr8_alloc( ld * nloc + nloci );
    }
    break ;
*/

  case SL_PREC_LINEJAC: /* Line Jacobi */
  case SL_PREC_LINEGS:  /* Line Gauss-Seidel */
  case SL_PREC_LINESOR: /* Line Successive Over Relaxation */
    {
      const size_t hbw  = 1 ;
      const size_t ld   = 3 * hbw + 1 ;
      const size_t nloci =
        ( sizeof(_F_INTEGER) * nloc + sizeof(_F_REAL_8) - 1 )
        / sizeof(_F_REAL_8) ;

      solve_data->pwork = (void *) fr8_alloc( ld * nloc + nloci );
    }
    break ;

  default:
    {
      char buf[512] ;
      sprintf(buf,"Unknown pressure preconditioner specification = %d",
        SPEC[2]);
      SLDIE(buf);
    }
  }

#ifdef DEBUGGING
  solve_data_backup = malloc( solve_data_size );
  memcpy( solve_data_backup , solve_data , solve_data_size );
#endif
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Copy grid to vector with scaling */

static void slgrid2vec(
  const int         USE_RESIDUAL ,
        _F_REAL_8 * V ,
  const _F_REAL_8  ALPHA ,
  const int igbeg ,
  const int igend )
{
  const int nblk   = solve_data->nblk ;
  const int ldvec  = solve_data->nloc ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  int i, ig ;
  _F_INTEGER * jmap ;

  if ( USE_RESIDUAL ) {
    if ( ALPHA == 1.0 ) {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_REAL_8      * const G    = blk->gres + blk->ldgrid * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          for ( ; jmap < jend ; jmap += ldjmap ) *V++ = G[*jmap];
        }
      }
    }
    else {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_REAL_8      * const G    = blk->gres + blk->ldgrid * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          for ( ; jmap < jend ; jmap += ldjmap ) *V++ = ALPHA * G[*jmap];
        }
      }
    }
  }
  else {
    if ( ALPHA == 1.0 ) {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_REAL_8      * const G    = blk->gsol + blk->ldgrid * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          for ( ; jmap < jend ; jmap += ldjmap ) *V++ = G[*jmap];
        }
      }
    }
    else {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_REAL_8      * const G    = blk->gsol + blk->ldgrid * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          for ( ; jmap < jend ; jmap += ldjmap ) *V++ = ALPHA * G[*jmap];
        }
      }
    }
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Copy vector to grid with scaling */

static void slvec2grid(
  const int USE_RESIDUAL ,
  const _F_REAL_8 * V ,
  const _F_REAL_8  ALPHA ,
  const int igbeg ,
  const int igend )
{
  const _F_INTEGER IONE = 1 ;
  const _F_INTEGER IZERO = 0 ;
  const _F_REAL_8  ZERO  = 0.0 ;
  const int nblk   = solve_data->nblk ;
  const int ldvec  = solve_data->nloc ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  int i, ig ;
  _F_INTEGER * jmap ;

  if ( USE_RESIDUAL ) {
    if ( ALPHA == 1.0 ) {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_INTEGER             LDG  = blk->ldgrid ;
                _F_REAL_8      * const G    = blk->gres + LDG * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          r8copy( &LDG, &ZERO, &IZERO, G, &IONE );
          for ( ; jmap < jend ; jmap += ldjmap ) G[*jmap] = *V++ ;
        }
      }
    }
    else {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_INTEGER             LDG  = blk->ldgrid ;
                _F_REAL_8      * const G    = blk->gres + LDG * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          r8copy( &LDG, &ZERO, &IZERO, G, &IONE );
          for ( ; jmap < jend ; jmap += ldjmap ) G[*jmap] = ALPHA * *V++ ;
        }
      }
    }
  }
  else {
    if ( ALPHA == 1.0 ) {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_INTEGER             LDG  = blk->ldgrid ;
                _F_REAL_8      * const G    = blk->gsol + LDG * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          r8copy( &LDG, &ZERO, &IZERO, G, &IONE );
          for ( ; jmap < jend ; jmap += ldjmap ) G[*jmap] = *V++ ;
        }
      }
    }
    else {
      for ( ig = igbeg ; ig < igend ; ++ig ) {
        for ( i = 0 ; i < nblk ; ++i ) {
          const solve_blk_type * const blk  = solve_data->blk + i ;
          const _F_INTEGER             LDG  = blk->ldgrid ;
                _F_REAL_8      * const G    = blk->gsol + LDG * ig ;
          const _F_INTEGER     * const jend = ( jmap = blk->jmap ) +
                                              blk->nloc * ldjmap ;
          r8copy( &LDG, &ZERO, &IZERO, G, &IONE );
          for ( ; jmap < jend ; jmap += ldjmap ) G[*jmap] = ALPHA * *V++ ;
        }
      }
    }
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

static void sljxg(
  _F_REAL_8 *  const VBASE , 
  const int iebeg ,
  const int ieend ,
  const int ivbeg ,
  const int ivend )
{
  const int nblk   = solve_data->nblk ;
  const int ldvec  = solve_data->nloc ;
  const int ns     = solve_data->ns ;
  const int nev    = solve_data->nev ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  int i, iv, ie, is ;
  const _F_INTEGER * jmap ;
        _F_REAL_8  * V ;
    char buf[512] ;
#ifdef DEBUGGING_PQ_JAC
    SLMSG("Jacobian multiply");
#endif

  for ( iv = ivbeg ; iv < ivend ; ++iv ) {
    const int ivoff = iv * nev ;

    V = VBASE ;

    for ( ie = iebeg ; ie < ieend ; ++ie ) {
      const int ieoff = ns * ( ie + ivoff );

      for ( i = 0 ; i < nblk ; ++i ) {
        const solve_blk_type * const blk  = solve_data->blk + i ;
        const int                    ldg  = blk->ldgrid ;
        const int                    nloc = blk->nloc ;
        const _F_INTEGER     * const jbeg = blk->jmap ;
        const _F_INTEGER     * const jend = jbeg + nloc * ldjmap ;
        const _F_REAL_8      * const GI   = blk->gsol + ldg * iv ;
        const _F_REAL_4      * const AJGI = blk->gjac + ldg * ieoff ;

        for ( jmap = jbeg ; jmap < jend ; jmap += ldjmap , ++V ) {
          const _F_REAL_4 * const A = AJGI + jmap[0] ;
          const int isend = 2 * jmap[1] ;

          for ( is = 2 ; is <= isend ; is += 2 ) {
#ifdef DEBUGGING_PQ_JAC
	      sprintf(buf,"i = %d, j = %d, A = %.15g",(jmap-jbeg)/ldjmap, jmap[1+is], A[ jmap[is] * ldg ]);
	      SLMSG(buf);
#endif
            *V += A[ jmap[is] * ldg ] * GI[ jmap[1+is] ];
          }
        }
      }
    }
  }
#ifdef DEBUGGING_PQ_JAC
    sprintf(buf,"fin de l'ecriture du jacobien");
    SLDIE(buf);
#endif
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Matrix-vector multiply:  y = alpha * A * x + beta * y */

static void slmatvec(
  const _F_INTEGER * const DATA ,  /* Input [0:3] = IE1, IE2, IV1, IV2 */
  const _F_REAL_8  * const ALPHA , /* Input  scaling  */
  const _F_REAL_8  * const XVEC ,  /* Input  vector   */
  const _F_REAL_8  * const BETA ,  /* Input  scaling  */
        _F_REAL_8  * const YVEC )  /* In/out vector   */
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

  /* Copy input vector to work (solution) grid, with scaling */

  slvec2grid( 0, XVEC, *ALPHA , IV , IV2 );

  /* Update the work grid via IPARS' update-range routine */

  {
    const _F_INTEGER I4DG = solve_data->idgsol ;
    const _F_INTEGER K4TG = solve_data->idktmp ;
    const _F_INTEGER I4V1 = IV1 ;
    const _F_INTEGER I4V2 = IV2 ;
    _slupdate( &I4DG , &I4V1, &I4V2, &K4TG );
  }

  /* Apply Jacobian */

  sljxg( YVEC, IE, IE2, IV, IV2 );
}

/*----------------------------------------------------------------------*/
/* Apply diagonal preconditioner to vector */

static void slpressdiagsol(
  const _F_INTEGER * const IOPT,  /* Input: which preconditioner option */
  const _F_REAL_8  * const X ,    /* Input  */
        _F_REAL_8  * const Y )    /* Output */
{
  const int nblk   = solve_data->nblk ;
  const int ns     = solve_data->ns ;
  const int nev    = solve_data->nev ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  const _F_REAL_8 * V = X ;
        _F_REAL_8 * W = Y ;

  int i, j, iv, ie, ib, is, ic ;

  const _F_INTEGER * jmap ;

  for ( i = 0 ; i < nblk ; ++i ) {
    const solve_blk_type * const blk  = solve_data->blk + i ;
    const int                    ldg  = blk->ldgrid ;
    const int                    nloc = blk->nloc ;
    const _F_INTEGER     * const jbeg = blk->jmap ;
    const _F_INTEGER     * const jend = jbeg + nloc * ldjmap ;
    const _F_REAL_4      * const AJGI = blk->gjac ;

    for ( jmap = jbeg ; jmap < jend ; jmap += ldjmap , ++V , ++W ) {
      *W = *V / *(AJGI + jmap[0]); /* Leading block is diagonal */
    }
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* John Wheeler's magic formula for Line-SOR Omega
   for uniform grid blocks in a reservior simulator.
 */

static double linesoromega( int iblk )
{
  _F_INTEGER NBLK = iblk + 1 ;
  _F_INTEGER NXG , NYG , NZG , NERROR ;
  double omega ;

  myblkdim(&NBLK,&NXG,&NYG,&NZG,&NERROR);

  omega = ( NYG < NZG ) ? NZG : NYG ;
  omega = 1.0 - 4.9348 / ( omega * omega );
  omega = 2.0 / ( 1.0 + sqrt( 1.0 - omega * omega ) );
  omega = omega + ( 2.0 - omega ) * 0.13 ;

  return omega ;
}

/*----------------------------------------------------------------------*/
/* Apply line (block) SOR preconditioner to vector
     ( *IOPT % 4 ) == 0  =>  Forward SFC sweep ordering
     ( *IOPT % 4 ) == 1  =>  Backward SFC sweep ordering
     ( *IOPT % 4 ) == 2  =>  Black ordering
     ( *IOPT % 4 ) == 3  =>  Red ordering
 */

static void slpresslinesorsol(
  const _F_INTEGER * const IOPT,  /* Input: which preconditioner option */ 
  const _F_REAL_8  * const OMEGA, /* Input: Omega input (0 == compute) */
  const _F_REAL_8  * const X ,    /* Input  */
        _F_REAL_8  *       Y )    /* Output */
{
  const int iomega = ( *OMEGA != 0.0 );
  const int nblk   = solve_data->nblk ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  const _F_INTEGER IZERO = 0 ;
  const _F_REAL_8  ZERO  = 0.0 ;
  const _F_INTEGER   IONE = 1 ;
  const _F_INTEGER   HBW  = 1 ;
  const _F_INTEGER   LDP  = 3 * HBW + 1 ;
  const _F_INTEGER   N    = solve_data->nloc ;
        _F_INTEGER   INFO = 0 ;
  _F_REAL_8 * VI = Y ;
  _F_INTEGER * jmap ;
  _F_INTEGER * jblk ;
  int i , j , k , is, istep ;
  int nstep;
  char buf[512] ;

  // get the current value of nstep from input file
  _set_gs_step(&nstep);
  
    /* Copy input vector to work (residual) grid, with scaling */
    //  if ( X != Y ) r8copy( &N, X, &IONE, Y, &IONE );

  /*------------------------------------------------------------------*/
  switch( *IOPT % 4 ) {
  case 1:
  case 3: {

    const int color = ( ( *IOPT % 4 ) == 1 ) ? 1 : 0 ;
    char   * jcolor ;

   /* Do nstep RB-linesor sweeps */
    for (istep = 0; istep < nstep; ++istep ) {
	/* 'Red' lines */

    const _F_REAL_8  * P  = solve_data->pwork ;
    const _F_INTEGER * IP = (_F_INTEGER *)( P + LDP * N );
          _F_REAL_8  * VI = Y ;

    /* Copy input vector to work (residual) grid, with scaling */
	if (istep > 0 ) r8copy( &N, X, &IONE, Y, &IONE );

#ifdef DEBUGGING_PQ_LSOR
		  SLMSG("'Red' lines");
#endif
    for ( i = 0 ; i < nblk ; ++i ) {
      const solve_blk_type * const blk   = solve_data->blk + i ;
      const double                 omega = iomega ? *OMEGA : 
        /* linesoromega(i) ;    */
            1.0 + istep*(linesoromega(i) - 1.0)/(nstep+1); 
       
      const int                    ldg   = blk->ldgrid ;
      const int                    nloc  = blk->nloc ;
      const _F_REAL_4      * const AJGI  = blk->gjac ;
            _F_REAL_8      * const GI    = blk->gsol ;
            _F_REAL_8      *       VB    = VI ;

      jmap   = blk->jmap ;
      jblk   = blk->jblk ;
      jcolor = blk->jcolor ;

      for ( j = 0 ; j < nloc ; ) {

        const _F_INTEGER NBJ = *jblk++ ;

        if ( color == *jcolor++ ) {

          const int igfirst = *jmap ;
          const int iglast  = *(jmap + ldjmap * ( NBJ - 1));

          /* Update current vector block */

          if ( istep > 0 ) {
	      for ( k = 0 ; k < NBJ ; ++k , jmap += ldjmap ) {
		  const _F_REAL_4 * const A = AJGI + jmap[0] ;
		  const int isend = 2 * jmap[1] ;
		  double tmp = 0.0 ;
		  for ( is = 2 ; is <= isend ; is += 2 ) {
		      const int ig = jmap[1+is] ;
		      if ( ig < igfirst || iglast < ig ) {
#ifdef DEBUGGING_PQ_LSOR
			  sprintf(buf,"k+j = %d, ig = %d", k+j, ig);
			  SLMSG(buf);
#endif
			  tmp += A[ jmap[is] * ldg ] * GI[ig] ;
		      }
		  }
		  VB[k] -= tmp ;
	      }
	  }
	  else jmap += ldjmap * NBJ ;

          /* Solve for current vector block */

          r8gbtrs("N",(_F_INTEGER *)&NBJ,&HBW,&HBW,&IONE,P,&LDP,IP,VB,(_F_INTEGER *)&NBJ,&INFO);

          if ( 0 != INFO ) {
            SLDIE("lapack band solve failed");
          }

          if ( omega != 1.0 ){
	      if (istep > 0 ) for ( k = 0 ; k < NBJ ; ++k ) VB[k] = GI[igfirst+k] += omega*(VB[k] - GI[igfirst+k]);
	      else for ( k = 0 ; k < NBJ ; ++k ) GI[igfirst+k] = VB[k] *= omega ;
	  }
	  else for ( k = 0 ; k < NBJ ; ++k ) GI[igfirst+k] = VB[k] ;
	}
        else {
          jmap += ldjmap * NBJ ;
        }

        P  += LDP * NBJ ;
        IP += NBJ ;
        VB += NBJ ;
        j  += NBJ ;
      }
      VI += nloc ;
    }

    /* Update grid */
    {
      const _F_INTEGER I4DG = solve_data->idgsol ;
      const _F_INTEGER K4TG = solve_data->idktmp ;
      const _F_INTEGER I4V1 = 1 ;
      const _F_INTEGER I4V2 = 1 ;
      _slupdate( &I4DG , &I4V1, &I4V2, &K4TG );
    }

    /* 'Black' lines */
#ifdef DEBUGGING_PQ_LSOR
    SLMSG("'Black' lines");
#endif

    P  = solve_data->pwork ;
    IP = (_F_INTEGER *)( P + LDP * N );
    VI = Y ;

    for ( i = 0 ; i < nblk ; ++i ) {
      const solve_blk_type * const blk   = solve_data->blk + i ;
      const double                 omega = iomega ? *OMEGA :
          /* linesoromega(i) ;    */
               1.0 + istep*(linesoromega(i) - 1.0)/(nstep+1); 
      const int                    ldg   = blk->ldgrid ;
      const int                    nloc  = blk->nloc ;
      const _F_REAL_4      * const AJGI  = blk->gjac ;
            _F_REAL_8      * const GI    = blk->gsol ;
            _F_REAL_8      *       VB    = VI ;

      jmap   = blk->jmap ;
      jblk   = blk->jblk ;
      jcolor = blk->jcolor ;

      for ( j = 0 ; j < nloc ; ) {

        const _F_INTEGER NBJ = *jblk++ ;

        if ( color == *jcolor++ ) {
          jmap += ldjmap * NBJ ;
        }
        else {
          const int igfirst = *jmap ;
          const int iglast  = *(jmap + ldjmap * ( NBJ - 1));

          /* Update current vector block */

          for ( k = 0 ; k < NBJ ; ++k , jmap += ldjmap ) {
            const _F_REAL_4 * const A = AJGI + jmap[0] ;
            const int isend = 2 * jmap[1] ;
            double tmp = 0.0 ;
	    // debug PQ
            for ( is = 2 ; is <= isend ; is += 2 ) {
              const int ig = jmap[1+is] ;
              if ( ig < igfirst || iglast < ig ) {
#ifdef DEBUGGING_PQ_LSOR
		  sprintf(buf,"k+j = %d, ig = %d", k+j, ig);
		  SLMSG(buf);
#endif
		tmp += A[ jmap[is] * ldg ] * GI[ig] ;
              }
            }
            VB[k] -= tmp ;
          }

          /* Solve for current vector block */

          r8gbtrs("N",&NBJ,&HBW,&HBW,&IONE,P,&LDP,IP,VB,&NBJ,&INFO);

          if ( 0 != INFO ) {
            SLDIE("lapack block solve failed");
          }

          if ( omega != 1.0 ){
	      if (istep > 0 ) for ( k = 0 ; k < NBJ ; ++k ) VB[k] = GI[igfirst+k] += omega*(VB[k] - GI[igfirst+k]);
	      else for ( k = 0 ; k < NBJ ; ++k ) GI[igfirst+k] = VB[k] *= omega ;
	  }
	  else for ( k = 0 ; k < NBJ ; ++k ) GI[igfirst+k] = VB[k] ;
        }

        P  += LDP * NBJ ;
        IP += NBJ ;
        VB += NBJ ;
        j  += NBJ ;
      }
      VI += nloc ;
    }

    /* Update grid */
    if (istep < nstep - 1 ) {
      const _F_INTEGER I4DG = solve_data->idgsol ;
      const _F_INTEGER K4TG = solve_data->idktmp ;
      const _F_INTEGER I4V1 = 1 ;
      const _F_INTEGER I4V2 = 1 ;
      _slupdate( &I4DG , &I4V1, &I4V2, &K4TG );
    }
    }
    } break ;
    /*----------------------------------------------------------------*/
  case 2: { /* Forward SFC sweep ordering */
    const _F_REAL_8  * P    = solve_data->pwork ;
    const _F_INTEGER * IP   = (_F_INTEGER *)( P + LDP * N );

    for ( i = 0 ; i < nblk ; ++i ) {
      const solve_blk_type * const blk   = solve_data->blk + i ;
      const double                 omega = iomega ? *OMEGA : linesoromega(i) ;
      const int                    ldg   = blk->ldgrid ;
      const int                    nloc  = blk->nloc ;
      const _F_INTEGER     * const mapj  = blk->mapj ;
      const _F_REAL_4      * const AJGI  = blk->gjac ;
            _F_REAL_8      *       VB    = VI ;

      jblk = blk->jblk ;
      jmap = blk->jmap ;

      for ( j = 0 ; j < nloc ; ) {

        const _F_INTEGER NBJ = *jblk++ ;

        /* Update current vector block */

        for ( k = 0 ; k < NBJ ; ++k , jmap += ldjmap ) {
          const _F_REAL_4 * const A = AJGI + jmap[0] ;
          const int isend = 2 * jmap[1] ;
          double tmp = 0.0 ;
          for ( is = 2 ; is < isend ; is += 2 ) {
            const int iv = mapj[ jmap[1+is] ] ;
            if ( 0 <= iv && iv < j ) tmp += A[ jmap[is] * ldg ] * VI[iv] ;
          }
          VB[k] -= tmp ;
        }

        /* Solve for current vector block */

        r8gbtrs("N",&NBJ,&HBW,&HBW,&IONE,P,&LDP,IP,VB,&NBJ,&INFO);

        if ( 0 != INFO ) {
          SLDIE("lapack band solve failed");
        }

        if ( omega != 1.0 ) 
	  for ( k = 0 ; k < NBJ ; ++k )  VB[k] *= omega ;

        P  += LDP * NBJ ;
        IP += NBJ ;
        VB += NBJ ;
        j  += NBJ ;
      }
      VI += nloc ;
    }
    
    } break ;
    /*----------------------------------------------------------------*/
  case 0: { /* Backward SFC sweep ordering */

    const _F_REAL_8  * PI  = solve_data->pwork ;
    const _F_INTEGER * IPI = (_F_INTEGER *)( PI + LDP * N );

    for ( i = 0 ; i < nblk ; ++i ) {
      const solve_blk_type * const blk   = solve_data->blk + i ;
      const double                 omega = iomega ? *OMEGA : linesoromega(i) ;
      const int                    ldg   = blk->ldgrid ;
      const int                    nloc  = blk->nloc ;
      const _F_INTEGER     * const mapj  = blk->mapj ;
      const _F_REAL_4      * const AJGI  = blk->gjac ;
            _F_REAL_8      *       VB    = VI + nloc ;
      const _F_REAL_8      *       PB    = PI + LDP * nloc ;
      const _F_INTEGER     *       IPB   = IPI + nloc ;

      jblk = blk->jblk + blk->nblk ;
      jmap = blk->jmap + nloc * ldjmap ;

      for ( j = nloc ; 0 < j ; ) {

        const _F_INTEGER NBJ = *--jblk ;

        PB   -= LDP * NBJ ;
        IPB  -= NBJ ;
        VB   -= NBJ ;
        jmap -= NBJ * ldjmap ;

        /* Update current vector block */

        for ( k = 0 ; k < NBJ ; ++k , jmap += ldjmap ) {
          const _F_REAL_4 * const A = AJGI + jmap[0] ;
          const int isend = 2 * jmap[1] ;
          double tmp = 0.0 ;
          for ( is = 2 ; is < isend ; is += 2 ) {
            const int iv = mapj[ jmap[1+is] ] ;
            if ( j <= iv ) tmp += A[ jmap[is] * ldg ] * VI[iv] ;
          }
          VB[k] -= tmp ;
        }

        j -= NBJ ;

        /* Solve for current vector block */

        r8gbtrs("N",&NBJ,&HBW,&HBW,&IONE,PB,&LDP,IPB,VB,&NBJ,&INFO);

        if ( 0 != INFO ) {
          SLDIE("lapack block solve failed");
        }

        if ( omega != 1.0 ) 
	  for ( k = 0 ; k < NBJ ; ++k ) VB[k] *= omega ;

      }

      VI  += nloc ;
      PI  += LDP * nloc ;
      IPI += nloc ;
   }
   } break ;
  }
}

/*----------------------------------------------------------------------*/

static void slpresslinefac()
{
  const int nblk   = solve_data->nblk ;
  const int ns     = solve_data->ns ;
  const int nev    = solve_data->nev ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  const _F_INTEGER         IONE  = 1 ;
  const _F_INTEGER         NALL  = solve_data->nloc ;
  const _F_INTEGER         HBW   = 1 ;
  const _F_INTEGER         MB    = 2 * HBW ;
  const _F_INTEGER         LDP   = 3 * HBW + 1 ;
        _F_REAL_8  * const PALL  = solve_data->pwork ;
        _F_INTEGER * const IPALL = (_F_INTEGER *)( PALL + LDP * NALL );
        _F_INTEGER         INFO  = 0 ;

  int i, j, k, iv, ie, ib, is ;
  const _F_INTEGER * jblk = NULL ;
  const _F_INTEGER * jmap = NULL ;

  /* Fill the preconditioner matrix */

  _F_REAL_8 * P = PALL ;
  _F_INTEGER * IP = IPALL ;

  {
    const int iend = LDP * NALL ;
    for ( i = 0 ; i < iend ; ++i ) PALL[i] = 0.0 ;
  }

  for ( i = 0 ; i < nblk ; ++i ) {
    const solve_blk_type * const blk  = solve_data->blk + i ;
    const int                    ldg  = blk->ldgrid ;
    const int                    nloc = blk->nloc ;
    const _F_INTEGER     * const mapj = blk->mapj ;
    const _F_REAL_4      * const AJGI = blk->gjac ;

    jblk = blk->jblk ;
    jmap = blk->jmap ;

    for ( j = 0 ; j < nloc ; ) {

      const _F_INTEGER NBJ = *jblk++ ;

      /* Fill block matrix */

      for ( k = 0 ; k < NBJ ; ++k , jmap += ldjmap ) {
        const _F_REAL_4 * const A = AJGI + jmap[0] ;
        const int isend = 2 * jmap[1] ;

        for ( is = 2 ; is <= isend ; is += 2 ) {
          const int ib = mapj[ jmap[is+1] ] - ( j + k );
          if ( - HBW <= ib && ib <= HBW ) {
            P[(MB-ib)+(k+ib)*LDP] = A[ jmap[is] * ldg ];
          }
        }
      }

      /* Band factorization */

      r8gbtrf(&NBJ,&NBJ,&HBW,&HBW,P,&LDP,IP,&INFO);

      if ( 0 != INFO ) {
        SLDIE("lapack band factorization failed");
      }

      IP += NBJ ;
      P  += LDP * NBJ ;
      j  += NBJ ;
    }
  }
}

/*----------------------------------------------------------------------*/
/* Block diagonal (Jacobi) preconditioning for component equations.
   The cell centered block has been "decoupled" to lower triangular
   -> forward substitution
 */

static void slcompblksol( _F_REAL_8 * X , _F_REAL_8 * Y )
{
  const int nblk   = solve_data->nblk ;
  const int ns     = solve_data->ns ;
  const int nev    = solve_data->nev ;
  const int nev1   = nev - 1 ;
  const int ldvec  = solve_data->nloc ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  const _F_REAL_8 * V = X ;
        _F_REAL_8 * W = Y ;

  int i, j, iv, ie, ib, is, ic ;

  const _F_INTEGER * jmap ;

#ifdef DEBUGGING
  _F_REAL_4 * ATMP = fr4_alloc( ( nev - 1 ) * ( nev - 1 ) );
#endif

  for ( i = 0 ; i < nblk ; ++i ) {
    const solve_blk_type * const blk   = solve_data->blk + i ;
    const int                    ldg   = blk->ldgrid ;
    const int                    ldgns = ldg * ns ;
    const int                    nloc  = blk->nloc ;
    const _F_INTEGER     * const jbeg  = blk->jmap ;
    const _F_INTEGER     * const jend  = jbeg + nloc * ldjmap ;
    const _F_REAL_4      * const AJGI  = blk->gjac + (nev+1) * ldgns ;

    for ( jmap = jbeg ; jmap < jend ; jmap += ldjmap , ++V , ++W ) {

      const _F_REAL_4 * const A = AJGI + jmap[0] ;

#ifdef DEBUGGING
      for ( iv = 0 ; iv < nev1 ; ++iv ) {
        for ( ie = 0 ; ie < nev1 ; ++ie ) {
          ATMP[ie+iv*(nev-1)] = A[(ie+iv*nev)*ldgns] ;
        }
      }
#endif

      /* Forward substitution */

      for ( iv = 0 ; iv < nev1 ; ++iv ) {
        W[iv*ldvec] = V[iv*ldvec] / A[iv*(1+nev)*ldgns] ;
        for ( ie = iv + 1 ; ie < nev1 ; ++ie ) {
          W[ie*ldvec] -= A[(ie+iv*nev)*ldgns] * W[iv*ldvec] ;
        }
      }
    }
  }
#ifdef DEBUGGING
  free( ATMP );
#endif
}

/*----------------------------------------------------------------------*/
/* Main block Gauss-Seidel preconditioner using equation/component
   preconditioner for diagonal blocks.
*/

static void slprecmainsol(
  _F_INTEGER * DATA , /* Input:  */
  _F_INTEGER * IOPT , /* Input:  */
  _F_REAL_8  * X ,    /* Input:  */
  _F_REAL_8  * Y )    /* Output: */
{
  const _F_INTEGER IONE = 1 ;
  const _F_INTEGER NEV  = solve_data->nev ;
  const _F_INTEGER NLOC = solve_data->nloc ;
  const _F_INTEGER NALL = NLOC * NEV ;

  const _F_INTEGER I4DG = solve_data->idgsol ;
  const _F_INTEGER K4TG = solve_data->idktmp ;

  int j ;

  /*------------------------------------------------------------------*/

  if ( X != Y ) r8copy( &NALL, X, &IONE, Y, &IONE );

  /* The pressure (leading) block is dominant */

  switch( solve_data->pspec[0] ) {
  case SL_PREC_DIAG: slpressdiagsol(  IOPT, Y, Y ); break ;

  case SL_PREC_LINEJAC:
    {

    }
    break ;
  case SL_PREC_LINEGS:
    {
      const _F_REAL_8 OMEGA = 1.0 ;
      slpresslinesorsol( IOPT  , (_F_REAL_8 *) &OMEGA, X, Y )  ;

    }
    break ;
  case SL_PREC_LINESOR:
    {
      const _F_REAL_8 ZERO = 0.0 ;
      slpresslinesorsol( IOPT  , (_F_REAL_8 *) &ZERO, X, Y )  ;
    }
    break ;

  default:
    ;
  }

  if ( 1 < NEV ) {

    /* Update RHS for remaining blocks */

    slvec2grid( 0, Y, -1.0, 0, 1);
    /* ++++++++ Update the work grid via IPARS' update-range routine */
    {
      const _F_INTEGER I4DG = solve_data->idgsol ;
      const _F_INTEGER K4TG = solve_data->idktmp ;
      const _F_INTEGER I4V1 = 1 ;
      const _F_INTEGER I4V2 = 1 ;
      _slupdate( &I4DG , &I4V1, &I4V2, &K4TG );
    }
    /* ++++++++++ */

    sljxg(Y+NLOC,1,NEV,0,1);

    /* Block Jacobi for remaining equations/variables */

    slcompblksol( Y + NLOC, Y + NLOC );
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Grab pointers to IPARS' grid element arrays */

#ifdef DEBUGGING
static _F_REAL_8 AJACNORM = 0.0 ;
#endif

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
  if ( *NBLK <= 0 || solve_data->nblk < *NBLK )
    SLDIE("Bad block number in a work routine");

  solve_data->blk[ *NBLK - 1 ].gjac = GJAC ;
  solve_data->blk[ *NBLK - 1 ].gres = GRES ;
  solve_data->blk[ *NBLK - 1 ].gsol = GDU ;

#ifdef DEBUGGING
  {
    _F_INTEGER NS = solve_data->ns ;
    _F_INTEGER NEV = solve_data->nev ;

    _sljacnorm(
      IDIM , JDIM , KDIM , LDIM ,
      IL1 , IL2 , JLV1 , JLV2 , KL1 , KL2 , KEYOUT , NBLK ,
      &NS, &NEV, GJAC , &AJACNORM );
  }
#endif

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
/* Apply "decoupling" to the multicomponent linear system.      */

static void sldecouple( _F_REAL_8 * V )
{
  const int NEV    = solve_data->nev ;
  const int nblk   = solve_data->nblk ;
  const int ldvec  = solve_data->nloc ;
  const int ns     = solve_data->ns ;
  const int ldjmap = 2 * ( solve_data->ns + 1 );

  _F_REAL_8 * const W = solve_data->work ;
  _F_REAL_8 * const W2 = solve_data->work + NEV ;

  const _F_INTEGER * jmap ;

  _F_INTEGER INFO ;

  int i, iv, ne, ie, ivoff, ieoff, is ;

  for ( i = 0 ; i < nblk ; ++i ) {
    const solve_blk_type * const blk   = solve_data->blk + i ;
    const int                    nloc  = blk->nloc ;
    const int                    ldg   = blk->ldgrid ;
    const int                    ldgns = ldg * ns ;
    const _F_INTEGER     * const jbeg  = blk->jmap ;
    const _F_INTEGER     * const jend  = jbeg + nloc * ldjmap ;
          _F_REAL_4      * const AI    = blk->gjac ;

    for ( jmap = jbeg ; jmap < jend ; jmap += ldjmap , ++V ) {
      const int isinc = 2 ;
      const int isbeg = isinc * 1 ;
      const int isend = isinc * jmap[1] ;

      for ( ne = NEV - 1 ; 0 < ne ; --ne ) {
        double mag ;

        { /* Generate Householder for cell center */

          _F_REAL_4 * const A = AI + jmap[0] + ne * NEV * ldgns ;

          mag = 0.0 ;
          for ( ie = 0 ; ie <= ne ; ++ie ) {
            const double val = W[ie] = A[ ldgns * ie ] ;
            mag += val * val ;
          }
        }

        W[ne] += ( 0 < W[ne] ) ? sqrt(mag) : - sqrt(mag) ;

        mag = 0.0 ;
        for ( ie = 0 ; ie <= ne ; ++ie ) mag += W[ie] * W[ie] ;

        /* Apply to all grid-cell columns */

        for ( is = isbeg ; is <= isend ; is += isinc ) {

          /* Top of column */

          _F_REAL_4 * const A = AI + jmap[0] + jmap[is] * ldg ;

          for ( iv = 0 ; iv < NEV ; ++iv ) {
            const int ivoff = iv * NEV ;
            double beta = 0.0 ;
            for ( ie = 0 ; ie <= ne ; ++ie ) {
              beta += W[ie] * A[ldgns * (ivoff + ie)] ;
            }
            beta *= 2.0 / mag ;
            for ( ie = 0 ; ie <= ne ; ++ie ) {
#ifdef DEBUGGING
              W2[ivoff+ie] = 
#endif
                A[ldgns*(ivoff+ie)] -= beta * W[ie] ;
            }
          }
        }

        /* Apply to vector */

        {
          _F_REAL_8 * const SV = V ;
          double beta = 0.0 ;
          for ( ie = 0 ; ie <= ne ; ++ie ) {
            beta += W[ie] * SV[ie*ldvec] ;
          }
          beta *= 2.0 / mag ;
          for ( ie = 0 ; ie <= ne ; ++ie ) {
            SV[ie*ldvec] -= beta * W[ie] ;
          }
        }
      }
    }
  }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* The four iteration parameters { TOL_BLK, MAX_ITER, RES_BLK, NUM_ITER }
   are one dimensional arrays of NEV+1 elements each.
   The first through NEV'th element of each array is associated with the
   component block.  The (NEV+1)'th element is associated with the overall
   block iteration.
*/

#define MAX_NEV 128

void _slblk(
  _F_INTEGER * IP_GJAC,  /* input:  IPARS id of Jacobian      */
  _F_INTEGER * IP_GRES,  /* input:  IPARS id of Residual      */
  _F_INTEGER * IP_GSOL,  /* in/out: IPARS id of 'GXDELTA'     */
  _F_REAL_4  * RES,      /* in/out: Convergence/actual residual ratio */
  _F_REAL_4  * ARES,     /* in/out: Absolute Convergence tolerance */
  _F_INTEGER * ITER,     /* in/out: Max/Actual iterations             */
  _F_INTEGER * INFO )    /* output: Status of overall solve           */
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

  _F_REAL_8 * const VSOL  = solve_data->sol ;
  _F_REAL_8 * const VRES  = solve_data->res ;
  _F_REAL_8 * const WORK  = solve_data->work ;
  _F_REAL_4 * const RNORM = solve_data->rnorm ;
  _F_REAL_4   RNORM_MIN = 0.0 ;

  _F_INTEGER fnunit = 0 ;
  _F_INTEGER imatvec[4] ;
  _F_INTEGER igdsum = 0 ;
  _F_INTEGER iprecon = 0 ;
  int i , j ;

  /*------------------------------------------------------------------*/


#ifdef DEBUGGING
  _slmsgunit( &fnunit );
  SLMSGLINE ;
  SLMSG("SLSOLVE BEGIN");
  if ( memcmp( solve_data_backup , solve_data , solve_data_size ) ) {
    SLDIE("SLSOLVE DATA Corrupted!");
  }
#endif

  /*------------------------------------------------------------------*/
  /* Obtain pointers to IPARS arrays */

  solve_data->idgjac = *IP_GJAC ;
  solve_data->idgres = *IP_GRES ;
  solve_data->idgsol = *IP_GSOL ;

  for ( i = 0 ; i < NBLK ; ++i ) {
    solve_data->blk[i].gjac = NULL ;
    solve_data->blk[i].gres = NULL ;
    solve_data->blk[i].gsol = NULL ;
  }

  {
    _F_INTEGER callworkdata[4] ;
#ifdef DEBUGGING
    _F_REAL_8 r1 , r2 ;

    AJACNORM = 0 ;
#endif
    callworkdata[0] = 3 ;
    callworkdata[1] = *IP_GJAC ;
    callworkdata[2] = *IP_GRES ;
    callworkdata[3] = *IP_GSOL ;
    mycallwork( slptrwork , callworkdata );

#ifdef DEBUGGING
    {
      char buf[128] ;
      r1 = AJACNORM ;

      _gr8sum( &igdsum, &IONE, &r1, &r2 );

      r2 = sqrt( r2 );
      sprintf(buf,"JAC-NORM = %.15g\n",r2);
      SLMSG(buf);
    }
#endif
  }

#ifdef DEBUGGING
  memcpy( solve_data_backup , solve_data , solve_data_size );
#endif

  /*------------------------------------------------------------------*/
  /* Copy residual to a more compact vector */

  slgrid2vec( 1 , VRES , ONE , IZERO , NEV );

  /*------------------------------------------------------------------*/

#ifdef DEBUGGING

  /*------------------------------------------------------------------*/
  /* DEBUGGING: Test grid-2-vector */

  slvec2grid( 0 , VRES , ONE , IZERO , NEV );
  slgrid2vec( 0 , WORK , ONE , IZERO , NEV );

  for ( i = 0 ; i < NALL ; ++i ) {
    if ( WORK[i] != VRES[i] ) {
      char buf[128] ;
      sprintf(buf,"grid2vec->vec2grid failed\n");
      SLDIE(buf);
    }
  }

#endif

  /*------------------------------------------------------------------*/
  /* DEBUGGING: replace RHS with JAC * RHS */

#ifdef DEBUGGING_SOL

  {
    char buf[128] ;
    _F_REAL_8 r1all, r2all ;
    int ir ;
    r1all = r8dot(&NALL,VRES,&IONE,VRES,&IONE);
    _gr8sum( &igdsum, &IONE, &r1all, &r2all );
    r1all = sqrt( r1all );
    r2all = sqrt( r2all );
    sprintf(buf,"    Input RHS : %.15g [local] %.15g [global]",
       r1all,r2all);
    SLMSG(buf);
  }

  *RES  = 1.0e-15 ;
  *ARES = 1.0e-15 ;
  *ITER = 500 ;

  imatvec[0] = 1 ;
  imatvec[1] = NEV ;
  imatvec[2] = 1 ;
  imatvec[3] = NEV ;

  r8copy( &NALL, VRES, &IONE, VSOL, &IONE );
  slmatvec( imatvec , &ONE, VSOL, &ZERO, VRES );

  {
    char buf[128] ;
    _F_REAL_8 r1all, r2all ;
    int ir ;
    r1all = r8dot(&NALL,VRES,&IONE,VRES,&IONE);
    _gr8sum( &igdsum, &IONE, &r1all, &r2all );
    r1all = sqrt( r1all );
    r2all = sqrt( r2all );
    sprintf(buf,"    JAC * RHS : %.15g [local] %.15g [global]",
       r1all,r2all);
    SLMSG(buf);
  }

  SLMSG("  Debugging: replaced RHS with JAC * RHS");

#endif

  /* END DEBUGGING */
  /*------------------------------------------------------------------*/
  /* Input residual norms */

  {
    char buf[128] ;
    _F_REAL_8 r1[MAX_NEV], r2[MAX_NEV], r1all, r2all ;
    int ir ;
    r1all = r2all = 0.0 ;
    for ( ir = 0 ; ir < NEV ; ir++ )
      r1all += r1[ir] = r8dot(&NLOC,VRES+ir*NLOC,&IONE,VRES+ir*NLOC,&IONE);
    _gr8sum( &igdsum, &NEV, r1, r2 );
#ifdef DEBUGGING
    sprintf(buf,"  Input residuals");
    SLMSG(buf);
#endif
    for ( ir = 0 ; ir < NEV ; ir++ ) {
      r2all += r2[ir] ;
      r1[ir] = sqrt( r1[ir] );
      RNORM[ir] = sqrt( r2[ir] );
#ifdef DEBUGGING
      sprintf(buf,"    BLK #%d : %.10g [local] %.10g [global]",
        ir+1,r1[ir],RNORM[ir]);
      SLMSG(buf);
#endif
    }
    r1all = sqrt( r1all );
    r2all = sqrt( r2all );
#ifdef DEBUGGING
    sprintf(buf,"    ALL    : %.10g [local] %.10g [global]",
       r1all,r2all);
    SLMSG(buf);
#endif
  }

  /*------------------------------------------------------------------*/
  /* Householder preconditioning of pressure block */

#ifdef DEBUGGING
  memcpy( solve_data_backup , solve_data , solve_data_size );
#endif

  sldecouple( VRES );

#ifdef DEBUGGING
  if ( memcmp( solve_data_backup , solve_data , solve_data_size ) ) {
    SLDIE("SLSOLVE DATA Corrupted!");
  }
#endif

#ifdef DEBUGGING
  {
    char buf[128] ;
    _F_REAL_8 r1all, r2all ;
    int ir ;
    r1all = r8dot(&NALL,VRES,&IONE,VRES,&IONE);
    _gr8sum( &igdsum, &IONE, &r1all, &r2all );
    r1all = sqrt( r1all );
    r2all = sqrt( r2all );
    sprintf(buf,"    ALL'dec: %.15g [local] %.15g [global]",
       r1all,r2all);
    SLMSG(buf);
  }
#endif

  /*------------------------------------------------------------------*/
  /* Form the pressure preconditioner */

  {
    char buf[128] ;
#ifdef DEBUGGING
    strcpy(buf,"  PRESSURE PRECONDITIONER: ");
#endif
    switch( solve_data->pspec[0] ) {
    case SL_PREC_DIAG:     strcat(buf,"DIAGONAL"); break ;
    case SL_PREC_LINEJAC:  strcat(buf,"LINE JAC");   slpresslinefac(); break;
    case SL_PREC_LINEGS:   strcat(buf,"LINE G-S");   slpresslinefac(); break;
    case SL_PREC_LINESOR:  strcat(buf,"LINE SOR");   slpresslinefac(); break;
    default: strcat(buf,"UNKNOWN - TERMINATING"); SLDIE(buf);
    }
#ifdef DEBUGGING
    SLMSG(buf);
#endif
  }

  /*------------------------------------------------------------------*/
  /* Ready to solve full matrix */

  imatvec[0] = 1 ;
  imatvec[1] = NEV ;
  imatvec[2] = 1 ;
  imatvec[3] = NEV ;

  /* Initial guess of zero */

  for ( j = 0 ; j < NALL ; ++j ) VSOL[j] = 0.0 ;

#ifdef DEBUGGIN
  if ( memcmp( solve_data_backup , solve_data , solve_data_size ) ) {
    SLDIE("SLSOLVE DATA Corrupted!");
  }
#endif

  RNORM_MIN = RNORM[0] ;
  for ( j = 1 ; j < NEV ; ++j )
    if ( RNORM[j] < RNORM_MIN ) RNORM_MIN = RNORM[j] ;
 
  if( *RES * RNORM_MIN < *ARES ) RNORM_MIN = *ARES / *RES;

  switch( solve_data->sspec[0] ) {
  case SL_SOLVE_GMRES:
    {
      const _F_INTEGER restart = solve_data->sspec[1] ;
      const _F_INTEGER NWORK   = restart + 3 ;
      const _F_INTEGER MH      = restart + 1 ;
      const _F_INTEGER NH      = restart + 6 ;
            _F_REAL_8  R8      = *RES * RNORM_MIN ;

      _slgmres(
        _gr8sum,      &igdsum,
        slprecmainsol,&iprecon,
        slmatvec,      imatvec,
        WORK , &NALL, &NWORK,                  /* WORK(NALL,NWORK) */
        WORK + NALL * NWORK, &MH, &NH,         /* H(MH,NH) */
        &NALL, VRES , VSOL , ITER, &R8, INFO, &fnunit );

      *RES = R8 ;
    }
    break ;
  }

#ifdef DEBUGGING
  if ( memcmp( solve_data_backup , solve_data , solve_data_size ) ) {
    SLDIE("SLSOLVE DATA Corrupted!");
  }
#endif

  /* Done iterating, converged or not.  Copy result to output grid. */

  slvec2grid( 0, VSOL, ONE, IZERO, NEV );

  /*------------------------------------------------------------------*/
  /* Output residual & solution norms */

  {
    char buf[256] ;
    _F_REAL_8 r1[MAX_NEV*2], r2[MAX_NEV*2], r1all[2], r2all[2] ;
    _F_INTEGER NEV2 = NEV * 2 ;
    int ir ;
    r1all[0] = r2all[0] = 0.0 ;
    r1all[1] = r2all[1] = 0.0 ;
    for ( ir = 0 ; ir < NEV ; ir++ ) {
      r1all[0] += r1[ir]
        = r8dot(&NLOC,WORK+ir*NLOC,&IONE,WORK+ir*NLOC,&IONE);
      r1all[1] += r1[ir+NEV]
        = r8dot(&NLOC,VSOL+ir*NLOC,&IONE,VSOL+ir*NLOC,&IONE);
    }
    _gr8sum( &igdsum, &NEV2, r1, r2 );
    for ( ir = 0 ; ir < NEV ; ir++ ) {
      r2all[0]  += r2[ir] ;
      r2all[1]  += r2[NEV+ir] ;
      r1[ir]     = sqrt( r1[ir] );
      r1[ir+NEV] = sqrt( r1[ir+NEV] );
      r2[ir]     = sqrt( r2[ir] );
      r2[ir+NEV] = sqrt( r2[ir+NEV] );
    }
    r1all[0] = sqrt( r1all[0] );
    r1all[1] = sqrt( r1all[1] );
    r2all[0] = sqrt( r2all[0] );
    r2all[1] = sqrt( r2all[1] );
#ifdef DEBUGGING
    sprintf(buf,"  Resulting residuals");
    SLMSG(buf);
    for ( ir = 0 ; ir < NEV ; ir++ ) {
      sprintf(buf,
        "    BLK #%d : %.10g [local] %.10g [global]",
        ir+1,r1[ir],r2[ir]);
      SLMSG(buf);
    }
    sprintf(buf,"    ALL    : %.10g [local] %.10g [global]", r1all[0],r2all[0]);
    SLMSG(buf);
    sprintf(buf,"  Resulting solutions");
    SLMSG(buf);
    for ( ir = 0 ; ir < NEV ; ir++ ) {
      sprintf(buf,
       "    BLK #%d : %.10g [local] %.10g [global]",
        ir+1,r1[ir+NEV],r2[ir+NEV]);
      SLMSG(buf);
    }
    sprintf(buf,"    ALL    : %.10g [local] %.10g [global]", r1all[1],r2all[1]);
    SLMSG(buf);
#endif
  }

  /*------------------------------------------------------------------*/

#ifdef DEBUGGING_SOL

  /* Compare it to the cooked solution, the original residual */

  slgrid2vec( 1 , VRES , ONE , IZERO , NEV );

  {
    char buf[128];
    const _F_INTEGER ITWO = 2 ;
    _F_REAL_8 tmp[2] , tmp2[2] ;
    tmp[0] = tmp[1] = tmp2[0] = tmp2[1] = 0.0 ;
    for ( j = 0 ; j < NALL ; ++j ) {
      const double mag = VRES[j] ;
      const double err = VSOL[j] - VRES[j] ;
      tmp[0] += mag * mag ;
      tmp[1] += err * err ;
    }
    _gr8sum( NULL, &ITWO, tmp, tmp2 );
    tmp[0] = sqrt( tmp2[0] );
    tmp[1] = sqrt( tmp2[1] );
    sprintf(buf,"Debugging solution mag = %.15g, error = %.15g\n",
      tmp[0],tmp[1]);
    SLMSG(buf);
  }

  SLDIE("SLBLK - end debugging solution");
#endif

#ifdef DEBUGGING
  if ( memcmp( solve_data_backup , solve_data , solve_data_size ) ) {
    SLDIE("SLSOLVE DATA Corrupted!");
  }

  SLMSG("SLSOLVE END");
  SLMSGLINE ;
#endif
  return ;
}

