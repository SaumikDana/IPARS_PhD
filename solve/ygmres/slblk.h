c---->------------------------------------------------------------------<
c  Requirements:
c
c   The leading variable must be the representative pressure.
c   All remaining variables should correspond to the selected
c   component concentrations or saturations.
c
c---->------------------------------------------------------------------<
c  Explaination:
c
c  Linear iterative solver for IPARS using cell-centered QR
c  left preconditioning and two-level right preconditioning.
c--
c  The decoupling/scaling left preconditioner applies Householder
c  reflections to convert the cell-centered block matrices to
c  upper triangular (QR-factorization).  Note that the Householder
c  transformations preserve the L2 norm of the linear system.
c  This preconditioner takes a linear combination of transport
c  equations to form a single composite, pressure-dominant,
c  transport equation.
c--
c  The upper and lower levels of the two-level right preconditioner are:
c    upper: line SOR - block Gauss-Seidel (by default, or others, by defining
c           GMRES_PREC in the input file) with pressure/component blocking and
c    lower: an application specified preconditioner for the 
c           composite pressure equation.
c
c  e.g. for upper level right preconditioner is:
c
c        | App  Apn |       | Mpp   0  |
c    A = |          |   M = |          |
c        | Anp  Ann |       | Anp  Mnn |
c
c  where  Mpp  is the application specified pressure preconditioner
c  and    Mnn  is a cell-centerd block Jacobi preconditioner for
c  the component equations.
c
c  Note that the diagonal blocks of the component (Mnn) preconditioner 
c  have been transformed to upper diagonal blocks by the QR left
c  preconditioning; as such application of their inverse is trivial.
c---->------------------------------------------------------------------<
c---->------------------------------------------------------------------<
      EXTERNAL SLIBLK
      EXTERNAL SLBLK
c---->
      INTEGER SL_SOL_GMRES
      INTEGER SL_SOL_FGMRES
c
      PARAMETER ( SL_SOL_GMRES  = 1 )
      PARAMETER ( SL_SOL_FGMRES = 2 )
c---->
      INTEGER SL_PRE_DIAG
      INTEGER SL_PRE_LINEGS
      INTEGER SL_PRE_LINESOR
      INTEGER SL_PRE_NEUPCG    
      INTEGER SL_PRE_NEUPBCGS    
      INTEGER SL_PRE_NEUGMRES    
      INTEGER SL_PRE_NEUSER    
      INTEGER SL_PRE_SEPARABLE
      INTEGER SL_PRE_SEPAR_GS  
      INTEGER SL_PRE_SEPAR_UMS 
      INTEGER SL_PRE_TMG  
      INTEGER SL_PRE_TMG2  
      INTEGER SL_PRE_AMG    
      INTEGER SL_PRE_AMG2    
      INTEGER SL_PRE_FCTRZ    
c
      PARAMETER ( SL_PRE_DIAG     = 1 )
      PARAMETER ( SL_PRE_LINEGS   = 3 )
      PARAMETER ( SL_PRE_LINESOR  = 4 )
      PARAMETER ( SL_PRE_NEUPCG   = 5 )
      PARAMETER ( SL_PRE_NEUPBCGS = 6 )
      PARAMETER ( SL_PRE_NEUGMRES = 7 )
      PARAMETER ( SL_PRE_NEUSER   = 8 )
      PARAMETER ( SL_PRE_SEPARABLE= 10)
      PARAMETER ( SL_PRE_SEPAR_GS = 11)
      PARAMETER ( SL_PRE_SEPAR_UMS= 12)
      PARAMETER ( SL_PRE_TMG    = 13)
      PARAMETER ( SL_PRE_TMG2   = 14)
      PARAMETER ( SL_PRE_AMG      = 15)
      PARAMETER ( SL_PRE_AMG2     = 16)
      PARAMETER ( SL_PRE_FCTRZ    = 17)
c---->------------------------------------------------------------------<
c  Initialize the solver: storage allocation and grid interogation.
c---->
c     SUBROUTINE SLIBLK( NEV, NS, JSMAP, KTMP, NBLK, SPEC )
c
c     INTEGER NEV, NS
c     INTEGER JSMAP(3,NS)
c     INTEGER KTMP
c     INTEGER NBLK
c     INTEGER SPEC(4)
c---->
c  NEV   : input : Number of equations/variables
c  NS    : input : Jacobian's stencil size
c  JSMAP : input : Jacobian's stencil map
c  KTMP  : input : IPARS type for grid-element arrays
c  NBLK  : input : Total number of fault blocks
c  SPEC  : input : Solver & pressure preconditioner specifications
c---->
c  if ( SPEC(1) .eq. SL_SOL_GMRES ) then
c    SPEC(2) = {restart for the GMRES solver}
c  end if
c  if ( SPEC(1) .eq. SL_SOL_FGMRES ) then
c    SPEC(2) = {restart for the Flexible GMRES solver}
c  end if
c---->
c  if ( SPEC(3) .eq. SL_PRE_DIAG ) then   {Jacobi preconditioning}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_LINEGS ) then {Line Gauss-Seidel}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_LINESOR ) then {Line SOR}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_NEUPCG ) then {PCG + Truncated Neumann ser.}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_NEUPBCGS ) then {PBCGStab + Truncated Neumann }
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_GMRES ) then {GMRES + Truncated Neumann }
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_NEUSER ) then { Truncated Neumann series}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_SEPARABLE ) then {SEPARABLE}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_SEPAR_GS  ) then {GS as a smoother,
c                                               SEPARABLE for low frequencies}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_SEPAR_UMS ) then {GS as a smoother,
c                                  SEPARABLE based on ums for low frequencies}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_TMG ) then {TMG}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_TMG2 ) then {TMG AND LSOR}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_AMG   ) then {ALGEBRAIC MULTIGRID}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_AMG2  ) then {ALGEBRAIC MULTIGRID AND LSOR}
c    SPEC(4) = {unused}
c
c  else if ( SPEC(3) .eq. SL_PRE_FCTRZ ) then {SPARSE FACTORIZER}
c    SPEC(4) = {unused}
c
c  end if
c---->------------------------------------------------------------------<
c---->------------------------------------------------------------------<
c Given the IPARS Jacobian and residual grid-element-arrays
c solve for the solution update grid-element-array.
c---->
c     SUBROUTINE SLBLK( IP_GJAC, IP_GRES, IP_GSOL, RES, ITER, INFO )
c
c     INTEGER IP_GJAC, IP_GRES, IP_GSOL, INFO, ITER
c     REAL*4    RES
c---->
c  IP_GJAC  : input  : IPARS grid-element array ID of the Jacobian
c  IP_GRES  : input  : IPARS grid-element array ID of the Residual
c  IP_GSOL  : input  : IPARS grid-element array ID of the Solution
c  RES      : in/out : Residual ratio: convergence / actual
c  ITER     : in/out : Iterations: maximum/actual
c  INFO     : output : = 0  ->  Converged
c                    : > 0  ->  Did not converge
c                    : < 0  ->  An error occured
c---->------------------------------------------------------------------<
c---->------------------------------------------------------------------<

