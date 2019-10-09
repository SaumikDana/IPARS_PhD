! HYPRE.DH - Header file for HYPRE data

      REAL*8 LSOL_TOL,STRONG_THRES,TRUNC_FACTOR

      integer*8  mpi_comm
      integer*8  parcsr_A
      integer*8  A
      integer*8  b
      integer*8  x
      integer*8  par_b
      integer*8  par_x
      integer*8  solver

      INTEGER LSOL_ITMAX,HYPRE_SOL_ID,RELAX_TYPE,CYCLE_TYPE,                   &
     &   COARSEN_TYPE,MEASURE_TYPE,SETUP_TYPE,PRECOND_ID,K_DIM,                &
     &   NUM_SWEEPS,MAX_LEVELS,PRINT_SOL,PRINT_LEVEL,                          &
     &   LSIZE,ILOW,IHIGH
      INTEGER PSIZE(0:1024)

      INTEGER N_HYPRE_ROWS,N_HYPRE_WORK,N_GELEI

      INTEGER POROHEX_LSIZE,POROHEX_ILOWER,POROHEX_IUPPER,                     &
     &   POROHEX_LFSIZE,POROHEX_IFLOWER,POROHEX_IFUPPER,                       &
     &   POROHEX_LALLSIZE,POROHEX_LFALLSIZE,POROHEX_LALLELEM,                  &
     &   POROHEX_GSIZE,POROHEX_GFSIZE,                                         &
     &   COUPLED_ITER,POROHEXLINT

      LOGICAL HYPRE_EVFEM,DBGEV

      COMMON /HYPRE/ LSOL_TOL,STRONG_THRES,TRUNC_FACTOR,                       &
     &   mpi_comm,parcsr_A,A,b,x,par_b,par_x,solver,                           &
     &   LSOL_ITMAX,                                                           &
     &   HYPRE_SOL_ID,RELAX_TYPE,CYCLE_TYPE,COARSEN_TYPE,                      &
     &   MEASURE_TYPE,SETUP_TYPE,PRECOND_ID,K_DIM,                             &
     &   NUM_SWEEPS,MAX_LEVELS,PRINT_SOL,PRINT_LEVEL,                          &
     &   LSIZE,PSIZE,ILOW,IHIGH,                                               &
     &   N_HYPRE_ROWS,N_HYPRE_WORK,N_GELEI,                                    &
     &   POROHEX_LSIZE,POROHEX_ILOWER,POROHEX_IUPPER,                          &
     &   POROHEX_LFSIZE,POROHEX_IFLOWER,POROHEX_IFUPPER,                       &
     &   POROHEX_LALLSIZE,POROHEX_LFALLSIZE,POROHEX_LALLELEM,                  &
     &   POROHEX_GSIZE,POROHEX_GFSIZE,                                         &
     &   COUPLED_ITER,POROHEXLINT,HYPRE_EVFEM,DBGEV

C     the following is from HYPRE.c
      integer HYPRE_PARCSR
      parameter  (HYPRE_PARCSR=5555)

