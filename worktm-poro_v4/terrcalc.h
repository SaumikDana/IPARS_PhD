!----------------------------------------------------------------------
! terrcalc.dh - Common block for manufactured solution, source function,
!               a priori error computation.
!
! Ben Ganis, Yerlan Amanbek  06/12/2016
!
!----------------------------------------------------------------------

      INTEGER ITEST
      REAL*8  PERR_SUB(10),PERR_LinftyL2,PERR_L2L2,
     &        VERR_SUB(10),VERR_LinftyL2,VERR_L2L2
      COMMON /TERRCOM/PERR_SUB,PERR_LinftyL2,PERR_L2L2,
     &                VERR_SUB,VERR_LinftyL2,VERR_L2L2,
     &                ITEST

!  ITEST  = TEST NUMBER FOR MANUFACTURED SOLUTION, SOURCE FUNCTION

