C  ESTDOUT.F - PRINT ELASTIC MODEL STANDARD OUTPUT

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE ESTDOUT()

C*********************************************************************
      SUBROUTINE ESTDOUTS ()
C*********************************************************************

C  Prints poroelastic model standard output - scalars

C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'control.h'

      END
C*********************************************************************
      SUBROUTINE ESTDOUTA ()
C*********************************************************************
C  Poroelastic model standard output - grid element arrays
C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'
      INCLUDE 'earydat.h'
      INCLUDE 'emodel.h'

      INTEGER JDISP(3),JSTRN(3),JSTRS(13),JASTRS(10),NDIM_ELASTIC
      EXTERNAL EAVERAGE_DISP,EAVERAGE_STRESS,EAVERAGE_STRAIN,ESTRESS_3D

      INTEGER JMANDEL(11)
      EXTERNAL MANDEL_PRINT,MANDEL_ERR

      LOGICAL ONCEONLY
      REAL*8 CURRENT_TIME,M2INCH,DELTIME
      DATA ONCEONLY /.TRUE./
      INTEGER :: NERROUT = 70
      PARAMETER (M2INCH=39.37007874016D0)
      ! METER TO INCH

! Special Output for Mandel Problem
      IF (MECH_BC_NCASE.EQ.100) THEN
         JMANDEL(1) = 9
         JMANDEL(2) = N_KEYOUT_CR
         JMANDEL(3) = N_EDISP
         JMANDEL(4) = N_PRESS
         JMANDEL(5) = N_XC
         JMANDEL(6) = N_YC
         JMANDEL(7) = N_ZC
         JMANDEL(8) = N_NODE_LID
         JMANDEL(9) = N_STRESS
         JMANDEL(10) = N_STRAIN
         CALL CALLWORK(MANDEL_PRINT,JMANDEL)

! saumik - error norm
         IF (ONCEONLY) THEN
            FINERR = 0.D0
            OPEN(NERROUT,FILE='ERR_FS.DAT',STATUS='unknown')
            ONCEONLY = .FALSE.
         ELSE
            OPEN(NERROUT,FILE='ERR_FS.DAT',STATUS='old',ACCESS='append')
         ENDIF
         CURRENT_TIME = TIM*24.D0*3600.D0
         JMANDEL(1) = 10
         JMANDEL(2) = N_PRESSVAL ! ANALYTICAL:PASCAL
         JMANDEL(3) = N_PRESS   ! NUMERICAL:PSI
         JMANDEL(4) = N_R8U
         JMANDEL(5) = N_EPV
         JMANDEL(6) = N_BIOTA
         JMANDEL(7) = N_EVOL
         JMANDEL(8) = N_MODUL
         JMANDEL(9) = N_POISS
         JMANDEL(10) = N_VSTRAIN
         JMANDEL(11) = N_POR
         CALL CALLWORK(MANDEL_ERR,JMANDEL)
! saumik - L^{\infty}
         FINERR=MAX(FINERR,R8UTIL)
         WRITE(NERROUT,'(0P,F9.3,E15.8)')CURRENT_TIME,DSQRT(FINERR)
! saumik - L_2
!         DELTIME=DELTIM*24.D0*3600.D0
!         FINERR=FINERR+R8UTIL
!         WRITE(NERROUT,'(0P,F9.2,E15.8)')CURRENT_TIME,
!     &        DSQRT(DELTIME*FINERR)
         CLOSE(NERROUT)
      ENDIF


      NDIM_ELASTIC = 3
         JDISP(1) = 2
         JDISP(2) = N_EDISP
         JDISP(3) = N_STRESS

         JSTRN(1) = 2
         JSTRN(2) = N_EDISP
         JSTRN(3) = N_STRESS

cbw
         JASTRS(1) = 7
         JASTRS(2) = N_MODUL
         JASTRS(3) = N_POISS
         JASTRS(4) = N_BIOTA
         JASTRS(5) = N_STRESS_INIT(1)
         JASTRS(6) = N_STRESS_INIT(2)
         JASTRS(7) = N_STRESS_INIT(3)
         JASTRS(8) = N_STRESS
cbw         JASTRS(9) = N_PREF
cbw         JASTRS(10) = N_PRESS
cbw
         JSTRS(1) = 10
         JSTRS(2) = N_KEYOUT_CR
         JSTRS(3) = N_MODUL
         JSTRS(4) = N_POISS
         JSTRS(5) = N_BIOTA
         JSTRS(6) = N_STRESS_INIT(1)
         JSTRS(7) = N_STRESS_INIT(2)
         JSTRS(8) = N_STRESS_INIT(3)
         JSTRS(9) = N_EDISP
cbw         JSTRS(10 )= N_PREF
cbw         JSTRS(11) = N_PRESS
         JSTRS(10) = N_STRESS
         JSTRS(11) = N_I4U

C  PRINT DISPLACEMENT AT THE GRID NODES

      IF(DISPOUT.GT.0) THEN
!bw         CALL CALLWORK(EAVERAGE_DISP,JDISP)
         IF(NDIM_ELASTIC.EQ.3) THEN
            TITU = 'DISPLACEMENTS IN X DIRECTION FOR FAULT BLOCK'
            CALL GEAOUT(N_TDISP,1,2)
!!bw            CALL GEAOUT(N_STRESS,1,1)
!            TITU = 'DISPLACEMENTS IN Y DIRECTION FOR FAULT BLOCK'
!            CALL GEAOUT(N_EDISP,2,2)
!!bw            CALL GEAOUT(N_STRESS,2,1)
!            TITU = 'DISPLACEMENTS IN Z DIRECTION FOR FAULT BLOCK'
!            CALL GEAOUT(N_EDISP,3,2)
!!bw            CALL GEAOUT(N_STRESS,3,1)
         ELSE
            IF(LEVELC) WRITE(NFOUT,10)
            NERRC = NERRC + 1
            RETURN
         ENDIF
      ENDIF

C  PRINT STRAIN AT THE GRID NODES

      IF(STRNOUT) THEN
!bw         CALL CALLWORK(EAVERAGE_STRAIN,JSTRN)
         IF(NDIM_ELASTIC.EQ.3) THEN
            TITU = 'VOLUMETRIC STRAIN FOR FAULT BLOCK'
            CALL GEAOUT(N_VSTRAIN,1,1)
            TITU = 'STRAIN_XX IN FAULT BLOCK'
            CALL GEAOUT(N_STRAIN,1,2)
            TITU = 'STRAIN_YY IN FAULT BLOCK'
            CALL GEAOUT(N_STRAIN,2,2)
            TITU = 'STRAIN_ZZ IN FAULT BLOCK'
            CALL GEAOUT(N_STRAIN,3,2)
            TITU = 'STRAIN_XY IN FAULT BLOCK'
            CALL GEAOUT(N_STRAIN,4,2)
            TITU = 'STRAIN_XZ IN FAULT BLOCK'
            CALL GEAOUT(N_STRAIN,5,2)
            TITU = 'STRAIN_YZ IN FAULT BLOCK'
            CALL GEAOUT(N_STRAIN,6,2)
         ELSE
            IF(LEVELC) WRITE(NFOUT,10)
            NERRC = NERRC + 1
            RETURN
         ENDIF
      ENDIF

C  PRINT PLASIC STRAIN AT THE GRID NODES
      IF(PSTRNOUT) THEN
         IF(NDIM_ELASTIC.EQ.3) THEN
            TITU = 'PLASTIC STRAIN_XX'
            CALL GEAOUT(N_PSTRAIN,1,2)
            TITU = 'PLASTIC STRAIN_YY'
            CALL GEAOUT(N_PSTRAIN,2,2)
            TITU = 'PLASTIC STRAIN_ZZ'
            CALL GEAOUT(N_PSTRAIN,3,2)
            TITU = 'PLASTIC STRAIN_XY'
            CALL GEAOUT(N_PSTRAIN,4,2)
            TITU = 'PLASTIC STRAIN_YZ'
            CALL GEAOUT(N_PSTRAIN,5,2)
            TITU = 'PLASTIC STRAIN_ZX'
            CALL GEAOUT(N_PSTRAIN,6,2)
         ELSE
            IF(LEVELC) WRITE(NFOUT,10)
            NERRC = NERRC + 1
            RETURN
         ENDIF
      ENDIF

C  PRINT PLASTIC RELATED STATE VARIABLES AT THE GRID NODES
      IF(PSTATEOUT) THEN
         IF(NDIM_ELASTIC.EQ.3) THEN
            TITU = 'VOLUME PLASTIC STRAIN'
            CALL GEAOUT(N_PSTATE,1,2)
            TITU = 'EQUIVALENT PLASTIC STRAIN: EQPL'
            CALL GEAOUT(N_PSTATE,2,2)
            TITU = 'YIELD STRESS'
            CALL GEAOUT(N_PSTATE,3,2)
         ELSE
            IF(LEVELC) WRITE(NFOUT,10)
            NERRC = NERRC + 1
            RETURN
         ENDIF
      ENDIF

C PRINT STRESS AT THE GRID NODES

      IF(STRSOUT) THEN

!bw         CALL CALLWORK(EAVERAGE_STRESS,JASTRS)

         IF(NDIM_ELASTIC.EQ.3) THEN
            TITU = 'STRESS_XX IN FAULT BLOCK'
            CALL GEAOUT(N_STRESS,1,2)
!bw            CALL GEAOUT(N_STRESS,1,1)
            TITU = 'STRESS_YY IN FAULT BLOCK'
            CALL GEAOUT(N_STRESS,2,2)
!bw            CALL GEAOUT(N_STRESS,2,1)
            TITU = 'STRESS_ZZ IN FAULT BLOCK'
            CALL GEAOUT(N_STRESS,3,2)
!bw            CALL GEAOUT(N_STRESS,3,1)
            TITU = 'STRESS_XY IN FAULT BLOCK'
            CALL GEAOUT(N_STRESS,4,2)
!bw            CALL GEAOUT(N_STRESS,4,1)
            TITU = 'STRESS_XZ IN FAULT BLOCK'
            CALL GEAOUT(N_STRESS,5,2)
!bw            CALL GEAOUT(N_STRESS,5,1)
            TITU = 'STRESS_YZ IN FAULT BLOCK'
            CALL GEAOUT(N_STRESS,6,2)
!bw            CALL GEAOUT(N_STRESS,6,1)

         ELSE
            IF(LEVELC) WRITE(NFOUT,10)
            NERRC = NERRC + 1
            RETURN
         ENDIF
!bw         I4UTIL = 8
!bw         CALL CALLWORK(ESTRESS_3D,JSTRS)
      ENDIF
  10  FORMAT(/'ERROR: NOT SETUP FOR POROELASTIC 1D OR 2D SYSTEM')

      END
C*********************************************************************
C                   END OF ESTDOUT.DF
C*********************************************************************
