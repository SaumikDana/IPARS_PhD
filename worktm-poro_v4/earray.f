C  EARRAY.F - CREATE ELASTIC MODEL GRID-ELEMENT ARRAYS

C  ROUTINES IN THIS MODULE:

C    TAMEEM ALMANI   07/27/2016 INCLUDE NECESSARY CHANGES FOR
C                               COUPLING WITH MECHANICS

C  SUBROUTINE EARRAY (KERR)
C*********************************************************************
      SUBROUTINE EARRAY (KERR)
C*********************************************************************
C  Create poroelastic model grid-element arrays
C**********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'earydat.h'
      INCLUDE 'emodel.h'
      INCLUDE 'blkary.h'
      INCLUDE 'visual.h'
      INCLUDE 'mpfaary.h'

      INTEGER IDUM(2)
      EXTERNAL PRINTWORK,PRINTKEYOUT

      INTEGER  KERR
      KERR = 0

C ALLOCATE ARRAYS IN FLOW MODEL

      IF (PEFLOW.EQ.17.OR.MBPOROE) THEN
              ! SAUMIK,BGANIS
         MODACT = 17
         CALL TARRAY(KERR)
         IF (KERR.GT.0) GO TO 99
         CALL MFARRAY(KERR)
         IF (KERR.GT.0) GO TO 99
      ENDIF

C       IF (PEFLOW.EQ.16.OR.MBPOROE) THEN
              ! SAUMIK,BGANIS
C          MODACT=16
C          CALL XARRAY(KERR)
C          IF (KERR.GT.0) GO TO 99
C          CALL MFARRAY(KERR)
C          IF (KERR.GT.0) GO TO 99
C       ENDIF

ctm
C      IF (PEFLOW.EQ.18.OR.MBPOROE) THEN
              ! SAUMIK,BGANIS
C         MODACT = 18
C         CALL HARRAY(KERR)
C         IF (KERR.GT.0) GO TO 99
C         CALL MFARRAY(KERR)
C         IF (KERR.GT.0) GO TO 99
C      ENDIF
ctm

      MODACT = 15

      CALL ALCGEA ('KEYOUT_CR ',4,0,N_KEYOUT_CR,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('MODULUS ',2,0,N_MODUL,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('POISSON ',2,0,N_POISS,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('BIOTA ',2,0,N_BIOTA,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('BIOTM ',2,0,N_BIOTM,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('ROCKD ',2,0,N_ROCKD,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('BULK_DEN ',2,0,N_BULK_DEN,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('DISP_COMP ',2,0,N_DISP_COMP,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('EPV ',2,0,N_EPV,KERR)
      IF (KERR.GT.0) GO TO 99
      IF(.NOT.MBPOROE) THEN ! SAUMIK
      CALL ALCGEA ('VSTRAIN ',2,0,N_VSTRAIN,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('VSTRAIN_INIT ',2,1,N_VSTRAIN_INIT,KERR)
      IF (KERR.GT.0) GO TO 99
      ENDIF
ctm TAMEEM
      CALL ALCGEA ('VSTRAIN_NM1 ',2,1,N_VSTRAIN_NM1,KERR)
      IF (KERR.GT.0) GO TO 99
ctm TAMEEM
      CALL ALCGEA ('STRXX_INIT ',2,1,N_STRESS_INIT(1),KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRYY_INIT ',2,1,N_STRESS_INIT(2),KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRZZ_INIT ',2,1,N_STRESS_INIT(3),KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRXY_INIT ',2,1,N_STRESS_INIT(4),KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRYZ_INIT ',2,1,N_STRESS_INIT(5),KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRXZ_INIT ',2,1,N_STRESS_INIT(6),KERR)
      IF (KERR.GT.0) GO TO 99

! change in displacement u-u0
      CALL ALCGEA ('DISP ',2,3,N_EDISP,KERR)
      IF (KERR.GT.0) GO TO 99

! true displacement u
      CALL ALCGEA ('TDISP ',2,3,N_TDISP,KERR)
      IF (KERR.GT.0) GO TO 99

! previous displacement
      CALL ALCGEA ('DISPN ',2,3,N_EDISPN,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('STRAIN ',2,6,N_STRAIN,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRAINN ',2,6,N_STRAINN,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRESS ',2,6,N_STRESS,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCGEA ('STRESSN ',2,6,N_STRESSN,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('POROHEX_GELEI ',4,1,N_POROHEX_GELEI,KERR)
      IF (KERR.GT.0) GO TO 99

!      CALL ALCGEA ('GELEM_I ',4,1,N_GELEM_I,KERR)
!      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('UPDATER8 ',2,24,N_UPDATE_R8,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('UPDATEI4 ',4,24,N_UPDATE_I4,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('UPDATEFG ',5,8,N_UPDATE_FG,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('KEYCRELE ',4,8,N_KEYCR_ELE,KERR)
      IF (KERR.GT.0) GO TO 99

c     interface arrays coupling with Ruijies code

      CALL ALCGEA ('STR_RESID ',2,3,N_STR_RESID,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('PROCN ',2,1,N_PROCN,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('PORO_NEIGHBOR ',4,6,N_PORO_NEIGHBOR,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA('N_PREF ',2,1,N_PREF,KERR)
      IF (KERR.GT.0) GO TO 99

C PARALLELIZATION OF POROHEX MODEL WITH NON-GROWING FRACTURE

      CALL ALCGEA ('ELEM_LID ',4,1,N_ELEM_LID,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('NODE_LID ',4,1,N_NODE_LID,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('FNODE_TYPE ',4,1,N_FNODE_TYPE,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('OFNODE_LID ',4,1,N_OFNODE_LID,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('OFNODE_GID ',4,1,N_OFNODE_GID,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('FWIDTH ',2,1,N_NODE_WIDTH,KERR)
      IF (KERR.GT.0) GO TO 99

! Ruijie - allocate plastic model parameters
!        - Yield Parameters
!        - Solid Flow Parameters
!        - Solid Hardening Parameters

      IF (MODEL_EP.EQ.1) THEN

        CALL ALCGEA('EP_ASSOCIATED ',4,1,N_PASSO,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('EP_YIELD_STRENGTH ',2,1,N_YIELD_SIG0,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('EP_YIELD_SLOPE ',2,1,N_YIELD_ALPHA,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('EP_FLOW_SLOPE ',2,1,N_FLOW_ALPHA,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('EP_HARDEN_MODEL ',4,1,N_HARDEN_MODEL,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('EP_HARDEN_C1 ',2,1,N_HARDEN_C1,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('EP_HARDEN_C2 ',2,1,N_HARDEN_C2,KERR)
        IF (KERR.GT.0) GO TO 99

!        - allocate plastic strain and state variables
!         - at current and previous time steps

        CALL ALCGEA('PSTRAIN ',2,6,N_PSTRAIN,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('PSTRAINN ',2,6,N_PSTRAINN,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('PSTATE ',2,3,N_PSTATE,KERR)
        IF (KERR.GT.0) GO TO 99
        CALL ALCGEA('PSTATEVN ',2,3,N_PSTATEN,KERR)
        IF (KERR.GT.0) GO TO 99

      ENDIF

C SAUMIK
      IF(SDPM) THEN
         CALL ALCGEA('XPERM_REF ',1,0,N_XPERM_REF,KERR)
         IF(KERR.GT.0) GO TO 99

         CALL ALCGEA('YPERM_REF ',1,0,N_YPERM_REF,KERR)
         IF(KERR.GT.0) GO TO 99

         CALL ALCGEA('ZPERM_REF ',1,0,N_ZPERM_REF,KERR)
         IF(KERR.GT.0) GO TO 99

         CALL ALCGEA('XPERM_R8 ',2,0,N_XPERM_R8,KERR)
         IF(KERR.GT.0) GO TO 99

         CALL ALCGEA('YPERM_R8 ',2,0,N_YPERM_R8,KERR)
         IF(KERR.GT.0) GO TO 99

         CALL ALCGEA('ZPERM_R8 ',2,0,N_ZPERM_R8,KERR)
         IF(KERR.GT.0) GO TO 99

         CALL ALCGEA('EMSTRESS ',2,0,N_EMSTRESS,KERR)
         IF(KERR.GT.0) GO TO 99

         CALL ALCGEA('EMSTRESS_REF ',2,0,N_EMSTRESS_REF,KERR)
         IF(KERR.GT.0) GO TO 99
      ENDIF
C SAUMIK

      IF(MBPOROE) THEN ! SAUMIK,BGANIS
      CALL ALCGEA('EFLDEN ',2,0,N_EFLDEN,KERR)
      IF(KERR.GT.0) GO TO 99

        CALL ALCGEA('EPV_FLOW ',2,0,N_EPV_FLOW,KERR)
        IF(KERR.GT.0) GO TO 99

        CALL ALCGEA('PRESS ',2,0,N_PRESS,KERR)
        IF(KERR.GT.0) GO TO 99

        CALL ALCGEA('ECR ',2,0,N_ECR,KERR)
        IF(KERR.GT.0) GO TO 99
      ENDIF

      CALL ALCGEA('PRESSVAL ',2,0,N_PRESSVAL,KERR)
      IF(KERR.GT.0) GO TO 99 ! MANDEL

      RETURN

   99 WRITE(0,*) 'Errors in EARRAY'
      STOP 1

      END


C*********************************************************************
      SUBROUTINE EFRAC_ARRAY(KERR)
C*********************************************************************
C  Create fracture type arrays
C**********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'earydat.h'
      INCLUDE 'emodel.h'
      INCLUDE 'blkary.h'
      INCLUDE 'hypre.h'

      INTEGER  KERR,AN

      IF (KERR.GT.0) THEN
         WRITE(0,*) 'ERROR: ENTERING EFRAC_ARRAY,KERR=',KERR
      ENDIF
      KERR = 0

      AN=MAX(POROHEX_LFALLSIZE,1)
      CALL ALCFEA ('OFNODE_L2GID ',4,AN,N_OFNODE_L2GID,KERR)
      IF (KERR.GT.0) GO TO 99
      CALL ALCFEA ('OFNODE_KEYOUT ',4,AN,N_OFNODE_KEYOUT,KERR)
      IF (KERR.GT.0) GO TO 99

      AN=MAX(9*POROHEX_LFALLSIZE,1)
      CALL ALCFEA ('OFNODE_AFFINE ',4,AN,N_OFNODE_AFFINE,KERR)
      IF (KERR.GT.0) GO TO 99

      AN=MAX(3*POROHEX_GFSIZE,1)
      CALL ALCFEA ('OFNODE_DISP ',2,AN,N_OFNODE_DISP,KERR)
      IF (KERR.GT.0) GO TO 99

      AN=MAX(3*TOTAL_CRACKED_FACE,1)
      CALL ALCFEA ('CRAC_IBC ',2,AN,N_CRAC_IBC,KERR)
      IF (KERR.GT.0) GO TO 99

      AN=MAX(POROHEX_GFSIZE,1)
      CALL ALCFEA ('OFNODE_GNUM ',4,AN,N_OFNODE_GNUM,KERR)
      IF (KERR.GT.0) GO TO 99

      GOTO 100

   99 WRITE(0,*) 'ERROR IN EFRAC_ARRAY,KERR=',KERR
      STOP 1

  100 RETURN
      END
