C  TARRAY.F - CREATE IMPLICIT SINGLE PHASE FLOW MODEL GRID-ELEMENT ARRAYS

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE TARRAY (KERR)

C  CODE HISTORY:

C  JOHN WHEELER     4/29/97  ALPHA CODE
C  BAHAREH MOMKEN   2/15/99  Hydrology-IMPES garray.df is used as
C                            template
C  JOHN WHEELER    04/03/99  IMPLICIT SINGLE PHASE MODEL
C*********************************************************************
      SUBROUTINE TARRAY (KERR)
C*********************************************************************

C  Creates single phase flow  model grid-element arrays

C  KERR = ERROR NUMBER (OUTPUT, INTEGER)

C  NOTE: See tarydat.h for array descriptions

C*********************************************************************
      IMPLICIT NONE
C        INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'
      INCLUDE 'tarydat.h'
        INCLUDE 'earydat.h'
      INCLUDE 'terrcalc.h'

      INTEGER KERR

cgp dbg
      LOGICAL DBG
      DATA DBG /.FALSE./

C  ALLOCATE GRID-ELEMENT ARRAY SPACE

      KERR=0

C --- SAUMIK,BGANIS
      IF(MBPOROE) THEN
      ! IF MBPOROE IS TRUE, ARRAYS N_MODULFLOW, N_POISSFLOW AND
      ! N_BIOTAFLOW ARE USED TO DETERMINE EFFECTIVE ROCK
      ! COMPRESSIBILITIES IN SINGLE PHASE FLOW BLOCK AT INITIAL TIME
         CALL ALCGEA ('MODULUSFLOW ',2,0,N_MODULFLOW,KERR)
         IF (KERR.GT.0) GO TO 99
         CALL ALCGEA ('POISSONFLOW ',2,0,N_POISSFLOW,KERR)
         IF (KERR.GT.0) GO TO 99
         CALL ALCGEA ('BIOTAFLOW ',2,0,N_BIOTAFLOW,KERR)
         IF (KERR.GT.0) GO TO 99
      ENDIF
C --- SAUMIK,BGANIS

      CALL ALCGEA ('FLDEN ',2,0,N_FLDEN,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('FLDENN ',2,0,N_FLDENN,KERR)
      IF (KERR.GT.0) GO TO 99

ctm   TAMEEM
      CALL ALCGEA ('FLDEN3 ',2,0,N_FLDEN3,KERR)
      IF (KERR.GT.0) GO TO 99
ctm   TAMEEM

      CALL ALCGEA ('PRES ',2,0,N_PRES,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('PRESN ',2,0,N_PRESN,KERR)
      IF (KERR.GT.0) GO TO 99

ctm   TAMEEM
      CALL ALCGEA ('PRESN3 ',2,0,N_PRESN3,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('PRESN_NM1 ',2,Q_MULTIRATE,N_PRESN_NM1,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('PRESN_N1 ',2,Q_MULTIRATE,N_PRESN_N1,KERR)
      IF (KERR.GT.0) GO TO 99

ctm   TAMEEM

      CALL ALCGEA ('VEL ',2,3,N_VEL,KERR)
      IF (KERR.GT.0) GO TO 99

C------ rock compressibility (real*8)

      CALL ALCGEA ('CR ',2,0,N_CR,KERR)
      IF(KERR.GT.0) GO TO 99

      CALL ALCGEA ('PV ',2,0,N_PV,KERR)
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('PVN ',2,0,N_PVN,KERR)
      IF (KERR.GT.0) GO TO 99

! saumik -  added the following on 09/13/17
      CALL ALCGEA ('VSTRAINN ',2,0,N_VSTRAINN,KERR)
      IF (KERR.GT.0) GO TO 99
! saumik

ctm   TAMEEM
      CALL ALCGEA ('PVN3 ',2,0,N_PVN3,KERR)
      IF (KERR.GT.0) GO TO 99
ctm   TAMEEM

      CALL ALCGEA ('PORPRES ',2,0,N_REFPRES,KERR)
      IF (KERR.GT.0) GO TO 99

C  NOTE THAT THE FRAMEWORK ALSO NEEDS POINTERS TO THE NEXT 3 ARRAYS

      CALL ALCGEA ('COFS ',2,27,N_COF,KERR)
      N_COFV(MODACT)=N_COF
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('RESIDS ',2,1,N_RESID,KERR)
      N_RESIDV(MODACT)=N_RESID
      IF (KERR.GT.0) GO TO 99

      CALL ALCGEA ('DELUNK ',2,1,N_DUNK,KERR)
      N_DUNKV(MODACT)=N_DUNK
      IF (KERR.GT.0) GO TO 99

cgp dbg
      IF (DBG) THEN
         WRITE(0,'(A, 3(A,I4))') 'TARRAY: ','N_COF=',N_COF,
     &               ', N_DUNK=',N_DUNK,', N_RESID=',N_RESID
         PAUSE
      ENDIF
cgp dbg

      CALL ALCGEA ('UPMOBPROD ',2,3,N_UPMOBPROD,KERR)
      IF(KERR.GT.0) GO TO 99

C ARRAY RELATED TO MFMFE: ONE DIMENSION HIGHER

      CALL ALCMPFAGEA('AINVF ',2,12,N_AINVF,KERR)
      IF (KERR.GT.0) GO TO 99

      IF(MODELON(15).AND.(.NOT.MBPOROE)) THEN ! SAUMIK,BGANIS
         N_PRESS = N_PRES
         N_PRESSN = N_PRESN
         N_EPV_FLOW = N_PV
         N_EPVN = N_PVN
         N_ECR = N_CR
         N_EFLDEN = N_FLDEN
         N_PREF = N_REFPRES
      ENDIF

! bag8 - for TPORE
      CALL ALCGEA ('DELTAP ',2,0,N_DELTAP,KERR)
      IF (KERR.GT.0) GO TO 99

C bag8 - error against true solution

      IF (ITEST.GT.0) THEN

        CALL ALCGEA ('PRES_ERR ',2,0,N_PRES_ERR,KERR)
        IF (KERR.GT.0) GO TO 99

      ENDIF

   99 RETURN
      END
