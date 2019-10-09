C  TSTDOUT.F - PRINT IMPLICIT SINGLE PHASE FLOW MODEL STANDARD OUTPUT

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE TSTDOUT ()

C  CODE HISTORY:

C  Bahareh Momken   2/16/99   Hydroloty-IMPES gstdout.df is used as template
C  JOHN WHEELER    04/03/99   IMPLICIT SINGLE PHASE MODEL
C  JOHN WHEELER     7/ 9/99   MULTIMODEL CAPABILITY

C*********************************************************************
      SUBROUTINE TSTDOUTS()
C*********************************************************************

C  Prints implicit single phase flow model standard output - scalars

C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'control.h'

      INCLUDE 'tbaldat.h'

      END
C*********************************************************************
      SUBROUTINE TSTDOUTA()
C*********************************************************************

C  Prints implicit single phase flow model standard output - arrays

C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INCLUDE 'tarydat.h'

      EXTERNAL PRINTPRESSURE
      INTEGER JEPROP(2)

C  PRINT FLUID PRESSURE

      TITU='FLUID PRESSURE FOR FAULT BLOCK'
      CALL GEAOUT(N_PRES,1,1)

      JEPROP(1) = 1
      JEPROP(2) = N_PRES
      CALL CALLWORK(PRINTPRESSURE,JEPROP)

      END

C********************************************************************
      SUBROUTINE PRINTPRESSURE(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,
     &           KL1,KL2,KEYOUT,NBLK,PRES)
C********************************************************************
      IMPLICIT NONE
      INCLUDE 'control.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  PRES(IDIM,JDIM,KDIM)
      REAL*8  XC(IDIM+1,JDIM+1,KDIM+1),YC(IDIM+1,JDIM+1,KDIM+1),
     &        ZC(IDIM+1,JDIM+1,KDIM+1)

      INTEGER :: NPRESOUT = 160
      INTEGER J,K,IOFF,JOFF,KOFF,NERR
      LOGICAL ONCEONLY
      DATA ONCEONLY/.TRUE./

      IF (ONCEONLY) THEN
        OPEN(NPRESOUT,FILE='PRESS_FS.DAT',STATUS='unknown')
        ONCEONLY = .FALSE.
      ELSE
        OPEN(NPRESOUT,FILE='PRESS_FS.DAT',STATUS='old',
     &    ACCESS='append')
      ENDIF

      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,NERR)

      DO K = KL1,KL2
      DO J = JL1V(K),JL2V(K)
      WRITE(NPRESOUT,'(0P,E15.6,I4,I4,I4)')PRES(IL1,J,K),J,K
      ENDDO
      ENDDO

      CLOSE(NPRESOUT)

      END
