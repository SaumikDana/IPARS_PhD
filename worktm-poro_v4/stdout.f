C  STDOUT.F - PRINT STANDARD OUTPUT

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE STDOUT  ()
C  SUBROUTINE OPENBUG ()

C  CODE HISTORY:

C  JOHN WHEELER     12/6/95     ALPHA CODE
C  JOHN WHEELER     6/26/98     ADD BUGOPEN ROUTINE
C  JOHN WHEELER     7/ 9/99     MULTIMODEL CAPABILITY

C*********************************************************************
      SUBROUTINE STDOUT ()
C*********************************************************************

C   EXECUTIVE ROUTINE FOR PRINTING STANDARD OUTPUT

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'unitsex.h'
      CHARACTER*50 TITL

      IF (LEVELE.AND.BUGKEY(1)) WRITE (NFBUG,*)'PROC',MYPRC,
     & ' ENTERING SUBROUTINE STDOUT'

      IF (LEVELC) THEN
         WRITE (NFOUT,*)
         TITL='*****'
         CALL PRTTIT(TITL)
         TITL='STANDARD OUTPUT'
         CALL PRTTIT(TITL)

         WRITE (NFOUT,1) TIM*CVMTIME,EXTTIME
    1    FORMAT(/' TIME =',T45,F12.3,1X,A20)
         WRITE (NFOUT,2) NSTEP
    2    FORMAT(' TIME STEP NUMBER =',T45,I12)
C       CALL LSORSO ()
C       CALL MULGRDSO ()

         DO 3 I=1,19
         IF (MODELON(I)) THEN
            WRITE(NFOUT,4) I,BALANCE(1,I,8)
    4       FORMAT(' MODEL',I3,' BALANCES',T46,F11.8)
            IF (MODEQS(I).GT.1)
     &         WRITE(NFOUT,5) (BALANCE(J,I,8),J=2,MODEQS(I))
    5       FORMAT(T46,F11.8)
         ENDIF
    3    CONTINUE

      ENDIF

C  MODEL SPECIFIC OUTPUT - SCALARS

C       MODACT=1
C       IF (MODELON(1)) CALL PSTDOUTS()
C       MODACT=2
C       IF (MODELON(2)) CALL ISTDOUTS()
C         MODACT=3
C         IF (MODELON(3)) CALL XSTDOUTS()
C       MODACT=16
C       IF (MODELON(16).OR.MBPOROE) CALL XSTDOUTS()
              ! SAUMIK,BGANIS
C         MODACT=4
C         IF (MODELON(4)) CALL CSTDOUTS()
C       MODACT=5
C       IF (MODELON(5)) CALL HSTDOUTS()
C       MODACT=19
C       IF (MODELON(19)) CALL HSTDOUTS()
C       MODACT=18
C       IF (MODELON(18)) CALL HSTDOUTS()
C       MODACT=6
C       IF (MODELON(6)) CALL GSTDOUTS()
C       MODACT=7
C       IF (MODELON(7)) CALL MSTDOUTS()
C      MODACT=9
C      IF (MODELON(9)) CALL SSTDOUTS()
C      MODACT=13
C      IF (MODELON(13)) CALL TSTDOUTS()
      MODACT=17
      IF (MODELON(17).OR.MBPOROE) CALL TSTDOUTS()
              ! SAUMIK,BGANIS

C  MODEL SPECIFIC OUTPUT - ARRAYS

C       MODACT=1
C       IF (MODELON(1)) CALL PSTDOUTA()
C       MODACT=2
C       IF (MODELON(2)) CALL ISTDOUTA()
C         MODACT=3
C         IF (MODELON(3)) CALL XSTDOUTA()
C       MODACT=16
C       IF (MODELON(16).OR.MBPOROE) CALL XSTDOUTA()
              ! SAUMIK,BGANIS
C         MODACT=4
C         IF (MODELON(4)) CALL CSTDOUTA()
C       MODACT=5
C       IF (MODELON(5)) CALL HSTDOUTA()
C       MODACT=19
C       IF (MODELON(19)) CALL HSTDOUTA()
C       MODACT=18
C       IF (MODELON(18)) CALL HSTDOUTA()
C       MODACT=6
C       IF (MODELON(6)) CALL GSTDOUTA()
C       MODACT=7
C       IF (MODELON(7)) CALL MSTDOUTA()
C      MODACT=9
C      IF (MODELON(9)) CALL SSTDOUTA()
C      MODACT=13
C      IF (MODELON(13)) CALL TSTDOUTA()
      MODACT=17
      IF (MODELON(17).OR.MBPOROE) CALL TSTDOUTA()
              ! SAUMIK,BGANIS
       MODACT=15
       IF (MODELON(15)) CALL ESTDOUTA()
              MODACT=0

      CALL WAITALL()

      END
C*********************************************************************
      SUBROUTINE OPENBUG ()
C*********************************************************************

C   OPENS DEBUG OUTPUT FILE FOR MULTIPLE PROCESSORS

C*********************************************************************
      INCLUDE 'control.h'

      CHARACTER*20 FN
      CHARACTER*8 EX
      CHARACTER*1 FN1(20),EX1(8),BLK
      EQUIVALENCE (FN1,FN),(EX1,EX)

      IF (BUGOPEN) RETURN

      BLK=' '
      EX=' '
      FN='DEBUG.'
      WRITE (EX,1) MYPRC
    1 FORMAT(I8)
      M=7
      DO 2 I=1,8
      IF (EX1(I).NE.BLK) THEN
         FN1(M)=EX1(I)
         M=M+1
      ENDIF
    2 CONTINUE

      WRITE(*,*) ' OPENING DEBUG FILE ',FN
      OPEN (NFBUG,FILE=FN,STATUS='UNKNOWN',ERR=13)

      BUGOPEN=.TRUE.
   13 RETURN

      END
