C  TIMER.FOR - EXECUTION TIMER

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE TIMSET ()
C  SUBROUTINE TIMON  (NTIM)
C  SUBROUTINE TIMOFF (NTIM)
C  SUBROUTINE TIMOUT ()

C  HISTORY:

C  JOHN WHEELER     11/10/95    ORIGINAL BETA CODE

C**********************************************************************
      SUBROUTINE TIMSET ()
C***********************************************************************

C  SETS TIMER NAMES AND CLASSES, AND CLEARS AND STOPS ALL TIMERS.
C  CALL ONCE AT BEGINNING OF THE SIMULATION

C  NOTES:

C  TIMKLS(I) = TIMER CLASS
C            = 0 ==> UNUSED
C            = 1 ==> TOTAL TIME
C            = 2 ==> COMMUNICATION TIMES
C            = 3 ==> ALL REDUNDANT CALCULATION TIMES
C            = 4 ==> MISCELLENOUS TIMES

C  TIMERS 20 TO 50 MAY BE DEFINED BY THE PHYSICAL MODELS

C  TIMERS 1 AND 2 ARE STARTED AND STOPPED BY THE FRAMWORK

C  TIMERS 3 TO 19 ARE DEFINED BY THE FRAMWORK BUT MUST BE STARTED AND
C  STOPPED AT APPROPRIATE POINTS IN THE PHYSICAL MODELS

C***********************************************************************
      INCLUDE 'times.h'

      NUMTIM=50

      DO 1 I=1,NUMTIM
      TIMNAM(I)=" "
      TIMTOT(I)=0.
      TIMIN(I)=0.
      TIMRUN(I)=.FALSE.
    1 TIMKLS(I)=0

C  ASSIGN TIMER NAMES AND CLASSES

      TIMNAM(1)='TOTAL TIME'
      TIMKLS(1)=1

      TIMNAM(2)='I/O COMMUNICATION'
      TIMKLS(2)=2

      TIMNAM(3)='COEFFICIENT COMMUNICATION'
      TIMKLS(3)=2

      TIMNAM(4)='LINEAR SOLVER COMMUNICATION'
      TIMKLS(4)=2

      TIMNAM(5)='MISC. COMMUNICATION'
      TIMKLS(5)=2

      TIMNAM(6)='ALL REDUNDANT CALCULATIONS'
      TIMKLS(6)=3

      TIMNAM(7)='TOTAL INITIALIZATION TIME'
      TIMKLS(7)=4

      TIMNAM(8)='TOTAL COEFFICIENT TIME'
      TIMKLS(8)=4

      TIMNAM(9)='TOTAL LINEAR SOLVER TIME'
      TIMKLS(9)=4

      TIMNAM(10)='TOTAL WELL MANAGEMENT TIME'
      TIMKLS(10)=4

      TIMNAM(11)='TOTAL RESTART OUTPUT TIME'
      TIMKLS(11)=4

      TIMNAM(12)='INTRA BLOCK TOTAL TIME'
      TIMKLS(12)=4

      TIMNAM(13)='VISUALIZATION TOTAL TIME'
      TIMKLS(13)=4

      END
C**********************************************************************
      SUBROUTINE TIMON (NTIM)
C**********************************************************************

C  STARTS A TIMER RUNNING.  THE TIME WILL NOT BE SAVED UNTIL TIMOFF
C  TIMGET, OR TIMOUT IS CALLED

C  NTIM  = TIMER NUMBER (INPUT, INTEGER)

C**********************************************************************
      INCLUDE 'times.h'
      REAL*8 MPI_WTIME

      TIMRUN(NTIM)=.TRUE.
C      TIMIN(NTIM)=TIMEF()

      TIMIN(NTIM)=MPI_WTIME()

C      CALL CPUTIME(TIMIN(NTIM))

      END
C**********************************************************************
      SUBROUTINE TIMOFF (NTIM)
C**********************************************************************

C  STOPS A TIMER

C  NTIM  = TIMER NUMBER (INPUT, INTEGER)

C**********************************************************************
      INCLUDE 'times.h'
      REAL*8 MPI_WTIME
C      REAL*8 T

      IF (TIMRUN(NTIM)) THEN

C      TIMTOT(NTIM)=TIMTOT(NTIM)+TIMEF()-TIMIN(NTIM)

      TIMTOT(NTIM)=TIMTOT(NTIM)+(MPI_WTIME()-TIMIN(NTIM))

C      CALL CPUTIME(T)
C      TIMTOT(NTIM)=TIMTOT(NTIM)+(T-TIMIN(NTIM))

         TIMRUN(NTIM)=.FALSE.
      ENDIF
      END
C**********************************************************************
      SUBROUTINE TIMGET (NTIM,TIMD)
C**********************************************************************

C  RETURNS THE ACCUMILATED TIME IN A TIMER.  IF A TIMER IS RUNNING, IT
C  IS NOT STOPPED.

C  NTIM  = TIMER NUMBER (INPUT, INTEGER)

C  TIMD   = TIME RETURNED (OUTPUT, REAL*8)

C**********************************************************************
      INCLUDE 'times.h'
      REAL*8 T,TIMD
      REAL*8 MPI_WTIME
      DATA T/0.D0/

      IF (TIMRUN(NTIM)) THEN

C      T=TIMEF()

      T=MPI_WTIME()

C      CALL CPUTIME(T)

         TIMD=TIMTOT(NTIM)+T-TIMIN(NTIM)
         TIMTOT(NTIM)=TIMD
         TIMIN(NTIM)=T
      ELSE
         TIMD=TIMTOT(NTIM)
      ENDIF
      END
C**********************************************************************
      SUBROUTINE TIMOUT ()
C***********************************************************************

C  ROUTINE COLLECTS, AVERAGES, AND PRINTS TIMING DATA

C***********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'times.h'
      INCLUDE 'mpif.h'

      CHARACTER*50 TITL
      REAL TIMCUR(50),TIMMIN(50),TIMMAX(50),TIMAVE(50),
     &   TIMS(50,1024)
      REAL*8 T
      INTEGER ISTAT(MPI_STATUS_SIZE)
      DATA T/0.D0/

      MTM=1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      DO 1 I=1,NUMTIM
      IF (TIMRUN(I)) THEN

C      T=TIMEF()

      T=MPI_WTIME()

C      CALL CPUTIME(T)

         TIMCUR(I)=TIMTOT(I)+T-TIMIN(I)
      ELSE
         TIMCUR(I)=TIMTOT(I)
      ENDIF
    1 CONTINUE

C  COLLECT TIMES ON PROCESSOR 0

      IF (NUMPRC.GT.1) THEN
         IF (MYPRC.EQ.0) THEN

            DO 7 I=1,NUMTIM
            TIMS(I,1)=TIMCUR(I)
            TIMMIN(I)=TIMCUR(I)
            TIMMAX(I)=TIMCUR(I)
    7       TIMAVE(I)=TIMCUR(I)

            MS=1
            DO 8 I=2,NUMPRC

        CALL MPI_RECV(TIMCUR,50,MPI_REAL,
     &  MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)
        MS=ISTAT(MPI_SOURCE)+1

            DO 8 J=1,NUMTIM
            TIMS(J,MS)=TIMCUR(J)
            IF (TIMCUR(J).LT.TIMMIN(J)) TIMMIN(J)=TIMCUR(J)
            IF (TIMCUR(J).GT.TIMMAX(J)) TIMMAX(J)=TIMCUR(J)
    8       TIMAVE(J)=TIMAVE(J)+TIMCUR(J)

         ELSE

        CALL MPI_SEND(TIMCUR,NUMTIM,MPI_REAL,0,MSGTAG(MTM),
     &  MPI_COMM_WORLD,IERR)

         ENDIF
      ENDIF

C  EXIT IF NOT PROCESSOR 0 OR IF ALL TIMERS READ 0.

      IF (MYPRC.NE.0) RETURN
      DO 2 I=1,NUMTIM
      IF (TIMCUR(I).NE.0.D0) GO TO 3
    2 CONTINUE
      RETURN

C  PRINT HEADER

    3 WRITE (NFOUT,*)
      TITL ='*****'
      CALL PRTTIT (TITL)
      TITL ='TIMER RESULTS IN SECONDS'
      CALL PRTTIT (TITL)

C  PRINT TIMES - SINGLE PROCESSOR

      IF (NUMPRC.LT.2) THEN
         DO 5 I=1,NUMTIM
         IF (TIMCUR(I).GT.0.) WRITE (NFOUT,6) TIMNAM(I),TIMCUR(I)
    6    FORMAT(/1X,A35,5X,F12.3)
    5    CONTINUE
         RETURN
      ENDIF

C  PRINT TIME SUMMARY - MULTIPLE PROCESSORS

      WRITE (NFOUT,14)
   14 FORMAT(/T11,'TIMER',T41,'AVERAGE',T52,'MINIMUM',T63,'MAXIMUM'/)
      DO 11 I=1,NUMTIM
      TIMAVE(I)=TIMAVE(I)/NUMPRC
      IF (TIMMAX(I).GT.0.)
     &   WRITE (NFOUT,12) TIMNAM(I),TIMAVE(I),TIMMIN(I),TIMMAX(I)
   12 FORMAT(1X,A35,3F11.3)
   11 CONTINUE

C  PRINT ALL TIMES - MULTIPLE PROCESSORS

      IF (LEVELC.AND.ALLTIMES) THEN
         WRITE(NFOUT,19)
   19    FORMAT(/' ********************  DETAILS  ********************')
         N1=1
   15    N2=N1+3
         IF (N2.GT.NUMPRC) N2=NUMPRC
         WRITE(NFOUT,16) (N-1,N=N1,N2)
   16    FORMAT(/T22,'PROCESSOR =',T34,4I10)
         DO 18 I=1,NUMTIM
         IF (TIMS(I,1).GT.0.)
     &   WRITE (NFOUT,17) TIMNAM(I),(TIMS(I,N),N=N1,N2)
   17    FORMAT(1X,A35,4F10.2)
   18    CONTINUE
         IF (N2.LT.NUMPRC) THEN
            N1=N2+1
            GO TO 15
         ENDIF
      ENDIF

      END
