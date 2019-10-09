C  PUTIL.F - MULTIPROCESSOR UTILITY ROUTINES

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE WAITALL ()
C  SUBROUTINE CHECK   (FLGIN, FLGOUT)
C  SUBROUTINE SUMIT   (NUMVAL, VEC)
C  SUBROUTINE MAXIT   (NUMVAL, VEC)
C  SUBROUTINE MAXITI  (NUMVAL, IVEC)
C  SUBROUTINE MINIT   (NUMVAL, VEC)
C  SUBROUTINE MINITI  (NUMVAL, IVEC)
C  SUBROUTINE SPREAD  (NUMVAL, IVEC)
C  SUBROUTINE SPREAD4 (NUMVAL, RVEC)
C  SUBROUTINE SPREAD8 (NUMVAL, RVEC)
C  SUBROUTINE SPREADC (NC, S)
C  SUBROUTINE MSGOUT  (A,L)
C  SUBROUTINE PROCOUT ()
C  SUBROUTINE WELLSHR (NW)
C  SUBROUTINE WELOWN  ()
C  SUBROUTINE HISPASS ()
C  SUBROUTINE WELSUM  (NW, NVAL, VEC)
C  SUBROUTINE WELGET  (NVAL, VEC)
C  SUBROUTINE WELPUT  (NVAL, VEC)
C  SUBROUTINE RSSR4   (N, RBUF, NERR)
C  SUBROUTINE RSSR8   (N, RBUF, NERR)
C  SUBROUTINE RSSI4   (N, IBUF, NERR)
C  SUBROUTINE RRR4    (NERR)
C  SUBROUTINE RRR8    (NERR)
C  SUBROUTINE RRI4    (NERR)
C  SUBROUTINE TYPEOUT (A,L,M)
C  SUBROUTINE POLL (MTAG,NP)
C  SUBROUTINE SETTAGS ()
C  SUBROUTINE ELEGET  (NW, NVAL, VEC)
C  SUBROUTINE ELEPUT  (NW, NVAL, VEC)
C  SUBROUTINE WELDIST (NW, NVAL, VEC)

C  CODE HISTORY:

C  JOHN WHEELER       1/5/95     ALPHA CODE
C  JOHN WHEELER      3/15/97     ADD WELL UTILITIES
C  JOHN WHEELER      2/28/98     ADD RESTART ROUTINES
C  JOHN WHEELER      9/12/98     ADD POLLING FOR RESTART OUTPUT
C  JOHN WHEELER      4/20/99     ADD GENERIC MULTI-MODEL CAPABILITY
C  RICK DEAN        12/20/01     ADD WELPUT,ELEGET,ELEPUT,WELDIST
C  SUNIL G THOMAS    -/--/09     CHANGED MODACT TO NON-ZERO VARIABLE
C                                CURRENT_MODEL, WHICH IS INTENDED TO
C                                BE A DRIVING MODEL TO AVOID -CB ERROR
C                                AND DEADLOCK ISSUE IN TRCHEM PARALL RUN.
C                                THIS WILL HAVE TO BE ACCOUNTED FOR IN
C                                MULTI MODEL RUNS (ONE WAY IS BY SETTING
C                                CURRENT_MODEL TO MODACT BEFORE CONTROL
C                                IS TRANSFERRED - AS IN FILE RESTART.DF).
C                                BUT CURRENT VERSION WORKS FOR ALL SINGLE
C                                MODEL CASES AND FLOW COUPLED TO TRCHEM

C  NOTES:

C     1)  ERROR NUMBERS 201 TO 240 ARE RESERVED FOR PARALLEL ROUTINES

C***********************************************************************
      SUBROUTINE WAITALL ()
C***********************************************************************

C  SYNCRONIZES ALL PROCESSORS
C  CAN BE CALLED ONLY BY THE FRAMEWORK

C***********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'mpif.h'

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,' ENTERING SUBROUTINE WAITALL'
         WRITE (*,*)' PROC',MYPRC,' ENTERING SUBROUTINE WAITALL'
      ENDIF

      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

      END
C*********************************************************************
      SUBROUTINE CHECK (FLGIN, FLGOUT)
C*********************************************************************

C  Broadcasts a logical flag from each processor to all other processors.
C  CAN BE CALLED ONLY BY THE FRAMEWORK

C  FLGIN = Flag from current processor (input, LOGICAL)

C  FLGOUT = .TRUE.  ==> All processors send .TRUE. (output, LOGICAL)
C         = .FALSE. ==> One or more processors send .FALSE.

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      LOGICAL FLGIN,FLGOUT,FLG

      FLGOUT=FLGIN

C  BROADCAST FLAG

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE CHECK'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE CHECK'
      ENDIF

      DO 1 I=1,NUMPRC
      NT=I-1
      IF (NT.NE.MYPRC) THEN

      CALL MPI_SEND(FLGIN,1,MPI_LOGICAL,NT,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ENDIF
    1 CONTINUE

C  RECEIVE FLAGS

      DO 2 I=2,NUMPRC

      CALL MPI_RECV(FLG,1,MPI_LOGICAL,MPI_ANY_SOURCE,MSGTAG(MTM),
     & MPI_COMM_WORLD,ISTAT,IERR)

      IF (.NOT.FLG) FLGOUT=.FALSE.
    2 CONTINUE

      END
C*********************************************************************
      SUBROUTINE SUMIT (NUMVAL, VEC)
C*********************************************************************

C  Forms one or more sums with each processor contributing one element
C  to each sum.  This routine is intended primarily for calculating
C  mass balances but could be used for other purposes.  The sums are
C  formed on processor 0.

C  NUMVAL = Number of values in VECIN and VECOUT (input, INTEGER)
C           (Must be the same on all processors)

C  VEC() = Current processor contributions to sums (input, REAL*8)
C        = Result sums (processor 0 only) (output, REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      REAL*8 VEC(*),VTERM(NUMVAL)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE SUMIT'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE SUMIT'
      ENDIF

      IF (MYPRC.NE.0) THEN

C  SEND INPUT VECTOR TO PROCESSOR 0

      CALL MPI_SEND(VEC,NUMVAL,MPI_DOUBLE_PRECISION,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ELSE

C  RECEIVE VECTORS ON PROCESSOR 0

         DO 4 I=2,NUMPRC
         IF ((CURRENT_MODEL.EQ.0).OR.MODPROC(CURRENT_MODEL,I)
     &                           .OR.MBPOROE) THEN ! SAUMIK,BGANIS

         CALL MPI_RECV(VTERM,NUMVAL,MPI_DOUBLE_PRECISION,
     &   MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

            DO 3 J=1,NUMVAL
    3       VEC(J)=VEC(J)+VTERM(J)
         ENDIF
    4    CONTINUE
      ENDIF

      END
C*********************************************************************
      SUBROUTINE MAXIT (NUMVAL, VEC)
C*********************************************************************

C  Picks out one or more maximums with each processor contributing one
C  element to each maximum.  This routine is intended primarily for checking
C  maximum residuals but could be used for other purposes.  The maximum
C  results are available on only processor 0.

C  NUMVAL = Number of values in VECIN and VECOUT (input, INTEGER)
C           (Must be the same on all processors)

C  VEC() = Current processor contributions to maximums (input, REAL*8)
C        = Result maximums (processor 0 only) (output, REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER NUMVAL,MTM
      INTEGER IERR,I,J
      REAL*8 VEC(*),VTERM(NUMVAL)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MAXIT'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MAXIT'
      ENDIF

      IF (MYPRC.NE.0) THEN

C  SEND INPUT VECTOR TO PROCESSOR 0

      CALL MPI_SEND(VEC,NUMVAL,MPI_DOUBLE_PRECISION,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ELSE

C  RECEIVE VECTORS ON PROCESSOR 0

         DO 4 I=2,NUMPRC
         IF ((CURRENT_MODEL.EQ.0).OR.MODPROC(CURRENT_MODEL,I)
     &                           .OR.MBPOROE) THEN ! SAUMIK,BGANIS

         CALL MPI_RECV(VTERM,NUMVAL,MPI_DOUBLE_PRECISION,
     &    MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

            DO 3 J=1,NUMVAL
            IF (VTERM(J).GT.VEC(J)) VEC(J)=VTERM(J)
    3       CONTINUE
         ENDIF
    4    CONTINUE
      ENDIF

      END
C*********************************************************************
      SUBROUTINE MAXITI (NUMVAL, IVEC)
C*********************************************************************

C  Picks out one or more maximums with each processor contributing one
C  element to each maximum.  This routine is intended primarily for checking
C  maximum residuals but could be used for other purposes.  The maximum
C  results are available on only processor 0.

C  NUMVAL = Number of values in VECIN and VECOUT (input, INTEGER)
C           (Must be the same on all processors)

C  IVEC() = Current processor contributions to maximums (input, INTEGER)
C        = Result maximums (processor 0 only) (output, INTEGER)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER NUMVAL,MTM
      INTEGER IVEC(*),IVTERM(NUMVAL)
      INTEGER IERR,I,J

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MAXIT'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MAXIT'
      ENDIF

      IF (MYPRC.NE.0) THEN

C  SEND INPUT VECTOR TO PROCESSOR 0

      CALL MPI_SEND(IVEC,NUMVAL,MPI_INTEGER,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ELSE

C  RECEIVE VECTORS ON PROCESSOR 0

         DO 4 I=2,NUMPRC
         IF ((CURRENT_MODEL.EQ.0).OR.MODPROC(CURRENT_MODEL,I)
     &                           .OR.MBPOROE) THEN ! SAUMIK,BGANIS

         CALL MPI_RECV(IVTERM,NUMVAL,MPI_INTEGER,
     &    MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

            DO 3 J=1,NUMVAL
            IF (IVTERM(J).GT.IVEC(J)) IVEC(J)=IVTERM(J)
    3       CONTINUE
         ENDIF
    4    CONTINUE
      ENDIF

      END

C*********************************************************************
      SUBROUTINE MINIT (NUMVAL, VEC)
C*********************************************************************

C  Picks out one or more minimums with each processor contributing one
C  element to each minimum.  This routine is intended primarily for
C  checking minimum residuals but could be used for other purposes.
C  The minimum results are available on only processor 0.

C  NUMVAL = Number of values in VECIN and VECOUT (input, INTEGER)
C           (Must be the same on all processors)

C  VEC() = Current processor contributions to minimums (input, REAL*8)
C        = Result minimums (processor 0 only) (output, REAL*8)

C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER NUMVAL,MTM
      REAL*8 VEC(*),VTERM(NUMVAL)
      INTEGER IERR,I,J

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MINIT'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MINIT'
      ENDIF

      IF (MYPRC.NE.0) THEN

C  SEND INPUT VECTOR TO PROCESSOR 0

      CALL MPI_SEND(VEC,NUMVAL,MPI_DOUBLE_PRECISION,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ELSE

C  RECEIVE VECTORS ON PROCESSOR 0

         DO 4 I=2,NUMPRC
         IF ((CURRENT_MODEL.EQ.0).OR.MODPROC(CURRENT_MODEL,I)
     &                           .OR.MBPOROE) THEN ! SAUMIK,BGANIS

         CALL MPI_RECV(VTERM,NUMVAL,MPI_DOUBLE_PRECISION,
     &    MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

            DO 3 J=1,NUMVAL
            IF (VTERM(J).LT.VEC(J)) VEC(J)=VTERM(J)
    3       CONTINUE
         ENDIF
    4    CONTINUE
      ENDIF

      END

C*********************************************************************
      SUBROUTINE MINITI (NUMVAL, IVEC)
C*********************************************************************

C  Picks out one or more minimums with each processor contributing one
C  element to each minimum.  This routine is intended primarily for
C  checking minimum residuals but could be used for other purposes.
C  The minimum results are available on only processor 0.

C  NUMVAL = Number of values in VECIN and VECOUT (input, INTEGER)
C           (Must be the same on all processors)

C  IVEC() = Current processor contributions to minimums (input, INTEGER)
C         = Result minimums (processor 0 only) (output, INTEGER)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER NUMVAL,MTM
      INTEGER IVEC(*),IVTERM(NUMVAL)
      INTEGER IERR,I,J

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MAXITI'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MAXITI'
      ENDIF

      IF (MYPRC.NE.0) THEN

C  SEND INPUT VECTOR TO PROCESSOR 0

      CALL MPI_SEND(IVEC,NUMVAL,MPI_INTEGER,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ELSE

C  RECEIVE VECTORS ON PROCESSOR 0

         DO 4 I=2,NUMPRC
         IF ((CURRENT_MODEL.EQ.0).OR.MODPROC(CURRENT_MODEL,I)
     &                           .OR.MBPOROE) THEN ! SAUMIK,BGANIS

         CALL MPI_RECV(IVTERM,NUMVAL,MPI_INTEGER,
     &    MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

            DO 3 J=1,NUMVAL
            IF (IVTERM(J).LT.IVEC(J)) IVEC(J)=IVTERM(J)
    3       CONTINUE
         ENDIF
    4    CONTINUE
      ENDIF

      END

C*********************************************************************
      SUBROUTINE SPREAD (NUMVAL, IVEC)
C*********************************************************************

C  Sends one or more INTEGER values from processor zero to all other
C  processors running the active physical model.

C  NUMVAL = Number of values in IVEC (input, INTEGER)
C           (Must be the same on all processors paticipating)

C  IVEC() = Data to be transmitted (input and output, INTEGER)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IVEC(*)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREAD'
         WRITE (*,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREAD'
      ENDIF

      IF (NUMMODEL.EQ.1.OR.CURRENT_MODEL.EQ.0.OR.MBPOROE) THEN
      ! SAUMIK,BGANIS

C  USE BROADCAST IF ALL PROCESSORS ARE TO RECEIVE

      CALL MPI_BCAST(IVEC,NUMVAL,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)

      ELSE

C USE SEND/RECEIVE IF NOT ALL PROCESSORS ARE TO RECEIVE

         MTM=CURRENT_MODEL+1
         MSGTAG(MTM)=MSGTAG(MTM)+1
         IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

         DO 1 I=2,NUMPRC

         IF (MODPROC(CURRENT_MODEL,I)) THEN
            IF (MYPRC.EQ.0) THEN

         CALL MPI_SEND(IVEC,NUMVAL,MPI_INTEGER,I-1,
     &   MSGTAG(MTM),MPI_COMM_WORLD,IERR)

            ELSE
               IF (MYPRC.EQ.I-1) THEN

           CALL MPI_RECV(IVEC,NUMVAL,MPI_INTEGER,
     &     MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

               ENDIF
            ENDIF
         ENDIF
    1    CONTINUE

      ENDIF

      END
C*********************************************************************
      SUBROUTINE SPREAD4 (NUMVAL, RVEC)
C*********************************************************************

C  Sends one or more REAL*4 values from processor zero to all other
C  processors running the active physical model.

C  NUMVAL = Number of values in RVEC (input, INTEGER)
C           (Must be the same on all processors paticipating)

C  RVEC() = Data to be transmitted (input and output, REAL*4)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      REAL*4 RVEC(*)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREAD4'
         WRITE (*,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREAD4'
      ENDIF

      IF (NUMMODEL.EQ.1.OR.CURRENT_MODEL.EQ.0.OR.MBPOROE) THEN
      ! SAUMIK,BGANIS

C  USE BROADCAST IF ALL PROCESSORS ARE TO RECEIVE

      CALL MPI_BCAST(RVEC,NUMVAL,MPI_REAL,0,MPI_COMM_WORLD,IERR)

      ELSE

C USE SEND/RECEIVE IF NOT ALL PROCESSORS ARE TO RECEIVE

         MTM=CURRENT_MODEL+1
         MSGTAG(MTM)=MSGTAG(MTM)+1
         IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

         DO 1 I=2,NUMPRC
         IF (MODPROC(CURRENT_MODEL,I)) THEN
            IF (MYPRC.EQ.0) THEN

         CALL MPI_SEND(RVEC,NUMVAL,MPI_REAL,I-1,
     &   MSGTAG(MTM),MPI_COMM_WORLD,IERR)

            ELSE
               IF (MYPRC.EQ.I-1) THEN

            CALL MPI_RECV(RVEC,NUMVAL,MPI_REAL,
     &      MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

               ENDIF
            ENDIF
         ENDIF
    1    CONTINUE

      ENDIF

      END
C*********************************************************************
      SUBROUTINE SPREAD8 (NUMVAL, RVEC)
C*********************************************************************

C  Sends one or more REAL*8 values from processor zero to all other
C  processors running the active physical model.

C  NUMVAL = Number of values in RVEC (input, INTEGER)
C           (Must be the same on all processors paticipating)

C  RVEC() = Data to be transmitted (input and output, REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      REAL*8 RVEC(*)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREAD8'
         WRITE (*,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREAD8'
      ENDIF

      IF (NUMMODEL.EQ.1.OR.CURRENT_MODEL.EQ.0.OR.MBPOROE) THEN
      ! SAUMIK,BGANIS

C  USE BROADCAST IF ALL PROCESSORS ARE TO RECEIVE

      CALL MPI_BCAST(RVEC,NUMVAL,MPI_DOUBLE_PRECISION,0,
     & MPI_COMM_WORLD,IERR)

      ELSE

C USE SEND/RECEIVE IF NOT ALL PROCESSORS ARE TO RECEIVE

         MTM=CURRENT_MODEL+1
         MSGTAG(MTM)=MSGTAG(MTM)+1
         IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

         DO 1 I=2,NUMPRC
         IF (MODPROC(CURRENT_MODEL,I)) THEN
            IF (MYPRC.EQ.0) THEN

         CALL MPI_SEND(RVEC,NUMVAL,MPI_DOUBLE_PRECISION,I-1,
     &   MSGTAG(MTM),MPI_COMM_WORLD,IERR)

            ELSE
               IF (MYPRC.EQ.I-1) THEN

            CALL MPI_RECV(RVEC,NUMVAL,MPI_DOUBLE_PRECISION,
     &      MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

               ENDIF
            ENDIF
         ENDIF
    1    CONTINUE

      ENDIF

      END
C*********************************************************************
      SUBROUTINE SPREADC (NC, S)
C*********************************************************************

C  Sends a character string from processor zero to all other
C  processors running the active physical model.

C  NC = Number of characters in S (input, INTEGER)
C           (Must be the same on all processors paticipating)

C  S() = Data to be transmitted (input and output, CHARACTER*1)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      CHARACTER*1 S(*)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREADC'
         WRITE (*,*)' PROC',MYPRC,' MODEL',CURRENT_MODEL,
     &      ' ENTERING SUBROUTINE SPREADC'
      ENDIF

      IF (NUMMODEL.EQ.1.OR.CURRENT_MODEL.EQ.0.OR.MBPOROE) THEN
      ! SAUMIK,BGANIS

C  USE BROADCAST IF ALL PROCESSORS ARE TO RECEIVE

      CALL MPI_BCAST(S,NC,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)

      ELSE

C USE SEND/RECEIVE IF NOT ALL PROCESSORS ARE TO RECEIVE

         MTM=CURRENT_MODEL+1
         MSGTAG(MTM)=MSGTAG(MTM)+1
         IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

         DO 1 I=2,NUMPRC

         IF (MODPROC(CURRENT_MODEL,I)) THEN
            IF (MYPRC.EQ.0) THEN

         CALL MPI_SEND(S,NC,MPI_CHARACTER,I-1,MSGTAG(MTM),
     &   MPI_COMM_WORLD,IERR)

            ELSE
               IF (MYPRC.EQ.I-1) THEN

            CALL MPI_RECV(S,NC,MPI_CHARACTER,MPI_ANY_SOURCE,
     &      MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

               ENDIF
            ENDIF
         ENDIF
    1    CONTINUE

      ENDIF

      END
C*********************************************************************
      SUBROUTINE MSGOUT (A,L)
C*********************************************************************

C  Sends a character string to processor 0 and prints it to NFOUT.
C  All processors must call this routine.  Strings longer than 80
C  characters will be printed in 80 character lines.
C  Only strings with length greater than 0 will be printed.

C  A = Character string to be printed (input, CHARACTER*1)

C  L = Length of A (max 400) (input, INTEGER)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      CHARACTER*1 A(L),AR(400)

      LO=L
      IF (LO.GT.400) LO=400

C  SEND STRING

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MSGOUT'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE MSGOUT'
      ENDIF

      IF (MYPRC.GT.0) THEN

      CALL MPI_SEND(A,LO,MPI_CHARACTER,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ENDIF

C  RECEIVE AND PRINT STRINGS

      IF (MYPRC.EQ.0) THEN
         DO 1 I=1,NUMPRC
         IF (I.EQ.1) THEN
            DO 2 J=1,LO
    2       AR(J)=A(J)
         ELSE
            IF (CURRENT_MODEL.NE.0) THEN
               IF (.NOT.MODPROC(CURRENT_MODEL,I)) GO TO 1
            ENDIF

      CALL MPI_RECV(AR,400,MPI_CHARACTER,MPI_ANY_SOURCE,
     & MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)
      CALL MPI_GET_COUNT(ISTAT,MPI_CHARACTER,LO,IERR)

         ENDIF
         IF (LO.GT.0) THEN
            WRITE (NFOUT,3) (AR(J),J=1,LO)
    3       FORMAT(80A1)
         ENDIF
    1 CONTINUE
      ENDIF

      END
C*********************************************************************
      SUBROUTINE PROCOUT ()
C***********************************************************************

C  ROUTINE PRINTS THE DISTRIBUTION OF GRID COLUMNS AMONG PROCESSORS
C  AND THE NUMBER OF GRID ELEMENTS SENT BY EACH PROCESSOR DURING UPDATE
C  (7 POINT TEMPLATE)

C***********************************************************************

C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'

      INTEGER NM(1024),NAE(4)
!bw NODAL COMMUNICATION OUTPUT
      INTEGER NAEN(8),N0N,NYMN
      CHARACTER*50 TT
      LOGICAL :: DBG = .FALSE.

      IF (NUMPRC.LT.11) THEN
!bw         JS=36
         JS=28
      ELSE
!bw         JS=24
         JS=14
      ENDIF

C  LOOP OVER FAULT BLOCKS

      DO 1 M=1,NUMBLK
      CALL BLKDIM (M,NX,NY,NZ,KERR)
      N0=N0MAP(M)
      NYM=NYMAP(M)

C  PRINT THE ARRAY TITLE

      WRITE (NFOUT,*)
      TT='GRID COLUMN ASSIGNMENTS FOR FAULT BLOCK'
      CALL MAKTIT (TT,50,M)
      CALL PRTTIT (TT)
      IF (DBG) WRITE(*,*) TT

C  J LOOP

      J2=0
    2 J1=J2+1
      J2=J1+JS
      IF (J2.GT.NY) J2=NY
      WRITE (NFOUT,3) (J,J=J1,J2)
      IF (DBG) WRITE (*,3) (J,J=J1,J2)
!bw    WRITE (NFOUT,3) (J,J=J1,J2,2)  ! skip by 2
    3 FORMAT(/'  K J',99I5)

C  PRINT PROCESSOR ASSIGNMENTS

      N=N0
      DO K=1,NZ
        N=N+NYM
        WRITE (NFOUT,5) K,(PRCMAP(N+J),J=J1,J2)
        IF (DBG) WRITE (*,5) K,(PRCMAP(N+J),J=J1,J2)
!bw      WRITE (NFOUT,5) K,(PRCMAP(N+J),J=J1,J2,2)  ! skip by 2
      ENDDO
    5 FORMAT(I5,99I5)

C  END J LOOP AND FAULT BLOCK LOOP

      IF (J2.LT.NY) GO TO 2
    1 CONTINUE

C  COUNT AND PRINT THE NUMBER OF GRID ELEMENTS TRANSMITTED BY EACH
C  PROCESSOR DURING AN UPDATE

      DO 20 N=1,NUMPRC
   20 NM(N)=0

      DO 21 M=1,NUMBLK
      CALL BLKDIM (M,NX,NY,NZ,KERR)
      N0=N0MAP(M)
      NYM=NYMAP(M)
      DO 21 N=0,NUMPRC-1
      DO 21 K=1,NZ
      NN=N0+K*NYM
      DO 21 J=1,NY
      NNJ=NN+J
      IF (PRCMAP(NNJ).EQ.N) THEN

         IF (J.LT.NY) THEN
            NAE(1)=PRCMAP(NNJ+1)
         ELSE
            NAE(1)=N
         ENDIF
         IF (J.GT.1) THEN
            NAE(2)=PRCMAP(NNJ-1)
         ELSE
            NAE(2)=N
         ENDIF
         IF (K.LT.NZ) THEN
            NAE(3)=PRCMAP(NNJ+NYM)
         ELSE
            NAE(3)=N
         ENDIF
         IF (K.GT.1) THEN
            NAE(4)=PRCMAP(NNJ-NYM)
         ELSE
            NAE(4)=N
         ENDIF

         DO 22 I=1,4
         IF (NAE(I).LT.0) NAE(I)=N
   22    CONTINUE

         DO 23 I=1,3
         DO 23 II=I+1,4
         IF (NAE(I).EQ.NAE(II)) NAE(II)=N
   23    CONTINUE

         DO 24 I=1,4
         IF (NAE(I).NE.N) NM(N+1)=NM(N+1)+NX
   24    CONTINUE

      ENDIF
   21 CONTINUE

      WRITE (NFOUT,25)
   25 FORMAT(/' ***************  GRID ELEMENTS SENT DURING UPDATE',
     & ' (7 POINT)  ****************'//' PROCESSOR   ELEMENTS')
      I=0
      DO 26 N=1,NUMPRC
      I=I+NM(N)
   26 WRITE (NFOUT,27) N-1,NM(N)
   27 FORMAT(I6,6X,I7)
      WRITE (NFOUT,28) I
   28 FORMAT('   TOTAL',4X,I7)

!bw Added output for element communication stencil (27-pt)
C  COUNT AND PRINT THE NUMBER OF GRID NODES TRANSMITTED BY EACH
C  PROCESSOR DURING AN UPDATE

      DO 40 N=1,NUMPRC
   40 NM(N)=0

      DO 41 M=1,NUMBLK
      CALL BLKDIM (M,NX,NY,NZ,KERR)
      N0=N0MAP(M)
      NYM=NYMAP(M)
      DO 41 N=0,NUMPRC-1
      DO 41 K=1,NZ
      NN=N0+K*NYM
      DO 41 J=1,NY
      NNJ=NN+J
      IF (PRCMAP(NNJ).EQ.N) THEN

         IF (J.LT.NY) THEN
            NAEN(1)=PRCMAP(NNJ+1)
         ELSE
            NAEN(1)=N
         ENDIF
         IF (J.GT.1) THEN
            NAEN(2)=PRCMAP(NNJ-1)
         ELSE
            NAEN(2)=N
         ENDIF
         IF (K.LT.NZ) THEN
            NAEN(3)=PRCMAP(NNJ+NYM)
         ELSE
            NAEN(3)=N
         ENDIF
         IF (K.GT.1) THEN
            NAEN(4)=PRCMAP(NNJ-NYM)
         ELSE
            NAEN(4)=N
         ENDIF
         IF (J.LT.NY.AND.K.LT.NZ) THEN
            NAEN(5)=PRCMAP(NNJ+NYM+1)
         ELSE
            NAEN(5)=N
         ENDIF
         IF (J.GT.1.AND.K.LT.NZ) THEN
            NAEN(6)=PRCMAP(NNJ+NYM-1)
         ELSE
            NAEN(6)=N
         ENDIF
         IF (J.LT.NY.AND.K.GT.1) THEN
            NAEN(7)=PRCMAP(NNJ-NYM+1)
         ELSE
            NAEN(7)=N
         ENDIF
         IF (J.GT.1.AND.K.GT.1) THEN
            NAEN(8)=PRCMAP(NNJ-NYM-1)
         ELSE
            NAEN(8)=N
         ENDIF

         DO 42 I=1,8
         IF (NAEN(I).LT.0) NAEN(I)=N
   42    CONTINUE

         DO 43 I=1,7
         DO 43 II=I+1,8
         IF (NAEN(I).EQ.NAEN(II)) NAEN(II)=N
   43    CONTINUE

         DO 44 I=1,8
         IF (NAEN(I).NE.N) NM(N+1)=NM(N+1)+NX
   44    CONTINUE

      ENDIF
   41 CONTINUE

      WRITE (NFOUT,60)
      WRITE (NFOUT,45)
   45 FORMAT(/' ***************  GRID ELEMENTS SENT DURING UPDATE',
     & ' (27 POINT)  ****************'//' PROCESSOR   ELEMENTS')
      I=0
      DO 46 N=1,NUMPRC
      I=I+NM(N)
   46 WRITE (NFOUT,47) N-1,NM(N)
   47 FORMAT(I6,6X,I7)
      WRITE (NFOUT,48) I
   48 FORMAT('   TOTAL',4X,I7)
   60 FORMAT(//)

!bw Added output for node communication stencil
C  COUNT AND PRINT THE NUMBER OF GRID NODES TRANSMITTED BY EACH
C  PROCESSOR DURING AN UPDATE

      DO 30 N=1,NUMPRC
   30 NM(N)=0

      DO 31 M=1,NUMBLK
      CALL BLKDIM (M,NX,NY,NZ,KERR)
      N0N=N0MAPN(M)
      NYMN=NYMAP(M)+1
      DO 31 N=0,NUMPRC-1
      DO 31 K=1,NZ+1
      NN=N0N+K*NYMN
      DO 31 J=1,NY+1
      NNJ=NN+J
      IF (PRCMAPN(NNJ).EQ.N) THEN

         IF (J.LT.NY+1) THEN
            NAEN(1)=PRCMAPN(NNJ+1)
         ELSE
            NAEN(1)=N
         ENDIF
         IF (J.GT.1) THEN
            NAEN(2)=PRCMAPN(NNJ-1)
         ELSE
            NAEN(2)=N
         ENDIF
         IF (K.LT.NZ+1) THEN
            NAEN(3)=PRCMAPN(NNJ+NYMN)
         ELSE
            NAEN(3)=N
         ENDIF
         IF (K.GT.1) THEN
            NAEN(4)=PRCMAPN(NNJ-NYMN)
         ELSE
            NAEN(4)=N
         ENDIF
         IF (J.LT.(NY+1).AND.K.LT.(NZ+1)) THEN
            NAEN(5)=PRCMAPN(NNJ+NYMN+1)
         ELSE
            NAEN(5)=N
         ENDIF
         IF (J.GT.1.AND.K.LT.(NZ+1)) THEN
            NAEN(6)=PRCMAPN(NNJ+NYMN-1)
         ELSE
            NAEN(6)=N
         ENDIF
         IF (J.LT.(NY+1).AND.K.GT.1) THEN
            NAEN(7)=PRCMAPN(NNJ-NYMN+1)
         ELSE
            NAEN(7)=N
         ENDIF
         IF (J.GT.1.AND.K.GT.1) THEN
            NAEN(8)=PRCMAPN(NNJ-NYMN-1)
         ELSE
            NAEN(8)=N
         ENDIF

         DO 32 I=1,8
         IF (NAEN(I).LT.0) NAEN(I)=N
   32    CONTINUE

         DO 33 I=1,7
         DO 33 II=I+1,8
         IF (NAEN(I).EQ.NAEN(II)) NAEN(II)=N
   33    CONTINUE

         DO 34 I=1,8
         IF (NAEN(I).NE.N) NM(N+1)=NM(N+1)+(NX+1)
   34    CONTINUE

      ENDIF
   31 CONTINUE

      WRITE (NFOUT,50)
      WRITE (NFOUT,35)
   35 FORMAT(/' ***************  GRID NODES SENT DURING UPDATE',
     & ' (27 POINT)  ****************'//' PROCESSOR   NODES')
      I=0
      DO 36 N=1,NUMPRC
      I=I+NM(N)
   36 WRITE (NFOUT,37) N-1,NM(N)
   37 FORMAT(I6,6X,I7)
      WRITE (NFOUT,38) I
   38 FORMAT('   TOTAL',4X,I7)
   50 FORMAT(//)

      END

C*********************************************************************
      SUBROUTINE WELLSHR (NW)
C*********************************************************************

C  Sends well location data from the current processor to all other
C  processors.  All processors must call this routine if any processor
C  calls it.  All processors have a complete set of LOCWEL on return.

C  NW = Well number (input, INTEGER)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'
      COMMON /WELSND/ NWS,NES,LW(6,100)
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      NWS=NW
      NES=NUMELE(NW)
      DO 1 K=1,NES
      DO 1 J=1,6
    1 LW(J,K)=LOCWEL(J,K,NW)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     & ', ENTERING SUBROUTINE WELLSHR'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     & ', ENTERING SUBROUTINE WELLSHR'
      ENDIF

C  SEND WELL DATA

      LM=6*NES+2
      DO 2 I=1,NUMPRC
      NT=I-1
      IF (NT.NE.MYPRC) THEN

      CALL MPI_SEND(NWS,LM,MPI_INTEGER,NT,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      ENDIF
    2 CONTINUE

C  RECEIVE WELL DATA

      LM=6*100+2
      DO 4 I=2,NUMPRC

      CALL MPI_RECV(NWS,LM,MPI_INTEGER,MPI_ANY_SOURCE,MSGTAG(MTM),
     & MPI_COMM_WORLD,ISTAT,IERR)

      KK=NUMELET(NWS)
      DO 3 K=1,NES
      KK=KK+1
      DO 3 J=1,6
    3 LOCWEL(J,KK,NWS)=LW(J,K)
    4 NUMELET(NWS)=KK

      END

C*********************************************************************
      SUBROUTINE WELOWN ()
C*********************************************************************

C  Assigns ownership of each well to a single processor

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'
      INTEGER NW(1024),NWP(1024,50)

      DO 1 I=1,NUMPRC
      NW(I)=0
      DO 1 J=1,NUMWEL
    1 NWP(I,J)=0

      DO 2 I=1,NUMPRC
      DO 2 J=1,NUMWEL
      DO 3 K=1,NUMELET(J)
      IF (I.EQ.LOCWEL(6,K,J)+1) THEN
         NWP(I,J)=1
         GO TO 2
      ENDIF
    3 CONTINUE
    2 CONTINUE

      DO 4 J=1,NUMWEL
      NB=10000
      DO 5 I=1,NUMPRC
      IF (NWP(I,J).GT.0.AND.NW(I).LT.NB) NB=I
    5 CONTINUE
      NW(NB)=NW(NB)+1
    4 NWELPRC(J)=NB-1

      END
C*********************************************************************
      SUBROUTINE HISPASS ()
C*********************************************************************

C  Sends well history data to processor 0 for output.
C  All processors must call this routine if any processor calls it.

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE HISPASS'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE HISPASS'
      ENDIF

      ML=100000*18

      IF (MYPRC.EQ.0) THEN

C  RECEIVE AND OUTPUT WELL HISTORY DATA

         DO 1 NP=2,NUMPRC

       CALL MPI_RECV(KNDHIS,ML,MPI_INTEGER,MPI_ANY_SOURCE,
     & MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)
      CALL MPI_GET_COUNT(ISTAT,MPI_CHARACTER,LM,IERR)

         IF (LM.GT.0) THEN

        MS=ISTAT(MPI_SOURCE)
        CALL MPI_RECV(WELHIS,ML,MPI_DOUBLE_PRECISION,MS,
     &  MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

            IF (MYPRC.EQ.0.AND.KHISOUT.NE.2) CALL WELLPRT()
            IF (MYPRC.EQ.0.AND.KHISOUT.NE.1) CALL WELLDSK()
         ENDIF
    1    CONTINUE

      ELSE

C  SEND WELL HISTORY DATA

         IF (MYHIS.EQ.0) THEN
            LM=0
         ELSE
            LM=ML
         ENDIF

      CALL MPI_SEND(KNDHIS,LM,MPI_INTEGER,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

         IF (MYHIS.GT.0) THEN

        CALL MPI_SEND(WELHIS,ML,MPI_DOUBLE_PRECISION,0,MSGTAG(MTM),
     &  MPI_COMM_WORLD,IERR)

         ENDIF
      ENDIF

      END
C*********************************************************************
      SUBROUTINE WELSUM (NW, NVAL, VEC)
C*********************************************************************

C  Forms one or more sums with each processor that has elements of a
C  well contributing to each sum.  This routine is intended primarily
C  for calculating average densities in the wellbore and total well rates.
C  Only processors that share elements of a well will exchange messages.
C  Note that this routine should be called from an executive routine.

C  NW    = Well number (input, INTEGER)

C  NVAL  = Number of values in VEC (input, INTEGER)

C  VEC() = Current processor contributions to sums on input.
C          Sum for all processors on output (REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      REAL*8 VEC(*),VTERM(NVAL)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM)=MSGTAG(MTM)+1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE WELSUM'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE WELSUM'
      ENDIF

C  TEST FOR WELL ELEMENTS ON THIS PROCESSOR

      DO 4 N=1,NUMELE(NW)
      IF (LOCWEL(6,N,NW).EQ.MYPRC) GOTO 5
    4 CONTINUE
      RETURN

C  SEND MESSAGES

    5 N1=NUMELE(NW)+1
      N2=NUMELET(NW)
      NP=MYPRC
      DO 1 N=N1,N2
      IF (LOCWEL(6,N,NW).NE.NP) THEN
         NP=LOCWEL(6,N,NW)
      CALL MPI_SEND(VEC,NVAL,MPI_DOUBLE_PRECISION,NP,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)
      ENDIF
    1 CONTINUE

C  RECEIVE MESSAGES AND SUM VEC

      NP=MYPRC
      DO 2 N=N1,N2
      IF (LOCWEL(6,N,NW).NE.NP) THEN
         NP=LOCWEL(6,N,NW)

      CALL MPI_RECV(VTERM,NVAL,MPI_DOUBLE_PRECISION,
     & MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

         DO 3 J=1,NVAL
    3    VEC(J)=VEC(J)+VTERM(J)
      ENDIF
    2 CONTINUE

      END

C*********************************************************************
      SUBROUTINE WELGET (NVAL, VEC)
C*********************************************************************

C  Sends well data from the processor that owns a well to processor 0.

C  NVAL  = First dimension of VEC (input, INTEGER)

C  VEC(i,j) = Data value i for well j (input and output, REAL*8)

C  NOTE:  MSGTAG is used to identify the well.

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      REAL*8 VEC(NVAL,*)

      MTM=CURRENT_MODEL+1
      IF (MSGTAG(MTM)+NUMWEL.GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM)+1,
     &   ', ENTERING SUBROUTINE WELGET'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM)+1,
     &   ', ENTERING SUBROUTINE WELGET'
      ENDIF

C  LOOP OVER THE WELLS

      DO 1 N=1,NUMWEL
      IF (CURRENT_MODEL.EQ.MODWEL(N)
     &    .OR.FLOWMODEL.EQ.MODWEL(N)
     &   ) THEN
         IF (MYPRC.GT.0) THEN
            IF (MYPRC.EQ.NWELPRC(N)) THEN

         CALL MPI_SEND(VEC(1,N),NVAL,MPI_DOUBLE_PRECISION,0,
     &    MSGTAG(MTM)+N,MPI_COMM_WORLD,IERR)

            ENDIF
         ELSE
            IF (NWELPRC(N).GT.0) THEN

         CALL MPI_RECV(VEC(1,N),NVAL,MPI_DOUBLE_PRECISION,
     &    MPI_ANY_SOURCE,MSGTAG(MTM)+N,MPI_COMM_WORLD,ISTAT,IERR)

            ENDIF
         ENDIF
      ENDIF
    1 CONTINUE
      MSGTAG(MTM)=MSGTAG(MTM)+NUMWEL

      END

C*********************************************************************
      SUBROUTINE WELPUT (NVAL, VEC)
C*********************************************************************

C  Sends well data from processor 0 to processor that owns a well.

C  NVAL  = First dimension of VEC (input, INTEGER)

C  VEC(i,j) = Data value i for well j (input and output, REAL*8)

C  NOTE:  MSGTAG is used to identify the well.

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      REAL*8 VEC(NVAL,*)

      MTM=CURRENT_MODEL+1
      IF (MSGTAG(MTM)+NUMWEL.GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM)+1,
     &   ', ENTERING SUBROUTINE WELPUT'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM)+1,
     &   ', ENTERING SUBROUTINE WELPUT'
      ENDIF

C  LOOP OVER THE WELLS

      DO 1 N=1,NUMWEL
      IF ((CURRENT_MODEL.EQ.MODWEL(N)
     &    .OR.FLOWMODEL.EQ.MODWEL(N)
     &    ).AND. NWELPRC(N).GT.0) THEN
         IF (MYPRC.GT.0) THEN
            IF (MYPRC.EQ.NWELPRC(N)) THEN

            CALL MPI_RECV(VEC(1,N),NVAL,MPI_DOUBLE_PRECISION,
     &                    MPI_ANY_SOURCE,MSGTAG(MTM)+N,
     &                    MPI_COMM_WORLD,ISTAT,IERR)
            ENDIF
         ELSE

         CALL MPI_SEND(VEC(1,N),NVAL,MPI_DOUBLE_PRECISION,
     &                 NWELPRC(N),MSGTAG(MTM)+N,
     &                 MPI_COMM_WORLD,IERR)

         ENDIF
      ENDIF
    1 CONTINUE
      MSGTAG(MTM)=MSGTAG(MTM)+NUMWEL

      END
C*********************************************************************
      SUBROUTINE RSSR4 (N, RBUF, NERR)
C*********************************************************************

C  Sends grid-element array to processor 0 for restart output

C  N = Number of values in RBUF (input, INTEGER)

C  RBUF() = Data to be transmitted (input, REAL*4)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'mpif.h'

      REAL*4 RBUF(*)

      MTM=CURRENT_MODEL+1

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RSSR4'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RSSR4'
      ENDIF

      CALL MPI_SEND(RBUF,N,MPI_REAL,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      IF (IERR.GT.0) THEN
         WRITE (NFOUT,*) 'ERROR # 204 - RESTART FAILURE IN RSSR4 (',
     &   IERR,')'
         NERR=NERR+1
         RETURN
      ENDIF

      END
C*********************************************************************
      SUBROUTINE RSSR8 (N, RBUF, NERR)
C*********************************************************************

C  Sends grid-element array to processor 0 for restart output

C  N = Number of values in RBUF (input, INTEGER)

C  RBUF() = Data to be transmitted (input, REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'mpif.h'

      REAL*8 RBUF(*)

      MTM=CURRENT_MODEL+1

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RSSR8'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RSSR8'
      ENDIF

      CALL MPI_SEND(RBUF,N,MPI_DOUBLE_PRECISION,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      IF (IERR.GT.0) THEN
         WRITE (NFOUT,*) 'ERROR # 204 - RESTART FAILURE IN RSSR8 (',
     &   IERR,')'
         NERR=NERR+1
         RETURN
      ENDIF

      END
C*********************************************************************
      SUBROUTINE RSSI4 (N, IRBUF, NERR)
C*********************************************************************

C  Sends grid-element array to processor 0 for restart output

C  N = Number of values in RBUF (input, INTEGER)

C  IRBUF() = Data to be transmitted (input, INTEGER)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'mpif.h'

      INTEGER IRBUF(*)

      MTM=CURRENT_MODEL+1

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RSSI4'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RSSI4'
      ENDIF

      CALL MPI_SEND(IRBUF,N,MPI_INTEGER,0,MSGTAG(MTM),
     & MPI_COMM_WORLD,IERR)

      IF (IERR.GT.0) THEN
         WRITE (NFOUT,*) 'ERROR # 204 - RESTART FAILURE IN RSSI4 (',
     &   IERR,')'
         NERR=NERR+1
         RETURN
      ENDIF

      END
C*********************************************************************
      SUBROUTINE RRR4 (NERR)
C*********************************************************************

C  Processor 0 receives REAL grid-element array data for restart output

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'output.h'
      INCLUDE 'layout.h'
      INCLUDE 'restc.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      MTM=CURRENT_MODEL+1

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RRR4'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RRR4'
      ENDIF

      NBU=3*480
      NP=0
      IF (CURRENT_MODEL.EQ.0) THEN
         NEEX=NETOTR0
      ELSE
         NEEX=NETOTR(CURRENT_MODEL)
      ENDIF

    1 IF (NEORS.GE.NEEX) RETURN

      CALL POLL(MSGTAG(MTM),NP)

      CALL MPI_RECV(RBUFR4,NBU,MPI_REAL,MPI_ANY_SOURCE,MSGTAG(MTM),
     & MPI_COMM_WORLD,ISTAT,IERR)

      IF (IERR.GT.0) THEN
         WRITE (*,*) 'ERROR # 203 - RESTART FAILURE IN RRR4 (',
     &   IERR,')'
         NERR=NERR+1
         RETURN
      ENDIF

      NB=RBUFR4(1)+.01
      JA=RBUFR4(2)+.01
      JB=RBUFR4(3)+.01
      KK=RBUFR4(4)+.01
      ME=RBUFR4(5)+.01
      MM=RBUFR4(6)+.01
      NEORS=NEORS+MM-6
      IF (FORMOUT) THEN
         WRITE (NFROUT,2) NB,JA,JB,KK,ME,MM-6
    2    FORMAT(6I8)
         WRITE (NFROUT,3) (RBUFR4(NN),NN=7,MM)
    3    FORMAT(8G15.8)
      ELSE
         WRITE (NFROUT) NB,JA,JB,KK,ME,MM-6
         WRITE (NFROUT) (RBUFR4(NN),NN=7,MM)
      ENDIF

      GO TO 1

      END
C*********************************************************************
      SUBROUTINE RRR8 (NERR)
C*********************************************************************

C  Processor 0 receives REAL*8 grid-element array data for restart output

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'output.h'
      INCLUDE 'layout.h'
      INCLUDE 'restc.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      MTM=CURRENT_MODEL+1

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RRR8'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE RRR8'
      ENDIF

      NBU=3*480
      NP=0
      IF (CURRENT_MODEL.EQ.0) THEN
         NEEX=NETOTR0
      ELSE
         NEEX=NETOTR(CURRENT_MODEL)
      ENDIF

    1 IF (NEORS.GE.NEEX) RETURN

      CALL POLL(MSGTAG(MTM),NP)

      CALL MPI_RECV(RBUFR8,NBU,MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,
     & MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

      IF (IERR.GT.0) THEN
         WRITE (*,*) 'ERROR # 203 - RESTART FAILURE IN RRR8 (',
     &   IERR,')'
         NERR=NERR+1
         RETURN
      ENDIF

      NB=RBUFR8(1)+.01
      JA=RBUFR8(2)+.01
      JB=RBUFR8(3)+.01
      KK=RBUFR8(4)+.01
      ME=RBUFR8(5)+.01
      MM=RBUFR8(6)+.01
      NEORS=NEORS+MM-6
      IF (FORMOUT) THEN
         WRITE (NFROUT,2) NB,JA,JB,KK,ME,MM-6
    2    FORMAT(6I8)
         WRITE (NFROUT,3) (RBUFR8(NN),NN=7,MM)
    3    FORMAT(5G23.16)
      ELSE
         WRITE (NFROUT) NB,JA,JB,KK,ME,MM-6
         WRITE (NFROUT) (RBUFR8(NN),NN=7,MM)
      ENDIF

      GO TO 1

      END
C*********************************************************************
      SUBROUTINE RRI4 (NERR)
C*********************************************************************

C  Processor 0 receives INTEGER grid-element array data for restart output

C  Note: This routine is also used to output LOGICAL data

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'output.h'
      INCLUDE 'layout.h'
      INCLUDE 'restc.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      MTM=CURRENT_MODEL+1

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     & ', ENTERING SUBROUTINE RRI4'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     & ', ENTERING SUBROUTINE RRI4'
      ENDIF

      NBU=3*480
      NP=0
      IF (CURRENT_MODEL.EQ.0) THEN
         NEEX=NETOTR0
      ELSE
         NEEX=NETOTR(CURRENT_MODEL)
      ENDIF

    1 IF (NEORS.GE.NEEX) RETURN

      CALL POLL(MSGTAG(MTM),NP)

      CALL MPI_RECV(RBUFI4,NBU,MPI_INTEGER,MPI_ANY_SOURCE,
     & MSGTAG(MTM),MPI_COMM_WORLD,ISTAT,IERR)

      IF (IERR.GT.0) THEN
         WRITE (*,*) 'ERROR # 203 - RESTART FAILURE IN RRI4 (',
     &   IERR,')'
         NERR=NERR+1
         RETURN
      ENDIF

      MM=RBUFI4(6)
      NEORS=NEORS+MM-6
      IF (FORMOUT) THEN
         WRITE (NFROUT,2) (RBUFI4(NN),NN=1,5),MM-6
    2    FORMAT(6I8)
         WRITE (NFROUT,3) (RBUFI4(NN),NN=7,MM)
    3    FORMAT(12I9)
      ELSE
         WRITE (NFROUT) (RBUFI4(NN),NN=1,5),MM-6
         WRITE (NFROUT) (RBUFI4(NN),NN=7,MM)
      ENDIF

      GO TO 1

      END
C*********************************************************************
      SUBROUTINE TYPEOUT (A,M)
C*********************************************************************

C  Transfers character strings to processor 0 for output to NFOUT.
C  Output will occur only if the number of substrings (M) is greater
C  than 0.  Care must be exercised to insure processor 0 calls to this
C  routine are matched by one (and only one) call by some other processor.
C  This routine has exclusive use of message tag 900.  Processor 0 can not
C  use this routine to output its own data.  Each substring must be 80
C  characters in length.  Trailing blanks in each substring will be discarded.

C  A = Character string (superstring) to be printed (input, CHARACTER*1)
C      Processor 0 uses this array to receive data.

C  M = Number of substrings (number of lines) (input, INTEGER)
C      (Max number of substrings on processor 0)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      CHARACTER*1 A(*)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,' ENTERING SUBROUTINE TYPEOUT'
         WRITE (*,*)' PROC',MYPRC,' ENTERING SUBROUTINE TYPEOUT'
      ENDIF

      L=80
      MT=900
      LS=L*M

      IF (MYPRC.GT.0) THEN

C  SEND STRINGS

         IF (LS.EQ.0) LS=1

      CALL MPI_SEND(A,LS,MPI_CHARACTER,0,MT,MPI_COMM_WORLD,IERR)

      ELSE

C  RECEIVE AND PRINT STRINGS

      CALL MPI_RECV(A,LS,MPI_CHARACTER,MPI_ANY_SOURCE,MT,
     & MPI_COMM_WORLD,ISTAT,IERR)
      CALL MPI_GET_COUNT(ISTAT,MPI_CHARACTER,LO,IERR)

         MM=LO/L
         J2=0
         DO 1 I=1,MM
         J1=J2+1
         J2=J2+L
         DO 3 J=J2,J1,-1
         J3=J
         IF (A(J).NE.' ') GO TO 1
    3    CONTINUE
    1    WRITE (NFOUT,2) (A(J),J=J1,J3)
    2    FORMAT(80A1)

      ENDIF

      END
C*********************************************************************
      SUBROUTINE POLL (MTAG,NP)
C*********************************************************************

C  Constrains the rate at which data is transmitted to processor 0.
C  If no messages with the tag MTAG are waiting to be received at
C  processor 0 then NP is stepped by 1 and processor NP is sent a message
C  that releases it from a wait state.  All processors must call this
C  routine if any processor calls it.

C  MTAG = Message tag to be tested at processor 0 (input, INTEGER)
C         This tag is also used to activate individual processors.

C  NP   = Processor number currently sending (input and output, INTEGER)
C         Normally, processor 0 should initialize NP with a value of 0.
C         Other processors do not use the argument.

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)
      LOGICAL FLG

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MTAG,
     &   ', ENTERING SUBROUTINE POLL'
         WRITE (*,*)' PROC',MYPRC,', TAG',MTAG,
     &   ', ENTERING SUBROUTINE POLL'
      ENDIF

      IF (NUMPRC.LT.2) RETURN

      IF (MYPRC.EQ.0) THEN

         IF (NUMPRC.LE.NP+1) RETURN

C  TEST FOR MTAG MESSAGES ON PROCESSOR 0

      CALL MPI_IPROBE(MPI_ANY_SOURCE,MTAG,MPI_COMM_WORLD,FLG,
     & ISTAT,IERR)

         IF (FLG) RETURN

C  ACTIVATE NEXT PROCESSOR FROM PROCESSOR 0

         DO 1 I=1,NUMPRC
         NP=NP+1
         IF (NUMPRC.LT.NP+1) RETURN
         IF (MODPROC(CURRENT_MODEL,NP+1)) GO TO 2
    1    CONTINUE
         RETURN
    2    CONTINUE

      CALL MPI_SEND(NP,1,MPI_INTEGER,NP,MTAG,MPI_COMM_WORLD,IERR)

      ELSE

C  WAIT FOR ACTIVATION

      CALL MPI_RECV(LL,1,MPI_INTEGER,0,MTAG,MPI_COMM_WORLD,ISTAT,
     & IERR)

      ENDIF

      END
C*********************************************************************
      SUBROUTINE SETTAGS ()
C*********************************************************************

C  INITIALIZE MESSAGE TAG SYSTEM

C*********************************************************************
      INCLUDE 'control.h'

      CURRENT_MODEL=0

      DO 1 I=1,19+1
      MSGTAG(I)=-999
      MSGTAG1(I)=-999
    1 MSGTAG2(I)=-999

      NM=1
C       NM=NM+1
C       NM=NM+1
C         NM=NM+1
C         NM=NM+1
C       NM=NM+1
C       NM=NM+1
C       NM=NM+1
C       NM=NM+1
C       NM=NM+1
C      NM=NM+1
C      NM=NM+1
C       NM=NM+1
       NM=NM+1
C       NM=NM+1
       NM=NM+1

      MD=(16000-1000)/NM
      M=1000

      MSGTAG(1)=M
      MSGTAG1(1)=M
      M=M+MD
      MSGTAG2(1)=M-1

C       MSGTAG(1+1)=M
C       MSGTAG1(1+1)=M
C       M=M+MD
C       MSGTAG2(1+1)=M-1

C       MSGTAG(2+1)=M
C       MSGTAG1(2+1)=M
C       M=M+MD
C       MSGTAG2(2+1)=M-1

C         MSGTAG(3+1)=M
C         MSGTAG1(3+1)=M
C         M=M+MD
C         MSGTAG2(3+1)=M-1

C         MSGTAG(4+1)=M
C         MSGTAG1(4+1)=M
C         M=M+MD
C         MSGTAG2(4+1)=M-1

C       MSGTAG(5+1)=M
C       MSGTAG1(5+1)=M
C       M=M+MD
C       MSGTAG2(5+1)=M-1

C       MSGTAG(19+1)=M
C       MSGTAG1(19+1)=M
C       M=M+MD
C       MSGTAG2(19+1)=M-1

C       MSGTAG(18+1)=M
C       MSGTAG1(18+1)=M
C       M=M+MD
C       MSGTAG2(18+1)=M-1

C       MSGTAG(6+1)=M
C       MSGTAG1(6+1)=M
C       M=M+MD
C       MSGTAG2(6+1)=M-1

C       MSGTAG(7+1)=M
C       MSGTAG1(7+1)=M
C       M=M+MD
C       MSGTAG2(7+1)=M-1

C      MSGTAG(9+1)=M
C      MSGTAG1(9+1)=M
C      M=M+MD
C      MSGTAG2(9+1)=M-1

C      MSGTAG(13+1)=M
C      MSGTAG1(13+1)=M
C      M=M+MD
C      MSGTAG2(13+1)=M-1

C       MSGTAG(14+1)=M
C       MSGTAG1(14+1)=M
C       M=M+MD
C       MSGTAG2(14+1)=M-1

       MSGTAG(15+1)=M
       MSGTAG1(15+1)=M
       M=M+MD
       MSGTAG2(15+1)=M-1

C       MSGTAG(16+1)=M
C       MSGTAG1(16+1)=M
C       M=M+MD
C       MSGTAG2(16+1)=M-1

       MSGTAG(17+1)=M
       MSGTAG1(17+1)=M
       M=M+MD
       MSGTAG2(17+1)=M-1

      END
C*********************************************************************
      SUBROUTINE ELEGET (NW, NVAL, VEC)
C*********************************************************************

C  Gathers well element data onto processor assigned to a well
C  Only processors that share elements of a well will exchange messages.

C  NW    = Well number (input, INTEGER)

C  NVAL  = Number of values per element in VEC (input, INTEGER)

C  VEC() = Current processor element values on input.
C          External processor element values on output (REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      REAL*8 VEC(*),VTERM(NVAL*100+1)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM) = MSGTAG(MTM) + 1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE ELEGET'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE ELEGET'
      ENDIF

C  TEST FOR WELL ELEMENTS ON THIS PROCESSOR

      IF(NUMELE(NW).EQ.0) RETURN
      IF(NUMELE(NW).EQ.NUMELET(NW)) RETURN

C  SEND MESSAGES TO PROCESSOR ASSIGNED TO WELL

      NP = NWELPRC(NW)
      IF(MYPRC.NE.NP) THEN
         N = NVAL*NUMELE(NW)
         VTERM(1)=MYPRC
         DO 1 I=1,N
            VTERM(I+1)=VEC(I)
    1    CONTINUE

         CALL MPI_SEND(VTERM,N+1,MPI_DOUBLE_PRECISION,NP,
     &                 MSGTAG(MTM),MPI_COMM_WORLD,IERR)

      ELSE

C  RECEIVE MESSAGES ON PROCESSOR ASSIGNED TO WELL
C  ASSUMES ELEMENTS ARE CONTIGUOUS FOR EACH PROCESSOR

         N1=NUMELE(NW)+1
         N2=NUMELET(NW)

C        FIND NUMBER OF PROCESSORS SENDING ELEMENTS FOR WELL
         NUMP=0
         J = -1
         DO 2 I=N1,N2
            IF(LOCWEL(6,I,NW).NE.J) THEN
               NUMP = NUMP + 1
               J = LOCWEL(6,I,NW)
            ENDIF
    2    CONTINUE

         DO 10 M=1,NUMP

         CALL MPI_RECV(VTERM,NVAL*100+1,MPI_DOUBLE_PRECISION,
     &                 MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,
     &                 ISTAT,IERR)

C           ELEMENTS RECEIVED FROM PROCESSOR NP
            NP = VTERM(1)+.01

C           FIND BOUNDS FOR PROCESSOR ELEMENTS (I1 - I2)
            DO 3 I=N1,N2
               IF(LOCWEL(6,I,NW).EQ.NP) THEN
                  I1=I
                  GO TO 4
               ENDIF
    3       CONTINUE
    4       DO 5 I=N2,I1,-1
               IF(LOCWEL(6,I,NW).EQ.NP) THEN
                  I2=I
                  GO TO 6
               ENDIF
    5       CONTINUE

    6       J=NVAL*(I1-N1)
            DO 7 K=1,NVAL*(I2-I1+1)
               VEC(J+K)=VTERM(K+1)
    7       CONTINUE

   10    CONTINUE

      ENDIF

      END
C*********************************************************************
      SUBROUTINE ELEPUT (NW, NVAL, VEC)
C*********************************************************************

C  Distributes well element data from processor assigned to a well
C  to processors containing some elements of the well
C  Only processors that share elements of a well will exchange messages.

C  NW    = Well number (input, INTEGER)

C  NVAL  = Number of values per element in VEC (input, INTEGER)

C  VEC() = Current processor element values on input.
C          External processor element values on output (REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      REAL*8 VEC(*),VTERM(NVAL*100+1)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM) = MSGTAG(MTM) + 1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE ELEPUT'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE ELEPUT'
      ENDIF

C  TEST FOR WELL ELEMENTS ON THIS PROCESSOR

      IF(NUMELE(NW).EQ.0) RETURN
      IF(NUMELE(NW).EQ.NUMELET(NW)) RETURN

C  RECEIVE MESSAGE FROM PROCESSOR ASSIGNED TO WELL

      NP = NWELPRC(NW)
      IF(MYPRC.NE.NP) THEN

         CALL MPI_RECV(VEC,NVAL*NUMELE(NW),MPI_DOUBLE_PRECISION,
     &                 MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,
     &                 ISTAT,IERR)

      ELSE

C  SEND MESSAGES TO PROCESSORS WITH ELEMENTS IN WELL
C  ASSUMES ELEMENTS ARE CONTIGUOUS FOR EACH PROCESSOR

         N1=NUMELE(NW)+1
         N2=NUMELET(NW)

C        FIND NUMBER OF PROCESSORS WITH ELEMENTS IN WELL
         NUMP=0
         J = -1
         DO 2 I=N1,N2
            IF(LOCWEL(6,I,NW).NE.J) THEN
               NUMP = NUMP + 1
               J = LOCWEL(6,I,NW)
            ENDIF
    2    CONTINUE

         NP = -1
         N1S = N1
         DO 10 M=1,NUMP

C           FIND BOUNDS FOR PROCESSOR ELEMENTS (I1 - I2)
            DO 3 I=N1S,N2
               IF(LOCWEL(6,I,NW).NE.NP) THEN
                  I1=I
                  NP = LOCWEL(6,I,NW)
                  GO TO 4
               ENDIF
    3       CONTINUE
    4       DO 5 I=N2,I1,-1
               IF(LOCWEL(6,I,NW).EQ.NP) THEN
                  I2=I
                  GO TO 6
               ENDIF
    5       CONTINUE

    6       J=NVAL*(I1-N1)
            N=NVAL*(I2-I1+1)
            DO 7 K=1,N
               VTERM(K)=VEC(J+K)
    7       CONTINUE


           CALL MPI_SEND(VTERM,N,MPI_DOUBLE_PRECISION,NP,
     &                   MSGTAG(MTM),MPI_COMM_WORLD,IERR)

            N1S=I2+1
   10    CONTINUE

      ENDIF

      END
C*********************************************************************
      SUBROUTINE WELDIST (NW, NVAL, VEC)
C*********************************************************************

C  Distributes well data from processor assigned to a well
C  to processors containing some elements of the well
C  Only processors that share elements of a well will exchange messages.

C  NW    = Well number (input, INTEGER)

C  NVAL  = Number of values in VEC (input, INTEGER)

C  VEC() = Current processor values on input.
C          External processor values on output (REAL*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      INCLUDE 'mpif.h'
      INTEGER ISTAT(MPI_STATUS_SIZE)

      REAL*8 VEC(*)

      MTM=CURRENT_MODEL+1
      MSGTAG(MTM) = MSGTAG(MTM) + 1
      IF (MSGTAG(MTM).GT.MSGTAG2(MTM)) MSGTAG(MTM)=MSGTAG1(MTM)

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE WELDIST'
         WRITE (*,*)' PROC',MYPRC,', TAG',MSGTAG(MTM),
     &   ', ENTERING SUBROUTINE WELDIST'
      ENDIF

C  TEST FOR WELL ELEMENTS ON THIS PROCESSOR

      IF(NUMELE(NW).EQ.0) RETURN
      IF(NUMELE(NW).EQ.NUMELET(NW)) RETURN

C  RECEIVE MESSAGE FROM PROCESSOR ASSIGNED TO WELL

      NP = NWELPRC(NW)
      IF(MYPRC.NE.NP) THEN

         CALL MPI_RECV(VEC,NVAL,MPI_DOUBLE_PRECISION,
     &                 MPI_ANY_SOURCE,MSGTAG(MTM),MPI_COMM_WORLD,
     &                 ISTAT,IERR)


      ELSE

C  SEND MESSAGES TO PROCESSORS WITH ELEMENTS IN WELL
C  ASSUMES ELEMENTS ARE CONTIGUOUS FOR EACH PROCESSOR

         NP = -1
         DO 2 I=NUMELE(NW)+1,NUMELET(NW)
            IF(LOCWEL(6,I,NW).NE.NP) THEN
               NP = LOCWEL(6,I,NW)

           CALL MPI_SEND(VEC,NVAL,MPI_DOUBLE_PRECISION,NP,
     &                   MSGTAG(MTM),MPI_COMM_WORLD,IERR)

            ENDIF
    2    CONTINUE

      ENDIF

      END
