C  OWELL.DF - OUTPUT WELL DATA

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE WELLOUT  (NW,KD,VAL)
C  SUBROUTINE WELDUMP  ()
C  SUBROUTINE WELLPRT  ()
C  SUBROUTINE WELLDSK  ()

C  CODE HISTORY:

C  JOHN WHEELER      3/10/97    ALPHA CODE
C  JOHN WHEELER     11/10/98    NEW WELL RATE UNITS

C***********************************************************************
      SUBROUTINE WELLOUT (NW,KD,VAL)
C***********************************************************************

C  Routine collects transient well data

C  NW  = Well number (input, INTEGER)

C  Kd  = Data identification key (input, INTEGER)

C  VAL = Data value to be output (input, REAL*8)

C***********************************************************************

      INCLUDE 'control.h'
      INCLUDE 'wells.h'
      REAL*8 VAL

      IF (KHISOUT.EQ.0) RETURN

      MYHIS=1
      NHISQ=NHISQ+1
      IF((NHISQ > 100000).AND.LEVELC) THEN
         WRITE(NFOUT,'(/,A,I4,A,I4)')
     &        'ERROR: IN WELLOUT NHISQ = ',NHISQ,' > $MXHISQ = ',
     &        100000
      ENDIF
      IF((NHISUSE > 18).AND.LEVELC) THEN
         WRITE(NFOUT,'(/,A,I4,A,I4)')
     &        'ERROR: IN WELLOUT NHISUSE = ',NHISUSE,' > $MXHIST = ',
     &        18
      ENDIF
      IF((NHISQ > 100000).OR.(NHISUSE > 18)) STOP 13
      KNDHIS(NHISQ,NHISUSE)=NW+(50+1)*KD
      WELHIS(NHISQ,NHISUSE)=VAL

      END

C***********************************************************************
      SUBROUTINE WELDUMP ()
C***********************************************************************

C  Routine directs output of transient well data

C***********************************************************************

      INCLUDE 'control.h'
      INCLUDE 'wells.h'
      INCLUDE 'blkary.h'

      IF (MYPRC.EQ.0.AND.KHISOUT.NE.2) THEN
         WRITE (NFOUT,*)
         TITU='******'
         CALL PRTTIT(TITU)
         TITU='WELL RATES'
         CALL PRTTIT(TITU)
         CALL WELLPRT()
         CALL WELLPRTC()
      ENDIF

      IF (MYPRC.EQ.0.AND.KHISOUT.NE.1) CALL WELLDSK()

      CALL HISPASS()

      MYHIS=0
      NHISUSE=0
      DO 1 I=1,100000
      DO 1 J=0,18
    1 KNDHIS(I,J)=0

      END

C***********************************************************************
      SUBROUTINE WELLPRT ()
C***********************************************************************

C  Routine sorts and prints transient well data

C***********************************************************************

      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      REAL*4 T(18),V(18)
      INTEGER KH(100000,18),NWT
      CHARACTER*1 WN(40,50),TH(40,34),CL,SP
      EQUIVALENCE (WELNAM,WN),(TITHIS,TH)
      DATA CL/':'/,SP/' '/

C  COPY HISTORY ID ARRAY SO AS NOT TO DESTROY

      DO 1 I=1,100000
      DO 1 J=1,18
    1 KH(I,J)=KNDHIS(I,J)

C  PICK OUT DATA OF A COMMON TYPE AND PRINT IT

      DO 2 J=1,18
      DO 2 I=1,100000
      IF (KH(I,J).GT.0) THEN
         N=0
         K=KH(I,J)
         DO 3 JJ=J,18
         DO 4 II=1,100000
         IF (KH(II,JJ).EQ.K) THEN
            N=N+1
            T(N)=TIMHIS(JJ)
            V(N)=WELHIS(II,JJ)
            KH(II,JJ)=0
            GO TO 3
         ENDIF
    4    CONTINUE
    3    CONTINUE
         IF (LEVELC) THEN
            NW=MOD(K,50+1)
            KD=K/(50+1)

            IF(WOUTFLG(NW)) THEN
            DO 6 L=40,1,-1
            IF (WN(L,NW).NE.' ') THEN
               LN=L
               GO TO 7
            ENDIF
    6       CONTINUE
            LN=0

    7       DO 8 L=40,1,-1
            IF (TH(L,KD).NE.' ') THEN
               LT=L
               GO TO 9
            ENDIF
    8       CONTINUE
            LT=0

    9       WRITE (NFOUT,5) (WN(L,NW),L=1,LN),CL,SP,(TH(L,KD),L=1,LT)
csgt    5       FORMAT(/1X,119A1)
    5       FORMAT(/1X,150A1)
            N1=1
   10       N2=N1+5
            IF (N2.GT.N) N2=N
            WRITE (NFOUT,11) (T(L),L=N1,N2)
            WRITE (NFOUT,12) (V(L),L=N1,N2)
   11       FORMAT(' TIME  ',6G15.5)
   12       FORMAT(' VALUE ',6G15.5)
            N1=N2+1
            IF (N1.LE.N) GO TO 10
            ENDIF

         ENDIF
      ENDIF
    2 CONTINUE

      END

C***********************************************************************
      SUBROUTINE WELLDSK ()
C***********************************************************************

C  Routine sorts transient well data AND OUTPUTS IT TO DISK

C***********************************************************************

      INCLUDE 'control.h'
      INCLUDE 'wells.h'

      REAL*4 T(18),V(18)
c     &      ,WI(18),WP(18),GI(18)
c     &      ,GP(18),OP(18),OI(18),SWI,SWP,SGI,SGP,SOP,SOI
      INTEGER KH(100000,18)
      CHARACTER*1 WN(40,50),TH(40,34),CL,SP
      EQUIVALENCE (WELNAM,WN),(TITHIS,TH)
      DATA CL/':'/,SP/' '/

C  COPY HISTORY ID ARRAY SO AS NOT TO DESTROY

      DO 1 I=1,100000
      DO 1 J=1,18
    1 KH(I,J)=KNDHIS(I,J)

C  PICK OUT DATA OF A COMMON TYPE AND OUTPUT IT

      DO 2 J=1,18
      DO 2 I=1,100000
      IF (KH(I,J).GT.0) THEN
         N=0
         K=KH(I,J)
         DO 3 JJ=J,18
         DO 4 II=1,100000
         IF (KH(II,JJ).EQ.K) THEN
            N=N+1
            T(N)=TIMHIS(JJ)
            V(N)=WELHIS(II,JJ)
            KH(II,JJ)=0
            GO TO 3
         ENDIF
    4    CONTINUE
    3    CONTINUE
         IF (LEVELC) THEN
            NW=MOD(K,50+1)
            KD=K/(50+1)

            IF(WOUTFLG(NW)) THEN
            DO 16 L=40,1,-1
            IF (WN(L,NW).NE.' ') THEN
               LN=L
               GO TO 17
            ENDIF
   16       CONTINUE
            LN=0

   17       DO 18 L=40,1,-1
            IF (TH(L,KD).NE.' ') THEN
               LT=L
               GO TO 19
            ENDIF
   18       CONTINUE
            LT=0

   19       WRITE (NFWELL,5) (WN(L,NW),L=1,LN),CL,SP,(TH(L,KD),L=1,LT)
    5       FORMAT(119A1)
            WRITE (NFWELL,6) NW,KD,N
    6       FORMAT(3I5)
            WRITE (NFWELL,7) (T(L),L=1,N)
            WRITE (NFWELL,7) (V(L),L=1,N)
    7       FORMAT(6G15.5)
            ENDIF

         ENDIF
      ENDIF
    2 CONTINUE

      END


C***********************************************************************
      SUBROUTINE WELLPRTC ()
C***********************************************************************

C  Routine sorts and prints transient well data

C***********************************************************************

      INCLUDE 'control.h'
      INCLUDE 'utldat.h'
      INCLUDE 'wells.h'

      REAL*4 T(0:18),V(0:18)
     &      ,WI(18),WP(18),GI(18)
     &      ,GP(18),OP(18),OI(18)
C     &      ,XV(18,2*(13+1))
C     &      ,XV(18,2*(13+1))
      INTEGER KH(100000,0:18),NWT
      CHARACTER*1 WN(40,50),THC(40,34),CL,SP
      EQUIVALENCE (WELNAM,WN),(TITHISC,THC)
      DATA CL/':'/,SP/' '/

C  COPY HISTORY ID ARRAY SO AS NOT TO DESTROY

      DO 1 I=1,100000
      DO 1 J=0,18
    1 KH(I,J)=KNDHIS(I,J)

C  PICK OUT DATA OF A COMMON TYPE AND PRINT IT

      DO 2 J=1,18
      DO 2 I=1,100000
      IF (KH(I,J).GT.0) THEN
         N=0
         K=KH(I,J)
         DO 3 JJ=J,18
         DO 4 II=1,100000
         IF (KH(II,JJ).EQ.K) THEN
            KOLD=KNDHIS(II,JJ-1)
            IF(KOLD.EQ.K) THEN
               T(N)=TIMHIS(JJ-1)
               V(N)=WELHIS(II,JJ-1)
            ELSE
               T(N)=TIMHIS(JJ-1)
               V(N)=0.0D0
            ENDIF
            N=N+1
            KD=K/(50+1)
            NW=MOD(K,50+1)
            T(N)=TIMHIS(JJ)
            V(N)=WELHIS(II,JJ)
            IF(KD.EQ.1) THEN
                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
                   SWIT=SWIT+V(N)*(T(N)-T(N-1))
                   SWIC(NW)=SWIC(NW)+V(N)*(T(N)-T(N-1))
                ELSE
                   SWIT=SWIT+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                   SWIC(NW)=SWIC(NW)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                ENDIF
                WI(N)=SWIC(NW)
            ELSEIF(KD.EQ.2) THEN
                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
                   SOPT=SOPT+V(N)*(T(N)-T(N-1))
                   SOPC(NW)=SOPC(NW)+V(N)*(T(N)-T(N-1))
                ELSE
                   SOPT=SOPT+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                   SOPC(NW)=SOPC(NW)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                ENDIF
                OP(N)=SOPC(NW)
            ELSEIF(KD.EQ.3) THEN
                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
                   SWPT=SWPT+V(N)*(T(N)-T(N-1))
                   SWPC(NW)=SWPC(NW)+V(N)*(T(N)-T(N-1))
                ELSE
                   SWPT=SWPT+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                   SWPC(NW)=SWPC(NW)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                ENDIF
                WP(N)=SWPC(NW)
            ELSEIF(KD.EQ.8) THEN
                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
                   SGIT=SGIT+V(N)*(T(N)-T(N-1))
                   SGIC(NW)=SGIC(NW)+V(N)*(T(N)-T(N-1))
                ELSE
                   SGIT=SGIT+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                   SGIC(NW)=SGIC(NW)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                ENDIF
                GI(N)=SGIC(NW)
            ELSEIF(KD.EQ.9) THEN
                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
                   SOIT=SOIT+V(N)*(T(N)-T(N-1))
                   SOIC(NW)=SOIC(NW)+V(N)*(T(N)-T(N-1))
                ELSE
                   SOIT=SOIT+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                   SOIC(NW)=SOIC(NW)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                ENDIF
                OI(N)=SOIC(NW)
            ELSEIF(KD.EQ.4) THEN
                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
                   SGPT=SGPT+V(N)*(T(N)-T(N-1))
                   SGPC(NW)=SGPC(NW)+V(N)*(T(N)-T(N-1))
                ELSE
                   SGPT=SGPT+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                   SGPC(NW)=SGPC(NW)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
                ENDIF
                GP(N)=SGPC(NW)
C            ELSEIF(KD.GT.9) THEN
C                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
C                   SXT(KD-9)=SXT(KD-9)+V(N)*(T(N)-T(N-1))
C                   SXC(KD-9,NW)=SXC(KD-9,NW)+V(N)*(T(N)-T(N-1))
C                ELSE
C                   SXT(KD-9)=SXT(KD-9)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
C                   SXC(KD-9,NW)=SXC(KD-9,NW)
C     &                         +0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
C                ENDIF
C                XV(N,KD-9)=SXC(KD-9,NW)

C            ELSEIF(KD.GT.9) THEN
C                IF(INTRPS(NTABPQ(NW)).EQ.0) THEN
C                   SXT(KD-9)=SXT(KD-9)+V(N)*(T(N)-T(N-1))
C                   SXC(KD-9,NW)=SXC(KD-9,NW)+V(N)*(T(N)-T(N-1))
C                ELSE
C                   SXT(KD-9)=SXT(KD-9)+0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
C                   SXC(KD-9,NW)=SXC(KD-9,NW)
C     &                         +0.5D0*(V(N)+V(N-1))*(T(N)-T(N-1))
C                ENDIF
C                XV(N,KD-9)=SXC(KD-9,NW)

            ENDIF
            KH(II,JJ)=0
            GO TO 3
         ENDIF
    4    CONTINUE
    3    CONTINUE
         IF (LEVELC) THEN
            NW=MOD(K,50+1)
            KD=K/(50+1)

            IF(WOUTFLG(NW).AND.(KD.NE.5).AND.(KD.NE.6).AND.(KD.NE.7))
     &      THEN
               DO 6 L=40,1,-1
                  IF (WN(L,NW).NE.' ') THEN
                     LN=L
                     GO TO 7
                  ENDIF
    6          CONTINUE
               LN=0

    7          DO 8 L=40,1,-1
                  IF (THC(L,KD).NE.' ') THEN
                     LT=L
                     GO TO 9
                  ENDIF
    8          CONTINUE
               LT=0

    9          WRITE (NWFCUM,5) (WN(L,NW),L=1,LN),CL,SP,
     &                          (THC(L,KD),L=1,LT)
    5          FORMAT(150A1)
               WRITE (NWFCUM,10) NW,KD,N
   10          FORMAT(3I5)

               IF(KD.EQ.1) THEN
                  WRITE(NWFCUM,11) (T(L),L=1,N)
                  WRITE(NWFCUM,11) (WI(L),L=1,N)
               ELSEIF(KD.EQ.2) THEN
                  WRITE(NWFCUM,11) (T(L),L=1,N)
                  WRITE(NWFCUM,11) (OP(L),L=1,N)
               ELSEIF(KD.EQ.3) THEN
                  WRITE(NWFCUM,11) (T(L),L=1,N)
                  WRITE(NWFCUM,11) (WP(L),L=1,N)
               ELSEIF(KD.EQ.8) THEN
                  WRITE(NWFCUM,11) (T(L),L=1,N)
                  WRITE(NWFCUM,11) (GI(L),L=1,N)
               ELSEIF(KD.EQ.9) THEN
                  WRITE(NWFCUM,11) (T(L),L=1,N)
                  WRITE(NWFCUM,11) (OI(L),L=1,N)
               ELSEIF(KD.EQ.4) THEN
                  WRITE(NWFCUM,11) (T(L),L=1,N)
                  WRITE(NWFCUM,11) (GP(L),L=1,N)
C               ELSEIF(KD.GT.9) THEN
C                  WRITE(NWFCUM,11) (T(L),L=1,N)
C                  WRITE(NWFCUM,11) (XV(L,KD-9),L=1,N)

C               ELSEIF(KD.GT.9) THEN
C                  WRITE(NWFCUM,11) (T(L),L=1,N)
C                  WRITE(NWFCUM,11) (XV(L,KD-9),L=1,N)


               ENDIF
   11          FORMAT(6G15.5)

            ENDIF
         ENDIF
      ENDIF
    2 CONTINUE

      END
