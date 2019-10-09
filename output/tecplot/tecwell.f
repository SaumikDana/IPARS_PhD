C  TECWELL.F - READ WELL OUTPUT FILES AND PRODUCE A TECPLOT DATA FILE

C*********************************************************************
      PROGRAM TECWELL
C*********************************************************************
      CHARACTER*60 FILNAM(20),PLTFIL,BLKNAM,DUMNAM,YAT(6)
      CHARACTER*120 TIT,TITT
      CHARACTER*1 TIT1(120)
      DIMENSION T(2000),Q(2000),TT(48),QT(48),QS(2000,4),TOT(2000),
     &  QOT(2000)
      INTEGER NW(100),KD(100),NSR(4)
      EQUIVALENCE (TIT1(1),TIT)
      DATA YAT/
     & 'WATER INJECTION RATE, BPD                                   ',
     & 'OIL PRODUCTION RATE, BPD                                    ',
     & 'WATER PRODUCTION RATE, BPD                                  ',
     & 'GAS PRODUCTION RATE, MSCF/DAY                               ',
     & 'WATER/OIL RATIO                                             ',
     & 'GAS/OIL RATIO                                               ' /

      NFWELI=16
      NFWELO=17
      NDF=0
      BLKNAM=' '
      NZN=0

      DO 27 J=1,4
      NSR(J)=0
      DO 27 I=1,2000
   27 QS(I,J)=0.

C  OPEN AND USE RESPONSE FILE IF IT EXISTS

      OPEN (NFWELI,FILE='WELLS.FIL',STATUS='OLD',ERR=2)
      READ (NFWELI,1) PLTFIL
    1 FORMAT(A60)
      DO 3 I=1,20
      READ (NFWELI,1,END=4) FILNAM(I)
      IF (FILNAM(I).EQ.BLKNAM) GO TO 4
    3 NDF=NDF+1
      GO TO 4

C  GET FILE NAMES FROM USER

    2 WRITE (*,6)
    6 FORMAT(' ENTER PLOT OUTPUT FILE NAME: ')
      READ (*,1) PLTFIL
      IF (PLTFIL.EQ.BLKNAM) STOP 13
      DO 5 I=1,20
      WRITE (*,7) I
    7 FORMAT(' ENTER WELL DATA FILE NAME',I3,' OR HIT ENTER:')
      READ (*,1) FILNAM(I)
      IF (FILNAM(I).EQ.BLKNAM) GO TO 4
    5 NDF=NDF+1

    4 IF (NDF.EQ.0) STOP 13

C  OPEN PLOT FILE AND OUTPUT HEADER

      DUMNAM=PLTFIL
      OPEN (NFWELO,FILE=PLTFIL,STATUS='UNKNOWN',ERR=13)
      WRITE (NFWELO,8)
    8 FORMAT('VARIABLES = "TIME", "RATE"')

C  START DATA INPUT

      NP=0
   21 NC=0
      DO 9 I=1,NDF
      CLOSE(NFWELI)
      DUMNAM=FILNAM(I)
      OPEN (NFWELI,FILE=FILNAM(I),STATUS='OLD',ERR=13)

C  READ A DATA BLOCK TO TEMPORARY SPACE

   10 READ (NFWELI,11,END=9) TITT
   11 FORMAT(A120)
      READ (NFWELI,12) NWT,KDT,NDT
   12 FORMAT(3I5)
      READ (NFWELI,14) (TT(L),L=1,NDT)
      READ (NFWELI,14) (QT(L),L=1,NDT)
   14 FORMAT(6G12.5)

C  DECIDE IF THIS IS THE BLOCK TO BE OUTPUT NEXT

      IF (NC.EQ.0) THEN
         DO 15 J=1,NP
         IF (NWT.EQ.NW(J).AND.KDT.EQ.KD(J)) GO TO 10
   15    CONTINUE
         NP=NP+1
         NW(NP)=NWT
         KD(NP)=KDT
         TIT=TITT
         MU=0
         NC=1
      ELSE
         IF (NWT.NE.NW(NP).OR.KDT.NE.KD(NP)) GO TO 10
      ENDIF

C  COPY INPUT BLOCK TO SAVE SPACE AND ADD TO FIELD SUM

      IF (KDT.GT.0.AND.KDT.LT.5.AND.MU.EQ.0) NSR(KDT)=NSR(KDT)+1
      DO 16 J=1,NDT
      IF (KDT.GT.0.AND.KDT.LT.5) QS(MU+J,KDT)=QS(MU+J,KDT)+QT(J)
      T(MU+J)=TT(J)
   16 Q(MU+J)=QT(J)
      MU=MU+NDT
      IF (KDT.GT.0.AND.KDT.LT.5) MUT=MU
      KDS=KDT
      GO TO 10

C  END INPUT FILE LOOP

    9 CONTINUE

      IF (MU.EQ.0) GO TO 22

C  START OUTPUT TO THE PLOT FILE

      NZN=NZN+1
      CALL YAXTIT (NFWELO,NZN,YAT(KDS))

      JS=120
      DO 17 J=120,1,-1
      IF (TIT1(J).NE.' ') GO TO 18
   17 JS=J
   18 TIT1(JS)='"'
      WRITE (NFWELO,19) (TIT1(J),J=1,JS)
   19 FORMAT('ZONE T="',120A1)

C  FILTER TO REDUCE DATA MASS

      CALL FILTER(MU,T,Q,MUOT,TOT,QOT)

C  OUTPUT PLOT CURVE

      WRITE(NFWELO,20) MUOT
   20 FORMAT(' I=',I5,', J=1, K=1, F=POINT, C=BLACK,',
     & ' DT=(SINGLE SINGLE)')

      WRITE (NFWELO,14)(TOT(J),QOT(J),J=1,MUOT)

      MU=0
      GO TO 21

C  OUTPUT FIELD SUMS

   22 DO 23 J=1,4
      IF (NSR(J).LT.2) GO TO 23

      NZN=NZN+1
      TIT='FIELD RATE, STBD or MSCFD'
      JS=120
      DO 24 JJ=120,1,-1
      IF (TIT1(JJ).NE.' ') GO TO 25
   24 JS=JJ
   25 TIT1(JS)='"'
      WRITE (NFWELO,19) (TIT1(JJ),JJ=1,JS)

      CALL FILTER(MUT,T,QS(1,J),MUOT,TOT,QOT)

      WRITE(NFWELO,20) MUOT
      WRITE (NFWELO,14)(TOT(JJ),QOT(JJ),JJ=1,MUOT)

   23 CONTINUE

      STOP 0

   13 WRITE(*,99) DUMNAM
   99 FORMAT (/' ERROR # 420; OPEN FAILED FOR FILE '/A50)

      END
C*********************************************************************
      SUBROUTINE YAXTIT (NFWELO,NZ,YAT)
C*********************************************************************
      CHARACTER*60 YAT
      CHARACTER*61 A
      CHARACTER*1 A1(61)
      EQUIVALENCE (A,A1(1))

      A=YAT
      DO 1 J=61,1,-1
      IF (A1(J).NE.' ') GO TO 2
    1 JS=J
      JS=2
    2 A1(JS)='"'
      WRITE (NFWELO,3) NZ,(A1(J),J=1,JS)
    3 FORMAT ('TEXT ZN=',I3,' T="',61A1)
      WRITE (NFWELO,4)
    4 FORMAT (1X,'CS=FRAME C=BLACK S=LOCAL X=4.35 Y=50 HU=FRAME LS=1',
     & ' AN=MIDCENTER'/1X,'BXM=20 LT=0.1 BXO=BLACK BXF=WHITE',
     & ' F=HELV-BOLD H=4 A=90')

      END
C*********************************************************************
      SUBROUTINE FILTER (M,T,Q,MA,TA,QA)
C*********************************************************************
      DIMENSION T(*),Q(*),TA(*),QA(*)

      MA=1
      TA(1)=T(1)
      QA(1)=Q(1)

      MM=M-1
      DO 1 I=2,MM
      D=(Q(I)-QA(MA))/(T(I)-TA(MA))-(Q(I+1)-Q(I))/(T(I+1)-T(I))
      D=ABS(D*(T(I+1)-TA(MA)))
      IF (D.GT..001*(ABS(Q(I))+.1)) THEN
         MA=MA+1
         TA(MA)=T(I)
         QA(MA)=Q(I)
      ENDIF
    1 CONTINUE

      MA=MA+1
      TA(MA)=T(M)
      QA(MA)=Q(M)

      END
