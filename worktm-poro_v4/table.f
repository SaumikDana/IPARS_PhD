C  1D TABLE AND FUNCTION PACKAGE

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE IUTIL  (NFEOT,MF)
C  SUBROUTINE TABLE  (SORC,LSORC,XNAM,YNAM,TTIL,NTAB,NERR)
C  SUBROUTINE LOOKUP (NTAB,X,Y,DYDX)
C  SUBROUTINE NUMBR  (A,R8,KEY,L,LST,K,KXYU)
C  SUBROUTINE SPLINE (NDEG,NINT,B,A,NDAT,XDAT,NCON,CON,NODOPT,NERR)
C  SUBROUTINE LINSOV (N,A,B,D,ND1A)
C  SUBROUTINE PRTTAB (VALS,NUMX,XNAM,YNAM,TITL)
C  SUBROUTINE PRTSPL (NDEG,NINT,B,B1,BN,A,XNAM,YNAM)
C  SUBROUTINE TABUNT (UNITSX,UNITSY)

C  HISTORY:

C  JOHN WHEELER      1987       ORIGINAL BETA CODE
C  JOHN WHEELER     3/14/95     REVISION FOR NG SIMULATOR
C  JOHN WHEELER    12/30/99     ADD TECPLOT OUTPUT

C  NOTES:

C  1) ERROR NUMBERS 401 TO 430 ARE RESERVED FOR 1-D TABLE ROUTINES

C*********************************************************************
      SUBROUTINE IUTIL (MF)
C*********************************************************************

C  ROUTINE INITIALIZES UTILITY DATA.  IT MUST BE CALLED (ONCE) BEFORE
C  ANY OTHER UTILITY ROUTINE

C  MF()   = USER FILE MAP (INPUT, INTEGER) (USED BY COMP.FOR)
C           THIS VECTOR MAPS THE USER NUMBERS TO ACTUAL FILE NUMBERS
C           IF ACTUAL FILE i IS ALREADY OPEN, SET MF() = -i

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'output.h'
      INCLUDE 'utldat.h'

      INTEGER MF(*)

      NTBU=0
c      NEXFRE=1

CSGT Changed to 3 to avoid negative indices in LOOKUP
      NEXFRE=3
      DO I=1,2
         VALUES(I)=0.0D0
      ENDDO
CSGT

      ISUNTDT=.FALSE.
      DO 1 I=1,6
    1 MFILE(I)=MF(I)
      END
C*********************************************************************
      SUBROUTINE TABLE(SORC,LSORC,XNAM,YNAM,TTIL,NTAB,NERR)
C*********************************************************************

C  ROUTINE PROCESSES DATA FOR AN XY TABLE
C  THIS ROUTINE UTILIZES THE BLOCK TEXT INPUT CAPABILITY
C  OF READER.FOR, THE UNITS CONVERSION CAPABILITY OF UNITS.FOR, AND
C  THE USER PROGRAM CAPABILITY OF COMP.FOR

C  SORC()  = BLOCK DATA SOURCE (INPUT, CHARACTER*1)

C  LSORC   = LENGTH OF SORC() (INPUT, INTEGER)

C  XNAM()  = X SYMBOL (INPUT, CHARACTER*1)
C            DEFAULT UNITS IN BRACKETS ARE OPTIONAL
C            MUST BE TERMINATED WITH A BLANK SPACE
C            MAX VARIABLE NAME LENGTH WITHOUT UNITS IS 8

C  YNAM()  = Y SYMBOL (INPUT, CHARACTER*1)
C            DEFAULT UNITS IN BRACKETS ARE OPTIONAL
C            MUST BE TERMINATED WITH A BLANK SPACE
C            MAX VARIABLE NAME LENGTH WITHOUT UNITS IS 8

C  TTIL    = TABLE TITLE (INPUT, CHARACTER*50)
C            MUST CONTAIN AT LEAST 1 NONBLANK CHARACTER IN THE FIRST 5
C            CHARACTERS TO BE PRINTED

C  NTAB    = TABLE NUMBER (OUTPUT, INTEGER)

C  NERR    = COUNTER STEPPED IF ERROR(S) ENCOUNTERED IN DATA
C            (INPUT & OUTPUT, INTEGER)

C*********************************************************************

C                           KEY WORDS

C  Program       5   (set interpolation key to 5)

C  Interpolation
C     Step       0
C     Linear     1    (default)
C     Spline2    2
C     Spline3    3
C     Log        4

C  Extrapolation
C     Constant   0
C     Same       1    (default)
C     Error      2
C     Return     3

C  Constraint  (splines only) (default none)
C     y At x

C  Derivative  (splines only) (default none)
C     dydx At x

C  Nodes  (splines only) (default none)
C     x x x ...

C  Pole  (splines only) (default none)
C     x

C  Otimise  (splines only) (default none)
C     n

C  Reverse   (default do not reverse x and y order under Data)

C  Data   (no default)
C     x y x y x y ...

C  Program   (only key word; code follows)
C            X, Y, and DYDX must be declared as externals

C  NOTES:

C  1)  Each secondary key word must follow a primary keyword

C  2)  The maximum number of Constraint and Derivative key words
C      (together) is 6.

C  3)  "Constant" extrapolation returns both y and dydx evaluated at
C      last data point; ie. dydx is not set to zero.  This prevents
C      wild oscillations in Newtonian iteration.

C  4)  TRANC1(NTBU) is set to the value of POLE, if defined; otherwise
C      it is set to the smallest value of Xi.

C  5)  XYZERO(NTBU) is set to the first value of x for which y is constrained
C      to a value of zero (splines).  If a constraint is not specified,  it
C      is set to the first data value of x for which y is zero.  If neither
C      condition is satisfied, it is set to a value of 1.D30.

C*********************************************************************

C      INCLUDE 'msjunk.h'

      PARAMETER (LRCKEY=7,LIPKEY=12,LEXKEY=16,KEYAT=21,MAXCON=6,
     & MAXNOD=18)

      REAL*8       R8,DDUM,X1SV,X2SV
      LOGICAL      LSOB,REVERSE,VISOUTL
      CHARACTER*1  SORC(*),XNAM(*),YNAM(*),XYNAM1(8,2),UNTS1(60,3),
     &             RECEND,BUF1(15),C
      CHARACTER*8  XYNAM(2),YNAMT
      CHARACTER*15 WDKEY(KEYAT),BUF
      CHARACTER*18 IP1,IP2
      CHARACTER*50 TTIL,BLKTIT

      INCLUDE 'control.h'
      INCLUDE 'utldat.h'
      INCLUDE 'scrat2.h'

      EQUIVALENCE (XYNAM1(1,1),XYNAM(1)),(BUF,BUF1(1)),
     & (XYNAM(1),XYNAM1(1,1)),(UNTS(1),UNTS1(1,1))

      DATA HAVEX/0/, WDKEY
     & /'Data           ','Interpolation  ',
     &  'Extrapolation  ','Constraint     ',
     &  'Derivative     ','Nodes          ',
     &  'Program        ','Step           ',
     &  'Linear         ','Spline2        ',
     &  'Spline3        ','Log            ',
     &  'Constant       ','Same           ',
     &  'Error          ','Return         ',
     &  'At             ','Pole           ',
     &  'Optimise       ','Reverse        ',
     &  'Plot           '/

      BLKTIT=' '
      IF (LEVELC.AND.(TTIL.NE.BLKTIT)) THEN
         WRITE (NFOUT,*)
         CALL PRTTIT (TTIL)
      ENDIF

C  STEP TABLE NUMBER

      IF (NTBU.EQ.76) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,1)
         GO TO 13
      ENDIF
    1 FORMAT (/' ERROR # 401; MAX TABLES EXCEEDED')
      NTBU=NTBU+1
      NTAB=0

C  SEPARATE VARIABLE NAMES AND STANDARD UNITS

      XYNAM(1)=' '
      UNTS(1)=' '
      DO 2 I=1,8
      J=I
      IF (XNAM(I).EQ.' ') GO TO 5
      IF (XNAM(I).EQ.'[') GO TO 3
    2 XYNAM1(I,1)=XNAM(I)
    3 DO 4 I=J,60
      UNTS1(I-J+1,1)=XNAM(I)
      IF (XNAM(I).EQ.']') GO TO 5
    4 CONTINUE

    5 XYNAM(2)=' '
      UNTS(2)=' '
      DO 6 I=1,8
      J=I
      IF (YNAM(I).EQ.' ') GO TO 9
      IF (YNAM(I).EQ.'[') GO TO 7
    6 XYNAM1(I,2)=YNAM(I)
    7 DO 8 I=J,60
      UNTS1(I-J+1,2)=YNAM(I)
      IF (YNAM(I).EQ.']') GO TO 9
    8 CONTINUE

      UNTS(3)=' '

C  INITIALIZE AND SET DEFAULTS

    9 NUMDAT=0
      NCON=0
      NNOD=0
      KURKW=1
      KXYU=1
      NDAT1=NEXFRE
      RECEND=CHAR(30)
      ISUNT(1)=.FALSE.
      ISUNT(2)=.FALSE.
      ISUNT(3)=.FALSE.
      INTRPS(NTBU)=1
      KXTRPS(NTBU)=1
      POLE=-100001.
      LSOB=LEVELB
      NODOPT=0
      TRANC1(NTBU)=1.D30
      XYZERO(NTBU)=1.D30
      REVERSE=.FALSE.
      VISOUTL=.FALSE.

C  START PROCESS LOOP

      NC=0
   10 NC=NC+1
   11 IF (NC.GT.LSORC) GO TO 30
      CALL NUMBR(SORC,R8,KEY,NC,LSORC,NC2,KXYU)

      IF (KEY.EQ.3) GO TO 10
      IF (KEY.EQ.2) GO TO 13

C  KEYWORD ENCOUNTERED

      IF (KEY.EQ.1) THEN
         BUF=' '
         DO 12 I=1,15
         C= SORC(NC)
         NC=NC+1
         IF (C.EQ.RECEND.OR.C.EQ.' ') GO TO 14
   12    BUF1(I)=C
   14    DO 15 I=1,KEYAT
         IF (BUF.EQ.WDKEY(I)) THEN
            K=I
            GO TO 17
         ENDIF
   15    CONTINUE
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,16) BUF
   16    FORMAT (/' ERROR # 410; ILLEGAL KEYWORD ',A15)
         GO TO 13

   17    IF (K.EQ.20) THEN
            REVERSE=.TRUE.
            KURKW=K
            GO TO 11
         ENDIF

         IF (K.EQ.21) THEN
            VISOUTL=.TRUE.
            KURKW=K
            GO TO 11
         ENDIF

         IF (K.EQ.19) THEN
            KURKW=K
            KXYU=1
            GO TO 11
         ENDIF

         IF (K.EQ.18) THEN
            KURKW=K
            KXYU=1
            GO TO 11
         ENDIF

         IF (K.LE.LRCKEY) THEN
            IF (K.EQ.7) GO TO 55
            KURKW=K
            KXYU=1
            IF (REVERSE.AND.(K.EQ.1)) KXYU=2
            IF (K.EQ.4) KXYU=2
            IF (K.EQ.5) KXYU=3
            GO TO 11
         ENDIF

         IF (K.LE.LIPKEY) THEN
            IF (KURKW.NE.2) THEN
               IF (LEVERR.LT.2) LEVERR=2
               IF (LEVELC) WRITE (NFOUT,18) BUF
   18          FORMAT (/' ERROR # 412; WRONG CONTEXT FOR KEYWORD ',A15)
               GO TO 13
            ENDIF
            KK=K-LRCKEY-1
            INTRPS(NTBU)=KK
            IF (SPLNOUT.AND.(K.EQ.2.OR.K.EQ.3)) LSOB=.TRUE.
            GO TO 11
         ENDIF

         IF (K.LE.LEXKEY) THEN
            IF (KURKW.NE.3) THEN
               IF (LEVERR.LT.2) LEVERR=2
               IF (LEVELC) WRITE (NFOUT,18) BUF
               GO TO 13
            ENDIF
            KXTRPS(NTBU)=K-LIPKEY-1
         ENDIF

         GO TO 11
      ENDIF

C  Data KEY WORD (NUMBER READ)

      IF (KURKW.EQ.1) THEN
         IF (NEXFRE+2.GT.20000) THEN
            IF (LEVERR.LT.2) LEVERR=2
            IF (LEVELC) WRITE (NFOUT,19) R8
   19       FORMAT (/' ERROR # 413; EXCESS TABLE DATA AT ',G15.8)
            GO TO 13
         ENDIF
         VALUES(NEXFRE)=R8
         NC=NC2
         IF (KXYU.EQ.1) THEN
            KXYU=2
            IF (REVERSE) THEN
               VALUES(NEXFRE)=VALUES(NEXFRE-1)
               VALUES(NEXFRE-1)=R8
               IF (VALUES(NEXFRE).EQ.0.D0.AND.XYZERO(NTBU).GT..99D30)
     &            XYZERO(NTBU)=R8
            ENDIF
         ELSE
            KXYU=1
            IF (R8.LT.TRANC1(NTBU)) TRANC1(NTBU)=R8
            IF (.NOT.REVERSE) THEN
               IF (R8.EQ.0.D0.AND.XYZERO(NTBU).GT..99D30)
     &            XYZERO(NTBU)=VALUES(NEXFRE-1)
            ENDIF
         ENDIF
         NEXFRE=NEXFRE+1
         GO TO 11
      ENDIF

C  Constraint AND Derivative KEY WORDS (NUMBER READ)

      IF (KURKW.EQ.4.OR.KURKW.EQ.5) THEN
         IF (KXYU.NE.1) THEN
            KXYU=1
            IF (NCON.EQ.MAXCON) THEN
               IF (LEVERR.LT.2) LEVERR=2
               IF (LEVELC) WRITE (NFOUT,20)
   20          FORMAT (/' ERROR # 414; EXCESS CONSTRAINTS')
               GO TO 13
            ENDIF
            NCON=NCON+1
            IF (KURKW.EQ.5) THEN
               SCONS(2,NCON)=0.D0
               SCONS(3,NCON)=1.D0
               IF (ISUNT(1)) R8=R8/FACMU(1)
               IF (ISUNT(2)) R8=R8*FACMU(2)
            ELSE
               SCONS(2,NCON)=1.D0
               SCONS(3,NCON)=0.D0
            ENDIF
            SCONS(4,NCON)=R8
         ELSE
            KXYU=2
            IF (KURKW.EQ.5) KXYU=3
            SCONS(1,NCON)=R8
         ENDIF
         NC=NC2
         GO TO 11
      ENDIF

C  Node KEY WORD (NUMBER READ)

      IF (KURKW.EQ.6) THEN
         IF (NNOD.EQ.MAXNOD) THEN
         IF (LEVERR.LT.2) LEVERR=2
            IF (LEVELC) WRITE (NFOUT,21)
   21       FORMAT (/' ERROR # 415; EXCESS SPLINE NODES')
            GO TO 13
         ENDIF
         NNOD=NNOD+1
         SNOD(NNOD)=R8
         NC=NC2
         GO TO 11
      ENDIF

C  Pole KEY WORD (NUMBER READ)

      IF (KURKW.EQ.18) THEN
         POLE=R8
         NC=NC2
         GO TO 11
      ENDIF

C  Otimise KEY WORD (NUMBER READ)

      IF (KURKW.EQ.19) THEN
         NODOPT=R8+.5D0
         NC=NC2
         GO TO 11
      ENDIF

C  UNEXPECTED NUMBER READ

      IF (LEVERR.LT.2) LEVERR=2
      IF (LEVELC) WRITE (NFOUT,22) R8
   22 FORMAT (/' ERROR # 416; UNEXPECTED NUMBER ',G15.8)
      GO TO 13

C  START POST PROCESSING

   30 NUM=NEXFRE-NDAT1
      IF (((NUM/2)*2).NE.NUM) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,31)
         GO TO 13
      ENDIF
   31 FORMAT (/' ERROR # 402; INCOMPLETE PAIR IN TABLE')
      IF (NUM.EQ.0) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,32)
         GO TO 13
      ENDIF
   32 FORMAT (/' ERROR # 417; NO DATA IN TABLE')

      NTAB=NTBU
      INDEX1(NTBU)=NDAT1
      INDEXL(NTBU)=NDAT1
      INDEX2(NTBU)=NEXFRE-2

C  PRINT TABLE VALUES

      I1=INDEX1(NTBU)
      I2=INDEX2(NTBU)+1
      MAX2=I2-I1
      IF (LEVELC)
     & CALL PRTTAB (VALUES(I1),MAX2,XYNAM(1),XYNAM(2),'     ')

C  PAPER PLOT RAW DATA OUTPUT (TECPLOT)

      X1SV=VALUES(I1)
      X2SV=VALUES(I2-1)
      MERR=0
      IF (VISOUTL.AND.LEVELC)
     & CALL VTABDAT(VALUES(I1),MAX2,XYNAM,TTIL,MERR)
      IF (MERR.GT.0) VISOUTL=.FALSE.

C  SAVE SOME DATA BEFORE TRANSFORMING

      DO 36 I=1,NUM
   36 DATS(I)=VALUES(I+I1-1)
      YNAMT=XYNAM(2)
      I3=I1+1
      KTRANS(NTBU)=0

C  PC TRANSFORM

      IF (POLE.GT.-100000..AND.NUM.GT.3.AND.INTRPS(NTBU).NE.0.AND.
     & INTRPS(NTBU).NE.4) THEN
         KTRANS(NTBU)=1
         YNAMT='Ys    '
         TRANC1(NTBU)=POLE
         IF (TRANC1(NTBU).GE.VALUES(I1)) THEN
            IF (LEVERR.LT.2) LEVERR=2
            IF (LEVELC) WRITE (NFOUT,37) TTIL
            GO TO 13
         ENDIF
   37    FORMAT (/' ERROR # 404; INVALID TRANSFORM OF TABLE '/1X,A50)
         DO 38 I=I3,I2,2
   38    VALUES(I)=VALUES(I)*(VALUES(I-1)-TRANC1(NTBU))
         IF (LSOB) WRITE (NFOUT,39) TRANC1(NTBU)
   39    FORMAT(/' Y TRANSFORMED TO Ys/(X -',F8.4,')')
      ENDIF

C  SET UP SINGLE PAIR TABLE

      IF (NUM.LT.4) THEN
          INTRPS(NTBU)=0
          KXTRPS(NTBU)=0
          VALUES(NEXFRE)=1.D30
          VALUES(NEXFRE+1)=1.D0
          NEXFRE=NEXFRE+2
          GO TO 77
      ENDIF

C  PRINT KEY DATA

      IF (INTRPS(NTBU).EQ.0) IP1='STEP              '
      IF (INTRPS(NTBU).EQ.1) IP1='LINEAR            '
      IF (INTRPS(NTBU).EQ.2) IP1='QUADRATIC SPLINE  '
      IF (INTRPS(NTBU).EQ.3) IP1='CUBIC SPLINE      '
      IF (INTRPS(NTBU).EQ.4) IP1='LOGARITHMIC       '
      IF (KXTRPS(NTBU).EQ.0) IP2='LAST VALUE DEFINED'
      IF (KXTRPS(NTBU).EQ.1) IP2=IP1
      IF (KXTRPS(NTBU).EQ.2) IP2='FATAL ERROR       '
      IF (KXTRPS(NTBU).EQ.3) IP2='OUT OF TABLE MSG. '
      IF (LSOB) WRITE (NFOUT,40) IP1,IP2
   40 FORMAT (/' INTERPOLATION = ',A18,'        EXTRAPOLATION = ',A18)

C  LOG INTERPOLATION

      IF (INTRPS(NTBU).EQ.4) THEN
         DO 41 I=I1,I2
         IF (VALUES(I).LE.0.D0) THEN
             IF (LEVERR.LT.2) LEVERR=2
             IF (LEVELC) WRITE (NFOUT,42) VALUES(I)
             GO TO 13
         ENDIF
   42    FORMAT(/' ERROR # 405; NON-POSITIVE VALUE IN LOG TABLE:',
     &    G13.5)
         VALUES(I)=DLOG(VALUES(I))
   41    CONTINUE
      ENDIF

C  PRINT SPLINE CONSTRAINTS

      IF (INTRPS(NTBU).NE.2.AND.INTRPS(NTBU).NE.3) GO TO 77

      IF (LSOB) THEN
          DO 43 I=1,NCON
          IF (SCONS(3,I).EQ.0..AND.ABS(SCONS(2,I)-1.).LT..0001) THEN
             WRITE (NFOUT,44) SCONS(4,I),SCONS(1,I)
             IF (SCONS(4,I).EQ.0.D0) XYZERO(NTBU)=SCONS(1,I)
             GO TO 43
          ENDIF
          IF (SCONS(2,I).EQ.0..AND.ABS(SCONS(3,I)-1.).LT..0001) THEN
             WRITE (NFOUT,45) SCONS(4,I),SCONS(1,I)
          ELSE
             WRITE (NFOUT,46) (SCONS(J,I),J=1,4)
          ENDIF
   43     CONTINUE
      ENDIF
   44 FORMAT (/' CONSTRAIN Y TO',G14.6,' AT X =',G14.6)
   45 FORMAT (/' CONSTRAIN DY/DX TO',G14.6,' AT X =',G14.6)
   46 FORMAT (/' CONSTRAINT AT X =',G14.7/' (',G14.7,') Y + (',G14.7,
     & ') DY/DX =',G14.7)

C  FIT SPLINE DATA, AND PRINT SPLINE EQUATIONS

      NINT=NNOD+1
      NDAT=NUM/2
      NERRO=NERR
      CALL SPLINE(INTRPS(NTBU),NINT,SNOD,SCOF,NDAT,VALUES(I1),NCON,
     & SCONS,NODOPT,NERR)
      IF (NERR.NE.NERRO) GO TO 77
      IF (LEVELA.OR.SPLNOUT) CALL PRTSPL (INTRPS(NTBU),NINT,SNOD,
     & VALUES(I1),VALUES(I2-1),SCOF,XYNAM(1),YNAMT)

C  PACK SPLINE COEFFICIENTS INTO DATA TABLE REPLACING RAW DATA

      NUM=I1+2
      IF (NNOD.GT.0) THEN
         DO 47 I=1,NNOD
         VALUES(NUM)=SNOD(I)
   47    NUM=NUM+2
      ENDIF
      INDEX2(NTBU)=NUM
      VALUES(NUM)=VALUES(I2-1)
      J=INTRPS(NTBU)+1
      DO 48 I=1,NINT
      VALUES(2*I+I1-1)=SCOF(1,I)
      DO 48 K=2,J
      NUM=NUM+1
   48 VALUES(NUM)=SCOF(K,I)
      NEXFRE=NUM+1

C  COMPUTE SPLINE STANDARD DEVIATION

      NDGF=NDAT-(INTRPS(NTBU)-1)*NINT+2
      IF (NDGF.LT.1.OR.(.NOT.LSOB)) GO TO 77
      S=0.
      J=1
      DO 49 I=1,NDAT
      CALL LOOKUP (NTAB,DATS(J),R8,DDUM)
      DDUM=R8-DATS(J+1)
      S=S+DDUM*DDUM
   49 J=J+2
      S=SQRT(S/NDGF)
      IF (ISUNT(2)) S=S/FACMU(2)
      WRITE (NFOUT,50) S
   50 FORMAT (/' SPLINE STANDARD DEVIATION =',G12.5)

      GO TO 77

C  USER PROGRAMED FUNCTION

   55 IF (LEVELB) THEN
         CALL PRTTAB (VALUES,0,XYNAM(1),XYNAM(2),'     ')
         WRITE (NFOUT,56)
      ENDIF
   56 FORMAT(/' USER PROGRAMMED FUNCTION')
      IF (HAVEX.EQ.0) THEN
         HAVEX=1
         CALL EXTDEF(TABX,1,'TABLEX',0,0,0,N)
         CALL EXTDEF(TABY,1,'TABLEY',0,0,0,N)
         CALL EXTDEF(DYDX,1,'DYDX  ',0,0,0,N)
         IF (N.NE.0) THEN
            IF (LEVERR.LT.2) LEVERR=2
            IF (LEVELC) WRITE (NFOUT,57)
            GO TO 13
         ENDIF
   57    FORMAT (/' ERROR # 403; TOO MANY EXTERNAL VARIABLES')
      ENDIF
      NC=NC-1
      DO 58 I=1,100
      IF (SORC(NC).EQ.RECEND) GO TO 59
   58 NC=NC+1
   59 NC=NC+1
      IF (NC.GE.LSORC) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,60)
         GO TO 13
      ENDIF
   60 FORMAT (/' ERROR # 411; NO PROGRAM')
      CALL CMPDRV(SORC(NC),LSORC-NC+1,NPG,.FALSE.,KERR)
      IF (KERR.NE.0) GO TO 13
      INTRPS(NTBU)=5
      INDEX1(NTBU)=NPG
      INDEXL(NTBU)=NEXFRE
      INDEX2(NTBU)=NEXFRE-2
      NTAB=NTBU

C  PAPER PLOT FIT DATA OUTPUT (TECPLOT)

   77 CONTINUE
      IF (VISOUTL.AND.LEVELC) CALL VTABFIT(NTAB,X1SV,X2SV,XYNAM)

C  NORMAL EXIT

      ISUNTDT=.FALSE.
      RETURN

C  ERROR EXIT

   13 NERR=NERR+1
      ISUNTDT=.FALSE.

      END
C*********************************************************************
      SUBROUTINE LOOKUP (NTAB,X,Y,DYDXA)
C*********************************************************************

C  ROUTINE LOOKS UP Y AND DY/DX IN A TABLE FOR A GIVEN X

C  NTAB   = INTERNAL TABLE NUMBER (INPUT, INTEGER)

C  X      = INDEPENDENT VARIABLE (INPUT, REAL*8)

C  Y      = DEPENDENT VARIABLE (OUTPUT, REAL*8)

C  DYDXA  = DERIVATIVE OF Y WITH RESPECT TO X (OUTPUT, REAL*8)

C*********************************************************************
      REAL*8 X,Y,XX,DUM1,DUM2,DUM3,DYDXA

      INCLUDE 'control.h'
      INCLUDE 'utldat.h'

      INTRP=INTRPS(NTAB)
      NENTY=INDEXL(NTAB)
      I1=INDEX1(NTAB)
      I2=INDEX2(NTAB)

C  TRANSFORM FOR LOG INTERPOLATION OR RUN USER PROGRAM

      IF (INTRP.GT.3) THEN

         IF (INTRP.EQ.5) THEN
            TABX=X
            CALL EXCDRV(I1,KERR)
            IF (KERR.NE.0) THEN
               IF (LEVERR.LT.3) LEVERR=3
               IF (LEVELC) WRITE (NFOUT,313)
               GO TO 13
            ENDIF
  313       FORMAT(' ERROR # 418, USER PROGRAM ERROR IN A TABLE')
            Y=TABY
            DYDXA=DYDX
            RETURN
         ENDIF

         IF (X.LE.0.D0) THEN
            IF (LEVERR.LT.2) LEVERR=2
            IF (LEVELC) WRITE (NFOUT,14) X
            GOTO 13
         ENDIF
         XX=DLOG(X)
      ELSE
         XX=X
      ENDIF
   14 FORMAT (/' ERROR # 406; LOG INTERPOLATION WITH X =',G14.6)

C  LOCATE INTERVAL FORWARD

      IF (XX.LT.VALUES(NENTY)) GO TO 1

      NX=NENTY+2
      DO 2 I=NX,I2,2
      IF (XX.LT.VALUES(I)) GO TO 3
    2 NENTY=I

C  EXTRAPOLATE ABOVE

      IF (KXTRPS(NTAB).EQ.2) THEN
         IF (LEVERR.LT.3) LEVERR=3
         WRITE (NFOUT,4) X,VALUES(I2),NUMPRC
         GO TO 13
      ENDIF
    4 FORMAT (/' ERROR # 407; INDEPENDENT VARIABLE',G14.6,
     & ' EXCEEDS',G14.6,'; PROCESSOR',I4)

      NENTY=I2-2
      IF (INTRP.EQ.0) THEN
         Y=VALUES(I2+1)
         DYDXA=0.D0
         INDEXL(NTAB)=NENTY
         RETURN
      ENDIF

      IF (KXTRPS(NTAB).EQ.3) THEN
         Y=-98765.D0
         DYDXA=0.D0
         INDEXL(NTAB)=NENTY
         RETURN
      ENDIF

      IF (KXTRPS(NTAB).EQ.0) XX=VALUES(I2)
      GO TO 3

C  LOCATE INTERVAL REVERSE

    1 IF (NENTY.EQ.I1) GO TO 5
      NX=NENTY-2
      DO 6 I=NX,I1,-2
      NENTY=I
      IF (XX.GE.VALUES(I)) GO TO 3
    6 CONTINUE

C  EXTRAPOLATE BELOW

    5 IF (I2.EQ.I1) THEN
         Y=VALUES(I1+1)
         DYDXA=0.D0
         RETURN
      ENDIF

      IF (KXTRPS(NTAB).EQ.2) THEN
         IF (LEVERR.LT.3) LEVERR=3
         WRITE (NFOUT,7) X,VALUES(I1),NUMPRC
         GO TO 13
      ENDIF
    7 FORMAT (/' ERROR # 408; INDEPENDENT VARIABLE',G14.6,
     1 ' LESS THAN',G14.6,'; PROCESSOR',I4)

      IF (KXTRPS(NTAB).EQ.3) THEN
         Y=-98765.D0
         DYDXA=0.D0
         INDEXL(NTAB)=NENTY
         RETURN
      ENDIF

      IF (KXTRPS(NTAB).EQ.0) XX=VALUES(I1)

C  INTERPOLATE

    3 INDEXL(NTAB)=NENTY
      GOTO (8,9,10,11),INTRP

      Y=VALUES(NENTY+1)
      DYDXA=0.D0
      INDEXL(NTAB)=NENTY
      RETURN

    9 I=I2+NENTY-I1+1
      DUM1=VALUES(I+1)*XX
      DUM2=VALUES(I)+DUM1
      Y=VALUES(NENTY+1)+DUM2*XX
      DYDXA=DUM2+DUM1
      IF (KTRANS(NTAB).NE.0) GO TO 12
      RETURN

   10 I=NENTY-I1+1
      I=I2+I+I/2
      DUM1=VALUES(I+2)*XX
      DUM2=DUM1+VALUES(I+1)
      Y=(XX*DUM2+VALUES(I))*XX+VALUES(NENTY+1)
      DYDXA=(DUM1+DUM2+DUM2)*XX+VALUES(I)
      IF (KTRANS(NTAB).NE.0) GO TO 12
      RETURN

   11 DUM1=VALUES(NENTY)
      DUM2=VALUES(NENTY+1)
      DUM3=(DUM2-VALUES(NENTY+3))/(DUM1-VALUES(NENTY+2))
      Y=DEXP(DUM2+(XX-DUM1)*DUM3)
      DYDXA=DUM3*Y/XX
      RETURN

    8 DUM1=VALUES(NENTY)
      DUM2=VALUES(NENTY+1)
      DUM3=(DUM2-VALUES(NENTY+3))/(DUM1-VALUES(NENTY+2))
      DYDXA=DUM3
      Y=DUM2+(XX-DUM1)*DUM3
      IF (KTRANS(NTAB).EQ.0) RETURN

C  TRANSFORM AND RETURN

   12 DUM1=1.D0/(XX-TRANC1(NTAB))
      Y=Y*DUM1
      DYDXA=(DYDXA-Y)*DUM1
      RETURN

C  ERROR EXIT

   13 STOP 13
      END
C*********************************************************************
      SUBROUTINE NUMBR (A,R8,KEY,L,LST,K,KXYU)
C*********************************************************************
C  ASCII TO REAL*8 TRANSLATION ROUTINE

C  A()      = SOURCE STRING (INPUT, CHARACTER*1)

C  R8       = NUMBER OUTPUT (OUTPUT, REAL*8)

C  KEY      = RESULT KEY (OUTPUT, INTEGER)
C           = 0 ==> NUMBER FOUND
C           = 1 ==> NOT A NUMBER
C           = 2 ==> UNITS ERROR (MESSAGE GENERATED)
C           = 3 ==> END OF RECORD OR END OF DATA

C  L        = 1ST OFFSET IN A() TO BE TESTED (INPUT & OUTPUT, INTEGER)
C             UPDATED TO SKIP BLANKS AND COMMAS

C  LST      = LAST OFFSET IN A() TO BE TESTED (INPUT, INTEGER)

C  K        = 1ST OFFSET IN A() FOLLOWING THE NUMBER (OUTPUT, INTEGER)

C  KXYU     = UNITS KEY (INPUT, INTEGER)

C  NOTES:

C    1)  IF KEY > 0 THEN R8 AND K ARE UNCHANGED

C    2)  END OF RECORD IS CHARACTER 30

C*********************************************************************
      PARAMETER (MAXCHR=100000000,MXUS=60)

      REAL*8 R8,D,V
      LOGICAL NONUM,PNT,SGN
      CHARACTER*1 NUM(10),BLANK,COMMA,PLUS,MINUS,POINT,EEE,LBRAC,
     * RBRAC,RECEND,A(*),UNTI1(MXUS,3)
      CHARACTER*50 E

      INCLUDE 'control.h'
      INCLUDE 'utldat.h'
      INCLUDE 'scrat2.h'

      EQUIVALENCE (UNTI1(1,1),UNTI(1))

      DATA NUM/'0','1','2','3','4','5','6','7','8','9'/,
     * BLANK/' '/,COMMA/','/,PLUS/'+'/,MINUS/'-'/,POINT/'.'/,EEE/'E'/
     * LBRAC/'['/,RBRAC/']'/

      KEY=1
      RECEND=CHAR(30)

C  SKIP ANY LEADING BLANKS OR COMMAS AND CHECK FOR END OF RECORD

      LL=L
      DO 1 I=LL,LST
      L=I
      IF (A(I).EQ.RECEND) GO TO 16
      J=I
      IF (A(I).NE.BLANK.AND.A(I).NE.COMMA) GO TO 2
    1 CONTINUE
   16 KEY=3
      RETURN

C  GET LEADING SIGN

    2 SGN=.FALSE.
      IF (A(J).EQ.PLUS) THEN
         J=J+1
      ELSE
         IF (A(J).EQ.MINUS) THEN
            SGN=.TRUE.
            J=J+1
         ENDIF
      ENDIF

C  GET DECIMAL FRACTION

      V=0.D0
      NONUM=.TRUE.
      PNT=.FALSE.
    3 IF (A(J).EQ.POINT) THEN
         IF (PNT) RETURN
         PNT=.TRUE.
         D=.1D0
         J=J+1
      ENDIF
      DO 4 I=1,10
      IF (A(J).EQ.NUM(I)) THEN
         IF (PNT) THEN
            V=V+D*(I-1)
            D=.1D0*D
         ELSE
            V=10.D0*V+I-1
         ENDIF
         J=J+1
         NONUM=.FALSE.
         GO TO 3
      ENDIF
    4 CONTINUE
      IF (NONUM) THEN
         IF (A(J).EQ.RECEND) KEY=3
         RETURN
      ENDIF
      IF (SGN) V=-V

C     GET EXPONENT

      IF (A(J).NE.EEE) GO TO 7
      J=J+1
      SGN=.FALSE.
      IF (A(J).EQ.PLUS) THEN
         J=J+1
      ELSE
         IF (A(J).EQ.MINUS) THEN
            SGN=.TRUE.
            J=J+1
         ENDIF
      ENDIF
      N=0
      NONUM=.TRUE.
    5 DO 6 I=1,10
      IF (A(J).EQ.NUM(I)) THEN
         N=10*N+I-1
         J=J+1
         NONUM=.FALSE.
         GO TO 5
      ENDIF
    6 CONTINUE
      IF (NONUM) RETURN
      IF (SGN) N=-N
      V=V*(10.D0**N)

C  GET UNITS

    7 IF (ISUNTDT) THEN
         ISUNTDT=.FALSE.
         DO 17 I=1,2
         ISUNT(I)=.TRUE.
         CALL CONVRT(UNTI(I),UNTS(I),FACMU(I),FACAU(I),KODRET,E)
         IF (KODRET.NE.0) THEN
            NERR=NERR+1
            ISUNT(I)=.FALSE.
            IF (LEVERR.LT.2) LEVERR=2
            IF (LEVELC) THEN
               WRITE (NFOUT,14) KODRET,E
               WRITE (NFOUT,15) UNTI1(1,I)
            ENDIF
   14       FORMAT(/' ERROR #',I4,'; ',A50)
   15       FORMAT(' IN UNITS ',60A1)
         ENDIF
   17    CONTINUE
      ENDIF

      IF (A(J).EQ.LBRAC) THEN
         LL=LST-J+1
         IF (LL.GT.MXUS) LL=MXUS
         UNTI(KXYU)=' '
         DO 8 I=1,LL
         JJ=J+I-1
         UNTI1(I,KXYU)=A(JJ)
         IF (A(JJ).EQ.RBRAC) GO TO 9
    8    CONTINUE
         E='MISSING ] IN UNITS SPECFICATION'
         KODRET=409
         GO TO 10
    9    CALL CONVRT(A(J),UNTS(KXYU),FACMU(KXYU),FACAU(KXYU),KODRET,E)
         IF (KODRET.NE.0) GO TO 10
         J=JJ+1
         ISUNT(KXYU)=.TRUE.
      ENDIF

C  APPLY UNITS CONVERSION

      IF (ISUNT(KXYU)) V=V*FACMU(KXYU)+FACAU(KXYU)

C  RETURN NUMBER

      KEY=0
      K=J
      R8=V
      RETURN

C  UNITS ERROR

   10 KEY=2
      IF (.NOT.LEVELC) RETURN
      LL=LST-L+1
      IF (LL.GT.65) LL=65
      DO 11 I=1,LL
      M=I
      IF (A(J+I).EQ.RECEND) GO TO 13
   11 CONTINUE
   13 CALL PUTERR(KODRET,E,A(L),M,1)

      END
C*********************************************************************
      SUBROUTINE SPLINE(NDEG,NINT,B,A,NDAT,XDAT,NCON,CON,NODOPT,NERR)
C*********************************************************************

C  ROUTINE FITS A SPLINE FUNCTION WITH CONSTRAINTS TO DATA

C  NDEG    = SPLINE DEGREE (INPUT, INTEGER)

C  NINT    = NUMBER OF SPLINE INTERVALS (INPUT, INTEGER)

C  B(I)    = LOCATION OF I-TH SPLINE NODE (INPUT & OUTPUT, REAL*8)

C  A(I,J)  = I-TH COEFFICIENT IN THE J-TH INTERVAL (OUTPUT, REAL*8)

C  NDAT    = NUMBER OF DATA PAIRS (INPUT, INTEGER)

C  XDAT(I) = X,Y DATA VECTOR (INPUT, REAL*8)

C  NCON    = NUMBER OF CONSTRAINTS (INPUT, INTEGER)

C  CON(I,J)= I'TH CONSTANT IN THE J'TH CONSTRAINT (INPUT,REAL*8)
C            AT X=CON(1,I) THE CONSTRAINT IS:
C            CON(2,I)*Y+CON(3,I)*(DY/DX)=CON(4,I)

C  NODOPT    = 0 ==> NO NODE OPTIMIZATION (DEFAULT)
C            = N ==> N TRIAL EVALUATIONS IN A SIMPLE SEARCH FOR
C                    BETTER NODE LOCATIONS

C  NERR    = COUNTER STEPPED IF ERRORS OCCUR
C            (INPUT & OUTPUT, INTEGER)

C*********************************************************************
      REAL*8 X,Y,DLB1,DLB2,DLB3,SDB,A(4,19),B(*),XDAT(*),CON(4,*)

      INCLUDE 'control.h'
      INCLUDE 'scrat2.h'

      ND1A=20
      NM1=NDEG-1
      NP=NDEG+1
      MM=NINT-1
      NPM=NDEG+NINT
      NMJ=NPM+NCON
      NMJP=NMJ+1
      IF ((NDAT+NCON-NPM).LT.0) THEN
          IF (LEVERR.LT.2) LEVERR=2
          IF (LEVELC) WRITE (NFOUT,19)
          NERR=NERR+1
          RETURN
      ENDIF
   19 FORMAT (' INSUFFICIENT DATA TO DETERMINE SPLINE FUNCTION')

C  OUTER LOOP FOR NODE OPTIMIZATION

      NOL=NODOPT+1
      IF (NINT.LT.2) NOL=1
      SDB=0.D0
      NCO=100
      DO 50 I=1,MM
   50 BSTEP(I)=.1

      DO 20 NOP=1,NOL

C  COMPUTE COEFFICIENTS AND RESIDUALS

      DO 1 I=1,NMJ
      DO 1 J=I,NMJP
    1 W(I,J)=0.D0

      NDV=2*NDAT-1
      DO 2 K=1,NDV,2
      X=XDAT(K)
      Y=XDAT(K+1)
      DLB1=1.D0
      DO 3 I=1,NP
      W(I,NMJP)=W(I,NMJP)+Y*DLB1
      DLB2=DLB1
      DO 4 J=I,NP
      W(I,J)=W(I,J)+DLB1*DLB2
    4 DLB2=DLB2*X
      IF (NINT.LT.2) GO TO 3
      L=NP
      DO 5 J=1,MM
      DLB2=X-B(J)
      IF (DLB2.LE.0.) GO TO 3
      L=L+1
    5 W(I,L)=W(I,L)+DLB1*(DLB2**NDEG)
    3 DLB1=DLB1*X
      L=NP
      DO 6 I=1,MM
      DLB1=X-B(I)
      IF (DLB1.LE.0.) GO TO 2
      DLB1=DLB1**NDEG
      L=L+1
      W(L,NMJP)=W(L,NMJP)+Y*DLB1
      LL=L
      DO 7 J=I,MM
      DLB2=X-B(J)
      IF (DLB2.LE.0.) GO TO 6
      W(L,LL)=W(L,LL)+DLB1*(DLB2**NDEG)
    7 LL=LL+1
    6 CONTINUE
    2 CONTINUE

      IF (NCON.EQ.0) GO TO 8
      L=NPM
      DO 9 I=1,NCON
      L=L+1
      W(L,NMJP)=CON(4,I)
      X=CON(1,I)
      DLB1=1.D0
      DO 10 J=1,NP
      W(J,L)=CON(2,I)*DLB1+CON(3,I)*DLB2*(J-1)
      DLB2=DLB1
   10 DLB1=DLB1*X
      IF (NINT.LT.2) GO TO 9
      LL=NP
      DLB3=CON(3,I)
      DLB3=DLB3*NDEG
      DO 11 J=1,MM
      LL=LL+1
      DLB1=X-B(J)
      IF (DLB1.GT.0.) W(LL,L)=(CON(2,I)*DLB1+DLB3)*(DLB1**NM1)
   11 CONTINUE
    9 CONTINUE

    8 DO 12 I=2,NMJ
      L=I-1
      DO 12 J=1,L
   12 W(I,J)=W(J,I)

C  SOLVE LINEAR SYSTEM

      CALL LINSOV (NMJ,W,BB,DLB1,ND1A)
      IF (DLB1.EQ.0.D0) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,18)
         NERR=NERR+1
         RETURN
      ENDIF
   18 FORMAT(' SINGULAR MATRIX IN SPLINE DETERMINATION')

C  COMPUTE SUM OF THE SQUARES OF THE RESIDUALS FOR NODE OPTIMIZATION

      IF (NOL.LT.2) GO TO 28

      DLB3=0.D0
      DO 21 K=1,NDV,2
      X=XDAT(K)
      Y=XDAT(K+1)
      DLB1=BB(1)
      DLB2=1.D0
      DO 22 I=2,NP
      DLB2=DLB2*X
   22 DLB1=DLB1+BB(I)*DLB2
      IF (NINT.GT.1) THEN
         DO 23 J=1,MM
         DLB2=X-B(J)
         IF (DLB2.LE.0.D0) GO TO 24
   23    DLB1=DLB1+BB(J+NP)*(DLB2**NDEG)
      ENDIF
   24 DLB3=DLB3+(DLB1-Y)**2
   21 CONTINUE

C  NODE OPTIMIZATION

      IF (NOP.EQ.1.OR.DLB3.LT.SDB) THEN
         SDB=DLB3
         DO 25 I=1,MM
   25    BBB(I)=B(I)
         DO 26 I=1,NPM
   26    BCF(I)=BB(I)
         IF (NOP.GT.1) THEN
            BSTEP(NCO)=1.2*BSTEP(NCO)
            IF (BSTEP(NCO).GT..5) BSTEP(NCO)=.5
            IF (BSTEP(NCO).LT.-.5) BSTEP(NCO)=-.5
         ENDIF
      ELSE
         B(NCO)=BBB(NCO)
         DO 27 I=1,NPM
   27    BB(I)=BCF(I)
         BSTEP(NCO)=-.6*BSTEP(NCO)
      ENDIF
      IF (NOP.LT.NOL) THEN
         NCO=NCO+1
         IF (NCO.GT.MM) NCO=1
         IF (BSTEP(NCO).GT.0.) THEN
            IF (NCO.LT.MM) THEN
               B(NCO)=B(NCO)+BSTEP(NCO)*(B(NCO+1)-B(NCO))
            ELSE
               B(MM)=B(MM)+BSTEP(MM)*(XDAT(NDV)-B(MM))
            ENDIF
         ELSE
            IF (NCO.GT.1) THEN
               B(NCO)=B(NCO)+BSTEP(NCO)*(B(NCO)-B(NCO-1))
            ELSE
               B(1)=B(1)+BSTEP(1)*(B(1)-XDAT(1))
            ENDIF
         ENDIF
      ENDIF

   20 CONTINUE

C  TRANSFORM COEFFICIENTS

   28 DO 14 I=1,NP
   14 W(I,1)=BB(I)
      IF (NINT.LT.2) GO TO 16
      LL=NP
      DO 15 I=2,NINT
      LL=LL+NP
      DLB1=BB(I+NDEG)
      L=LL
      W(L,1)=W(L-NP,1)+DLB1
      DO 15 J=1,NDEG
      DLB1=(J-NP)*DLB1*B(I-1)/J
      L=L-1
   15 W(L,1)=W(L-NP,1)+DLB1

C  RETURN COEFFICIENTS

   16 L=0
      DO 17 I=1,NINT
      DO 17 J=1,NP
      L=L+1
   17 A(J,I)=W(L,1)
      END
C*********************************************************************
      SUBROUTINE LINSOV (N,A,B,D,ND1A)
C*********************************************************************

C  SOLVES A LINEAR SYSTEM OF EQUATIONS USING DOUBLE PRECISION
C  AND RETURNS THE DETERMINANT OF THE COEFFICIENT MATRIX

C  N      = NUMBER OF EQUATIONS (INPUT, INTEGER)

C  A(,)   = AUGMENTED COEFFICIENT MATRIX (INPUT, REAL*8)
C           A(I,N+1) IS THE RESIDUAL OF THE I-TH EQUATION

C  B()    = SOLUTION VECTOR (OUTPUT, REAL*8)

C  D      = DETERMINANT OF COEFFICIENT MATRIX (OUTPUT, REAL*8)
C           IF D=0 THEN B() HAS NO MEANING

C  ND1A   = FIRST DIMENSION OF THE A ARRAY

C*********************************************************************
      REAL*8 A(ND1A,*),B(*),D,DUM

C SPECIAL CASES

      IF (N.GT.2) GO TO 9
      IF (N.EQ.1) THEN
         D=A(1,1)
         IF (D.NE.0.D0) B(1)=A(1,2)/D
         RETURN
      ENDIF
      D=A(1,1)*A(2,2)-A(1,2)*A(2,1)
      IF (D.EQ.0.D0) RETURN
      B(1)=(A(2,2)*A(1,3)-A(1,2)*A(2,3))/D
      B(2)=(A(1,1)*A(2,3)-A(2,1)*A(1,3))/D
      RETURN

    9 NP=N+1
      NM=N-1
      D=1.D0

C  PIVOTING AND FORWARD ELIMINATION

      DO 1 I=1,NM
      IP=I+1
      DO 1 J=IP,N
      IF (DABS(A(J,I)).LE.DABS(A(I,I))) GO TO 2
      D=-D
      DO 3 K=I,NP
      DUM=A(I,K)
      A(I,K)=A(J,K)
    3 A(J,K)=DUM
    2 IF (A(I,I).EQ.0.0) GO TO 1
      DUM=A(J,I)/A(I,I)
      DO 4 K=IP,NP
    4 A(J,K)=A(J,K)-A(I,K)*DUM
    1 CONTINUE

C  DETERMINANT EVALUATION

      DO 5 I=1,N
    5 D=D*A(I,I)
      IF (D.EQ.0.D0) RETURN

C  BACK SUBSTITUTION

      B(N)=A(N,NP)/A(N,N)
      DO 7 I=1,NM
      IS=NP-I
      DUM=0.D0
      DO 8 J=IS,N
    8 DUM=DUM+B(J)*A(IS-1,J)
    7 B(IS-1)=(A(IS-1,NP)-DUM)/A(IS-1,IS-1)
      RETURN
      END
C*********************************************************************
      SUBROUTINE PRTTAB (VALS,NUMX,XNAM,YNAM,TITL)
C*********************************************************************

C   ROUTINE PRINTS DATA FROM AN X-Y TABLE

C   VALS() = TABLE DATA - X AND Y (INPUT, REAL*8)

C   NUMX   = INDEX OF LAST X VALUE (INPUT, INTEGER)

C   XNAM   = X TITLE (INPUT, CHARACTER*8)

C   YNAM   = Y TITLE (INPUT, CHARACTER*8)

C   TITL   = TABLE TITLE (INPUT, CHARACTER*50)
C            MUST CONTAIN AT LEAST 1 NONBLANK CHARACTER IN THE FIRST 5
C            CHARACTERS TO BE PRINTED

**********************************************************************
      REAL*8 VALS(*),R8,R8V(10)
      CHARACTER*1 TITL(*),BLK
      CHARACTER*8 XNAM,YNAM
      CHARACTER*60 UN,UB

      INCLUDE 'control.h'
      INCLUDE 'scrat2.h'

      DATA BLK/' '/

      DO 16 I=1,5
      IF (TITL(I).NE.BLK) THEN
         WRITE (NFOUT,*)
         CALL PRTTIT (TITL)
         GO TO 17
      ENDIF
   16 CONTINUE

   17 IF (NUMX.LT.3) THEN
          IF (NUMX.EQ.0) GO TO 15
          R8=VALS(2)
          IF (ISUNT(2)) THEN R8=(R8-FACAU(2))/FACMU(2)
          WRITE (NFOUT,5) YNAM,VALS(2)
          RETURN
      ENDIF
    5 FORMAT(/1X,A8,'=',G15.8,' (CONSTANT)')

      N2=-1
    3 N1=N2+2
      N2=N2+10
      IF (N2.GT.NUMX) N2=NUMX
      WRITE (NFOUT,4)
    4 FORMAT('  ')
      IF (ISUNT(1)) THEN
         J=0
         DO 10 I=N1,N2,2
         J=J+1
   10    R8V(J)=(VALS(I)-FACAU(1))/FACMU(1)
         WRITE (NFOUT,9) XNAM,(R8V(I),I=1,J)
      ELSE
         WRITE (NFOUT,9) XNAM,(VALS(I),I=N1,N2,2)
      ENDIF
    9 FORMAT(1X,A8,5G14.7)

      M1=N1+1
      M2=N2+1
      IF (ISUNT(2)) THEN
         J=0
         DO 11 I=M1,M2,2
         J=J+1
   11    R8V(J)=(VALS(I)-FACAU(2))/FACMU(2)
         WRITE (NFOUT,9) YNAM,(R8V(I),I=1,J)
      ELSE
         WRITE (NFOUT,9) YNAM,(VALS(I),I=M1,M2,2)
      ENDIF

      IF (N2.LT.NUMX) GO TO 3

   15 WRITE (NFOUT,*) ' '
      IF (ISUNT(1)) THEN
         UN=UNTI(1)
      ELSE
         UN=UNTS(1)
      ENDIF
      UB=' '
      IF (UN.NE.UB) THEN
         WRITE (NFOUT,12) XNAM,UN
      ELSE
         WRITE (NFOUT,14) XNAM
      ENDIF
   12 FORMAT(1X,A8,' HAS UNITS OF ',A50)
   14 FORMAT(1X,A8,' IS DIMENSIONLESS')

      IF (ISUNT(2)) THEN
         UN=UNTI(2)
      ELSE
         UN=UNTS(2)
      ENDIF
      IF (UN.NE.UB) THEN
         WRITE (NFOUT,12) YNAM,UN
      ELSE
         WRITE (NFOUT,14) YNAM
      ENDIF

      END
C*********************************************************************
      SUBROUTINE PRTSPL (NDEG,NINT,B,B1,BN,A,XNAM,YNAM)
C*********************************************************************

C  ROUTINE PRINTS SPLINE EQUATIONS

C  NDEG    = SPLINE DEGREE (INPUT, INTEGER)

C  NINT    = NUMBER OF SPLINE INTERVALS (INPUT, INTEGER)

C  B(I)    = LOCATION OF I'TH SPLINE NODE (INPUT, REAL*8)

C  B1      = LOWER BOUND OF DATA (INPUT, REAL*8)

C  BN      = UPPER BOUND OF DATA (INPUT, REAL*8)

C  A(I,J)  = I'TH COEFFICIENT IN THE J'TH INTERVAL
C            (INPUT, REAL*8)

C  XNAM,YNAM = X AND Y SYMBOLS (INPUT, CHARACTER*8)

C*********************************************************************
      REAL*8 A(4,*),B(*),B1,BN,DB1,DB2,DB3,DB4
      CHARACTER*8 XNAM,YNAM
      CHARACTER*64 FMA
      CHARACTER*1 FMB(64)

      INCLUDE 'control.h'
      INCLUDE 'scrat2.h'

      EQUIVALENCE (FMA,FMB)

      WRITE (NFOUT,1) XNAM,YNAM
    1 FORMAT(/' SPLINE FUNCTION WITH X = ',A8,' AND Y = ',A8)

      DO 2 J=1,NINT

      IF (J.EQ.1) THEN
         DB1=B1
      ELSE
         DB1=B(J-1)
      ENDIF
      IF (J.EQ.NINT) THEN
         DB2=BN
      ELSE
         DB2=B(J)
      ENDIF
      IF (ISUNT(1)) THEN
         DB1=(DB1-FACAU(1))/FACMU(1)
         DB2=(DB2-FACAU(1))/FACMU(1)
      ENDIF
      WRITE (NFOUT,5) DB1,DB2
    5 FORMAT (/' FOR X GREATER THAN OR EQUAL TO',G15.8,
     1 ' AND LESS THAN',G15.8)

C  CONVERT SPLINE COEFFICIENTS BACK TO USER'S INPUT UNITS

      DB1=A(1,J)
      DB2=A(2,J)
      DB3=A(3,J)
      IF (NDEG.EQ.3) THEN
         DB4=A(4,J)
      ELSE
         DB4=0.D0
      ENDIF
      IF (ISUNT(1)) THEN
         DB1=DB1+FACAU(1)*(DB2+FACAU(1)*(DB3+FACMU(1)*DB4))
         DB2=FACMU(1)*(DB2+FACAU(1)*(2.D0*DB3+3.D0*FACAU(1)*DB4))
         DB3=FACMU(1)*FACMU(1)*(DB3+3.D0*FACAU(1)*DB4)
         DB4=DB4*FACMU(1)**3
      ENDIF
      IF (ISUNT(2)) THEN
         DB1=(DB1-FACAU(2))/FACMU(2)
         DB2=DB2/FACMU(2)
         DB3=DB3/FACMU(2)
         DB4=DB4/FACMU(2)
      ENDIF

C  DRESS UP THE FORMAT

      FMA='('' Y(X)='',G14.8,'' +'',G13.8,'' X +'',G13.8,'' X**2 +'',G13
     1.8,'' X**3'')'
      IF (DB2.LT.0.D0) THEN
         DB2=-DB2
         FMB(19)='-'
      ENDIF
      IF (DB3.LT.0.D0) THEN
         DB3=-DB3
         FMB(32)='-'
      ENDIF
      IF (DB4.LT.0.D0) THEN
         DB4=-DB4
         FMB(48)='-'
      ENDIF

C  PRINT SPLINE COEFFICIENTS

      IF (NDEG.EQ.2) THEN
          FMB(48)=' '
          DO 7 K=50,63
    7     FMB(K)=' '
          WRITE (NFOUT,FMA) DB1,DB2,DB3
      ELSE
          WRITE (NFOUT,FMA) DB1,DB2,DB3,DB4
      ENDIF
    2 CONTINUE
      END
C*********************************************************************
      SUBROUTINE TABUNT (UNITSX,UNITSY)
C*********************************************************************
C  ROUTINE SETS DEFAULT UNITS FOR THE NEXT (ONLY) CALL TO TABLE.
C  ANY SPECIFICATION OF UNITS BY THE USER IN THE INPUT DATA REPLACES
C  THIS DEFAULT.

C  UNITSX  = DEFAULT EXTERNAL UNITS FOR X (INPUT, CHARACTER*1)

C  UNITSY  = DEFAULT EXTERNAL UNITS FOR Y (INPUT, CHARACTER*1)

C  NOTES:  1)  TERMINATE ARGUMENTS WITH A ]

C          2)  USE "[]" FOR DIMENSIONLESS UNITS

C*********************************************************************
      CHARACTER*1 UNITSX(*),UNITSY(*),UNTI1(60,3)

      INCLUDE 'utldat.h'
      INCLUDE 'scrat2.h'

      EQUIVALENCE (UNTI1(1,1),UNTI(1))

      ISUNTDT=.TRUE.
      UNTI(1)=' '
      DO 1 I=1,60
      UNTI1(I,1)=UNITSX(I)
      IF (UNITSX(I).EQ.']') GO TO 2
    1 CONTINUE

    2 UNTI(2)=' '
      DO 3 I=1,60
      UNTI1(I,2)=UNITSY(I)
      IF (UNITSY(I).EQ.']') RETURN
    3 CONTINUE
      END
