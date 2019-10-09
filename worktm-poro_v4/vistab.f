C  VISTAB.F - 1D PLOT FOR TABLE INPUT (TECPLOT)

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE VTABINIT (NERR)
C  SUBROUTINE VTABDAT  (VALS,MAX2,XYNAM,TITL,MERR)
C  SUBROUTINE VTABFIT  (NTAB,X1,X2,XYNAM)
C  SUBROUTINE CPYLAYOUT()

C  HISTORY:

C  JOHN WHEELER      12/30/99     ORIGINAL BETA CODE

C*********************************************************************
      SUBROUTINE VTABINIT(NERR)
C*********************************************************************

C  ROUTINE INITIALIZES TECPLOT OUTPUT FOR TABLES

C  NERR = Error number steped by 1 on error (input & output, INTEGER)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'vistabc.h'

      CHARACTER*50 DUMNAM
      CHARACTER*1 TPPLT1(50),TPLAY1(50),DUMNAM1(50)
      EQUIVALENCE (TPPLT,TPPLT1(1)),(TPLAY,TPLAY1(1)),
     & (DUMNAM,DUMNAM1(1))

      TECPLT=.FALSE.
      TECLAY=.FALSE.
      NFTABBAS=12
      NFTABPLT=13
      NFTABLAY=14
      NPLOTS=0

C  GET FILE NAMES

      CALL GETVALS('TECPLOT ',TPPLT,'CS',0,0,0,50,NFP,NERR)
      CALL GETVALS('TECBASE ',TPBASE,'CS',0,0,0,50,NFB,NERR)
      IF (NFP.GT.0) THEN
         TECPLT=.TRUE.
      ELSE
         RETURN
      ENDIF
      IF (NFB.GT.0) TECLAY=.TRUE.

C  BUILD .PLT AND .LAY FILE NAMES

      J=47
      DO 1 I=47,1,-1
      IF (TPPLT1(I).NE.' ') GO TO 2
    1 J=I
    2 TPPLT1(J)='.'
      TPLAY=TPPLT
      TPPLT1(J+1)='P'
      TPPLT1(J+2)='L'
      TPPLT1(J+3)='T'
      TPLAY1(J+1)='L'
      TPLAY1(J+2)='A'
      TPLAY1(J+3)='Y'

      WRITE(NFOUT,5) (TPPLT1(I),I=1,J+3)
    5 FORMAT(' TECPLOT DATA OUTPUT FILE (TECPLOT)',T50,50A1)
      IF (TECLAY) WRITE(NFOUT,6) (TPLAY1(I),I=1,J+3)
    6 FORMAT(' TECPLOT LAYOUT OUTPUT FILE (TECPLOT)',T50,50A1)

C  OPEN PLOT FILE AND WRITE VARIABLES RECORD

      DUMNAM=TPPLT
      OPEN (NFTABPLT,FILE=TPPLT,STATUS='UNKNOWN',ERR=13)
      WRITE (NFTABPLT,3)
    3 FORMAT('VARIABLES = "X", "Y"')
      CLOSE(NFTABPLT)

C  OPEN LAYOUT FILE AND COPY LAYOUT HEADER

      IF (TECLAY) THEN
         DUMNAM=TPLAY
         OPEN (NFTABLAY,FILE=TPLAY,STATUS='UNKNOWN',ERR=13)
         DUMNAM=TPPLT
         J=50
         DO 15 I=50,2,-1
         IF (DUMNAM1(I).NE.' ') GO TO 16
   15    J=I
   16    DUMNAM1(J)='"'
         WRITE (NFTABLAY,4) (DUMNAM1(I),I=1,J)
    4    FORMAT('#!MC 700'/'$!VarSet |DATFIL| = "',50A1)
         CALL CPYLAYOUT()
         CLOSE (NFTABLAY)
      ENDIF

      RETURN

   13 WRITE(*,14) DUMNAM
   14 FORMAT (/' ERROR # 420; OPEN FAILED FOR FILE '/A50)
      TECPLT=.FALSE.

      END
C*********************************************************************
      SUBROUTINE VTABDAT(VALS,MAX2,XYNAM,TITL,MERR)
C*********************************************************************

C  ROUTINE OUTPUTS RAW DATA TO A TECPLOT XY DATA FILE

C  VALS() = XY DATA VECTOR (INPUT, REAL*8)

C  MAX2 = INDEX OF LAST X VALUE IN VALS() (INPUT, INTEGER)

C  XYNAM() = X AND Y VARIABLE NAMES (INPUT, CHARACTER*8)

C  TTIL    = TABLE TITLE (INPUT, CHARACTER*50)
C            MUST CONTAIN AT LEAST 1 NONBLANK CHARACTER IN THE FIRST 5
C            CHARACTERS TO BE PRINTED

C  MERR = ERROR KEY (INPUT AND OUTPUT, INTEGER)
C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'vistabc.h'

      REAL*8 VALS(*)
      CHARACTER*50 TITL,DUMLAB
      CHARACTER*8 XYNAM(2)
      CHARACTER*14 ZONAM
      CHARACTER*1 ZONAM1(14),DUMLAB1(50)
      EQUIVALENCE (ZONAM,ZONAM1(1)),(DUMLAB,DUMLAB1(1))

      IF (.NOT.TECPLT) RETURN
      NPLOTS=NPLOTS+1

C  OPEN PLOT FILE AND OUTPUT RAW DATA ZONE

      DUMLAB=TPPLT
      CLOSE (NFTABPLT)
C      OPEN (NFTABPLT,FILE=TPPLT,STATUS='OLD',POSITION='APPEND',ERR=13)
      OPEN (NFTABPLT,FILE=TPPLT,STATUS='OLD',ACCESS='APPEND',ERR=13)

      ZONAM=XYNAM(2)
      DO 1 I=1,8
      J=I
      IF (ZONAM1(I).EQ.' ') GO TO 2
    1 CONTINUE
      J=9
    2 ZONAM1(J+1)='D'
      ZONAM1(J+2)='A'
      ZONAM1(J+3)='T'
      ZONAM1(J+4)='A'
      ZONAM1(J+5)='"'

      WRITE(NFTABPLT,3) ZONAM
    3 FORMAT('ZONE T="',A14)
      WRITE(NFTABPLT,4) (MAX2+1)/2
    4 FORMAT(' I=',I4,', J=1, K=1, F=POINT, C=BLACK,',
     &   ' DT=(SINGLE SINGLE )')

      WRITE (NFTABPLT,5)(VALS(I),I=1,MAX2+1)
    5 FORMAT(6E12.5)

C  OPEN LAYOUT FILE, WRITE VARIABLES, AND WRITE FRAME DATA

      IF (.NOT.TECLAY) RETURN

      DUMLAB=TPLAY
      CLOSE (NFTABLAY)
C      OPEN (NFTABLAY,FILE=TPLAY,STATUS='OLD',POSITION='APPEND',ERR=13)
      OPEN (NFTABLAY,FILE=TPLAY,STATUS='OLD',ACCESS='APPEND',ERR=13)

      DUMLAB=TITL
      J=50
      DO 7 I=50,2,-1
      IF (DUMLAB1(I).NE.' ') GO TO 30
    7 J=I
   30 DUMLAB1(J)='"'
      WRITE (NFTABLAY,6) (DUMLAB1(I),I=1,J)
    6 FORMAT('$!VarSet |PLTTITLE| = "',50A1)

      DUMLAB=XYNAM(1)
      J=50
      DO 8 I=50,2,-1
      IF (DUMLAB1(I).NE.' ') GO TO 31
    8 J=I
   31 DUMLAB1(J)='"'
      WRITE (NFTABLAY,9) (DUMLAB1(I),I=1,J)
    9 FORMAT('$!VarSet |XAXTITLE| = "',50A1)

      DUMLAB=XYNAM(2)
      J=50
      DO 10 I=50,2,-1
      IF (DUMLAB1(I).NE.' ') GO TO 32
   10 J=I
   32 DUMLAB1(J)='"'
      WRITE (NFTABLAY,11) (DUMLAB1(I),I=1,J)
   11 FORMAT('$!VarSet |YAXTITLE| = "',50A1)

      WRITE (NFTABLAY,12) 2*NPLOTS-1
   12 FORMAT('$!VarSet |PLTZONE1| =',I3)
      WRITE (NFTABLAY,14) 2*NPLOTS
   14 FORMAT('$!VarSet |PLTZONE2| =',I3)

      CALL CPYLAYOUT()
      CLOSE (NFTABLAY)

      RETURN

   13 WRITE(NFOUT,4) DUMLAB
      WRITE(*,19) DUMLAB
   19 FORMAT (/' ERROR # 420; OPEN FAILED FOR FILE '/A50)
      MERR=1
      END
C*********************************************************************
      SUBROUTINE VTABFIT(NTAB,X1,X2,XYNAM)
C*********************************************************************

C  ROUTINE OUTPUTS FIT DATA TO A TECPLOT XY DATA FILE

C  NTAB = TABLE NUMBER (INPUT, INTEGER)

C  X1,X2 = FIRST AND LAST X VALUES TO BE PLOTTED (INPUT, REAL*8)

C  MAX2 = INDEX OF LAST X VALUE IN VALS() (INPUT, INTEGER)

C  XYNAM() = X AND Y VARIABLE NAMES (INPUT, CHARACTER*8)

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'vistabc.h'

      REAL*8 X,X1,X2,DX,V(6),DUB
      CHARACTER*8 XYNAM(2)
      CHARACTER*14 ZONAM
      CHARACTER*1 ZONAM1(14)
      EQUIVALENCE (ZONAM,ZONAM1(1))

      IF (.NOT.TECPLT) RETURN
      NPNT=120

      ZONAM=XYNAM(2)
      DO 6 I=1,8
      J=I
      IF (ZONAM1(I).EQ.' ') GO TO 7
    6 CONTINUE
      J=9
    7 ZONAM1(J+1)='F'
      ZONAM1(J+2)='I'
      ZONAM1(J+3)='T'
      ZONAM1(J+4)='"'

      WRITE(NFTABPLT,1) ZONAM
    1 FORMAT('ZONE T="',A14)
      WRITE(NFTABPLT,2) NPNT
    2 FORMAT(' I=',I4,', J=1, K=1, F=POINT, C=BLACK,',
     &   ' DT=(SINGLE SINGLE )')

      DX=(X2-X1)/(NPNT-1)

      X=X1-DX
      J=0
      DO 3 I=1,NPNT
      X=X+DX
      IF (ABS(X-X1).LT.1.D-10) X=X1
      IF (ABS(X-X2).LT.1.D-10) X=X2
      J=J+1
      V(J)=X
      J=J+1
      CALL LOOKUP(NTAB,X,V(J),DUB)
      IF (ABS(V(J)).LT.1.D-10) V(J)=0.D0
      IF (J.EQ.6) THEN
         WRITE(NFTABPLT,4) V
    4    FORMAT(6E12.5)
         J=0
      ENDIF
    3 CONTINUE
      IF (J.GT.0) WRITE(NFTABPLT,2) (V(K),K=1,J)

      CLOSE (NFTABPLT)
      END
C*********************************************************************
      SUBROUTINE CPYLAYOUT()
C*********************************************************************

C  ROUTINE COPIES THE LAYOUT HEADER OR A FRAME TO THE OUTPUT LAYOUT

C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'vistabc.h'

      CHARACTER*13 ENDFRM,REC13
      CHARACTER*100 REC100
      CHARACTER*1 REC1(100)
      EQUIVALENCE (REC100,REC1(1))

      DATA ENDFRM/'### END FRAME'/

C  OPEN THE SOURCE FILE AND READ NUMBER OF FRAMES/PAGE

      CLOSE (NFTABBAS)
      OPEN (NFTABBAS,FILE=TPBASE,STATUS='OLD',ERR=13)
      READ (NFTABBAS,1) NFPPG
    1 FORMAT(I2)

C  FIND THE STARTING POINT FOR THE COPY

      IF (NFRAM.GT.0) THEN
         NTG=MOD(NFRAM-1,NFPPG)+1
         N=0
         DO 2 I=1,1000
         READ (NFTABBAS,3) REC13
    3    FORMAT(A13)
         IF (REC13.EQ.ENDFRM) THEN
            N=N+1
            IF (N.EQ.NTG) GO TO 4
         ENDIF
    2    CONTINUE
      ENDIF
    4 NFRAM=NFRAM+1

C  COPY

      DO 5 I=1,1000
      READ (NFTABBAS,6) REC100
    6 FORMAT(A100)
      DO 7 J=100,2,-1
      K=J
      IF (REC1(J).NE.' ') GO TO 8
    7 CONTINUE
      K=1
    8 WRITE (NFTABLAY,9) (REC1(J),J=1,K)
    9 FORMAT(100A1)
      REC13=REC100
      IF (REC13.EQ.ENDFRM) RETURN
    5 CONTINUE

   13 WRITE(*,14) TPBASE
   14 FORMAT (/' ERROR # 420; OPEN FAILED FOR FILE '/A50)
      TECPLT=.FALSE.

      END
