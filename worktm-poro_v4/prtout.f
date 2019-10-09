C  PRTOUT.F - PRINT UTILITIES

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE PRTTIT  (TITL)
C  SUBROUTINE MAKTIT  (T,NS,I)
C  SUBROUTINE GEAPRT  (IDIM, JDIM, KDIM, LDIM, IL1, IL2, JL1V, JL2V, KL1,
C                      KL2, KEYOUT, NBLK, ARRY, NEL, KIND, IEX, KARY)
C  SUBROUTINE R4GETS  (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
C                      KDIM,N,ARRY)
C  SUBROUTINE R8GETS  (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
C                      KDIM,N,ARRY)
C  SUBROUTINE I2GETS  (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
C                      KDIM,N,ARRY)
C  SUBROUTINE I4GETS  (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
C                      KDIM,N,ARRY)
C  SUBROUTINE PRTVEC4 (TIT,NVEC,VEC)

C  CODE HISTORY:

C  JOHN WHEELER      2/20/96    ALPHA CODE

C*********************************************************************
      SUBROUTINE PRTTIT (TITL)
C*********************************************************************

C   ROUTINE PRINTS A CENTERED TITLE WITH **** ON BOTH SIDES

C   TITL   = TABLE TITLE (INPUT, CHARACTER*50)

**********************************************************************
      CHARACTER*1 TITL(*),AST(40),BLK

      INCLUDE 'control.h'

      DATA AST/40*'*'/,BLK/' '/

      DO 1 I=1,50
      I1=I
      IF (TITL(I).NE.' ') GO TO 2
    1 CONTINUE
      I1=25
    2 DO 3 I=50,1,-1
      I2=I
      IF (TITL(I).NE.' ') GO TO 4
    3 CONTINUE
      I2=25
    4 IL=72-I2+I1
      IR=(IL+1)/2
      IL=IL/2
      WRITE (NFOUT,5) BLK,(AST(I),I=1,IL),BLK,BLK,(TITL(J),J=I1,I2),
     & BLK,BLK,(AST(K),K=1,IR)
    5 FORMAT(80A1)
      END
C*********************************************************************
      SUBROUTINE MAKTIT (T,NS,I)
C*********************************************************************

C  PUTS A NUMBER AT THE END OF A STRING

C  T = STRING (INPUT & OUTPUT, CHARACTER*1).

C  NS = MAX STRING LENGTH (INPUT, INTEGER)

C  I = NUMBER TO BE INSERTED (INPUT, INTEGER)
C      I MUST BE LESS THAN 10000

C*********************************************************************
      CHARACTER*1 T(*),TT1(6),BLK
      CHARACTER*6 TT
      EQUIVALENCE (TT1,TT)
      DATA BLK/' '/

      TT=' '
      IF (I.LT.10) THEN
         WRITE (TT,1) I
    1    FORMAT(I2)
      ELSE
         IF (I.LT.100) THEN
            WRITE (TT,2) I
    2       FORMAT(I3)
         ELSE
            IF (I.LT.1000) THEN
               WRITE (TT,3) I
    3          FORMAT(I4)
            ELSE
               WRITE (TT,4) I
    4          FORMAT(I5)
            ENDIF
         ENDIF
      ENDIF

      DO 5 J=NS,1,-1
      IF (T(J).NE.BLK) GO TO 6
    5 K1=J
    6 K2=K1+5
      IF (K2.GT.NS) K2=NS
      J=0
      DO 7 K=K1,K2
      J=J+1
    7 T(K)=TT1(J)

      END
C***********************************************************************
      SUBROUTINE GEAPRT (IDIM, JDIM, KDIM, LDIM, IL1, IL2, JL1V, JL2V,
     & KL1, KL2, KEYOUT, NBLK, ARRY, NEL, KIND, IEX, KARY)
C***********************************************************************

C  ROUTINE PRINTS ALL OR PART OF A GRID-ELEMENT ARRAY.  THIS IS NOT (REPEAT
C  NOT) A WORK ROUTINE.  IT IS CALLED ONLY BY GEAOUT.
C  THE VALUES TO BE PRINTED ARE DETERMINED BY THE GLOBAL INDEXES
C  I1AP,I2AP,ISAP,J1AP,J2AP,JSAP,K1AP,K2AP,KSAP IN COMMON /OUTPUT/
C  THE TABLE TITLE IS TITU IN COMMON /BLKARY/

C  ARRY  = ARRAY TO BE PRINTED (INPUT, NO TYPE)

C  NEL   = NUMBER OF ELEMENTS IN BLOCK NBLK THAT BELONG TO THE CURRENT
C          PROCESSOR (INPUT, INTEGER)

C  KIND = DATA TYPE OF THE ARRAY (INPUT, INTEGER)
C       = 1 ==> REAL*4
C       = 2 ==> REAL*8
C       = 3 ==> INTEGER
C       = 4 ==> INTEGER

C  IEX   = PRODUCT OF 4TH AND HIGHER INDEXES (INPUT, INTEGER)
C          (SET TO 1 IF THERE ARE NO HIGHER INDEXES)

C  KARY  = ARRAY KEY (INPUT, INTEGER)
C        = 1 ==> BLOCK CENTER ARRAY
C        = 2 ==> BLOCK CORNER ARRAY

C***********************************************************************
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'output.h'
      INCLUDE 'blkary.h'

      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM),JL(12)
      REAL*4 ARRY(IDIM,JDIM,KDIM)
      CHARACTER*50 TT

      IF (LEVELE.AND.BUGKEY(1)) WRITE (NFBUG,*)'PROC',MYPRC,
     & ' ENTERING SUBROUTINE GEAPRT'

C  PRINT THE ARRAY TITLE

      IF (LEVELC) THEN
         WRITE (NFOUT,*)
         TT=TITU
         CALL MAKTIT (TT,50,NBLK)
         CALL PRTTIT (TT)
      ENDIF

C  GET OFFSET FROM LOCAL TO GLOBAL INDEXES AND GLOBAL DIMENSIONS

      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,IERR)
      CALL BLKDIM(NBLK,IDIMG,JDIMG,KDIMG,IERR)
      IIEX=IEX
      IF (IIEX.LT.1) IIEX=1

C  LOOP OVER THE NUMBER OF REGIONS TO BE PRINTED

      DO 1 L=1,NUMREG(NBLK)

C  SET AND TEST GLOBAL INDEX RANGES TO BE PRINTED

      I1G=MAX(1,I1AP(NBLK,L))
      I2G=MIN(IDIMG,I2AP(NBLK,L))
      IF (I2G.LT.I1G) GO TO 1
      ITS=MAX(1,ISAP(NBLK,L))

      J1G=MAX(1,J1AP(NBLK,L))
      J2G=MIN(JDIMG,J2AP(NBLK,L))
      IF (J2G.LT.J1G) GO TO 1
      JTS=MAX(1,JSAP(NBLK,L))

      K1G=MAX(1,K1AP(NBLK,L))
      K2G=MIN(KDIMG,K2AP(NBLK,L))
      IF (K2G.LT.K1G) GO TO 1
      KTS=MAX(1,KSAP(NBLK,L))

      IF (KARY.EQ.2) THEN
         I2G=I2G+1
         J2G=J2G+1
         K2G=K2G+1
      ENDIF
      IF (KIND.LT.3) THEN
         JVPR=6
      ELSE
         JVPR=12
      ENDIF

    2 NJIND=0
      DO 3 J=J1G,J2G,JTS
    3 NJIND=NJIND+1
      NIIND=0
      DO 4 I=I1G,I2G,ITS
    4 NIIND=NIIND+1
      IF (NJIND*NIIND.GT.480) THEN
         IF (NJIND.LT.2.AND.NIIND.LT.2) GO TO 1
         IF (NJIND.GT.NIIND) THEN
            JTS=JTS+1
         ELSE
            ITS=ITS+1
         ENDIF
         GO TO 2
      ENDIF

      NL=NIIND*NJIND

C  GLOBAL K INDEX LOOP

      KGO=0
      DO 5 KD=K1G,K2G,KTS
      KG=KD
      IF (KG+KTS.GT.K2G) KG=K2G

C  PUT CURRENT K PLANE IN PRINT BUFFER A
C  PARALLEL VERSION USES BUFFER B IN THIS PROCESS

      IF (NUMPRC.EQ.1) THEN
         I1=I1G-IOFF
         I2=I2G-IOFF
         J1=J1G-JOFF
         J2=J2G-JOFF
         K=KG-KOFF
         GO TO (11,12,13,14),KIND
   11    CALL R4GETS (I1,I2,ITS,J1,J2,JTS,K,IIEX,KEYOUT,IDIM,JDIM,
     &      KDIM,NL,KARY,ARRY)
         GO TO 6
   12    CALL R8GETS (I1,I2,ITS,J1,J2,JTS,K,IIEX,KEYOUT,IDIM,JDIM,
     &      KDIM,NL,KARY,ARRY)
         GO TO 6
   13    CALL I2GETS (I1,I2,ITS,J1,J2,JTS,K,IIEX,KEYOUT,IDIM,JDIM,
     &      KDIM,NL,KARY,ARRY)
         GO TO 6
   14    CALL I4GETS (I1,I2,ITS,J1,J2,JTS,K,IIEX,KEYOUT,IDIM,JDIM,
     &      KDIM,NL,KARY,ARRY)

      ELSE
         CALL GETMP (I1G,I2G,ITS,IOFF,J1G,J2G,JTS,JOFF,KG,KOFF,
     &    IIEX,KEYOUT,IDIM,JDIM,KDIM,NBLK,KARY,KIND,ARRY)

      ENDIF
      IF (MYPRC.GT.0) GO TO 5

C  TEST FOR K PLANE IDENTICAL TO THE PREVIOUS K PLANE

    6 IF (KGO.GT.0) THEN
         GO TO (31,32,33,33),KIND
   31    DO 7 N=1,NL
         IF (PBUF4A(N).NE.PBUF4C(N)) GO TO 10
    7    CONTINUE
         GO TO 34
   32    DO 8 N=1,NL
         IF (PBUF8A(N).NE.PBUF8C(N)) GO TO 10
    8    CONTINUE
         GO TO 34
   33    DO 35 N=1,NL
         IF (IPBUF4A(N).NE.IPBUF4C(N)) GO TO 10
   35    CONTINUE

   34    WRITE (NFOUT,9) KG,KGO
    9    FORMAT(/' K =',I4,' PLANE IS IDENTICAL TO K =',I4,
     &    ' PLANE (FOR VALUES PRINTED)')
         GO TO 5
      ENDIF

C  PRINT UNIQUE K PLANE

   10 J2GG=J1G-JTS
      N=0

   16 J1GG=J2GG+JTS
      J2GG=J1GG
      DO 17 J=1,JVPR
      JP=J
      JL(J)=J2GG
      IF (J2GG.EQ.J2G.OR.J.EQ.JVPR) GO TO 18
      J2GG=J2GG+JTS
      IF (J2GG+JTS.GT.J2G) J2GG=J2G
   17 CONTINUE

   18 WRITE (NFOUT,19) KG
   19 FORMAT(/' K =',I4)

      IF (KIND.LT.3) THEN
         WRITE (NFOUT,20) (JL(J),J=1,JP)
   20    FORMAT('   I / J =',I6,5I12)
         DO 21 ID=I1G,I2G,ITS
         I=ID
         IF (I+ITS.GT.I2G) I=I2G
         IF (KIND.EQ.1) THEN
            WRITE (NFOUT,22) I,(PBUF4A(N+J),J=1,JP)
         ELSE
            WRITE (NFOUT,22) I,(PBUF8A(N+J),J=1,JP)
         ENDIF
   21    N=N+JP
   22    FORMAT(I5,2X,6G12.4)
      ELSE
         WRITE (NFOUT,23) (JL(J),J=1,JP)
   23    FORMAT('   I / J =',12I5)
         DO 24 ID=I1G,I2G,ITS
         I=ID
         IF (I+ITS.GT.I2G) I=I2G
         WRITE (NFOUT,25) I,(IPBUF4A(N+J),J=1,JP)
   24    N=N+JP
   25    FORMAT(I5,5X,12I5)
      ENDIF

      IF (J2GG.LT.J2G) GO TO 16

C  COPY BUFFER A TO BUFFER C

      KGO=KG
      GO TO (41,42,43,43),KIND
   41 DO 44 N=1,NL
   44 PBUF4C(N)=PBUF4A(N)
      GO TO 5
   42 DO 45 N=1,NL
   45 PBUF8C(N)=PBUF8A(N)
      GO TO 5
   43 DO 46 N=1,NL
   46 IPBUF4C(N)=IPBUF4A(N)

C  END K AND REGION LOOPS

    5 CONTINUE
    1 CONTINUE

      END
C***********************************************************************
      SUBROUTINE R4GETS (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
     & KDIM,N,KARY,ARRY)
C***********************************************************************

C  COPIES A REAL*4 VARIABLE TO A PRINT BUFFER FOR GEAOUT().
C  SINGLE PROCESSOR VERSION

C***********************************************************************
      INCLUDE 'output.h'
      REAL*4 ARRY(IDIM,JDIM,KDIM,*)
      INTEGER KEYOUT(IDIM,JDIM,KDIM)

      N=0
      DO 1 ID=I1,I2,ITS
      I=ID
      IF (I+ITS.GT.I2) I=I2

      DO 1 JD=J1,J2,JTS
      J=JD
      IF (J+JTS.GT.J2) J=J2
      N=N+1
      IF (KARY.EQ.2.OR.KEYOUT(I,J,K).EQ.1) THEN
         PBUF4A(N)=ARRY(I,J,K,IEX)
      ELSE
         PBUF4A(N)=0.
      ENDIF
    1 CONTINUE

      END
C***********************************************************************
      SUBROUTINE R8GETS (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
     & KDIM,N,KARY,ARRY)
C***********************************************************************

C  COPIES A REAL*8 VARIABLE TO A PRINT BUFFER FOR GEAOUT().
C  SINGLE PROCESSOR VERSION

C***********************************************************************
      INCLUDE 'output.h'
      REAL*8 ARRY(IDIM,JDIM,KDIM,*)
      INTEGER KEYOUT(IDIM,JDIM,KDIM)

      N=0
      DO 1 ID=I1,I2,ITS
      I=ID
      IF (I+ITS.GT.I2) I=I2
      DO 1 JD=J1,J2,JTS
      J=JD
      IF (J+JTS.GT.J2) J=J2
      N=N+1
      IF (KARY.EQ.2.OR.KEYOUT(I,J,K).EQ.1) THEN
         PBUF8A(N)=ARRY(I,J,K,IEX)
      ELSE
         PBUF8A(N)=0.
      ENDIF
    1 CONTINUE

      END
C***********************************************************************
      SUBROUTINE I2GETS (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
     & KDIM,N,KARY,ARRY)
C***********************************************************************

C  COPIES A INTEGER VARIABLE TO A PRINT BUFFER FOR GEAOUT().
C  SINGLE PROCESSOR VERSION

C***********************************************************************
      INCLUDE 'output.h'
      INTEGER ARRY(IDIM,JDIM,KDIM,*)
      INTEGER KEYOUT(IDIM,JDIM,KDIM)

      N=0
      DO 1 ID=I1,I2,ITS
      I=ID
      IF (I+ITS.GT.I2) I=I2
      DO 1 JD=J1,J2,JTS
      J=JD
      IF (J+JTS.GT.J2) J=J2
      N=N+1
      IF (KARY.EQ.2.OR.KEYOUT(I,J,K).EQ.1) THEN
         IPBUF4A(N)=ARRY(I,J,K,IEX)
      ELSE
         IPBUF4A(N)=0
      ENDIF
    1 CONTINUE

      END
C***********************************************************************
      SUBROUTINE I4GETS (I1,I2,ITS,J1,J2,JTS,K,IEX,KEYOUT,IDIM,JDIM,
     & KDIM,N,KARY,ARRY)
C***********************************************************************

C  COPIES A INTEGER VARIABLE TO A PRINT BUFFER FOR GEAOUT().
C  SINGLE PROCESSOR VERSION

C***********************************************************************
      INCLUDE 'output.h'
      INTEGER ARRY(IDIM,JDIM,KDIM,*)
      INTEGER KEYOUT(IDIM,JDIM,KDIM)

      N=0
      DO 1 ID=I1,I2,ITS
      I=ID
      IF (I+ITS.GT.I2) I=I2
      DO 1 JD=J1,J2,JTS
      J=JD
      IF (J+JTS.GT.J2) J=J2
      N=N+1
      IF (KARY.EQ.2.OR.KEYOUT(I,J,K).EQ.1) THEN
         IPBUF4A(N)=ARRY(I,J,K,IEX)
      ELSE
         IPBUF4A(N)=0
      ENDIF
    1 CONTINUE

      END
C***********************************************************************
      SUBROUTINE PRTVEC4 (TIT,NVEC,VEC)
C***********************************************************************

C  PRINTS A REAL*4 VECTOR WITH TITLE AND INDEXES

C  TIT  = VECTOR TITLE (INPUT, CHARACTER*50)

C  NVEC = NUMBER OF ELEMENTS IN THE VECTOR (INPUT, INTEGER)

C  VEC()= VECTOR TO BE PRINTED (INPUT, REAL*4)

C***********************************************************************
      INCLUDE 'control.h'
      REAL*4 VEC(NVEC)
      CHARACTER*50 TIT

      WRITE (NFOUT,*)
      CALL PRTTIT (TIT)

      I2=0
    1 I1=I2+1
      I2=I1+4
      IF (I2.GT.NVEC) I2=NVEC
      WRITE (NFOUT,2) (I,I=I1,I2)
    2 FORMAT(/I10,4I15)
      WRITE (NFOUT,3) (VEC(I),I=I1,I2)
    3 FORMAT(5G15.7)
      IF (I2.LT.NVEC) GO TO 1

      END
