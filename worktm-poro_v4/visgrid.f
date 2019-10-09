C  VISGRID.F - 3-D GRID PLOT FILE OUTPUT (TECPLOT)

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE VTSGRID (NERR)
C  SUBROUTINE TECGRID (IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,KL2,
C                      KEYOUT,NBLK)

C  HISTORY:

C  JOHN WHEELER      1/11/00     ORIGINAL BETA CODE

C*********************************************************************
      SUBROUTINE VISGRID()
C*********************************************************************

C  ROUTINE DRIVES OUTPUT OF A 3-D GRID PLOT FILE FOR TECPLOT

C  NOTE: THIS PROGRAM WILL NOT RUN ON MULTIPLE PROCESSORS

C*********************************************************************
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'wells.h'
      INCLUDE 'rockpg.h'

      INTEGER IARG(2)
      CHARACTER*50 GRIDFILE
      CHARACTER*1 GF1(50)
      EQUIVALENCE (GF1(1),GRIDFILE)
      EXTERNAL TECGRID

      NFGRID=15

      CALL GETVALS('GRIDFILE ',GRIDFILE,'CS',0,0,0,50,N,NERR)
      IF (N.EQ.0) RETURN

      DO 1 I=50,1,-1
      J=I
      IF (GF1(I).NE.' ') GO TO 2
    1 CONTINUE
      RETURN
    2 WRITE(NFOUT,3) (GF1(I),I=1,J)
    3 FORMAT(' TECPLOT GRID OUTPUT FILE (GRIDFILE)',T50,50A1)

      OPEN (NFGRID,FILE=GRIDFILE,STATUS='UNKNOWN',ERR=13)

C  OUTPUT HEADER

      WRITE (NFGRID,4)
    4 FORMAT('#!MC 700'/'VARIABLES = "X", "Y", "Z", "B"')

C  OUTPUT GRID

      IARG(1)=0
      CALL CALLWORK(TECGRID,IARG)

C  OUTPUT WELLS

      DO 5 I=1,NUMWEL
      NB=NBWELI(1,I)
      RNBLK=NB
      RNBLK=RNBLK+1.D-6
      X=ELEXYZ(1,1,I)
      Y=ELEXYZ(2,1,I)
      Z=ELEXYZ(3,1,I)
      IF (HDEPMOD) THEN
         DEP=0.
         CALL EXCDRV(NPG,KE)
         IF (KE.NE.0) THEN
            IF (LEVERR.LT.3) LEVERR=3
            IF (LEVELC) WRITE (NFOUT,9)
    9       FORMAT(' ERROR # 418, USER PROGRAM ERROR IN DEPTHMOD')
            RETURN
         ENDIF
         X=X+DOWN(1,NB)*DEP
         Y=Y+DOWN(2,NB)*DEP
         Z=Z+DOWN(3,NB)*DEP
      ENDIF
      X1=X-DOWN(1,NB)*50.
      Y1=Y-DOWN(2,NB)*50.
      Z1=Z-DOWN(3,NB)*50.
      WRITE(NFGRID,6) I
    6 FORMAT('ZONE T="WELL',I3,'"')
      WRITE(NFGRID,7)
    7 FORMAT(' I=2, J=1, K=1, F=POINT, C=BLACK,',
     &   ' DT=(SINGLE SINGLE SINGLE SINGLE )')
    5 WRITE (NFGRID,8) X1,Y1,Z1,RNBLK,X,Y,Z,RNBLK
    8 FORMAT(4F10.2)

      CLOSE(NFGRID)
      RETURN

   13 WRITE(*,14) GRIDFILE
   14 FORMAT (/' ERROR # 420; OPEN FAILED FOR FILE '/A50)

      END
C*********************************************************************
      SUBROUTINE TECGRID(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,KL2,
     &           KEYOUT,NBLK)
C*********************************************************************

C  ROUTINE OUTPUTS A 3-D GRID PLOT FILE FOR TECPLOT.  THIS IS A WORK ROUTINE

C*********************************************************************
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'rockpg.h'

      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      INTEGER IB(8),JB(8),KB(8)
      DATA IB/0,1,1,0,0,1,1,0/,JB/0,0,1,1,0,0,1,1/,KB/0,0,0,0,1,1,1,1/

      NFGRID=15

      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,MERR)
      RNBLK=NBLK
      RNBLK=RNBLK+1.D-6

      DO 1 K=KL1,KL2
      KG=K+KOFF
      JL1=JL1V(K)
      JL2=JL2V(K)
      DO 1 J=JL1,JL2
      JG=J+JOFF
      DO 1 I=IL1,IL2
      IF (KEYOUT(I,J,K).EQ.1.AND.(KEYOUT(I-1,J,K).EQ.0.OR.
     &   KEYOUT(I,J+1,K).EQ.0.OR.KEYOUT(I,J,K+1).EQ.0)) THEN

         WRITE(NFGRID,4)
    4    FORMAT('ZONE N=8, E=1, F=FEPOINT, ET=BRICK')
         IG=I+IOFF
         DO 5 L=1,8
         X=XREC(IG+IB(L),NBLK)
         Y=YREC(JG+JB(L),NBLK)
         Z=ZREC(KG+KB(L),NBLK)
         IF (HDEPMOD) THEN
            DEP=0.
            CALL EXCDRV(NPG,KE)
            IF (KE.NE.0) THEN
               IF (LEVERR.LT.3) LEVERR=3
               IF (LEVELC) WRITE (NFOUT,6)
    6          FORMAT(' ERROR # 418, USER PROGRAM ERROR IN DEPTHMOD')
               RETURN
            ENDIF
            X=X+DOWN(1,NBLK)*DEP
            Y=Y+DOWN(2,NBLK)*DEP
            Z=Z+DOWN(3,NBLK)*DEP
         ENDIF
    5    WRITE(NFGRID,7) X,Y,Z,NBLK
    7    FORMAT(3F10.2,I3)
         WRITE(NFGRID,8)
    8    FORMAT('1 2 3 4 5 6 7 8')

      ENDIF
    1 CONTINUE
      END
