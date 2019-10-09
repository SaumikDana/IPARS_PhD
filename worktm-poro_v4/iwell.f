C  IWELL.DF - INPUT WELL DATA

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE IWELLS   (NERR, KINP)
C  SUBROUTINE WELLIJK1 (IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
C                       KL2,KEYOUT,NBLK,NW,XPERM,YPERM,ZPERM)
C  SUBROUTINE PUTUTIT (TIT,EXU)
C  SUBROUTINE WELLCHECK (NERR)
C

C  CODE HISTORY:

C  JOHN WHEELER      1/29/96    ALPHA CODE
C  JOHN WHEELER      9/15/98    SUPPORT FOR NEW WELLBORE DENSITY CALCULATION
C  JOHN WHEELER     11/10/98    NEW WELL RATE UNITS
C  RICK DEAN        11/29/01    ADD CHECK FOR COMPOSITIONAL WELL RATES
C  SUNIL G THOMAS   08/22/07    ADDED MODS FOR CUMULATIVE WELL OUTPUTS
C                               AND COMPONENT WELL RATES.
C  GERGINA PENCHEVA  4/22/2016  ADDED STOP-ON-ERROR STATEMENTS AND ADDITIONAL
C                               WARNINGS ON INPUT DATA (IN THE OUTPUT FILE)

C***********************************************************************
      SUBROUTINE IWELLS (NERR, KINP)
C***********************************************************************

C  Routine inputs both initial and transient well data

C  NERR = Error number stepped by 1 on error (input & output, INTEGER)

C  KINP = Input type
C       = 1 ==> initial data
C       = 2 ==> transient data

C***********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'wells.h'
      INCLUDE 'blkary.h'
      INCLUDE 'layout.h'
      INCLUDE 'unitsex.h'
      INTEGER NERR,KINP

      INTEGER LOCW(50),LENW(50),NWI(50),KP(50),NA(5),
     & KWP(50),ITENS,IUNITS
     & ,NA2(11)
      REAL*4 WT(3,2,50),WB(3,2,50),WS(2,50),
     & WD(2,50)
      REAL*8 PLM(50)
      CHARACTER*1 BLKBUF(200000)
      CHARACTER*2  TAG
      CHARACTER*20 YLB
      CHARACTER*30 WFOLD,WFCOLD
      CHARACTER*50 TIT
      INTEGER I,J,K,N,NDUM,KWOUTO,NEWEL,IV,JV,KV,LV,NV

      EXTERNAL WELLIJK1

      IF (LEVELE.AND.BUGKEY(1)) WRITE (NFBUG,*)'PROC',MYPRC,
     & ' ENTERING SUBROUTINE IWELLS'

C  INITIALIZE CUMULATIVE WELL DATA

      IF(KINP.EQ.1) THEN
         DO I=1,50
            SWIC(I)=0.0D0
            SGIC(I)=0.0D0
            SOIC(I)=0.0D0
            SWPC(I)=0.0D0
            SOPC(I)=0.0D0
            SGPC(I)=0.0D0
C            DO J=1,2*(13+1)
C               SXC(J,I)=0.0D0
C            END DO
C            DO J=1,2*(13+1)
C               SXC(J,I)=0.0D0
C            END DO
         ENDDO
         SWIT=0.0D0
         SGIT=0.0D0
         SOIT=0.0D0
         SOPT=0.0D0
         SGPT=0.0D0
C         DO J=1,2*(13+1)
C            SXT(J)=0.0D0
C         END DO
C         DO J=1,2*(13+1)
C            SXT(J)=0.0D0
C         END DO
      ENDIF

C  INITIAL INPUT ONLY

      IF (KINP.EQ.1) THEN

         NUMWEL=0
         CALL GETVAL('NUMWELL ',NUMWEL,'I4',0,0,0,0,NDUM,NERR)
         IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'NUMWELL'
         IF (NUMWEL.LT.1) RETURN
         IF (NUMWEL.GT.50) THEN
            NERR=NERR+1
            N=50
            IF (LEVELC) WRITE (NFOUT,10) NUMWEL,N
   10       FORMAT(' ERROR 509; NUMBER OF WELLS (',I4,') EXCEEDS',I4)
            NUMWEL=50
         ENDIF
   77    FORMAT(/' Input file ERROR for ',A)

         DO I=1,NUMWEL
         WELBHP(I)=0.D0
         WELDEN(I)=0.D0
         WELNAM(I)="WELL"
         NWELPRC(I)=-1
         CALL MAKTIT (WELNAM(I),40,I)
         KWELL(I)=0
         KWPERM(I)=2
         PLIMIT(I)=0.D0
         NWELLI(I)=1
         NTABPQ(I)=0
         NUMELE(I)=0
         NUMELET(I)=0
         WOUTFLG(I)=.TRUE.
         WELL_ALLOW_SHUTOFF(I)=.TRUE.
         DO J=1,2
         NBWELI(J,I)=1
         ! SAUMIK - TEMPORARY FIX SETTING WELLBLOCK FOR BIG CRANFIELD PROBLEM
         IF(MBPOROE) NBWELI(J,I)=2
         WELSKIN(J,I)=0.
         WELDIAM(J,I)=.5
         DO K=1,3
         WELLTOP(K,J,I)=-99.
         WELLBOT(K,J,I)=-99.
         ENDDO
         ENDDO
         ENDDO

         KHISOUT=1
         MYHIS=0
         KWFILE=0
         KWCF=0
         WELFILE='WELLS.OUT'
         WFCUM='WELLCUM.OUT'
         DO J=0,18
         DO I=1,100000
         KNDHIS(I,J)=0
         ENDDO
         ENDDO

         IF (LEVELC) THEN
            WRITE (NFOUT,*)
            TITU='******'
            CALL PRTTIT(TITU)
            TITU='WELL DATA'
            CALL PRTTIT(TITU)
         ENDIF
         CALL GETVALS('WELLNAME ',WELNAM,'CS',NUMWEL,0,0,40,NDUM,
     &    NERR)
         IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLNAME'
         DO J=1,100
         DO I=1,6
         WELGEOM(I,J)=0.
         ENDDO
         ENDDO
         CALL GETVAL('WELLGEOM ',WELGEOM,'R4',6,100,0,0,NDUM,NERR)
         IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLGEOM'
         NWELG=0
         DO J=1,100
         IF (WELGEOM(5,J).GT.0.) NWELG=J
         ENDDO

         CALL GETVAL('WELLBLOCK ',NBWELI,'I4',2,NUMWEL,0,0,NDUM,
     &      NERR)
         IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLBLOCK'
         CALL GETVAL('INTERVALS ',NWELLI,'I4',NUMWEL,0,0,0,NDUM,NERR)
         IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'INTERVALS'

! bag8
         CALL GETVAL('WELL_ALLOW_SHUTOFF ',WELL_ALLOW_SHUTOFF,'L4',
     &      2,NUMWEL,0,0,NDUM,NERR)
         IF ((NERR.GT.0).AND.LEVELC)
     &     WRITE(NFOUT,77) 'WELL_ALLOW_SHUTOFF'


         NTYPOUT=MIN(4,NUMWEL)
         CALL GETVAL('TYPICAL ',NTYPOUT,'I4',0,0,0,0,NDUM,NERR)
         IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'TYPICAL'
         IF (NTYPOUT.GT.NUMWEL) NTYPOUT=NUMWEL

      ENDIF

C  BOTH INITIAL AND TRANSIENT INPUT

      IF (NUMWEL.LT.1) RETURN

      TITHIS(1)="WATER INJECTION RATE"
      CALL PUTUTIT(TITHIS(1),EXTWELL)
      TITHIS(2)="OIL PRODUCTION RATE"
      CALL PUTUTIT(TITHIS(2),EXTWELL)
      TITHIS(3)="WATER PRODUCTION RATE"
      CALL PUTUTIT(TITHIS(3),EXTWELL)
      TITHIS(4)="GAS PRODUCTION RATE"
      CALL PUTUTIT(TITHIS(4),EXTWELG)
      TITHIS(5)="WATER/OIL RATIO"
      TITHIS(6)="GAS/OIL RATIO"
      TITHIS(7)="BOTTOM HOLE PRESSURE"
      CALL PUTUTIT(TITHIS(7),EXTPRES)
      TITHIS(8)="GAS INJECTION RATE"
      CALL PUTUTIT(TITHIS(8),EXTWELG)
      TITHIS(9)="OIL INJECTION RATE"
      CALL PUTUTIT(TITHIS(9),EXTWELL)
C      DO I=1,12
C         IF((9+2*I).GT.34) THEN
C            NERR=NERR+1
C            IF (LEVELC) WRITE (NFOUT,101) 34,34+2*(12-I+1)
C  101       FORMAT(' ERROR 509; TOO FEW DATA KINDS (',I4,'), TRY ',I4)
C            STOP 13
C         ENDIF
C         IF(I+1.LT.10) THEN
C            TAG=CHAR(48+(I+1))
C         ELSEIF((I+1).GE.10.AND.(I+1).LT.100) THEN
C            ITENS=(I+1)/10
C            IUNITS=MOD(MOD(I+1,ITENS),10)
C            TAG=CHAR(48+ITENS)//CHAR(48+IUNITS)
C         ENDIF
C         TITHIS(9+2*I-1)="INJECTION RATE, COMPONENT "//TAG
C         CALL PUTUTIT(TITHIS(9+2*I-1),EXTWELX)
C         TITHIS(9+2*I)="PRODUCTION RATE, COMPONENT "//TAG
C         CALL PUTUTIT(TITHIS(9+2*I),EXTWELX)
C      ENDDO
C      IF((NUMWEL*3*2).GT.100000) THEN
C         NERR=NERR+1
C         IF (LEVELC) WRITE (NFOUT,102) 100000,NUMWEL*3*2
C  102    FORMAT(' ERROR 509; TOO FEW HISQ (',I4,'), TRY ',I4)
C         STOP 13
C      ENDIF

C      DO I=1,12
C         IF((9+2*I).GT.34) THEN
C            NERR=NERR+1
C            IF (LEVELC) WRITE (NFOUT,101) 34,34+2*(12-I+1)
C  101       FORMAT(' ERROR 509; TOO FEW DATA KINDS (',I4,'), TRY ',I4)
C            STOP 13
C         ENDIF
C         IF(I+1.LT.10) THEN
C            TAG=CHAR(48+(I+1))
C         ELSEIF((I+1).GE.10.AND.(I+1).LT.100) THEN
C            ITENS=(I+1)/10
C            IUNITS=MOD(MOD(I+1,ITENS),10)
C            TAG=CHAR(48+ITENS)//CHAR(48+IUNITS)
C         ENDIF
C         TITHIS(9+2*I-1)="INJECTION RATE, COMPONENT "//TAG
C         CALL PUTUTIT(TITHIS(9+2*I-1),EXTWELX)
C         TITHIS(9+2*I)="PRODUCTION RATE, COMPONENT "//TAG
C         CALL PUTUTIT(TITHIS(9+2*I),EXTWELX)
C      ENDDO
C      IF((NUMWEL*3*2).GT.100000) THEN
C         NERR=NERR+1
C         IF (LEVELC) WRITE (NFOUT,102) 100000,NUMWEL*3*2
C  102    FORMAT(' ERROR 509; TOO FEW HISQ (',I4,'), TRY ',I4)
C         STOP 13
C      ENDIF

      TITHISC(1)="WATER CUMULATIVE INJECTION"
      CALL PUTUTIT(TITHISC(1),EXTWELLC)
      TITHISC(2)="OIL CUMULATIVE PRODUCTION"
      CALL PUTUTIT(TITHISC(2),EXTWELLC)
      TITHISC(3)="WATER CUMULATIVE PRODUCTION"
      CALL PUTUTIT(TITHISC(3),EXTWELLC)
      TITHISC(4)="GAS CUMULATIVE PRODUCTION"
      CALL PUTUTIT(TITHISC(4),EXTWELGC)
      TITHISC(8)="GAS CUMULATIVE INJECTION"
      CALL PUTUTIT(TITHISC(8),EXTWELGC)
      TITHISC(9)="OIL CUMULATIVE INJECTION"
      CALL PUTUTIT(TITHISC(9),EXTWELLC)
C      DO I=1,12
C         IF(I+1.LT.10) THEN
C            TAG=CHAR(48+(I+1))
C         ELSEIF((I+1).GE.10.AND.(I+1).LT.100) THEN
C            ITENS=(I+1)/10
C            IUNITS=MOD(MOD(I+1,ITENS),10)
C            TAG=CHAR(48+ITENS)//CHAR(48+IUNITS)
C         ENDIF
C         TITHISC(9+2*I-1)="CUMULATIVE INJECTION, COMPONENT "//TAG
C         CALL PUTUTIT(TITHISC(9+2*I-1),EXTWELXC)
C         TITHISC(9+2*I)="CUMULATIVE PRODUCTION, COMPONENT "//TAG
C         CALL PUTUTIT(TITHISC(9+2*I),EXTWELXC)
C      ENDDO

C      DO I=1,12
C         IF(I+1.LT.10) THEN
C            TAG=CHAR(48+(I+1))
C         ELSEIF((I+1).GE.10.AND.(I+1).LT.100) THEN
C            ITENS=(I+1)/10
C            IUNITS=MOD(MOD(I+1,ITENS),10)
C            TAG=CHAR(48+ITENS)//CHAR(48+IUNITS)
C         ENDIF
C         TITHISC(9+2*I-1)="CUMULATIVE INJECTION, COMPONENT "//TAG
C         CALL PUTUTIT(TITHISC(9+2*I-1),EXTWELXC)
C         TITHISC(9+2*I)="CUMULATIVE PRODUCTION, COMPONENT "//TAG
C         CALL PUTUTIT(TITHISC(9+2*I),EXTWELXC)
C      ENDDO


      KWOUTO=KHISOUT
      CALL GETVAL('WELLOUTKEY ',KHISOUT,'I4',0,0,0,0,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLOUTKEY'

      WFOLD=WELFILE
      CALL GETVALS('WELLFILE ',WELFILE,'CS',0,0,0,30,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLFILE'

      WFCOLD=WFCUM
      CALL GETVALS('WELLFCUM ',WFCUM,'CS',0,0,0,30,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLFCUM'

      IF (LEVELC) THEN
         IF (KWOUTO.NE.KHISOUT.OR.KINP.EQ.1) WRITE (NFOUT,21) KHISOUT
   21    FORMAT(/' WELL HISTORY OUTPUT KEY (WELLOUTKEY)',T49,I10)
         IF (WFOLD.NE.WELFILE.OR.KINP.EQ.1) WRITE (NFOUT,22) WELFILE
   22    FORMAT(/' WELL HISTORY FILE NAME (WELLFILE)',T49,A30)
         IF (WFCOLD.NE.WFCUM.OR.KINP.EQ.1) WRITE (NFOUT,23) WFCUM
   23    FORMAT(/' WELL CUMULATIVE HISTORY FILE NAME (WFCUM)',T49,A30)
      ENDIF

      IF (WFOLD.NE.WELFILE.AND.KWFILE.EQ.1) THEN
         KWFILE=0
      ENDIF
      IF (KHISOUT.GT.1.AND.KWFILE.EQ.0) THEN
         CLOSE (NFWELL)
         OPEN (NFWELL,FILE=WELFILE,STATUS='UNKNOWN',ERR=13)
         KWFILE=1
      ENDIF

      IF (WFCOLD.NE.WFCUM.AND.KWCF.EQ.1) THEN
         KWCF=0
      ENDIF
      IF (KHISOUT.GT.1.AND.KWCF.EQ.0) THEN
         CLOSE (NWFCUM)
         OPEN (NWFCUM,FILE=WFCUM,STATUS='UNKNOWN',ERR=19)
         KWCF=1
      ENDIF

      DO I=1,NUMWEL
      KP(I)=KINP
      KWP(I)=KWPERM(I)
      PLM(I)=PLIMIT(I)
      NWI(I)=NWELLI(I)
      ENDDO

      DO I=1,NUMWEL
      IF (NWELLI(I).NE.NWI(I)) KP(I)=1
      NWI(I)=KWELL(I)
      DO J=1,NWELLI(I)
      WS(J,I)=WELSKIN(J,I)
      WD(J,I)=WELDIAM(J,I)
      DO K=1,3
      WT(K,J,I)=WELLTOP(K,J,I)
      WB(K,J,I)=WELLBOT(K,J,I)
      ENDDO
      ENDDO
      ENDDO

      CALL GETVAL('KINDWELL ',KWELL,'I4',NUMWEL,0,0,0,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'KINDWELL'
      CALL GETVAL('KINDPERM ',KWPERM,'I4',NUMWEL,0,0,0,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'KINDPERM'
      CALL GETVAL('PLIMIT ',PLIMIT,'R8',NUMWEL,0,0,0,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'PLIMIT'
      CALL DEFAULT(EXTDIST)
      CALL GETVAL('WELLTOP[ft] ',WELLTOP,'R4',3,2,NUMWEL,0,
     &   NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLTOP'
      CALL DEFAULT(EXTDIST)
      CALL GETVAL('WELLBOTTOM[ft] ',WELLBOT,'R4',3,2,NUMWEL,0,
     &   NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WELLBOTTOM'
      CALL DEFAULT(EXTDIST)
      CALL GETVAL('DIAMETER[ft] ',WELDIAM,'R4',2,NUMWEL,0,0,
     &   NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'DIAMETER'
      CALL GETVAL('SKIN ',WELSKIN,'R4',2,NUMWEL,0,0,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'SKIN'
      CALL GETVAL('WOUTFLG ',WOUTFLG,'L4',NUMWEL,0,0,0,NDUM,NERR)
      IF ((NERR.GT.0).AND.LEVELC) WRITE(NFOUT,77) 'WOUTFLG'

C  LOCATE WELL ELEMENTS ON THE CURRENT PROCESSOR AND COMPUTE WELL ELEMENT
C  PRODUCTIVITY INDEX CONSTANT FACTORS

      NA(1)=4
      NA(2)=N_KPU
      NA(3)=N_XPERM
      NA(4)=N_YPERM
      NA(5)=N_ZPERM

      NEWEL=0
      DO 3 I=1,NUMWEL
      IF (KWELL(I).NE.NWI(I)) KP(I)=1
      IF (KWPERM(I).NE.KWP(I)) KP(I)=1
      IF (PLIMIT(I).NE.PLM(I)) KP(I)=1
      DO 4 J=1,NWELLI(I)
      IF (WELSKIN(J,I).NE.WS(J,I)) KP(I)=1
      IF (WELDIAM(J,I).NE.WD(J,I)) KP(I)=1
      DO 5 K=1,3
      IF (WELLTOP(K,J,I).NE.WT(K,J,I)) KP(I)=1
      IF (WELLBOT(K,J,I).NE.WB(K,J,I)) KP(I)=1
    5 CONTINUE
    4 CONTINUE

      IF (KP(I).EQ.1) THEN

         DEPBOT(I)=-1.D20
         DEPTOP(I)=1.D20
         NEWEL=1
         KPU=I
         NUMELE(I)=0
         NUMELET(I)=0
         IF (KNDGRD.EQ.1) THEN
            CALL CALLWORK(WELLIJK1,NA)
         ELSEIF (KNDGRD.EQ.3) THEN
               CALL CALLWELLIJK3()
         ENDIF


         DO 16 J=1,NWELG
         LV=WELGEOM(5,J)+.001
         IF (I.EQ.LV.AND.WELGEOM(6,J).GT.0.) THEN
            IV=WELGEOM(1,J)+.001
            JV=WELGEOM(2,J)+.001
            KV=WELGEOM(3,J)+.001
            NV=WELGEOM(4,J)+.001
            DO 17 K=1,NUMELE(I)
            IF (IV.EQ.LOCWEL(3,K,I).AND.JV.EQ.LOCWEL(4,K,I).AND.
     &         KV.EQ.LOCWEL(4,K,I).AND.NV.EQ.LOCWEL(1,K,I))
     &         ELEGEOM(K,I)=WELGEOM(6,J)
   17       CONTINUE
         ENDIF
   16    CONTINUE

         DO 18 K=1,NUMELE(I)
   18    ELECONS(K,I)=ELELEN(K,I)*ELEPERM(K,I)*ELEGEOM(K,I)

C  DISTRIBUTE WELL DATA TO ALL PROCESSORS

      CALL WELLSHR (I)

      ENDIF

    3 CONTINUE

C  SINGLE PROCESSOR CHECK FOR PROPER WELL LOCATIONS

      IF (KINP.EQ.1) CALL WELLCHECK (NERR)
      IF(NERR.NE.0.AND.LEVERR.GT.2) GOTO 39

C  ASSIGN OWNERSHIP OF EACH WELL TO A SINGLE PROCESSOR

      IF (KINP.EQ.1) CALL WELOWN ()

C  ASSIGN OWNERSHIP OF EACH WELL TO A SINGLE PHYSICAL MODEL

      DO 20 I=1,NUMWEL
   20 MODWEL(I)=FMODBLK(NBWELI(1,I))

C  PRINT WELL DATA

      DO 15 I=1,NUMWEL

      IF ((KP(I).EQ.1).AND.LEVELC) THEN
         WRITE (NFOUT,6) WELNAM(I)
    6    FORMAT(/1X,A40)

         IF (KWELL(I).LT.31) THEN
            IF (KWELL(I).EQ.0) WRITE (NFOUT,*)
     &         'WELL TYPE 0 - SHUT IN'
            IF (KWELL(I).EQ.1) WRITE (NFOUT,*)
     &         'WELL TYPE 1 - WATER INJECTION, PRESSURE SPECIFIED'
            IF (KWELL(I).EQ.2) WRITE (NFOUT,*)
     &         'WELL TYPE 2 - WATER INJECTION, MASS RATE SPECIFIED'
            IF (KWELL(I).EQ.3) WRITE (NFOUT,*)
     &         'WELL TYPE 3 - GAS INJECTION, PRESSURE SPECIFIED'
            IF (KWELL(I).EQ.4) WRITE (NFOUT,*)
     &         'WELL TYPE 4 - GAS INJECTION, MASS RATE SPECIFIED'
            IF (KWELL(I).EQ.5) WRITE (NFOUT,*)
     &         'WELL TYPE 5 - WATER INJEC., RES. VOLUME RATE SPECIFIED'
            IF (KWELL(I).EQ.6) WRITE (NFOUT,*)
     &         'WELL TYPE 6 - GAS INJEC., RES. VOLUME RATE SPECIFIED'
            IF (KWELL(I).EQ.7) WRITE (NFOUT,*)
     &         'WELL TYPE 7 - WATER INJEC., CUM. MASS RATE SPECIFIED'
            IF (KWELL(I).EQ.8) WRITE (NFOUT,*)
     &         'WELL TYPE 8 - GAS INJEC., CUM. MASS RATE SPECIFIED'

            IF (KWPERM(I).EQ.1) WRITE (NFOUT,*)
     &         'MINIMUM INJEC. RELATIVE PERM. = .05'
            IF (KWPERM(I).EQ.2) WRITE (NFOUT,*)
     &         'MINIMUM INJEC. RELATIVE PERM. = SUM OF OTHER PERMS.'

cgp
            IF (KWELL(I).EQ.2) WRITE (NFOUT,49) PLIMIT(I)
   49       FORMAT(' UPPER LIMIT ON BHP (PLIMIT)',T48,G12.2,' PSI')
cgp
         ELSE
            IF (KWELL(I).EQ.31) WRITE (NFOUT,*)
     &         'WELL TYPE 31 - PRODUCTION, PRESSURE SPECIFIED'
            IF (KWELL(I).EQ.32) WRITE (NFOUT,*)
     &         'WELL TYPE 32 - PRODUCTION, TOTAL MASS RATE SPECIFIED'
            IF (KWELL(I).EQ.33) WRITE (NFOUT,*)
     &         'WELL TYPE 33 - PRODUCTION, OIL MASS RATE SPECIFIED'
            IF (KWELL(I).EQ.34) WRITE (NFOUT,*)
     &         'WELL TYPE 34 - PRODUCTION, GAS MASS RATE SPECIFIED'
            IF (KWELL(I).EQ.35) WRITE (NFOUT,*)
     &         'WELL TYPE 35 - PRODUCTION, WATER MASS RATE SPECIFIED'
            IF (KWELL(I).EQ.36) WRITE (NFOUT,*)
     &         'WELL TYPE 36 - PRODUCTION, WATER+OIL MASS RATE SPEC.'
            IF (KWELL(I).EQ.37) WRITE (NFOUT,*)
     &         'WELL TYPE 37 - PRODUCTION, TOTAL RES. VOLUME RATE SPEC.'
            IF (KWELL(I).EQ.38) WRITE (NFOUT,*)
     &         'WELL TYPE 38 - PRODUCTION, OIL RES. VOLUME RATE SPEC.'
            IF (KWELL(I).EQ.39) WRITE (NFOUT,*)
     &         'WELL TYPE 39 - PRODUCTION, GAS RES. VOLUME RATE SPEC.'
            IF (KWELL(I).EQ.40) WRITE (NFOUT,*)
     &         'WELL TYPE 40 - PRODUCTION, CUM. TOTAL MASS RATE SPEC.'
            IF (KWELL(I).EQ.41) WRITE (NFOUT,*)
     &         'WELL TYPE 41 - PRODUCTION, CUM. OIL MASS RATE SPEC.'
            IF (KWELL(I).EQ.42) WRITE (NFOUT,*)
     &         'WELL TYPE 42 - PRODUCTION, CUM. GAS MASS RATE SPEC.'
            IF (KWELL(I).EQ.43) WRITE (NFOUT,*)
     &         'WELL TYPE 43 - PRODUCTION, CUM. WATER MASS RATE SPEC.'

            IF (KWELL(I).GT.31) WRITE (NFOUT,50) PLIMIT(I)
   50       FORMAT(' LOWER LIMIT ON BHP (PLIMIT)',T48,G12.2,' PSI')
         ENDIF

         DO 7 J=1,NWELLI(I)
    7    WRITE (NFOUT,8) J,WELDIAM(J,I),WELSKIN(J,I)*
     &      CVMDIST,(WELLTOP(K,J,I)*CVMDIST,K=1,3),
     &      (WELLBOT(K,J,I)*CVMDIST,K=1,3)
    8    FORMAT(' INTERVAL',I3,', DIAMETER =',G11.4,'ft, SKIN =',G11.4/
     &      T15,'WELL TOP X,Y,Z    =',3G15.5/
     &      T15,'WELL BOTTOM X,Y,Z =',3G15.5)

         WRITE (NFOUT,12)
   12    FORMAT(' BLOCK INTERVAL   I   J   K')
         DO 11 J=1,NUMELET(I)
   11    WRITE (NFOUT,14) (LOCWEL(K,J,I),K=1,5)
   14    FORMAT(I4,I8,I7,2I4)

      ENDIF

   15 CONTINUE

C  EXTRACT WELL BHP/RATE AND FORM TABLES
C  NOTE THAT THE BLACK-OIL MODEL USES STOCKTANK INTERNAL UNITS RATHER THAN
C  POUNDS.  SEE ABOVE OR WELLS.DH FOR DEFINITIONS OF KWELL()

      DO 1 I=1,NUMWEL
      LOCW(I)=0
    1 LENW(I)=0
      CALL GETBLK('WELLPQ ',BLKBUF,200000,NUMWEL,LOCW,LENW,NERR)
      IF ((NERR.GT.0).AND.LEVELC)
     &   WRITE(NFOUT,'(A,I3)') 'Error reading WELLPQ in well ',I
      DO 2 I=1,NUMWEL
      IF (LENW(I).GT.0) THEN
         YLB=' '

         IF (KWELL(I).LT.31) THEN
C                  0  1  2  3  4  5  6
            GOTO (40,41,42,41,44,43,43),KWELL(I)+1
         ELSE
C                 31 32 33 34 35 36 37 38 39
            GOTO (41,42,42,44,42,42,43,43,43),KWELL(I)-30
         ENDIF

   41    YLB='BHP[psi]'
         CALL TABUNT (EXTTIME,EXTPRES)
         GO TO 40

   42    IF (MODWEL(I).EQ.2 .OR. MODWEL(I).EQ.3
     &       .OR.MODWEL(I).EQ.16) THEN
            YLB='RATE[stb/day]'
         ELSE
            YLB='RATE[lb/day]'
         ENDIF
         CALL TABUNT (EXTTIME,EXTWELL)
         GO TO 40

   43    YLB='RATE[cu-ft/day]'
         CALL TABUNT (EXTTIME,'[cu-ft/day]         ')
         GO TO 40

   44    IF (MODWEL(I).EQ.2 .OR. MODWEL(I).EQ.3
     &       .OR.MODWEL(I).EQ.16) THEN
            YLB='RATE[mscf/day]'
         ELSE
            YLB='RATE[lb/day]'
         ENDIF
         CALL TABUNT (EXTTIME,EXTWELG)

   40    TIT=WELNAM(I)
         CALL TABLE(BLKBUF(LOCW(I)),LENW(I),'TIME[day] ',YLB,TIT,
     &    NTABPQ(I),NERR)
         IF ((NERR.GT.0).AND.LEVELC)
     &      WRITE(NFOUT,'(A,I3)') 'Error in TIME for well ',I
      ENDIF
    2 CONTINUE

C  INPUT OF MODEL SPECIFIC WELL DATA

c GP: this call was missing for TRCHEM model
C      MODACT=14
C      IF (MODELON(14)) THEN
C         CALL TRWDATA(NERR,KINP)
C         GOTO 66
C      ENDIF

C      MODACT=7
C      IF (MODELON(7)) THEN
C         CALL MWDATA(NERR,KINP)
C         GOTO 66
C      ENDIF

C      MODACT=2
C      IF (MODELON(2)) CALL IWDATA(NERR,KINP)
C      MODACT=3
C      IF (MODELON(3)) CALL XWDATA(NERR,KINP)
! saumik
C      MODACT=16
C      IF (MODELON(16).OR.MBPOROE) CALL XWDATA(NERR,KINP)
C      MODACT=5
C      IF (MODELON(5)) CALL HWDATA(NERR,KINP)
C      MODACT=19
C      IF (MODELON(19)) CALL HWDATA(NERR,KINP)
C      MODACT=18
C      IF (MODELON(18)) CALL HWDATA(NERR,KINP)
C      MODACT=13
C      IF (MODELON(13)) CALL TWDATA(NERR,KINP)
! saumik
      MODACT=17
      IF (MODELON(17).OR.MBPOROE) CALL TWDATA(NERR,KINP)

   66 MODACT=0

      RETURN

   13 NERR=NERR+1
      WRITE (NFOUT,27) WELFILE
   27 FORMAT (/' ERROR # 431; OPEN FILE FAILED FOR '/A30)
      GOTO 29

   19 NERR=NERR+1
      WRITE (NFOUT,28) WFCUM
   28 FORMAT (/' ERROR # 431; OPEN FILE FAILED FOR '/A30)
      GOTO 29

   29 CONTINUE
      RETURN

   39 NERR=NERR+1
      RETURN

      END
C*********************************************************************
      SUBROUTINE WELLIJK1 (IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                     KL2,KEYOUT,NBLK,NW,XPERM,YPERM,ZPERM)
C*********************************************************************

C  Routine locates the grid elements of a well for the orthogonal
C  grid option.  Assigns well to a processor.  Computes open interval,
C  permeability normal to the wellbore, and default geometric factor
C  for each element penatrated.  This is a work routine.

C  NW = Well number (input, INTEGER)

C  XPERM(I,J,K),YPERM(I,J,K),ZPERM(I,J,K) = Element permeabilities in
C  the x,y, and z directions.

C MORE THAN ONE PROCESSOR MAY CLAIM PARTS OF A WELL

C*********************************************************************
C      INCLUDE 'msjunk.h'

      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'wells.h'
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*4 XPERM(IDIM,JDIM,KDIM),YPERM(IDIM,JDIM,KDIM),
     & ZPERM(IDIM,JDIM,KDIM)
      REAL*8 XW1,XW2,YW1,YW2,ZW1,ZW2,XG1,XG2,YG1,YG2,ZG1,ZG2,XT,YT,ZT,
     & DXW,DYW,DZW,TOLW,DUM1,DUM2,DUM3,XI(6),YI(6),ZI(6),DMM,DLL
      LOGICAL ONCE,DBG

! bag8 debug
      DBG = .FALSE.
      ONCE = .TRUE.

      IF (LEVELE.AND.BUGKEY(1)) WRITE (NFBUG,*)'PROC',MYPRC,
     & ', BLOCK',NBLK,', WELL',NW,' ENTERING SUBROUTINE WELLIJK1'

      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,MERR)

C  LOOP OVER WELL INTERVALS

      NI=NWELLI(NW)
      DO 1 N=1,NI
      IF (NBWELI(N,NW).NE.NBLK) GO TO 1
      XW1=WELLTOP(1,N,NW)
      XW2=WELLBOT(1,N,NW)
      YW1=WELLTOP(2,N,NW)
      YW2=WELLBOT(2,N,NW)
      ZW1=WELLTOP(3,N,NW)
      ZW2=WELLBOT(3,N,NW)
      DXW=XW2-XW1
      DYW=YW2-YW1
      DZW=ZW2-ZW1
      TOLW=1.D-6*(DXW**2+DYW**2+DZW**2)
      IF (TOLW.LE.0.D0) GO TO 1

C  LOOP OVER GRID ELEMENTS

      DO 2 K=KL1,KL2
      KG=K+KOFF
      ZG1=ZREC(KG,NBLK)
      ZG2=ZREC(KG+1,NBLK)
      IF (ZG2.GT.ZG1) THEN
         IF ((ZW1.LE.ZG1.AND.ZW2.LE.ZG1).OR.
     &    (ZW1.GE.ZG2.AND.ZW2.GE.ZG2)) GO TO 2
      ELSE
         IF (ZG1.EQ.ZG2) GO TO 2
         IF ((ZW1.LE.ZG2.AND.ZW2.LE.ZG2).OR.
     &    (ZW1.GE.ZG1.AND.ZW2.GE.ZG1)) GO TO 2
      ENDIF
      JL1=JL1V(K)
      JL2=JL2V(K)

      DO 3 J=JL1,JL2
      JG=J+JOFF
      YG1=YREC(JG,NBLK)
      YG2=YREC(JG+1,NBLK)
      IF (YG2.GT.YG1) THEN
         IF ((YW1.LE.YG1.AND.YW2.LE.YG1).OR.
     &    (YW1.GE.YG2.AND.YW2.GE.YG2)) GO TO 3
      ELSE
         IF (YG1.EQ.YG2) GO TO 3
         IF ((YW1.LE.YG2.AND.YW2.LE.YG2).OR.
     &    (YW1.GE.YG1.AND.YW2.GE.YG1)) GO TO 3
      ENDIF

      DO 4 I=IL1,IL2
      IF (KEYOUT(I,J,K).LE.0) GO TO 4
      IG=I+IOFF
      XG1=XREC(IG,NBLK)
      XG2=XREC(IG+1,NBLK)
      IF (XG2.GT.XG1) THEN
         IF ((XW1.LE.XG1.AND.XW2.LE.XG1).OR.
     &    (XW1.GE.XG2.AND.XW2.GE.XG2)) GO TO 4
      ELSE
         IF (XG1.EQ.XG2) GO TO 4
         IF ((XW1.LE.XG2.AND.XW2.LE.XG2).OR.
     &    (XW1.GE.XG1.AND.XW2.GE.XG1)) GO TO 4
      ENDIF

      NIF=0
      IF (XW2.NE.XW1) THEN
         DUM1=(XG1-XW1)/DXW
         IF (DUM1.LT.0.D0) DUM1=0.D0
         IF (DUM1.GT.1.D0) DUM1=1.D0
         YT=YW1+DUM1*DYW
         IF ((YG1-YT)*(YG2-YT).LE.0.D0) THEN
            ZT=ZW1+DUM1*DZW
            IF ((ZG1-ZT)*(ZG2-ZT).LE.0.D0) THEN
               NIF=NIF+1
               XI(NIF)=XW1+DUM1*DXW
               YI(NIF)=YT
               ZI(NIF)=ZT
            ENDIF
         ENDIF
         DUM1=(XG2-XW1)/(XW2-XW1)
         IF (DUM1.LT.0.D0) DUM1=0.D0
         IF (DUM1.GT.1.D0) DUM1=1.D0
         YT=YW1+DUM1*(YW2-YW1)
         IF ((YG1-YT)*(YG2-YT).LE.0.D0) THEN
            ZT=ZW1+DUM1*(ZW2-ZW1)
            IF ((ZG1-ZT)*(ZG2-ZT).LE.0.D0) THEN
               NIF=NIF+1
               XI(NIF)=XW1+DUM1*DXW
               YI(NIF)=YT
               ZI(NIF)=ZT
            ENDIF
         ENDIF
      ENDIF

      IF (YW2.NE.YW1) THEN
         DUM1=(YG1-YW1)/(YW2-YW1)
         IF (DUM1.LT.0.D0) DUM1=0.D0
         IF (DUM1.GT.1.D0) DUM1=1.D0
         XT=XW1+DUM1*(XW2-XW1)
         IF ((XG1-XT)*(XG2-XT).LE.0.D0) THEN
            ZT=ZW1+DUM1*(ZW2-ZW1)
            IF ((ZG1-ZT)*(ZG2-ZT).LE.0.D0) THEN
               NIF=NIF+1
               XI(NIF)=XT
               YI(NIF)=YW1+DUM1*DYW
               ZI(NIF)=ZT
            ENDIF
         ENDIF
         DUM1=(YG2-YW1)/(YW2-YW1)
         IF (DUM1.LT.0.D0) DUM1=0.D0
         IF (DUM1.GT.1.D0) DUM1=1.D0
         XT=XW1+DUM1*(XW2-XW1)
         IF ((XG1-XT)*(XG2-XT).LE.0.D0) THEN
            ZT=ZW1+DUM1*(ZW2-ZW1)
            IF ((ZG1-ZT)*(ZG2-ZT).LE.0.D0) THEN
               NIF=NIF+1
               XI(NIF)=XT
               YI(NIF)=YW1+DUM1*DYW
               ZI(NIF)=ZT
            ENDIF
         ENDIF
      ENDIF

      IF (ZW2.NE.ZW1) THEN
         DUM1=(ZG1-ZW1)/(ZW2-ZW1)
         IF (DUM1.LT.0.D0) DUM1=0.D0
         IF (DUM1.GT.1.D0) DUM1=1.D0
         XT=XW1+DUM1*(XW2-XW1)
         IF ((XG1-XT)*(XG2-XT).LE.0.D0) THEN
            YT=YW1+DUM1*(YW2-YW1)
            IF ((YG1-YT)*(YG2-YT).LE.0.D0) THEN
               NIF=NIF+1
               XI(NIF)=XT
               YI(NIF)=YT
               ZI(NIF)=ZW1+DUM1*DZW
            ENDIF
         ENDIF
         DUM1=(ZG2-ZW1)/(ZW2-ZW1)
         IF (DUM1.LT.0.D0) DUM1=0.D0
         IF (DUM1.GT.1.D0) DUM1=1.D0
         XT=XW1+DUM1*(XW2-XW1)
         IF ((XG1-XT)*(XG2-XT).LE.0.D0) THEN
            YT=YW1+DUM1*(YW2-YW1)
            IF ((YG1-YT)*(YG2-YT).LE.0.D0) THEN
               NIF=NIF+1
               XI(NIF)=XT
               YI(NIF)=YT
               ZI(NIF)=ZW1+DUM1*DZW
            ENDIF
         ENDIF
      ENDIF

      IF (NIF.LT.2) GO TO 4
      DUM1=0.D0
      DO 5 L=2,NIF
      DO 5 M=1,L-1
      DUM2=(XI(M)-XI(L))**2+(YI(M)-YI(L))**2+(ZI(M)-ZI(L))**2
      IF (DUM2.GT.DUM1) THEN
         MM=M
         LL=L
         DUM1=DUM2
      ENDIF
    5 CONTINUE
      IF (DUM1.LT.TOLW) GO TO 4

      NWELPRC(NW)=MYPRC
      NUMELE(NW)=NUMELE(NW)+1
      M=NUMELE(NW)
      NUMELET(NW)=M
      LOCWEL(1,M,NW)=NBLK
      LOCWEL(2,M,NW)=NI
      LOCWEL(3,M,NW)=IG
      LOCWEL(4,M,NW)=JG
      LOCWEL(5,M,NW)=KG
      LOCWEL(6,M,NW)=MYPRC
      ELELEN(M,NW)=SQRT(DUM1)
      DMM=DOWN(1,NBLK)*XI(MM)+DOWN(2,NBLK)*YI(MM)+DOWN(3,NBLK)*ZI(MM)
      DLL=DOWN(1,NBLK)*XI(LL)+DOWN(2,NBLK)*YI(LL)+DOWN(3,NBLK)*ZI(LL)
      ELEDEP(M,NW)=.5D0*(DMM+DLL)
      ELEXYZ(1,M,NW)=.5D0*(XI(MM)+XI(LL))
      ELEXYZ(2,M,NW)=.5D0*(YI(MM)+YI(LL))
      ELEXYZ(3,M,NW)=.5D0*(ZI(MM)+ZI(LL))
      IF (DMM.LT.DEPTOP(NW)) DEPTOP(NW)=DMM
      IF (DLL.LT.DEPTOP(NW)) DEPTOP(NW)=DLL
      IF (DMM.GT.DEPBOT(NW)) DEPBOT(NW)=DMM
      IF (DLL.GT.DEPBOT(NW)) DEPBOT(NW)=DLL

C  THIS IS A GUESS FOR THE AVERAGE PERMEABILITY NORMAL TO THE WELLBORE

      DUM1=(XW2-XW1)**2
      DUM2=(YW2-YW1)**2
      DUM3=(ZW2-ZW1)**2
      ELEPERM(M,NW)=(DUM3*SQRT(XPERM(I,J,K)*YPERM(I,J,K))+
     & DUM2*SQRT(XPERM(I,J,K)*ZPERM(I,J,K))+
     & DUM1*SQRT(YPERM(I,J,K)*ZPERM(I,J,K)))/(DUM1+DUM2+DUM3)

C  DEFAULT GEOMETRIC FACTOR

      DUM1=(XG2-XG1)*(YG2-YG1)*(ZG2-ZG1)
      IF (DUM1.LT.0.D0) DUM1=-DUM1
      DUM1=.208*SQRT(DUM1/ELELEN(M,NW))
      ELEGEOM(M,NW)=6.283185/(LOG(DUM1*2.D0/WELDIAM(N,NW))+
     & WELSKIN(N,NW))

! bag8 debug
      IF (DBG) THEN
      IF (ONCE.AND.MYPRC.EQ.0)
     &  WRITE(*,*)'WELLIJK1 debug output for NW=',NW
      WRITE(10+MYPRC,'(A,5I4)')'In WELLIJK: NW,N,I,J,K=',NW,N,I,J,K
      WRITE(10+MYPRC,*)'NWELPRC=',NWELPRC(NW)
      WRITE(10+MYPRC,*)'NUMELE=',NUMELE(NW)
      WRITE(10+MYPRC,*)'NUMELET=',NUMELET(NW)
      WRITE(10+MYPRC,*)'LOCWEL=',LOCWEL(1:6,M,NW)
      WRITE(10+MYPRC,*)'ELELEN=',ELELEN(M,NW)
      WRITE(10+MYPRC,*)'ELEDEP=',ELEDEP(M,NW)
      WRITE(10+MYPRC,*)'ELEXYZ=',ELEXYZ(1:3,M,NW)
      WRITE(10+MYPRC,*)'DEPTOP=',DEPTOP(NW)
      WRITE(10+MYPRC,*)'DEPBOT=',DEPBOT(NW)
      WRITE(10+MYPRC,*)'ELEPERM=',ELEPERM(M,NW)
      WRITE(10+MYPRC,*)'ELEGEOM=',ELEGEOM(M,NW)
      ONCE = .FALSE.
      ENDIF

    4 CONTINUE
    3 CONTINUE
    2 CONTINUE
    1 CONTINUE

      END
C
C
C*********************************************************************
      SUBROUTINE PUTUTIT (TIT,EXU)
C*********************************************************************

C  Routine adds units to a well history title.

C  TIT = TITLE (INPUT AND OUTPUT, CHARACTER*40)

C  EXU = EXTERNAL UNITS (INPUT, CHARACTER*20)

C*********************************************************************
      CHARACTER*1 TIT(40),EXU(20)

      I1=41
      DO 1 I=40,1,-1
      IF (TIT(I).NE.' ') GO TO 2
    1 I1=I

    2 IF (I1.GT.35) RETURN
      DO 3 I=1,20
      I1=I1+1
      IF (I1.GT.40) RETURN
    3 TIT(I1)=EXU(I)

      END

C
C*********************************************************************
      SUBROUTINE WELLCHECK (NERR)
C*********************************************************************
C  Checks well location for a single processor
C*********************************************************************
      INCLUDE 'control.h'
      INCLUDE 'wells.h'
      INTEGER NW(1024),NWP(1024,50)
      INTEGER WHICH, NERR, NWERR
c-------------------------------
      NWERR = 0

      I=1
      DO 1 I=1,NUMPRC
      NW(I)=0
      DO 1 J=1,NUMWEL
    1 NWP(I,J)=0

      I=1
      DO 2 I=1,NUMPRC
      DO 2 J=1,NUMWEL
      DO 3 K=1,NUMELET(J)

C         WRITE(*,*) J,K,LOCWEL(6,K,J)

      IF (I.EQ.LOCWEL(6,K,J)+1) THEN
         NWP(I,J)=1
         GO TO 2
      ENDIF
    3 CONTINUE
    2 CONTINUE

c verify whether the well has been assigned to a processor or not
c if not, error: the well is on grid lines or outside the reservoir

      NWERR = 0
      DO J = 1, NUMWEL
         WHICH = 0
         I = 1
         DO I = 1, NUMPRC
         IF (NWP(I,J).EQ.1) WHICH = I
c            write(*,*) 'WELL ',J,' IS ON PROC ',I
        ENDDO
         IF (WHICH.EQ.0) THEN
      IF(MYPRC.EQ.0) THEN
              WRITE(*,*) 'WELL ',J,' DOES NOT BELONG TO ANY PROCESSOR.'
              WRITE(NFOUT,*) 'ERROR :',
     &                   'WELL ',J,' DOES NOT BELONG TO ANY PROCESSOR.'
      ENDIF
            NWERR = NWERR + 1
         ENDIF
      ENDDO

      IF (NWERR.GT.0) THEN
         NERR = NERR + NWERR
         IF (LEVERR.LT.3) LEVERR = 3
      ENDIF
      END
