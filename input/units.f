C  UNITS CONVERSION PACKAGE

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE CONVRT (UIN,UOUT,FACM,FACA,KODRET,ERRMSG)
C  SUBROUTINE MAKESI (UIN,UTOP,UBOT,FACM,FACA,KODRET,ERRMSG)
C  SUBROUTINE FNDUNT (UIN,INDX)
C  ENTRY SETCVF(UTLCF,STDD)
C  SUBROUTINE INSTR (S1,NS1,S2,NS2,N)

C  HISTORY:

C  JOHN WHEELER     1/27/95    ORIGINAL BETA CODE
C  JOHN WHEELER    11/10/98    REVISE STOCK TANK CONVERSION FACTORS
C  JOHN WHEELER     3/ 5/99    ADD mscf TO STOCK TANK CONVERSION FACTORS

C  NOTES:

C     1) ERROR NUMBERS 131 TO 140 ARE RESERVED FOR UNITS CONVERSION

C     2) ELEMENTS OF THE 3 UNITS TABLES, FROMV, TOV, AND CFACV IN THIS
C        MODULE MUST CORRESPOND

C     3) 5 SMALLER TABLES ARE EQUIVALENCED INTO EACH OF THE ACTUAL
C        TABLES TO AVOID THE 19 CONTINUATION RECORDS RESTRICTION OF
C        SOME COMPILERS

C     4) PARAMETER DEFINITIONS
C        MXUN = NUMBER OF ENTRIES IN THE UNITS TABLES
C        MXUS = LENGTH OF INPUT AND OUTPUT UNITS STRINGS
C        MXU1 = LENGTH OF THE FROM UNIT IN THE UNITS TABLES
C        MXU2 = LENGTH OF THE TO UNIT IN THE UNITS TABLES

C*********************************************************************
      SUBROUTINE CONVRT (UIN,UOUT,FACM,FACA,KODRET,ERRMSG)
C*********************************************************************

C  ROUTINE GENERATES UNITS CONVERSION FACTORS
C  VALOUT = VALIN * FACM + FACA

C  UIN()  = FROM UNITS STRING (INPUT, CHARACTER*1, DIMENSION MAXUS)
C           TERMINATE WITH ] OR FILL WITH BLANKS. THE SYMBOL [, IF
C           PRESENT, WILL BE IGNORED

C  UOUT() = TO UNITS STRING (INPUT, CHARACTER*1, DIMENSION MAXUS)
C           TERMINATE WITH ] OR FILL WITH BLANKS.  THE SYMBOL [, IF
C           PRESENT, WILL BE IGNORED

C  FACM   = MULTIPLICATION CONVERSION FACTOR (OUTPUT, REAL*8)

C  FACA   = OFFSET CONVERSION FACTOR (OUTPUT, REAL*8)

C  KODRET = RETURN CODE. (OUTPUT, INTEGER)
C         = 0 ==> NO ERRORS ENCOUNTERED
C         > 0 ==> ERROR NUMBER

C  ERRMSG = ERROR MESSAGE IF KODRET > 0 (OUTPUT, CHARACTER*50)

C*********************************************************************
      PARAMETER (MXUN=246,MXUS=60,MXU1=13,MXU2=10)

      REAL*8 FACM,FACA,FACMI,FACAI,FACMO,FACAO
      CHARACTER*1 UIN(MXUS),UOUT(MXUS),UTOPI(MXUS),UBOTI(MXUS),
     & UTOPO(MXUS),UBOTO(MXUS)
      CHARACTER*50 ERRMSG

C CONVERT INPUT AND OUTPUT UNITS TO SI UNITS

      KODRET=0
      CALL MAKESI(UIN,UTOPI,UBOTI,FACMI,FACAI,KODRET,ERRMSG)
      IF (KODRET.NE.0) RETURN
      CALL MAKESI(UOUT,UTOPO,UBOTO,FACMO,FACAO,KODRET,ERRMSG)
      IF (KODRET.NE.0) RETURN

C TEST INPUT AND OUTPUT SI UNITS ARE IDENTICAL

      NI=0
      DO 1 I=1,MXUS
      IF (UTOPI(I).EQ.' ') GO TO 2
    1 NI=I
    2 NO=0
      DO 3 I=1,MXUS
      IF (UTOPO(I).EQ.' ') GO TO 4
    3 NO=I
    4 IF (NI.NE.NO) GO TO 13
      DO 5 I=1,NI
      DO 6 J=1,NO
      IF (UTOPI(I).EQ.UTOPO(J)) THEN
         UTOPO(J)=' '
         GO TO 5
      ENDIF
    6 CONTINUE
      GO TO 13
    5 CONTINUE

      NI=0
      DO 7 I=1,MXUS
      IF (UBOTI(I).EQ.' ') GO TO 8
    7 NI=I
    8 NO=0
      DO 9 I=1,MXUS
      IF (UBOTO(I).EQ.' ') GO TO 10
    9 NO=I
   10 IF (NI.NE.NO) GO TO 13
      DO 11 I=1,NI
      DO 12 J=1,NO
      IF (UBOTI(I).EQ.UBOTO(J)) THEN
         UBOTO(J)=' '
         GO TO 11
      ENDIF
   12 CONTINUE
      GO TO 13
   11 CONTINUE

C RETURN RESULT

      FACM=FACMI/FACMO
      FACA=(FACAI-FACAO)/FACMO
      RETURN

C ERROR EXIT

   13 KODRET=134
      ERRMSG='INCONSISTENT PHYSICAL UNITS'

      END

C*********************************************************************
      SUBROUTINE MAKESI (UIN,UTOP,UBOT,FACM,FACA,KODRET,ERRMSG)
C*********************************************************************
C  ROUTINE GENERATES THE SI EQUIVALENT OF ANY UNITS STRING AND RETURNS
C  THE CONVERSION FACTORS
C  VOUT = VIN * FACM + FACA

C  UIN()  = FROM UNITS STRING (INPUT, CHARACTER*1, DIMENSION MAXUS)

C  UTOP() = TO SI UNITS STRING, NUMERATOR
C           (OUTPUT, CHARACTER*1, DIMENSION MAXUS)

C  UBOT() = TO SI UNITS STRING, DENOMINATOR
C           (OUTPUT, CHARACTER*1, DIMENSION MAXUS)

C  FACM   = MULTIPLICATION CONVERSION FACTOR (OUTPUT, REAL*8)

C  FACA   = OFFSET CONVERSION FACTOR (OUTPUT, REAL*8)

C  KODRET = RETURN CODE. (OUTPUT, INTEGER)
C         = 0 ==> NO ERRORS ENCOUNTERED
C         = 1 ==> ERROR NUMBER

C  ERRMSG = ERROR MESSAGE IF KODRET > 0 (OUTPUT, CHARACTER*50)

C*********************************************************************
      PARAMETER (MXUN=246,MXUS=60,MXU1=13,MXU2=10)

      REAL*8 CFACV(MXUN),CFAC1(48),CFAC2(48),CFAC3(48),CFAC4(49),
     & CFAC5(53), FACM, FACA, STDD
      CHARACTER*1 UIN(MXUS),UTOP(MXUS),UBOT(MXUS),UTL1(MXU1),
     & ERMS1(50), USI1(MXU2)
      CHARACTER*10 TOV(MXUN),TO1(48),TO2(48),TO3(48),TO4(49),TO5(53),
     & USI
      CHARACTER*13 UTL,UTLCF
      CHARACTER*50 ERRMSG,ERMS

      EQUIVALENCE (TO1(1),TOV(1)),(TO2(1),TOV(49)),(TO3(1),TOV(97)),
     & (TO4(1),TOV(145)),(TO5(1),TOV(194))
      EQUIVALENCE (CFAC1(1),CFACV(1)),(CFAC2(1),CFACV(49)),
     & (CFAC3(1),CFACV(97)),(CFAC4(1),CFACV(145)),(CFAC5(1),CFACV(194))
      EQUIVALENCE (ERMS,ERMS1(1)),(UTL,UTL1(1)),(USI,USI1(1))

C TO UNITS TABLE (SI UNITS)

      DATA TO1 /'m         ','kmm/ss    ','kmm/ss    ',
     &'kmm/ss    ','K         ','K         ',
     &'kmm/ssaa  ','/s        ','kmm/ss    ',
     &'K         ','mmm       ','M         ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','km/ss     ','k/mss     ',
     &'K         ','S         ','a         ',
     &'mm        ','k/mss     ','m         ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       '/
      DATA TO2 /'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/M       ',
     &'k/M       ','k/M       ','k/mss     ',
     &'mm        ','mmm       ','k         ',
     &'mmm       ','mmm/s     ','/s        ',
     &'mmm       ','c         ','kmm/ss    ',
     &'m         ','kmm/ss    ','kmm/ss    ',
     &'k         ','mmm       ','m         ',
     &'k/mss     ','k/mss     ','k/mss     ',
     &'k/mss     ','mmm       ','k/ms      ',
     &'mm/s      ','mmm       ','k         ',
     &'k         ','mmm       ','mmm       '/
      DATA TO3 /'k         ','k         ','mmm       ',
     &'as        ','mmm       ','/s        ',
     &'mm        ','s         ','r         ',
     &'m         ','km/ss     ','kmm/ss    ',
     &'kmm/ss    ','aassss/kmm','cS/mm     ',
     &'as        ','m         ','m         ',
     &'s         ','m         ','k/mss     ',
     &'k/mss     ','m         ','m/ss      ',
     &'mmm       ','k         ','mmm       ',
     &'mmm       ','mmm       ','k/ssaa    ',
     &'kmm/MssK  ','k         ','M         ',
     &'r         ','k         ','mm        ',
     &'kmm/sss   ','kmm/sss   ','kmm/sss   ',
     &'s         ','m         ','k/mss     ',
     &'k/mss     ','k/mss     ','k         ',
     &'M         ','k/mss     ','kmm/ss    '/
      DATA TO4 /'kmm/ss    ','kmm/ss    ','km/ss     ',
     &'km/ss     ','m         ','m/s       ',
     &'kmm/sss   ','k         ','M         ',
     &'km/ss     ','m         ','c/mm      ',
     &'cS        ','cS/mm     ','m         ',
     &'m         ','k/mss     ','k/mss     ',
     &'/s        ','mm        ','k         ',
     &'aasss/kmm ','m         ','m         ',
     &'a         ','/s        ','mm        ',
     &'s         ','kmm/sssa  ','m         ',
     &'s         ','r         ','m         ',
     &'m         ','mmm       ','m         ',
     &'k/mss     ','s         ','m/s       ',
     &'as/k      ','s         ','k         ','kmm/ssa   ',
     &'s         ','kmm/sssaa ','k         ',
     &'mmm       ','k         ','m         '/
      DATA TO5 /'km/ss     ','mmm       ','m         ',
     &'mmm       ','mmm       ','k/ms      ',
     &'k/mmm     ','k/mss     ','mmm       ',
     &'mmm       ','r         ','mm/ss     ',
     &'m         ','r/s       ','r/s       ',
     &'as/k      ','s         ','k         ',
     &'M         ','M         ',
     &'k         ','M         ','M         ',
     &'mm        ','r         ','k         ',
     &'mm        ','mm        ','mm        ',
     &'mm        ','mm        ','mm        ',
     &'mm        ','ss        ','mm        ',
     &'S         ','k         ','k         ',
     &'k         ','mm/s      ','mmm       ',
     &'mmm       ','k/ssaa    ','k         ',
     &'kmm/ss    ','k         ','k         ',
     &'kmm/sss   ','k/mss     ','kmm/sssa  ',
     &'kmm/sss   ','m         ','s         '/

C MULTIPLICATION FACTOR TABLE

      DATA CFAC1 /1.D-10,1055.05586D0,1055.87D0,
     &1054.35D0,1.D0,.555555556D0,
     &1.D0,1.D0,1.D0,
     &1.D0,1.D-3,1.D0,
     &28.967D0,30.07012D0,44.09721D0,
     &16.04303D0,28.01055D0,44.00995D0,
     &2.01594D0,18.01534D0,28.0134D0,
     &31.9988D0,1.D0,1.D0,
     &.555555556D0,1.D0,1.D0,
     &4046.873D0,101325D0,1.495979D11,
     &107.87D0,26.9815D0,39.948D0,
     &74.9216D0,196.967D0,10.811D0,
     &137.34D0,79.909D0,12.01115D0,
     &40.08D0,112.4D0,35.453D0,
     &58.9332D0,51.996D0,63.54D0,
     &18.9984D0,55.847D0,1.00797D0/
      DATA CFAC2 /4.0026D0,200.59D0,126.9044D0,
     &39.102D0,6.9417D0,24.312D0,
     &54.938D0,14.0067D0,22.9898D0,
     &58.71D0,15.9994D0,30.9738D0,
     &207.19D0,195.09D0,32.064D0,
     &28.086D0,118.69D0,238.03D0,
     &50.9415D0,65.37D0,1.D5,
     &1.D-28,.1589873D0,158.826429D0,
     &2.359737D-3,1.8401308D-6,1.D0,
     &3.523907D-2,1.D0,4.1868D0,
     &2.54D-4,4.19002D0,4.184D0,
     &2.D-4,1.D-6,.01D0,
     &97.968504D0,98.06378D0,1333.22D0,
     &1329.4685D0,.226534773D0,1.D-3,
     &1.D-6,2.83168466D-2,28.2885501D0,
     &28.316061D0,1.6387064D-5,1.D0/
      DATA CFAC3 /999.00072D0,999.97226D0,.764554858D0,
     &1.D0,2.365882D-4,3.7D10,
     &9.869233D-13,86400D0,1.74532925D-2,
     &.1D0,1.D-5,1.D-7,
     &1.60219D-19,1.D0,10.76391D0,
     &96521.9D0,201.168D0,1.8288D0,
     &1209600D0,.3048D0,2986.08D0,
     &2988.984D0,.3048006D0,9.80665D0,
     &3.785412D-3,3.7816293D0,4.54609D-3,
     &4.546092D-3,4.404884D-3,1.D-4,
     &8314.31976D0,1.D-3,1.D-3,
     &1.570796327D-2,6.479891D-5,1.D4,
     &745.6999D0,745.7D0,735.499D0,
     &3600D0,2.54D-2,248.84D0,
     &3386.3788D0,3376.85D0,1.D0,
     &1.D0,1.D-3,4186.8D0/
      DATA CFAC4 /4190.02D0,4184D0,9.80665D0,
     &4448.222D0,1000D0,.5144444D0,
     &1.D0,.45359237D0,.45359237D0,
     &4.44822162D0,4828.032D0,3183.099D0,
     &1.D0,1.D0,9.46055D15,
     &1.D0,9796.8504D0,1.D2,
     &3.7D7,9.869233D-16,1.D-6,
     &1.D0,1609.344D0,1.D-6,
     &1.D-6,3.7D4,1.D-19,
     &1.D-6,1.D-6,2.54D-5,
     &60D0,2.90888208D-4,1852D0,
     &1609.347D0,1.D-6,1.D-3,
     &133.322D0,2.628D6,.44704D0,
     &2.58D-7,1.D-3,19.1763537D0,1.D-8,
     &1.D-9,1.D0,2.8349523D-2,
     &2.957353D-5,3.110348D-2,3.085678D16/
      DATA CFAC5 /.138255D0,8.809768D-3,4.217518D-3,
     &4.731765D-4,5.506105D-4,.1D0,
     &1.D3,6894.757D0,9.463529D-4,
     &1.101221D-3,1.D0,.01D0,
     &5.0292D0,1.047197551D-1,6.283185307D0,
     &2.58D-4,1.D0,1.91763537D-2,
     &1.26337876D-3,1.19530748D-3,
     &.677206541D0,4.4615800D-2,4.22118852D-2,
     &2589988.11D0,4.84813681D0,14.5939D0,
     &1.D-4,9.290304D-2,6.4516D-4,
     &1.D0,2589988.11D0,2589997.77D0,
     &1.D-6,1.D0,.83612736D0,
     &1.D0,1.588264D2,1.588264D2,
     &1.588264D2,1.D-4,1.478676D-5,
     &4.928922D-6,1.D0,907.1847D0,
     &4.184D9,1016.047D0,1.D3,
     &3516.8D0,133.322D0,1.D0,
     &1.D0,.9144D0,3.15576D7/

      FACA=0.D0
      FACM=1.D0
      NDIV=0
      NTOP=0
      NBOT=0
      NS=1

C START CONVERSION LOOP

    1 IF (NS.GT.MXUS) GO TO 14

C OPTIONAL TERMINATION BY ]

      IF (UIN(NS).EQ.']') GO TO 14

C SKIP [, BLANKS, AND

      IF ((UIN(NS).EQ.' ').OR.(UIN(NS).EQ.'*').OR.(UIN(NS).EQ.'['))
     & THEN
         NS=NS+1
         GO TO 1
      ENDIF

C TEST FOR UNITS DIVISION

      IF (UIN(NS).EQ.'/') THEN
         IF (NDIV.GT.0) THEN
            KODRET=131
            ERRMSG='MORE THAN ONE /'
            RETURN
         ENDIF
         NDIV=1
         NS=NS+1
         GO TO 1
      ENDIF

C EXTRACT A UNIT AND LOOK UP ITS CONVERSION

      J=0
      UTL=' '
      DO 2 I=NS,MXUS
      IF ((UIN(I).EQ.' ').OR.(UIN(I).EQ.'/').OR.(UIN(I).EQ.'*')
     & .OR.(UIN(I).EQ.']')) GO TO 3
      J=J+1
    2 UTL1(J)=UIN(I)
    3 NS=NS+J
      CALL FNDUNT (UTL,INDX)
      IF (INDX.EQ.0) THEN
         KODRET=132
         ERMS='INVALID UNIT: '
         DO 4 I=1,J
         IF (UTL1(I).EQ.':') GO TO 20
    4    ERMS1(I+14)=UTL1(I)
         ERRMSG=ERMS
         RETURN
   20    KODRET=135
         ERRMSG='UNITS MISSING ]'
         RETURN
      ENDIF

C SEPARATE SI NUMERATOR AND DENOMINATOR

      USI=TOV(INDX)
      IF (NDIV.EQ.0) THEN
         FACM=FACM*CFACV(INDX)
         DO 5 I=1,MXU2
         IF (USI1(I).EQ.' ') GO TO 1
         IF (USI1(I).EQ.'/') THEN
            J=I+1
            GO TO 6
         ENDIF
         IF (NTOP.GE.MXUS) GO TO 13
         NTOP=NTOP+1
    5    UTOP(NTOP)=USI1(I)
         GO TO 1
    6    DO 7 I=J,MXU2
         IF (USI1(I).EQ.' ') GO TO 1
         IF (NBOT.GE.MXUS) GO TO 13
         NBOT=NBOT+1
    7    UBOT(NBOT)=USI1(I)
      ELSE
         FACM=FACM/CFACV(INDX)
         DO 8 I=1,MXU2
         IF (USI1(I).EQ.' ') GO TO 1
         IF (USI1(I).EQ.'/') THEN
            J=I+1
            GO TO 9
         ENDIF
         IF (NBOT.GE.MXUS) GO TO 13
         NBOT=NBOT+1
    8    UBOT(NBOT)=USI1(I)
         GO TO 1
    9    DO 10 I=J,MXU2
         IF (USI1(I).EQ.' ') GO TO 1
         IF (NTOP.GE.MXUS) GO TO 13
         NTOP=NTOP+1
   10    UTOP(NTOP)=USI1(I)
      ENDIF
      GO TO 1

C END OF UNITS EXTRACTION LOOP

C ERROR EXIT

   13 KODRET=133
      ERRMSG='UNITS STRING TOO LONG'
      RETURN

C ADD TERMINAL BLANK

   14 IF (NTOP.LT.MXUS) UTOP(NTOP+1) = ' '
      IF (NBOT.LT.MXUS) UBOT(NBOT+1) = ' '

C TEST FOR CONVERSION ADDITION FACTOR

      IF (NTOP.EQ.1.AND.NBOT.EQ.0.AND.UTOP(1).EQ.'K') THEN
         DO 15 I=1,MXUS
         IF (UIN(I).EQ.'C') THEN
            FACA=273.15D0
            GO TO 16
         ENDIF
         IF (UIN(I).EQ.'F') THEN
            FACA=255.372222D0
            GO TO 16
         ENDIF
         IF (UIN(I).NE.' '.AND.UIN(I).NE.'[') GO TO 16
   15    CONTINUE
      ENDIF

C CANCEL BETWEEN SI NUMERATOR AND DENOMINATOR THEN COMPRESS

   16 DO 17 I=1,NTOP
      DO 17 J=1,NBOT
      IF (UTOP(I).EQ.UBOT(J)) THEN
         UTOP(I)=' '
         UBOT(J)=' '
      ENDIF
   17 CONTINUE
      J=0
      DO 18 I=1,NTOP
      IF (UTOP(I).NE.' ') THEN
         J=J+1
         IF (I.GT.J) THEN
            UTOP(J)=UTOP(I)
            UTOP(I)=' '
         ENDIF
      ENDIF
   18 CONTINUE
      J=0
      DO 19 I=1,NBOT
      IF (UBOT(I).NE.' ') THEN
         J=J+1
         IF (I.GT.J) THEN
            UBOT(J)=UBOT(I)
            UBOT(I)=' '
         ENDIF
      ENDIF
   19 CONTINUE

      KODRET=0
      RETURN

C*********************************************************************
      ENTRY SETCVF(UTLCF,STDD)
C*********************************************************************

C  REDEFINES A UNITS NUMERICAL DEFINITION

C  UTLCF = UNITS STRING TO BE REDEFINED (INPUT, CHARACTER*MXU1)

C  STDO = NEW CONVERSION FACTOR TO SI UNITS (INPUT, REAL*8)

C  THIS ROUTINE IS USED TO SET stb, scf, AND SIMILAR UNITS

C*********************************************************************

      CALL FNDUNT(UTLCF,INDX)
      IF (INDX.GT.0) CFACV(INDX)=STDD
      END

C*********************************************************************
      SUBROUTINE FNDUNT (UIN,INDX)
C*********************************************************************
C  ROUTINE LOCATES A UNIT IN THE UNITS TABLE USING BISECTION
C
C  UIN    = UNIT TO BE FOUND (INPUT, CHARACTER*MXU1)
C
C  UTAB() = UNITS TABLE (INPUT, CHARACTER*MXU1)
C
C  INDX   = TABLE INDEX (OUTPUT, INTEGER)
C         = 0 ==> UNIT NOT FOUND
C
C*********************************************************************
      PARAMETER (MXUN=246,MXUS=60,MXU1=13,MXU2=10)

      CHARACTER*13 UIN,FROMV(MXUN),FROM1(48),FROM2(48),FROM3(48),
     & FROM4(49),FROM5(53)

      EQUIVALENCE (FROM1(1),FROMV(1)),(FROM2(1),FROMV(49)),
     & (FROM3(1),FROMV(97)),(FROM4(1),FROMV(145)),(FROM5(1),FROMV(194))

C FROM UNITS TABLE

      DATA FROM1 /'A            ','Btu          ','Btu{m}       ',
     &'Btu{t}       ','C            ','F            ',
     &'H            ','Hz           ','J            ',
     &'K            ','L            ','M            ',
     &'Mwt-Air      ','Mwt-C2H6     ','Mwt-C3H8     ',
     &'Mwt-CH4      ','Mwt-CO       ','Mwt-CO2      ',
     &'Mwt-H2       ','Mwt-H2O      ','Mwt-N2       ',
     &'Mwt-O2       ','N            ','Pa           ',
     &'R            ','S            ','a            ',
     &'acre         ','atm          ','au           ',
     &'awtAg        ','awtAl        ','awtAr        ',
     &'awtAs        ','awtAu        ','awtB         ',
     &'awtBa        ','awtBr        ','awtC         ',
     &'awtCa        ','awtCd        ','awtCl        ',
     &'awtCo        ','awtCr        ','awtCu        ',
     &'awtF         ','awtFe        ','awtH         '/
      DATA FROM2 /'awtHe        ','awtHg        ','awtI         ',
     &'awtK         ','awtLi        ','awtMg        ',
     &'awtMn        ','awtN         ','awtNa        ',
     &'awtNi        ','awtO         ','awtP         ',
     &'awtPb        ','awtPt        ','awtS         ',
     &'awtSi        ','awtSn        ','awtU         ',
     &'awtV         ','awtZn        ','bar          ',
     &'barn         ','bbl          ','bbl-H2O      ',
     &'bf           ','bpd          ','bq           ',
     &'bu           ','c            ','cal          ',
     &'caliber      ','cal{m}       ','cal{t}       ',
     &'carat        ','cc           ','cm           ',
     &'cm-H2O       ','cm-H2O{4C}   ','cm-Hg        ',
     &'cm-Hg{60F}   ','cord         ','cp           ',
     &'cs           ','cu-ft        ','cu-ft-H2O    ',
     &'cu-ft-H2O{4C}','cu-in.       ','cu-m         '/
      DATA FROM3 /'cu-m-H2O     ','cu-m-H2O{4C} ','cu-yd        ',
     &'cul          ','cup          ','curie        ',
     &'darcy        ','day          ','deg          ',
     &'dm           ','dyne         ','erg          ',
     &'ev           ','farad        ','fc           ',
     &'fd           ','fl           ','fm           ',
     &'fn           ','ft           ','ft-H2O       ',
     &'ft-H2O{4C}   ','ft{s}        ','g            ',
     &'gal          ','gal-H2O      ','gal{Can}     ',
     &'gal{UK}      ','gal{dry}     ','gauss        ',
     &'gc           ','gm           ','gmM          ',
     &'grad         ','grain        ','ha           ',
     &'hp           ','hp{UK}       ','hp{m}        ',
     &'hr           ','in.          ','in.-H2O      ',
     &'in.-Hg       ','in.-Hg{60F}  ','k            ',
     &'kM           ','kPa          ','kcal         '/
      DATA FROM4 /'kcal{m}      ','kcal{t}      ','kf           ',
     &'kip          ','km           ','knot         ',
     &'kw           ','lb           ','lbM          ',
     &'lbf          ','lg           ','lmb          ',
     &'lumen        ','lux          ','ly           ',
     &'m            ','m-H2O        ','mb           ',
     &'mc           ','md           ','mg           ',
     &'mho          ','mi           ','mic          ',
     &'mica         ','miccurie     ','micdarcy     ',
     &'mics         ','micv         ','mil          ',
     &'min          ','min{a}       ','mi{n}        ',
     &'mi{s}        ','ml           ','mm           ',
     &'mm-Hg        ','mo           ','mph          ',
     &'mr           ','ms           ','mscf         ','mxw          ',
     &'ns           ','ohm          ','oz           ',
     &'oz{f}        ','oz{t}        ','parsec       '/
      DATA FROM5 /'pd           ','peck         ','pica         ',
     &'pint         ','pint{d}      ','poise        ',
     &'ppm          ','psi          ','quart        ',
     &'quart{d}     ','r            ','rad          ',
     &'rod          ','rpm          ','rps          ',
     &'rtg          ','s            ','scf          ',
     &'scf[0C]      ','scf[60F]     ',
     &'scm          ','scm[0C]      ','scm[60F]     ',
     &'sct          ','sec          ','slug         ',
     &'sq-cm        ','sq-ft        ','sq-in.       ',
     &'sq-m         ','sq-mi        ','sq-mi{s}     ',
     &'sq-mm        ','sq-sec       ','sq-yd        ',
     &'sr           ','stb          ','stbo         ',
     &'stbw         ','stokes       ','tbs          ',
     &'tes          ','tesla        ','ton          ',
     &'ton{TNT}     ','ton{l}       ','ton{m}       ',
     &'ton{r}       ','torr         ','v            ',
     &'w            ','yd           ','yr           '/

      N1=0
      N2=MXUN+1
      INDX=0
      DO 1 I=1,10
      N=(N1+N2)/2
      IF (UIN.LT.FROMV(N)) THEN
          N2=N
      ELSE
         IF (UIN.EQ.FROMV(N)) THEN
            INDX=N
            RETURN
         ENDIF
         N1=N
      ENDIF
      IF (N2-N1.LT.2) THEN
         IF (N1.GT.1.AND.UIN.EQ.FROMV(N1-1)) INDX=N1-1
         IF (N2.LT.MXUN.AND.UIN.EQ.FROMV(N2+1)) INDX=N2+1
         RETURN
      ENDIF
    1 CONTINUE
      END
C*********************************************************************
      SUBROUTINE INSTR (NS1,S1,NS2,S2,N)
C*********************************************************************

C  TESTS FOR ONE STRING IN ANOTHER

C  NS1 = LENGTH OF STRING S1 (INPUT, INTEGER)

C  S1 = STRING TO BE LOCATED (INPUT, CHARACTER*1)

C  NS2 = LENGTH OF STRING S2 (INPUT, INTEGER)

C  S2 = STRING TO BE TESTED (INPUT, CHARACTER*1)

C  N = LOCATION OF S1 IN S2 (0 IF NO LOCATION) (OUTPUT, INTEGER)

C*********************************************************************
      CHARACTER*1 S1(NS1),S2(NS2)

      N=0
      M=NS2-NS1+1
      DO 1 I=1,M
      DO 2 J=1,NS1
      IF (S2(I+J-1).NE.S1(J)) GO TO 1
    2 CONTINUE
      N=I
      RETURN
    1 CONTINUE

      END
