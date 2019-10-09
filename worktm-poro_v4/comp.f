C  COMPILER PACKAGE

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE CMPDRV (PROG,NPROGD,NPG,DEBUG,KERR)
C  ENTRY      EXCDRV (NPG,KERR)
C  SUBROUTINE CPINIT (KODE,R,VNAM,KVDAT)
C  SUBROUTINE CPPRG  (PROG,NPROGD,KODE,NKODED,NKODEF,R,NREGD,NREGF,
C     VNAM,KVDAT,NVARD,NVARF,CARRAY,NCARRD,NCARRF,LCSTG,NLCSTD,
C     NLCSTF,LINERR,ERRMSG,DEBUG)
C  SUBROUTINE CPEXP  (SORC,NSORC1,NSORC2,KODE,R,VNAM,KVDAT,CARRAY,
C     LCSTG,KERR,ERRMSG)
C  SUBROUTINE EXEC   (KODE,R,KVDAT,CARRAY,LCSTG,NFILE,KERR,ERRMSG)
C  SUBROUTINE NUMGET (A,NRD,N2,VAL,KODE)
C  SUBROUTINE GETNAM (A,NRD,N2,NAM,KRET)
C  SUBROUTINE GETSTG (A,N1,N2,NSTG,CARRAY,LCSTG)

C  HISTORY:

C  JOHN WHEELER       2/9/95    ORIGINAL BETA CODE

C  TO DO:

C    1) PROGRAM CALL STATEMENT

C    2) REPROGRAM RETURN/END STATEMENT

C    3) PROGRAM SUBROUTINE STATEMENT

C    4) PROGRAM PROGRAM STATEMENT ==> DONE BUT NOT TESTED

C    5) EXECUTION CODE FOR CALL STATEMENT

C    6) EXECUTION CODE FOR RETURN STATEMENT

C  NOTES:

C    1) ERROR NUMBERS 301 TO 400 ARE RESERVED FOR COMPILER ROUTINES

C    2) THE FOLLOWING VARIABLES MUST BE PRESERVED BETWEEN THE COMPILE
C       AND EXECUTION PHASES:

C       KODE(4,*)  = PSEDUDO ASSEMBLY INSTRUCTIONS (INTEGER)
C       R(*)       = REGISTER ARRAY (REAL*8)
C       KVDAT(4,*) = VARIABLE DATA (INTEGER)
C       CARRAY(*)  = STRING ARRAY (CHARACTER*1)
C       LCSTG(*)   = STRING POINTERS (INTEGER)

C**********************************************************************

C                              SYNTAX

C  ARITHMETIC OPERATORS: +, -, *, /, **

C  PRECEDENCE OPERATORS: ( )

C  NUMBER FORMATS:
C     N, N.n , NEN , N.nEN , .n , .nEN , +.n , -.n
C     WHERE N = INTEGER WHICH MAY BE SIGNED
C           n = UNSIGNED INTEGER
C     ALL CONSTANTS AND VARIABLES ARE TREATED AS REAL*8; HOWEVER,
C     SUBSCRIPTS AND DO LOOP INDEXES WILL BE ROUNDED TO THE NEAREST
C     INTEGER BEFORE USE.

C  VARIABLES:
C     A VARIABLE MUST BE DEFINED BEFORE IT IS USED ON THE RIGHT SIDE OF
C     OF AN = SIGN.  THERE ARE TWO TYPES OF VARIABLES, NORMAL AND
C     EXTERNAL.  A NORMAL VARIABLE IS DEFINED BY THE SOURCE CODE WHEN
C     IT APPEARS ON THE LEFT SIDE OF AN = SIGN OR IN A DIMENSION
C     STATEMENT.  AN EXTERNAL VARIABLE IS DEFINED BY THE APPLICATION
C     CODE AND MUST APPEAR IN AN EXTERNAL STATEMENT.  VARIABLE NAMES
C     MUST BEGIN WITH AN ALPHABETIC CHARACTER AND BE LESS THAN 7
C     CHARACTERS LONG.

C  VARIABLE ARRAYS:
C     VARIABLES MAY HAVE UP TO 3 SUBSCRIPTS.  A DIMENSION STATEMENT
C     MUST BE THE FIRST REFERENCE TO A NORMAL ARRAY.  DEFINITION OF
C     AN EXTERNAL VARIABLE INCLUDES AN IMPLIED DIMENSION.  THE MAXIMUM
C     VALUE OF A SINGLE DIMENSION IS 32000.

C  FUNCTIONS:
C     EXP(), LOG(), ABS(), SQR(), SIN(), COS(), TAN(), FIX(), ASIN(),
C     ATAN(), SINH(), COSH()

C  LOGICAL OPERATORS: .EQ. , .LT. , .GT. , .LT. , .GE. , .LE. , .NE.
C                     .AND. , .OR. , .NOT.

C  STATEMENTS:

C     VARIABLE = expression

C     DIMENSION A(I{,J{,K}}), B(....

C     GO TO N

C     DO N I=J,K,L

C     CONTINUE

C     IF (expression) THEN
C        statements
C     ELSE
C        statements
C     ENDIF

C     IF (expression) statement

C     SUBROUTINE NAME(A1,A2,......)     A1,A2,... = variables

C     PROGRAM NAME

C     EXTERNAL A1,A2, ....   (NO DIMENSIONS ON ARRAYS)
C     A1,A2,... = external variables

C     RETURN

C     END       (IMPLIED RETURN AT EACH END STATEMENT)

C     STOP I

C     CALL NAME(A1,A2,....)         A1,A2,... = expressions

C     OPEN (expression,'file name','status')

C     CLOSE expression

C     WRITE (expression,N) expression,expression,.....

C     READ (expression,N) expression,expression,.....

C     FORMAT

C  NOTES:

C  1)  THE MAXIMUM STATEMENT LENGTH IS APPROXIMATELY 400 (NSTML)
C      NONBLANK CHARACTERS; HOWEVER, THE COMPILER MAY USE UP TO 50
C      OF THESE CHARACTERS FOR INTERNAL PURPOSES.

C  2)  A STATEMENT MAY START IN ANY CHARACTER OF A RECORD.

C  3)  STATEMENT NUMBERS: IF A RECORD BEGINS WITH A NUMBER, THAT NUMBER
C      WILL BE INTERPERTED TO BE A STATEMENT NUMBER.

C**********************************************************************

C                PSEUDO MACHINE INSTRUCTIONS
C                                                       PRECE-  SINGLE
C OP  i  j  k  l  m  n  OPERATION                       DENCE   CODE
C --  -  -  -  -  -  -  ------------------------------  ------  ----
C
C  1  i  j  k           Rk = Ri + Rj                       5      +
C  2  i  j  k           Rk = Ri - Rj                       4      -
C  3  i  j  k           Rk = Ri * Rj                       3      *
C  4  i  j  k           Rk = Ri / Rj                       2      /
C  5  i     k           Rk = Ri                           16      =
C  6  i     k           Rk = -Ri                           6      -
C  7  i  j  k           IF Ri =  Rj THEN Rk=1 ELSE Rk=0    7     29
C  8  i  j  k           IF Ri >  Rj THEN Rk=1 ELSE Rk=0    8      >
C  9  i  j  k           IF Ri <  Rj THEN Rk=1 ELSE Rk=0    9      <
C 10  i  j  k           IF Ri >= Rj THEN Rk=1 ELSE Rk=0   10     28
C 11  i  j  k           IF Ri <= Rj THEN Rk=1 ELSE Rk=0   11     27
C 12  i  j  k           IF Ri <> Rj THEN Rk=1 ELSE Rk=0   12     26
C 13  i     k           IF NOT Ri THEN Rk=1 ELSE Rk=0     13     25
C 14  i  j  k           IF Ri AND Rj THEN Rk=1 ELSE Rk=0  14     24
C 15  i  j  k           IF Ri OR Rj THEN Rk=1 ELSE Rk=0   15     23
C 16  i  j  k           Rk = Ri ** Rj                      1      ^

C 21  i     k           Rk = Ki                 i = KVDAT index
C 22  i  j  k           Rk = Ki(Rj)             i = KVDAT index
C 23  i     k           Kk = Ri                 k = KVDAT index
C 24  i  j  k           Kk(Rj) = Ri             k = KVDAT index
C 25                    STOP
C 26  i                 JUMP TO i
C 27  i  j              JUMP TO i IF Rj =  0.
C 28                    INITIALIZE DO LOOP
C 29  i  j              RETURN TO i        pop j numbers off the stack
C 30  i  j  k           OPEN (Ri,Sj,Sk)    Sj = file name
C                                          Sk = status
C 31  i                 CLOSE Ri

C 40  i  j  k  l        Rk = Ki(Rj,Rl)          i = KVDAT index
C 41  i  j  k  l  m     Rk = Ki(Rj,Rl,Rm)       i = KVDAT index
C 42  i  j  k  l        Kk(Rj,Rl) = Ri          k = KVDAT index
C 43  i  j  k  l  m     Kk(Rj,Rl,Rm) = Ri       k = KVDAT index

C 44  i  j  k  l  m     DO i Rk = Rj,Rl,Rm

C 45  i  j  k  l  m  n  WRITE (Rj,Si) Rl,Rm,...
C                                k = number of registers output
C 46  i  j  k  l  m  n  READ (Rj,Si) Rl,Rm,...
C                                k = number of registers input
C 47  i  j  k  l  m  n  CALL NAME (Rj,Rk,Rl,...)
C                                i = subroutine instruction number

C 61  i     k           Rk = EXP(Ri)
C 62  i     k           Rk = LOG(Ri)   (BASE e)
C 63  i     k           Rk = ABS(Ri)
C 64  i     k           Rk = SQRT(Ri)
C 65  i     k           Rk = SIN(Ri)
C 66  i     k           Rk = COS(Ri)
C 67  i     k           Rk = TAN(Ri)
C 68  i     k           Rk = FIX(Ri)   (TRUNCATE)
C 69  i     k           Rk = ASIN(Ri)
C 70  i     k           Rk = ATAN(Ri)
C 71  i     k           Rk = SINH(Ri)
C 72  i     k           Rk = COSH(Ri)
C 73  i     k           Rk = RAND(Ri)   (UNIFORM DISTRIBUTION, 0 TO 1)
C 74  i     k           Rk = SEED(Ri)

C NOTES:

C   1) Ri ==> REGISTER i

C   2) Ki ==> EITHER REGISTER OR EXTERNAL VARIABLE i DEPENDING ON KVDAT

C   3) KVDAT HOLDS VARIABLE, FUNCTION, AND SUBROUTINE DATA

C      KVDAT(1,) = 1ST DIMENSION (0 ==> SCALAR)
C      KVDAT(2,) = 2ND DIMENSION (0 ==> NO DIMENSION)
C      KVDAT(3,) = 3RD DIMENSION (0 ==> NO DIMENSION)
C      KVDAT(4,) =  N ==> BASE REGISTER NUMBER
C      KVDAT(4,) = -N ==> EXTERNAL VARIABLE N (0 <= N < 1000)
C      KVDAT(4,) = -N ==> FUNCTION WITH OP CODE N-940 (1000 < N < 2000)
C      KVDAT(4,) = -N ==> SUBROUTINE N-2000 (2000 < N < 3000)

C      FOR FUNCTIONS, KVDAT(1,) IS THE NUMBER OF ARGUMENTS BUT
C      CURRENT CODE SUPPORTS ONLY (EXACTLY) ONE ARGUMENT.

C      FOR SUBROUTINES, KVDAT(1,) IS THE NUMBER OF ARGUMENTS AND
C      KVDAT(2,) IS THE 1ST INSTRUCTION NUMBER OF THE ROUTINE.

C**********************************************************************

C                 PARAMETER DEFINITIONS

C     NUMSL  = MAXIMUM NUMBER OF STATEMENT NUMBERS
C     NIRL   = MAXIMUM NUMBER OF REGISTER POINTERS PER STATEMENT
C     NSTML  = MAXIMUM STATEMENT LENGTH (INCLUDING EXPANSION)
C              (HARD CODED IN ROUTINE EXEC, VARIABLE ASTG)
C     NREGRL = MAXIMUM NUMBER OF ARGUMENTS PER STATEMENT
C     NDOL   = MAXIMUM NUMBER OF NESTED DO LOOPS
C     NKEYW  = NUMBER OF KEY WORDS FOR STATEMENT IDENTIFICATION
C     NIFL   = MAXIMUM NUMBER OF NESTED IF STATEMENTS

C**********************************************************************

C                SOME VARIABLE DEFINITIONS

C     LSTFUN = KVDAT INDEX OF LAST INTRINSIC FUNCTION
C     N1KVD  = 1ST KVDAT INDEX OF CURRENT ROUTINE
C     NRU1   = HIGHEST NUMBER OF PERMANENT REGISTERS USED
C     NRU2   = LOWEST NUMBER OF TEMPORY REGISTERS USED
C     LSUBR  = TRUE ==> SUBROUTINE, FALSE ==> PROGRAM

C**********************************************************************
      SUBROUTINE CMPDRV(PROG,NPROGD,NPG,DEBUG,KERR)
C**********************************************************************
C  DRIVES THE COMPILER MODULE

C  PROG()  = PROGRAM SOURCE CODE (INPUT, CHARACTER*1)

C  NPROGD  = NUMBER OF CHARACTERS IN PROG() (INPUT, INTEGER)

C  NPG     = PROGRAM NUMBER (OUTPUT, INTEGER)
C            INVALID IF KERR > 0

C  DEBUG   = PRINT DEBUG CODE IF TRUE (INPUT, LOGICAL)

C  KERR    = ERROR CODE (OUTPUT, INTEGER)
C          = 0 ==> NO ERROR
C          > 0 ==> ERROR NUMBER (MESSAGE GENERATED)

C**********************************************************************

      INTEGER    IKODE(6),IREG(6),IVAR(6),
     &             ICAR(6),ISTG(6)
      INTEGER    KODE(4,4000),KVDAT(4,400),LCSTG(50)
      LOGICAL    DEBUG
      REAL*8       R(4000)
      CHARACTER*1  PROG(*),CARRAY(1000)
      CHARACTER*6  VNAM(400)
      CHARACTER*40 ERRMSG

      INCLUDE 'control.h'

      COMMON /CMPSPC/ R,IKODE,IREG,IVAR,ICAR,ISTG,KODE,KVDAT,LCSTG,
     & CARRAY,VNAM

      DATA NPGNF/1/,NKODEF/1/,NREGF/1/,NVARF/1/,NCARRF/1/,NLCSTF/1/

      CALL CPPRG(PROG,NPROGD,KODE(1,NKODEF),4000-NKODEF,NK,
     &   R(NREGF),4000-NREGF,NR,VNAM(NVARF),KVDAT(1,NVARF),
     &   400-NVARF,NV,CARRAY(NCARRF),1000-NCARRF,NC,
     &   LCSTG(NLCSTF),50-NLCSTF,NS,KERR,LINERR,ERRMSG,DEBUG)
      IF (KERR.GT.0) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) THEN
            WRITE (NFOUT,*) 'COMPILE ERROR IN CODE LINE',LINERR
            WRITE (NFOUT,1) KERR,ERRMSG
    1       FORMAT (' ERROR #',I4,', ',A40)
         ENDIF
         RETURN
      ELSE
         IF (NPGNF.GT.6) THEN
            IF (LEVERR.LT.2) LEVERR=2
            IF (LEVELC) WRITE (NFOUT,*)
     &         ' ERROR # 350 - TOO MANY PROGRAMS'
            KERR=350
            RETURN
         ENDIF
         NPG=NPGNF
         NPGNF=NPGNF+1
         IKODE(NPG)=NKODEF
         NKODEF=NKODEF+NK
         IREG(NPG)=NREGF
         NREGF=NREGF+NR
         IVAR(NPG)=NVARF
         NVARF=NVARF+NV
         ICAR(NPG)=NCARRF
         NCARRF=NCARRF+NC
         ISTG(NPG)=NLCSTF
         NLCSTF=NLCSTF+NS
      ENDIF
      RETURN

C**********************************************************************
      ENTRY EXCDRV(NPG,KERR)
C**********************************************************************
C  DRIVES THE EXECUTION MODULE

C  NPG     = PROGRAM NUMBER (INPUT, INTEGER)

C  KERR    = ERROR CODE (OUTPUT, INTEGER)
C          = 0 ==> NO ERROR
C          > 0 ==> ERROR NUMBER (MESSAGE GENERATED)

C**********************************************************************

      IF (NPG.GE.NPGNF) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,*)
     &      ' ERROR # 351 - PROGRAM NOT COMPILED'
         KERR=351
         RETURN
      ENDIF
      CALL EXEC(KODE(1,IKODE(NPG)),R(IREG(NPG)),KVDAT(1,IVAR(NPG)),
     &   CARRAY(ICAR(NPG)),LCSTG(ISTG(NPG)),KERR,ERRMSG)
      IF (KERR.GT.0) THEN
         IF (LEVERR.LT.2) LEVERR=2
         IF (LEVELC) WRITE (NFOUT,1) KERR,ERRMSG
      ENDIF
      END
C**********************************************************************
      SUBROUTINE CPINIT(KODE,R,VNAM,KVDAT)
C**********************************************************************
C  INITIALIZES THE COMPILER.  THIS ROUTINE IS CALLED BY CPPRG

C  KODE(4,) = EXECUTION ARRAY (OUTPUT, INTEGER)

C  R()      = REGISTER ARRAY (INPUT AND OUTPUT, REAL*8)

C  VNAM()   = VARIABLE NAME ARRAY (INPUT AND OUTPUT, CHARACTER*6)

C  KVDAT(4,)= VARIABLE DATA (INPUT AND OUTPUT, INTEGER)

C**********************************************************************
      INCLUDE 'compc.h'

      CHARACTER*6 VNAM(*)
      INTEGER KODE(4,*),KVDAT(4,*)
      REAL*8 R(*)

C  SET COUNTERS AND TAGS

      NKODU=0
      NUMU=0
      NVU=0
      NRU2=NREGL+1
      NIFU=0
      KDIM=.FALSE.
      KSTG=.FALSE.
      NDOU=0
      LSUBR=.FALSE.
      NSTGU=1
      TAG=CHAR(31)
      STAG=CHAR(30)

C  CLEAR REGISTERS AND CODE

      DO 1 I=1,NREGL
    1 R(I)=0.D0
      DO 2 J=1,NKODL
      DO 2 I=1,4
    2 KODE(I,J)=0

C  SET FIXED REGISTERS FOR 0 & 1

      NRONE=2
      R(NRONE)=1.D0
      NRU1=2

C  SET UP OPERATOR TABLES

      SUB2(1)=CHAR(29)
      SUB2(2)='>'
      SUB2(3)='<'
      SUB2(4)=CHAR(28)
      SUB2(5)=CHAR(27)
      SUB2(6)=CHAR(26)
      SUB2(7)=CHAR(25)
      SUB2(8)=CHAR(24)
      SUB2(9)=CHAR(23)
      OPPS(1)='^'
      OPPS(2)='/'
      OPPS(3)='*'
      OPPS(4)='-'
      OPPS(5)='+'
      OPPS(6)='-'
      OPPS(16)='='
      DO 4 I=1,9
    4 OPPS(I+6)=SUB2(I)

C  SET UP INTRINSIC FUNCTIONS

      VNAM(NVU+1)='EXP   '
      VNAM(NVU+2)='LOG   '
      VNAM(NVU+3)='ABS   '
      VNAM(NVU+4)='SQRT  '
      VNAM(NVU+5)='SIN   '
      VNAM(NVU+6)='COS   '
      VNAM(NVU+7)='TAN   '
      VNAM(NVU+8)='FIX   '
      VNAM(NVU+9)='ASIN  '
      VNAM(NVU+10)='ACOS  '
      VNAM(NVU+11)='SINH  '
      VNAM(NVU+12)='COSH  '
      VNAM(NVU+13)='RAND  '
      VNAM(NVU+14)='SEED  '
      DO 5 I=1,14
      KVDAT(1,NVU+I)=1
    5 KVDAT(4,NVU+I)=-1000-I
      NVU=NVU+14
      LSTFUN=NVU
      N1KVD=NVU+1

      END
C**********************************************************************
      SUBROUTINE CPPRG(PROG,NPROGD,KODE,NKODED,NKODEF,R,NREGD,NREGF,
     & VNAM,KVDAT,NVARD,NVARF,CARRAY,NCARRD,NCARRF,LCSTG,NLCSTD,
     & NLCSTF,KERR,LINERR,ERRMSG,DEBUG)
C**********************************************************************
C  COMPILES A PROGRAM INTO AN EXECUTION MODULE FOR A PSEUDO MACHINE

C  PROG()   = CODE SOURCE (INPUT, CHARACTER*1)
C             ENTIRE PROGRAM MUST BE COMPRESSED INTO A SUPER STRING
C             WITH ASCII CHARACTER 30 SEPARATING STATEMENTS

C  NPROGD   = OFFSET OF LAST CHARACTER IN PROG() (INPUT, INTEGER)

C  KODE(4,) = EXECUTION ARRAY (OUTPUT, INTEGER)

C  NKODED   = SECOND DIMENSION OF KODE(4,) (INPUT, INTEGER)

C  NKODEF   = NEXT FREE OFFSET IN KODE(4,) (OUTPUT, INTEGER)

C  R()      = REGISTER ARRAY (INPUT AND OUTPUT, REAL*8)

C  NREGD    = DIMENSION OF REGISTER ARRAY R() (INPUT, INTEGER)

C  NREGF    = NEXT FREE OFFSET IN R() (OUTPUT, INTEGER)

C  VNAM()   = VARIABLE NAME ARRAY (INPUT AND OUTPUT, CHARACTER*6)

C  KVDAT(4,)= VARIABLE DATA (INPUT AND OUTPUT, INTEGER)

C  NVARD    = DIMENSION OF VNAM() AND KVDAT(3,) (INPUT, INTEGER)

C  NVARF    = NEXT FREE OFFSET IN VNAM() AND KVDAT(4,)
C             (OUTPUT, INTEGER)

C  CARRAY() = STRING STORAGE SPACE (INPUT AND OUTPUT, CHARACTER*1)

C  NCARRD   = DIMENSION OF CARRAY() (INPUT, INTEGER)

C  NCARRF   = NEXT FREE OFFSET IN CARRAY() (OUTPUT, INTEGER)

C  LCSTG()  = STRING POINTER SPACE (INPUT AND OUTPUT, INTEGER)

C  NLCSTD   = DIMENSION OF LCSTG() (INPUT, INTEGER)

C  NLCSTF   = NEXT FREE OFFSET IN LCSTG() (OUTPUT, INTEGER)

C  KERR     = ERROR NUMBER (OUTPUT, INTEGER)
C           = 0 ==> NO ERROR

C  LINERR   = ERROR STATEMENT NUMBER (OUTPUT, INTEGER)
C           = 0 ==> NO ERROR
C           > 0 ==> STATEMENT NUMBER

C  ERRMSG   = ERROR MESSAGE IF KERR > 0 (OUTPUT, CHARACTER*40)

C  DEBUG    = PRINT DEBUG CODE IF TRUE (INPUT, LOGICAL)

C**********************************************************************
C      INCLUDE 'msjunk.h'

      PARAMETER (NKEYW=18, NIFL=8)

      INCLUDE 'compc.h'
      INCLUDE 'control.h'

      CHARACTER*40 ERRMSG,ERRS(19)
      CHARACTER*10 KWRD(NKEYW)
      CHARACTER*6  VNAM(*),NAMS,THN
      CHARACTER*1  PROG(*),LP,RP,BLK,NAMS1(6),EQUAL,KWRD1(10,NKEYW),
     &             CARRAY(*)
      INTEGER KODE(4,*),KVDAT(4,*),KODIF(NIFL),KNDIF(NIFL),LCSTG(*)
      INTEGER KWRDL(NKEYW)
      LOGICAL DEBUG
      REAL*8 R(*),VAL

      EQUIVALENCE (NAMS,NAMS1),(KWRD,KWRD1)

      DATA LP/'('/,RP/')'/,BLK/' '/,EQUAL/'='/,THN/'THEN  '/,
     & KWRDL/9,4,6,3,2,8,4,5,5,7,4,10,3,7,8,4,5,4/,
     & KWRD/'DIMENSION ','GOTO      ','RETURN    ','IF(       ',
     &      'DO        ','CONTINUE  ','ELSE      ','ENDIF     ',
     &      'WRITE     ','FORMAT(   ','CALL      ','SUBROUTINE',
     &      'END       ','PROGRAM   ','EXTERNAL  ','OPEN      ',
     &      'CLOSE     ','READ      '/

      DATA ERRS/' SYNTAX ERROR                           ',
     & ' TOO MANY STATEMENT NUMBERS             ',
     & ' UNRECOGNIZED STATEMENT TYPE            ',
     & ' PROGRAM TOO LONG                       ',
     & ' UNDEFINED STATEMENT NUMBER             ',
     & ' TOO MANY VARIABLES                     ',
     & ' TOO MANY DIMENSIONS                    ',
     & ' OUT OF MEMORY                          ',
     & ' TOO MANY DO LOOPS NESTED               ',
     & ' VARIABLE NAME TOO LONG                 ',
     & ' UNPAIRED PARENTHESES                   ',
     & ' IF STATEMENTS NESTED TOO DEEP          ',
     & ' IF STATEMENT SEQUENCE ERROR            ',
     & ' TOO MANY CHARACTER STRINGS             ',
     & ' INVALID DO LOOP SYNTAX                 ',
     & ' TOO MANY TOTAL CHARACTERS IN STRINGS   ',
     & ' DO STATEMENT NUMBER MISSING            ',
     & ' UNDEFINED EXTERNAL VARIABLE            ',
     & '                                        '/

C  COPY DATA SET BY SUBROUTINE ARGUMENTS TO /COMPC/

      NKODL=NKODED
      NREGL=NREGD
      NVL=NVARD
      NCHRL=NCARRD
      NSTGL=NLCSTD

C  INITIALIZE MISC DATA

      CALL CPINIT(KODE,R,VNAM,KVDAT)
      KERR=0
      IC2=0
      NSTAT=0
      LCSTG(1)=0

C  START STATEMENT LOOP
C  COMPLETE DO LOOPS

   10 IF (NDOU.GT.0) THEN
      IF(NUMCUR.EQ.NUMDO(NDOU)) THEN
         NKODU=NKODU+1
         IF (NKODU.GT.NKODL) GO TO 804
         KODE(1,NKODU)=26
         KODE(2,NKODU)=LOCDO(NDOU)
         KODE(3,NKODU)=1
         M=LOCDO(NDOU)
         KODE(2,M)=NKODU+1
         NDOU=NDOU-1
         GO TO 10
      ENDIF
      ENDIF

C  COMPLETE (STATEMENT) IF STATEMENTS

      IF (NIFU.GT.0) THEN
      IF(KNDIF(NIFU).EQ.0) THEN
         I=KODIF(NIFU)
         KODE(2,I)=NKODU+1
         NIFU=NIFU-1
      ENDIF
      ENDIF

C  LOCATE BEGINNING OF NEXT NONBLANK RECORD

      NSTAT=NSTAT+1
      J=IC2+1
      IC1=0
      DO 11 I=J,NPROGD
      IF (PROG(I).EQ.STAG) THEN
         IF (IC1.GT.0) THEN
            IC2=I-1
            GO TO 15
         ENDIF
      ELSE
         IF (IC1.EQ.0.AND.PROG(I).NE.BLK) IC1=I
      ENDIF
   11 CONTINUE
      IF (IC1.EQ.0) GO TO 200
      IC2=NPROGD

C  PROCESS STATEMENT NUMBERS

   15 CALL NUMGET(PROG(IC1),IC2-IC1+1,N,VAL,K)
      IF (K.EQ.1) THEN
         NUMU=NUMU+1
         IF (NUMU.GT.NUMSL) GO TO 802
         NUMCUR=VAL+.5D0
         NUMVAL(NUMU)=NUMCUR
         NUMLOC(NUMU)=NKODU+1
         IC1=IC1+N
      ELSE
         NUMCUR=0
      ENDIF

C  DETERMINE STATEMENT TYPE

   16 DO 24 J=1,NKEYW
      L=1
      N=IC1-1
   23 N=N+1
      IF (N.GT.IC2) GO TO 24
      IF (PROG(N).EQ.BLK) GO TO 23
      IF (PROG(N).EQ.KWRD1(L,J)) THEN
         IF (L.EQ.KWRDL(J)) THEN
            KSTA=J+1
            IC1=N+1
            GO TO 25
         ELSE
            L=L+1
            GO TO 23
         ENDIF
      ENDIF
   24 CONTINUE
      KSTA=1
      DO 20 I=IC1,IC2
      IF (PROG(I).EQ.EQUAL) GO TO 25
   20 CONTINUE
      GO TO 803

C  RESET POINTERS THEN BRANCH ON STATEMENT TYPE

   25 NRU2=NREGL+1
      NIRU=1
      GO TO (900,910,920,930,940,950,960,970,980,990,1000,1010,1020,
     &   930,1030,1040,1050,1060,1070),KSTA

C  REPLACEMENT STATEMENT

  900 CALL CPEXP(PROG,IC1,IC2,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      IF (KERR.GT.0) GO TO 898
      GO TO 10

C  DIMENSION STATEMENT

  910 KDIM=.TRUE.
      CALL CPEXP(PROG,IC1,IC2,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      IF (KERR.GT.0) GO TO 898
      KDIM=.FALSE.
      GO TO 10

C  GO TO STATEMENT

  920 CALL NUMGET(PROG(IC1),IC2-IC1+1,N,VAL,K)
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=26
      KODE(2,NKODU)=VAL+.5D0
      GO TO 10

C  RETURN AND END STATEMENTS

  930 NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=25
      GO TO 10

C  DO STATEMENT

  950 NDOU=NDOU+1
      IF (NDOU.GT.NDOL) GO TO 809
      CALL NUMGET(PROG(IC1),IC2-IC1+1,N,VAL,K)
      IF (K.NE.1) GO TO 801
      IC1=IC1+N
      CALL GETNAM (PROG(IC1),IC2-IC1+1,N,NAMS,K)
      IF (K.EQ.2) GO TO 801
      IF (K.EQ.3) GO TO 810
      NKOD0=NKODU
  951 IC1=IC1+1
      IF (IC1.GE.IC2) GO TO 815
      IF (PROG(IC1-1).NE.EQUAL) GO TO 951
      CALL CPEXP(PROG,IC1,IC2,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      IF (KERR.GT.0) GO TO 898
      IF (NREGR.LT.2.OR.NREGR.GT.3) GO TO 815
      IF (NREGR.LT.3) VREGR(3)=NRONE
      DO 952 I=1,3
      IF (VREGR(I).GT.NRU1) THEN
         DO 953 J=NKODU,NKOD0,-1
         IF (KODE(4,J).EQ.VREGR(I)) THEN
            NRU1=NRU1+1
            IF (NRU1.GE.NRU2) GO TO 808
            KODE(4,J)=NRU1
            VREGR(I)=NRU1
            GO TO 952
         ENDIF
  953    CONTINUE
      ENDIF
  952 CONTINUE
      NKODU=NKODU+3
      IF (NKODU.GT.NKODL) GO TO 804
      DO 954 I=N1KVD,NVU
      IF (NAMS.EQ.VNAM(I)) THEN
         NR=KVDAT(4,I)
         GO TO 955
      ENDIF
  954 CONTINUE
      NRU1=NRU1+1
      IF (NRU1.GE.NRU2) GO TO 808
      NVU=NVU+1
      IF (NVU.GT.NVL) GO TO 806
      VNAM(NVU)=NAMS
      KVDAT(1,NVU)=0
      KVDAT(2,NVU)=0
      KVDAT(3,NVU)=0
      KVDAT(4,NVU)=NRU1
      NR=NRU1
  955 KODE(1,NKODU-2)=28
      KODE(1,NKODU-1)=44
      KODE(3,NKODU-1)=VREGR(1)
      KODE(4,NKODU-1)=NR
      KODE(2,NKODU)=VREGR(2)
      KODE(3,NKODU)=VREGR(3)
      NUMDO(NDOU)=VAL+.49999
      LOCDO(NDOU)=NKODU-1
      GO TO 10

C  CONTINUE STATEMENT

  960 GO TO 10

C  IF STATEMENT

  940 J=1
      DO 941 I=IC1,IC2
      IF (PROG(I).EQ.LP) J=J+1
      IF (PROG(I).EQ.RP) THEN
         J=J-1
         IF (J.EQ.0) GO TO 942
      ENDIF
  941 L=I
      GO TO 811
  942 CALL CPEXP(PROG,IC1,L,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      IF (KERR.GT.0) GO TO 898
      IF (NREGR.NE.1) GO TO 801
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=27
      KODE(3,NKODU)=VREGR(1)
      NIFU=NIFU+1
      IF (NIFU.GT.NIFL) GO TO 812
      KODIF(NIFU)=NKODU
      CALL GETNAM (PROG(L+2),IC2-L-1,N,NAMS,K)
      IF (K.EQ.2) GO TO 801
      IF (K.EQ.3) GO TO 810
      IF (NAMS.EQ.THN) THEN
         KNDIF(NIFU)=1
      ELSE
         KNDIF(NIFU)=0
         IC1=L+2
         GO TO 16
      ENDIF
      GO TO 10

C  ELSE STATEMENT

  970 IF (NIFU.EQ.0.OR.KNDIF(NIFU).NE.1) GO TO 813
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=26
      KODE(3,NKODU)=1
      I=KODIF(NIFU)
      KODE(2,I)=NKODU+1
      KODIF(NIFU)=NKODU
      KNDIF(NIFU)=2
      GO TO 10

C  ENDIF STATEMENT

  980 IF (NIFU.EQ.0.OR.KNDIF(NIFU).EQ.0) GO TO 813
      I=KODIF(NIFU)
      KODE(2,I)=NKODU+1
      NIFU=NIFU-1
      GO TO 10

C  WRITE STATEMENT

  990 CALL CPEXP(PROG,IC1,IC2,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      IF (KERR.GT.0) GO TO 898
      IF (NREGR.LT.2) GO TO 801
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=45
      KODE(2,NKODU)=R(VREGR(2))+.5D0
      KODE(3,NKODU)=VREGR(1)
      KODE(4,NKODU)=NREGR-2
      DO 991 I=3,NREGR
      J=MOD(I,3)+2
      IF (J.EQ.2) THEN
         NKODU=NKODU+1
         IF (NKODU.GT.NKODL) GO TO 804
      ENDIF
  991 KODE(J,NKODU)=VREGR(I)
      GO TO 10

C  READ STATEMENT

 1070 L=NKODU+1
      CALL CPEXP(PROG,IC1,IC2,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      IF (KERR.GT.0) GO TO 898
      IF (NREGR.LT.2) GO TO 801
      N=NREGR/3+1
      IF ((NKODU+N).GT.NKODL) GO TO 804
      DO 1072 I=NKODU,L,-1
      IF (KODE(1,I).EQ.0) THEN
         KODE(1,I+N)=0
         KODE(2,I+N)=KODE(2,I)
         KODE(3,I+N)=KODE(3,I)
         KODE(4,I+N)=KODE(4,I)
      ELSE
         KODE(1,I+N)=KODE(1,I)+2
         KODE(2,I+N)=KODE(4,I)
         KODE(3,I+N)=KODE(3,I)
         KODE(4,I+N)=KODE(2,I)
      ENDIF
 1072 CONTINUE
      NKODU=NKODU+N
      KODE(1,L)=46
      KODE(2,L)=R(VREGR(2))+.5D0
      KODE(3,L)=VREGR(1)
      KODE(4,L)=NREGR-2
      DO 1071 I=3,NREGR
      J=MOD(I,3)+2
      IF (J.EQ.2) THEN
         L=L+1
         KODE(1,L)=0
      ENDIF
 1071 KODE(J,L)=VREGR(I)
      GO TO 10

C  FORMAT STATEMENT

 1000 NSTGU=NSTGU+1
      IF (NSTGU.GT.NSTGL) GO TO 814
      NUMLOC(NUMU)=NSTGU
      K=LCSTG(NSTGU-1)
      IF ((K+IC2-IC1+2).GT.NCHRL) GO TO 816
      J=IC1-1
      DO 1001 I=J,IC2
      K=K+1
 1001 CARRAY(K)=PROG(I)
      LCSTG(NSTGU)=K
      GO TO 10

C  CALL STATEMENT

 1010 CONTINUE
      GO TO 10

C  SUBROUTINE STATEMENT

 1020 IF (NDOU.GT.0) GO TO 817
      LSUBR=.TRUE.
      N1KVD=NVU+1
      NUMU=0
      GO TO 10

C  PROGRAM STATEMENT

 1030 IF (NDOU.GT.0) GO TO 817
      LSUBR=.FALSE.
      N1KVD=NVU+1
      NUMU=0
      GO TO 10

C  EXTERNAL STATEMENT

 1040 DO 1041 I=IC1,IC2
      IF (PROG(I).EQ.',') PROG(I) = BLK
 1041 CONTINUE
 1042 IF (IC1.GT.IC2) GO TO 10
      CALL GETNAM(PROG(IC1),IC2-IC1+1,N,NAMS,K)
      IF (K.EQ.2) GO TO 801
      IF (K.EQ.3) GO TO 810
      CALL EXTNUM(NAMS,I,ID1,ID2,ID3)
      IF (I.LT.0) GO TO 818
      IC1=IC1+N
      NVU=NVU+1
      IF (NVU.GT.NVL) GO TO 806
      VNAM(NVU)=NAMS
      KVDAT(1,NVU)=ID1
      KVDAT(2,NVU)=ID2
      KVDAT(3,NVU)=ID3
      KVDAT(4,NVU)=-I
      GO TO 1042

C  OPEN STATEMENT

 1050 KSTG=.TRUE.
      CALL CPEXP(PROG,IC1,IC2,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      KSTG=.FALSE.
      IF (KERR.GT.0) GO TO 898
      IF ((NREGR.NE.3).OR.(VREGR(1).GT.-1).OR.(VREGR(2).GT.-1)
     &   .OR.(VREGR(3).LT.1)) GO TO 801
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=30
      KODE(2,NKODU)=VREGR(3)
      KODE(3,NKODU)=-VREGR(1)
      KODE(4,NKODU)=-VREGR(2)
      GO TO 10

C  CLOSE STATEMENT

 1060 CALL CPEXP(PROG,IC1,IC2,KODE,R,VNAM,KVDAT,CARRAY,LCSTG,KERR,
     &   ERRMSG)
      IF (KERR.GT.0) GO TO 898
      IF (NREGR.NE.1) GO TO 801
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=31
      KODE(2,NKODU)=VREGR(1)
      GO TO 10

C  END OF STATEMENT LOOP
C  COMPLETE GO TO, READ, AND WRITE STATEMENTS

  200 DO 202 I=1,NKODU
      IF ((KODE(1,I).EQ.26.AND.KODE(3,I).EQ.0).OR.KODE(1,I).EQ.45.OR.
     &   KODE(1,I).EQ.46) THEN
         DO 201 J=1,NUMU
         IF (KODE(2,I).EQ.NUMVAL(J)) THEN
            KODE(2,I)=NUMLOC(J)
            GO TO 202
         ENDIF
  201    CONTINUE
         GO TO 805
      ENDIF
  202 CONTINUE

C  NORMAL EXIT

      IF (NIFU.NE.0) GO TO 813
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 804
      KODE(1,NKODU)=25
      NSTAT=0
      NKODEF=NKODU+1
      NREGF=NRU1+1
      NVARF=NVU+1
      NCARRF=LCSTG(NSTGU)+1
      NLCSTF=NSTGU+1
      GO TO 897

C  ERROR EXITS

  801 KERR=1
      GO TO 899
  802 KERR=2
      GO TO 899
  803 KERR=3
      GO TO 899
  804 KERR=4
      GO TO 899
  805 KERR=5
      GO TO 899
  806 KERR=6
      GO TO 899
  807 KERR=7
      GO TO 899
  808 KERR=8
      GO TO 899
  809 KERR=9
      GO TO 899
  810 KERR=10
      GO TO 899
  811 KERR=11
      GO TO 899
  812 KERR=12
      GO TO 899
  813 KERR=13
      GO TO 899
  814 KERR=14
      GO TO 899
  815 KERR=15
      GO TO 899
  816 KERR=16
      GO TO 899
  817 KERR=17
      GO TO 899
  818 KERR=18
  899 ERRMSG=ERRS(KERR)
  898 LINERR=NSTAT
      KERR=KERR+300
      NKODEF=1
      NREGF=1
      NVARF=1
      NCARRF=1
      NLCSTF=1
  897 IF (DEBUG) THEN
         DO 896 J=1,NKODU
  896    WRITE (NFOUT,895) J,(KODE(L,J),L=1,4)
  895    FORMAT(7I5)
         DO 894 J=1,NVU
  894    WRITE (NFOUT,893) J,VNAM(J),(KVDAT(L,J),L=1,4)
  893    FORMAT(I5,2X,A6,3I5,I6)
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE CPEXP(SORC,NSORC1,NSORC2,KODE,R,VNAM,KVDAT,
     & CARRAY,LCSTG,KERR,ERRMSG)
C**********************************************************************
C  ROUTINE COMPILES AN EXPRESSION FOR A PSEUDO MACHINE. IT ALSO ASIGNS
C  VARIABLES TO REGISTERS

C  SORC()    = CODE SOURCE (INPUT, CHARACTER*1)

C  NSORC1    = OFFSET OF FIRST CHARACTER IN SORC (INPUT, INTEGER)

C  NSORC2    = OFFSET OF LAST CHARACTER IN SORC (INPUT, INTEGER)

C  KODE(4,)  = EXECUTION ARRAY (OUTPUT, INTEGER)

C  R()       = REGISTER ARRAY (INPUT AND OUTPUT, REAL*8)

C  VNAM()    = VARIABLE NAME ARRAY (INPUT AND OUTPUT, CHARACTER*6)

C  KVDAT(4,) = VARIABLE DATA (INPUT AND OUTPUT, INTEGER)

C  CARRAY()   = STRING STORAGE SPACE (INPUT, CHARACTER*1)

C  LCSTG()    = STRING POINTER SPACE (INPUT, INTEGER)

C  KERR      = ERROR CODE (OUTPUT, INTEGER)
C            = 0 ==> NO ERROR
C            > 0 ==> ERROR NUMBER

C  ERRMSG    = ERROR MESSAGE IF KERR > 0 (OUTPUT, CHARACTER*40)

C**********************************************************************
      INCLUDE 'compc.h'

      CHARACTER*40 ERRMSG,ERRS(16)
      CHARACTER*6 VNAM(*),NAMS
      CHARACTER*4 SUB1W(9)
      CHARACTER*1 SORC(*),LP,RP,BLK,CC,AAA,NAMS1(6),SUB1(4,9),CARRAY(*)
      INTEGER KODE(4,*),KVDAT(4,*),OPNUM(16),LCSTG(*)
      INTEGER LDIM(3)
      REAL*8 R(*),VAL

      EQUIVALENCE (NAMS,NAMS1),(SUB1,SUB1W)

      DATA LP/'('/,RP/')'/,BLK/' '/,AAA/'A'/,
     & SUB1W/'EQ. ','GT. ','LT. ','GE. ','LE. ','NE. ','NOT.','AND.',
     & 'OR. '/,OPNUM/16,4,3,2,1,6,7,8,9,10,11,12,13,14,15,5/
      DATA ERRS/' SYNTAX ERROR                           ',
     &      ' EXPRESSION TOO LONG                    ',
     &      ' MISMATCHED PARENTHECES                 ',
     &      ' ILLEGAL CHARACTER                      ',
     &      ' OUT OF MEMORY                          ',
     &      ' EXPRESSION TOO COMPLEX                 ',
     &      ' VARIABLE NAME TOO LONG                 ',
     &      ' INVALID DIMENSION STATEMENT            ',
     &      ' INSUFFICIENT FUNCTION ARGUMENTS        ',
     &      ' PROGRAM TOO LONG                       ',
     &      ' INSUFFICIENT INDEXES                   ',
     &      ' FUNCTION NAME REDEFINED                ',
     &      ' TOO MANY VARIABLE NAMES                ',
     &      ' SUBROUTINE NAME USED AS A VARIABLE     ',
     &      ' UNMATCHED QUOTE                        ',
     &      ' OUT OF STRING SPACE                    '/

C  COPY SOURCE CODE TO COMPILER WORK ARRAY WITH SUBSTITUTIONS
C  1)  REPLACE MULTICHARACTER OPERATORS BY SINGLE CHARACTER
C  2)  ENCLOSE ENTIRE EXPRESSION IN ()
C  3)  ENCLOSE RIGHT SIDE OF EQUATION IN ()
C  4)  INSURE SPACE OR ( AFTER EACH OPERATOR
C  5)  EXTRACT STRING CONSTANTS

      NCHR=1
      A(1)=LP
      IEQ=0
      I=NSORC1-1
      NREGR=0

    1 I=I+1
      IF (I.GT.NSORC2) GO TO 2
      CC=SORC(I)
      IF (CC.EQ.BLK) GO TO 1
      IF (CC.EQ.'*'.AND.SORC(I+1).EQ.'*') THEN
         I=I+1
         CC='^'
         GO TO 5
      ENDIF
      IF (CC.EQ.'+'.OR.CC.EQ.'-'.OR.CC.EQ.'*'.OR.CC.EQ.'/') GO TO 5
      IF (CC.EQ.'=') THEN
         NCHR=NCHR+1
         A(NCHR)='='
         IEQ=1
         CC=LP
      ENDIF
      IF (CC.EQ.',') THEN
         NCHR=NCHR+1
         A(NCHR)=RP
         CC=LP
      ENDIF
      IF (CC.EQ.'.') THEN
         DO 4 J=1,9
         DO 3 K=1,4
         IF (SORC(I+K).NE.SUB1(K,J)) GO TO 4
         IF (SUB1(K,J).EQ.'.') THEN
            I=I+K
            CC=SUB2(J)
            GO TO 5
         ENDIF
    3    CONTINUE
    4    CONTINUE
      ENDIF
      IF (KSTG.AND.(CC.EQ.CHAR(39))) THEN
         CALL GETSTG (SORC,I,NSORC2,K,CARRAY,LCSTG)
         IF (K.EQ.0) GO TO 915
         IF (K.LT.0) GO TO 916
         NREGR=NREGR+1
         IF (NREGR.GT.NREGRL) GO TO 906
         VREGR(NREGR)=-K
         GO TO 1
      ENDIF
      NCHR=NCHR+1
      IF (NCHR.GT.NSTML) GO TO 902
      A(NCHR)=CC
      GO TO 1

    5 NCHR=NCHR+2
      IF (NCHR.GT.NSTML) GO TO 902
      A(NCHR-1)=CC
      A(NCHR)=BLK
      GO TO 1

    2 NCHR=NCHR+1+IEQ
      IF (NCHR.GT.NSTML) GO TO 902
      A(NCHR)=RP
      IF (IEQ.GT.0) A(NCHR-1)=RP

C  START COMPILE LOOP
C  LOCATE INNER SUBEXPRESSION BOUNDED BY ()

   10 IL=0
      DO 11 I=1,NCHR
      IF (A(I).EQ.LP) IL=I
      IF (A(I).EQ.RP) THEN
         IF (IL.EQ.0) GO TO 903
         IR=I
         A(IL)=BLK
         A(IR)=BLK
         GO TO 12
      ENDIF
   11 CONTINUE
      IF (IL.NE.0) GO TO 903
      GO TO 60

C  START LOOP TO REPLACE VARIABLES, NUMBERS, AND FUNCTIONS BY REGISTERS

   12 IC=IL
   14 IC=IC+1
      IF (IC.EQ.IR) GO TO 40
      CC=A(IC)

C  SKIP REGISTER TAG AND NUMBER

      IF (CC.EQ.TAG) THEN
         IC=IC+1
         GO TO 14
      ENDIF

C  SKIP BLANK OR OPERATOR

      IF (CC.EQ.BLK) GO TO 14
      DO 15 I=1,16
      IF (CC.EQ.OPPS(I)) GO TO 14
   15 CONTINUE

C  PROCESS NUMERIC CONSTANTS

      IF (CC.LT.AAA) THEN
         CALL NUMGET(A(IC),IR-IC,J,VAL,K)
         IF (K.EQ.2) GO TO 904
         DO 17 I=1,NRU1
         IF (R(I).EQ.VAL) THEN
            NN=I
            GO TO 18
         ENDIF
   17    CONTINUE
         NRU1=NRU1+1
         IF (NRU1.GE.NRU2) GO TO 905
         R(NRU1)=VAL
         NN=NRU1
   18    DO 16 I=1,J
   16    A(I+IC-1)=BLK
         NIRU=NIRU+1
         IF (NIRU.GT.NIRL) GO TO 906
         IREG(NIRU)=NN
         A(IC-1)=TAG
         A(IC)=CHAR(NIRU)
         IC=IC+J-1
         GO TO 14
      ENDIF

C TEST FOR VARIABLE OR FUNCTION NAME

      CALL GETNAM (A(IC),IR-IC,J,NAMS,K)
      IF (K.EQ.2) GO TO 904
      IF (K.EQ.3) GO TO 907
      DO 20 I=1,J
   20 A(I+IC-1)=BLK
      IC=IC+J-1
      DO 23 I=N1KVD,NVU
      IF (NAMS.EQ.VNAM(I)) THEN
         NV=I
         GO TO 21
      ENDIF
   23 CONTINUE
      DO 26 I=1,LSTFUN
      IF (NAMS.EQ.VNAM(I)) THEN
         NV=I
         GO TO 21
      ENDIF
   26 CONTINUE

C  NEW VARIABLE

      NVU=NVU+1
      IF (NVU.GT.NVL) GO TO 913
      NV=NVU
      VNAM(NVU)=NAMS
      NRU1=NRU1+1
      IF (NRU1.GE.NRU2) GO TO 905
      KVDAT(1,NVU)=0
      KVDAT(2,NVU)=0
      KVDAT(3,NVU)=0
      KVDAT(4,NVU)=NRU1

C  DIMENSION STATEMENT PROCESSING

      IF (KDIM) THEN
         ND=0
         I=IC-1
         NR=1
   25    I=I+1
         IF (I.GE.IR) THEN
            IF (NR.LT.1) GO TO 908
            NRU1=NRU1+NR-1
            IF (NRU1.GE.NRU2) GO TO 905
            GO TO 10
         ENDIF
         IF (A(I).EQ.TAG) THEN
            ND=ND+1
            IF (ND.GT.3) GO TO 908
            A(I)=BLK
            I=I+1
            J=ICHAR(A(I))
            J=IREG(J)
            J=R(J)+.5D0
            KVDAT(ND,NVU)=J
            NR=NR*J
            A(I)=BLK
         ELSE
            IF (A(I).NE.BLK) GO TO 901
         ENDIF
         GO TO 25
      ENDIF

C  EVALUATE FUNCTIONS

   21 IF (KVDAT(4,NV).LT.-1000) THEN
         IF (KVDAT(4,NV).EQ.-2000) GO TO 914
         IF (IC.LT.IEQ.AND.IL.EQ.1) GO TO 912
         DO 22 I=IC,IR
         IF (A(I).EQ.TAG) THEN
            NL=ICHAR(A(I+1))
            GO TO 24
         ENDIF
         IF (A(I).NE.BLK) GO TO 909
   22    CONTINUE
         GO TO 909
   24    N=IREG(NL)
         IF (N.GT.NRU1) THEN
            M=N
         ELSE
            NRU2=NRU2-1
            IF (NRU1.GE.NRU2) GO TO 905
            M=NRU2
            IREG(NL)=M
         ENDIF
         NKODU=NKODU+1
         IF (NKODU.GT.NKODL) GO TO 910
         KODE(1,NKODU)=-KVDAT(4,NV)-940
         KODE(2,NKODU)=N
         KODE(4,NKODU)=M
         GO TO 14
      ENDIF

C  PROCESS INTERNAL SCALAR VARIABLES

      IF (KVDAT(1,NV).EQ.0.AND.KVDAT(4,NV).GT.0) THEN
         KIND=0
         NIRU=NIRU+1
         IF (NIRU.GT.NIRL) GO TO 906
         IREG(NIRU)=KVDAT(4,NV)
         A(IC-1)=TAG
         A(IC)=CHAR(NIRU)
         GO TO 14
      ENDIF

C  PROCESS EXTERNAL VARIABLES AND INTERNAL VARIABLE ARRAYS

      KIND=1
      ND=0
      IF (KVDAT(1,NV).GT.0) ND=1
      IF (KVDAT(2,NV).GT.0) ND=2
      IF (KVDAT(3,NV).GT.0) ND=3
      M=0
      DO 34 I=1,ND
      LDIM(I)=0
      J=IC
      DO 33 K=J,IR
      IF (A(K).EQ.TAG) THEN
         IC=K+1
         LI=ICHAR(A(IC))
         LDIM(I)=IREG(LI)
         IF (LDIM(I).GT.NRU1) M=LDIM(I)
         A(K)=BLK
         A(IC)=BLK
         GO TO 34
      ENDIF
      IF (A(K).NE.BLK) GO TO 911
   33 CONTINUE
   34 CONTINUE
      IF (ND.GT.0) THEN
         IF(LDIM(ND).EQ.0) GO TO 911
      ENDIF
      NKODU=NKODU+1
      IF (NKODU.GT.NKODL) GO TO 910
      IF (M.LT.NRU2) THEN
         NRU2=NRU2-1
         IF (NRU1.GE.NRU2) GO TO 905
         M=NRU2
      ENDIF
      KODE(2,NKODU)=NV
      KODE(3,NKODU)=LDIM(1)
      KODE(4,NKODU)=M
      IF (ND.GT.1) THEN
         KODE(1,NKODU)=38+ND
         NKODU=NKODU+1
         IF (NKODU.GT.NKODL) GO TO 910
         KODE(2,NKODU)=LDIM(2)
         KODE(3,NKODU)=LDIM(3)
      ELSE
         KODE(1,NKODU)=21+ND
      ENDIF
      A(IC-1)=TAG
      IF (ND.EQ.0) THEN
         NIRU=NIRU+1
         IF (NIRU.GT.NIRL) GO TO 906
         LI=NIRU
      ENDIF
      A(IC)=CHAR(LI)
      IREG(LI)=M
      GO TO 14

C  DO ARITHMETIC

   40 J1=1
   41 J2=J1
      DO 42 J=J2,16
      J1=J
      ILR=0
      I=IL
   43 IF (A(I).EQ.TAG) THEN
         ILR=I
         I=I+1
         GO TO 45
      ENDIF
      IF (A(I).EQ.OPPS(J)) THEN
         IRR=0
         IP=I+1
         DO 44 II=IP,IR
         IF (A(II).EQ.TAG) THEN
            NIR=ICHAR(A(II+1))
            N=IREG(NIR)
            IF (ILR.EQ.0) THEN
               IF (J.EQ.4) THEN
                  M=N
                  N=0
                  NOP=6
                  GO TO 47
               ENDIF
               IF (J.EQ.5) THEN
                  A(I)=BLK
                  GO TO 41
               ENDIF
               IF (J.EQ.13) THEN
                  M=N
                  N=0
                  NOP=13
                  GO TO 47
               ENDIF
               GO TO 901
            ENDIF
            M=ICHAR(A(ILR+1))
            M=IREG(M)
            NOP=OPNUM(J)
            A(ILR)=BLK
            A(ILR+1)=BLK
   47       A(I)=BLK
            IF (NOP.EQ.5) GO TO 50
            K=N
            IF (K.LT.NRU2.AND.M.GT.NRU1) K=M
            IF (K.LT.NRU2) THEN
               NRU2=NRU2-1
               IF (NRU1.GE.NRU2) GO TO 905
               K=NRU2
            ENDIF
            IREG(NIR)=K
            NKODU=NKODU+1
            IF (NKODU.GT.NKODL) GO TO 910
            KODE(1,NKODU)=NOP
            KODE(2,NKODU)=M
            KODE(3,NKODU)=N
            KODE(4,NKODU)=K
            GO TO 41
         ENDIF
   44    CONTINUE
         GO TO 913
      ENDIF
   45 I=I+1
      IF (I.LT.IR) GO TO 43
   42 CONTINUE
      GO TO 10

C  PROCESS = OPERATOR

   50 L=NKODU
      IF (KODE(1,L).EQ.0) L=L-1
      IF (KIND.EQ.0) THEN
         IF (N.LT.NRU2) THEN
            NKODU=NKODU+1
            IF (NKODU.GT.NKODL) GO TO 910
            KODE(1,NKODU)=5
            KODE(2,NKODU)=N
            KODE(4,NKODU)=M
         ELSE
            KODE(4,L)=M
         ENDIF
      ELSE
         KODE(1,L)=KODE(1,L)+2
         KODE(4,L)=KODE(2,L)
         KODE(2,L)=N
      ENDIF

C  NORMAL EXIT

   60 KERR=0
      I=0
   61 I=I+1
      IF (I.GT.NCHR) RETURN
      IF (A(I).EQ.TAG) THEN
         I=I+1
         NREGR=NREGR+1
         IF (NREGR.GT.NREGRL) GO TO 906
         J=ICHAR(A(I))
         VREGR(NREGR)=IREG(J)
         GO TO 61
      ENDIF
      IF (A(I).EQ.BLK) GO TO 61

C  ERROR EXIT

  901 KERR=1
      GO TO 999
  902 KERR=2
      GO TO 999
  903 KERR=3
      GO TO 999
  904 KERR=4
      GO TO 999
  905 KERR=5
      GO TO 999
  906 KERR=6
      GO TO 999
  907 KERR=7
      GO TO 999
  908 KERR=8
      GO TO 999
  909 KERR=9
      GO TO 999
  910 KERR=10
      GO TO 999
  911 KERR=11
      GO TO 999
  912 KERR=12
      GO TO 999
  913 KERR=13
      GO TO 999
  914 KERR=14
      GO TO 999
  915 KERR=15
      GO TO 999
  916 KERR=16
  999 ERRMSG=ERRS(KERR)
      KERR=KERR+30
      RETURN
      END
C**********************************************************************
      SUBROUTINE EXEC(KODE,R,KVDAT,CARRAY,LCSTG,KERR,ERRMSG)
C**********************************************************************
C  EXECUTES PROGRAM PSEUDO MACHINE INSTRUCTIONS

C  KODE(4,)   = EXECUTION ARRAY (INPUT, INTEGER)

C  R()        = REGISTER ARRAY (INPUT AND OUTPUT, REAL*8)

C  KVDAT(4,)  = VARIABLE DATA (INPUT, INTEGER)

C  CARRAY()   = STRING STORAGE SPACE (INPUT, CHARACTER*1)

C  LCSTG()    = STRING POINTER SPACE (INPUT, INTEGER)

C  KERR       = ERROR CODE (OUTPUT, INTEGER)
C             = 0 ==> NO ERROR
C             > 0 ==> ERROR NUMBER

C  ERRMSG     = ERROR MESSAGE IF KERR > 0 (OUTPUT, CHARACTER*40)

C**********************************************************************
C      INCLUDE 'msjunk.h'

      INCLUDE 'control.h'
      INCLUDE 'compc.h'
      INCLUDE 'output.h'

      INTEGER KODE(4,*),KVDAT(4,*),LCSTG(*)
      REAL*8 R(*),RW(NREGRL)
      CHARACTER*1 CARRAY(*),ASTG1(400)
      CHARACTER*40 ERRMSG,ERRS(9)
      CHARACTER*400 ASTG
      CHARACTER*100 BSTG,CSTG
      LOGICAL L1DO

      EQUIVALENCE (ASTG,ASTG1(1)),(BSTG,ASTG1(1)),(CSTG,ASTG1(201))

      DATA ERRS/' DIVISION BY ZERO                       ',
     &          ' ILLEGAL OPERATION                      ',
     &          ' NEGATIVE NUMBER TO A POWER             ',
     &          ' SQUARE ROOT OF NEGATIVE NUMBER         ',
     &          ' LOG OF NEGATIVE NUMBER                 ',
     &          ' INVALID EXTERNAL VARIABLE INDEX        ',
     &          ' INVALID FILE NUMBER                    ',
     &          ' OPEN FILE FAILED                       ',
     &          '                                        '/

C  BRANCH TO INSTRUCTION

      NI=0
  301 NI=NI+1
  302 KOP=KODE(1,NI)
      I=KODE(2,NI)
      J=KODE(3,NI)
      K=KODE(4,NI)
      IF (KOP.LT.17) GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),KOP
      IF (KOP.LT.32) GO TO (21,22,23,24,25,26,27,28,29,30,31),KOP-20
      IF (KOP.LT.47) THEN
         IF (KOP.EQ.45) GO TO 45
         IF (KOP.EQ.46) GO TO 46
         NI=NI+1
         L=KODE(2,NI)
         M=KODE(3,NI)
         GO TO (40,41,42,43,44),KOP-39
      ENDIF
      GO TO (61,62,63,64,65,66,67,68,69,70,71,72,73,74),KOP-60
      GO TO 402

C  ELEMENTARY ARITHMETIC

    1 R(K)=R(I)+R(J)
      GO TO 301
    2 R(K)=R(I)-R(J)
      GO TO 301
    3 R(K)=R(I)*R(J)
      GO TO 301
    5 R(K)=R(I)
      GO TO 301
    4 IF (R(J).EQ.0.D0) GO TO 401
      R(K)=R(I)/R(J)
      GO TO 301
    6 R(K)=-R(I)
      GO TO 301

C  LOGICAL OPERATIONS

    7 IF (R(I).EQ.R(J)) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
    8 IF (R(I).GT.R(J)) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
    9 IF (R(I).LT.R(J)) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
   10 IF (R(I).GE.R(J)) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
   11 IF (R(I).LE.R(J)) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
   12 IF (R(I).NE.R(J)) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
   13 IF (R(I).EQ.0.D0) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
   14 IF (R(I).NE.0.D0.AND.R(J).NE.0.D0) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
   15 IF (R(I).NE.0.D0.OR.R(J).NE.0.D0) THEN
         R(K)=1.D0
      ELSE
         R(K)=0.D0
      ENDIF
      GO TO 301
   16 IF (R(I).LT.0.D0) GO TO 403
      IF (R(I).EQ.0.D0.AND.R(J).LT.0.D0) GO TO 401
      R(K)=R(I)**R(J)
      GO TO 301

C  STOP, UNCONDITIONAL JUMP, CONDITIONAL JUMP, INITIALIZE DO, RETURN

   25 KERR=0
      RETURN
   26 NI=I
      GO TO 302
   27 IF (R(J).EQ.0.D0) NI=I-1
      GO TO 301
   28 L1DO=.TRUE.
      GO TO 301
   29 GO TO 301

C  OPEN FILE

   30 II=R(I)+.5D0
      IF (II.LT.1.OR.II.GT.6) GO TO 407
      II=MFILE(II)
      IF (II.LT.1) GO TO 301
      BSTG=' '
      M=LCSTG(J-1)+1
      N=LCSTG(J)
      DO 130 L=M,N
  130 ASTG1(L-M+1)=CARRAY(L)
      CSTG=' '
      M=LCSTG(K-1)+1
      N=LCSTG(K)
      DO 131 L=M,N
  131 ASTG1(L-M+201)=CARRAY(L)
      OPEN (II,FILE=BSTG,STATUS=CSTG,ERR=408)
      MFILE(II)=-II
      GO TO 301

C  CLOSE FILE

   31 N=R(I)+.5
      IF (N.LT.1.OR.N.GT.6) GO TO 407
      IF (MFILE(N).GT.0) GO TO 301
      CLOSE (MFILE(N))
      GO TO 301

C  Rk = Ki

   21 NN=KVDAT(4,I)
      IF (NN.GT.0) THEN
         R(K)=R(NN)
      ELSE
         CALL EXTGET(R(K),-NN,0,0,0,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C  Rk = Ki(Rj)

   22 NN=KVDAT(4,I)
      LL=R(J)+.5D0
      IF (NN.GT.0) THEN
         R(K)=R(NN+LL-1)
      ELSE
         CALL EXTGET(R(K),-NN,LL,0,0,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C  Rk = Ki(Rj,Rl)

   40 NN=KVDAT(4,I)
      IF (NN.GT.0) THEN
         LL=(R(L)-1.D0)*KVDAT(1,I)+R(J)+.5D0
         R(K)=R(NN+LL-1)
      ELSE
         LL=R(J)+.5D0
         LM=R(L)+.5D0
         CALL EXTGET(R(K),-NN,LL,LM,0,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C  Rk = Ki(Rj,Rl,Rm)

   41 NN=KVDAT(4,I)
      IF (NN.GT.0) THEN
         LL=((R(M)-1.D0)*KVDAT(2,I)+R(L)-1.D0)*KVDAT(1,I)+R(J)+.5D0
         R(K)=R(NN+LL-1)
      ELSE
         LL=R(J)+.5D0
         LM=R(L)+.5D0
         LN=R(M)+.5D0
         CALL EXTGET(R(K),-NN,LL,LM,LN,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C  Kk = Ri

   23 NN=KVDAT(4,K)
      IF (NN.GT.0) THEN
         R(NN)=R(I)
      ELSE
         CALL EXTPUT(R(I),-NN,0,0,0,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C  Kk(Rj) = Ri

   24 NN=KVDAT(4,K)
      LL=R(J)+.5D0
      IF (NN.GT.0) THEN
         R(NN+LL-1)=R(I)
      ELSE
         CALL EXTPUT(R(I),-NN,LL,0,0,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C Kk(Rj,Rl) = Ri

   42 NN=KVDAT(4,K)
      IF (NN.GT.0) THEN
         LL=(R(L)-1.D0)*KVDAT(1,K)+R(J)+.5D0
         R(NN+LL-1)=R(I)
      ELSE
         LL=R(J)+.5D0
         LM=R(L)+.5D0
         CALL EXTPUT(R(I),-NN,LL,LM,0,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C  Kk(Rj,Rl,Rm) = Ri

   43 NN=KVDAT(4,K)
      IF (NN.GT.0) THEN
         LL=((R(M)-1.D0)*KVDAT(2,K)+R(L)-1.D0)*KVDAT(1,K)+R(J)+.5D0
         R(NN+LL-1)=R(I)
      ELSE
         LL=R(J)+.5D0
         LM=R(L)+.5D0
         LN=R(M)+.5D0
         CALL EXTPUT(R(I),-NN,LL,LM,LN,KE)
         IF (KE.NE.0) GO TO 406
      ENDIF
      GO TO 301

C  DO i Rk = Rj,Rl,Rm

   44 IF (L1DO) THEN
         R(K)=R(J)
         L1DO=.FALSE.
      ELSE
         R(K)=R(K)+R(M)
      ENDIF
      IF (R(M).GT.0.AND.R(K).LT.(R(L)+1.D-6*R(M))) GO TO 301
      IF (R(M).GT.0.) THEN
         IF (R(K).LT.(R(L)+1.D-6*R(M))) GO TO 301
      ELSE
         IF (R(M).LT.0.AND.R(K).GT.(R(L)-1.D-6*R(M))) GO TO 301
      ENDIF
      NI=I
      GO TO 302

C  WRITE (Rj,Si) Rl,Rm,...        k = number of values to write

   45 II1=LCSTG(I-1)+1
      II2=LCSTG(I)
      ASTG=" "
      DO 146 II=II1,II2
  146 ASTG1(II-II1+1)=CARRAY(II)
      J=R(J)+.5D0
      IF (J.LT.1.OR.J.GT.6) GO TO 407
      J=-MFILE(J)
      IF (J.LT.1) GO TO 407
      IF (K.GT.0) THEN
         DO 147 II=1,K
         II1=MOD(II+2,3)+2
         IF (II1.EQ.2) NI=NI+1
         L=KODE(II1,NI)
  147    RW(II)=R(L)
         WRITE (J,ASTG) (RW(II),II=1,K)
      ELSE
         WRITE (J,ASTG)
      ENDIF
      GO TO 301

C  READ (Rj,Si) Rl,Rm,...         k = number of values to read

   46 II1=LCSTG(I-1)+1
      II2=LCSTG(I)
      ASTG=" "
      DO 142 II=II1,II2
  142 ASTG1(II-II1+1)=CARRAY(II)
      J=R(J)+.5D0
      IF (J.LT.1.OR.J.GT.6) GO TO 407
      J=-MFILE(J)
      IF (J.LT.1) GO TO 407
      IF (K.GT.0) THEN
         READ (J,ASTG) (RW(II),II=1,K)
         DO 143 II=1,K
         II1=MOD(II+2,3)+2
         IF (II1.EQ.2) NI=NI+1
         L=KODE(II1,NI)
  143    R(L)=RW(II)
      ELSE
         READ (J,ASTG)
      ENDIF
      GO TO 301

C  FUNCTIONS

   61 R(K)=DEXP(R(I))
      GO TO 301
   62 IF (R(I).LT.0.D0) GO TO 405
      R(K)=DLOG(R(I))
      GO TO 301
   63 R(K)=DABS(R(I))
      GO TO 301
   64 IF (R(I).LT.0.D0) GO TO 404
      R(K)=DSQRT(R(I))
      GO TO 301
   65 R(K)=DSIN(R(I))
      GO TO 301
   66 R(K)=DCOS(R(I))
      GO TO 301
   67 R(K)=DTAN(R(I))
      GO TO 301
   68 LL=R(I)
      R(K)=LL
      GO TO 301
   69 R(K)=DASIN(R(I))
      GO TO 301
   70 R(K)=DATAN(R(I))
      GO TO 301
   71 R(K)=DSINH(R(I))
      GO TO 301
   72 R(K)=DCOSH(R(I))
      GO TO 301
   73 R(K)=RANDOM(0)
      GO TO 301
   74 LL=R(I)+.00001D0
      R(K)=RANDOM(LL)
      GO TO 301

C  ERROR EXITS

  401 KERR=1
      GO TO 313
  402 KERR=2
      GO TO 313
  403 KERR=3
      GO TO 313
  404 KERR=4
      GO TO 313
  405 KERR=5
      GO TO 313
  406 KERR=6
      GO TO 313
  407 KERR=7
      GO TO 313
  408 KERR=8

  313 ERRMSG=ERRS(KERR)
      IF (LEVERR.LT.2) LEVERR=2
      IF (LEVELC) WRITE (NFOUT,314) NI
  314 FORMAT(' ERROR AT CODE LINE',I5)
      KERR=KERR+350
      RETURN
      END
C***********************************************************************
      SUBROUTINE NUMGET (A,NRD,N2,VAL,KODE)
C***********************************************************************
C  NUMBER TRANSLATION ROUTINE, ASCII TO REAL*8

C  A()    = ASCII SOURCE (INPUT, CHARACTER*1)
C  NRD    = MAX CHARACTERS IN A (INPUT, INTEGER)
C  N2     = LAST OFFSET OF NUMBER IN A (OUTPUT, INTEGER)
C  VAL    = NUMBER RETURNED (OUTPUT, REAL*8)
C  KODE   = RETURN CODE (OUTPUT, INTEGER)
C         = 1 ==> NUMBER RETURNED
C         = 2 ==> NOT A NUMBER

C  NOTE: THIS VERSION SKIPS LEADING BLANKS AND A BLANK AFTER + OR -,
C        OTHERWISE A BLANK IS A TERMINATOR

C***********************************************************************
      CHARACTER*1 A(*),TST(15)
      REAL*8 VAL,V,FM,SGN
      DATA TST/'0','1','2','3','4','5','6','7','8','9','.','E','+',
     1 '-',' '/
      KODE=2
      IF (A(1).GT.'9') RETURN
      V=0.D0
      NEX=0
      NSEX=1
      NEE=0
      ND=0
      NN=0
      NS=0
      NEXS=1
      SGN=1.D0
      FM=1.D-1
      N=0
      K=15

C  CLASSIFY NEXT CHARACTER

    3 N2=N
      N=N+1
      IF (N.GT.NRD) GO TO 6
      KO=K
      DO 4 I=1,15
      IF (A(N).EQ.TST(I)) THEN
         K=I
         GO TO 5
      ENDIF
    4 CONTINUE
      GO TO 6
    5 IF (K.LT.11) GO TO 7
      IF (K.EQ.11) GO TO 8
      IF (K.EQ.12) GO TO 9
      IF (K.EQ.15) THEN
         IF (KO.GT.12) GO TO 3
         GO TO 6
      ENDIF

C  PROCESS SIGN

      IF (NEXS.EQ.0) GO TO 6
      NEXS=0
      IF (K.EQ.13) GO TO 10
      IF (NS.GT.0.) GO TO 6
      IF (NEE.EQ.0) THEN
          SGN=-1.D0
      ELSE
          NSEX=-1
      ENDIF
   10 IF (NS.GT.0) RETURN
      NS=1
      GO TO 3

C  PROCESS DECIMAL

    8 IF (ND.GT.0.OR.NEE.GT.0) RETURN
      NEXS=0
      ND=1
      GO TO 3

C  PROCESS E

    9 IF (NEE.GT.0) RETURN
      NEE=1
      NS=0
      NEXS=1
      GO TO 3

C  PROCESS NUMBER

    7 NN=1
      NS=1
      NEXS=0
      IF (NEE.GT.0) GO TO 11
      IF (ND.EQ.0) THEN
          V=1.D1*V+(K-1)
      ELSE
          V=V+FM*(K-1)
          FM=FM*1.D-1
      ENDIF
      GO TO 3
   11 NEX=10*NEX+K-1
      GO TO 3

C  RETURN RESULT

    6 IF (NN.EQ.0.OR.NEXS.EQ.1) RETURN
      IF (NEX.GT.0) V=V*(1.D1**(NSEX*NEX))
      V=SGN*V
      KODE=1
      VAL=V
      RETURN
      END
C***********************************************************************
      SUBROUTINE GETNAM (A,NRD,N2,NAM,KRET)
C***********************************************************************
C  VARIABLE NAME PARSING ROUTINE

C  A()    = ASCII SOURCE (INPUT, CHARACTER*1)

C  NRD    = MAX CHARACTERS IN A (INPUT, INTEGER)

C  N2     = LAST OFFSET OF NAME IN A (OUTPUT, INTEGER)

C  NAM    = NAME RETURNED (OUTPUT, CHARACTER*6)

C  KRET   = RETURN CODE (OUTPUT, INTEGER)
C         = 1 ==> NAME RETURNED
C         = 2 ==> NAME NOT FOUND
C         = 3 ==> NAME TOO LONG

C  NOTES:
C     1)  LEADING BLANKS ARE SKIPPED
C***********************************************************************
      CHARACTER*1 A(*),AAA,ZZZ,ZERO,NINE,BLK,NAMS1(6)
      CHARACTER*6 NAM,BLKNAM,NAMS
      EQUIVALENCE (NAMS,NAMS1)

      DATA AAA/'A'/,ZZZ/'Z'/,ZERO/'0'/,NINE/'9'/,BLK/' '/,
     & BLKNAM/'      '/

      KRET=2
      DO 1 I=1,NRD
      K=I
      IF (A(I).NE.BLK) GO TO 2
    1 CONTINUE
      RETURN
    2 IF (A(K).LT.AAA.OR.A(K).GT.ZZZ) RETURN
      NAMS=BLKNAM
      J=0
      DO 3 I=K,NRD
      IF (A(I).GT.ZZZ.OR.A(I).LT.ZERO) GO TO 4
      IF (A(I).GT.NINE.AND.A(I).LT.AAA) GO TO 4
      N2=I
      J=J+1
      IF (J.GT.6) THEN
         KRET=3
         RETURN
      ENDIF
    3 NAMS1(J)=A(I)
    4 KRET=1
      NAM=NAMS
      RETURN
      END
C***********************************************************************
      SUBROUTINE GETSTG (AA,N1,N2,NSTG,CARRAY,LCSTG)
C***********************************************************************
C  QUOTED STRING PARSING ROUTINE

C  AA()     = ASCII SOURCE (INPUT, CHARACTER*1)

C  N1       = OFFSET OF 1ST QUOTE IN AA ON INPUT (INTEGER)
C           = OFFSET OF 2ND QUOTE IN AA ON OUTPUT

C  N2       = LAST OFFSET IN AA TO BE TESTED (INPUT, INTEGER)

C  NSTG     = STRING NUMBER RETURNED (OUTPUT, INTEGER)
C           = 0 ==> SECOND QUOTE NOT FOUND
C           =-1 ==> OUT OF STRING SPACE

C  CARRAY() = STRING STORAGE SPACE (INPUT & OUTPUT, CHARACTER*1)

C  LCSTG()  = STRING POINTER SPACE (INPUT & OUTPUT, INTEGER)

C***********************************************************************
      INCLUDE 'compc.h'

      CHARACTER*1 AA(*)
      INTEGER LCSTG(*)
      CHARACTER*1 CARRAY(*)

C  FIND THE 2ND QUOTE

      J=N1+1
      DO 1 I=J,N2
      IF (AA(I).EQ.CHAR(39)) THEN
         N1=I
         GO TO 2
      ENDIF
    1 CONTINUE
      NSTG=0
      RETURN

C  CHECK AVAILABLE STRING SPACE

    2 NSTG=-1
      IF (NSTGU.GE.NSTGL) RETURN
      I=LCSTG(NSTGU)
      IF ((I+N1-J).GE.NCHRL) RETURN

C  COPY STRING TO STRING SPACE AND RETURN

      NSTGU=NSTGU+1
      K=N1-1
      DO 3 N=J,K
      I=I+1
    3 CARRAY(I)=AA(N)
      LCSTG(NSTGU)=I
      NSTG=NSTGU
      END
C***********************************************************************
      FUNCTION RANDOM(ISEED)
C***********************************************************************
C  GENERATES A RANDOM NUMBER BETWEEN 0. AND 1.
C  FROM NUMERICAL RECIPES (MODIFIED)

C  ISEED =  A NUMBER USED TO SEED THE SEQUENCE (INPUT, INTEGER)
C        >  0 START A NEW SEQUENCE
C        <= 0 CONTINUE EXISTING SEQUENCE
C        IF ON THE FIRST CALL TO RANDOM, ISEED IS <= 0 THEN THE SEQUENCE
C        IS SEEDED WITH 1

C***********************************************************************
      DIMENSION R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=1./M1)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=1./M2)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      DATA IFF/0/,IDUM/0/,R/97*0./

      IF (ISEED.GT.0.OR.IFF.EQ.0) THEN
         IFF=1
         IDUM=MAX(ISEED,1)
         IX1=MOD(IC1-IDUM,M1)
         IX1=MOD(IA1*IX1+IC1,M1)
         IX2=MOD(IX1,M2)
         IX1=MOD(IA1*IX1+IC1,M1)
         IX3=MOD(IX1,M3)
         IDUM=1
         DO 1 J=1,97
         IX1=MOD(IA1*IX1+IC1,M1)
         IX2=MOD(IA2*IX2+IC2,M2)
    1    R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      ENDIF

      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF (J.GT.97) J=MOD(J,97)
      RANDOM=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1

      END
