C  SIZE.FOR - SYMBOL SUBSTITUTION PROGRAM

C  THIS PROGRAM CAN REDIMENSION A FORTRAN OR C FILE.  IT SEARCHES A
C  SOURCE FILE FOR A SPECIFIC SET OF SYMBOLS AND REPLACES THOSE SYMBOLS
C  WITH OTHER SYMBOLS OR NUMBERS.  SIZE CAN ALSO ACTIVATE OR DISABLE
C  MACHINE OR OPERATING SYSTEM SPECIFIC CODE.

C  HISTORY:

C  JOHN WHEELER      2/22/95    ORIGINAL BETA CODE
C  JOHN WHEELER      9/1/97     ADD UPPER, PCSLASH, AND Include_File
C  JOHN WHEELER     12/17/97    ADD CALLUP
C  JOHN WHEELER      5/8/98     IMPROVE INCLUDE FILE CAPABILITY
C  JOHN WHEELER     10/12/98    FILE NAMES FROM COMMAND LINE
C  JOHN WHEELER      3/30/01    ADD REMOVE C++ COMMENT CAPABILITY

C  NOTES:

C  1) The program reads 5 types of instructions from a control file.
C     Each type of instruction is headed by one of the following
C     control words:

C     Set_Symbols      ==> Define symbols for use only in defining
C                          other symbols.  Do not search for these
C                          symbols in source files.
C     Replace_Symbols  ==> Search source files for these symbols and
C                          replace with other symbols or numbers
C     Target_Directory ==> Place modified source files in this
C                          directory
C     Source_Directory ==> Obtain original source files from this
C                          directory
C     Source_Files     ==> Original and modified source file names

C     Include_File     ==> Include file. May be nested.

C     Control words may be read more than once but only the last
C     values input after a control word will be retained.  Order the
C     Set_Symbols group before the Replace_Symbols group since the
C     former control word clears all symbol definitions.

C  2) In the source files, a word to be replaced must be preceeded
C     by a $.  Inside of quotes ("" or ''), $ will be processed as a
C     normal character.  Examples:

C     DIMENSION X($XDIM)
C     WRITE (*,*) 'THE TOTAL PROJECT COST IS $',DOL

C  3) Set_Symbols and Replace_Symbols records have the same format.
C     A symbol is followed by its definition.  There are three types
C     of definitions:

C     a) Character definitions which may be either a series of char-
C        acters with no embedded blanks or a string of characters
C        enclosed in quotes (").  To simply remove a symbol, set the
C        replacement symbol to "".

C     b) Numeric definitions which consist of numbers, numeric symbols
C        previously defined, and the operators +, -, *, /, (, and ).
C        A numeric definition must start with an equal sign (=),
C        otherwise it will be interperted as a character definition.

C     c) Size control definitions which determine how the SIZE program
C        itself responds to certain conditions.  Currently there are
C        four control definitions:

C        1)  PCSLASH YES which causes / in the Target_Directory,
C            Source_Directory, and Include_File records to be converted
C            to \.  PCSLASH NO causes no change in the directory names.
C            This command affects only directory names that follow it.

C        2)  UPPER YES which causes any lowercase characters in an
C            alpha-numeric source word following $UPPER to be converted to
C            uppercase.  UPPER NO leaves the source word unchanged.

C        3)  CALLUP YES which causes any lowercase characters in an 
C            alpha-numeric source word following $CALLUP to be converted to
C            uppercase.  CALLUP NO leaves the source word unchanged.

C        4)  NOCOMMENT YES which causes c++ coments begun with // to be
C            deleted.  This control is applied to all files including those
C            that are copied but not otherwise processed.

C  4) Target_Directory and Source_Directory records consist of a
C     string of characters terminated by either \ or /.

C  5) Source_Files records consist of an input file name optionally
C     followed by an output file name or an extension for an output
C     file name.  The input file name must have an extension with d as
C     its first character, otherwise the file is simply copied without
C     modification.  The output file name may consist of only an
C     extension in which case the primary part of the name will be
C     obtained from the input file name.  An output file can not have
C     the extension beginning with d.

C  6) The maximum source record length is 100 characters.  The maximum
C     output record length is 80 characters.  The maximum control
C     record length is 100 characters.  The maximum symbol length
C     is 20 characters.  The maximum directory plus file name length
C     is 60 characters.  The maximum length of the command line is 254
C     characters.

C  7) Optionally, a single file may be updated.  When the control file is
C     requested, follow the name of the control file with a space and
C     the base name of the source file to be updated. (The base name may
C     include an extension which will be discarded before processing occurs.)
C     This can be useful for incorporating SIZE into a makefile.  In a
C     makefile the lines:

C     read1.f:../input/read1.df control.h baldat.h
C              echo ../tests/itest.siz $@ > ech
C              ../exec/size < ech

C     will cause the file read1.f to be created from the source file
C     ../input/read1.df using the symbol subtitutions in itest.siz.  Note
C     that for this option to work, the control file must include:

C     Source_Directory
C     ../input/
C     Source_Files
C     read1.df   .f

C  8) Optionally, a single file may be updated even though the file is not
C     referenced in the control file.  Follow the name of the control file
C     with a space, the full name of the source file (including directory and
C     extension), another space, and then the full name of the target file.
C     In a makefile, the lines:

C     read1.f:../input/read1.df control.h baldat.h
C              echo ../tests/itest.siz $** $@ > ech
C              ../exec/size < ech

C     will cause the file read1.f to be created from the source file
C     ../input/read1.df using the symbol subtitutions in itest.siz.  The
C     make macro $** actually causes all the dependents (source files) to
C     be listed so SIZE actually uses the first, the second, and the last
C     file names on the command line.  

C  9) All symbols including control words are case sensitive.  File
C     names may also be case sensitive depending on the operating
C     system.

C*********************************************************************
      PARAMETER (MAXSYM=200,MAXFIL=200)

      INTEGER       NUMS(MAXSYM)
      LOGICAL       DOSUB,SINGLE,TRACE,PCSLASH,ISNUM(MAXSYM),NOCOMMENT
      LOGICAL       DOSUBV(MAXFIL),NODATA
      CHARACTER*1   RIN1(100),TSYM1(20),FIN1(60),FOUT1(60),EXT1(20),
     & QUOT,DUMMY1(254),DIMFIL1(80),SFILE1(20),TFILE1(20),SLASHPC1(20),
     & FTARG1(60),FSORC1(60)
      CHARACTER*20  SYMB(MAXSYM),SYMR(MAXSYM),TSYM,EXT,SFILE,TFILE
      CHARACTER*20  SLASH,YES,UPPER,SLASHPC,CALLUP,NOCOMT,SFILEV(MAXFIL)
      CHARACTER*60  FIN,FOUT,FINV(MAXFIL),FOUTV(MAXFIL),FTARG,FSORC
      CHARACTER*80  DIMFIL
      CHARACTER*254 DUMMY
      CHARACTER*100 RIN

      EQUIVALENCE (RIN,RIN1(1)),(TSYM,TSYM1(1)),(FIN,FIN1(1)),
     & (FOUT,FOUT1(1)),(EXT,EXT1(1)),(SFILE,SFILE1(1)),
     & (TFILE,TFILE1(1)),(DUMMY,DUMMY1(1)),(DIMFIL,DIMFIL1(1)),
     & (SLASHPC,SLASHPC1(1)),(FTARG,FTARG1(1)),(FSORC,FSORC1(1))

      TRACE=.FALSE.

      PCSLASH=.FALSE.
      SLASH='PCSLASH'
      UPPER='UPPER'
      CALLUP='CALLUP'
      NOCOMT='NOCOMMENT'
      NUPPER=0
      NCALLUP=0
      YES='YES'
      NCW=0
      NSYM1=0
      NSYM2=0
      NCIN=0
      NCOUT=0
      NSORC=0
      QUOT=CHAR(34)
      SLASHPC=' '
      SLASHPC1(1)=CHAR(92)
      SINGLE=.FALSE.
      NODATA=.FALSE.
      NOCOMMENT=.FALSE.

C  GET COMMAND LINE

      WRITE (*,1)
    1 FORMAT(' ENTER COMMAND LINE: ')
      READ (*,2) DUMMY
    2 FORMAT(A254)

      IF (TRACE) WRITE (*,*) 'COMMAND LINE:'
      IF (TRACE) WRITE (*,*) DUMMY

C  GET CONTROL FILE NAME

      I1=1
      CALL GETNAM (DUMMY1,254,I1,DIMFIL1,80,PCSLASH)
      IF (I1.EQ.0) THEN
         WRITE (*,*) 'ERROR - NO CONTROL FILE NAME'
         STOP 13
      ENDIF

C  GET SECOND FILE NAME, IF ANY

      CALL GETNAM (DUMMY1,254,I1,FTARG1,60,PCSLASH)
      IF (I1.GT.0) SINGLE=.TRUE.

C  GET LAST FILE NAME, IF ANY

    6 CALL GETNAM (DUMMY1,254,I1,FIN1,60,PCSLASH)
      IF (I1.GT.0) THEN
         NODATA=.TRUE.
         FSORC=FIN
         GO TO 6
      ENDIF

      IF (NODATA) THEN
         FIN=FTARG
         FTARG=FSORC
         FSORC=FIN
      ENDIF

C  OPEN A SIZE DATA FILE

   92 NFILE=4
   68 IF (TRACE) WRITE (*,*) 'OPENING FILE ',DIMFIL,' AS #',NFILE
      OPEN (NFILE,FILE=DIMFIL,STATUS='OLD')

C  READ A SIZE DATA FILE RECORD

    3 READ (NFILE,4,END=20) RIN
    4 FORMAT (A100)

C  PARSE AND TEST FOR BLANK RECORD, CONTROL WORD, OR NOT A SYMBOL

      N1=1
      CALL PARSE (RIN1,N1,100,NEX,KOD)
      IF (KOD.EQ.0) GO TO 3
      IF (KOD.GE.3.AND.KOD.LE.11) THEN
         NCW = KOD-2
         IF (NCW.EQ.1) THEN
            NSYM1=0
            NSYM2=0
         ENDIF
         GO TO 3
      ENDIF
      IF (NCW.EQ.0) THEN
         WRITE (*,*) 'ERROR - NO CONTROL WORD'
         GO TO 13
      ENDIF

C  PROCESS INCLUDE FILE

      IF (NCW.EQ.6) THEN
         NFILE=NFILE+1
         I1=1
         CALL GETNAM (RIN1,100,I1,DIMFIL1,80,PCSLASH)
         IF (I1.EQ.0) GO TO 3
         GO TO 68
      ENDIF

C  MUST BE A SYMBOL

      IF (KOD.NE.1) THEN
         WRITE (*,*) 'ERROR - EXPECTED SYMBOL NOT FOUND IN'
         GO TO 13
      ENDIF

C  COPY 1ST WORD THEN BRANCH ON CONTROL WORD IN EFFECT

      N2=1
      CALL GETNAM (RIN1,100,N2,TSYM1,20,PCSLASH)
      GO TO (11,11,12,14,15,13,13,13,13),NCW

C  PROCESS SYMBOL DEFINITION

   11 IF (NSYM2.GE.MAXSYM) THEN
         WRITE (*,*) 'ERROR - TOO MANY SYMBOLS'
         GO TO 13
      ENDIF
      CALL PARSE (RIN1,NEX,100,N2,KOD)

      IF (KOD.EQ.18) THEN
         CALL EVAL(RIN1,N2,100,NSYM2,SYMB,ISNUM,NUMS,NN,KOD)
         IF (KOD.NE.0) GO TO 13
         NSYM2=NSYM2+1
         IF (NCW.EQ.1) NSYM1=NSYM2
         ISNUM(NSYM2)=.TRUE.
         SYMB(NSYM2)=TSYM
         NUMS(NSYM2)=NN
         WRITE (TSYM,51) NN
   51    FORMAT(I12)
         I1=1
         CALL GETNAM (TSYM1,12,I1,EXT1,20,PCSLASH)
         SYMR(NSYM2)=EXT

      ELSE

         IF (KOD.EQ.19) THEN
            L=NEX+1
            DO 53 I=L,100
            IF (RIN1(I).EQ.QUOT) THEN
               N2=I
               GO TO 54
            ENDIF
   53       CONTINUE
            WRITE (*,*) 'ERROR - UNPAIRED QUOTE'
            GO TO 13
         ENDIF

         IF (KOD.NE.1.AND.KOD.NE.2) THEN
            WRITE (*,*) 'ERROR - EXPECTED SYMBOL NOT FOUND, CODE',KOD
            GO TO 13
         ENDIF

   54    NSYM2=NSYM2+1
         IF (NCW.EQ.1) NSYM1=NSYM2
         ISNUM(NSYM2)=.FALSE.
         SYMB(NSYM2)=TSYM
         L=N2-NEX+1
         TSYM=' '
         DO 50 I=1,L
   50    TSYM1(I)=RIN1(NEX+I-1)
         SYMR(NSYM2)=TSYM
         IF (SYMB(NSYM2).EQ.SLASH) THEN
            IF (SYMR(NSYM2).EQ.YES) THEN
               PCSLASH=.TRUE.
               SYMR(NSYM2)=SLASHPC
            ELSE
               SYMR(NSYM2)='/'
            ENDIF
         ENDIF
         IF (SYMB(NSYM2).EQ.UPPER) THEN
            IF (SYMR(NSYM2).EQ.YES) NUPPER=NSYM2
            SYMR(NSYM2)='""'
         ENDIF
         IF (SYMB(NSYM2).EQ.CALLUP) THEN
            IF (SYMR(NSYM2).EQ.YES) NCALLUP=NSYM2
            SYMR(NSYM2)='""'
         ENDIF
         IF (SYMB(NSYM2).EQ.NOCOMT) THEN
            IF (SYMR(NSYM2).EQ.YES) NOCOMMENT=.TRUE.
            SYMR(NSYM2)='""'
         ENDIF
      ENDIF
      IF (TRACE) WRITE (*,*) SYMB(NSYM2),SYMR(NSYM2)
      GO TO 3

C  SET OUTPUT DIRECTORY

   12 FOUT=' '
      NCOUT=0
      DO 30 I=1,60
      IF (RIN1(I).EQ.'$') GO TO 3
      IF (PCSLASH.AND.RIN1(I).EQ.'/') RIN1(I)=CHAR(92)
      IF (RIN1(I).NE.' ') THEN
         NCOUT=NCOUT+1
         FOUT1(NCOUT)=RIN1(I)
      ENDIF
   30 CONTINUE
      GO TO 3

C  SET INPUT DIRECTORY

   14 FIN=' '
      NCIN=0
      DO 31 I=1,60
      IF (RIN1(I).EQ.'$') GO TO 3
      IF (PCSLASH.AND.RIN1(I).EQ.'/') RIN1(I)=CHAR(92)
      IF (RIN1(I).NE.' ') THEN
         NCIN=NCIN+1
         FIN1(NCIN)=RIN1(I)
      ENDIF
   31 CONTINUE
      GO TO 3

C  PROCESS SOURCE FILE NAMES

   15 L=NCIN+1
      DO 42 I=L,60
   42 FIN1(I)=' '
      L=NCOUT+1
      DO 43 I=L,60
   43 FOUT1(I)=' '

      JCIN=NCIN
      IPIN=0
      DO 32 I=1,100
      K=I
      IF (RIN1(I).EQ.'$') GO TO 33
      IF (RIN1(I).EQ.' ') THEN
         IF (JCIN.GT.NCIN) GO TO 33
      ELSE
         JCIN=JCIN+1
         IF (JCIN.GT.60) THEN
            WRITE (*,*) 'ERROR - PATH TOO LONG'
            GO TO 13
         ENDIF
         FIN1(JCIN)=RIN1(I)
         IF (FIN1(JCIN).EQ.'.') IPIN=JCIN
      ENDIF
   32 CONTINUE
   33 IF (JCIN.EQ.NCIN) GO TO 3
      IF (IPIN.EQ.(NCIN+1)) THEN
         WRITE (*,*) 'ERROR - INVALID SOURCE FILE NAME'
         GO TO 13
      ENDIF

      JCOUT=NCOUT
      IPOUT=0
      DO 34 I=K,100
      IF (RIN1(I).EQ.'$') GO TO 35
      IF (RIN1(I).EQ.' ') THEN
         IF (JCOUT.GT.NCOUT) GO TO 35
      ELSE
         JCOUT=JCOUT+1
         IF (JCOUT.GT.60) THEN
            WRITE (*,*) 'ERROR - PATH TOO LONG'
            GO TO 13
         ENDIF
         FOUT1(JCOUT)=RIN1(I)
         IF (FOUT1(JCOUT).EQ.'.') IPOUT=JCOUT
      ENDIF
   34 CONTINUE

   35 IF (IPOUT.EQ.(NCOUT+1).OR.JCOUT.EQ.NCOUT) THEN
         L1=NCIN+1
         L2=JCIN
         IF (IPOUT.GT.0) THEN
            IF (IPIN.GT.0) L2=IPIN-1
            L=L2-L1+1
            DO 38 I=JCOUT,IPOUT,-1
   38       FOUT1(I+L)=FOUT1(I)
            JCOUT=JCOUT+L
            IPOUT=IPOUT+L
         ENDIF
         DO 39 I=L1,L2
         L=NCOUT+I-L1+1
         IF (L.GT.JCOUT) JCOUT=L
         IF (FIN1(I).EQ.'.') IPOUT=L
   39    FOUT1(L)=FIN1(I)
      ENDIF

      DOSUB=.FALSE.
      IF (IPIN.GT.0.AND.JCIN.GT.IPIN.AND.(FIN1(IPIN+1).EQ.'d'.OR.
     &   FIN1(IPIN+1).EQ.'D')) DOSUB=.TRUE.

      IF (IPOUT.GT.0.AND.NCOUT.GT.IPOUT.AND.(FOUT1(IPIN+1).EQ.'d'.OR.
     &   FOUT1(IPIN+1).EQ.'D')) THEN
         WRITE (*,*) 'ERROR - OUTPUT FILE EXTENSION BEGINS WITN D'
         GO TO 13
      ENDIF

      DO 70 I=1,60
      I1=I
      IF (FIN1(I).NE.' ') GO TO 71
   70 CONTINUE
      STOP 13
   71 I2=I1
      DO 72 I=I2,60
      IF (FIN1(I).EQ.'/'.OR.FIN1(I).EQ.CHAR(92)) I1=I+1
   72 CONTINUE
      TFILE=' '
      DO 73 I=I1,60
      IF (FIN1(I).EQ.'.'.OR.FIN1(I).EQ.' ') GO TO 74
   73 TFILE1(I-I1+1)=FIN1(I)

   74 NSORC=NSORC+1
      IF (NSORC.GT.MAXFIL) THEN
         WRITE(*,*) 'ERROR - MAX NUMBER OF FILE NAMES EXCEEDED'
         STOP 13
      ENDIF
      DOSUBV(NSORC)=DOSUB
      FINV(NSORC)=FIN
      FOUTV(NSORC)=FOUT
      SFILEV(NSORC)=TFILE

      GO TO 3

C  BACK UP TO PREVIOUS INCLUDE FILE OR END SIZE DATA FILE PROCESSING

   20 CLOSE (NFILE)
      IF (TRACE) WRITE (*,*) 'CLOSING FILE #',NFILE
      IF (NFILE.GT.4) THEN
         NFILE=NFILE-1
         NCW=6
         GO TO 3
      ENDIF

C  PROCESS SINGLE FILE, NO CONTROL DATA OPTION

      IF (NODATA) THEN

         IF (TRACE) WRITE (*,*) 'SINGLE FILE, NO CONTROL DATA OPTION'

         WRITE (*,*) 'INPUT FILE  = ',FSORC
         WRITE (*,*) 'OUTPUT FILE = ',FTARG
         CALL DOCOPY (FSORC,FTARG,NSYM2-NSYM1,SYMB(NSYM1+1),
     &      SYMR(NSYM1+1),.TRUE.,NUPPER,NCALLUP,NOCOMMENT)

         STOP 0

      ENDIF

C  PROCESS SINGLE FILE, CONTROL DATA OPTION

      IF (SINGLE) THEN

         IF (TRACE) WRITE (*,*) 'SINGLE FILE, CONTROL DATA OPTION'

         SFILE=' '
         DO 96 I=1,20
         IF (FTARG1(I).EQ.'.') GO TO 97
   96    SFILE1(I)=FTARG1(I)

   97    DO 75 I=1,NSORC
         NSC=I
         IF (SFILE.EQ.SFILEV(I)) GO TO 76
   75    CONTINUE
         WRITE(*,*) 'ERROR - BASE FILE NAME NOT FOUND: ',SFILE
         STOP 13

   76    WRITE (*,*) 'INPUT FILE  = ',FINV(NSC)
         WRITE (*,*) 'OUTPUT FILE = ',FOUTV(NSC)
         CALL DOCOPY (FINV(NSC),FOUTV(NSC),NSYM2-NSYM1,SYMB(NSYM1+1),
     &   SYMR(NSYM1+1),DOSUBV(NSC),NUPPER,NCALLUP,NOCOMMENT)

         STOP 0

      ENDIF

C  PROCESS ALL SOURCE DATA FILES OPTION

      IF (TRACE) WRITE (*,*) 'ALL SOURCE DATA FILES OPTION'


      DO 77 NSC=1,NSORC

      WRITE (*,*) 'INPUT FILE  = ',FINV(NSC)
      WRITE (*,*) 'OUTPUT FILE = ',FOUTV(NSC)
   77 CALL DOCOPY (FINV(NSC),FOUTV(NSC),NSYM2-NSYM1,SYMB(NSYM1+1),
     &   SYMR(NSYM1+1),DOSUBV(NSC),NUPPER,NCALLUP,NOCOMMENT)

      STOP 0

C  ERROR EXIT

   13 WRITE(*,*) RIN
      STOP 13

      END
C*********************************************************************
      SUBROUTINE DOCOPY (FIN,FOUT,NSYMB,SYMB,SYMR,DOSUB,NUPPER,NCALLUP,
     & NOCOMMENT)
C*********************************************************************

C  ROUTINE COPIES A FILE WITH OR WITHOUT SYMBOL SUBSTITUTION

C  FIN    = INPUT FILE (INPUT, CHARACTER*60)

C  FOUT   = OUTPUT FILE (INPUT, CHARACTER*60)

C  NSYMB  = NUMBER OF SUBSTITUTION SYMBOLS (INPUT, INTEGER)

C  SYMB(20,*)= SUBSTITUTION SYMBOLS (INPUT, CHARACTER*1)

C  SYMR(20,*)= REPLACEMENT SYMBOLS (INPUT, CHARACTER*1)

C  DOSUB  = SUBTITUTION TRIGGER (INPUT, LOGICAL)

C  NUPPER = SYMBOL NUMBER FOR UPPERCASE (0 IF NO) (INPUT, INTEGER)

C  NCALLUP = SYMBOL NUMBER FOR UPPERCASE (0 IF NO) (INPUT, INTEGER)

C  NOCOMMENT = REMOVE // COMMENTS TRIGGET (INPUT, LOGICAL)

C*********************************************************************
CMP Changed for the T3E - Manish 04/97
C      LOGICAL     DOSUB,FEX,NQOT1,NQOT2
      LOGICAL     DOSUB,NQOT1,NQOT2,NOCOMMENT
      LOGICAL       FEX

      CHARACTER*1   DOL,BLK,QOT1,QOT2,CIN1(100),COUT1(80),SYMB(20,*),
     &              SYMR(20,*)
      CHARACTER*60  FIN,FOUT
      CHARACTER*100 CIN
      CHARACTER*80  COUT

      EQUIVALENCE (CIN,CIN1(1)),(COUT,COUT1(1))

      DATA DOL/'$'/,BLK/' '/

      QOT1=CHAR(34)
      QOT2=CHAR(39)

C  OPEN SOURCE AND TARGET FILES

      CLOSE (2)
      OPEN (2,FILE=FIN,STATUS='OLD')
      CLOSE (3)
      INQUIRE(FILE=FOUT,EXIST=FEX)
      IF (FEX) THEN
         OPEN (3,FILE=FOUT,STATUS='OLD')
         CLOSE (3,STATUS='DELETE')
      ENDIF
      OPEN (3,FILE=FOUT,STATUS='NEW')

C  COPY FILE LOOP

    1 READ (2,2,END=20) CIN
    2 FORMAT(A100)

      J=0
      DO 3 I=1,100
      IF (NOCOMMENT.AND.CIN1(I).EQ.'/'.AND.I.LT.100.AND.
     &   CIN1(I+1).EQ.'/') THEN
         DO 16 K=I,100
   16    CIN1(K)=' '
      ENDIF
      IF (CIN1(I).EQ.DOL.AND.DOSUB) GO TO 5
      IF (CIN1(I).NE.BLK) J=I
    3 CONTINUE

C  NO SUBSTITUTION

      IF (J.GT.80) GO TO 13
      WRITE (3,4) (CIN1(I),I=1,J)
    4 FORMAT(80A1)
      GO TO 1

C  MAKE SUBSTITUTION IF NOT IN QUOTES

    5 NQOT1=.TRUE.
      NQOT2=.TRUE.
      KS=0
      KT=0
      LT=0
      DO 14 I=1,100
      IF (CIN1(I).NE.BLK) KST=I
   14 CONTINUE
    6 KS=KS+1
      IF (KS.GT.KST) GO TO 12
      IF (CIN1(KS).EQ.QOT1) NQOT1=.NOT. NQOT1
      IF (CIN1(KS).EQ.QOT2) NQOT2=.NOT. NQOT2
      IF (CIN1(KS).EQ.DOL.AND.NQOT1.AND.NQOT2) THEN
         LS=0
         DO 8 I=1,NSYMB
         DO 7 J=1,20
         IF (SYMB(J,I).EQ.BLK) THEN
            IF (J.GE.LS) THEN
               LS=J-1
               IB=I
            ENDIF
            GO TO 8
         ENDIF
         IF (CIN1(KS+J).NE.SYMB(J,I)) GO TO 8
    7    CONTINUE
    8    CONTINUE
         IF (LS.EQ.0) THEN
            WRITE (*,*) 'ERROR - UNDEFINED SYMBOL ENCOUNTERED'
            WRITE (*,*) CIN
            STOP 13
         ENDIF
         DO 10 J=20,1,-1
         NS2=J
         IF (SYMR(J,IB).NE.BLK) GO TO 15
   10    CONTINUE
   15    IF (SYMR(NS2,IB).EQ.QOT1) THEN
            NS1=2
            NS2=NS2-1
         ELSE
            NS1=1
         ENDIF
         DO 11 J=NS1,NS2
         KT=KT+1
         IF (KT.GT.80) GO TO 13
   11    COUT1(KT)=SYMR(J,IB)
         KS=KS+LS
         LT=KT
         IF (IB.EQ.NUPPER) CALL UCASE(CIN1,KS+1,KST)
         IF (IB.EQ.NCALLUP) CALL UCASE(CIN1,KS+1,KST)
      ELSE
         KT=KT+1
         IF (KT.GT.80) GO TO 13
         COUT1(KT)=CIN1(KS)
         IF (COUT1(KT).NE.BLK) LT=KT
      ENDIF
      GO TO 6
   12 WRITE (3,4) (COUT1(I),I=1,LT)
      GO TO 1

C  EXITS

   20 RETURN

   13 WRITE (*,*) 'ERROR - OUTPUT RECORD LONGER THAN 80 CHARACTERS'
      WRITE (*,*) CIN
      STOP 13
      END

C*********************************************************************
      SUBROUTINE UCASE (S,K1,K2)
C*********************************************************************

C  CONVERTS ANY LOWERCASE CHARACTERS IN A SEQUENCE OF CHARACTERS TO
C  UPPERCASE.  CONVERSION IS LIMITED TO ONE ALPHA-NUMERIC WORD.

C  S(*) = ARRAY OF CHARACTERS TO BE CONVERTED TO UPPERCASE
C         (INPUT AND OUTPUT, CHARACTER*1)

C  K1 = INDEX OF FIRST CHARACTER TO BE CONVERTED (INPUT, INTEGER)

C  K2 = MAX INDEX THAT MAY BE CONVERTED (INPUT, INTEGER)

C*********************************************************************

      CHARACTER*1 S(*),CAPA,LOWA,CAPZ,LOWZ,ZERO,NINE,T

      DATA CAPA/'A'/,LOWA/'a'/,CAPZ/'Z'/,LOWZ/'z'/,ZERO/'0'/,NINE/'9'/

      I=ICHAR(CAPA)-ICHAR(LOWA)
      DO 1 K=K1,K2
      T=S(K)
      IF (T.GE.LOWA.AND.T.LE.LOWZ) THEN
         S(K)=CHAR(ICHAR(T)+I)
      ELSE
         IF((T.LT.CAPA.OR.T.GT.CAPZ).AND.(T.LT.ZERO.OR.T.GT.NINE))
     &      RETURN
      ENDIF
    1 CONTINUE
      END

C*********************************************************************
      SUBROUTINE EVAL (RIN,N1,N2,NSYM,SYMB,ISNUM,NUMS,NUMRET,KOD)
C*********************************************************************

C  ROUTINE EVALUATES A SIMPLE EXPRESSION

C  RIN(*)  = INPUT STRING (INPUT, CHARACTER*1)

C  N1      = 1ST CHARACTER IN RIN TO BE TESTED (INPUT, INTEGER)

C  N2      = LAST CHARACTER IN RIN TO BE TESTED (INPUT, INTEGER)

C  NSYM    = NUMBER OF SYMBOLS DEFINED (INPUT, INTEGER)

C  SYMB(*) = SYMBOLS DEFINED (INPUT, CHARACTER*20)

C  ISNUM() = TRUE IF SYMBOL IS A NUMBER (INPUT, LOGICAL)

C  NUMS(*) = VALUES OF SYMBOLS DEFINED (INPUT, INTEGER)

C  NUMRET  = RESULT (OUTPUT, INTEGER)

C  KOD     = RETURN CODE (OUTPUT, INTEGER)
C          = 0 ==> NUMBER RETURNED
C          > 0 ==> ERROR

C*********************************************************************

      INTEGER     NUMS(*),TOKEN(25)
      LOGICAL       ISNUM(*)
      CHARACTER*1   TSYM1(20),NUM(10)
      CHARACTER*20  TSYM,SYMB(*)
      CHARACTER*1   RIN(*)

      EQUIVALENCE (TSYM,TSYM1(1))
      DATA NUM/'0','1','2','3','4','5','6','7','8','9'/

C  CONVERT TO TOKENS
C  1 TO 6 ==> OPERATOR
C  > 10 ==> NUMBER

      NS=NSYM
      N=0
      NEX=N1
    1 M=NEX
      CALL PARSE(RIN,M,N2,NEX,KOD)

      IF (KOD.EQ.0) GO TO 5

      IF (KOD.EQ.1) THEN
         L=NEX-M
         IF (L.GT.20) THEN
            WRITE (*,*) 'ERROR - SYMBOL TOO LONG'
            GO TO 13
         ENDIF
         TSYM=' '
         DO 2 I=1,L
    2    TSYM1(I)=RIN(M+I-1)
         DO 3 I=1,NSYM
         IF (TSYM.EQ.SYMB(I)) THEN
            IF (.NOT.ISNUM(I)) THEN
               WRITE (*,*) 'ERROR - NONNUMERIC SYMBOL IN EXPRESSION'
               GO TO 13
            ENDIF
            N=N+1
            TOKEN(N)=I+10
            GO TO 1
         ENDIF
    3    CONTINUE
         WRITE (*,*) 'ERROR - UNDEFINED SYMBOL IN EXPRESSION'
         GO TO 13
      ENDIF

      IF (KOD.EQ.2) THEN
         NN=0
         L=NEX-1
         NM=1
         DO 4 I=L,M,-1
         DO 22 J=1,10
         IF (RIN(I).EQ.NUM(J)) THEN
            NN=NN+NM*(J-1)
            GO TO 4
         ENDIF
   22    CONTINUE
         WRITE (*,*) 'ERROR - ILLEGAL NUMBER'
         GO TO 13
    4    NM=10*NM
         NS=NS+1
         NUMS(NS)=NN
         N=N+1
         TOKEN(N)=NS+10
         GO TO 1
      ENDIF

      IF (KOD.GT.11) THEN
         N=N+1
         TOKEN(N)=KOD-11
         GO TO 1
      ENDIF
      GO TO 69

    5 IF (N.EQ.0) THEN
         WRITE (*,*) 'ERROR - EXPECTED EXPRESSION NOT FOUND'
         GO TO 13
      ENDIF

C  START EVALUATION LOOP - LOOK FOR INTER ()

      JEND=1
    6 IF (JEND.EQ.0) GO TO 20
      JEND=0
      DO 7 I=1,N
      IF (TOKEN(I).EQ.5) THEN
         L1=I+1
         JEND=1
      ENDIF
    7 CONTINUE
      IF (JEND.EQ.0) THEN
         L1=1
         L2=N
      ELSE
         DO 8 I=L1,N
         IF (TOKEN(I).EQ.6) THEN
            L2=I-1
            TOKEN(I)=0
            TOKEN(L1-1)=0
            GO TO 9
         ENDIF
    8    CONTINUE
         WRITE (*,*) 'ERROR - UNMATCHED PARENTHESES'
         GO TO 13
      ENDIF

C  EVALUATE INNER EXPRESSION

    9 LL1=L1+1
      LL2=L2-1
      IF (TOKEN(L1).LT.10) LL1=L1
      DO 10 J=1,4
      DO 10 I=LL1,LL2
      IF (TOKEN(I).EQ.J) THEN

         DO 12 K=I+1,L2
         IF (TOKEN(K).GT.10) THEN
            IR=K
            GO TO 14
         ENDIF
         IF (TOKEN(K).GT.0) GO TO 69
   12    CONTINUE
         GO TO 69
   14    DO 15 K=I-1,L1,-1
         IF (TOKEN(K).GT.10) THEN
            IL=K
            GO TO 16
         ENDIF
         IF (TOKEN(K).GT.0) GO TO 69
   15    CONTINUE

         IF (J.LT.3) GO TO 69
         NN=NUMS(TOKEN(IR)-10)
         IF (J.EQ.3) NN=-NN
         GO TO 17

   16    IF (J.EQ.1) NN=NUMS(TOKEN(IL)-10)/NUMS(TOKEN(IR)-10)
         IF (J.EQ.2) NN=NUMS(TOKEN(IL)-10)*NUMS(TOKEN(IR)-10)
         IF (J.EQ.3) NN=NUMS(TOKEN(IL)-10)-NUMS(TOKEN(IR)-10)
         IF (J.EQ.4) NN=NUMS(TOKEN(IL)-10)+NUMS(TOKEN(IR)-10)

         TOKEN(IL)=0
   17    TOKEN(I)=0
         NS=NS+1
         NUMS(NS)=NN
         TOKEN(IR)=NS+10
      ENDIF
   10 CONTINUE
      GO TO 6

C  EXIT

   20 DO 21 I=1,N
      IF (TOKEN(I).GT.10) THEN
         KOD=0
         NUMRET=NUMS(TOKEN(I)-10)
         RETURN
      ENDIF
   21 CONTINUE
   69 WRITE (*,*) 'ERROR - ILLEGAL SYNTAX'
   13 KOD=1
      END
C*********************************************************************
      SUBROUTINE PARSE (RIN,N1,N2,NEX,KOD)
C*********************************************************************

C  ROUTINE PARSES NEXT WORD FROM A RECORD AND CLASSIFIES IT

C  RIN(*) = INPUT STRING (INPUT, CHARACTER*1)

C  N1  = 1ST CHARACTER IN RIN TO BE TESTED (INPUT & OUTPUT, INTEGER)
C        N1 IS RESET TO SKIP ANY LEADING BLANKS

C  N2  = LAST CHARACTER IN RIN TO BE TESTED (INPUT, INTEGER)

C  NEX = 1ST CHARACTER AFTER WORD LOCATED (OUTPUT, INTEGER)
C        MAY BE THE SAME SYMBOL AS N1

C  KOD = RETURN CODE (OUTPUT, INTEGER)
C      = 0 ==> NOTHING OR COMMENT
C      = 1 ==> SYMBOL (NOT A CONTROL WORD)
C      = 2 ==> NUMBER
C      = 3 ==> Set_Symbols
C      = 4 ==> Replace_Symbols
C      = 5 ==> Target_Directory
C      = 6 ==> Source_Directory
C      = 7 ==> Source_Files
C      = 8 ==> Include_File
C      = 9 ==>               (spare index)
C      =10 ==>               (spare index)
C      =11 ==>               (spare index)
C      =12 ==> /
C      =13 ==> *
C      =14 ==> -
C      =15 ==> +
C      =16 ==> (
C      =17 ==> )
C      =18 ==> =
C      =19 ==> "

C*********************************************************************

      CHARACTER*1  A,B,ZERO,NINE,OPP(8),RIN(*),CONWD1(20,8)
      CHARACTER*20 CONWD(8)
      INTEGER LENCON(8)

      EQUIVALENCE (CONWD,CONWD1(1,1))

      DATA ZERO/'0'/,NINE/'9'/,OPP/'/','*','-','+','(',')','=','X'/,
     & CONWD/'Set_Symbols         ','Replace_Symbols     ',
     &       'Target_Directory    ','Source_Directory    ',
     &       'Source_Files        ','Include_File        ',
     &       'Include_File        ','Include_File        '/,
     & LENCON/11,15,16,16,12,12,12,12/

      OPP(8)=CHAR(34)

C  SKIP ANY LEADING BLANKS

      M=N1
      DO 1 I=M,N2
      N1=I
C     IF (RIN(I).NE.' ') GO TO 2
      IF (RIN(I).GT.' ') GO TO 2
    1 CONTINUE
    8 KOD=0
      NEX=N2+1
      RETURN

C  TEST FOR COMMENT OR OPERATOR

    2 A=RIN(N1)
      IF (A.EQ.'$') GO TO 8
      DO 3 I=1,8
      IF (A.EQ.OPP(I)) THEN
         KOD=I+11
         NEX=N1+1
         RETURN
      ENDIF
    3 CONTINUE

C  FIND END OF SYMBOL OR NUMBER

      DO 4 I=N1,N2
      NX=I
      B=RIN(I)
      IF (B.EQ.' ') GO TO 5
      DO 4 J=1,6
      IF (B.EQ.OPP(J)) GO TO 5
    4 CONTINUE
      NX=N2+1

C  TEST FOR NUMBER

    5 IF (A.GE.ZERO.AND.A.LE.NINE) THEN
         KOD=2
         NEX=NX
         RETURN
      ENDIF

C  TEST FOR CONTROL WORD

      L=NX-N1
      DO 7 I=1,8
      IF (L.EQ.LENCON(I)) THEN
         DO 6 J=1,L
         IF (RIN(N1+J-1).NE.CONWD1(J,I)) GO TO 7
    6    CONTINUE
         KOD=I+2
         NEX=NX
         RETURN
      ENDIF
    7 CONTINUE

C  MUST BE A SYMBOL

      KOD=1
      NEX=NX
      END
C*********************************************************************
      SUBROUTINE GETNAM (A,LA,I1,B,LB,PCSLASH)
C*********************************************************************

C  ROUTINE PARSES NEXT WORD FROM A CHARACTER STRING

C  A = INPUT STRING (INPUT, CHARACTER*1) (80 CHARACTERS)

C  LA = MAX LENGTH OF THE INPUT STRING (INPUT, INTEGER)

C  I1 = FIRST CHARACTER IN A TO BE TESTED ON INPUT
C       CHARACTER NUMBER AFTER WORD ON OUTPUT (OR 0 IF NO WORD)

C  B = OUTPUT STRING (OUTPUT, CHARACTER*1)

C  LB = MAX LENGTH OF THE OUTPUT STRING (INPUT, INTEGER)

C  PCSLASH = .FALSE. ==> NO CONVERSION
C          = .TRUE.  ==> CONVERT SLASH TO IBMPC FORMAT

C*********************************************************************
      CHARACTER*1 A(*),B(*),C
      LOGICAL PCSLASH

      DO 1 I=1,LB
    1 B(I)=' '

      IF (I1.EQ.0) RETURN

      DO 2 I=I1,LA
      J=I
      IF (A(I).NE.' ') GO TO 3
    2 CONTINUE
      I1=0
      RETURN

    3 IB=0
      I1=0
      DO 4 I=J,LA
      I1=I
      C=A(I)
      IF (C.LE.' '.OR.IB.EQ.LB.OR.C.EQ.'$'.OR.C.EQ.'=') GO TO 5
      IB=IB+1
      B(IB)=C
      IF (PCSLASH.AND.C.EQ.'/') B(IB)=CHAR(92)
    4 CONTINUE

    5 IF (IB.EQ.0) I1=0

      END
