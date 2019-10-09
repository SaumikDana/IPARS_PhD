C  IPARS.F - MAIN DRIVER FOR AN IMPLICIT PARALLEL ACCURATE RESERVOIR SIMULATOR

C  ROUTINES IN THIS MODULE:

C  PROGRAM    IPARS
C  SUBROUTINE OPENIO  (NERR)
C  SUBROUTINE IVPARM  (NERR)
C  SUBROUTINE NEXTIME (NERR)
C  SUBROUTINE SETSTEP ()
C  SUBROUTINE STEP    (NERR)

C  CODE HISTORY:

C  JOHN WHEELER     11/10/95    ORIGINAL ALPHA CODE
C  JOHN WHEELER      7/10/96    DEBUG PARALLEL CODE
C  B MOMKEN         12/01/98    ADDED VISUALIZTION & MULTI_BLOCK TIMERS
C  JOHN WHEELER      4/20/99    GENERIC MULTI-MODEL CAPABILITY
C  RICK DEAN        11/29/01    ADDED STEPMULT AND MODIFIED TIMESTEP CUT
C  RICK DEAN        12/16/02    ADDED DTSTAB FOR CFL STABILITY LIMITS
C  BEN GANIS/RUIJIE LIU
C                   2014-2015   ADDED ELASTO_PLASTICITY MODULE
C  GURPREET SINGH               ADDED MFMFE MODULE
C                   2012-2014     COMPOSITIONAL IMPLICIT MFMFE
C                   01/15-04/15   SINGLE PHASE MFMFE
C                   09/15-11/15   TWO PHASE IMPLICIT MFMFE (APPROX)
C                   10/16         TWO PHASE IMPLICIT BRICKS (APPROX)
C  NOTES:

C     1) ERROR NUMBERS 431 TO 460 ARE RESERVED FOR DRIVER ROUTINES

C*********************************************************************
      SUBROUTINE IPARS()
C*********************************************************************

C  MAIN PROGRAM DRIVER

C  NO ARGUMENTS

C  NOTES:

C     1.  Keep this routine as simple as possible.  Most machine and
C         model specific code should reside in subroutines.

C*********************************************************************
      USE scrat1mod
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'

      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'wells.h'
      INTEGER MFILE(6)
      INTEGER NERR
      REAL*8 UU,VV
      CHARACTER*20 BLKEND
      CHARACTER*9 NEXTCASE
      EXTERNAL SIGNAL_HANDLER
ctm   TAMEEM
      REAL*8 TOTAL_TIME
ctm   TAMEEM

      DATA MFILE/-1,-2,3,4,5,6/

C gus: allocate big character array
      ALLOCATE(A(100000000),STAT=NERR)
      IF (NERR.NE.0) CALL KILL_IPARS('Could not allocate char array A')

C  PLATFORM SPECIFIC INITIALIZATION

      DEALII = .FALSE.
      DATASPACES = .FALSE.
      NERR=0
      NUMPRC=1
      MYPRC=0
      MYPID=0
       CALL SETPRCS (NERR)
       IF (NERR.GT.0) CALL KILL_IPARS('Errors in SETPRCS')

C  GET FILE NAMES, OPEN FILES

      NFIN=1
      NFOUT=2
      NFINC=7
      NFRIN=8
      NFROUT=9
      NFUTIL=10
      NFWELL=11
      NWFCUM=21
      NFRESP=13
      BATCH=.FALSE.
      IF (MYPRC.EQ.0) THEN
         NFBUG=NFOUT
         BUGOPEN=.TRUE.
      ELSE
         NFBUG=12
         BUGOPEN=.FALSE.
      ENDIF

C  LOOP POINT FOR MULTIPLE JOBS

    2 RTIMIN=-1.D0
      IF (MYPRC.EQ.0) THEN
         CALL OPENIO(NERR)
         IF (NERR.GT.0) CALL KILL_IPARS('Errors in OPENIO')
      ENDIF

C  INITIALIZE MESSAGE TAGS

      CALL SETTAGS()

C  INITIALIZE UTILITIES AND DISTRIBUTE RESTART TIME

      LEVERR=0

      CALL IUTIL(MFILE)
      CALL TIMSET()
      CALL TIMON(1)
      CALL TIMON(7)

C bag8 - jump to signal handler if user sends kill signal
      CALL SIGNAL(2,SIGNAL_HANDLER,-1)    ! SIGINT
      CALL SIGNAL(15,SIGNAL_HANDLER,-1)   ! SIGTERM

      BUGKEY(1)=.FALSE.
      IF (NUMPRC.GT.1) CALL SPREAD8(1,RTIMIN)

C  READ AND DISTRIBUTE INITIAL DATA

      CALL SETNBG()

      IF (MYPRC.EQ.0) THEN
         BLKEND='EndInitial'

         WRITE(*,'(A,$)')'Calling READER...'
         CALL READER(BLKEND,NFIN,NFINC,NERR)
         IF (NERR.EQ.0) THEN
            WRITE(*,*)'Done!'
         ELSE
            WRITE(*,*)'Error!'
            GOTO 101
         ENDIF
      ENDIF
      IF (NUMPRC.GT.1) CALL SNDABUF(.TRUE.)

C  PROCESS INITIAL DATA

      CALL GETIDAT(NERR)
      IF (NERR.GT.0) CALL KILL_IPARS('Errors in GETIDAT')
c      IF (NERR.GT.0) GO TO 101

C  EXCHANGE INITIAL INTERFACE DATA WITH NEIGHBORING NODES

      CALL WAITALL()
      IF (NUMPRC.GT.1) CALL COMMI(NERR)
      IF (NERR.GT.0) CALL KILL_IPARS('Errors in COMMI')

      IF (VMDEBUG) CALL PRINT_VM_INFO('Before IVPARM')

C  INITIALIZE VARIOUS PARAMETERS

      CALL IVPARM(NERR)
      IF (NERR.GT.0) CALL KILL_IPARS('Errors in IVPARM')
c      IF (NERR.GT.0) GO TO 101
      IF (RTIMIN.GT.0.D0) TIM=RTIMIN
      CALL WAITALL()


! bag8 - moved from inside GETIDAT->SBLKIN->DUALINIT
!        to after COMMI and KEYOUT update
C      CALL DUALINIT(NERR)
C      CALL TIMON(12)
CC      MODACT=3
CC      IF (MODELON(3).OR.FLOWMODEL.EQ.MODACT)
CC     &             CALL XBLKIN(NERR)
CC      MODACT=16
CC      IF (MODELON(16).OR.FLOWMODEL.EQ.MODACT)
CC     &             CALL XBLKIN(NERR)
C      MODACT=0
C      CALL TIMOFF(12)
C      IF (NERR.GT.0) CALL KILL_IPARS('Error in DUALINIT')
C      CALL HYPRE_EVFEM_INIT()

C  READ 1ST TRANSIENT DATA TIME

      CALL NEXTIME(NERR)
      IF (NERR.GT.0) GO TO 101
      CALL TIMOFF(7)

ctm   TAMEEM
      IF(Q_MULTIRATE.EQ.1) THEN
           MULTIRATE_FLAG = 1        ! MULTIRATE OFF
      ELSE
           MULTIRATE_FLAG = 0        ! MULTIRATE ON
      ENDIF
      TOTAL_NUM_LIN_ITER_F = 0
      TOTAL_NUM_LIN_ITER_M = 0
      MC_SKIPPED = 1
! SAUMIK - ALWAYS TRUE FOR NON-MULTIRATE RUNS
      COUP_ITRN = 0
ctm   TAMEEM


C  READ, DISTRIBUTE, AND PROCESS RESTART DATA

    1 IF (RTIMIN.GT.0.D0.AND.(ACTTIM(2)-RTIMIN).GT.-DTIMTOL) THEN
         CALL RESIN(NERR)
         IF (NERR.GT.0) GO TO 101
         RTIMIN=-1.D0
      ENDIF

C  READ, DISTRIBUTE, AND PROCESS TRANSIENT DATA

      IF (TIM-ACTTIM(2).GT.-DTIMTOL) THEN
         IF (MYPRC.EQ.0) THEN
            BLKEND='EndTime'
            CALL READER(BLKEND,NFIN,NFINC,NERR)
            IF (NERR.GT.0) GO TO 101
         ENDIF
      IF (NUMPRC.GT.1) CALL SNDABUF(LEVELE)
         CALL GETTDAT(NERR)
         IF (NERR.GT.0) CALL KILL_IPARS('Errors in GETTDAT')
      CALL WAITALL()
         IF (NERR.GT.0) GO TO 101
         CALL NEXTIME(NERR)
         IF (NERR.GT.0) GO TO 101
         GO TO 1
      ENDIF

C  SELECT NEXT TIME STEP SIZE

      CALL SETSTEP ()

C  MAKE ONE TIME STEP

      IF (KHISOUT.GT.0) THEN
         TIMHIS(NHISUSE)=TIM
         IF((NHISUSE == 0).AND.(NSTEP < 1)) THEN
            NHISQ=0
C            MODACT=14
C            IF (MODELON(14)) THEN
C               CALL TRSTEP(NERR)
C               IF (NERR.GT.0) STOP 'Errors in TRSTEP'
C               MODACT=0
C               GOTO 5
C            ENDIF
            CALL STEP(NERR)
            IF (NERR.GT.0) CALL KILL_IPARS('Errors in STEP')
C    5       CONTINUE
         ENDIF
      ENDIF
      NSTEP=NSTEP+1
      NHISUSE=NHISUSE+1
      NHISQ=0
C      MODACT=14
C      IF (MODELON(14)) THEN
C         CALL TRSTEP(NERR)
C         IF (NERR.GT.0) STOP 'Errors in TRSTEP'
C         MODACT=0
C         GOTO 6
C      ENDIF
      CALL STEP(NERR)
      IF (NERR.GT.0) CALL KILL_IPARS('Errors in STEP')
C    6 CONTINUE

C  OUTPUT STANDARD HARDCOPY

      IF (NERR.GT.0) GO TO 101
      TIM=TIM+DELTIM
ctm   TAMEEM
      IF(MODELON(15).AND.(Q_MULTIRATE.NE.1)) THEN
         CALL TIMGET(1,TOTAL_TIME)
         WRITE(*,*) 'TIM = ',TIM,',TOTAL TIME = ', TOTAL_TIME
         WRITE(*,*) 'TIME ', TIM
         WRITE(*,*) 'ELAPSED ', TOTAL_TIME
         WRITE(*,*) 'LINEAR_F ', TOTAL_NUM_LIN_ITER_F
         WRITE(*,*) 'LINEAR_M ', TOTAL_NUM_LIN_ITER_M
         COUP_ITRN = 0
      ENDIF
ctm   TAMEEM

C  OUTPUT WELL DATA

      IF (KHISOUT.GT.0) THEN
         TIMHIS(NHISUSE)=TIM
         IF (NHISUSE.EQ.18) CALL WELDUMP()
      ENDIF

      KSTDO=.TRUE.
      IF (TIM-ACTTIM(3).GT.-DTIMTOL) THEN
         KSTDO=.FALSE.
         CALL STDOUT ()
         ACTTIM(3)=ACTTIM(3)+DTIMOUT
      ENDIF

C  OUTPUT RESTART DATA

      IF (TIM-ACTTIM(4).GT.-DTIMTOL) THEN
         CALL TIMON(11)
         CALL RESOUT (NERR)
         ACTTIM(4)=ACTTIM(4)+DTIMRES
         CALL TIMOFF(11)
      ENDIF

C  OUTPUT VISUALIZATION DATA

      IF(VISALL) THEN
         CALL TIMON(13)
         CALL VISOUT (NERR)
         IF (NERR.GT.0) CALL KILL_IPARS('Errors in VISOUT')
         CALL TIMOFF(13)
                GOTO 1001
      ELSE
         IF (TIM-ACTTIM(6).GT.-DTIMTOL) THEN
         CALL TIMON(13)
         CALL VISOUT (NERR)
         IF (NERR.GT.0) CALL KILL_IPARS('Errors in VISOUT')
         CALL TIMOFF(13)
             ACTTIM(6)=ACTTIM(6)+DVISOUT
         ENDIF
      ENDIF
1001  CONTINUE

C  TERMINATE OR START NEXT CASE

 101  IF (NERR .EQ. 0) THEN
         IF (TIM-ACTTIM(1).LE.-DTIMTOL) GO TO 1
         IF (KHISOUT.GT.0.AND.NHISUSE.GT.0) CALL WELDUMP()
         IF (KSTDO) CALL STDOUT ()
         CALL TIMOUT ()

         UU=ITNEWTR
         VV=UU/NSTEPR
c         UU=ITLINR/UU
         IF (MYPRC.EQ.0) WRITE (NFOUT,117) ITLINR,ITNEWTR,NSTEPR,UU,VV
  117    FORMAT(/' TOTAL LINEAR ITERATIONS THIS RUN =',T40,I7/
     &           ' TOTAL NEWTONIAN ITERATIONS THIS RUN =',T40,I7/
     &           ' TOTAL TIME STEP THIS RUN =',T40,I7/
     &           ' LINEAR / NEWTONIAN =',T41,G10.3/
     &           ' NEWTONIAN / TIME STEP =',T41,G10.3)
      ENDIF

      MODACT=15
      IF (MODELON(15)) THEN
         CALL EQUIT(NERR)
         GOTO 7
      ENDIF
C      MODACT=14
C      IF (MODELON(14)) THEN
C         CALL TRQUIT(NERR)
C         GOTO 7
C      ENDIF
C      MODACT=7
C      IF (MODELON(7)) THEN
C          CALL MQUIT(NERR)
C         GOTO 7
C      ENDIF
C      MODACT=2
C      IF (MODELON(2)) CALL IQUIT(NERR)
C      MODACT=3
C      IF (MODELON(3)) CALL XQUIT(NERR)
! saumik
C      MODACT=16
C      IF (MODELON(16).OR.MBPOROE) CALL XQUIT(NERR)
C      MODACT=5
C      IF (MODELON(5)) CALL HQUIT(NERR)
C      MODACT=19
C      IF (MODELON(19)) CALL HQUIT(NERR)
C      MODACT=18
C      IF (MODELON(18)) CALL HQUIT(NERR)
C      MODACT=13
C      IF (MODELON(13)) CALL TQUIT(NERR)
! saumik
      MODACT=17
      IF (MODELON(17).OR.MBPOROE) CALL TQUIT(NERR)
    7 MODACT=0

      IF (BATCH) THEN
         READ (NFRESP,3,ERR=4) NEXTCASE
    3    FORMAT(A9)
         IF (NEXTCASE.EQ.'NEXT CASE'.OR.NEXTCASE.EQ.'next case') THEN
            CALL FREEALL()
            CALL CLEAREV()
C            IF (ADAPTIVITY) CALL FREEADAPT()
            GO TO 2
         ENDIF
      ENDIF

    4 CONTINUE
      CALL KILLPRC(NERR)

      IF (NERR.EQ.0) STOP 0

! bag8 - important message for IPARS "users"
      IF (MYPRC.EQ.0) WRITE(*,*)'PLEASE SEE OUTPUT FILE FOR ERRORS'
      STOP 13

      END

C*********************************************************************
      SUBROUTINE SIGNAL_HANDLER()
C*********************************************************************
C bag8 - This routine will get called if kill signal is sent to ipars
C        process.
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'control.h'
      INTEGER NERR
      NERR = 0
      IF (MYPRC.EQ.0) WRITE(*,*)'In SIGNAL_HANDLER'
      CALL KILLPRC(9)
      END

C*********************************************************************
      SUBROUTINE OPENIO (NERR)
C*********************************************************************

C  GET MAIN I/O FILE NAMES FROM USER OR RESPONSE FILE
C  OPEN THE FILES

C  NERR = ERROR NUMBER STEPED BY 1 ON ERROR (INPUT & OUTPUT, INTEGER)

C  IF THE RESPONSE FILE IPARS.IN EXISTS IN THE CURRENT DIRECTORY,
C  FILE NAMES WILL BE READ FROM THAT FILE RATHER THAN THE TERMINAL.
C  THIS OPTION PROVIDES A BATCH MODE FOR THE SIMULATOR.

C*********************************************************************

      INCLUDE 'control.h'
      INCLUDE 'output.h'
      INCLUDE 'restc.h'
      CHARACTER*60 FILNAM,BLKNAM

      LOGICAL OUTOP

C  OPEN AND USE RESPONSE FILE IF IT EXISTS

      OUTOP=.FALSE.
      IF (.NOT.BATCH) OPEN (NFRESP,FILE='IPARS.IN',STATUS='OLD',ERR=6)
      BATCH=.TRUE.

C  OPEN KEYWORD INPUT FILE

    6 IF (BATCH) THEN
         READ (NFRESP,2) FILNAM
      ELSE
         WRITE (*,1)
    1    FORMAT(' ENTER INPUT FILE NAME: ')
         READ (*,2) FILNAM
    2    FORMAT(A60)
      ENDIF
      OPEN (NFIN,FILE=FILNAM,STATUS='OLD',ERR=13)

C  OPEN STANDARD OUTPUT FILE

      IF (BATCH) THEN
         READ (NFRESP,2) FILNAM
      ELSE
         WRITE (*,3)
    3    FORMAT(' ENTER OUTPUT FILE NAME: ')
         READ (*,2) FILNAM
      ENDIF
      CLOSE (NFOUT)
      OPEN (NFOUT,FILE=FILNAM,STATUS='UNKNOWN',ERR=13)
      OUTOP=.TRUE.

C  OPEN RESTART INPUT FILE, GET RESTART TIME

      IF (BATCH) THEN
         READ (NFRESP,2) FILNAM
      ELSE
         WRITE (*,4)
    4    FORMAT(' ENTER INPUT RESTART FILE NAME (RETURN IF NONE): ')
         READ (*,2) FILNAM
      ENDIF
      BLKNAM=' '
      IF (FILNAM.EQ.BLKNAM) THEN
         RTIMIN=-1.D0
      ELSE
         OPEN (NFRIN,FILE=FILNAM,STATUS='OLD',ERR=8)
         READ (NFRIN,7,ERR=8) RTIMIN
    7    FORMAT(G23.16)
         FORMIN=.TRUE.
         GO TO 9

    8    CLOSE (NFRIN)
         OPEN (NFRIN,FILE=FILNAM,STATUS='OLD',FORM='UNFORMATTED',
     &   ERR=13)
         READ (NFRIN) RTIMIN
         FORMIN=.FALSE.
      ENDIF

C  GET RESTART OUTPUT FILE PRIMARY NAME

    9 IF (BATCH) THEN
         READ (NFRESP,2) RSFOUT
      ELSE
         WRITE (*,5)
    5    FORMAT(' ENTER OUTPUT RESTART FILE PRIMARY NAME ',
     &      '(RETURN IF NONE): ')
         READ (*,2) RSFOUT
      ENDIF

      RETURN

   13 NERR=NERR+1
      IF (BATCH.AND.OUTOP) THEN
         WRITE (NFOUT,14) FILNAM
      ELSE
         WRITE (*,14) FILNAM
      ENDIF
   14 FORMAT (/' ERROR # 431; OPEN FILE FAILED FOR '/A60)

      END
C*********************************************************************
      SUBROUTINE IVPARM (NERR)
C*********************************************************************

C  INITIALIZE VARIOUS PARAMETERS
C  OUTPUT DEBUG DATA

C  NERR = ERROR NUMBER STEPED BY 1 ON ERROR (INPUT & OUTPUT, INTEGER)

C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'

      INCLUDE 'control.h'
      INCLUDE 'blkary.h'
      INCLUDE 'layout.h'
      INCLUDE 'restc.h'

      INTEGER I,J,K,NERR,NARG(10),IBUG(1)
      REAL*8  DTSTEP
      EXTERNAL CPYARYR8,TRANC1,TRANC2,DEBUGW,FILL_N_MYPRC

C  SET SOME DEFAULTS

      NSTEP=0
      TIM=0.D0
      DELTIM=.1D0
      ACTTIM(4)=99.D15
      DTIMRES=99.D15
      DTIMOUT=99.D15
      DTIMMUL=1.3D0
      DTIMMAX=30.44D0
      DTIMMIN=1.D-3
      STEPMULT=10000.D0
      DTSTAB=99.D15
      DTSTEP=99.D15
      DTIMTOL=.1D0*DTIMMIN
      FORMOUT=.FALSE.
      VISALL =.FALSE.
      ITLINR=0
      ITNEWTR=0
      NSTEPR=0

C  INITIALIZE FRAMEWORK BALANCE DATA

      DO 1 K=1,19
      DO 1 J=1,13
      DO 1 I=1,8
    1 BALANCE(J,K,I)=0.D0

C  SET KEYOUT IN THE COMMUNICATIONS LAYERS

! update keyout
      CALL TIMON(22)

      CALL UPDATE(0,2)

           IF (KNDGRD.EQ.1) THEN
        CALL UPDATE(N_XPERM,1)
        CALL UPDATE(N_YPERM,1)
        CALL UPDATE(N_ZPERM,1)
           ELSE

! bag8 - update PERMXY,PERMYZ,PERMXZ and XC,YC,ZC for mpfa parallelization
      CALL UPDATE_MPFA_FRAMEWORK()
           ENDIF

      CALL TIMOFF(22)

c mpesz: output of permeabilities before they become transmissabilities,
c or well information, provided it was requested in the data file

       IF(.NOT.MBPOROE) THEN
      CALL TIMON(13)
      CALL PERMOUT()
      CALL TIMOFF(13)
       ENDIF

C COMPUTE ELEINDEX(IDIM,JDIM,KDIM) AND LSIZE FOR HYPRE SOLVER

! SAUMIK - ONLY FOR FLOW BLOCK(S)
       IF(MBPOROE) THEN
C      MODACT=16
      MODACT=17
       ENDIF

cgp: Extend to bricks
      CALL GET_GELEI_LSIZE(NERR)
      IF (NERR.GT.0) CALL KILL_IPARS('Errors in GET_GELEI_LSIZE')

! SAUMIK
       IF(MBPOROE) MODACT=0

C  CALCULATE TRANSMISSABILITY CONSTANTS

      IF (KNDGRD.EQ.1) THEN

         NARG(1)=6
         NARG(2)=N_TCOFX
         NARG(3)=N_TCOFY
         NARG(4)=N_TCOFZ
         NARG(5)=N_XPERM
         NARG(6)=N_YPERM
         NARG(7)=N_ZPERM
         CALL CALLWORK(TRANC1,NARG)

      ELSEIF (KNDGRD.EQ.3) THEN

         NARG(1)=9
         NARG(2)=N_TCOFX
         NARG(3)=N_TCOFY
         NARG(4)=N_TCOFZ
         NARG(5)=N_XPERM
         NARG(6)=N_YPERM
         NARG(7)=N_ZPERM
         NARG(8)=N_XC
         NARG(9)=N_YC
         NARG(10)=N_ZC
         CALL CALLWORK(TRANC2,NARG)

      ENDIF

C  COMPLETE INITIALIZATION OF MODEL SPECIFIC DATA

      MODACT=15
      IF (MODELON(15)) THEN
         CALL EIVDAT(NERR)
         IF (NERR.GT.0) STOP 'Errors in EIVDAT'
         IF (VMDEBUG) CALL PRINT_VM_INFO('After EIVDAT')
         GOTO 2
      ENDIF

C      MODACT=14
C      IF (MODELON(14)) THEN
C         CALL TRIVDAT(NERR)
C         IF (NERR.GT.0) STOP 'Errors in TRIVDAT'
C         GOTO 2
C      ENDIF

C      MODACT=7
C      IF (MODELON(7)) THEN
C         CALL MIVDAT(NERR)
C         IF (NERR.GT.0) STOP 'Errors in MIVDAT'
C         GOTO 2
C      ENDIF

C      MODACT=2
C      IF (MODELON(2)) CALL IIVDAT(NERR)
C      IF (NERR.GT.0) STOP 'Errors in IIVDAT'
C        MODACT=3
C      IF (MODELON(3)) CALL XIVDAT(NERR)
C      IF (NERR.GT.0) STOP 'Errors in XIVDAT'
C      MODACT=16
C      IF (MODELON(16)) CALL XIVDAT(NERR)
C      IF (NERR.GT.0) STOP 'Errors in XIVDAT'
C      MODACT=5
C      IF (MODELON(5)) CALL HIVDAT(NERR)
C      IF (NERR.GT.0) STOP 'Errors in HIVDAT'
C      MODACT=19
C      IF (MODELON(19)) CALL HIVDAT(NERR)
C      IF (NERR.GT.0) STOP 'Errors in HIVDAT'
C      MODACT=18
C      IF (MODELON(18)) CALL HIVDAT(NERR)
C      IF (NERR.GT.0) STOP 'Errors in HIVDAT'
C      MODACT=13
C      IF (MODELON(13)) CALL TIVDAT(NERR)
C      IF (NERR.GT.0) STOP 'Errors in TIVDAT'
      MODACT=17
      IF (MODELON(17)) CALL TIVDAT(NERR)
      IF (NERR.GT.0) STOP 'Errors in TIVDAT'
    2 MODACT=0

C  DEBUG OUTPUT - WORK ROUTINE ARGUMENTS

      IF (BUGKEY(2).AND.LEVELC) THEN
         IBUG(1)=0
         CALL CALLWORK (DEBUGW,IBUG)
      ENDIF

! bag8 - for visualizing processor assignment
      IF (PLOT_MYPRC) CALL CALLWORK(FILL_N_MYPRC,[1,N_MYPRC])

C OUTPUT INITIAL SOLUTION, IF REQUESTED

      CALL TIMON(13)
      CALL VISINITOUT ()
      IF (NERR.GT.0) CALL KILL_IPARS('Errors in VISINITOUT')
      CALL TIMOFF(13)

      END
C*********************************************************************
      SUBROUTINE DEBUGW (IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,KL2,
     & KEYOUT,NBLK)
C*********************************************************************

C  DEBUG OUTPUT - WORK ROUTINE ARGUMENTS FOR PROCESSOR 0

C*********************************************************************
      INCLUDE 'control.h'
      INTEGER JL1V(KDIM),JL2V(KDIM),    KEYOUT(IDIM,JDIM,KDIM)

      WRITE (NFOUT,1) MYPRC,NBLK,IDIM,JDIM,KDIM,IL1,IL2,JL1V(KL1),
     * JL2V(KL1),KL1,KL2
    1 FORMAT(/' DEBUG OUTPUT'/' PROC =',I3,', BLOCK =',I3,', IDIM =',
     & I4,', JDIM =',I4,', KDIM =',I4,', IL1 =',I4/' IL2 =',I4,
     & ', JL1(KL1) =',I4,', JL2(KL1) =',I4,', KL1 =',I4,', KL2 =',I4)

      WRITE (NFOUT,2) (J,J=1,JDIM)
    2 FORMAT(/' KEYOUT ARRAY FOR IL1'/(' K J',25I3))
      DO K=1,KDIM
      WRITE (NFOUT,3) K,(KEYOUT(IL1,J,K),J=1,JDIM)
      ENDDO
    3 FORMAT(I3,',',25I3/(4X,25I3))

      END
C*********************************************************************
      SUBROUTINE NEXTIME (NERR)
C*********************************************************************

C  READS AND DISTRIBUTES THE TIME AT WHICH THE NEXT SET OF TRANSIENT
C  DATA IS TO BE APPLIED

C  NERR = ERROR NUMBER STEPED BY 1 ON ERROR (INPUT & OUTPUT, INTEGER)

C*********************************************************************
      USE scrat1mod
      INCLUDE 'control.h'
!      INCLUDE 'scrat1.h'
      INCLUDE 'mpif.h'
      CHARACTER*1 BLK
      CHARACTER*9 BGTM,RECK
      CHARACTER*50 REC
      INTEGER I
      EQUIVALENCE (REC,RECK)
!      EQUIVALENCE (REC,RECK,A(1))
      DATA BGTM/'BeginTime'/,BLK/' '/

      IF (MYPRC.EQ.0) THEN
    8    READ (NFIN,1,END=2) REC
    1    FORMAT(A50)

         DO I = 1,50
            A(I) = REC(I:I)
         ENDDO

         IF (RECK.NE.BGTM) GO TO 8
         LAST=50
         DO 6 I=10,50
         J=I
         IF (A(I).NE.BLK) GO TO 7
    6    CONTINUE
    7    CALL GETNUM (ACTTIM(2),KEY,J,K)
         IF (KEY.NE.0) THEN
            NERR=NERR+1
            WRITE (NFOUT,5) REC
    5       FORMAT (/' ERROR # 433; EXPECTED TIME NOT FOUND IN '/A50)
            ACTTIM(2)=99.D15
         ENDIF
         GO TO 3
    2    ACTTIM(2)=99.D15
      ENDIF

    3 IF (NUMPRC.GT.1) THEN

      CALL MPI_BCAST(ACTTIM(2),1,MPI_DOUBLE_PRECISION,0,
     & MPI_COMM_WORLD,IERR)

      ENDIF
      END
C*********************************************************************
      SUBROUTINE SETSTEP ()
C*********************************************************************

C  SETS THE NEXT TIME STEP SIZE

C*********************************************************************

      IMPLICIT NONE
      INCLUDE 'control.h'
      INTEGER N
      LOGICAL :: DBG = .FALSE.

! bag8 debug
      IF (DBG.AND.MYPRC.EQ.0) THEN
        WRITE(*,*)'In SETSTEP'
        WRITE(*,*)'TIM=',TIM
        WRITE(*,*)'DTIMMUL=',DTIMMUL
        WRITE(*,*)'STEPMULT=',STEPMULT
        WRITE(*,*)'DTIMMAX=',DTIMMAX
        WRITE(*,*)'DTSTAB=',DTSTAB
        WRITE(*,*)'DTIMTOL=',DTIMTOL
      ENDIF

      IF(TIM > 0.0D0) DELTIM=MIN(DTIMMUL,STEPMULT)*DELTIM
      DELTIM=MIN(DELTIM,DTIMMAX,DTSTAB)
      IF (DBG.AND.MYPRC.EQ.0) WRITE(*,*)'DELTIM(1)=',DELTIM        ! bag8 debug
      DO 1 N=1,NACTTIM
      IF (TIM+2.D0*DELTIM.GT.ACTTIM(N)) THEN
         IF (DBG.AND.MYPRC.EQ.0) WRITE(*,*)'N=',N
         IF (TIM+DELTIM.GT.ACTTIM(N)-DTIMTOL) THEN
            DELTIM=ACTTIM(N)-TIM
            IF (DBG.AND.MYPRC.EQ.0) WRITE(*,*)'DELTIM(2)=',DELTIM  ! bag8 debug
         ELSE
            DELTIM=.51D0*(ACTTIM(N)-TIM)
            IF (DBG.AND.MYPRC.EQ.0) WRITE(*,*)'DELTIM(3)=',DELTIM  ! bag8 debug
         ENDIF
      ENDIF
    1 CONTINUE
      IF (DELTIM.LT.DTIMMIN) THEN
         DELTIM=DTIMMIN
         IF (DBG.AND.MYPRC.EQ.0) WRITE(*,*)'DELTIM(4)=',DELTIM     ! bag8 debug
      ENDIF

      END

C*********************************************************************
      SUBROUTINE STEP (NERR)
C*********************************************************************

C  ROUTINE MAKES ONE TIME STEP

C  NERR = ERROR NUMBER STEPED BY 1 ON ERROR (INPUT & OUTPUT, INTEGER)

C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'

      INCLUDE 'control.h'
      INCLUDE 'blkary.h'
      INCLUDE 'wells.h'

      INTEGER I,J,MM,N,KONVG,KV,NERR,ITLIN
      REAL*8 DUM
      INTEGER NBEM(19)
      CHARACTER*8 ERT
      DATA NBEM/19*0/

      IF((NHISUSE == 0).AND.(NSTEP < 1)) GO TO 31

      NCUT=0
      ERT='(MODEL)'
      NBEM(13)=3
      NBEM(5)=10
      NBEM(2)=32

    7 NEWT=0     ! FLOW NEWTON COUNTER (FOR CURRENT COUPLING ITERATION)
      NEWT_TL=0  ! FLOW NEWTON COUNTER (TOTAL FOR CURR TIME)
      ITLIN=0    ! FLOW LINEAR ITERATION COUNTER (CURRENT NEWTON)
      ITLINT=0   ! FLOW LINEAR ITERATION COUNTER (TOTAL FOR CURR TIME)
      GCITER=0   ! FIXED-STRESS ITERATIVE COUPLING ITERATION COUNTER

ctm   TAMEEM
      IF(MODELON(15).AND.(Q_MULTIRATE.NE.1)) THEN
      GCITER_C = 0
      M_ITER = 0
      CALL GSAVE_OLDTIMP_MR(NERR)
      ENDIF
ctm   TAMEEM


C  NEWTONIAN ITERATION LOOP
C  COMPUTE JACOBIAN AND RESIDUALS
C  START INTER-BLOCK DATA TRANSFER

    1 NEWT=NEWT+1
      NEWT_TL=NEWT_TL+1

ctm   TAMEEM
      IF(MODELON(15)) THEN
! SAUMIK - COUP_ITRN ALWAYS ZERO FOR NON-MULTIRATE RUNS
      IF(COUP_ITRN.EQ.1) THEN
         IF (MYPRC.EQ.0)
     &     WRITE(*,*) 'IPARS: COUP ITER, M_ITER: ',GCITER_C,M_ITER
         COUP_ITRN = 0
! SAUMIK - NEWT NOT RESET TO ONE IN NON-MULTIRATE RUNS
         NEWT = 1
      ELSE
         IF (MYPRC.EQ.0) WRITE(*,*), 'IPARS: NEWTON ITERATION: ',NEWT
      ENDIF
      ENDIF
c  The output statements should be here - but you need a flag
c  to distinguish between Newton and coupling iterations ..
ctm   TAMEEM

   31 CONTINUE

      IF (VMDEBUG) CALL PRINT_VM_INFO('Before STEP1')

      MODACT=15
      IF (MODELON(15)) THEN
         CALL ESTEP1(NERR)
         GOTO 100
      ENDIF

C      MODACT=2
C      IF (MODELON(2)) CALL ISTEP1(NERR)
C      MODACT=3
C      IF (MODELON(3)) CALL XSTEP1(NERR)
C      MODACT=16
C      IF (MODELON(16)) CALL XSTEP1(NERR)
C      MODACT=5
C      IF (MODELON(5)) CALL HSTEP1(NERR)
C      MODACT=19
C      IF (MODELON(19)) CALL HSTEP1(NERR)
C      MODACT=18
C      IF (MODELON(18)) CALL HSTEP1(NERR)
C      MODACT=7
C      IF (MODELON(7)) CALL MSTEP1(NERR)
C      MODACT=13
C      IF (MODELON(13)) CALL TSTEP1(NERR)
      MODACT=17
      IF (MODELON(17)) CALL TSTEP1(NERR)

 100  MODACT=0
      IF((NHISUSE == 0).AND.(NSTEP < 1)) GO TO 32
      IF (NERR.GT.0) GO TO 3


C  PASS INTERFACE BUFFERS FROM B BLOCK PROCESSOR TO A BLOCK PROCESSOR

C       IF(MODELON(3)) THEN
C          NBEM(3)=NBEMC(3)
C       ENDIF
C       IF(MODELON(16)) THEN
C          NBEM(16)=NBEMC(16)
C       ENDIF
C       CALL PIFBUF8(NBEM,NERR)

C  COLLECT INITIAL MASS DATA ON PROCESSOR 0 THEN DISTRIBUTE IT

      IF (NEWT.EQ.1.AND.TIM.EQ.0.D0) THEN
         CALL SUMIT(13*19,BALANCE(1,1,4))
         CALL SPREAD8(13*19,BALANCE(1,1,4))
      ENDIF

C  CONTINUE TIME STEP
C  COMPLETE INTER-BLOCK JACOBIAN/RESIDUAL CALCULATIONS
C  CHECK NEWTONIAN CONVERGENCE

   32 CONTINUE

      KONVG=1
      KV=1

      IF (VMDEBUG) CALL PRINT_VM_INFO('Before STEP2')

      MODACT=15
      IF (MODELON(15)) THEN
         CALL ESTEP2(KV,NERR)
         KONVG=MAX(KONVG,KV)
         GOTO 200
      ENDIF

C      MODACT=2
C      IF (MODELON(2)) CALL ISTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

C      MODACT=3
C      IF (MODELON(3)) CALL XSTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

C      MODACT=16
C      IF (MODELON(16)) CALL XSTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

C      MODACT=5
C      IF (MODELON(5)) CALL HSTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

C      MODACT=19
C      IF (MODELON(19)) CALL HSTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

C      MODACT=18
C      IF (MODELON(18)) CALL HSTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

C      MODACT=7
C      IF (MODELON(7)) CALL MSTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

C      MODACT=13
C      IF (MODELON(13)) CALL TSTEP2(KV,NERR)
C      KONVG=MAX(KONVG,KV)

      MODACT=17
      IF (MODELON(17)) CALL TSTEP2(KV,NERR)
      KONVG=MAX(KONVG,KV)
  200     MODACT=0

      IF((NHISUSE == 0).AND.(NSTEP < 1)) GO TO 2

C  KONVG = NEWTONIAN CONVERGENCE FLAG
C        = 1 ==> CONVERGED
C        = 2 ==> CONTINUE ITERATION
C        = 3 ==> FAILED
C        = 4 ==> CUT TIME STEP (SET BELOW ONLY BY THIS ROUTINE)

      IF (NEWT.GT.1) THEN
         DUM=KONVG
         CALL MAXIT(1,DUM)
         KONVG=DUM+.1D0
         CALL SPREAD(1,KONVG)
         IF (KONVG.EQ.1) GO TO 2
         IF (KONVG.EQ.3) THEN
            ERT='(NEWTON)'
            GO TO 3
         ENDIF
      ENDIF

C  SOLVE LINEAR SYSTEM

      CALL TIMON(9)

CC      CALL LSOR(ITLIN,NERR)
CC      CALL MULGRD(ITLIN,NERR)
CC      CALL TICAMA(ITLIN,NERR)
CC      CALL GMRES(ITLIN,NERR)
CC      CALL PCG(ITLIN,NERR)
      CALL HYPRE_SOLVE(ITLIN,NERR)

      ITLINT=ITLINT+ITLIN
      CALL TIMOFF(9)
      IF (NERR.EQ.0) GO TO 2
      ERT='(LINEAR)'

C  STEP FAILED; CUT TIME STEP OR ABORT

    3 NCUT=NCUT+1

C       IF (NCUT.GT.2 .OR. (NCUT.EQ.1 .AND. DELTIM.LT..24D0*DTIMMAX
      IF (NCUT.GT.10 .OR. (NCUT.EQ.1 .AND. DELTIM.LT..01D0*DTIMMAX
     &    .AND. .NOT.MODELON(3) .AND. .NOT.MODELON(16)))THEN
         IF (KONVG.EQ.3) THEN
            NERR=NERR+1
            IF (LEVELC) THEN
               WRITE(NFOUT,4) NSTEP
               WRITE(*,4) NSTEP
            ENDIF
    4       FORMAT(' ERROR - NEWTONIAN ITERATION FAILURE AT STEP',I5)
         ELSE
            KONVG=3
            IF (LEVELC) THEN
               WRITE(NFOUT,5) NSTEP
               WRITE(*,5) NSTEP
            ENDIF
    5       FORMAT(' ERROR - LINEAR ITERATION FAILURE AT STEP',I5)
         ENDIF
         GO TO 2
      ENDIF

      DELTIM=.5001*DELTIM
      NERR=0
      KONVG=4

      IF (LEVELC) THEN
         WRITE(NFOUT,6) DELTIM,NSTEP,TIM,ERT
         WRITE(*,6) DELTIM,NSTEP,TIM,ERT
    6    FORMAT(' FLOW CUTTING TIME STEP TO',F9.4,' AT STEP',I5,
     &     '  TIME',F11.3,2X,A8)
!         IF (DELTIM.LT.DTIMMIN) STOP 'DELTIM below DTIMMIN'
      ENDIF

C  COMPLETE, CONTINUE, CUT, OR ABORT TIMESTEP

    2 CONTINUE

      IF (VMDEBUG) CALL PRINT_VM_INFO('Before STEP3')

      MODACT=15
      IF (MODELON(15)) THEN
         CALL ESTEP3(KONVG,NERR)
         GOTO 300
      ENDIF

C      MODACT=2
C      IF (MODELON(2)) CALL ISTEP3(KONVG,NERR)
C      MODACT=3
C      IF (MODELON(3)) CALL XSTEP3(KONVG,NERR)
C      MODACT=16
C      IF (MODELON(16)) CALL XSTEP3(KONVG,NERR)
C      MODACT=5
C      IF (MODELON(5)) CALL HSTEP3(KONVG,NERR)
C      MODACT=19
C      IF (MODELON(19)) CALL HSTEP3(KONVG,NERR)
C      MODACT=18
C      IF (MODELON(18)) CALL HSTEP3(KONVG,NERR)
C      MODACT=7
C      IF (MODELON(7)) CALL MSTEP3(KONVG,NERR)
C      MODACT=13
C      IF (MODELON(13)) CALL TSTEP3(KONVG,NERR)
      MODACT=17
      IF (MODELON(17)) CALL TSTEP3(KONVG,NERR)
  300    MODACT=0

      IF((NHISUSE == 0).AND.(NSTEP < 1)) RETURN

C  LOOP TO NEXT NEWTONIAN ITERATION OR RESTART TIME STEP

      IF (NERR.EQ.0) THEN
         IF (KONVG.EQ.2) GO TO 1
         IF (KONVG.EQ.4) GO TO 7
      ENDIF

C  COMPUTE BALANCES

      CALL SUMIT(3*13*19,BALANCE(1,1,1))

      ITLINR=ITLINR+ITLINT
      IF (GCITER.EQ.0) THEN
        ITNEWTR=ITNEWTR+NEWT_TL-1
      ELSE
        ITNEWTR=ITNEWTR+NEWT_TL-GCITER
      ENDIF
      NSTEPR=NSTEPR+1

      IF (MYPRC.EQ.0) THEN

         N=0
         DO 8 I=1,19
         IF (MODELON(I)) THEN

            IF (I.EQ.15) CYCLE

            N=N+1
            DO 9 J=1,MODEQS(I)

            BALANCE(J,I,5)=BALANCE(J,I,5)+BALANCE(J,I,2)
            BALANCE(J,I,6)=BALANCE(J,I,6)+BALANCE(J,I,3)
            DUM=BALANCE(J,I,1)+BALANCE(J,I,4)
            IF (DUM.NE.0.D0) THEN
               BALANCE(J,I,8)=1.D0+2.D0*(BALANCE(J,I,1)-BALANCE(J,I,4)
     &         -BALANCE(J,I,5)+BALANCE(J,I,6)-BALANCE(J,I,7))/DUM
            ELSE
               BALANCE(J,I,8)=1.D0
            ENDIF

            IF (BUGKEY(5)) WRITE(NFOUT,15) NSTEP,I,J,BALANCE(J,I,1),
     &         BALANCE(J,I,4),BALANCE(J,I,1)-BALANCE(J,I,4),
     &         BALANCE(J,I,5),BALANCE(J,I,6),BALANCE(J,I,2),
     &         BALANCE(J,I,3),BALANCE(J,I,7),BALANCE(J,I,8)
   15       FORMAT(' STEP',I6,' MODEL',I3,' EQS.',I2,' CURRENT',
     &         G17.10,' INITIAL',G17.10/' ACCUM.',G17.10,
     &         ' CUM. INJ.',G17.10,' CUM. TRANS.',G17.10/
     &         ' STEP INJ.',G17.10,' STEP TRANS.',G17.10,
     &         ' BC FLUX',G17.10,' BAL.',G17.10)

    9       CONTINUE

            IF (LEVELC) THEN
               MM=MODEQS(I)
               IF (MM.GT.3) MM=3
               IF (N.EQ.1) THEN
                  IF (GCITER.GT.0) THEN
                    IF (TIM.LT.999.D0) THEN
                       WRITE(*,20) NSTEP,TIM+DELTIM,NEWT_TL-GCITER,
     &                   ITLINT,GCITER,I,(BALANCE(J,I,8),J=1,MM)
   20                  FORMAT(1P,' STEP',I6,' TIME',G11.4,' NEWT',I3,
     &                   ' LIN',I4,' GCITER',I4,' MODEL',I3,
     &                   ' BAL ',0P,3F10.7)
                    ELSE
                       WRITE(*,24) NSTEP,TIM+DELTIM,NEWT_TL-GCITER,
     &                   ITLINT,GCITER,I,(BALANCE(J,I,8),J=1,MM)
   24                  FORMAT(1P,' STEP',I8,' TIME',G11.4,' NEWT',I3,
     &                   ' LIN',I4,' GCITER',I4,' MODEL',I3,
     &                   ' BAL ',0P,3F10.7)
                  ENDIF
                    ELSE
                    IF (TIM.LT.999.D0) THEN
                       WRITE(*,10) NSTEP,TIM+DELTIM,NEWT_TL-1,
     &                   ITLINT,I,(BALANCE(J,I,8),J=1,MM)
   10                  FORMAT(1P,' STEP',I6,' TIME',G11.4,' NEWT',I3,
     &                   ' LIN',I4,' MODEL',I3,' BAL ',0P,3F10.7)
                    ELSE
                       WRITE(*,14) NSTEP,TIM+DELTIM,NEWT_TL-1,
     &                   ITLINT,I,(BALANCE(J,I,8),J=1,MM)
   14                  FORMAT(1P,' STEP',I8,' TIME',G11.4,' NEWT',I3,
     &                   ' LIN',I4,' MODEL',I3,' BAL ',0P,3F10.7)
                    ENDIF
                  ENDIF
               ELSE
                  WRITE(*,11) I,(BALANCE(J,I,8),J=1,MM)
   11             FORMAT(T38,' MODEL',I3,' BAL',3F10.7)
               ENDIF
               IF (MM.LT.MODEQS(I))
     &            WRITE(*,12) I,(BALANCE(J,I,8),J=4,MODEQS(I))
   12             FORMAT(T49,3F10.7)
            ENDIF
         ENDIF
    8    CONTINUE
      ENDIF
cgp dbg: after printing step summary
c         PAUSE

      END

!----------------------------------------------------------------------
! bag8 debug - routines for writing and reading (and comparing)
!              grid element arrays for parallel debugging
!----------------------------------------------------------------------

      MODULE geamod
      IMPLICIT NONE
      SAVE

      LOGICAL :: GEA_EXISTS = .FALSE.
      LOGICAL :: GEA_BINARY = .TRUE.
      INTEGER :: GEAUNIT = 30
      CHARACTER*80 :: GEAFILE = 'gea.out'
      REAL*8 :: GTOL = 1.D-10

      END MODULE

!----------------------------------------------------------------------

      SUBROUTINE DEBUG_GEA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &     KL2,KEYOUT,NBLK,GEA,NDIM4)
      USE geamod
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK,NDIM4
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  GEA(IDIM,JDIM,KDIM)

      IF (GEADBG.EQ.0) THEN
        RETURN
      ELSEIF (GEADBG.EQ.1) THEN
        CALL WRITE_GEA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &     KL2,KEYOUT,NBLK,GEA,NDIM4)
      ELSEIF (GEADBG.EQ.2) THEN
        CALL READ_GEA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &     KL2,KEYOUT,NBLK,GEA,NDIM4)
      ELSE
        STOP 'Unknown GEADBG flag'
      ENDIF

      END

!----------------------------------------------------------------------

      SUBROUTINE WRITE_GEA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &     KL2,KEYOUT,NBLK,GEA,NDIM4)
      USE geamod
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK,NDIM4
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)

      REAL*8  GEA(IDIM,JDIM,KDIM,NDIM4)    ! Case for REAL*8 gea
!      REAL*8  GEA(NDIM4,IDIM,JDIM,KDIM)    ! Case when 4th index first

      IF (MYPRC.EQ.0) WRITE(*,*)'In WRITE_GEA'

      IF (NUMPRC.NE.1) THEN
        WRITE(*,*)'WRITE_GEA Error: NUMPRC must be 1'
        STOP 1
      ENDIF

      IF (.NOT.GEA_EXISTS) THEN
        IF (GEA_BINARY) THEN
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      form='binary',
     &      status='unknown')
          WRITE(GEAUNIT) NXDIM(NBLK),NYDIM(NBLK),
     &                   NZDIM(NBLK),NDIM4
        ELSE
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      status='unknown')
          WRITE(GEAUNIT,'(4I6)') NXDIM(NBLK),NYDIM(NBLK),
     &                           NZDIM(NBLK),NDIM4
        ENDIF
        GEA_EXISTS = .TRUE.
      ENDIF

      IF (GEA_BINARY) THEN
        WRITE(GEAUNIT) TIM,DELTIM,NSTEP,NEWT
        WRITE(GEAUNIT) GEA(ILAY+1:IDIM-ILAY,
     &                     JLAY+1:JDIM-JLAY,
     &                     KLAY+1:KDIM-KLAY,
     &                     1:NDIM4)
!        WRITE(GEAUNIT) GEA(1:NDIM4,
!     &                     ILAY+1:IDIM-ILAY,
!     &                     JLAY+1:JDIM-JLAY,
!     &                     KLAY+1:KDIM-KLAY)
      ELSE
        WRITE(GEAUNIT,'(2E16.9,2I9)') TIM,DELTIM,NSTEP,NEWT
        WRITE(GEAUNIT,'(6E16.9)') GEA(ILAY+1:IDIM-ILAY,
     &                                JLAY+1:JDIM-JLAY,
     &                                KLAY+1:KDIM-KLAY,
     &                                1:NDIM4)
!        WRITE(GEAUNIT,'(6E16.9)') GEA(1:NDIM4,
!     &                                ILAY+1:IDIM-ILAY,
!     &                                JLAY+1:JDIM-JLAY,
!     &                                KLAY+1:KDIM-KLAY)
      ENDIF

      END

!----------------------------------------------------------------------

      SUBROUTINE READ_GEA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &     KL2,KEYOUT,NBLK,GEA,NDIM4)
      USE geamod
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK,NDIM4
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)

      REAL*8  GEA(IDIM,JDIM,KDIM,NDIM4)    ! Case for REAL*8 gea
!      REAL*8  GEA(NDIM4,IDIM,JDIM,KDIM)    ! Case when 4th index first

      REAL*8, ALLOCATABLE :: GEA2(:,:,:,:)
      INTEGER NSTEP2,NEWT2,IERR,IOFF,JOFF,KOFF,
     &        I,J,K,I2,J2,K2,IDIFF,IBUF,N
      REAL*8  TIM2,DELTIM2

      INTEGER NXTMP,NYTMP,NZTMP,N4TMP

      IF (MYPRC.EQ.0) WRITE(*,*)'In READ_GEA'
      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,IERR)
      IDIFF = 0

      IF (.NOT.GEA_EXISTS) THEN
        IF (GEA_BINARY) THEN
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      form='binary',
     &      access='sequential',
     &      status='old')
          READ(GEAUNIT) NXTMP,NYTMP,NZTMP,N4TMP
        ELSE
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      status='old')
          READ(GEAUNIT,'(4I6)') NXTMP,NYTMP,NZTMP,N4TMP
        ENDIF
        IF (MYPRC.EQ.0) THEN
          WRITE(*,*)'Grid NX,NY,NZ=',NXDIM(NBLK),NYDIM(NBLK),NZDIM(NBLK)
          WRITE(*,*)'File NX,NY,NZ=',NXTMP,NYTMP,NZTMP
        ENDIF
        IF (NXDIM(NBLK).NE.NXTMP) STOP 'Wrong NX'
        IF (NYDIM(NBLK).NE.NYTMP) STOP 'Wrong NY'
        IF (NZDIM(NBLK).NE.NZTMP) STOP 'Wrong NZ'
        IF (NDIM4.NE.N4TMP) STOP 'Wrong NDIM4'
        GEA_EXISTS = .TRUE.
      ENDIF

      ALLOCATE(GEA2(NXDIM(NBLK),NYDIM(NBLK),NZDIM(NBLK),NDIM4),
     &         STAT=IERR)
!      ALLOCATE(GEA2(NDIM4,NXDIM(NBLK),NYDIM(NBLK),NZDIM(NBLK)),
!     &         STAT=IERR)
      IF (IERR.NE.0) STOP 'Could not allocate GEA2'

      IF (GEA_BINARY) THEN
        READ(GEAUNIT) TIM2,DELTIM2,NSTEP2,NEWT2
        READ(GEAUNIT) GEA2
      ELSE
        READ(GEAUNIT,'(2E16.9,2I9)') TIM2,DELTIM2,NSTEP2,NEWT2
        READ(GEAUNIT,'(6E16.9)') GEA2
      ENDIF

!      WRITE(*,*)'TIM,TIM2=',TIM,TIM2
!      WRITE(*,*)'DELTIM,DELTIM2=',DELTIM,DELTIM2
!      WRITE(*,*)'NSTEP,NSTEP2=',NSTEP,NSTEP2
!      WRITE(*,*)'NEWT,NEWT2=',NEWT,NEWT2
      IF (ABS(TIM-TIM2).GT.GTOL) STOP 'Wrong TIM'
      IF (ABS(DELTIM-DELTIM2).GT.GTOL) STOP 'Wrong DELTIM'
      IF (ABS(NSTEP-NSTEP2).GT.0) STOP 'Wrong NSTEP'
      IF (ABS(NEWT-NEWT2).GT.0) STOP 'Wrong NEWT'

      DO I=1,IDIM
      DO J=1,JDIM
      DO K=1,KDIM
      DO N=1,NDIM4
        IF (KEYOUT(I,J,K).EQ.1) THEN
          I2=I+IOFF
          J2=J+JOFF
          K2=K+KOFF

          IF (ABS(GEA(I,J,K,N)-GEA2(I2,J2,K2,N)).GT.GTOL) THEN
!          IF (ABS(GEA(N,I,J,K)-GEA2(N,I2,J2,K2)).GT.GTOL) THEN
!            IF (IDIFF.EQ.0) THEN
              WRITE(*,'(A,I4,A,4I6,2(A,E16.9))')
     &          'Diff: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
     &          ' GEA=',GEA(I,J,K,N),' GEA2=',GEA2(I2,J2,K2,N)
!              WRITE(*,'(A,I4,A,4I6,2(A,E16.9))')
!     &          'Diff: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
!     &          ' GEA=',GEA(N,I,J,K),' GEA2=',GEA2(N,I2,J2,K2)
!            ENDIF

            IDIFF=IDIFF+1

          ENDIF

        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO

      DEALLOCATE(GEA2)

      IBUF=IDIFF
      CALL MPI_ALLREDUCE(IBUF, IDIFF, 1, MPI_INTEGER, MPI_SUM,
     &  MPI_COMM_WORLD, IERR)
      IF (IDIFF.GT.0) THEN
        IF (MYPRC.EQ.0)
     &    WRITE(*,'(I6,A)') IDIFF,' differences detected in GEA!'
        STOP
      ELSE
        IF (MYPRC.EQ.0)
     &    WRITE(*,*) 'No differences detected in GEA!'
!        STOP
      ENDIF

      END

!----------------------------------------------------------------------

      SUBROUTINE DEBUG_GEA_MPFA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,
     &     KL1,KL2,KEYOUT,NBLK,KCR,GEA,NDIM4)
      USE geamod
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK,NDIM4
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      INTEGER KCR(IDIM+1,JDIM+1,KDIM+1)

!      INTEGER GEA(IDIM+1,JDIM+1,KDIM+1,NDIM4)    ! Case for INTEGER gea
!      REAL*8  GEA(IDIM+1,JDIM+1,KDIM+1,NDIM4)    ! Case for REAL*8 gea
      REAL*8  GEA(NDIM4,IDIM+1,JDIM+1,KDIM+1)    ! Case when 4th index first

      IF (GEADBG.EQ.0) THEN
        RETURN
      ELSEIF (GEADBG.EQ.1) THEN
        CALL WRITE_GEA_MPFA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &     KL2,KEYOUT,NBLK,KCR,GEA,NDIM4)
      ELSEIF (GEADBG.EQ.2) THEN
        CALL READ_GEA_MPFA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &     KL2,KEYOUT,NBLK,KCR,GEA,NDIM4)
      ELSE
        STOP 'Unknown GEADBG flag'
      ENDIF

      END

!----------------------------------------------------------------------

      SUBROUTINE WRITE_GEA_MPFA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,
     &     KL1,KL2,KEYOUT,NBLK,KCR,GEA,NDIM4)
      USE geamod
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK,NDIM4
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      INTEGER KCR(IDIM+1,JDIM+1,KDIM+1)

!      INTEGER GEA(IDIM+1,JDIM+1,KDIM+1,NDIM4)    ! Case for INTEGER gea
!      REAL*8  GEA(IDIM+1,JDIM+1,KDIM+1,NDIM4)    ! Case for REAL*8 gea
      REAL*8  GEA(NDIM4,IDIM+1,JDIM+1,KDIM+1)    ! Case when 4th index first

      INTEGER IERR,I,J,K,N,I2,J2,K2,N2,NTMP
      REAL*8, ALLOCATABLE :: GEA2(:,:,:,:)

      IF (MYPRC.EQ.0) WRITE(*,*)'In WRITE_GEA'

      IF (NUMPRC.NE.1) THEN
        WRITE(*,*)'WRITE_GEA Error: NUMPRC must be 1'
        STOP 1
      ENDIF

      NTMP=NDIM4

! For debugging AINVF1(24,,,) as size 12
!      IF (NDIM4.NE.24) STOP 'Expecting NDIM4=24'
!      NTMP=NDIM4/2
!      ALLOCATE(GEA2(NTMP,NXDIM(NBLK)+1,NYDIM(NBLK)+1,
!     &              NZDIM(NBLK)+1),STAT=IERR)
!      IF (IERR.NE.0) STOP 'Could not allocate GEA2'
!      DO I=ILAY+1,IDIM-ILAY+1
!      DO J=JLAY+1,JDIM-JLAY+1
!      DO K=KLAY+1,KDIM-KLAY+1
!      DO N=1,NDIM4
!        IF (MOD(N,2).EQ.1) CYCLE
!        I2=I-ILAY
!        J2=J-JLAY
!        K2=K-KLAY
!        N2=(N+1)/2
!        GEA2(N2,I2,J2,K2)=GEA(N,I,J,K)
!      ENDDO
!      ENDDO
!      ENDDO
!      ENDDO

      IF (.NOT.GEA_EXISTS) THEN
        IF (GEA_BINARY) THEN
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      form='binary',
     &      status='unknown')
          WRITE(GEAUNIT) NXDIM(NBLK)+1,NYDIM(NBLK)+1,
     &                   NZDIM(NBLK)+1,NTMP
        ELSE
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      status='unknown')
          WRITE(GEAUNIT,'(4I6)') NXDIM(NBLK)+1,NYDIM(NBLK)+1,
     &                           NZDIM(NBLK)+1,NTMP
        ENDIF
        GEA_EXISTS = .TRUE.
      ENDIF

      IF (GEA_BINARY) THEN
        WRITE(GEAUNIT) TIM,DELTIM,NSTEP,NEWT
!        WRITE(GEAUNIT) GEA(ILAY+1:IDIM-ILAY+1,
!     &                     JLAY+1:JDIM-JLAY+1,
!     &                     KLAY+1:KDIM-KLAY+1,
!     &                     1:NDIM4)
        WRITE(GEAUNIT) GEA(1:NDIM4,
     &                     ILAY+1:IDIM-ILAY+1,
     &                     JLAY+1:JDIM-JLAY+1,
     &                     KLAY+1:KDIM-KLAY+1)
!        WRITE(GEAUNIT) GEA2
!        WRITE(GEAUNIT) KCR
      ELSE
        WRITE(GEAUNIT,'(2E16.9,2I9)') TIM,DELTIM,NSTEP,NEWT
!        WRITE(GEAUNIT,'(6E16.9)') GEA(ILAY+1:IDIM-ILAY+1,
!     &                                JLAY+1:JDIM-JLAY+1,
!     &                                KLAY+1:KDIM-KLAY+1,
!     &                                1:NDIM4)
        WRITE(GEAUNIT,'(6E16.9)') GEA(1:NDIM4,
     &                                ILAY+1:IDIM-ILAY+1,
     &                                JLAY+1:JDIM-JLAY+1,
     &                                KLAY+1:KDIM-KLAY+1)
!        WRITE(GEAUNIT,'(6E16.9)') GEA2
!        WRITE(GEAUNIT,'(11I4)') KCR(ILAY+1:IDIM-ILAY+1,
!     &                              JLAY+1:JDIM-JLAY+1,
!     &                              KLAY+1:KDIM-KLAY+1)
      ENDIF

!      DEALLOCATE(GEA2)

      END

!----------------------------------------------------------------------

      SUBROUTINE READ_GEA_MPFA(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,
     &     KL1,KL2,KEYOUT,NBLK,KCR,GEA,NDIM4)
      USE geamod
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK,NDIM4
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      INTEGER KCR(IDIM+1,JDIM+1,KDIM+1)

!      INTEGER GEA(IDIM+1,JDIM+1,KDIM+1,NDIM4)    ! Case for INTEGER gea
!      REAL*8  GEA(IDIM+1,JDIM+1,KDIM+1,NDIM4)    ! Case for REAL*8 gea
      REAL*8  GEA(NDIM4,IDIM+1,JDIM+1,KDIM+1)    ! Case when 4th index first

!      INTEGER, ALLOCATABLE :: GEA2(:,:,:,:)
      REAL*8, ALLOCATABLE :: GEA2(:,:,:,:)
      INTEGER NSTEP2,NEWT2,IERR,IOFF,JOFF,KOFF,
     &        I,J,K,I2,J2,K2,IDIFF,IBUF,N
      REAL*8  TIM2,DELTIM2

      INTEGER NXTMP,NYTMP,NZTMP,N4TMP

      IF (MYPRC.EQ.0) WRITE(*,*)'In READ_GEA'
      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,IERR)
      IDIFF = 0

      IF (.NOT.GEA_EXISTS) THEN
        IF (GEA_BINARY) THEN
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      form='binary',
     &      access='sequential',
     &      status='old')
          READ(GEAUNIT) NXTMP,NYTMP,NZTMP,N4TMP
        ELSE
          OPEN(unit=GEAUNIT,file=TRIM(GEAFILE),
     &      status='old')
          READ(GEAUNIT,'(4I6)') NXTMP,NYTMP,NZTMP,N4TMP
        ENDIF
        IF (MYPRC.EQ.0) THEN
          WRITE(*,*)'Grid NX,NY,NZ=',NXDIM(NBLK)+1,NYDIM(NBLK)+1,
     &                               NZDIM(NBLK)+1
          WRITE(*,*)'File NX,NY,NZ=',NXTMP,NYTMP,NZTMP
        ENDIF
        IF (NXDIM(NBLK)+1.NE.NXTMP) STOP 'Wrong NX'
        IF (NYDIM(NBLK)+1.NE.NYTMP) STOP 'Wrong NY'
        IF (NZDIM(NBLK)+1.NE.NZTMP) STOP 'Wrong NZ'
        IF (NDIM4.NE.N4TMP) STOP 'Wrong NDIM4'
        GEA_EXISTS = .TRUE.
      ENDIF

!      ALLOCATE(GEA2(NXTMP,NYTMP,NZTMP,NDIM4),STAT=IERR)
      ALLOCATE(GEA2(NDIM4,NXTMP,NYTMP,NZTMP),STAT=IERR)
      IF (IERR.NE.0) STOP 'Could not allocate GEA2'

      IF (GEA_BINARY) THEN
        READ(GEAUNIT) TIM2,DELTIM2,NSTEP2,NEWT2
        READ(GEAUNIT) GEA2
      ELSE
        READ(GEAUNIT,'(2E16.9,2I9)') TIM2,DELTIM2,NSTEP2,NEWT2
        READ(GEAUNIT,'(6E16.9)') GEA2
!        READ(GEAUNIT,'(11I4)') GEA2
      ENDIF

!      WRITE(*,*)'TIM,TIM2=',TIM,TIM2
!      WRITE(*,*)'DELTIM,DELTIM2=',DELTIM,DELTIM2
!      WRITE(*,*)'NSTEP,NSTEP2=',NSTEP,NSTEP2
!      WRITE(*,*)'NEWT,NEWT2=',NEWT,NEWT2
      IF (ABS(TIM-TIM2).GT.GTOL) STOP 'Wrong TIM'
      IF (ABS(DELTIM-DELTIM2).GT.GTOL) STOP 'Wrong DELTIM'
      IF (ABS(NSTEP-NSTEP2).GT.0) STOP 'Wrong NSTEP'
      IF (ABS(NEWT-NEWT2).GT.0) STOP 'Wrong NEWT'

      WRITE(*,*)REPEAT('-',72)
      WRITE(*,*)'Differences:'
      WRITE(*,*)REPEAT('-',72)
      DO I=1,IDIM+1
      DO J=1,JDIM+1
      DO K=1,KDIM+1
      DO N=1,NDIM4
        IF (KCR(I,J,K).GT.0) THEN
          I2=I+IOFF
          J2=J+JOFF
          K2=K+KOFF

!          IF (ABS(GEA(I,J,K,N)-GEA2(I2,J2,K2,N)).GT.0) THEN
!          IF (ABS(GEA(I,J,K,N)-GEA2(I2,J2,K2,N)).GT.GTOL) THEN
          IF (ABS(GEA(N,I,J,K)-GEA2(N,I2,J2,K2)).GT.GTOL) THEN
!            IF (IDIFF.EQ.0) THEN
!              WRITE(*,'(A,I4,A,4I6,2(A,I4))')
!     &          'Diff: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
!     &          ' GEA=',GEA(I,J,K,N),' GEA2=',GEA2(I2,J2,K2,N)
!              WRITE(*,'(A,I4,A,4I6,2(A,E16.9))')
!     &          'Diff: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
!     &          ' GEA=',GEA(I,J,K,N),' GEA2=',GEA2(I2,J2,K2,N)
              WRITE(*,'(A,I4,A,4I6,2(A,E16.9))')
     &          'Diff: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
     &          ' GEA=',GEA(N,I,J,K),' GEA2=',GEA2(N,I2,J2,K2)
!            ENDIF
            IDIFF=IDIFF+1
          ENDIF

        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDDO

! Check KCR
!      DO I=ILAY+1,IDIM-ILAY+1
!      DO J=JLAY+1,JDIM-JLAY+1
!      DO K=KLAY+1,KDIM-KLAY+1
!          I2=I+IOFF
!          J2=J+JOFF
!          K2=K+KOFF
!          IF (ABS(KCR(I,J,K)-GEA2(I2,J2,K2,1)).GT.0) THEN
!!            IF (IDIFF.EQ.0) THEN
!              WRITE(*,'(A,I4,A,3I4,2(A,I4))')
!     &          'Diff: MYPRC=',MYPRC,' I2,J2,K2=',I2,J2,K2,
!     &          ' KCR=',KCR(I,J,K),' KCR2=',GEA2(I2,J2,K2,1)
!!            ENDIF
!            IDIFF=IDIFF+1
!          ENDIF
!      ENDDO
!      ENDDO
!      ENDDO

      IBUF=IDIFF
      CALL MPI_ALLREDUCE(IBUF, IDIFF, 1, MPI_INTEGER, MPI_SUM,
     &  MPI_COMM_WORLD, IERR)
      IF (IDIFF.GT.0) THEN
        IF (MYPRC.EQ.0)
     &    WRITE(*,'(I6,A)') IDIFF,' differences detected in GEA!'
          STOP
      ELSE
        IF (MYPRC.EQ.0)
     &    WRITE(*,*) 'No differences detected in GEA!'
!          STOP
      ENDIF

! Print non-zero entries
!      WRITE(*,*)REPEAT('-',72)
!      WRITE(*,*)'Non-zeros:'
!      WRITE(*,*)REPEAT('-',72)
!      DO I=ILAY+1,IDIM-ILAY+1
!      DO J=JLAY+1,JDIM-JLAY+1
!      DO K=KLAY+1,KDIM-KLAY+1
!      DO N=1,NDIM4
!        I2=I+IOFF
!        J2=J+JOFF
!        K2=K+KOFF
!
!!        IF (ABS(GEA(I,J,K,N)).GT.0) THEN
!        IF (ABS(GEA(N,I,J,K)).GT.GTOL) THEN
!!            WRITE(*,'(A,I4,A,4I6,2(A,I4))')
!!     &        'Non-zero: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
!!     &        ' GEA=',GEA(I,J,K,N),' GEA2=',GEA2(I2,J2,K2,N)
!!            WRITE(*,'(A,I4,A,4I6,2(A,E16.9))')
!!     &        'Non-zero: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
!!     &        ' GEA=',GEA(I,J,K,N),' GEA2=',GEA2(I2,J2,K2,N)
!            WRITE(*,'(A,I4,A,4I6,2(A,E16.9))')
!     &        'Non-zero: MYPRC=',MYPRC,' I2,J2,K2,N=',I2,J2,K2,N,
!     &        ' GEA=',GEA(N,I,J,K),' GEA2=',GEA2(N,I2,J2,K2)
!        ENDIF
!
!      ENDDO
!      ENDDO
!      ENDDO
!      ENDDO

      DEALLOCATE(GEA2)

      END

!----------------------------------------------------------------------

      SUBROUTINE CLOSE_GEA()
      USE geamod
      IMPLICIT NONE

      IF (GEA_EXISTS) CLOSE(GEAUNIT)

      END

