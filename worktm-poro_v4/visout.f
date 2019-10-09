C  VISOUT.F - PRINT VISUALIZATION OUTPUT

C  ROUTINES IN THIS MODULE:

c  SUBROUTINE VIS_INIT (nerr)
C  SUBROUTINE VISOUT  (nerr)
c  SUBROUTINE GET_VISPARAMS (nerr)
c  SUBROUTINE GET_VISVARS (nerr)
c  SUBROUTINE VIS_ARYNUM (num)
c  SUBROUTINE VIS_TIME ( NERR )
c  SUBROUTINE VIS_OUTPUT ()
c  SUBROUTINE PERMOUT()
c  SUBROUTINE PERMCPY()

C  CODE HISTORY:

c  Malgo Peszynska, 5/21/98     initial version
c  Malgo Peszynska, 5/27/98     modified routines so that
c       all visulization output be controlled by the data file
c  Malgo Peszynska, 9/25/98     expanded logic for setting
c       vis. parameters: from initial AND transient data blocks
c       added some time step output, modularized the code
c  Sunil G. Thomas  9/25/09     added interfaces to vtk visoutputs.
c  Gergina Pencheva 10/12/15    added flag for binary/ascii vtk output
c  Ben Ganis        6/21/16     implemented visflag 9

c -----------------------------------------------------------
      SUBROUTINE VIS_INIT (NERR)
c ===============================
c initializes vis. variables, sets defaults for output
c ===============================
      IMPLICIT NONE
C       include 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'visual.h'

      INTEGER NERR
c ------------------------------------------------

c ========== set defaults (no visualization)
      VIS_SCL = 0
      VIS_NVEC = 0

      VISFLAG = 3        ! Tecplot visualization
      VIS_FNAME = "TEC"
      VIS_FSTYLE = 'NO_PC'

! gp - ascii or binary visual output (for visflag 7 to 9)
      VIS_BINARY = .TRUE.

! bag8 - state variable for visflag 9
      TECEXISTS = .FALSE.
      ALLZONES = .FALSE.

      CALL GET_MAXANAM(VIS_NAMELEN)

c allocate a dummy visualization grid array: this one will hold
c values 0.0 used when wrong variable name is set in input file

      CALL ALCGEA ('VIS_DUMMY ',2,0,N_VISDUM,NERR)

      IF (NERR.NE.0) GO TO 1

c =========  verify if there is any visualization request in the input file
c the logic is: if visout, dvisout active, then visout is called
c which does the job for the current set of variables which is allowed to
c be set at initialization OR in each transient block.
c if any errors arise for vis output, if they are not fatal.
c then the code should continue computation but quit producing vis output

      CALL VISUAL_INIT()

      CALL GET_VISPARAMS (NERR)

      CALL PNTMMODMB(MBPOROE,VIS_SCL_POROHEX,
     &     VIS_SCL_FLOW(17),VIS_SCL_FLOW(16))
      ! SAUMIK, BGANIS

 1    CONTINUE
      RETURN
      END

C*********************************************************************
      SUBROUTINE VISQUIT (NERR)
c ===============================
c   executive routine for quitting visualization output
c================================
      IMPLICIT NONE
      INTEGER NERR,KERR
C       INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'visual.h'

      IF ((VISFLAG.EQ.9).OR.(VISFLAG.EQ.10)) CALL VIS_TEC_CLOSE()

      RETURN
      END

C*********************************************************************
      SUBROUTINE VISOUT (NERR)
c ===============================
c   executive routine for printing visualization output
c================================
      IMPLICIT NONE
C       INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INCLUDE 'layout.h'
      INTEGER NERR,KERR

      LOGICAL ONCEONLY
      DATA ONCEONLY /.TRUE./

c ------------------------------------------------------------

C      IF(.NOT.IFPV3) RETURN

      IF (ONCEONLY) THEN
         ONCEONLY=.FALSE.

c "register" the visualization parameters for callwork

         CALL PNTVAR(VISFLAG, N_VISFLAG,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(VIS_BINARY, N_VIS_BINARY,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(XREC(1,1), N_VIS_XREC,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(YREC(1,1), N_VIS_YREC,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(ZREC(1,1), N_VIS_ZREC,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(DXREC(1,1), N_VIS_DXREC,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(DYREC(1,1), N_VIS_DYREC,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(DZREC(1,1), N_VIS_DZREC,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(VIS_SCL, N_VIS_SCL,KERR)
         IF (KERR.GT.0) GO TO 13

         IF(MBPOROE) THEN ! SAUMIK,BGANIS
            CALL PNTVAR(VIS_SCL_POROHEX, N_VIS_SCL_POROHEX,KERR)
            IF (KERR.GT.0) GO TO 13
      CALL PNTVAR(VIS_SCL_FLOW(17),
     &            N_VIS_SCL_FLOW(17),KERR)
      IF (KERR.GT.0) GO TO 13
C      CALL PNTVAR(VIS_SCL_FLOW(16),
C     &            N_VIS_SCL_FLOW(16),KERR)
C      IF (KERR.GT.0) GO TO 13
         ENDIF

         CALL PNTVAR(VIS_VEC, N_VIS_VEC,KERR)
         IF (KERR.GT.0) GO TO 13
         CALL PNTVAR(VIS_NVARS, N_VIS_NVARS,KERR)
         IF (KERR.GT.0) GO TO 13

c  model specific initialization, if necessary
         MODACT = 15
         IF (MODELON(15)) THEN
            CALL EVIS_INIT()
            GOTO 1
         ENDIF

C         MODACT=14
C         IF (MODELON(14)) THEN
C            CALL TRVIS_INIT()
C            GOTO 1
C         ENDIF

C         MODACT=7
C         IF (MODELON(7)) THEN
C            CALL MVIS_INIT()
C            GOTO 1
C         ENDIF

C         MODACT=2
C         IF (MODELON(2)) CALL IVIS_INIT()
C         MODACT=3
C         IF (MODELON(3)) CALL XVIS_INIT()
C         MODACT=16
C         IF (MODELON(16)) CALL XVIS_INIT()
C         MODACT=5
C         IF (MODELON(5)) CALL HVIS_INIT()
C         MODACT=19
C         IF (MODELON(19)) CALL HVIS_INIT()
C         MODACT=18
C         IF (MODELON(18)) CALL HVIS_INIT()
C         MODACT=13
C         IF (MODELON(13)) CALL TVIS_INIT()
         MODACT=17
         IF (MODELON(17)) CALL TVIS_INIT()
    1    MODACT=0

      ENDIF

c output info about the current time step and update C info about time


      CALL VIS_TIME(NERR)


c  model specific output

      MODACT = 15
      IF (MODELON(15)) THEN
         CALL EVIS_OUTPUT()
         GOTO 2
      ENDIF

C      MODACT=14
C      IF (MODELON(14)) THEN
C         CALL TRVIS_OUTPUT()
C         GOTO 2
C      ENDIF

C      MODACT=7
C      IF (MODELON(7)) THEN
C         CALL MVIS_OUTPUT()
C         GOTO 1
C      ENDIF

C      MODACT=2
C      IF (MODELON(2)) CALL IVIS_OUTPUT()
C      MODACT=3
C      IF (MODELON(3)) CALL XVIS_OUTPUT()
C      MODACT=16
C      IF (MODELON(16)) CALL XVIS_OUTPUT()
C      MODACT=5
C      IF (MODELON(5)) CALL HVIS_OUTPUT()
C      MODACT=19
C      IF (MODELON(19)) CALL HVIS_OUTPUT()
C      MODACT=18
C      IF (MODELON(18)) CALL HVIS_OUTPUT()
C      MODACT=13
C      IF (MODELON(13)) CALL TVIS_OUTPUT()
      MODACT=17
      IF (MODELON(17)) CALL TVIS_OUTPUT()

    2 MODACT=0

      IF ((VISFLAG.EQ.7).OR.(VISFLAG.EQ.8).OR.(VISFLAG.EQ.10)) THEN
      CALL VIS_PVTU_QUIT()
      ENDIF

! bag8 - In case of grid adaptivity, deallocate grid information
!        so it can be recomputed on each time step.
C      IF (ADAPTIVITY) CALL VIS_TEC_DEALLOC()

      RETURN

 13   CONTINUE
      NERR = NERR + 1
      RETURN

      END

c -----------------------------------------------------------
      SUBROUTINE GET_VISPARAMS ( NUMERR )
c ====================================
c     called from getidat (idata.f) and gettimd (tdata.f)
c     to get parameters for visualization
c ====================================
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'visual.h'
      INCLUDE 'mpif.h'

      INTEGER NUMERR
      INTEGER STATUS,SYSTEM,IERR
      INTEGER NERR,NDUM,I,VIS_FSTYLE_FLAG,IFNEW_FLAG
      LOGICAL VIS_ERROR, NEW
      DATA IFNEW_FLAG /1/
C      REAL*4  PV3XS

c ------------------------------------------------
c flag set only if initial output (perms, porosities and such) is requested
c Note: currently VISINIT and IFPERM or ifwell
c can NOT be simultanously requested.
c The default action is to set IFPERM to .false. and proceed.

      IFPERM=.FALSE.
      CALL GETVAL('PERMOUT ',IFPERM,'FG',0,0,0,0,NDUM,NERR)

      IFWELL=.FALSE.
      CALL GETVAL('WELLVIS ',IFWELL,'FG',0,0,0,0,NDUM,NERR)

      IFVISINIT=.FALSE.
      CALL GETVAL('VISINIT ',IFVISINIT,'FG',0,0,0,0,NDUM,NERR)

      IF ( (IFPERM.OR.IFWELL).AND.IFVISINIT) THEN
         IFPERM = .false.
         IFWELL = .false.
         IF (MYPRC.EQ.0) THEN
        WRITE(NFOUT,*) 'ERROR: BOTH VISINIT AND ',
     &  'PERMOUT OR WELLOUT',
     &  'SPECIFIED IN INPUT FILE. SKIPPING PERMOUT and WELLOUT .'
         ENDIF
      ENDIF

c =========================================

C      IFPV3 = .FALSE.
C      CALL GETVAL('PV3OUT ',IFPV3,'FG',0,0,0,0,NDUM,NERR)
C      PV3XS = 1.D0
C      CALL GETVAL('PV3XSCALE ',PV3XS,'R4',0,0,0,0,NDUM,NERR)
C      IF(NDUM.GT.0) CALL PV3SCALE(PV3XS,XYZ111(1,1))

c =========================================
      VIS_ERROR = .FALSE.
      NEW = .FALSE.

c local nerr : do not quit computations even if visualization pars
c were messed up, just print the info
      NERR = 0

C =========================================
C === THE VALUES OF VIS_SCL, VIS_NVEC MUST BE SET FOR VIS. TO BE ACTIVE

      CALL GETVAL('VIS_SCL ',VIS_SCL,'I4',0,0,0,0,NDUM,NERR)
      IF (NDUM.GT.0) NEW = .TRUE.
      CALL GETVAL('VIS_NVEC ',VIS_NVEC,'I4',0,0,0,0,NDUM,NERR)
      IF (NDUM.GT.0) NEW = .TRUE.

C      IF(VIS_NVEC.GT.0) VIS_NVEC = 0

      VIS_VEC = 3 * VIS_NVEC
      VIS_NVARS = VIS_SCL + VIS_VEC

      IF (VIS_NVARS.LT.0.OR.VIS_NVARS.GT.MAXVISVARS) THEN
         VIS_ERROR = .TRUE.
         GO TO 1
      ELSE

c === get the names of the variables, translate them etc.

         call GET_VISVARS(nerr,new)

      ENDIF

c =============================================
c ==== the parameters below have default values
c ==== get visflag or pv3out and verify it

C      IF(IFPV3) GOTO 101

      CALL GETVAL('VISFLAG ',VISFLAG,'I4',0,0,0,0,NDUM,NERR)
      IF (NDUM.GT.0) NEW = .TRUE.
      IF (VISFLAG.LE.0.OR.VISFLAG.GT.MAXVISFLAG)VIS_ERROR = .TRUE.

cgp ABORT IF VISFLAG=8 IS CHOSEN FOR NON-HEXADRAL GRID OR
C    ANY FLAG OTHER THAN 8 OR 9 IS CHOSEN FOR HEXA GRID
      IF (NEW) THEN
        IF (KNDGRD==3) THEN
          IF ((VISFLAG/=8).AND.(VISFLAG/=9).AND.(VISFLAG/=10)) THEN
            STOP 'Wrong VISFLAG for hexahedra'
          ENDIF
        ELSE
          IF (VISFLAG==8) THEN
            STOP 'Wrong VISFLAG for bricks'
          ENDIF
        ENDIF
      ENDIF

      IF(VISFLAG.EQ.4) CALL SETSLICES()

      CALL GETVAL('VIS_BINARY ',VIS_BINARY,'L4',0,0,0,0,NDUM,NERR)

c bag8, gp === get directory name
      CALL GETVALS('VIS_DIR ',VIS_DIR,'CS',0,0,0,50,NDUM,
     &     NERR)
      IF (NDUM.GT.0) THEN
        NEW = .TRUE.
        IF (MYPRC.EQ.0) THEN
          status = SYSTEM('[ -d '//trim(VIS_DIR)//' ]')
          if (status.ne.0) then
            write(*,*)'Creating directory: ',trim(VIS_DIR)
            status = SYSTEM('mkdir '//trim(VIS_DIR))
            if (status.ne.0) then
              write(*,*)'Directory creation failed!'
              stop 42
            endif
          endif
        ENDIF
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      ENDIF
c bag8 ===

c === get root of file names

      CALL GETVALS('VIS_FNAME ',VIS_FNAME,'CS',0,0,0,50,NDUM,
     &     NERR)
      IF (NDUM.GT.0) NEW = .TRUE.

c == get the file style flag and verify it

      CALL GETVALS('VIS_FSTYLE ',VIS_FSTYLE,'CS',0,0,0,20,NDUM,
     &     NERR)
      IF (NDUM.GT.0) NEW = .TRUE.

      VIS_FSTYLE_FLAG = 0
      IF (VIS_FSTYLE.EQ.'NO_PC') VIS_FSTYLE_FLAG = 1
      IF (VIS_FSTYLE.EQ.'PC') VIS_FSTYLE_FLAG = 2
      IF(VIS_FSTYLE_FLAG.LT.1.OR.VIS_FSTYLE_FLAG.GT.2)
     &     VIS_ERROR =.TRUE.

 101  continue

c =========================================
c == process error and initialization flags

      IF (VIS_ERROR.OR.NERR.NE.0)      GO TO 1

c === if any of the parameters have changed, call initialization routines
c     NOTE : consider setting a new root file name each time a set of
c     parameters is changed: otherwise some files may be overwritten

      IF (NEW) THEN
         CALL VIS_OFF_SET(VIS_NVARS,VIS_OFFSETS)

         CALL VIS_NAME_SET(VIS_NVARS,VIS_NAMELEN,VIS_VARNAMES)
         CALL VIS_VNODAL_SET(VIS_NVARS,VIS_VAL_NODAL)

C         IF(IFPV3)
C     &        CALL VIS_PV3_SET
C     &        (VIS_NVARS,PV3_KEY,PV3_LIM1,PV3_LIM2)

         CALL VIS_INFO_SET(NUMPRC,MYPRC,
     &        10,201,205,205)
         CALL VIS_FNAME_SET(
     &        VIS_FSTYLE_FLAG,VISFLAG,
     &        VIS_FNAME,VIS_DIR)

         IF (IFNEW_FLAG.EQ.1) IFNEW_FLAG =0
      ENDIF

 1    CONTINUE
C ====================== DEBUG OUTPUT
      IF (VIS_ERROR.OR.NERR.NE.0) THEN
         IF(MYPRC.EQ.0) then
         WRITE(NFOUT,*) '--------------------------------------'
         WRITE(NFOUT,*) 'WARNING :'
         WRITE(NFOUT,*) 'VISUALIZATION ERRcode:' ,NERR, VIS_ERROR
         WRITE(NFOUT,*) 'FOR VISFLAG: ', VISFLAG
         WRITE(NFOUT,*) 'VIS. FILE NAME=',VIS_FNAME
         WRITE(NFOUT,*) 'VIS. FILE STYLE=',VIS_FSTYLE,' AND FLAG=',
     &        VIS_FSTYLE_FLAG
         WRITE(NFOUT,*) 'WITH NUMBER OF VISUALIZATION VARIABLES =',
     &        VIS_NVARS
         WRITE(NFOUT,*) 'VISVARIABLES:'
         WRITE(NFOUT,*) 'IPARS NAME -> VIS NAME'
         DO I =1, VIS_NVARS
            WRITE(NFOUT,*) VIS_IPARS_NAMES(I), ' -> ', VIS_VARNAMES(I),
     &           ' OFFSET =', VIS_OFFSETS(I)
         ENDDO
         WRITE(NFOUT,*) 'VISUALIZATION ERRORS IGNORED.',
     &        'EXECUTION CONTINUED AT TIME STEP=',NSTEP,' TIME=',TIM
         WRITE(NFOUT,*) '--------------------------------------'
         ENDIF
      ENDIF

      END


c ====================================================================
      SUBROUTINE GET_VISVARS(NERR,NEW)
c ----------------------------------
c gets the keyword names of the visualization variables
c from the data file, uses model dependent routines to get
c their IPARS pointers
c --------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NERR
      LOGICAL NEW

      INCLUDE 'control.h'
      INCLUDE 'visual.h'
C --------------------------------------- LOCAL VARIABLES
      INTEGER I,NDUM
      LOGICAL SCALAR
      PARAMETER (SCALAR=.TRUE.)
      CHARACTER*20 VSCL ( MAXVISVARS )
      CHARACTER*20 VVEC ( MAXVISVARS )

C -----------------------------------------------------------
      DO I=1, MAXVISVARS
         VSCL (I) = " "
         VVEC (I) = " "
      ENDDO

c ========= get the names of the scalars, if there are any

      CALL GETVALS('VIS_SCL_NAMES ',
     &    VSCL,'CS',vis_scl,0,0,20,NDUM,NERR)

      IF (NDUM.GT.0) THEN
         NEW = .TRUE.
c===  and translate them: set
c
c VIS_VARNAMES: the names that will be put in the Tecplot file
c VIS_IPARS_NAMES: the names that are "official" to IPARS
c VIS_OFFSETS: the offsets as decided in IPARS

         FIRSTVAR=.TRUE.

         DO I=1,VIS_SCL

            MODACT = 15
            IF (MODELON(15)) THEN
               CALL EVIS_TRANSL(SCALAR,I,VSCL(I))
               GOTO 1
            ENDIF

C            MODACT=14
C            IF (MODELON(14)) THEN
C               CALL TRVIS_TRANSL(SCALAR,I,VSCL(I))
C               GOTO 1
C            ENDIF

C            MODACT=7
C            IF (MODELON(7)) THEN
C               CALL MVIS_TRANSL(SCALAR,I,VSCL(I))
C               GOTO 1
C            ENDIF

C            MODACT=2
C            IF(MODELON(2)) CALL IVIS_TRANSL(SCALAR,I,VSCL(I))
C            MODACT=3
C            IF(MODELON(3)) CALL XVIS_TRANSL(SCALAR,I,VSCL(I))
C            MODACT=16
C            IF(MODELON(16)) CALL XVIS_TRANSL(SCALAR,I,VSCL(I))
C            MODACT=5
C            IF(MODELON(5)) CALL HVIS_TRANSL(SCALAR,I,VSCL(I))
C            MODACT=19
C            IF(MODELON(19)) CALL HVIS_TRANSL(SCALAR,I,VSCL(I))
C            MODACT=18
C            IF(MODELON(18)) CALL HVIS_TRANSL(SCALAR,I,VSCL(I))
C            MODACT=13
C            IF(MODELON(13)) CALL TVIS_TRANSL(SCALAR,I,VSCL(I))
            MODACT=17
            IF(MODELON(17)) CALL TVIS_TRANSL(SCALAR,I,VSCL(I))

    1       MODACT=0

            IF(FIRSTVAR) FIRSTVAR=.FALSE.

         ENDDO
      ENDIF

c ========= get the names of the vectors, if any

      CALL GETVALS('VIS_VEC_NAMES ',
     &    VVEC,'CS',VIS_NVEC,0,0,20,NDUM,NERR)
      IF (NDUM.GT.0) THEN
         NEW = .TRUE.

c====     and translate them: set
c
c VIS_VARNAMES: the names that will be put in the Tecplot file
c VIS_IPARS_NAMES: the names that are "official" to IPARS
c VIS_OFFSETS: the offsets as decided in IPARS
c N_VIS_VARS: IPARS grid array numbers for the variables

         DO I=1,VIS_NVEC

            MODACT=15
            IF (MODELON(15)) THEN
               CALL EVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
               GOTO 2
            ENDIF

C            MODACT=14
C            IF (MODELON(14)) THEN
C               CALL TRVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C               GOTO 2
C            ENDIF

C            MODACT=7
C            IF (MODELON(7)) THEN
C               CALL MVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C               GOTO 1
C            ENDIF

C            MODACT=2
C            IF(MODELON(2)) CALL IVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C            MODACT=3
C            IF(MODELON(3)) CALL XVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C            MODACT=16
C            IF(MODELON(16)) CALL XVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C            MODACT=5
C            IF(MODELON(5)) CALL HVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C            MODACT=19
C            IF(MODELON(19)) CALL HVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C            MODACT=18
C            IF(MODELON(18)) CALL HVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
C            MODACT=13
C            IF(MODELON(13)) CALL TVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))
            MODACT=17
            IF(MODELON(17)) CALL TVIS_TRANSL(.NOT.SCALAR,I,VVEC(I))

    2 MODACT=0

            CALL VIS_SETVEC_NAM(I,VVEC(I))

         ENDDO
      ENDIF

      END

c ----------------------------------------------------------------
      SUBROUTINE VIS_ARYNUM (NUM)
c ===============================================================
c finds the IPARS pointer corresponding to the visualization variable
c of number <num>
c uses get_arynum to query IPARS C structures for variable of that
c name
c if something goes wrong, sets the pointer to the dummy variable
c ===============================================================
      IMPLICIT NONE
      INTEGER NUM,NUM2

      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INTEGER NUMARY,NDIM4,NERR
      CHARACTER*20 TMP

c ----------------------------------------------------------------------
c == get the number of the grid array,if available for the current model
      NUMARY = 0
      NERR = 0
      TMP=VIS_IPARS_NAMES(NUM)

      CALL GET_ARYNUM(NDIM4,NUMARY,NERR,
     &     TMP,20)

c     write(*,*) current_model,
c     & ' vis_arynum called ',num,' ',vis_ipars_names(num),
c     & ' ',numary,nerr,vis_offsets(num)

C     === IF NUMARY FOUND OK, CHECK THE OFFSETS: IF NOT OK, USE 0

      IF(NERR.EQ.0) THEN

         N_VIS_VARS (NUM,CURRENT_MODEL) = NUMARY
c         write(*,*) "setting n_vis_vars, for array, ",num," cur_mod =",
c     &               current_model," is, ",n_vis_vars(num,current_model)

cmpesz: use the other one for trchem
         IF(VIS_OFFSETS(NUM).LT.0.OR.
     &        VIS_OFFSETS(NUM).GT.NDIM4) THEN
            VIS_OFFSETS(NUM) = 0
         ENDIF
      ELSE

c bag8 - do we need bins hack or not?
cbw   fix for coupling with POROELASTIC MODEL, need to wait driving
cbw   model to search for the array again
c      N_VIS_VARS (NUM,CURRENT_MODEL) = N_VISDUM
c      VIS_OFFSETS(NUM) = 0
c      IF (MODACT .NE. CURRENT_MODEL) THEN
c         NERR = 0
c         RETURN
c      ENDIF
cbw

C==   IF SOMETHING WENT WRONG, THEN USE POINTER TO VIS_DUMM BUT DO NOT
C     STOP THE COMPUTATIONS ONLY PRINT INFORMATION TO STDOUT FILE

         IF(MYPRC.EQ.0) THEN
         WRITE(NFOUT,*) 'VISUALIZATION ERRcode:',NERR
         WRITE(NFOUT,*) ' VARIABLE: ',NUM,' ',
     &        VIS_IPARS_NAMES(NUM),' NOT FOUND FOR MODEL ',
     &        CURRENT_MODEL
         WRITE(NFOUT,*) 'VISDUM USED INSTEAD. EXECUTION CONTINUED.'
         ENDIF

c bag8 - do we need bins hack or not?
cbw         N_VIS_VARS (NUM,CURRENT_MODEL) = N_VISDUM
cbw         VIS_OFFSETS(NUM) = 0

      ENDIF

c     write(*,*) 'wrote for model=',current_model,' var ',num,
c     &          ' grid arynum=',n_vis_vars(num,current_model)

      END

c ===============================================================
      SUBROUTINE VIS_ARYNUMMB (NUM,NUM2)
c ===============================================================
c finds the IPARS pointer corresponding to the visualization variable
c of number <num>
c uses get_arynum to query IPARS C structures for variable of that
c name
c if something goes wrong, sets the pointer to the dummy variable
c ===============================================================

      IMPLICIT NONE
      INTEGER NUM,NUM2

      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INTEGER NUMARY,NDIM4,NERR
      CHARACTER*20 TMP
      CHARACTER*15 TMP1

c == get the number of the grid array,if available for the current model

      NUMARY = 0
      NERR = 0
      TMP=VIS_IPARS_NAMES(NUM)
      !GIVEN IN GVISUAL.H & EVISUAL.H RESPECTIVELY!

      CALL GET_ARYNUM(NDIM4,NUMARY,NERR,TMP,20)
      !NO GRID BLOCK GRANULARITY HERE!!
      !NUMARY IS GLOBAL ARRAY NUMBER!!

C == IF NUMARY FOUND OK, CHECK THE OFFSETS: IF NOT OK, USE 0

      CURRENT_MODEL = MODACT
      !ADDED THIS FOR MULTIBLOCK VISUALIZATION!

      IF(NERR.EQ.0) THEN

         N_VIS_VARS (NUM2,CURRENT_MODEL) = NUMARY
         !CHANGED FROM NUM TO NUM2 IN LIEU OF MULTIBLOCK PROBLEM!

cmpesz: use the other one for trchem

         IF(VIS_OFFSETS(NUM).LT.0.OR.
     &        VIS_OFFSETS(NUM).GT.NDIM4) THEN
            VIS_OFFSETS(NUM) = 0
         ENDIF
      ELSE

C == IF SOMETHING WENT WRONG, THEN USE POINTER TO VIS_DUMM BUT DO NOT
C    STOP THE COMPUTATIONS ONLY PRINT INFORMATION TO STDOUT FILE

         IF(MYPRC.EQ.0) THEN
         WRITE(NFOUT,*) 'VISUALIZATION ERRcode:',NERR
         WRITE(NFOUT,*) ' VARIABLE: ',NUM,' ',
     &        VIS_IPARS_NAMES(NUM),' NOT FOUND FOR MODEL ',
     &        CURRENT_MODEL
         WRITE(NFOUT,*) 'VISDUM USED INSTEAD. EXECUTION CONTINUED.'
         ENDIF

      ENDIF

      END

C ****************************************************************
      SUBROUTINE VIS_TIME ( NERR )
C ===============================================================
c appends current time information (nsteps, time) to
c the file Vis.tim
c sets the current tstep informnation for use by the C vis routines
c ===============================================================
      IMPLICIT NONE
      INTEGER NERR,IERR
      INCLUDE 'control.h'
      INCLUDE 'visual.h'

      CHARACTER*50 VISTIM

C THE NFTIM BELOW COULD BE CHANGED AND THE FILE OPEINING DEALT WITH IN DRIVER
      INTEGER NFTIM,LT
      DATA NFTIM / 15/

      LOGICAL ONCEONLY
      DATA ONCEONLY /.TRUE./

      IF (MYPRC.EQ.0) THEN

c bag8,gp - support for dirname
         LT = len_trim(VIS_DIR)
!         WRITE(*,*)'LT=',LT
         IF ((LT.LE.1).OR.(LT.GE.50)) THEN
           VISTIM='Vis.tim'
         ELSE
           VISTIM=VIS_DIR(1:LT-1)//'/Vis.tim'
         ENDIF

         IF (ONCEONLY) THEN
            ONCEONLY = .FALSE.
            OPEN(NFTIM,FILE=trim(VISTIM),STATUS='UNKNOWN',ERR=13,
     &           IOSTAT=IERR)
            WRITE(NFTIM,*) 'NSTEP        TIME'
         ENDIF
         CLOSE(NFTIM)

      ENDIF

c set the time step: this is necessary for filenames
c and zonenames

      CALL VIS_TSTEP_SET(NSTEP,TIM)
      IF ((VISFLAG.EQ.7).OR.(VISFLAG.EQ.8).OR.(VISFLAG.EQ.10)) THEN
      CALL VIS_PVTU_SET(VIS_SCL,VIS_VEC)
      ENDIF
      RETURN

 13   NERR = NERR +1

      WRITE(*,*) 'PROBLEMS WITH FILE Vis.tim, IOSTAT=',IERR
      STOP 42

      END

c ****************************************************************
      SUBROUTINE VIS_OUTPUT ()
C ===============================================================
C CALLS THE VISUALIZATION C ROUTINE VIS_TECOUTPUT()
C ===============================================================
      IMPLICIT NONE

C       include 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INCLUDE 'layout.h'
      INCLUDE 'blkary.h'

      INTEGER VISDAT_T(MAXVISVARS+13)      ! TECPLOT OR PV3
      INTEGER VISDAT_BV(MAXVISVARS+10)     ! BRICKS WITH VTK
      INTEGER VISDAT_HV(MAXVISVARS+8)      ! HEXAHEDRA WITH VTK
      INTEGER I,PAYDAT(2)
      LOGICAL ONCEONLY
      DATA ONCEONLY/.TRUE./
c GP: Check if VISFLAG is such that we call any visoutput routine
      LOGICAL CALL_CHECK,VIS
      DATA CALL_CHECK/.TRUE./

      EXTERNAL VIS_TECOUTPUT
C      EXTERNAL VIS_PV3OUTPUT
      EXTERNAL VIS_VTKOUTPUT
      EXTERNAL VIS_VTKOUTPUT_MPFA  ! gp:  ??

C -----------------------------------
C SET THE ARGUMENTS TO THE CALL WORK ROUTINE
C GP NOTE: the lists with variables to visualize could be different for
c          each call to vis_output, so we should NOT fill them just once !!

      IF (MBPOROE) THEN ! SAUMIK,BGANIS
         ! SET N_VIS_SCL BASED ON WHETHER POROHEX OR FLOW CALLS VIS_OUTPUT
         CURRENT_MODEL = MODACT
         IF (CURRENT_MODEL.EQ.15) THEN
            N_VIS_SCL = N_VIS_SCL_POROHEX
            VIS_NVARS = VIS_SCL_POROHEX
         ELSE
            N_VIS_SCL = N_VIS_SCL_FLOW(MODACT)
            VIS_NVARS = VIS_SCL_FLOW(MODACT)
         ENDIF
      ENDIF

            VISDAT_BV = 0
            VISDAT_BV(1)=10+VIS_NVARS
            VISDAT_BV(2)=N_VISFLAG
            VISDAT_BV(3)=N_VIS_BINARY
            VISDAT_BV(4)=N_VIS_XREC
            VISDAT_BV(5)=N_VIS_YREC
            VISDAT_BV(6)=N_VIS_ZREC
            VISDAT_BV(7)=N_VIS_DXREC
            VISDAT_BV(8)=N_VIS_DYREC
            VISDAT_BV(9)=N_VIS_DZREC
            VISDAT_BV(10)=N_VIS_SCL
            VISDAT_BV(11)=N_VIS_VEC
            DO I=1,VIS_NVARS
               VISDAT_BV(11+I) = N_VIS_VARS(I,CURRENT_MODEL)
            ENDDO

            VISDAT_HV = 0
            VISDAT_HV(1)=7+VIS_NVARS
            VISDAT_HV(2)=N_VISFLAG
            VISDAT_HV(3)=N_VIS_BINARY
            VISDAT_HV(4)=N_XC
            VISDAT_HV(5)=N_YC
            VISDAT_HV(6)=N_ZC
            VISDAT_HV(7)=N_VIS_SCL
            VISDAT_HV(8)=N_VIS_VEC
            DO I=1,VIS_NVARS
               VISDAT_HV(8+I) = N_VIS_VARS(I,CURRENT_MODEL)
            ENDDO

            VISDAT_T = 0
            VISDAT_T(1)=12+VIS_NVARS
            VISDAT_T(2)=N_VISFLAG
            VISDAT_T(3)=N_VIS_XREC
            VISDAT_T(4)=N_VIS_YREC
            VISDAT_T(5)=N_VIS_ZREC
            VISDAT_T(6)=N_VIS_DXREC
            VISDAT_T(7)=N_VIS_DYREC
            VISDAT_T(8)=N_VIS_DZREC
            VISDAT_T(9)=N_XC
            VISDAT_T(10)=N_YC
            VISDAT_T(11)=N_ZC
            VISDAT_T(12)=N_VIS_SCL
            VISDAT_T(13)=N_VIS_VEC
            DO I=1,VIS_NVARS
               VISDAT_T(13+I) = N_VIS_VARS(I,CURRENT_MODEL)
            ENDDO

C        CALL CALLWORK(VIS_PV3OUTPUT, VISDAT_T)   ! obsolete
C        GOTO 100

c GP: Check if we call any of the visoutput routines

      IF (CALL_CHECK) THEN
c         CALL_CHECK = .FALSE.
         IF(.NOT.MBPOROE) CALL_CHECK = .FALSE.
         ! SAUMIK,BGANIS - IF MBPOROE IS TRUE, VIS_OUTPUT NEEDS
         ! TO BE CALLED TWICE, ONCE FOR POROHEX AND ONCE FOR FLOW
         VIS = .FALSE.
         IF (VISFLAG.EQ.7) THEN
            VIS = .TRUE.
         ELSEIF (VISFLAG.EQ.8) THEN
            VIS = .TRUE.
         ELSE
            VIS = .TRUE.
         ENDIF
         IF (.NOT.VIS) STOP 'Wrong VISFLAG'
      ENDIF

      IF (VISFLAG.EQ.7) THEN
        CALL VTKCALLWORK(VIS_VTKOUTPUT, VISDAT_BV)
      ELSEIF (VISFLAG.EQ.8) THEN
        CALL VTKCALLWORK(VIS_VTKOUTPUT_MPFA, VISDAT_HV)
      ELSEIF (VISFLAG.EQ.10) THEN
        IF (KNDGRD.EQ.3) THEN
          CALL VTKCALLWORK(VIS_VTKOUTPUT_MPFA, VISDAT_HV)
        ELSE
          CALL VTKCALLWORK(VIS_VTKOUTPUT, VISDAT_BV)
        ENDIF
        CALL CALLWORK(VIS_TECOUTPUT, VISDAT_T)
      ELSE
        CALL CALLWORK(VIS_TECOUTPUT, VISDAT_T)
      ENDIF

 100  CONTINUE
      END

C ---------------------------------------------------------------
      SUBROUTINE VISINITOUT()
C ========================
C CALL VISOUT PROVIDED THIS IS REQUESTED IN THE DATA FILE
C
      INCLUDE 'visual.h'
      integer viserr

      viserr = 0
      IF ( IFVISINIT ) CALL VISOUT(VISERR)

      END
C ---------------------------------------------------------------
      SUBROUTINE PERMOUT()
C ========================
C CALL VISOUT PROVIDED THIS IS REQUESTED IN THE DATA FILE
C
      INTEGER NERR,JDUM(3),NDIR,N_NDIR,NARG(7)

C       include 'msjunk.h'

      include 'visual.h'
      include 'blkary.h'

      EXTERNAL PERMCPY
      EXTERNAL WELLVIS
c------------------------------------
      IF (IFPERM) THEN
C         WRITE(*,*) 'PERM OUTPUT REQUESTED'
c copy the real*4 perms to real*8 tcofx etc. Note that
c this is done before tcofx has the meaning of transmissabilities
         NARG(1)=6
         NARG(2)=N_TCOFX
         NARG(3)=N_TCOFY
         NARG(4)=N_TCOFZ
         NARG(5)=N_XPERM
         NARG(6)=N_YPERM
         NARG(7)=N_ZPERM

         CALL CALLWORK(PERMCPY,NARG)
      ENDIF
      IF (IFWELL) THEN
C         WRITE(*,*) 'WELL LOCATION OUTPUT REQUESTED'
         CALL PNTVAR(NDIR,N_NDIR,NERR)
         JDUM(1) = 2
         JDUM(2) = N_VISDUM
         JDUM(3) = N_NDIR
         NDIR = 1
         CALL CALLWORK(WELLVIS,JDUM)
      ENDIF

      IF (IFPERM.OR.IFWELL) THEN
         NERR = 0
         CALL VISOUT(NERR)
      ENDIF

      END

C*********************************************************************
      SUBROUTINE PERMCPY (IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,KL2,
     &                   KEYOUT,NBLK,TX,TY,TZ,PX,PY,PZ)
C*********************************************************************

C  Copy perms to tcofs
C  This is a work routine.

C  TX(I,J,K) = permeaibility (OUTPUT, REAL*8)
C  TY(I,J,K)
C  TZ(I,J,K)

C  PX(I,J,K) = Permeability (INPUT AND OUTPUT, REAL*4)
C  PY(I,J,K)   (0 is put in keyed out elements)
C  PZ(I,J,K)
C*********************************************************************
C      INCLUDE 'msjunk.h'

      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*8 TX(IDIM,JDIM,KDIM),TY(IDIM,JDIM,KDIM),TZ(IDIM,JDIM,KDIM)
      REAL*4 PX(IDIM,JDIM,KDIM),PY(IDIM,JDIM,KDIM),PZ(IDIM,JDIM,KDIM)

C  COPY PERMEABILITIES TO TRANSMISSABILITY COEFFICIENT CONSTANT

      DO 2 K=KL1,KL2
         JL1=JL1V(K)
         JL2=JL2V(K)
         DO 2 J=JL1,JL2
            DO 2 I=IL1,IL2
               TX(I,J,K)=PX(I,J,K)
               TY(I,J,K)=PY(I,J,K)
               TZ(I,J,K)=PZ(I,J,K)
 2    CONTINUE

      END

c ----------------------------------------------------------------
