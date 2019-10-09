!----------------------------------------------------------------------
! visual9.df - modern tecplot output for bricks and hexahedral grids,
!              no cumbersome postprocessing required, supports binary.
! Author: Ben Ganis
! bganis@ices.utexas.edu
! 6/21/16
!----------------------------------------------------------------------

      MODULE tecbinmod
      IMPLICIT NONE
      SAVE

      LOGICAL :: TECBIN
      LOGICAL :: TECDBG = .FALSE.
      INTEGER :: TECUNIT = 20
      INTEGER*4, ALLOCATABLE :: PASSIVEVARLIST(:),VALUELOCATION(:),
     &                          SHAREVARFROMZONE(:)

      TYPE tecbinblk
        INTEGER :: NUMELE,NUMNOD,MAXNUM
        INTEGER, ALLOCATABLE :: NODNUM(:,:,:),NOD2IJK(:,:)
        DOUBLE PRECISION, ALLOCATABLE :: ARR(:)
        INTEGER*4, ALLOCATABLE :: CONNECT(:,:)
      END TYPE

      TYPE (tecbinblk), ALLOCATABLE :: TECBLK(:)

      END MODULE

!----------------------------------------------------------------------
      SUBROUTINE VIS_TEC_GRID1(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,
     &     JL2V,KL1,KL2,KEYOUT,NBLK,XC,YC,ZC,NVARS)
!----------------------------------------------------------------------
! Opens the visual output file and
! writes file header, zone headers, and coordinates.
!----------------------------------------------------------------------
      USE tecbinmod
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'visual.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V(KDIM),JL2V(KDIM),
     &        KL1,KL2,KEYOUT(IDIM,JDIM,KDIM),NBLK,NVARS
      REAL*8  XC(IDIM+1,JDIM+1,KDIM+1),YC(IDIM+1,JDIM+1,KDIM+1),
     &        ZC(IDIM+1,JDIM+1,KDIM+1)
!----------------------------------------------------------------------
      CHARACTER*60 BLOCKNAME
      CHARACTER TECNAME*(3*50)
      INTEGER IOFF,JOFF,KOFF
      INTEGER IERR,LT,LT2,I,J,K,L,II,JJ,KK,N,CTR
      INTEGER, SAVE :: FIRSTBLK=0,ZNUM=0
      LOGICAL HASCC

      INTEGER*4 :: FILETYPE=0,DEBUG=0,ISDOUBLE=1,ZONETYPE=5,ISBLOCK=1
      CHARACTER*256 :: TITLE,SCRATCHDIR,ZONETITLE
      CHARACTER*2048 :: VARIABLES
      INTEGER*4 :: SHARECONNECTIVITY
      INTEGER*4 TECINI112,TECZNE112,TECDAT112

! Offset for cell center to 8 hex vertices
      INTEGER OFFSET1(3,8)
      DATA OFFSET1/0,0,0, 1,0,0, 1,1,0, 0,1,0,
     &             0,0,1, 1,0,1, 1,1,1, 0,1,1/
!----------------------------------------------------------------------

      TECBIN = VIS_BINARY
      IF (.FALSE.) THEN
          IF (TECBIN.AND.(MYPRC.EQ.0))
     &      WRITE(*,*)'VIS_BINARY = TRUE was ignored.'
          TECBIN = .FALSE.
!          STOP 'Need to compile with vistecbin.mak, '//
!     &       'or set VIS_BINARY = FALSE.'
      ENDIF

      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,IERR)

! Keep track of local zone number
      IF (FIRSTBLK.EQ.0) FIRSTBLK=NBLK
      IF (NBLK.EQ.FIRSTBLK) THEN
        ZNUM=0
        IF ((TECEXISTS).AND.(.NOT.ADAPTIVITY)) ALLZONES=.TRUE.
      ENDIF
      ZNUM=ZNUM+1

      IF (TECDBG) THEN
        WRITE(*,*)'In VIS_TEC_GRID1, NBLK=',NBLK
        WRITE(*,*)'TECBIN = ',TECBIN
        WRITE(*,*)'TECEXISTS = ',TECEXISTS
        WRITE(*,*)'ALLZONES = ',ALLZONES
        WRITE(*,*)'FIRSTBLK = ',FIRSTBLK
        WRITE(*,*)'ZNUM = ',ZNUM
      ENDIF

! Count number of elements and nodes, since they aren't saved
! as IPARS grid element arrays (not too expensive...)
      IF (.NOT.ALLOCATED(TECBLK)) THEN
        ALLOCATE(TECBLK(NUMBLK),STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate TECBLK'
      ENDIF

      IF (.NOT.ALLOCATED(TECBLK(NBLK)%NODNUM)) THEN
        ALLOCATE(TECBLK(NBLK)%NODNUM(IDIM+1,JDIM+1,KDIM+1),STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate NODNUM'
        TECBLK(NBLK)%NODNUM=0
        ALLOCATE(TECBLK(NBLK)%NOD2IJK((IDIM+1)*(JDIM+1)*(KDIM+1),3),
     &    STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate NOD2IJK'
        TECBLK(NBLK)%NOD2IJK=0
        TECBLK(NBLK)%NUMELE=0
        TECBLK(NBLK)%NUMNOD=0
        DO K = KL1,KL2
        DO J = JL1V(K),JL2V(K)
        DO I = IL1,IL2
        IF (KEYOUT(I,J,K).EQ.1) THEN
        TECBLK(NBLK)%NUMELE=TECBLK(NBLK)%NUMELE+1
        DO L=1,8
          II = I + OFFSET1(1,L)
          JJ = J + OFFSET1(2,L)
          KK = K + OFFSET1(3,L)
          IF (TECBLK(NBLK)%NODNUM(II,JJ,KK).EQ.0) THEN
            TECBLK(NBLK)%NUMNOD=TECBLK(NBLK)%NUMNOD+1
            TECBLK(NBLK)%NODNUM(II,JJ,KK)=TECBLK(NBLK)%NUMNOD
            TECBLK(NBLK)%NOD2IJK(TECBLK(NBLK)%NUMNOD,1:3)=[II,JJ,KK]
          ENDIF
        ENDDO
        ENDIF
        ENDDO
        ENDDO
        ENDDO

        TECBLK(NBLK)%MAXNUM=MAX(TECBLK(NBLK)%NUMNOD,TECBLK(NBLK)%NUMELE)
        ALLOCATE(TECBLK(NBLK)%ARR(TECBLK(NBLK)%MAXNUM),STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate ARR'

        IF (TECDBG) THEN
          WRITE(*,*)'NBLK=',NBLK
          WRITE(*,*)'NUMNOD=',TECBLK(NBLK)%NUMNOD
          WRITE(*,*)'NUMELE=',TECBLK(NBLK)%NUMELE
        ENDIF
      ENDIF

! Skip output if empty fault block
      IF (TECBLK(NBLK)%NUMNOD.EQ.0.OR.TECBLK(NBLK)%NUMELE.EQ.0) THEN
        IF (TECDBG) WRITE(*,*)'Empty Zone, MYPRC,NBLK=',MYPRC,NBLK
        RETURN
      ENDIF

! Arrays for binary output
      IF (TECBIN.AND..NOT.ALLOCATED(PASSIVEVARLIST)) THEN
        ALLOCATE(PASSIVEVARLIST(3+NVARS),STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate PASSIVEVARLIST'
        ALLOCATE(VALUELOCATION(3+NVARS),STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate VALUELOCATION'
        ALLOCATE(SHAREVARFROMZONE(3+NVARS),STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate SHAREVARFROMZONE'
      ENDIF

! Form TECNAME
        LT = len_trim(VIS_DIR)
        IF ((LT.LE.1).OR.(LT.GE.50)) THEN
          LT = len_trim(VIS_FNAME)
          WRITE(TECNAME,'(2A,I4.4,A)')
     &      TRIM(VIS_FNAME(1:LT-1)),
     &      '_p',MYPRC,'.plt'
        ELSE
          LT = len_trim(VIS_FNAME)
          LT2 = len_trim(VIS_DIR)
          WRITE(TECNAME,'(4A,I4.4,A)')
     &      TRIM(VIS_DIR(1:LT2-1)),'/',TRIM(VIS_FNAME(1:LT-1)),
     &      '_p',MYPRC,'.plt'
        ENDIF

! If writing to visual output for the first time, create new file
      IF (.NOT.TECEXISTS) THEN
        VARIABLES = 'X,Y,Z'
        DO I=1,NVARS
          VARIABLES = TRIM(VARIABLES) // ',' //
     &                TRIM(VIS_VARNAMES(I))
        ENDDO
        IF (TECDBG) WRITE(*,*)'VARIABLES=',TRIM(VARIABLES)

        IF (.NOT.TECBIN) THEN
          IF (TECDBG) WRITE(*,*)'TECNAME = ',TRIM(TECNAME)
          OPEN(TECUNIT,file=TRIM(TECNAME),STATUS='unknown')
          WRITE(TECUNIT,'(3A)')'TITLE = "',TRIM(DATTL(1)),'"'
          WRITE(TECUNIT,'(A,$)')'VARIABLES = ',TRIM(VARIABLES)
          WRITE(TECUNIT,*)
        ELSE
          SCRATCHDIR = '/tmp' // CHAR(0)
          VARIABLES = TRIM(VARIABLES) // CHAR(0)
          TECNAME = TRIM(TECNAME) // CHAR(0)
          IF (TECDBG) WRITE(*,*)'TECNAME = ',TRIM(TECNAME)
          IERR = TECINI112(TRIM(DATTL(1)),TRIM(VARIABLES),
     &      TRIM(TECNAME),TRIM(SCRATCHDIR),FILETYPE,DEBUG,
     &      ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECINI, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECINI112'
        ENDIF
! Else if writing to visual output on subsequent time, append
      ELSE
        IF (.NOT.TECBIN)
     &    OPEN(TECUNIT,file=TRIM(TECNAME),STATUS='old',ACCESS='append')
      ENDIF

      CALL BLKNAME(NBLK,BLOCKNAME,IERR)
      HASCC=.FALSE.
      DO I=1,NVARS
        IF (VIS_VAL_NODAL(I).EQ.0) THEN
          HASCC=.TRUE.
          EXIT
        ENDIF
      ENDDO

      IF (.NOT.TECBIN) THEN
        WRITE(TECUNIT,*)
        WRITE(TECUNIT,'(3A)')'ZONE T="',TRIM(BLOCKNAME),'"'
        WRITE(TECUNIT,'(1P,A,E15.6)')'SOLUTIONTIME=',TIM
        WRITE(TECUNIT,'(A,I)')'STRANDID=',NBLK
        WRITE(TECUNIT,'(A)')'ZONETYPE=FEBRICK'
        WRITE(TECUNIT,'(A)')'DATAPACKING=BLOCK'
        WRITE(TECUNIT,'(A,I)')'NODES=',TECBLK(NBLK)%NUMNOD
        WRITE(TECUNIT,'(A,I)')'ELEMENTS=',TECBLK(NBLK)%NUMELE
        IF (HASCC) THEN
          WRITE(TECUNIT,'(A,$)')'VARLOCATION=(['
          CTR=0
          DO I=1,NVARS
            IF (VIS_VAL_NODAL(I).EQ.0) THEN
              IF (CTR.NE.0) WRITE(TECUNIT,'(A,$)')','
              IF (3+I.LT.10) THEN
                WRITE(TECUNIT,'(I1,$)')3+I
              ELSEIF (3+I.LT.100) THEN
                WRITE(TECUNIT,'(I2,$)')3+I
              ELSE
                WRITE(TECUNIT,'(I3,$)')3+I
              ENDIF
              CTR=CTR+1
            ENDIF
          ENDDO
          WRITE(TECUNIT,'(A)')']=CELLCENTERED)'
        ENDIF
        IF (ALLZONES) THEN
          WRITE(TECUNIT,'(A,I)')'CONNECTIVITYSHAREZONE=',ZNUM
          WRITE(TECUNIT,'(A,I,A)')'VARSHARELIST=([1-3]=',ZNUM,')'
        ENDIF
      ELSE
        VALUELOCATION = 1
        IF (HASCC) THEN
          DO I=1,NVARS
            IF (VIS_VAL_NODAL(I).EQ.0) VALUELOCATION(3+I)=0
          ENDDO
        ENDIF
        PASSIVEVARLIST = 0
        SHARECONNECTIVITY = 0
        SHAREVARFROMZONE = 0
        IF (ALLZONES) THEN
          SHARECONNECTIVITY = ZNUM
          SHAREVARFROMZONE(1:3) = ZNUM
        ENDIF
        IF (TECDBG) THEN
          WRITE(*,*)'BLOCKNAME=',TRIM(BLOCKNAME)
          WRITE(*,*)'ZONETYPE=',ZONETYPE
          WRITE(*,*)'NUMNOD=',TECBLK(NBLK)%NUMNOD
          WRITE(*,*)'NUMELE=',TECBLK(NBLK)%NUMELE
          WRITE(*,*)'TIM=',TIM
          WRITE(*,*)'NBLK=',NBLK
          WRITE(*,*)'ISBLOCK=',ISBLOCK
          WRITE(*,*)'PASSIVEVARLIST=',PASSIVEVARLIST
          WRITE(*,*)'VALUELOCATION=',VALUELOCATION
          WRITE(*,*)'SHAREVARFROMZONE=',SHAREVARFROMZONE
          WRITE(*,*)'SHARECONNECTIVITY=',SHARECONNECTIVITY
        ENDIF
        IERR = TECZNE112(TRIM(BLOCKNAME),ZONETYPE,
     &           TECBLK(NBLK)%NUMNOD,TECBLK(NBLK)%NUMELE,
     &           0,0,0,0,TIM,NBLK,0,ISBLOCK,0,0,0,0,0,
     &           PASSIVEVARLIST,VALUELOCATION,SHAREVARFROMZONE,
     &           SHARECONNECTIVITY)
        IF (TECDBG) WRITE(*,*)'After TECZNE, IERR=',IERR
        IF (IERR.NE.0) STOP 'Error with TECZNE112'
      ENDIF

! Output coordinates if first zone
      IF (.NOT.ALLZONES) THEN
        DO I=1,TECBLK(NBLK)%NUMNOD
          II=TECBLK(NBLK)%NOD2IJK(I,1)
          JJ=TECBLK(NBLK)%NOD2IJK(I,2)
          KK=TECBLK(NBLK)%NOD2IJK(I,3)
          IF (KNDGRD.EQ.1) THEN
            TECBLK(NBLK)%ARR(I)=XREC(II+IOFF,NBLK)
          ELSE
            TECBLK(NBLK)%ARR(I)=XC(II,JJ,KK)
          ENDIF
        ENDDO
        IF (TECDBG) WRITE(*,*)'# X'
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(A)')'# X'
          WRITE(TECUNIT,'(1P,4E15.6)')
     &      TECBLK(NBLK)%ARR(1:TECBLK(NBLK)%NUMNOD)
        ELSE
          IERR = TECDAT112(TECBLK(NBLK)%NUMNOD,
     &             TECBLK(NBLK)%ARR,ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECDAT, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECDAT112'
        ENDIF

        DO I=1,TECBLK(NBLK)%NUMNOD
          II=TECBLK(NBLK)%NOD2IJK(I,1)
          JJ=TECBLK(NBLK)%NOD2IJK(I,2)
          KK=TECBLK(NBLK)%NOD2IJK(I,3)
          IF (KNDGRD.EQ.1) THEN
            TECBLK(NBLK)%ARR(I)=YREC(JJ+JOFF,NBLK)
          ELSE
            TECBLK(NBLK)%ARR(I)=YC(II,JJ,KK)
          ENDIF
        ENDDO
        IF (TECDBG) WRITE(*,*)'# Y'
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(A)')'# Y'
          WRITE(TECUNIT,'(1P,4E15.6)')
     &      TECBLK(NBLK)%ARR(1:TECBLK(NBLK)%NUMNOD)
        ELSE
          IERR = TECDAT112(TECBLK(NBLK)%NUMNOD,
     &             TECBLK(NBLK)%ARR,ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECDAT, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECDAT112'
        ENDIF

        DO I=1,TECBLK(NBLK)%NUMNOD
          II=TECBLK(NBLK)%NOD2IJK(I,1)
          JJ=TECBLK(NBLK)%NOD2IJK(I,2)
          KK=TECBLK(NBLK)%NOD2IJK(I,3)
          IF (KNDGRD.EQ.1) THEN
            TECBLK(NBLK)%ARR(I)=ZREC(KK+KOFF,NBLK)
          ELSE
            TECBLK(NBLK)%ARR(I)=ZC(II,JJ,KK)
          ENDIF
        ENDDO
        IF (TECDBG) WRITE(*,*)'# Z'
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(A)')'# Z'
          WRITE(TECUNIT,'(1P,4E15.6)')
     &      TECBLK(NBLK)%ARR(1:TECBLK(NBLK)%NUMNOD)
        ELSE
          IERR = TECDAT112(TECBLK(NBLK)%NUMNOD,
     &             TECBLK(NBLK)%ARR,ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECDAT, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECDAT112'
        ENDIF
      ENDIF

      END

!----------------------------------------------------------------------
      SUBROUTINE VIS_TEC_ARRAY(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,
     &     JL2V,KL1,KL2,KEYOUT,NBLK,IVAR,VAR,IOFF)
!----------------------------------------------------------------------
! Writes one nodal or cell-centered variable in block format.
!----------------------------------------------------------------------
      USE tecbinmod
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V(KDIM),JL2V(KDIM),
     &        KL1,KL2,KEYOUT(IDIM,JDIM,KDIM),NBLK,IVAR,IOFF
      REAL*8  VAR(IDIM,JDIM,KDIM,*)
      INTEGER I,J,K,L,CTR,II,JJ,KK,IERR

      INTEGER*4 :: ISDOUBLE=1
      INTEGER*4 TECDAT112

! Offset for cell center to 8 hex vertices
      INTEGER OFFSET1(3,8)
      DATA OFFSET1/0,0,0, 1,0,0, 1,1,0, 0,1,0,
     &             0,0,1, 1,0,1, 1,1,1, 0,1,1/
!----------------------------------------------------------------------

      IF (TECDBG) WRITE(*,*)'In VIS_TEC_ARRAY, IOFF=',IOFF

! Skip output if empty fault block
      IF (TECBLK(NBLK)%NUMNOD.EQ.0.OR.TECBLK(NBLK)%NUMELE.EQ.0) THEN
        IF (TECDBG) WRITE(*,*)'Empty Zone, MYPRC,NBLK=',MYPRC,NBLK
        RETURN
      ENDIF

      IF (VIS_VAL_NODAL(IVAR).EQ.0) THEN
        CTR=0
        DO K = KL1,KL2
        DO J = JL1V(K),JL2V(K)
        DO I = IL1,IL2
        IF (KEYOUT(I,J,K).EQ.1) THEN
          CTR=CTR+1
          TECBLK(NBLK)%ARR(CTR)=VAR(I,J,K,IOFF)
        ENDIF
        ENDDO
        ENDDO
        ENDDO
        IF (TECDBG) WRITE(*,*)'# ',TRIM(VIS_VARNAMES(IVAR))
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(2A)')'# ',TRIM(VIS_VARNAMES(IVAR))
          WRITE(TECUNIT,'(1P,4E15.6)')
     &      TECBLK(NBLK)%ARR(1:TECBLK(NBLK)%NUMELE)
        ELSE
          IERR = TECDAT112(TECBLK(NBLK)%NUMELE,
     &             TECBLK(NBLK)%ARR,ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECDAT, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECDAT112'
        ENDIF
      ELSE
        DO I = 1,TECBLK(NBLK)%NUMNOD
          II = TECBLK(NBLK)%NOD2IJK(I,1)
          JJ = TECBLK(NBLK)%NOD2IJK(I,2)
          KK = TECBLK(NBLK)%NOD2IJK(I,3)
          TECBLK(NBLK)%ARR(I)=VAR(II,JJ,KK,IOFF)
        ENDDO
        IF (TECDBG) WRITE(*,*)'# ',TRIM(VIS_VARNAMES(IVAR))
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(2A)')'# ',TRIM(VIS_VARNAMES(IVAR))
          WRITE(TECUNIT,'(1P,4E15.6)')
     &      TECBLK(NBLK)%ARR(1:TECBLK(NBLK)%NUMNOD)
        ELSE
          IERR = TECDAT112(TECBLK(NBLK)%NUMNOD,
     &             TECBLK(NBLK)%ARR,ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECDAT, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECDAT112'
        ENDIF
      ENDIF

      END

!----------------------------------------------------------------------
      SUBROUTINE VIS_TEC_ARRAY_R4(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,
     &     JL2V,KL1,KL2,KEYOUT,NBLK,IVAR,VAR,IOFF)
!----------------------------------------------------------------------
! Writes one nodal or cell-centered variable in block format.
!----------------------------------------------------------------------
      USE tecbinmod
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V(KDIM),JL2V(KDIM),
     &        KL1,KL2,KEYOUT(IDIM,JDIM,KDIM),NBLK,IVAR,IOFF
      REAL*4  VAR(IDIM,JDIM,KDIM,*)
      INTEGER I,J,K,L,CTR,II,JJ,KK,IERR

      INTEGER*4 :: ISDOUBLE=1
      INTEGER*4 TECDAT112

! Offset for cell center to 8 hex vertices
      INTEGER OFFSET1(3,8)
      DATA OFFSET1/0,0,0, 1,0,0, 1,1,0, 0,1,0,
     &             0,0,1, 1,0,1, 1,1,1, 0,1,1/
!----------------------------------------------------------------------

      IF (TECDBG) WRITE(*,*)'In VIS_TEC_ARRAY_R4, IOFF=',IOFF

! Skip output if empty fault block
      IF (TECBLK(NBLK)%NUMNOD.EQ.0.OR.TECBLK(NBLK)%NUMELE.EQ.0) THEN
        IF (TECDBG) WRITE(*,*)'Empty Zone, MYPRC,NBLK=',MYPRC,NBLK
        RETURN
      ENDIF

      IF (VIS_VAL_NODAL(IVAR).EQ.0) THEN
        CTR=0
        DO K = KL1,KL2
        DO J = JL1V(K),JL2V(K)
        DO I = IL1,IL2
        IF (KEYOUT(I,J,K).EQ.1) THEN
          CTR=CTR+1
          TECBLK(NBLK)%ARR(CTR)=DBLE(VAR(I,J,K,IOFF))
        ENDIF
        ENDDO
        ENDDO
        ENDDO
        IF (TECDBG) WRITE(*,*)'# ',TRIM(VIS_VARNAMES(IVAR))
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(2A)')'# ',TRIM(VIS_VARNAMES(IVAR))
          WRITE(TECUNIT,'(1P,4E15.6)')
     &      TECBLK(NBLK)%ARR(1:TECBLK(NBLK)%NUMELE)
        ELSE
          IERR = TECDAT112(TECBLK(NBLK)%NUMELE,
     &             TECBLK(NBLK)%ARR,ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECDAT, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECDAT112'
        ENDIF
      ELSE
        DO I = 1,TECBLK(NBLK)%NUMNOD
          II = TECBLK(NBLK)%NOD2IJK(I,1)
          JJ = TECBLK(NBLK)%NOD2IJK(I,2)
          KK = TECBLK(NBLK)%NOD2IJK(I,3)
          TECBLK(NBLK)%ARR(I)=VAR(II,JJ,KK,IOFF)
        ENDDO
        IF (TECDBG) WRITE(*,*)'# ',TRIM(VIS_VARNAMES(IVAR))
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(2A)')'# ',TRIM(VIS_VARNAMES(IVAR))
          WRITE(TECUNIT,'(1P,4E15.6)')
     &      TECBLK(NBLK)%ARR(1:TECBLK(NBLK)%NUMNOD)
        ELSE
          IERR = TECDAT112(TECBLK(NBLK)%NUMNOD,
     &             TECBLK(NBLK)%ARR,ISDOUBLE)
          IF (TECDBG) WRITE(*,*)'After TECDAT, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECDAT112'
        ENDIF
      ENDIF

      END

!----------------------------------------------------------------------
      SUBROUTINE VIS_TEC_GRID2(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,
     &     JL2V,KL1,KL2,KEYOUT,NBLK)
!----------------------------------------------------------------------
! Writes zone connectivity information.
!----------------------------------------------------------------------
      USE tecbinmod
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V(KDIM),JL2V(KDIM),
     &        KL1,KL2,KEYOUT(IDIM,JDIM,KDIM),NBLK,NSCL,NVEC
!----------------------------------------------------------------------
      INTEGER IERR,I,J,K,L,II,JJ,KK,CTR

      INTEGER*4 :: ISDOUBLE=1
      INTEGER*4 TECNOD112

! Offset for cell center to 8 hex vertices
      INTEGER OFFSET1(3,8)
      DATA OFFSET1/0,0,0, 1,0,0, 1,1,0, 0,1,0,
     &             0,0,1, 1,0,1, 1,1,1, 0,1,1/
!----------------------------------------------------------------------

      IF (TECDBG) WRITE(*,*)'In VIS_TEC_GRID2'

! Skip output if empty fault block
      IF (TECBLK(NBLK)%NUMNOD.EQ.0.OR.TECBLK(NBLK)%NUMELE.EQ.0) THEN
        IF (TECDBG) WRITE(*,*)'Empty Zone, MYPRC,NBLK=',MYPRC,NBLK
        RETURN
      ENDIF

      IF (.NOT.ALLZONES) THEN

! Always count number of elements and nodes, since they aren't saved
! as IPARS grid element arrays (not too expensive...)
        ALLOCATE(TECBLK(NBLK)%CONNECT(8,TECBLK(NBLK)%NUMELE),STAT=IERR)
        IF (IERR.NE.0) STOP 'Could not allocate CONNECT'

        CTR=0
        DO K = KL1,KL2
        DO J = JL1V(K),JL2V(K)
        DO I = IL1,IL2
        IF (KEYOUT(I,J,K).EQ.1) THEN
          CTR=CTR+1
          DO L=1,8
            II = I + OFFSET1(1,L)
            JJ = J + OFFSET1(2,L)
            KK = K + OFFSET1(3,L)
            TECBLK(NBLK)%CONNECT(L,CTR)=TECBLK(NBLK)%NODNUM(II,JJ,KK)
          ENDDO
        ENDIF
        ENDDO
        ENDDO
        ENDDO

        IF (TECDBG) WRITE(*,*)'# connectivity'
        IF (.NOT.TECBIN) THEN
          WRITE(TECUNIT,'(A)')'# connectivity'
          WRITE(TECUNIT,'(8I)') TECBLK(NBLK)%CONNECT
        ELSE
          IERR = TECNOD112(TECBLK(NBLK)%CONNECT)
          IF (TECDBG) WRITE(*,*)'After TECNOD, IERR=',IERR
          IF (IERR.NE.0) STOP 'Error with TECNOD112'
        ENDIF

      ENDIF

      IF (.NOT.TECBIN) CLOSE(TECUNIT)
      IF (.NOT.TECEXISTS) TECEXISTS = .TRUE.

      END

!----------------------------------------------------------------------
      SUBROUTINE VIS_TEC_DEALLOC()
!----------------------------------------------------------------------
! Deallocates dynamic tecplot memory.
!----------------------------------------------------------------------
      USE tecbinmod
      IMPLICIT NONE
      INTEGER N,IERR
      INCLUDE 'layout.h'
      INCLUDE 'visual.h'

      IF (TECDBG) WRITE(*,*)'In VIS_TEC_DEALLOC'
      IF (TECEXISTS) THEN
        IF (ALLOCATED(TECBLK)) THEN
          DO N=1,NUMBLK
            IF (ALLOCATED(TECBLK(N)%NODNUM)) THEN
              IF (ALLOCATED(TECBLK(N)%NODNUM))
     &          DEALLOCATE(TECBLK(N)%NODNUM)
              IF (ALLOCATED(TECBLK(N)%NOD2IJK))
     &          DEALLOCATE(TECBLK(N)%NOD2IJK)
              IF (ALLOCATED(TECBLK(N)%ARR))
     &          DEALLOCATE(TECBLK(N)%ARR)
              IF (ALLOCATED(TECBLK(N)%CONNECT))
     &          DEALLOCATE(TECBLK(N)%CONNECT)
            ENDIF
          ENDDO
          DEALLOCATE(TECBLK)
        ENDIF
        IF (TECBIN) THEN
          IF (ALLOCATED(PASSIVEVARLIST))
     &      DEALLOCATE(PASSIVEVARLIST)
          IF (ALLOCATED(VALUELOCATION))
     &      DEALLOCATE(VALUELOCATION)
          IF (ALLOCATED(SHAREVARFROMZONE))
     &      DEALLOCATE(SHAREVARFROMZONE)
        ENDIF
      ENDIF

      END

!----------------------------------------------------------------------
      SUBROUTINE VIS_TEC_CLOSE()
!----------------------------------------------------------------------
! Closes the visual output file.
!----------------------------------------------------------------------
      USE tecbinmod
      IMPLICIT NONE
      INCLUDE 'layout.h'
      INCLUDE 'visual.h'
      INTEGER N,IERR
      INTEGER*4 TECEND

      CALL VIS_TEC_DEALLOC()
      IF (TECDBG) WRITE(*,*)'In VIS_TEC_CLOSE'

      IF (TECEXISTS) THEN
        IF (TECBIN) THEN
          IERR = TECEND()
          IF (TECDBG) WRITE(*,*)'After TECEND'
          IF (IERR.NE.0) STOP 'Error with TECEND'
        ENDIF
      ENDIF

      END
