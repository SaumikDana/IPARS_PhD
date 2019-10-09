C  DIVIDE.F - DIVIDE GRID AMONG PROCESSORS

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE DIVIDE ()

C  CODE HISTORY:

C  JOHN WHEELER      9/15/95    ORIGINAL BETA CODE
C  JOHN WHEELER      7/21/96    IMPROVED SCHEME
C  JOHN WHEELER      1/23/97    ADD COLUMN KEYOUT PATTERN
C  JOHN WHEELER      8/12/98    IMPROVED SCHEME
C  JOHN WHEELER      8/19/99    MULTIMODEL CAPABILITY
C  SUNIL G THOMAS    -/--/09    MAY NEED MODPRC(FMODBLK(),..) AS WELL
C                               AS NETOTFR(FMODBLK(),..) TO KEEP THE
C                               GENERIC MULTIMODEL STRUCTURE WITH AND
C                               WITHOUT COUPLING TO TRCHEM
C  SAUMIK DANA       16/09/15   ADD METHODS FOR DIVIDING ELEMENTS
C                               AMONG MPI PROCESSES USING METIS

C*********************************************************************
      SUBROUTINE DIVIDE ()
C*********************************************************************

C  Divides the grid elements among multiple processors.  Call this
C  routine even though a single proccessor is used.  This routine
C  initiallizes the divsion; do NOT call it to do dynamic load
C  balancing.  This routine forces each active model to have some
C  elements on processor 0.

C  NOTE: On entry to DIVIDE:
C        PRCMAP() = -1 ==> Grid column in the system
C        PRCMAP() = -2 ==> Grid column not in the system
C        On exit from DIVIDE:
C        PRCMAP() >= 0 ==> Processor assignment
C        PRCMAP() <  0 ==> Grid column not in the system

C  NOTE: METHOD = 1 ==> SQUARE AND STRIP ASSIGNMENT OF PROCESSORS
C               = 2 ==> K DIRECTION STRIP ASSIGNMENT OF PROCESSORS
C               = 3 ==> J DIRECTION STRIP ASSIGNMENT OF PROCESSORS
C               = 5 ==> 7-PT ELEMENT PARTITIONING
C               = 6 ==> 27-PT ELEMENT PARTITIONING
C               = 7 ==> 27-PT NODE PARTITIONING
C*********************************************************************
C      INCLUDE 'msjunk.h'

      INCLUDE 'control.h'
      INCLUDE 'layout.h'

      INTEGER NCPB(10),NEP0(10)
      LOGICAL NEWFB,NEWPRC,KDOWN,STP0
      INTEGER M,N,NX,NY,NZ,N0,N0N,NYM,NYMN,KERR,J,K

      IF (LEVELE.AND.BUGKEY(1)) THEN
         WRITE (NFBUG,*)'PROC',MYPRC,' ENTERING SUBROUTINE DIVIDE'
         WRITE (*,*)'PROC',MYPRC,' ENTERING SUBROUTINE DIVIDE'
      ENDIF

C  COUNT THE TOTAL NUMBER OF ACTIVE GRID ELEMENTS AND COLUMNS

      NER=0
      DO 1 N=1,NUMBLK
      MYBLK(N)=.FALSE.
      CALL BLKDIM (N,NX,NY,NZ,KERR)
c++++++++++YURI VASSILEVSKI++++++++++++++++++++++++++
C      CALL DIVIDEINSERT1(N,NX,NY,NZ,NUMBLK,NUMPRC)
c++++++++++++++++++++++++++++++++++++++++++++++++++++
      N0=N0MAP(N)
      NC=0
      DO 2 K=1,NZ
      NN=N0+NY*K
      DO 2 J=1,NY
      IF (PRCMAP(NN+J).EQ.-1) NC=NC+1
    2 CONTINUE
      NCPB(N)=NC
      NER=NER+NC*NX
      NETOTR(MODBLK(N))=NETOTR(MODBLK(N))+NC*NX
    1 CONTINUE

      NETOTR0=NER

C  COUNT THE TOTAL NUMBER OF ACTIVE PHYSICAL MODELS
C  DISTRIBUTE PROCESSOR ZERO AMONG MODELS

      NAM=0
      DO 28 I=1,19
      DO 29 J=1,1024
   29 MODPROC(I,J)=.FALSE.
   28 CONTINUE

      DO 30 I=1,19
      IF(I.EQ.14) CYCLE
      IF (MODELON(I)) NAM=NAM+1
   30 CONTINUE

C      IF (METHOD.GE.5.AND.METHOD.LE.7) THEN
C         CALL IPARS_METIS()
C         GO TO 99
C      ENDIF

      STP0=.FALSE.
      IF(.NOT.MBPOROE) THEN
      ! NOT AN INTERFACE PROBLEM
      ! DO NOT WANT TO BE ADDING THE FLOW ELEMENTS TO THE TOTAL COUNT
      ! AND RE-DISTRIBUTING ELEMENTS AMONG PROCESSES BASED ON THAT
      IF (NAM.GT.1.AND.NUMPRC.GT.1) THEN
         STP0=.TRUE.
         DO 31 I=1,NUMBLK
         NEP0(I)=0
         DO 32 J=1,I-1
         IF (MODBLK(J).EQ.MODBLK(I)) GO TO 31
   32    CONTINUE
         CALL BLKDIM (I,NX,NY,NZ,KERR)
         N=NER/(NX*NAM*NUMPRC)
         IF (N.GT.NCPB(I)) N=NCPB(I)
         IF (N.LT.1) N=1
         NEP0(I)=N*NX
         NCPB(I)=NCPB(I)-N
   31    CONTINUE
      ENDIF
      ENDIF

c++++++++++YURI VASSILEVSKI++++++++++++++++++++++++++
      IF (METHOD.EQ.4) THEN
C       CALL DIVIDEINSERT2(NUMBLK,NUMPRC,10,NFOUT,LEVELC,*13)
      END IF
c++++++++++++++++++++++++++++++++++++++++++++++++++++

C  DIVIDE ELEMENTS AMONG PROCESSORS
C  INITIALIZE:
C  NER = TOTAL ELEMENTS THAT REMAIN TO BE ASSIGNED
C  NPC = CURRENT PROCESSOR NUMBER
C  NERP = NUMBER OF ELEMENTS THAT REMAIN TO BE ASSIGNED TO CURRENT PROCESSOR
C  NFC = CURRENT FAULT BLOCK NUMBER
C  NERF = NUMBER OF ELEMENTS THAT REMAIN TO BE ASSIGNED IN CURRENT FAULT BLOCK

      NPC=-1
      NERP=0
      NFC=0
      NERF=0

C  SPECIAL TREATMENT FOR PROCESSOR ZERO AND MULTIMODEL
C  INSURE SOME ELEMENTS FOR EACH MODEL ARE ASSIGNED TO PROCESSOR 0

    3 IF (STP0) THEN
         NPC=0
         IF (NERP.GT.0) GO TO 40
         DO 33 N=1,NUMBLK
         IF (NEP0(N).GT.0) THEN
            NFC=N
            NERP=NEP0(N)
            NEP0(N)=0
            CALL BLKDIM (N,NX,NY,NZ,KERR)
            NERF=NCPB(N)*NX
            N0=N0MAP(N)
            NEWPRC=.TRUE.
            NEWFB=.TRUE.
            GO TO 40
         ENDIF
   33    CONTINUE
         STP0=.FALSE.
         NFC=0
         NERF=0
      ENDIF

C  STEP TO NEXT PROCESSOR IF CURRENT PROCESSOR HAS ITS QUOTA OF ELEMENTS

      IF (NERP.LT.1) THEN
         IF (NPC.GE.NUMPRC-1) GO TO 99
         NPC=NPC+1
         NEWPRC=.TRUE.
         NERP=NER/(NUMPRC-NPC)
      ENDIF

C  STEP TO NEXT FAULT BLOCK IF NO MORE ELEMENTS IN CURRENT BLOCK

      IF (NERF.LT.1) THEN
         NFC=NFC+1
         CALL BLKDIM (NFC,NX,NY,NZ,KERR)
         NERF=NCPB(NFC)*NX
         N0=N0MAP(NFC)
         NEWFB=.TRUE.
      ENDIF

C BRANCH ON ASSIGNMENT METHOD

   40 GO TO (10,100,200),METHOD

C  METHOD 1 - SQUARES

   10 IF (NEWFB) THEN
         KDOWN=.TRUE.
         J2=0
      ENDIF
      IF (NEWFB.OR.NEWPRC) THEN
         NEWFB=.FALSE.
         NEWPRC=.FALSE.
         S=NERP
         NJKS=SQRT(S/NX+.1)
         N=NJKS+1
         IF (ABS(NERP-NX*NJKS*NJKS).GT.ABS(NERP-NX*N*N)) NJKS=N
         IF (NJKS.LT.1) NJKS=1
      ENDIF

   11 DO 12 J=1,NY
      NN=N0+J
      DO 12 K=1,NZ
      IF (PRCMAP(NN+K*NY).EQ.-1) THEN
         IF (J.GT.J2) J1=J
         GO TO 14
      ENDIF
   12 CONTINUE

   14 J2=J1+NJKS-1
      IF (J2.GT.NY) J2=NY
      IF (J2.LT.NY.AND.NJKS*NJKS*NX.GT.4*NERP) J2=J1+NERP/(NJKS*NX)

      IF (KDOWN) THEN

         DO 15 K=1,NZ
         NN=N0+K*NY
         DO 15 J=J1,J2
         IF (PRCMAP(NN+J).EQ.-1) THEN
            PRCMAP(NN+J)=NPC
            NER=NER-NX
            NERP=NERP-NX
            NERF=NERF-NX
            IF (MYPRC.EQ.NPC) MYBLK(NFC)=.TRUE.
            MODPROC(MODBLK(NFC),NPC+1)=.TRUE.
            MODPROC(FMODBLK(NFC),NPC+1)=.TRUE.
            IF (NERP.LE.0.OR.NERF.LE.0) GO TO 3
         ENDIF
   15    CONTINUE

         KDOWN=.FALSE.
         NP=NPC-1
         IF (NP.LT.0) GO TO 3
         KK1=NZ-NJKS/2
         IF (KK1.LT.NZ/2+1) KK1=NZ/2+1
         DO 16 J=J1,J2
         NN=N0+J
         DO 16 K=NZ,1,-1
         IF (PRCMAP(NN+K*NY).EQ.NPC) THEN
            DO 17 JJ=J2,J-1,-1
            MM=N0+JJ
            DO 17 KK=NZ,KK1,-1
            IF (PRCMAP(MM+KK*NY).EQ.NP) THEN
               PRCMAP(NN+K*NY)=NP
               PRCMAP(MM+KK*NY)=NPC
               GO TO 16
            ENDIF
   17       CONTINUE
         ENDIF
   16    CONTINUE

      ELSE

         DO 18 K=NZ,1,-1
         NN=N0+K*NY
         DO 18 J=J1,J2
         IF (PRCMAP(NN+J).EQ.-1) THEN
            PRCMAP(NN+J)=NPC
            NER=NER-NX
            NERP=NERP-NX
            NERF=NERF-NX
            MODPROC(MODBLK(NFC),NPC+1)=.TRUE.
            MODPROC(FMODBLK(NFC),NPC+1)=.TRUE.
            IF (MYPRC.EQ.NPC) MYBLK(NFC)=.TRUE.
            IF (NERP.LE.0.OR.NERF.LE.0) GO TO 3
         ENDIF
   18    CONTINUE

         KDOWN=.TRUE.
         NP=NPC-1
         IF (NP.LT.0) GO TO 3
         KK2=NJKS/2
         IF (KK2.GT.NZ/2+1) KK2=NZ/2+1
         DO 19 J=J1,J2
         NN=N0+J
         DO 19 K=1,NZ
         IF (PRCMAP(NN+K*NY).EQ.NPC) THEN
            DO 20 JJ=J2,J-1,-1
            MM=N0+JJ
            DO 20 KK=1,KK2
            IF (PRCMAP(MM+KK*NY).EQ.NP) THEN
               PRCMAP(NN+K*NY)=NP
               PRCMAP(MM+KK*NY)=NPC
               GO TO 19
            ENDIF
   20       CONTINUE
         ENDIF
   19    CONTINUE
      ENDIF

      GO TO 3

C  K DIRECTION STRIP (METHOD 2)

  100 DO 101 J=1,NY
      NN=N0+J
      DO 101 K=1,NZ
      MM=NN+K*NY
      IF (PRCMAP(MM).EQ.-1) THEN
         PRCMAP(MM)=NPC
         NER=NER-NX
         NERP=NERP-NX
         NERF=NERF-NX
         MODPROC(MODBLK(NFC),NPC+1)=.TRUE.
         MODPROC(FMODBLK(NFC),NPC+1)=.TRUE.
         IF (MYPRC.EQ.NPC) MYBLK(NFC)=.TRUE.
         IF (NERP.LE.0.OR.NERF.LE.0) GO TO 3
      ENDIF
  101 CONTINUE

      GO TO 3

C  J DIRECTION STRIP (METHOD 3)

  200 DO 201 K=1,NZ
      NN=N0+K*NY
      DO 201 J=1,NY
      IF (PRCMAP(NN+J).EQ.-1) THEN
         PRCMAP(NN+J)=NPC
         NER=NER-NX
         NERP=NERP-NX
         NERF=NERF-NX
         MODPROC(MODBLK(NFC),NPC+1)=.TRUE.
         MODPROC(FMODBLK(NFC),NPC+1)=.TRUE.
         IF (MYPRC.EQ.NPC) MYBLK(NFC)=.TRUE.
         IF (NERP.LE.0.OR.NERF.LE.0) GO TO 3
      ENDIF
  201 CONTINUE

      GO TO 3

c++++++++++YURI VASSILEVSKI++++++++++++++++++++++++++
C  J DIRECTION STRIP WITH PREDEFINED STRIP BOUNDS (METHOD 4)
  300 CONTINUE
      IF (NUMPRC.EQ.1) THEN
       GOTO 200
      ELSE
C       CALL DIVIDEINSERT3(MYPRC,NPC,NFC,N0,NX,NY,NZ,
C     &                    PRCMAP,NER,NERF,NERP,NFOUT,LEVELC,*13)
      END IF

      GO TO 3
c++++++++++++++++++++++++++++++++++++++++++++++++++++


   99 CONTINUE

CBW
C CONSTRUCT PRCMAPN() FOR NODAL BASED PROCESSOR ASSIGNMENT
      DO M=1,NUMBLK
         CALL BLKDIM(M,NX,NY,NZ,KERR)
         N0=N0MAP(M)
         N0N=N0MAPN(M)
         NYM=NYMAP(M)
         NYMN=NYM+1
         DO K=1,NZ
            DO J=1,NY
               IF (PRCMAP(N0+K*NYM+J).GE.0) THEN
                  PRCMAPN(N0N+K*NYMN+J)=PRCMAP(N0+K*NYM+J)
                  PRCMAPN(N0N+K*NYMN+J+1)=PRCMAP(N0+K*NYM+J)
                  PRCMAPN(N0N+(K+1)*NYMN+J)=PRCMAP(N0+K*NYM+J)
                  PRCMAPN(N0N+(K+1)*NYMN+J+1)=PRCMAP(N0+K*NYM+J)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
CBW

      RETURN

c++++++++++YURI VASSILEVSKI++++++++++++++++++++++++++++
 13   CONTINUE
      IF (LEVERR.LT.3) LEVERR = 3
      IF(LEVELC) WRITE(NFOUT,*)  'FATAL ERROR IN DIVIDE'
      IF(LEVELC) WRITE(NFOUT,*)
     &     '*******************************************'
c++++++++++++++++++++++++++++++++++++++++++++++++++++++
      END

C*********************************************************************
      SUBROUTINE IPARS_METIS()
C*********************************************************************

C  NOTE: On entry to DIVIDE:
C        PRCMAP() = -1 ==> Grid column in the system
C        PRCMAP() = -2 ==> Grid column not in the system
C        On exit from DIVIDE:
C        PRCMAP() >= 0 ==> Processor assignment
C        PRCMAP() <  0 ==> Grid column not in the system

C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'

      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'mpif.h'

      INTEGER M,N,NX,NY,NZ,N0,N0N,NYM,NYMN,KERR,J,K,NERR
      INTEGER NELEMS,NNODES,CTR,OBJVAL,NPC

      IF (MYPRC.EQ.0) THEN

C INITIALIZE PRCMAPN() FOR NODAL BASED PROCESSOR ASSIGNMENT

         DO M=1,NUMBLK
            CALL BLKDIM(M,NX,NY,NZ,KERR)
            N0=N0MAP(M)
            N0N=N0MAPN(M)
            NYM=NYMAP(M)
            NYMN=NYM+1
            DO K=1,NZ
               DO J=1,NY
                  IF (PRCMAP(N0+K*NYM+J).EQ.-1) THEN
                     PRCMAPN(N0N+K*NYMN+J)=PRCMAP(N0+K*NYM+J)
                     PRCMAPN(N0N+K*NYMN+J+1)=PRCMAP(N0+K*NYM+J)
                     PRCMAPN(N0N+(K+1)*NYMN+J)=PRCMAP(N0+K*NYM+J)
                     PRCMAPN(N0N+(K+1)*NYMN+J+1)=PRCMAP(N0+K*NYM+J)
                  ENDIF
               ENDDO
            ENDDO
         ENDDO

C COUNT NUMBER OF TOTAL ARIAL ELEMENTS IN THE SYSTEM

         CTR = 0
         DO M=1,NUMBLK
            CALL BLKDIM(M,NX,NY,NZ,KERR)
            N0=N0MAP(M)
            NYM=NYMAP(M)
            DO K=1,NZ
               DO J=1,NY
                  IF (PRCMAP(N0+K*NYM+J).EQ.-1) THEN
                     CTR=CTR+1
                     PRCMAP(N0+K*NYM+J)=CTR
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         NELEMS=CTR

C COUNT NUMBER OF TOTAL ARIAL NODES IN THE SYSTEM

         CTR=0
         DO M=1,NUMBLK
            CALL BLKDIM(M,NX,NY,NZ,KERR)
            N0N=N0MAPN(M)
            NYMN=NYMAP(M)+1
            DO K=1,NZ+1
               DO J=1,NY+1
                  IF (PRCMAPN(N0N+K*NYMN+J).EQ.-1) THEN
                     CTR=CTR+1
                     PRCMAPN(N0N+K*NYMN+J)=CTR
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         NNODES=CTR

C      CALL IPMETIS (N0MAP,NYMAP,PRCMAP,N0MAPN,
C     &              PRCMAPN,NELEMS,NNODES,
C     &              METHOD,OBJVAL,NERR)

         IF (NERR.NE.0) THEN
            WRITE(NFOUT,'(A,I5)')
     &           "ERROR: IPRASMETIS CRASH,ERRCODE=",NERR
            WRITE(*,'(A,I5)')
     &           "ERROR: IPRASMETIS CRASH,ERRCODE=",NERR
            STOP 13
         ELSE
            WRITE(NFOUT,'(A,I12)')
     &           "TOTAL # OF EDGECUTS FROM METIS = ",OBJVAL
         ENDIF
      ENDIF

C BROADCAST THE PRCMAP ARRAY FROM PROCESSOR 0 TO ALL PROCESSORS

      CALL MPI_BCAST(PRCMAP,6000000,
     &               MPI_INTEGER,0,MPI_COMM_WORLD,KERR)

C SET MYBLK() AND MODPROC(,)

      DO M=1,NUMBLK
         CALL BLKDIM(M,NX,NY,NZ,KERR)
         N0=N0MAP(M)
         NYM=NYMAP(M)
         DO K=1,NZ
            DO J=1,NY
               NPC=PRCMAP(N0+K*NYM+J)
               MODPROC(MODBLK(M),NPC+1)=.TRUE.
               MODPROC(FMODBLK(M),NPC+1)=.TRUE.
               IF (NPC.EQ.MYPRC) THEN
                  MYBLK(M)=.TRUE.
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      END

!----------------------------------------------------------------------
! bag8 - print virtual memory info for memory debugging
!----------------------------------------------------------------------
      SUBROUTINE PRINT_VM_INFO(MSG)
      IMPLICIT NONE
      INCLUDE 'control.h'
      CHARACTER*(*) :: MSG
      CHARACTER*256 :: BUF,PREFIX, PID = 'n/a', VMSIZE = 'n/a',
     &                 VMPEAK = 'n/a'
      INTEGER :: IDX

!ifndef __linux__
!      RETURN
!endif
      IF (.NOT.VMDEBUG) RETURN

      OPEN(10,file='/proc/self/status',status='old',err=200)
      DO WHILE (.TRUE.)
      READ(10,'(A)',end=100) BUF
      IDX = SCAN(BUF,CHAR(9))        ! Find index of first tab
      PREFIX = BUF(1:IDX-1)
      IF (PREFIX.EQ.'Pid:') THEN
        PID = ADJUSTL(BUF(IDX+1:256))
      ELSEIF (PREFIX.EQ.'VmSize:') THEN
        VMSIZE = ADJUSTL(BUF(IDX+1:256))
      ELSEIF (PREFIX.EQ.'VmPeak:') THEN
        VMPEAK = ADJUSTL(BUF(IDX+1:256))
      ENDIF
      END DO
 100  CLOSE(10)
      WRITE(*,'(A,I2,8A)')'MYPRC=',MYPRC,', PID=',TRIM(PID),
     &  ', MSG=',MSG,': VmPeak=',TRIM(VMPEAK),', VmSize=',TRIM(VMSIZE)
 200  RETURN

      END SUBROUTINE PRINT_VM_INFO

