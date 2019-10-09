!SGT: Program to generate fault-block & mesh info for IPARSv2 input file.
!     General case that treats non-uniform grids and general fault-block 
!     intersections.  08/09/08
PROGRAM MB_INPUT
IMPLICIT NONE
CHARACTER(10) FILIN,FILOUT
CHARACTER(10),    DIMENSION(:), ALLOCATABLE :: BNAM
INTEGER :: NFIN,NFOUT,NB,NM,MAXB,MXNX,MXNY,MXNZ,NBR,NIDC,IERR,I,J,K
INTEGER,        DIMENSION(:,:), ALLOCATABLE :: NEIB,GMID,MFLG,NID
INTEGER,      DIMENSION(:,:,:), ALLOCATABLE :: MDIM
INTEGER,          DIMENSION(:), ALLOCATABLE :: NXB,NYB,NZB,NMB,MAL,MTY
DOUBLE PRECISION :: LB(2),UB(2)
DOUBLE PRECISION,   DIMENSION(:), ALLOCATABLE :: DOWX,DOWY,DOWZ
DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: DXB,DYB,DZB
DOUBLE PRECISION,   DIMENSION(:), ALLOCATABLE :: XOFFB,YOFFB,ZOFFB
PARAMETER (MXNX=30,MXNY=50,MXNZ=50)
DATA NFIN/1/,NFOUT/2/,FILIN/'raw_in.dat'/,FILOUT/'fltblk.dat'/

INTERFACE 
   SUBROUTINE INTERSECT2D(XOFF1,YOFF1,NX1,NY1,DX1,DY1,&
                          XOFF2,YOFF2,NX2,NY2,DX2,DY2,&
                          LB1,UB1,LB2,UB2)
      INTEGER :: NX1,NY1,NX2,NY2
      DOUBLE PRECISION :: XOFF1,YOFF1,XOFF2,YOFF2,LB1,UB1,LB2,UB2
      DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: DX1,DY1,DX2,DY2
   END SUBROUTINE INTERSECT2D
END INTERFACE

! Open raw-data input file and read-in fault-block info.
OPEN(UNIT=NFIN,FILE=FILIN,STATUS='old')

READ(NFIN,*,END=101,ERR=102)
READ(NFIN,*,END=101,ERR=103) NB,NM

! Allocate arrays
ALLOCATE(BNAM(1:NB),STAT=IERR)
ALLOCATE(DOWX(1:NB),STAT=IERR)
ALLOCATE(DOWY(1:NB),STAT=IERR)
ALLOCATE(DOWZ(1:NB),STAT=IERR)
ALLOCATE(MTY(1:NM),STAT=IERR)
ALLOCATE(NXB(1:NB),STAT=IERR)
ALLOCATE(NYB(1:NB),STAT=IERR)
ALLOCATE(NZB(1:NB),STAT=IERR)
ALLOCATE(DXB(1:MXNX,NB),STAT=IERR)
ALLOCATE(DYB(1:MXNY,NB),STAT=IERR)
ALLOCATE(DZB(1:MXNZ,NB),STAT=IERR)
ALLOCATE(XOFFB(1:NB),STAT=IERR)
ALLOCATE(YOFFB(1:NB),STAT=IERR)
ALLOCATE(ZOFFB(1:NB),STAT=IERR)
ALLOCATE(NMB(1:NB),STAT=IERR)
ALLOCATE(MAL(1:NM),STAT=IERR)
ALLOCATE(NID(1:NM,2),STAT=IERR)
ALLOCATE(NEIB(1:NB,6),STAT=IERR)
ALLOCATE(GMID(1:NB,6),STAT=IERR)
ALLOCATE(MDIM(1:NB,6,2),STAT=IERR)
ALLOCATE(MFLG(1:NB,6),STAT=IERR)

! Read in mortar-types & alignments
READ(NFIN,*,END=101,ERR=104) (MTY(I),I=1,NM)
READ(NFIN,*,END=101,ERR=105) (MAL(I),I=1,NM)
READ(NFIN,*,END=101,ERR=106)

! Loop over blocks and store info in arrays
DO I=1,NB
   READ(NFIN,*,END=101,ERR=107)
   IF(I.LT.10) BNAM(I)='BLOCK'//CHAR(48+I)
   IF(I.GE.10.AND.I.LT.100) &
      BNAM(I)='BLOCK'//CHAR(48+I/10)//CHAR(48+MOD(I,10))
   IF(I.GE.100.AND.I.LT.1000) &
      BNAM(I)='BLOCK'//CHAR(48+I/100)//CHAR(48+MOD(I,100)/10)// & 
               CHAR(48+MOD(MOD(I,100),10))
   READ(NFIN,*,END=101,ERR=108)
   READ(NFIN,*,END=101,ERR=109) DOWX(I),DOWY(I),DOWZ(I)
   READ(NFIN,*,END=101,ERR=110)
   READ(NFIN,*,END=101,ERR=111) NXB(I),NYB(I),NZB(I)
   IF(NXB(I).GT.MXNX) THEN 
      WRITE(0,*) 'BLK ',I,' NXB = ',NXB(I),' > MXNX = ',MXNX
      STOP 'ERROR: INSUFFICIENT ALLOCATION FOR MXNX!!'
   ELSEIF(NYB(I).GT.MXNY) THEN
      WRITE(0,*) 'BLK ',I,' NYB = ',NYB(I),' > MXNY = ',MXNY
      STOP 'ERROR: INSUFFICIENT ALLOCATION FOR MXNY!!'
   ELSEIF(NZB(I).GT.MXNZ) THEN
      WRITE(0,*) 'BLK ',I,' NZB = ',NZB(I),' > MXNZ = ',MXNZ
      STOP 'ERROR: INSUFFICIENT ALLOCATION FOR MXNZ!!'
   ENDIF
   READ(NFIN,*,END=101,ERR=112)
   READ(NFIN,*,END=101,ERR=113) (DXB(J,I),J=1,NXB(I))
   READ(NFIN,*,END=101,ERR=114)
   READ(NFIN,*,END=101,ERR=115) (DYB(J,I),J=1,NYB(I))
   READ(NFIN,*,END=101,ERR=116)
   READ(NFIN,*,END=101,ERR=117) (DZB(J,I),J=1,NZB(I))
   READ(NFIN,*,END=101,ERR=118)
   READ(NFIN,*,END=101,ERR=119) XOFFB(I),YOFFB(I),ZOFFB(I)
   READ(NFIN,*,END=101,ERR=120)
   ! Read in total # mortars for blk i, faces of mort j on blk i, global mort ids.
   READ(NFIN,*,END=101,ERR=121) NMB(I),(NEIB(I,J),J=1,NMB(I)),&
       (GMID(I,J),J=1,NMB(I))
   ! Read in mort dimensions of all mortars on blk i, mortar flags
   READ(NFIN,*,END=101,ERR=122) ((MDIM(I,J,K),K=1,2),J=1,NMB(I)),&
       (MFLG(I,J),J=1,NMB(I))
   READ(NFIN,*,END=101,ERR=106)
ENDDO

GOTO 101

102 WRITE(0,*) 'ERROR READING BLOCKS-MORTARS HEADER!!'
    GOTO 199

103 WRITE(0,*) 'ERROR READING NUMBER OF BLOCKS/MORTARS!!'
    GOTO 199

104 WRITE(0,*) 'ERROR READING MORTAR TYPE!!'
    GOTO 199

105 WRITE(0,*) 'ERROR READING MORTAR ALIGNMENT!!'
    GOTO 199

106 WRITE(0,*) 'ERROR READING BLANK LINE BETWEEN BLOCKS!!'
    GOTO 199

107 WRITE(0,*) 'ERROR READING BLOCK HEADER!!'
    GOTO 199

108 WRITE(0,*) 'ERROR READING DOWN HEADER!!'
    GOTO 199

109 WRITE(0,*) 'ERROR READING DOWN!!'
    GOTO 199

110 WRITE(0,*) 'ERROR READING NX-NY-NZ HEADER!!'
    GOTO 199

111 WRITE(0,*) 'ERROR READING NX-NY-NZ!!'
    GOTO 199

112 WRITE(0,*) 'ERROR READING DX HEADER!!'
    GOTO 199

113 WRITE(0,*) 'ERROR READING DX!!'
    GOTO 199

114 WRITE(0,*) 'ERROR READING DY HEADER!!'
    GOTO 199

115 WRITE(0,*) 'ERROR READING DY!!'
    GOTO 199

116 WRITE(0,*) 'ERROR READING DZ HEADER!!'
    GOTO 199

117 WRITE(0,*) 'ERROR READING DZ!!'
    GOTO 199

118 WRITE(0,*) 'ERROR READING OFFSET HEADER!!'
    GOTO 199

119 WRITE(0,*) 'ERROR READING OFFSET!!'
    GOTO 199

120 WRITE(0,*) 'ERROR READING MORTAR INFO HEADER!!'
    GOTO 199

121 WRITE(0,*) 'ERROR READING NO OF LOCAL MORTARS AND FACE INFO!!'
    GOTO 199

122 WRITE(0,*) 'ERROR READING LOCAL MORTAR DIMENSIONS AND READ-FLAG!!'
    GOTO 199

199 STOP 'TERMINATING PROGRAM!!'

101 CONTINUE

CLOSE(NFIN)

! Write out fault-block/local mortar info & mesh data in IPARSv2 format
OPEN(UNIT=NFOUT,FILE=FILOUT,STATUS='replace')
WRITE(NFOUT,8) NB
WRITE(NFOUT,9) NM
WRITE(NFOUT,*)

! Calculate block neighbor
DO 100 I=1,NM
   NIDC=0
   DO J=1,NB
      DO K=1,NMB(J)
         IF(GMID(J,K).EQ.I) THEN
           NIDC=NIDC+1
           NID(I,NIDC)=J
         ENDIF
         IF(NIDC.EQ.2) GOTO 100
      ENDDO
   ENDDO
100 CONTINUE

DO I=1,NB
   IF(I.LT.10) THEN
      WRITE(NFOUT,10) I,BNAM(I)
      WRITE(NFOUT,11) I,DOWX(I),DOWY(I),DOWZ(I)
      WRITE(NFOUT,12) I,NXB(I),I,NYB(I),I,NZB(I)
      WRITE(NFOUT,13) I
      WRITE(NFOUT,14) (DXB(J,I),J=1,NXB(I))
      WRITE(NFOUT,15) I
      WRITE(NFOUT,16) (DYB(J,I),J=1,NYB(I))
      WRITE(NFOUT,17) I
      WRITE(NFOUT,18) (DZB(J,I),J=1,NZB(I))
      WRITE(NFOUT,19) I,XOFFB(I),YOFFB(I),ZOFFB(I)
      WRITE(NFOUT,*) 
      WRITE(NFOUT,20) I,NMB(I)
      WRITE(NFOUT,*) 
      DO J=1,NMB(I)
         WRITE(NFOUT,21) I,J,GMID(I,J)
         WRITE(NFOUT,22) I,J,NEIB(I,J)
         NBR=NID(GMID(I,J),1)
         IF(NID(GMID(I,J),1).EQ.I) NBR=NID(GMID(I,J),2)
         IF((NEIB(I,J).EQ.1).OR.(NEIB(I,J).EQ.2)) THEN
            CALL INTERSECT2D(YOFFB(I),ZOFFB(I),NYB(I),NZB(I),DYB(1:NYB(I),I),&
                     DZB(1:NZB(I),I),YOFFB(NBR),ZOFFB(NBR),NYB(NBR),NZB(NBR),&
                     DYB(1:NYB(NBR),NBR),DZB(1:NZB(NBR),NBR),LB(1),UB(1),&
                     LB(2),UB(2))
         ELSEIF((NEIB(I,J).EQ.3).OR.(NEIB(I,J).EQ.4)) THEN
            CALL INTERSECT2D(XOFFB(I),ZOFFB(I),NXB(I),NZB(I),DXB(1:NXB(I),I),&
                     DZB(1:NZB(I),I),XOFFB(NBR),ZOFFB(NBR),NXB(NBR),NZB(NBR),&
                     DXB(1:NXB(NBR),NBR),DZB(1:NZB(NBR),NBR),LB(1),UB(1),&
                     LB(2),UB(2))
         ELSE
            CALL INTERSECT2D(XOFFB(I),YOFFB(I),NXB(I),NYB(I),DXB(1:NXB(I),I),&
                     DYB(1:NYB(I),I),XOFFB(NBR),YOFFB(NBR),NXB(NBR),NYB(NBR),&
                     DXB(1:NXB(NBR),NBR),DYB(1:NYB(NBR),NBR),LB(1),UB(1),&
                     LB(2),UB(2))
         ENDIF
         WRITE(NFOUT,23) I,J,LB(1),I,J,UB(1)
         WRITE(NFOUT,24) I,J,LB(2),I,J,UB(2)
         WRITE(NFOUT,25) I,J,MDIM(I,J,1),I,J,MDIM(I,J,2)
         WRITE(NFOUT,26) I,J,MFLG(I,J)
         WRITE(NFOUT,*) 
      ENDDO 
      WRITE(NFOUT,*)
               
   ELSEIF(I.GE.10.AND.I.LT.100) THEN
      WRITE(NFOUT,30) I,BNAM(I)
      WRITE(NFOUT,31) DOWX(I),DOWY(I),DOWZ(I) 
      WRITE(NFOUT,32) I,NXB(I),I,NYB(I),I,NZB(I)
      WRITE(NFOUT,33) I
      WRITE(NFOUT,34) (DXB(J,I),J=1,NXB(I))
      WRITE(NFOUT,35) I
      WRITE(NFOUT,36) (DYB(J,I),J=1,NYB(I))
      WRITE(NFOUT,37) I
      WRITE(NFOUT,38) (DZB(J,I),J=1,NZB(I))
      WRITE(NFOUT,39) I,XOFFB(I),YOFFB(I),ZOFFB(I)
      WRITE(NFOUT,*) 
      WRITE(NFOUT,40) I,NMB(I)
      WRITE(NFOUT,*) 
      DO J=1,NMB(I)
         WRITE(NFOUT,41) I,J,GMID(I,J)
         WRITE(NFOUT,42) I,J,NEIB(I,J)
         NBR=NID(GMID(I,J),1)
         IF(NID(GMID(I,J),1).EQ.I) NBR=NID(GMID(I,J),2)
         IF((NEIB(I,J).EQ.1).OR.(NEIB(I,J).EQ.2)) THEN
            CALL INTERSECT2D(YOFFB(I),ZOFFB(I),NYB(I),NZB(I),DYB(1:NYB(I),I),&
                     DZB(1:NZB(I),I),YOFFB(NBR),ZOFFB(NBR),NYB(NBR),NZB(NBR),&
                     DYB(1:NYB(NBR),NBR),DZB(1:NZB(NBR),NBR),LB(1),UB(1),&
                     LB(2),UB(2))
         ELSEIF((NEIB(I,J).EQ.3).OR.(NEIB(I,J).EQ.4)) THEN
            CALL INTERSECT2D(XOFFB(I),ZOFFB(I),NXB(I),NZB(I),DXB(1:NXB(I),I),&
                     DZB(1:NZB(I),I),XOFFB(NBR),ZOFFB(NBR),NXB(NBR),NZB(NBR),&
                     DXB(1:NXB(NBR),NBR),DZB(1:NZB(NBR),NBR),LB(1),UB(1),&
                     LB(2),UB(2))
         ELSE
            CALL INTERSECT2D(XOFFB(I),YOFFB(I),NXB(I),NYB(I),DXB(1:NXB(I),I),&
                     DYB(1:NYB(I),I),XOFFB(NBR),YOFFB(NBR),NXB(NBR),NYB(NBR),&
                     DXB(1:NXB(NBR),NBR),DYB(1:NYB(NBR),NBR),LB(1),UB(1),&
                     LB(2),UB(2))
         ENDIF
         WRITE(NFOUT,43) I,J,LB(1),I,J,UB(1)
         WRITE(NFOUT,44) I,J,LB(2),I,J,UB(2)
         WRITE(NFOUT,45) I,J,MDIM(I,J,1),I,J,MDIM(I,J,2)
         WRITE(NFOUT,46) I,J,MFLG(I,J)
         WRITE(NFOUT,*) 
      ENDDO 
      WRITE(NFOUT,*)
   ELSE
      STOP 'ERROR: BLKS >= 100 NOT SUPPORTED!!' 
   ENDIF

8  FORMAT('MB_NUMBLKS = ',I3)   
9  FORMAT('MB_NUMMORTARS = ',I3)   
10 FORMAT('BLOCKNAME(',I1,') = "',A6,'"')
11 FORMAT('DOWN(1 TO 3,',I1,') = ',F4.1,1X,F4.1,1X,F4.1)
12 FORMAT('NX(',I1,') = ',I2,4X,'NY(',I1,') = ',I2,4X,'NZ(',I1,') = ',I2)
13 FORMAT('DX(,',I1,') = ')
14 FORMAT(2X,8F8.4)
15 FORMAT('DY(,',I1,') = ')
16 FORMAT(2X,8F8.4)
17 FORMAT('DZ(,',I1,') = ')
18 FORMAT(2X,8F8.4)
19 FORMAT('XYZ111(,',I1,') = ',F10.4,1X,F10.4,1X,F10.4)
20 FORMAT('MB_MYMORTAR_NUMBER(',I1,') = ',I1)
21 FORMAT('MB_MYMORTAR_ID(',I1,',',I1,') = ',I3)
22 FORMAT('MB_MYMORTAR_FACE(',I1,',',I1,') = ',I1)
23 FORMAT('MB_MYMORTAR_LB1(',I1,',',I1,') = ',F10.4,2X, &
          'MB_MYMORTAR_UB1(',I1,',',I1,') = ',F10.4)
24 FORMAT('MB_MYMORTAR_LB2(',I1,',',I1,') = ',F10.4,2X, &
          'MB_MYMORTAR_UB2(',I1,',',I1,') = ',F10.4)
25 FORMAT('MB_MYMORTAR_DIM1(',I1,',',I1,') = ',I2,2X, &
          'MB_MYMORTAR_DIM2(',I1,',',I1,') = ',I2)
26 FORMAT('MB_MYMORTAR_FLG(',I1,',',I1,') = ',I1)

30 FORMAT('BLOCKNAME(',I2,') = "',A7,'"')
31 FORMAT('DOWN(1 TO 3,',I2,') = ',F4.1,1X,F4.1,1X,F4.1)
32 FORMAT('NX(',I2,') = ',I2,4X,'NY(',I2,') = ',I2,4X,'NZ(',I2,') = ',I2)
33 FORMAT('DX(,',I2,') = ')
34 FORMAT(2X,8F8.4)
35 FORMAT('DY(,',I2,') = ')
36 FORMAT(2X,8F8.4)
37 FORMAT('DZ(,',I2,') = ')
38 FORMAT(2X,8F8.4)
39 FORMAT('XYZ111(,',I2,') = ',F10.4,1X,F10.4,1X,F10.4)
40 FORMAT('MB_MYMORTAR_NUMBER(',I1,') = ',I1)
41 FORMAT('MB_MYMORTAR_ID(',I2,',',I1,') = ',I3)
42 FORMAT('MB_MYMORTAR_FACE(',I2,',',I1,') = ',I1)
43 FORMAT('MB_MYMORTAR_LB1(',I2,',',I1,') = ',F10.4,2X, &
          'MB_MYMORTAR_UB1(',I2,',',I1,') = ',F10.4)
44 FORMAT('MB_MYMORTAR_LB2(',I2,',',I1,') = ',F10.4,2X, &
          'MB_MYMORTAR_UB2(',I2,',',I1,') = ',F10.4)
45 FORMAT('MB_MYMORTAR_DIM1(',I2,',',I1,') = ',I2,2X, &
          'MB_MYMORTAR_DIM2(',I2,',',I1,') = ',I2)
46 FORMAT('MB_MYMORTAR_FLG(',I2,',',I1,') = ',I1)
ENDDO

! Global mortar info
WRITE(NFOUT,47) 
47 FORMAT('$ GLOBAL MORTAR INFO')
WRITE(NFOUT,*) 

! Type and align
DO I=1,NM
IF(I.LT.10) THEN
   WRITE(NFOUT,50) I,MTY(I)
ELSEIF((I.GE.10).AND.(I.LT.100)) THEN
   WRITE(NFOUT,51) I,MTY(I)
ELSEIF((I.GE.100).AND.(I.LT.1000)) THEN
   WRITE(NFOUT,52) I,MTY(I)
ENDIF
50 FORMAT('MB_MORTAR_TYPE(',I1,') = ',I1)
51 FORMAT('MB_MORTAR_TYPE(',I2,') = ',I1)
52 FORMAT('MB_MORTAR_TYPE(',I3,') = ',I1)
ENDDO
WRITE(NFOUT,*)

DO I=1,NM
IF(I.LT.10) THEN
   WRITE(NFOUT,53) I,MAL(I)
ELSEIF((I.GE.10).AND.(I.LT.100)) THEN
   WRITE(NFOUT,54) I,MAL(I)
ELSEIF((I.GE.100).AND.(I.LT.1000)) THEN
   WRITE(NFOUT,55) I,MAL(I)
ENDIF 
53 FORMAT('MB_MORTAR_ALIGN(',I1,') = ',I1)
54 FORMAT('MB_MORTAR_ALIGN(',I2,') = ',I1)
55 FORMAT('MB_MORTAR_ALIGN(',I3,') = ',I1)
ENDDO
WRITE(NFOUT,*)

! Write block neighbor info
DO I=1,NM
   DO J=1,2
      IF(I.LT.10) THEN
         WRITE(NFOUT,60) J,I,NID(I,J)
      ELSEIF((I.GE.10).AND.(I.LT.100)) THEN
         WRITE(NFOUT,61) J,I,NID(I,J)
      ELSEIF((I.GE.100).AND.(I.LT.1000)) THEN
         WRITE(NFOUT,62) J,I,NID(I,J)
      ELSE
         STOP 'ERROR: TOO MANY MORTARS, >= 1000!!'
      ENDIF
   ENDDO
60 FORMAT('MB_MORTAR_NEIGH(',I1,',',I1,') = ',I3)
61 FORMAT('MB_MORTAR_NEIGH(',I1,',',I2,') = ',I3)
62 FORMAT('MB_MORTAR_NEIGH(',I1,',',I3,') = ',I3)
ENDDO
WRITE(NFOUT,*)

CLOSE(NFOUT)

! Deallocate all arrays on heap
IF(ALLOCATED(BNAM)) DEALLOCATE(BNAM,STAT=IERR)
IF(ALLOCATED(DOWX)) DEALLOCATE(DOWX,STAT=IERR)
IF(ALLOCATED(DOWY)) DEALLOCATE(DOWY,STAT=IERR)
IF(ALLOCATED(DOWZ)) DEALLOCATE(DOWZ,STAT=IERR)
IF(ALLOCATED(MTY)) DEALLOCATE(MTY,STAT=IERR)
IF(ALLOCATED(NXB)) DEALLOCATE(NXB,STAT=IERR)
IF(ALLOCATED(NYB)) DEALLOCATE(NYB,STAT=IERR)
IF(ALLOCATED(NZB)) DEALLOCATE(NZB,STAT=IERR)
IF(ALLOCATED(DXB)) DEALLOCATE(DXB,STAT=IERR)
IF(ALLOCATED(DYB)) DEALLOCATE(DYB,STAT=IERR)
IF(ALLOCATED(DZB)) DEALLOCATE(DZB,STAT=IERR)
IF(ALLOCATED(XOFFB)) DEALLOCATE(XOFFB,STAT=IERR)
IF(ALLOCATED(YOFFB)) DEALLOCATE(YOFFB,STAT=IERR)
IF(ALLOCATED(ZOFFB)) DEALLOCATE(ZOFFB,STAT=IERR)
IF(ALLOCATED(NMB)) DEALLOCATE(NMB,STAT=IERR)
IF(ALLOCATED(MAL)) DEALLOCATE(MAL,STAT=IERR)
IF(ALLOCATED(NID)) DEALLOCATE(NID,STAT=IERR)
IF(ALLOCATED(NEIB)) DEALLOCATE(NEIB,STAT=IERR)
IF(ALLOCATED(GMID)) DEALLOCATE(GMID,STAT=IERR)
IF(ALLOCATED(MDIM)) DEALLOCATE(MDIM,STAT=IERR)
IF(ALLOCATED(MFLG)) DEALLOCATE(MFLG,STAT=IERR)

STOP
END

SUBROUTINE INTERSECT2D(XOFF1,YOFF1,NX1,NY1,DX1,DY1,&
                       XOFF2,YOFF2,NX2,NY2,DX2,DY2,&
                       LB1,UB1,LB2,UB2)
IMPLICIT NONE
INTEGER :: I,NX1,NY1,NX2,NY2 
DOUBLE PRECISION :: XOFF1,YOFF1,XOFF2,YOFF2
DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: DX1,DX2,DY1,DY2
DOUBLE PRECISION :: LB1,UB1,LB2,UB2,XL1,XH1,YL1,YH1,&
                    XL2,XH2,YL2,YH2

XL1=XOFF1
XH1=XL1
DO I=1,NX1
   XH1=XH1+DX1(I)
ENDDO
YL1=YOFF1
YH1=YL1
DO I=1,NY1
   YH1=YH1+DY1(I)
ENDDO
XL2=XOFF2
XH2=XL2
DO I=1,NX2
   XH2=XH2+DX2(I)
ENDDO
YL2=YOFF2
YH2=YL2
DO I=1,NY2
   YH2=YH2+DY2(I)
ENDDO

IF(XL1.GE.XL2.AND.XL1.LE.XH2) THEN
   LB1=XL1
ELSEIF(XL2.GE.XL1.AND.XL2.LE.XH1) THEN
   LB1=XL2
ELSE
   STOP 'ERROR: BLOCKS DO NOT INTERSECT IN X-DIR!!'
ENDIF

IF(XH1.GE.XL2.AND.XH1.LE.XH2) THEN
   UB1=XH1
ELSEIF(XH2.GE.XL1.AND.XH2.LE.XH1) THEN
   UB1=XH2
ELSE
   STOP 'ERROR: BLOCKS DO NOT INTERSECT IN X-DIR!!'
ENDIF

IF(YL1.GE.YL2.AND.YL1.LE.YH2) THEN
   LB2=YL1
ELSEIF(YL2.GE.YL1.AND.YL2.LE.YH1) THEN
   LB2=YL2
ELSE
   STOP 'ERROR: BLOCKS DO NOT INTERSECT IN Y-DIR!!'
ENDIF

IF(YH1.GE.YL2.AND.YH1.LE.YH2) THEN
   UB2=YH1
ELSEIF(YH2.GE.YL1.AND.YH2.LE.YH1) THEN
   UB2=YH2
ELSE
   STOP 'ERROR: BLOCKS DO NOT INTERSECT IN Y-DIR!!'
ENDIF

RETURN
END SUBROUTINE INTERSECT2D
