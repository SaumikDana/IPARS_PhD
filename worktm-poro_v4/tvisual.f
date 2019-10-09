C  TVISUAL.F - PRINT SINGLE PHASE FLOW MODEL STANDARD OUTPUT

C  ROUTINES IN THIS MODULE:

c SUBROUTINE TVIS_TRANSL(SCALAR,NUM,NAME)
c SUBROUTINE TVIS_INIT() - interface to the framework VIS INIT
c                             routines
c SUBROUTINE TVIS_OUTPUT() - interface to the framework VIS_OUTPUT
c                               routines
c SUBROUTINE COMP_HEAD()
c SUBROUTINE COMP_HEADW()
c===============================================================
C  CODE HISTORY:
c MPeszynska,  5/26/98   SKELETON
c B. MOMKEN    3/26/99 modified for single phase flow
c MPeszynska   11/15/99 copied for single phase implicit flow
c MPeszynska   5/01 added definition and computation of head or of potential
c  Added check that ensures that if variable is NOT found for this model
c  that the code will default to VIS_DUMMY
c S. G. THOMAS 9/25/09  mods for streamlining vis options better
c ---------------------------------------------


      SUBROUTINE TVIS_TRANSL(SCALAR,NUM,NAME)
c ---------------------------------------------
c lookup the <name> in the glossary for the model
c which is read from tvisual.h
c ---- for different models: copy the routine, change the include file
c
c set VIS_VARNAMES, VIS_OFFSETS and VIS_IPARS_VARS:
c
c VIS_VARNAMES: the names that will be put in the Tecplot file
c VIS_IPARS_NAMES: the names that are "official" to IPARS
c VIS_OFFSETS: the offsest as decided in IPARS
c --------------
c if not in glossary, copy names verbatim, set offset to 1 (default)
c ---------------------------------------------
      IMPLICIT NONE
c parameters
      CHARACTER*20 NAME
      INTEGER NUM
      LOGICAL SCALAR

c includes
      INCLUDE 'visual.h'
      INCLUDE 'tvisual.h'
      INCLUDE 'control.h'

c local variables
      INTEGER I,K,NNUM,NNUM2
      LOGICAL SUCCESS

c -----------------------------------------------------------

      IF(FIRSTVAR) THEN
         HEADOUT = .FALSE.
         POTOUT = .FALSE.
      ENDIF

      IF (SCALAR) THEN
C -----------------------------
         VIS_VARNAMES(NUM) = NAME
         VIS_OFFSETS(NUM) = 1
         VIS_IPARS_NAMES (NUM ) = NAME

         SUCCESS=.FALSE.
         DO I=1,IPARS_NSCL
            IF (IPARS_SCL_NAMES(1,I).EQ.NAME) THEN
               IF(MBPOROE) THEN ! SAUMIK,BGANIS
                  VIS_SCL_FLOW(MODACT) = VIS_SCL_FLOW(MODACT) + 1
                  IF (NUM.EQ.1) STARTMODACT = MODACT
               ENDIF
               VIS_VARNAMES(NUM) = IPARS_SCL_NAMES(2,I)
               VIS_IPARS_NAMES (NUM) = IPARS_SCL_NAMES(3,I)
               VIS_OFFSETS (NUM) = IPARS_SCL_OFFSETS(I)

c recognize that head or potential should be printed
               IF(I.EQ.HEADPOS) HEADOUT = .TRUE.
               IF(I.EQ.POTPOS)  POTOUT = .TRUE.

               SUCCESS = .TRUE.
               GOTO 11
            ENDIF
         ENDDO
 11      CONTINUE

c         CALL VIS_ARYNUM(NUM)

         IF(MBPOROE) THEN
            CALL VIS_ARYNUMMB(NUM,VIS_SCL_FLOW(MODACT))
         ELSE
            CALL VIS_ARYNUM(NUM)
         ENDIF

      ELSE
c ---------------------------------
c  vector case :

         DO K=1,3
            NNUM = VIS_SCL + (NUM-1)*3 +K
            VIS_VARNAMES( NNUM ) = NAME
            VIS_OFFSETS( NNUM  ) = 1
            VIS_IPARS_NAMES (NNUM) = NAME
         ENDDO

         SUCCESS=.FALSE.
         DO I=1,IPARS_NVEC
            IF (IPARS_VEC_NAMES(1,I).EQ.NAME) THEN
               DO K=1,3
                  NNUM = VIS_SCL + (NUM-1)*3 +K
                  VIS_VARNAMES(NNUM) = IPARS_VEC_NAMES(1+K,I)
                  VIS_IPARS_NAMES(NNUM) = IPARS_VEC_NAMES(4+K,I)
                  VIS_OFFSETS (NNUM) = IPARS_VEC_OFFSETS(K,I)
               ENDDO
               SUCCESS=.TRUE.
               GOTO 12
            ENDIF
         ENDDO

 12      CONTINUE

c bganis, gp:  Rework the loop below, caused hanging if optimization was used
         NNUM2 = VIS_SCL + (NUM-1)*3
         DO K=1,3
            NNUM = NNUM2 + K
            IF(.NOT.SUCCESS) VIS_IPARS_NAMES (NNUM) = "    "
            CALL VIS_ARYNUM(NNUM)
         ENDDO

      ENDIF

      END


c ====================================================================
      SUBROUTINE TVIS_INIT ()
c --------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'visual.h'

C      CALL DPropVTK()

      END


c===========================================================
      SUBROUTINE TVIS_OUTPUT ()
c-----------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'control.h'
      INCLUDE 'layout.h'
      INCLUDE 'blkary.h'
      INCLUDE 'mpfaary.h'

      INCLUDE 'tarydat.h'

      INCLUDE 'visual.h'

      INTEGER TVISDAT(10), N_ARRAY,I

c------------------------
c to request output of head or potential
      LOGICAL HEADOUT, POTOUT, ONCEONLY
      COMMON /TVIS/ HEADOUT, POTOUT
      DATA TVISDAT /10*0/, ONCEONLY/.TRUE./
      EXTERNAL TVELCOMP
c -------------------------------------------------------------
c the value of VISFLAG can be set anywhere, for example in restart
c files
c
      IF((VISFLAG.LT.1).OR.(VISFLAG.GT.MAXVISFLAG)) THEN
         GO TO 1
      ENDIF

c ---------------------------------------------- extra computations

      IF(HEADOUT.AND.POTOUT) THEN
         HEADOUT=.false.
      ENDIF

c      write(*,*) 'headout and potout requested ',headout,potout

      IF(HEADOUT.OR.POTOUT) THEN
         CALL COMP_HEAD(N_PRES,N_FLDEN,N_DEPTH,N_FLDENN)
      ENDIF

C ---------------------------------------
C update all variables using template 2

      IF(MBPOROE) THEN ! SAUMIK,BGANIS
         VIS_NVARS = VIS_SCL_FLOW(MODACT)
         CURRENT_MODEL = MODACT
      ENDIF

      DO I=1, VIS_NVARS
         N_ARRAY = N_VIS_VARS(I,CURRENT_MODEL)
      CALL UPDATE(N_ARRAY,2)
      ENDDO

 10   continue

      IF(ONCEONLY) THEN
         ONCEONLY = .FALSE.
         TVISDAT(1) = 9
         TVISDAT(2) = N_TRAN
         TVISDAT(3) = N_AINVF
         TVISDAT(4) = N_PRES
         TVISDAT(5) = N_VPROP
         TVISDAT(6) = N_FPROP
         TVISDAT(7) = N_VDIM
         TVISDAT(8) = N_FDIM
         TVISDAT(9) = N_KCR
         TVISDAT(10)  = N_VEL
      ENDIF

      CALL CALLWORK(TVELCOMP,TVISDAT)

c ------------------ VISUALIZATION OUTPUT
c pass the arguments to the framework  VIS ROUTINE

      CALL VIS_OUTPUT()

 1    CONTINUE
      RETURN
      END

c --------------------------------------------------------------------

      SUBROUTINE COMP_HEAD(N_PRES,N_FLDEN,N_DEPTH,N_POTVIS)
c --------------------------------------------------------------------
      INTEGER N_PRES,N_FLDEN,N_DEPTH,N_POTVIS
      INTEGER A(5)
      EXTERNAL COMP_HEADW

c --------------------------------------------------------------------
      A(1)=4
      A(2)=N_PRES
      A(3)=N_FLDEN
      A(4)=N_DEPTH
      A(5)=N_POTVIS

      CALL CALLWORK(COMP_HEADW,A)

      END

C*********************************************************************
      SUBROUTINE COMP_HEADW(IDIM,JDIM,KDIM,LDIM,
     &     IL1,IL2,JL1V,JL2V,KL1,KL2,
     &     KEYOUT,NBLK,Pwat,flden,depth,head)
C*********************************************************************
C  ROUTINE computes potential given pres,depth and den
C  PWAT(I,J,K) = WATER PRESSURE, PSI (OUTPUT, REAL*8)
C***********************************************************************
      implicit none
      INCLUDE 'layout.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),    KEYOUT(IDIM,JDIM,KDIM)
      INTEGER KROCK(IDIM,JDIM,KDIM)
      REAL*8 PWAT(IDIM,JDIM,KDIM),       DEPTH(IDIM,JDIM,KDIM),
     &     FLDEN(IDIM,JDIM,KDIM),     HEAD(IDIM,JDIM,KDIM)

      INTEGER I,J,K,L,JL1,JL2
      REAL*8 H

c------------------------
c to request output of head or potential
      LOGICAL HEADOUT, POTOUT
      COMMON /TVIS/ HEADOUT, POTOUT

c=============================================
      IF(.NOT.POTOUT.AND..NOT.HEADOUT) RETURN

      DO 1 K=KL1,KL2
         JL1=JL1V(K)
         JL2=JL2V(K)
         DO 1 J=JL1,JL2
            DO 1 I=IL1,IL2
               IF (KEYOUT(I,J,K).EQ.1) THEN
                  H=GRAV*FLDEN(I,J,K)
c compute potential and if headout requested, divide it by grav and density

                  IF(POTOUT) THEN
                     HEAD(I,J,K) = PWAT(I,J,K)-H*DEPTH(I,J,K)
                  ELSE IF(HEADOUT) THEN
                     IF(H.NE.0.D0)
     &                    HEAD(I,J,K)= PWAT(I,J,K)/H -DEPTH(I,J,K)
cmpesz: for Vauclin paper: output results in cm not feet
c     &                    HEAD(I,J,K)= (PWAT(I,J,K)/H -DEPTH(I,J,K))/
c     &                    0.3048 *100.0
                  ENDIF

               ENDIF
 1          CONTINUE

      END


