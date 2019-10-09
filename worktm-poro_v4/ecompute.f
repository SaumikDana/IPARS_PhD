C ECOMPUTE.F

C*********************************************************************
      SUBROUTINE ESETARYR8N(N_ARY,VAL,N)
C*********************************************************************
C CALL A WORK ROUTINE TO SET GRID ELEMENT ARRAY OF DIMENSION N TO VAL
C
C INPUT:
C   N_ARY = GRID ELEMENT ARRAY NUMBER
C   VAL = VALUE TO SET
C   N = 4TH DIMENSION OF THE GRID ELEMENT ARRAY
C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INTEGER N_ARY,N
      REAL*8 VAL
      EXTERNAL SETARYR8N

      INTEGER JZERO(4)

      JZERO(1) = 3
      JZERO(2) = N_ARY
      JZERO(3) = N_R8U
      JZERO(4) = N_I4U
      I4UTIL = N
      R8UTIL = VAL
      CALL CALLWORK(SETARYR8N,JZERO)

      END

C*********************************************************************
      SUBROUTINE ESETARYI4N(N_ARY,N)
C*********************************************************************
C CALL A WORK ROUTINE TO SET GRID ELEMENT ARRAY OF DIMENSION N TO 0
C
C INPUT:
C   N_ARY = GRID ELEMENT ARRAY NUMBER
C   N = 4TH DIMENSION OF THE GRID ELEMENT ARRAY
C*********************************************************************
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INTEGER N_ARY,N
      EXTERNAL SETARYI4N

      INTEGER JZERO(3)

      JZERO(1) = 2
      JZERO(2) = N_ARY
      JZERO(3) = N_I4U
      I4UTIL = N
      CALL CALLWORK(SETARYI4N,JZERO)

      END
C*********************************************************************
      SUBROUTINE SETARYI4N (IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                   KL2,KEYOUT,NBLK,IARY,N)
C*********************************************************************

C  Set ALL elements of an Integer grid-element array to zero.
C  This is a work routine.

C  IARY(I,J,K,N) = Array to be initialized (output, integer*4)

C  N = 4th dimension of the grid element array

C*********************************************************************
      IMPLICIT NONE
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,JL1V(KDIM),JL2V(KDIM)
      INTEGER KEYOUT(IDIM,JDIM,KDIM),NBLK,N,NT,I
      INTEGER IARY(IDIM*JDIM*KDIM*N)

      NT = IDIM*JDIM*KDIM*N
      DO I = 1,NT
         IARY(I) = 0
      END DO

      END

C*********************************************************************
      SUBROUTINE PEPORE_VOL(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &           KL2,KEYOUT,NBLK,BIOTM,BIOTA,VSTRAIN,STRAIN_INIT,PV0,
     &           PV,PREFC,PRESS,EVOL,MODUL,POISS)
C*********************************************************************
C Compute new pore volume based on pressure and volumetric strain change
C
C INPUT:
C   BIOTM(I1,I2,I3) = BIOTS MODULUS 1/M
C   STRAIN_INIT(I1,I2,I3) = INITIAL VOL. STRAIN CONTRIBUTION TO PV (BBL)
C   PV0(I1,I2,I3) = INITIAL PORE VOLUME (BBL)
C   PREFC(I1,I2,I3) = REFERENCE PORE VOLUME (PSI)
C   PRESS(I1,I2,I3) = PORE PRESSURE (PSI)
C
C OUTPUT:
C   PV(I1,I2,I3) = GRID ELEMENT PORE VOLUME (BBL)
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'control.h'
      INCLUDE 'emodel.h'
      INCLUDE 'layout.h'
C      INCLUDE 'xthermal.h'
      INCLUDE 'blkary.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),       KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  STRAIN_INIT(IDIM,JDIM,KDIM), PRESS(IDIM,JDIM,KDIM)
      REAL*8  PREFC(IDIM,JDIM,KDIM),       BIOTM(IDIM,JDIM,KDIM)
      REAL*8  PV(IDIM,JDIM,KDIM), EVOL(IDIM,JDIM,KDIM)
      REAL*8  PV0(IDIM,JDIM,KDIM)
      REAL*8  VSTRAIN(IDIM,JDIM,KDIM), BIOTA(IDIM,JDIM,KDIM)
      REAL*8  MODUL(IDIM,JDIM,KDIM), POISS(IDIM,JDIM,KDIM)

      INTEGER I1,I2,I3,JL1,JL2,IDBG,JDBG,KDBG
      REAL*8  ZERO,ONE,VOL0,VOL1,VOL2,VOL,PVOLD
      PARAMETER(ZERO = 0.0D0, ONE = 1.0D0)
      REAL*8 SUM_VSTRAIN_INIT, SUM_PREFC
      LOGICAL ONCE,DBG
      REAL*8 V1,V2,V3,VP0


ctm   TAMEEM
      SUM_VSTRAIN_INIT = 0.0
      SUM_PREFC = 0.0
ctm   TAMEEM

      DO I3 = KL1,KL2
         JL1 = JL1V(I3)
         JL2 = JL2V(I3)
         DO I2 = JL1,JL2
            DO I1 = IL1,IL2

               IF(KEYOUT(I1,I2,I3).NE.1) CYCLE

! SAUMIK - POROHEX ELEMENTS NOT TALKING TO FLOW
               IF(MBPOROE.AND.PV0(I1,I2,I3).EQ.0.D0) CYCLE

               V1 = MODUL(I1,I2,I3)
               V2 = POISS(I1,I2,I3)
               V3 = BIOTA(I1,I2,I3)
               VP0 = PV0(I1,I2,I3)/EVOL(I1,I2,I3)

               VOL1 = BIOTA(I1,I2,I3)*(VSTRAIN(I1,I2,I3)
     &                -STRAIN_INIT(I1,I2,I3))
! SAUMIK - 3.D0*(1-ALPHA)*(ALPHA-PHI_0)*(1-2.D0*NU)/E
               VOL2 = 3.D0*(1.D0-V3)*(V3-VP0)*(1.D0-2.D0*V2)/V1
     &             * (PRESS(I1,I2,I3)-PREFC(I1,I2,I3))
               VOL = (VOL1+VOL2)*EVOL(I1,I2,I3)
               PVOLD=PV(I1,I2,I3)
               PV(I1,I2,I3) = PV0(I1,I2,I3) + VOL
               VOL0 = 0.001D0 * PV0(I1,I2,I3)

ctm   TAMEEM
               SUM_VSTRAIN_INIT = SUM_VSTRAIN_INIT +
     &           STRAIN_INIT(I1,I2,I3)
               SUM_PREFC = SUM_PREFC + PREFC(I1,I2,I3)
ctm   TAMEEM

            ENDDO
         ENDDO
      ENDDO

      END

C*********************************************************************
      SUBROUTINE PEPV_ERR(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                  KL2,KEYOUT,NBLK,EPV,PV_FLOW,MXPVERR)
C*********************************************************************
C compute the maximum relative pore volume error between the flow model
C calculation and the poroelastic model calculation
C
C INPUT:
C   EPV(I,J,K) = PORE VOLUME COMPUTED IN THE POROELASTIC MODEL (BBL)
C   PV_FLOW(I,J,K) = PORE VOLUME COMPUTED IN THE FLOW MODEL (BBL)
C
C OUTPUT:
C   MXPVERR = MAXIMUM RELATIVEL PORE VOLUME ERROR IN THE CURRENT BLOCK
C*********************************************************************
      INCLUDE 'emodel.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM),JL1V(KDIM),JL2V(KDIM)
      REAL*8  PV_FLOW(IDIM,JDIM,KDIM),EPV(IDIM,JDIM,KDIM)
      REAL*8  MXPVERR

      INTEGER I,J,K
      REAL*8  ERR1,MXERR

      ERR1=0.D0
      MXERR=0.D0
      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).GT.0) THEN
                  IF (EPV(I,J,K).NE.0.D0) THEN
                  ERR1=DABS((EPV(I,J,K)-PV_FLOW(I,J,K))/EPV(I,J,K))
                  ENDIF
                  IF(ERR1.GT.MXERR) MXERR=ERR1
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      MXPVERR=MXERR

      END
C*********************************************************************
      SUBROUTINE PEPV_ERR_MB(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                 KL2,KEYOUT,NBLK,EPV,PV_FLOW,POR,RC,EVOL,MXPVERR)
C*********************************************************************
C compute the maximum relative pore volume error between the flow model
C calculation and the poroelastic model calculation
C
C INPUT:
C   EPV(I,J,K) = PORE VOLUME COMPUTED IN THE POROELASTIC MODEL (BBL)
C   PV_FLOW(I,J,K) = PORE VOLUME COMPUTED IN THE FLOW MODEL (BBL)
C
C OUTPUT:
C   MXPVERR = MAXIMUM RELATIVEL PORE VOLUME ERROR IN THE CURRENT BLOCK
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'emodel.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM),JL1V(KDIM),JL2V(KDIM)
      REAL*8  EPV(IDIM,JDIM,KDIM),PV_FLOW(IDIM,JDIM,KDIM)
      REAL*8  MXPVERR
      REAL*8  POR(IDIM,JDIM,KDIM),RC(IDIM,JDIM,KDIM) ! SAUMIK,BGANIS
      REAL*8  EVOL(IDIM,JDIM,KDIM)

      INTEGER I,J,K
      REAL*8  ERR1,MXERR

!      WRITE(*,*)'In PEPV_ERR'
      ERR1=0.D0
      MXERR=0.D0
      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).GT.0) THEN
! SAUMIK - ONLY POROHEX ELEMENTS TALKING TO FLOW NEED BE CONSIDERED
                  IF(MBPOROE.AND.POR(I,J,K).EQ.0.D0) CYCLE
                  RC(I,J,K)=(EPV(I,J,K)-PV_FLOW(I,J,K))/EPV(I,J,K)
                  ERR1=DABS(RC(I,J,K))
                  RC(I,J,K)=RC(I,J,K)*EPV(I,J,K)/EVOL(I,J,K)
                  IF(ERR1.GT.MXERR) MXERR=ERR1
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      MXPVERR=MXERR

      END
C*********************************************************************
      SUBROUTINE FLOW_ERR_MB1(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                  KL2,KEYOUT,NBLK,PV,PVN,EVOL,RC)
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'emodel.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM),JL1V(KDIM),JL2V(KDIM)
      REAL*8  PV(IDIM,JDIM,KDIM),PVN(IDIM,JDIM,KDIM)
      REAL*8  RC(IDIM,JDIM,KDIM),EVOL(IDIM,JDIM,KDIM) ! SAUMIK,BGANIS

      INTEGER I,J,K

      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).GT.0) THEN
                  IF(PV(I,J,K).EQ.0.D0) CYCLE
                  RC(I,J,K)=(PV(I,J,K)-PVN(I,J,K))/EVOL(I,J,K)
                  ! SAUMIK,BGANIS
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      END

C*********************************************************************
      SUBROUTINE FLOW_ERR_MB2(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                  KL2,KEYOUT,NBLK,PV,PVN,RC)
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'emodel.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM),JL1V(KDIM),JL2V(KDIM)
      REAL*8  PV(IDIM,JDIM,KDIM),PVN(IDIM,JDIM,KDIM)
      REAL*8  RC(IDIM,JDIM,KDIM) ! SAUMIK,BGANIS

      INTEGER I,J,K

      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).GT.0) THEN
                  IF(PV(I,J,K).EQ.0.D0) CYCLE
                  RC(I,J,K)=PV(I,J,K)-PVN(I,J,K)
                  ! SAUMIK,BGANIS
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      END
C*********************************************************************
      SUBROUTINE COMPUTERESID(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &       KL2,KEYOUT,NBLK,RCV,RCP,BIOTAFLOW,MODULFLOW,POISSFLOW,EVOL)
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'emodel.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM),JL1V(KDIM),JL2V(KDIM)
      REAL*8  RCV(IDIM,JDIM,KDIM),RCP(IDIM,JDIM,KDIM)
      REAL*8  BIOTAFLOW(IDIM,JDIM,KDIM),MODULFLOW(IDIM,JDIM,KDIM),
     &        POISSFLOW(IDIM,JDIM,KDIM),EVOL(IDIM,JDIM,KDIM)

      INTEGER I,J,K
      REAL*8 V1,V2,V3,KDRINV

      RESIDP = 0.D0
      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).GT.0) THEN
               V1 = MODULFLOW(I,J,K)
               V2 = POISSFLOW(I,J,K)
               V3 = BIOTAFLOW(I,J,K)
               KDRINV = 3.D0*(1.D0-2.D0*V2)/V1
               RESIDP = RESIDP + V3**2.D0*KDRINV*RCP(I,J,K)**2.D0
     &                  *EVOL(I,J,K)
               RESIDP = RESIDP + 1.D0/KDRINV*RCV(I,J,K)**2.D0
     &                  *EVOL(I,J,K)
               RESIDP = RESIDP - 2.D0*V3*RCP(I,J,K)*RCV(I,J,K)
     &                  *EVOL(I,J,K)
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      END
C*********************************************************************
      SUBROUTINE PESETARYR8N(N_ARY,VAL,N)
C*********************************************************************
C CALL A WORK ROUTINE TO SET GRID ELEMENT ARRAY OF DIMENSION N TO VAL
C
C INPUT:
C   N_ARY = GRID ELEMENT ARRAY NUMBER
C   VAL = VALUE TO SET
C   N = 4TH DIMENSION OF THE GRID ELEMENT ARRAY
C*********************************************************************
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'

      INTEGER N_ARY,N
      REAL*8 VAL
      EXTERNAL SETARYR8N

      INTEGER JZERO(4)

      JZERO(1) = 3
      JZERO(2) = N_ARY
      JZERO(3) = N_R8U
      JZERO(4) = N_I4U
      I4UTIL = N
      R8UTIL = VAL
      CALL CALLWORK(SETARYR8N,JZERO)

      END


C********************************************************************
      SUBROUTINE EMASS_1NC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &               KL2,KEYOUT,NBLK,PVFLOW,M,DUNK)
C********************************************************************
      IMPLICIT NONE
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM), KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  M(IDIM,JDIM,KDIM), DUNK(IDIM,JDIM,KDIM)
      REAL*8  PVFLOW(IDIM,JDIM,KDIM)

      INTEGER I,J,K

!      WRITE(*,*)'In EMASS_1NC'
      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).LE.0) CYCLE
               IF(PVFLOW(I,J,K).LE. 1.D-10) CYCLE
               M(I,J,K)=PVFLOW(I,J,K)*DUNK(I,J,K)
            ENDDO
         ENDDO
      ENDDO

      END

C********************************************************************
      SUBROUTINE EMASS_2NC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &               KL2,KEYOUT,NBLK,PVFLOW,WMAS,OMAS,WDEN,ODEN)
C********************************************************************
      IMPLICIT NONE
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM), KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  WMAS(IDIM,JDIM,KDIM), OMAS(IDIM,JDIM,KDIM),
     &        WDEN(IDIM,JDIM,KDIM), ODEN(IDIM,JDIM,KDIM),
     &        PVFLOW(IDIM,JDIM,KDIM)

      INTEGER I,J,K

      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).LE.0) CYCLE
               WMAS(I,J,K)=PVFLOW(I,J,K)*WDEN(I,J,K)
               OMAS(I,J,K)=PVFLOW(I,J,K)*ODEN(I,J,K)
            ENDDO
         ENDDO
      ENDDO

      END

C********************************************************************
      SUBROUTINE SCALE_1NC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                     KL2,KEYOUT,NBLK,EPV,M,DUNK)
C********************************************************************
      IMPLICIT NONE
      REAL*8  ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  M(IDIM,JDIM,KDIM),    DUNK(IDIM,JDIM,KDIM)
      REAL*8  EPV(IDIM,JDIM,KDIM)

      INTEGER I,J,K

!      WRITE(*,*)'In SCALE_1NC'
      DO K = KL1,KL2
         DO J = JL1V(K),JL2V(K)
            DO I = IL1,IL2
               IF(KEYOUT(I,J,K).GT.0.AND.EPV(I,J,K).GT.1.D-10) THEN
                  DUNK(I,J,K) = M(I,J,K) / EPV(I,J,K)
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      END

C********************************************************************
      SUBROUTINE SCALE_2NC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                     KL2,KEYOUT,NBLK,EPV,WMAS,OMAS,WDEN,ODEN)
C********************************************************************
      IMPLICIT NONE
      REAL*8  ZERO
      PARAMETER (ZERO=0.D0)
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  WMAS(IDIM,JDIM,KDIM), OMAS(IDIM,JDIM,KDIM),
     &        WDEN(IDIM,JDIM,KDIM), ODEN(IDIM,JDIM,KDIM)
      REAL*8  EPV(IDIM,JDIM,KDIM)

      INTEGER I,J,K

      DO K = KL1,KL2
         DO J = JL1V(K),JL2V(K)
            DO I = IL1,IL2
               IF(KEYOUT(I,J,K).GT.0.AND.EPV(I,J,K).GT.ZERO) THEN
                  WDEN(I,J,K) = WMAS(I,J,K) / EPV(I,J,K)
                  ODEN(I,J,K) = OMAS(I,J,K) / EPV(I,J,K)
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      END

C********************************************************************
      SUBROUTINE MANDEL_BC(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                     KL2,KEYOUT,NBLK,MANDELDISP,XC,YC,ZC,
     &                     NODE_LID,KEYOUT_CR,TYP,PRESSVAL)
C********************************************************************
      IMPLICIT NONE
      include 'control.h'
      include 'layout.h'
      include 'emodel.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  MANDELDISP(IDIM,KDIM,3),XC(IDIM+1,JDIM+1,KDIM+1),
     &        YC(IDIM+1,JDIM+1,KDIM+1),ZC(IDIM+1,JDIM+1,KDIM+1)
      INTEGER NODE_LID(IDIM,JDIM,KDIM),KEYOUT_CR(IDIM,JDIM,KDIM),
     &        TYP(IDIM,KDIM,3)
      INTEGER NERR,NITEMS
      REAL*8  NU,NUU,SERIAL_A(1000),MU,UX,UY,UXN,UYN,PARA_B,
     &        FORCE,MODULUS,PERM,XSIZE,YSIZE,CURRENT_TIME,
     &        XCORD,YCORD,P
      INTEGER I,J,K,DIR
      REAL*8  M2INCH,PA2PSI
      REAL*8 PRESSVAL(IDIM,JDIM,KDIM)
      PARAMETER (M2INCH=39.37007874016D0)
!     METER TO INCH
      PARAMETER (PA2PSI=0.0001450378911491D0)
      ! PASCAL TO PSI


      REAL*8 PREVIOUS_TIME,MINUY,MAXUY
      LOGICAL ONCEONLY
      DATA ONCEONLY /.TRUE./, PREVIOUS_TIME/0.D0/

      IF (NSTEP.EQ.0) THEN
        WRITE(*,*)'Skipping MANDEL_BC at initialization'
        RETURN
      ENDIF

      MINUY=1.E30
      MAXUY=-1.E30

      NU = MANDEL_NU
!     DRAINED POISSON RATIO

      NUU = MANDEL_NUU  ! FOR ALPHA=0.8
!     UNDRAINED POISSON RATIO

      SERIAL_A = 0.D0
      MU = MANDEL_MU
!     VISCOSITY IN CENTIPOISE

      PARA_B = SKEMPTON
!     SKEMPTON COEFFICIENT

      FORCE = MANDEL_FORCE
!     FORCE INTENSITY IN NEWTON/METER

      MODULUS = MANDEL_E/PA2PSI
!     YOUNG MODULUS IN PA

      PERM = MANDEL_PERM
!     PERMEABILITY IN MILLIDARCY

      XSIZE = MANDEL_XSIZE
!     A IN METER

      YSIZE = MANDEL_YSIZE
!     B IN METER

C TRUNCATE AT THE FIRST 50 ITEMS IN THE SERIES SOLUTION

      NITEMS = 50
      CALL MANDEL_CAL_A(NU,NUU,SERIAL_A,NITEMS)
      ! 50 ITEMS IN SERIES SOLUTION

      CURRENT_TIME = (TIM+DELTIM)*24.D0*3600.D0

      IF (ONCEONLY) PREVIOUS_TIME = CURRENT_TIME

C +Y FACE, TIME-DEPENDENT DISPLACEMENT

      DO K = 1,KDIM
         DO I = IL1,IL2+1
            DO DIR = 1,3
            IF(TYP(I,K,DIR).EQ.2) THEN
            ! DIRICHLET BND
            DO J = JDIM-1,1,-1
               IF(NODE_LID(I,J,K).GT.0 .AND.
     &            KEYOUT_CR(I,J+1,K).EQ.0) THEN

                  ! NODES CORRESPONDING TO Y+ FACE
                  XCORD = XC(I,J,K)*12.D0/M2INCH
                  YCORD = YC(I,J,K)*12.D0/M2INCH
                  ! CONVERTING FEET TO METER

                  IF (ONCEONLY) THEN
                    UYN=0.D0
                    CALL MANDEL_SOLN(CURRENT_TIME,XCORD,YCORD,
     &                             PARA_B,FORCE,MODULUS,PERM,
     &                             XSIZE,YSIZE,NU,NUU,MU,UX,UY,
     &                             P,STRSSYY,SERIAL_A,NITEMS,
     &                             EPSXX,EPSYY)
                  ELSE
                    CALL MANDEL_SOLN(PREVIOUS_TIME,XCORD,YCORD,
     &                             PARA_B,FORCE,MODULUS,PERM,
     &                             XSIZE,YSIZE,NU,NUU,MU,UXN,UYN,
     &                             P,STRSSYY,SERIAL_A,NITEMS,
     &                             EPSXX,EPSYY)
                    CALL MANDEL_SOLN(CURRENT_TIME,XCORD,YCORD,
     &                             PARA_B,FORCE,MODULUS,PERM,
     &                             XSIZE,YSIZE,NU,NUU,MU,UX,UY,
     &                             P,STRSSYY,SERIAL_A,NITEMS,
     &                             EPSXX,EPSYY)
                  ENDIF

                  UXN = UXN*M2INCH/12.D0
                  UX = UX*M2INCH/12.D0
                  ! CONVERTING METER TO FEET

                  UYN = UYN*M2INCH/12.D0
                  UY = UY*M2INCH/12.D0
                  ! CONVERTING METER TO FEET

                  IF (UY.GT.MAXUY) MAXUY=UY
                  IF (UY.LT.MINUY) MINUY=UY

                  MANDELDISP(I,K,DIR) = UY-UYN
                  ! IMPOSE TIME DEPENDENT BC

                  EXIT
                  ! EXIT !!
               ENDIF
            ENDDO
            ENDIF
            ENDDO
         ENDDO
      ENDDO

      IF (ONCEONLY) ONCEONLY=.FALSE.
      PREVIOUS_TIME=CURRENT_TIME

      ! CALL ONCE MORE TO GET ANALYTIC POROELASTIC SOLUTION
      ! FOR PORE PRESSURE AT CERTAIN CELL-CENTER
      I=IL1; J=JL1V(KL1); K=KL1
      XCORD=0.5D0*(XC(I,J,K)+XC(I+1,J,K))
      YCORD=0.5D0*(YC(I,J,K)+YC(I,J+1,K))

      XCORD = XCORD*12.D0/M2INCH
      YCORD = YCORD*12.D0/M2INCH
      ! CONVERTING FEET TO METER

      CALL MANDEL_SOLN(CURRENT_TIME,XCORD,YCORD,
     &               PARA_B,FORCE,MODULUS,PERM,
     &               XSIZE,YSIZE,NU,NUU,MU,UX,UY,
     &               P,STRSSYY,SERIAL_A,NITEMS,
     &               EPSXX,EPSYY)
      PRESVAL=P

      ! CALL ONCE MORE TO GET ANALYTIC POROELASTIC SOLUTION
      ! FOR DISPLACEMENTS AT X=A, Y=0 NODE
      I=IL2+1; J=JL1V(KL1); K=KL1
      XCORD=XC(I,J,K)
      YCORD=YC(I,J,K)

      XCORD = XCORD*12.D0/M2INCH
      YCORD = YCORD*12.D0/M2INCH
      ! CONVERTING FEET TO METER

      CALL MANDEL_SOLN(CURRENT_TIME,XCORD,YCORD,
     &               PARA_B,FORCE,MODULUS,PERM,
     &               XSIZE,YSIZE,NU,NUU,MU,UX,UY,
     &               P,STRSSYY,SERIAL_A,NITEMS,
     &               EPSXX,EPSYY)
      UXVAL=UX

! SAUMIK - PRESSVAL GRID ELEMENT ARRAY REQUIRED FOR ERROR NORM
      DO K=KL1,KL2
      DO J=JL1V(K),JL2V(K)
      DO I=IL1,IL2

      XCORD=0.5D0*(XC(I,J,K)+XC(I+1,J,K))
      YCORD=0.5D0*(YC(I,J,K)+YC(I,J+1,K))

      XCORD = XCORD*12.D0/M2INCH
      YCORD = YCORD*12.D0/M2INCH
      ! CONVERTING FEET TO METER

      CALL MANDEL_SOLN(CURRENT_TIME,XCORD,YCORD,
     &                 PARA_B,FORCE,MODULUS,PERM,
     &                 XSIZE,YSIZE,NU,NUU,MU,UX,UY,
     &                 P,STRSSYY,SERIAL_A,NITEMS,
     &                 EPSXX,EPSYY)
      PRESSVAL(I,J,K)=P
      ENDDO
      ENDDO
      ENDDO

      END

C********************************************************************
      SUBROUTINE MANDEL_CAL_A(NU,NUU,A,N)
C********************************************************************
      IMPLICIT NONE

      INTEGER I,N
      REAL*8 A(1000),CC,PI,NU,NUU,X0,ERROR,LEFT,RIGHT,MIDDLE,
     &       ERRORMOD

! isotropy
      CC = (1-NU)/(NUU-NU)
      ! TAN(BETA_I)/BETA_I

! transverse isotropy
!      CC = 7.5375D0

      PI = 3.1415926535897932D0

C     BISECTION METHOD TO FIND ROOTS TO TAN(A) - CC*A = 0
C     EQUIVALENT TO F(X) = 0

      A(1:1000) = 0.D0
      ERRORMOD = 1.0
      LEFT = 0.D0
      RIGHT = PI/2.D0
      MIDDLE = (LEFT+RIGHT)/2.D0
      DO I = 1,N
         DO WHILE (ERRORMOD > 1.0D-8)

            error = cc*middle -tan(middle)
            if (error>1.d-10) then
               left = middle
            elseif (error<-1.d-10) then
               right = middle
            else
               left = middle
               right = middle
            endif

            MIDDLE = (LEFT+RIGHT)/2.D0
            ERRORMOD = ABS(TAN(MIDDLE) - CC*MIDDLE)
            X0 = MIDDLE
         END DO
         A(I) = X0
         ERRORMOD = 1.0
         LEFT = X0+PI
         RIGHT = (I+1/2.D0)*PI
         MIDDLE = (LEFT + RIGHT)/2.D0
      END DO

      END

C********************************************************************
      SUBROUTINE MANDEL_SOLN(TIME,X,Y,B,F,EE,PERM,L,LY,NU,NUU,MUF,
     &                       UX,UY,P,STRSS_YY,A,N,EPS_XX,EPS_YY)
C********************************************************************
      IMPLICIT NONE

      REAL*8 ZERO_D
      REAL*8 ONE_D
      REAL*8 HALF_D
      REAL*8 TWO_D
      PARAMETER (ZERO_D=0.D0,ONE_D=1.0D0,HALF_D=0.5D0,TWO_D=2.0D0)

      INTEGER I,N
      REAL*8 PI,B,F,EE,K,L,LY,NU,NUU,X0,PERM,MUF
      REAL*8 C1,C2,C3,C4,C5,C6,C7,C8,C9,B1,D2,S1,S2,S3,S4,S5
      REAL*8 UX,UY,P,STRSS_YY,MU
      REAL*8 A(1000),C,TIME,X,Y,ALPHA,EPS_XX,EPS_YY

! TRANSVERSE ISOTROPY
! MANDEL
!      REAL*8 PA2PSI
!      PARAMETER (PA2PSI=0.0001450378911491D0)
!      ! PASCAL TO PSI

! ISOTROPY
      K = PERM*9.869233D-16
      ! PERMEABILITY; MILLIDARCY TO M^2 CONVERSION

      MU = MUF*0.001D0
      ! VISCOSITY; CENTIPOISE TO POISE CONVERSION

      PI = 3.1415926535897932D0

      C = K*B*B*EE*(1-NU)*(1+NUU)**2.D0/(9.D0*(1+NU)*(1-NUU)*
     &    (NUU-NU)*MU)
      ! DIFFUSIVITY COEFFICIENT

      C1 = F*NU*(1+NU)/EE/L
      ! F*NU/(2*G*A)

      C2 = -2*F*(1+NU)*NUU/EE/L
      ! -F*NUU/(G*A)

      C3 = 2*F*(1+NU)/EE
      ! F/G

      C4 = -F*(1-NU)*(1+NU)/EE/L
      ! -F*(1-NU)/(2*G*A)

      C5 = 2*F*(1+NU)*(1-NUU)/EE/L
      ! F*(1-NUU)/(G*A)

      C6 = -F/L

      C7 = -2*F*(NUU-NU)/L/(1-NU)
      C8 = 2*F/L

      C9 = 2*F*B*(1+NUU)/3.D0/L
      ! 2*F*B*(1+NUU)/(3*A)

      UX = C1*X
      UY = C4*LY
      P = 0.D0
      STRSS_YY = C6
      EPS_XX = C1
      EPS_YY = C4

! TRANSVERSE ISOTROPY
! MANDEL

!      C5 = -F/L*24.1D9*PA2PSI/
!     &      (24.1D9*PA2PSI*21.D9*PA2PSI-(7.62D9*PA2PSI)**2.D0)
!      UY = C5*LY
!      C6 = C5*2.D0*(.5172D0/3.8987D0-1.D0)
!      P = 0.D0

      DO I = 1,N
         S2 = SIN(A(I))/(A(I)-SIN(A(I))*COS(A(I)))
         ! SIN(BETA_I)/(BETA_I - SIN(BETA_I) * COS(BETA_I))

         D2 = COS(A(I))/(A(I)-SIN(A(I))*COS(A(I)))

         S3 = EXP(-A(I)*A(I)*C*TIME/L/L)
         ! EXP(-BETA_I^2 * C * T / A^2)

         UX = UX + C2*S2*COS(A(I))*S3*X +
     &             C3*D2*SIN(A(I)*X/L)*S3

         EPS_XX = EPS_XX + C2*S2*COS(A(I))*S3 +
     &            C3*D2*A(I)/L*COS(A(I)*X/L)*S3

         UY = UY + C5*S2*COS(A(I))*S3*LY
         ! TIME DEPENDENT Y-DISPLACEMENT BC AT +Y FACE

         EPS_YY = EPS_YY + C5*S2*COS(A(I))*S3

         P = P + C9*S2*S3*(COS(A(I)*X/L)-COS(A(I)))

         B1 = C7*S2*S3*COS(A(I)*X/L) + C8*S2*COS(A(I))*S3

         STRSS_YY = STRSS_YY + B1

      END DO

      END

C********************************************************************
      SUBROUTINE MANDEL_PRINT(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,
     &           KL1,KL2,KEYOUT,NBLK,KEYOUT_CR,EDISP,PRES,XC,YC,ZC,
     &           NODE_LID,STRESS,STRAIN)
C********************************************************************
      IMPLICIT NONE
      include 'control.h'
      include 'layout.h'
      include 'emodel.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  EDISP(IDIM,JDIM,KDIM,3),PRES(IDIM,JDIM,KDIM)
      REAL*8  XC(IDIM+1,JDIM+1,KDIM+1),YC(IDIM+1,JDIM+1,KDIM+1),
     &        ZC(IDIM+1,JDIM+1,KDIM+1)
      INTEGER KEYOUT_CR(IDIM,JDIM,KDIM),NERR,I,J,K
      INTEGER NODE_LID(IDIM,JDIM,KDIM)
      REAL*8  CURRENT_TIME,XCORD,YCORD,XCENTER
      REAL*8  M2INCH,PA2PSI
      REAL*8 STRESS(IDIM,JDIM,KDIM,6),STRAIN(IDIM,JDIM,KDIM,6)
      REAL*8 TOL
      DATA TOL /1.D-8/
      PARAMETER (M2INCH=39.37007874016D0)
      ! METER TO INCH

      PARAMETER (PA2PSI=0.0001450378911491D0)
      ! PASCAL TO PSI

      INTEGER :: NDISPOUT = 50
      INTEGER :: NPRESOUT = 60

      LOGICAL ONCEONLY
      DATA ONCEONLY/.TRUE./

      REAL*8 FORCE,XSIZE,NU,MODULUS,SHEAR,FACTOR

      IF(MYPRC.NE.0) RETURN

      NU = 0.2
!     DRAINED POISSON RATIO

      MODULUS = 5.94E+09
!     YOUNG MODULUS IN PA

      FORCE = MANDEL_FORCE
!     FORCE INTENSITY IN NEWTON/METER

      XSIZE = MANDEL_XSIZE
!     A IN METER

      SHEAR = MODULUS/(2.D0*(1.D0+NU))
!     SHEAR MODULUS IN PA

      FACTOR = 2.D0*SHEAR/FORCE
!     MULTIPLIER TO DISPLACEMENT FOR OUTPUT

      IF (ONCEONLY) THEN
        OPEN(NDISPOUT,FILE='PEDISP_FS.DAT',STATUS='unknown')
        OPEN(NPRESOUT,FILE='PEPRESS_FS.DAT',STATUS='unknown')
        ONCEONLY = .FALSE.
!        WRITE(NPRESOUT,*)'TIME,COMPUTED PRESSURE,ANALYTIC
!     &  PRESSURE,AP/F(COMPUTED),AP/F(ANALYTIC)'
!        WRITE(NPRESOUT,*)'TIME,COMPUTED PRESSURE,ANALYTIC PRESSURE'
!        WRITE(NDISPOUT,*)'TIME,COMPUTED DISPX,ANALYTIC DISPX'
      ELSE
        OPEN(NDISPOUT,FILE='PEDISP_FS.DAT',STATUS='old',
     &    ACCESS='append')
        OPEN(NPRESOUT,FILE='PEPRESS_FS.DAT',STATUS='old',
     &    ACCESS='append')
      ENDIF

      CURRENT_TIME = TIM*24.D0*3600.D0
      ! DAY TO SECONDS

C PRINTOUT PORE PRESSURE

      I=IL1; J=JL1V(KL1); K=KL1
      WRITE(NPRESOUT,'(0P,F9.3,A,E15.8,A,E15.8,A,E15.8,A,E15.8)')
     &CURRENT_TIME,',',PRES(I,J,K),',',PRESVAL*PA2PSI,
     &',',PRES(I,J,K)/PA2PSI*XSIZE/FORCE,
     &',',PRESVAL*XSIZE/FORCE ! 1P --> 0P ...!
!      WRITE(NPRESOUT,'(0P,F9.2,A,E15.8,A,E15.8)')CURRENT_TIME,',',
!     &  PRES(I,J,K),',',PRESVAL*PA2PSI

C PRINTOUT X-DISPLACEMENT

      I=IL2+1; J=JL1V(KL1); K=KL1
      WRITE(NDISPOUT,'(0P,F9.2,A,E15.8,A,E15.8)')
     &CURRENT_TIME,',',EDISP(I,J,K,1)*12.D0/M2INCH*FACTOR,
     &',',UXVAL*FACTOR

      CLOSE(NDISPOUT)
      CLOSE(NPRESOUT)

      END

C*********************************************************************
      SUBROUTINE MANDEL_ERR(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,KL1,
     &                  KL2,KEYOUT,NBLK,PRESSVAL,PRESS,MXPVERR,EPV,
     &                  BIOTA,EVOL,MODUL,POISS,VSTRAIN,POR)
C*********************************************************************
      IMPLICIT NONE

      INCLUDE 'emodel.h'
      INCLUDE 'tfluidsc.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM),JL1V(KDIM),JL2V(KDIM)
      REAL*8  PRESSVAL(IDIM,JDIM,KDIM),PRESS(IDIM,JDIM,KDIM)
      REAL*8  EPV(IDIM,JDIM,KDIM),BIOTA(IDIM,JDIM,KDIM),
     &        EVOL(IDIM,JDIM,KDIM),MODUL(IDIM,JDIM,KDIM),
     &        POISS(IDIM,JDIM,KDIM),VSTRAIN(IDIM,JDIM,KDIM),
     &        POR(IDIM,JDIM,KDIM)
      REAL*8  MXPVERR

      INTEGER I,J,K
      REAL*8  DIFF,TOTERR,KDRINV,PHI,MINV,PHI_INIT

      REAL*8 PA2PSI
      PARAMETER (PA2PSI=0.0001450378911491D0)
      ! PASCAL TO PSI

      DIFF=0.D0
      TOTERR=0.D0

      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF(KEYOUT(I,J,K).NE.1) CYCLE
! SAUMIK - INVERSE OF BULK MODULUS; PHI;
!          INVERSE OF CONSTRAINED STORAGE COEFFICIENT
               KDRINV=3.D0*(1.D0-2.D0*POISS(I,J,K))/MODUL(I,J,K)
               PHI=EPV(I,J,K)/((1.D0+VSTRAIN(I,J,K))*EVOL(I,J,K))
               MINV=(1.D0-BIOTA(I,J,K))*(BIOTA(I,J,K)-PHI)*KDRINV
     &              +PHI*FLCMP
! SAUMIK - DIFFERENCE IN PSI; ANALYTICAL - NUMERICAL
               DIFF=PRESSVAL(I,J,K)*PA2PSI-PRESS(I,J,K)
! SAUMIK - L^2
               DIFF=DIFF*EVOL(I,J,K)*MINV
               TOTERR=TOTERR+DIFF**2.D0
            ENDDO
         ENDDO
      ENDDO

      MXPVERR=TOTERR

      END

C bag8, djw
C*********************************************************************
      SUBROUTINE EUPDATECR(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,
     &                 KL1,KL2,KEYOUT,NBLK,MODUL,POISS,BIOTA,
     &                 PV,ECR,EVOL,VSTRAIN)
C*********************************************************************
C Update CR array when using CR_TYPE > 1
C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'layout.h'

      INCLUDE 'emodel.h'
      INCLUDE 'ebdary.h'

      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER JL1V(KDIM),JL2V(KDIM),  KEYOUT(IDIM,JDIM,KDIM)
      REAL*8  MODUL(IDIM,JDIM,KDIM),  POISS(IDIM,JDIM,KDIM)
      REAL*8  BIOTA(IDIM,JDIM,KDIM),  ECR(IDIM,JDIM,KDIM)
      REAL*8  PV(IDIM,JDIM,KDIM),     EVOL(IDIM,JDIM,KDIM)
      REAL*8  VSTRAIN(IDIM,JDIM,KDIM)

      INTEGER I,J,K,JL1,JL2,IOFF,JOFF,KOFF,NERR
      REAL*8  V1,V2,V3,V4,U1,U2,X,ZERO,ONE,TWO,THREE,VP,KDRINV
      REAL*8  DX,DY,DZ,VB,LAMBDAVAL,MUVAL

! TRANSVERSE ISOTROPY
! MANDEL
      REAL*8 PA2PSI
      PARAMETER (PA2PSI=0.0001450378911491D0)
      ! PASCAL TO PSI

      IF (CR_TYPE.LE.1) RETURN

      CALL BLKOFF(NBLK,IOFF,JOFF,KOFF,NERR)

      DO K = KL1,KL2
        JL1 = JL1V(K)
        JL2 = JL2V(K)
        DO J = JL1,JL2
          DO I = IL1,IL2
            IF (KEYOUT(I,J,K).LE.0) CYCLE
            VP = PV(I,J,K)/EVOL(I,J,K)
            V1 = MODUL(I,J,K)
            V2 = POISS(I,J,K)
            X = 1.0D0 - 2.0D0 * V2
            IF (X.GE.ZERO) THEN
              LAMBDAVAL = V2 * V1 / ((1.0D0 + V2)* X)
            ELSE
              LAMBDAVAL = 1.0D15
            ENDIF
            MUVAL = 0.5D0 * V1 / (1.D0 + V2)
            V3 = BIOTA(I,J,K)
            KDRINV = 3.D0*(1.D0-2.D0*V2)/V1
            IF (CR_TYPE.EQ.2) THEN
! ISOTROPIC
              IF(NSTEP.EQ.0) THEN ! EIVDAT
              ECR(I,J,K)=KDRINV*(V3-VP)*(1.D0+VSTRAIN(I,J,K))
              ELSE ! ESTEP
              ECR(I,J,K)=KDRINV*(V3*(1.D0+VSTRAIN(I,J,K))-VP)
              ENDIF
! TRANSVERSELY ISOTROPIC
!               ECR(I,J,K)=.9854D-9/PA2PSI
            ENDIF
            ECR(I,J,K)=ECR(I,J,K)*EVOL(I,J,K)
          ENDDO
        ENDDO
      ENDDO

      END
