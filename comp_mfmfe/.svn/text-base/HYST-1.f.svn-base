C  Option 1 based on Parker-Lenhard model  (IHYST =1)
      PROGRAM  HYST1
C                             
C     ------------------------------------------------------------------
C     PURPOSE : CALCULATES RELATIVE PERMEABILITY AND CAPILLARY PRESSURE 
c               using Brooks-Corey model
C         3/3/2009 can handle  STRONGLY WATER-WET MEDIA
C         3/3/2009  can handle  GAS/WATER 
c
C     CALL    : NONE
C     ------------------------------------------------------------------
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension s(20,2),sr(20,2),prc(20),p(20,2)
      dimension rperm(20,2),sgt(20),sgtrap(20)
      dimension swnorm(20),swmin(20),rterm(20)
      dimension s1rw(20),s2rw(20),swbar(20)
      dimension stnorm(20),wksp1(20),pd(20)
      dimension rpermg(20)
C
c   input data
c     no. of cells NBL
c     water saturation s(i,1), residual water and gas sat s1rw(i),s2rw(i)
c     water pressure (p(i,1)
c     capillary entry pressure, pd
c     cap. pressure and rel. perm exponent, lambda
c     hyst flag (ihyst)
c     time step (icnt)
c
c     read input data
      ONEM6 = 0.000001
      ZERO = 0.0
      ONE  = 1.0
      NBL =   5
      ICNT = 2
      IHYST = 1
      LAMBDA = 2.0
      do 1 I = 1,NBL
         S(I,1) = 1.0
         S1RW(I) = 0.2
         S2RW(I) = 0.2
         P(I,1) = 14.7
         PD(I) = 0.2
         RPERMG(I) = 0.4
  1   CONTINUE       
c*****************************************
c     
C     INITIALIZE SOME ARRAYS
C      WATER : phase 1
c      GAS   : phase 2
c
      DO 10 L = 1,2
      DO 10 I = 1,NBL
         PRC(I) = 0.0    ! capillary pressure
         RPERM(I,L) = 0.0  ! relative permeability
10    CONTINUE
      DO 20 I = 1,NBL
         P(I,2) = P(I,1)
         S(I,2) = 1-S(I,1)
         RTERM(I) = 0.0
         SGT(I) = 0.0
         SgTRAP(I) = 0.0
         SR(I,1) = S1RW(I)  !  residual water 
         SR(I,2) = S2RW(I)  !  max residual gas corresponding to main imbibition
 20   CONTINUE
C
C     INITIALIZE THE MIN. WATER SAT. AND TRAPPED gas SATURATION (AT TIME 0)
C     MAIN DRAINAGE: NO TRAPPED gas, SWMIN=SWI
C
      IF (ICNT.EQ.1) THEN
         DO 42 I = 1,NBL
            SWNORM(I) = (S(I,1)-SR(I,1))/(1.0-SR(I,1))
            SWNORM(I) = MAX(ZERO,SWNORM(I))
            SWNORM(I) = MIN(ONE,SWNORM(I))
            SWMIN(I) = SWNORM(I)          ! track min water saturation
            SGT(I) = 0.0
42       CONTINUE
         GO TO 450 
      ENDIF
C
      IF (IHYST.EQ.0) THEN
         DO 410 I = 1,NBL
            IF (S(I,2).GE.ONEM6) THEN
               SGT(I) = SR(I,2)/(1.-SR(I,1))
            ENDIF
 410     CONTINUE
      ENDIF
C
C     COMPUTE TRAPPED gas SATURATION (SgT)
C     DETERMINE DRAINAGE OR IMBIBITION DIRECTION
C
      IF (IHYST.EQ.1) THEN         ! with hysteresis
         DO 420 I = 1,NBL
            IF (S(I,2).GE.ONEM6) THEN
               IF (SR(I,2).GT.ONEM6) THEN
                  WKSP1(I) = S2RW(I)/(1.-SR(I,1))
                  RTERM(I) = ONE/WKSP1(I)-1.  ! Land Eq. R term
               ENDIF
               IF (SWNORM(I).LE.SWMIN(I)) THEN  ! moving along drainage
                  SWMIN(I) = SWNORM(I)  ! reset historical min sat.
                  SGTRAP(I) = 0.0
               ELSE
                  SGTRAP(I) = (1.-SWMIN(I))/(1.+RTERM(I)*(1.-SWMIN(I)))   
               ENDIF
            ELSE
               SWMIN(I) = SWNORM(I)
               SgTRAP(I) = 0.0
            ENDIF
420      CONTINUE
c
C     CALCULATE TRAPPED GAS SATURATION AT NONZERO CAPILLARY PRESSURE
C
         DO 440 I = 1,NBL
               IF (SWNORM(I).LE.SWMIN(I)) THEN  !  drainage
                  SGT(I) = 0.0
               ELSE    !  Lenhard interpolation method
                  IF (SGTRAP(I)+SWMIN(I).GE.ONE) THEN
                     SGT(I) = 0.0 
                  ELSE
                     SGT(I) = SGTRAP(I)*(SWNORM(I)-SWMIN(I))/
     *                    (1.-SGTRAP(I)-SWMIN(I))
                     SGT(I) = MAX(ZERO,SGT(I))
                  ENDIF
               ENDIF
 440     CONTINUE
      ENDIF
450   CONTINUE
C
C     COMPUTE APPARENT WETTING (WATER) PHASE SATURATION (SWBAR)
C
      DO 460 I = 1,NBL
         SWBAR(I) = SWNORM(I)+SGT(I)
         SWBAR(I) = MIN(ONE,SWBAR(I))
 460  CONTINUE
C
C     WATER RELATIVE PERMEABILITY
C     use Burdine equation
C
      DO 470 I = 1,NBL
         RPERM(I,1) = SWNORM(I)**(2.+3.*LAMBDA)/LAMBDA
470   CONTINUE
C
C     GAS RELATIVE PERMEABILITY AND CAPILLARY PRESSURE IF PRESENT
C 
      DO 480 I = 1,NBL
         IF (S(I,2).GT.ONEM6) THEN
c   water/gas capillary pressure
            IF (SWBAR(I).GT.PRCSN) PRC(I) = PD(I)*SWBAR(I)
     *                               **(-1.d00/LAMBDA) 
            P(I,2) = P(I,1)+PRC(I)
            STNORM(I) = SWNORM(I)+S(I,2)/(1.-SR(I,1))
            STNORM(I) = MIN(ONE,STNORM(I))
            IF (STNORM(I).LE.SWBAR(I)) THEN
               RPERM(I,2) = 0.0
            ELSE
               RPERM(I,2) = RPERMG(I)*(1.-SWBAR(I))*(1-SWBAR(I))*(1.
     *               -SWBAR(I)**((2.+LABMDA)/LAMBDA))
            endif
         ENDIF
 480  CONTINUE
C
C     CHECK FOR SINGLE PHASE BLOCKS
C
      DO 520 I = 1,NBL
         IF (S(I,1).GE.ONE) THEN
            RPERM(I,1) = 1.0
            RPERM(I,2) = 0.0
         ENDIF
 520  CONTINUE
      DO 530 I = 1,NBL
         IF (S(I,2).GE.ONE) THEN
            RPERM(I,2) = 1.0
         ENDIF
  530 CONTINUE
c 
C
      END
