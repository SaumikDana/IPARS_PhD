C  TSTEP.F - MAKE ONE TIME STEP WITH THE IMPLICIT SINGLE PHASE FLOW MODEL

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE TSTEP1   (NERR)
C  SUBROUTINE TSTEP2   (KONVG,NERR)
C  SUBROUTINE TSTEP3   (KONVG,NERR)

C  CODE HISTORY:

C  BAHAREH MOMKEN 3/16/99  hydrology(IMPES) gstep.df is used as template
C  JOHN WHEELER   4/03/99  IMPLICIT SINGLE PHASE MODEL
C  JOHN WHEELER   6/22/99  SPLIT TSTEP TO 3 ROUTINES FOR MULTIMODEL

C ***********************************************************************
        SUBROUTINE TSTEP1 (NERR)
C************************************************************************

C  Implicit single-phase model exective routine to start a time step.
C  1.  Compute Jacobian and residual for Newtonian iteration.
C  2.  Start inter-block calculations.
C
C  NERR = ERROR KEY STEPPED BY ONE FOR EACH ERROR
C         (INPUT AND OUTPUT, INTEGER )
C*********************************************************************
      IMPLICIT NONE
C    INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'
      INCLUDE 'wells.h'
      INCLUDE 'mpfaary.h'
      INCLUDE 'tarydat.h'
      INCLUDE 'tbaldat.h'
      INCLUDE 'terrcalc.h'
      INCLUDE 'earydat.h'

      LOGICAL ONCEONLY
      DATA ONCEONLY /.TRUE./

      INTEGER IPROP(20), ITRAN(18), IWELL(6), IWELSUM(4)

      INTEGER NERR

      DATA IPROP   / 20*0 /
      DATA ITRAN   / 18*0 /
      DATA IWELL   / 6*0 /
      DATA IWELSUM / 4*0 /

c list all routines to be called by callwork:

      EXTERNAL TPROP, TTRAN, TWELL, TWELSUMS, DEBUG_GEA
C      EXTERNAL TSPRB3

c  define work routine arguments

      IF(ONCEONLY) THEN
         ONCEONLY = .FALSE.

         IF (LEVELE.AND.BUGKEY(1)) WRITE (NFBUG,100)'PROC',MYPRC,
     &    ' ENTERING SUBROUTINE TSTEP1, OLD TAG =',MSGTAG(13+1)
 100     FORMAT(A,I,A,I)

         IPROP(1) = 19
         IPROP(2) = N_FLDEN
         IPROP(3) = N_PRES
         IPROP(4) = N_FLDENN
         IPROP(5) = N_PRESN
         IPROP(6) = N_COF
         IPROP(7) = N_RESID
         IPROP(8) = N_PV
         IPROP(9) = N_PVN
         IPROP(10) = N_CR
         IPROP(11) = N_POR
         IPROP(12) = N_PORTRUE
         IPROP(13) = N_PRESN3
         IPROP(14) = N_VSTRAIN
         IPROP(15) = N_VSTRAINN
         IF(MBPOROE) THEN
         IPROP(16) = N_BIOTAFLOW
         IPROP(17) = N_MODULFLOW
         IPROP(18) = N_POISSFLOW
         ELSE
         IPROP(16) = N_BIOTA
         IPROP(17) = N_MODUL
         IPROP(18) = N_POISS
         ENDIF
         IPROP(19) = N_EVOL
         IPROP(20) = N_PVN3

         ITRAN(1) = 17
         ITRAN(2) = N_KCR
         ITRAN(3) = N_VPROP
         ITRAN(4) = N_VDIM
         ITRAN(5) = N_FPROP
         ITRAN(6) = N_FDIM
         ITRAN(7) = N_PERMINV
         ITRAN(8) = N_XC
         ITRAN(9) = N_YC
         ITRAN(10) = N_ZC
         ITRAN(11) = N_FLDEN
         ITRAN(12) = N_PRES
         ITRAN(13) = N_AINVF
         ITRAN(14) = N_TRAN
         ITRAN(15) = N_AINV
         ITRAN(16) = N_COF
         ITRAN(17) = N_RESID
         ITRAN(18) = N_UPMOBPROD

         IWELL(1) = 5
         IWELL(2) = N_DEPTH
         IWELL(3) = N_PRES
         IWELL(4) = N_FLDEN
         IWELL(5) = N_COF
         IWELL(6) = N_RESID

         IWELSUM(1) = 3
         IWELSUM(2) = N_DEPTH
         IWELSUM(3) = N_PRES
         IWELSUM(4) = N_FLDEN

      ENDIF

C  START NEWTONIAN ITERATION LOOP
C  EVALUATE DENSITY AND ACCUMULATION TERMS

      CALL TIMON(21)
      CURRENT=0.D0
      CALL CALLWORK(TPROP,IPROP)
      CALL TIMOFF(21)

C  EVALUATE PROPERTIES ON DIRICHLET BOUNDARIES

      CALL TIMON(18)
      CALL TBDPROP()
      CALL TIMOFF(18)

      IF((NHISUSE == 0).AND.(NSTEP < 1)) GO TO 1

      IF (NEWT.EQ.1.AND.TIM.EQ.0.D0) BALANCE(1,MODACT,4)=CURRENT

C  EXCHANGE PHYSICAL PARAMETERS WITH NEIGHBORING PROCESSORS

      CALL TIMON(22)
      CALL UPDATE(N_PRES,2)
      CALL UPDATE(N_FLDEN,2)
      CALL TIMOFF(22)

! bag8
C      DO_INTF = .FALSE.

C  EVALUATE TRANSPORT IN COEFFICIENTS AND RESIDUALS

      CALL TIMON(21)
      CALL CALLWORK(TTRAN,ITRAN)
C         CALL CALLTSPRB3(TSPRB3,ITRAN)
      CALL TIMOFF(21)

C  EVALUATE TRANSPORT ACROSS DIRICHLET BOUNDARIES

      CALL TIMON(18)
      CALL TBDTRAN()
      CALL TIMOFF(18)

C evaluate well conditions and add well conditions to the
C pressure matrix and residual

    1 CONTINUE
      CALL TIMON(10)
      CALL CLEARWELLS()
      CALL CALLWORK(TWELSUMS,IWELSUM)
      CALL PARALLWELLS()
      CALL CALLWORK(TWELL,IWELL)
      CALL TIMOFF(10)

C bag8 - source function for manufactured solution
      IF (ITEST.GT.0) CALL TSOURCE()

      IF((NHISUSE == 0).AND.(NSTEP < 1)) RETURN

! bag8 - SEND BLOCK INTERFACE DATA
C      CALL TMDUALS()

      END
C ***********************************************************************
        SUBROUTINE TSTEP2 (KONVG,NERR)
C************************************************************************
C  Implicit single-phase model executive routine to continue a time step
C  1.  Complete inter-block calculations
C  2.  Check Newtonian convergence
C
C  NERR = ERROR KEY STEPPED BY ONE FOR EACH ERROR
C         (INPUT AND OUTPUT, INTEGER )

C  KONVG = CONVERGENCE FLAG (OUTPUT, INTEGER)
C        = 1 ==> CONVERGED
C        = 2 ==> CONTINUE ITERATION
C        = 3 ==> FAILED
C        = 4 ==> CUT TIME STEP (SET BY FRAMEWORK ONLY)
C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'
      INCLUDE 'wells.h'
      INCLUDE 'layout.h'
      INCLUDE 'tarydat.h'
      INCLUDE 'tbaldat.h'
C      INCLUDE 'mpfaary.h'

      LOGICAL ONCEONLY
      DATA ONCEONLY /.TRUE./
      INTEGER NUMBUG6,IW,I,KONVG,NERR,KERR
      INTEGER IBUGOUT(6),IMXRESID(2)
      DATA NUMBUG6/0/,IBUGOUT/6*0/,IMXRESID/2*0/
      EXTERNAL TBUGOUT,TMAXRESID

! bag8
C      INTEGER ITRAN(18)
C      DATA ITRAN/18*0/
C      EXTERNAL TTRAN

      IF((NHISUSE == 0).AND.(NSTEP < 1)) RETURN

c  define work routine arguments

      IF(ONCEONLY) THEN
         ONCEONLY = .FALSE.

         IF (LEVELE.AND.BUGKEY(1)) WRITE (NFBUG,100)'PROC',MYPRC,
     &    ' ENTERING SUBROUTINE TSTEP2, OLD TAG =',MSGTAG(13+1)
 100     FORMAT(A,I,A,I)

         IBUGOUT(1) = 5
         IBUGOUT(2) = N_POR
         IBUGOUT(3) = N_PRES
         IBUGOUT(4) = N_FLDEN
         IBUGOUT(5) = N_COF
         IBUGOUT(6) = N_RESID

         IMXRESID(1) = 1
         IMXRESID(2) = N_RESID

! bag8
C         ITRAN(1) = 17
C         ITRAN(2) = N_KCR
C         ITRAN(3) = N_VPROP
C         ITRAN(4) = N_VDIM
C         ITRAN(5) = N_FPROP
C         ITRAN(6) = N_FDIM
C         ITRAN(7) = N_PERMINV
C         ITRAN(8) = N_XC
C         ITRAN(9) = N_YC
C         ITRAN(10) = N_ZC
C         ITRAN(11) = N_FLDEN
C         ITRAN(12) = N_PRES
C         ITRAN(13) = N_AINVF
C         ITRAN(14) = N_TRAN
C         ITRAN(15) = N_AINV
C         ITRAN(16) = N_COF
C         ITRAN(17) = N_RESID
C         ITRAN(18) = N_UPMOBPROD

      ENDIF

C  RECEIVE BLOCK INTERFACE DATA

      FLUXOM=0.D0

! bag8 - multiblock hexahedra
C      IF (EVFEM_HEX.EQ.5) THEN
C        CALL TMSET_PERMINV5()
C        CALL TMFAKEDIAGONALCOMM_4BLOCK()
C        DO_INTF = .TRUE.
C        CALL CALLWORK(TTRAN,ITRAN)
C        CALL TBDTRAN()
C        DO_INTF = .FALSE.
C      ELSEIF (EVFEM_HEX.EQ.6) THEN
!C        CALL TMSET_PERMINV6a()
C        CALL TMSET_PERMINV6b()
C        DO_INTF = .TRUE.
C        CALL CALLWORK(TTRAN,ITRAN)
C        CALL TBDTRAN()
C        DO_INTF = .FALSE.
C      ENDIF

! bag8 - perform Jacobi method on interface (matching case)
C      CALL TMDUALR()

C  DEBUG OUTPUT

      IF (BUGKEY(6).AND.(NEWT.LT.3).AND.(NUMBUG6.LT.7)) THEN
         CALL CALLWORK(TBUGOUT,IBUGOUT)
         NUMBUG6=NUMBUG6+1
         IF (LEVELC) THEN
            DO 6 IW=1,NUMWEL
            IF (MODWEL(IW).EQ.MODACT
C     &     .OR. MODACT.EQ.FLOWMODEL
     &         )
     &         WRITE (NFBUG,7) IW,(WELIPC(I,IW),I=1,3)
    6       CONTINUE
         ENDIF
    7    FORMAT(' WELL',I3,' QWI',G11.4,' QWP',G11.4,' BHP',F8.2)
      ENDIF

C  CHECK NEWTONIAN CONVERGENCE

      RESIDT=0.D0
      RESIDM=0.D0
      KONVG=2
      IF (NEWT.EQ.1) THEN
         CVTOL2=BALANCE(1,MODACT,4)*CVTOL
         CVTOL1=5.D0*CVTOL2/NEMOD(17)
      ELSE
         CALL CALLWORK(TMAXRESID,IMXRESID)

         CALL MAXIT(1,RESIDM)
         CALL SPREAD8(1,RESIDM)
         CALL SUMIT(1,RESIDT)
         CALL SPREAD8(1,RESIDT)

         IF (NEWT.LE.MAXITS) THEN
            IF (RESIDM.LT.CVTOL1.AND.ABS(RESIDT).LT.CVTOL2) KONVG=1
         ELSE
            IF (RESIDM.LT.5.D0*CVTOL1.AND.
     &         ABS(RESIDT).LT.5.D0*CVTOL2) THEN
               KONVG=1
            ELSE
               KONVG=3
            ENDIF
         ENDIF
      ENDIF

      IF (BUGKEY(7)) THEN
         IF (LEVELC) WRITE (NFBUG,14) NEWT-1,RESIDM,RESIDT
         IF (MYPRC.EQ.0) WRITE (*,14) NEWT-1,RESIDM,RESIDT
   14    FORMAT(' NEWT =',I3,' Rmax =',G10.3,' Rtot =',G10.3)
      ENDIF

      END
C ***********************************************************************
        SUBROUTINE TSTEP3 (KONVG,NERR)
C************************************************************************

C  Implicit single-phase model exective routine to complete a time step
C    1.  Update unknowns for next Newtonian iteration (KONVG = 2)
C    2.  Wrap up time step (KONVG = 1)
C    3.  Restart time step (KONVG = 4)
C    4.  Abort time step  (KONVG = 3)
C    5.  Continue reservoir/fracture (KONVG = 5)
C
C  KONVG = CONVERGENCE FLAG (INPUT, INTEGER)
C        = 1 ==> CONVERGED
C        = 2 ==> CONTINUE ITERATION
C        = 3 ==> FAILED
C        = 4 ==> CUT TIME STEP
C        = 5 ==> Continue reservoir/fracture iteration

C  NERR = ERROR KEY STEPPED BY ONE FOR EACH ERROR
C         (INPUT AND OUTPUT, INTEGER )
C*********************************************************************
      IMPLICIT NONE
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'blkary.h'
      INCLUDE 'wells.h'
      INCLUDE 'tarydat.h'
      INCLUDE 'tbaldat.h'
      INCLUDE 'tfluidsc.h'
      INCLUDE 'terrcalc.h'
      INCLUDE 'earydat.h'

      LOGICAL ONCEONLY
      DATA ONCEONLY /.TRUE./
      INTEGER ICOPY(3), IUPPRES(3), ICOPY2(3), IPV(10)

      INTEGER KONVG,NERR,INP
      INTEGER PEKONVG

      DATA ICOPY/ 3*0 /, IUPPRES / 3*0 /, ICOPY2/ 3*0 /,
     &     IPV/ 10*0 /

c list all routines to be called by callwork:

      EXTERNAL TUPPRES, CPYARYR8, TPORE

c  define work routine arguments

      IF(ONCEONLY) THEN
         ONCEONLY = .FALSE.

         IF (LEVELE.AND.BUGKEY(1)) WRITE (NFBUG,100)'PROC',MYPRC,
     &    ' ENTERING SUBROUTINE TSTEP3, OLD TAG =',MSGTAG(13+1)
 100     FORMAT(A,I,A,I)

         ICOPY(1)=2
         ICOPY(2)=N_PRESN
         ICOPY(3)=N_PRES

         ICOPY2(1)=2
         ICOPY2(2)=N_PVN
         ICOPY2(3)=N_PV

         IUPPRES(1) = 2
         IUPPRES(2) = N_DUNK
         IUPPRES(3) = N_PRES

         IPV(1) = 9
         IPV(2) = N_CR
         IPV(3) = N_PV
         IPV(4) = N_PRES
         IPV(5) = N_DELTAP
         IPV(6) = N_EVOL
         IF(MBPOROE) THEN
         IPV(7) = N_MODULFLOW
         IPV(8) = N_POISSFLOW
         IPV(9) = N_BIOTAFLOW
         ELSE
         IPV(7) = N_MODUL
         IPV(8) = N_POISS
         IPV(9) = N_BIOTA
         ENDIF
         IPV(10) = N_VSTRAIN

      ENDIF

      IF((NHISUSE == 0).AND.(NSTEP < 1)) GO TO 1

C  BRANCH ON CONVERGENCE STATE

      GO TO (1,2,1,4),KONVG

C  CONTINUE NEWTONIAN ITERATION

    2 IF (BUGKEY(6).AND.NEWT.LT.3) THEN
         TITU='CHANGE IN PRESSURE FOR FAULT BLOCK'
         CALL GEAOUT(N_DUNK,1,1)
      ENDIF

      CALL TIMON(20)
      CALL CALLWORK(TUPPRES, IUPPRES)
      CALL UPDATE(N_PRES,2)
      CALL TIMOFF(20)

      CALL CALLWORK(TPORE,IPV)

      RETURN

C  NEWTONIAN ITERATION CONVERGED OR FAILED
C  UPDATE WELL DATA AND BALANCES AT END OF TIME STEP

    1 CALL TWELLOUTPUT()
      IF((NHISUSE == 0).AND.(NSTEP < 1)) RETURN

      CALL TIMON(18)
      CALL TBDBAL ()
      CALL TIMOFF(18)

      BALANCE(1,MODACT,1)=CURRENT
      BALANCE(1,MODACT,2)=WELIS
      BALANCE(1,MODACT,3)=FLUXOM
      BALANCE(1,MODACT,7)=FLITNP

CGUS
C COMPUTE (POST-PROCESS) FACE-CENTERED VELOCITIES
C INP = 0 (RT0 VELOCITY: 1 FLUX PER FACE)
C     = 1 (EBDDF1 VELOCITY: 4 FLUXES PER FACE)
C INP = 1 NOT OPERATIONAL YET
C
      INP = 0
      CALL TVEL_EX(INP)

C  OUTPUT DIRICHLET BOUNDARY DATA

      CALL BND_OUT()

ctm   TAMEEM
      IF(Q_MULTIRATE.GT.1) THEN
              MULTIRATE_FLAG = MULTIRATE_FLAG + 1
      ENDIF
c      WRITE(*,*) 'MULTIRATE_FLAG: ', MULTIRATE_FLAG
ctm   TAMEEM


c original code
cbag8
      PEKONVG = 0
      IF (KONVG.EQ.1) THEN
         IF(MODELON(15)) THEN
           IF(MBPOROE) MODACT=15 ! SAUMIK,BGANIS
           CALL EITER(PEKONVG,NERR)
           IF(MBPOROE) MODACT=17 ! SAUMIK,BGANIS
           IF (NERR.EQ.100) THEN
               IF(NCUT.LE.10) THEN
                  IF (MYPRC.EQ.0) THEN
           WRITE(0,*)'CUTTING TIME STEP: NCUT =',NCUT
                  ENDIF
                  KONVG=4
                  GOTO 4
                  ELSE
                    IF (MYPRC.EQ.0) THEN
           WRITE(0,*)'TERMINATION DUE TO TOO MANY CUTBACKS'
                    ENDIF
                ENDIF
           ENDIF
           IF (PEKONVG.NE.1) THEN
              KONVG = 2
! SAUMIK - NEWT RESET TO ZERO AFTER EVERY FIXED-STRESS ITERATION
              NEWT = 0
           ENDIF
         ENDIF
      ENDIF

      IF(MODELON(15).AND.(Q_MULTIRATE.NE.1)) THEN
! SAUMIK - ONLY IF MULTIRATE IS ON
      COUP_ITRN = 1
ctm   TAMEEM
      IF(MC_SKIPPED.EQ.0) THEN
            GCITER_C = GCITER_C + 1
      ENDIF
      IF(MC_SKIPPED.EQ.0) THEN
        IF (Q_MULTIRATE.NE.1) THEN
         WRITE(*,*), 'TSTEP: ************************'
         WRITE(*,*) 'FIRST BRANCH: Mech. not skipped!'
         WRITE(*,*) 'AFTER MECHANICS CALL1, PEKONVG =', PEKONVG
         WRITE(*,*) 'AFTER MECHANICS CALL2, KONVG =', KONVG
         WRITE(*,*) 'AFTER MECHANICS CALL3, GCITER_C =', GCITER_C
         WRITE(*,*), 'TSTEP: ************************'
        ENDIF
        IF (KONVG.EQ.1) THEN
c            IF (MYPRC.EQ.0) THEN
c               WRITE(*,*) 'MC_SKIPPED:', MC_SKIPPED
c            ENDIF
            IF(COMPUTE_NORM.EQ.1) THEN
c computing the norm for the last iterative coupling iteration
c (after convergence)
               CALL COMPUTE_KONVG_NORM(NERR)
               WRITE(*,*) 'COMP_NORM  ', 0.0
            ENDIF
            RETURN
        ELSE
c             This is the place where you will put the convg. calc.
            IF(COMPUTE_NORM.EQ.1) THEN
               IF(GCITER_C.GT.1) THEN
c computing the norm for one iterative coupling iteration
c (before convergence)
                  CALL COMPUTE_KONVG_NORM(NERR)
               ELSE
                  M_ITER = M_ITER + 1
                  GCITER_C = 0
                  CALL GSAVE_FOR_MULTIRATE(NERR)
                  GCITER_C = 1
               ENDIF
c checking the special case of single rate
               IF(Q_MULTIRATE.EQ.1) THEN
c           CALL GSAVE_FOR_MULTIRATE(GCITER_C,1,NERR)
           M_ITER = 1
           CALL GSAVE_FOR_MULTIRATE(NERR)
               ENDIF
            ENDIF
            M_ITER = 0
            TIM = TIM - (Q_MULTIRATE-1.0)*DELTIM
            CALL RET_OLDTIMP_MR(NERR)
        ENDIF
      ELSE
        WRITE(*,*) 'TSTEP: SECOND BRANCH: Mechanics skipped'
        M_ITER = M_ITER + 1
        IF(COMPUTE_NORM.EQ.1) THEN
c       CALL GSAVE_FOR_MULTIRATE(GCITER_C, M_ITER, NERR)
       CALL GSAVE_FOR_MULTIRATE(NERR)
        ENDIF
        TIM=TIM+DELTIM
c re-check this again ..
        CALL GSAVE_OLDTIMP(NERR)
      ENDIF
      ENDIF
ctm   TAMEEM

C  bag8 - ERROR CALCULATIONS

      IF (ITEST.GT.0) CALL TERRCALC()

      RETURN

C  TIME STEP CUT

    4 CALL CALLWORK(CPYARYR8,ICOPY)

cbag8
      CALL CALLWORK(CPYARYR8,ICOPY2)

      CALL ECUTTIME(NERR)

      RETURN
      END


C*********************************************************************
      SUBROUTINE TSETARYR8(N_ARY,N4,VAL)
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INTEGER  N_ARY,N4
      REAL*8   VAL

      INTEGER JWORK(4)
      EXTERNAL SETARYR8N

      JWORK(1) = 3
      JWORK(2) = N_ARY
      JWORK(3) = N_R8U
      JWORK(4) = N_I4U
      R8UTIL = VAL
      I4UTIL = N4
      CALL CALLWORK(SETARYR8N,JWORK)
      END

C*********************************************************************
      SUBROUTINE TSETARYR4(N_ARY,N4,VAL)
C*********************************************************************
      IMPLICIT NONE
      INCLUDE 'blkary.h'
      INTEGER  N_ARY,N4
      REAL*8   VAL

      INTEGER JWORK(4)
      EXTERNAL SETARYR4N

      JWORK(1) = 3
      JWORK(2) = N_ARY
      JWORK(3) = N_R4U
      JWORK(4) = N_I4U
      R4UTIL = VAL
      I4UTIL = N4
      CALL CALLWORK(SETARYR4N,JWORK)
      END

C*********************************************************************
      SUBROUTINE TPORE(IDIM,JDIM,KDIM,LDIM,IL1,IL2,JL1V,JL2V,
     &                 KL1,KL2,KEYOUT,NBLK,CR,PV,P,DP,EVOL,
     &                 MODUL,POISS,BIOTA,VSTRAIN)
C*********************************************************************
C Calculate new pore volume using rock compressibility term
C
C INPUT:
C   CR(I,J,K) = ROCK COMPRESSIBILITY (1/(PSI*BBL))
C   PV(I,J,K) = GRID ELEMENT PORE VOLUME AT NONLINEAR ITERATION K (CU-FT)
C
C OUTPUT:
C   PV(I,J,K) = GRID ELEMENT PORE VOLUME AT NONLINEAR ITERATION
C               K+1 (CU-FT)
C*********************************************************************
      IMPLICIT NONE
      INTEGER IDIM,JDIM,KDIM,LDIM,IL1,IL2,KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM),JL1V(KDIM),JL2V(KDIM)
      REAL*8  PV(IDIM,JDIM,KDIM), DP(IDIM,JDIM,KDIM),
     &        CR(IDIM,JDIM,KDIM), EVOL(IDIM,JDIM,KDIM),
     &        P(IDIM,JDIM,KDIM), MODUL(IDIM,JDIM,KDIM),
     &        POISS(IDIM,JDIM,KDIM), BIOTA(IDIM,JDIM,KDIM),
     &        VSTRAIN(IDIM,JDIM,KDIM)
      INTEGER I,J,K

      DO K=KL1,KL2
         DO J=JL1V(K),JL2V(K)
            DO I=IL1,IL2
               IF (KEYOUT(I,J,K).GT.0) THEN

! BAG8 - COMPUTE CHANGE IN PRESSURE SINCE LAST CALL TO TPORE
                  DP(I,J,K) = P(I,J,K) - DP(I,J,K)

! BAG8 - NOTE CR IS MEANT TO BE ROCK COMPRESSIBILITY TIMES VOLUME HERE
                 PV(I,J,K)=PV(I,J,K)+CR(I,J,K)*DP(I,J,K)

! BAG8 - STORE CURRENT PRESSURE FOR NEXT CALL TO TPORE
                 DP(I,J,K)=P(I,J,K)

               ENDIF
            ENDDO
         ENDDO
      ENDDO

      END
