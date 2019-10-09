C  CODE HISTORY:        
C      THE IPARS TEAM          04/02/1997   FRAMEWORK
C      RICK DEAN               03/02/2001   INITIAL VERSION
C      SUNIL G. THOMAS         09/01/2007   MODIFIED FOR THERMAL
C*********************************************************************

      INTEGER NWRK, IWRK, NWRKSZE
      PARAMETER (NWRK = 12)
      COMMON/WRKSPACE/IWRK(NWRK),NWRKSZE

      REAL*8 TOL_FLASH,TOL_RR,TOL_TRIV,MDENH,TOL_ZFAC,TOL_SAT,TOL_PW,
     &       TOL_RATE,STOPTIME,DUMPTIME1,DUMPTIME2
      INTEGER MAXFLITS,LTCOMP,IFLPV0,IDUMP
      LOGICAL TESTALL,BORROW_K,NEG_FLASH,PARTIALJAC
      COMMON /MODELS/TOL_FLASH,TOL_RR,TOL_TRIV,TOL_ZFAC,STOPTIME,
     &        TOL_SAT,MDENH,TOL_PW,TOL_RATE,DUMPTIME1,DUMPTIME2,
     &        MAXFLITS,LTCOMP,IFLPV0,IDUMP,TESTALL,BORROW_K,
     &        NEG_FLASH,PARTIALJAC
     
C IWRK - Pointer for storage in work vector for flash routines
C NWRKSZE - Size of work vector

C IVEC(1) - NORM  
C     (2) - COEFW
C     (3) - ADOIL
C     (4) - APOIL
C     (5) - BPOIL
C     (6) - CPOIL
C     (7) - PRLOGO
C     (8) - ADGAS
C     (9) - APGAS
C    (10) - BPGAS
C    (11) - CPGAS
C    (12) - PRLOGG

C TOL_FLASH   - Tolerance for fugacity equations
C TOL_RR      - Tolerance for the Rachford-Rice equation
C TOL_TRIV    - Tolerance for trivial soln
C TOL_ZFAC    - Tolerance for cubic equation 
C TESTALL     - True, Test all single phase cells for stability
C               False, Partial test of cells for stability
C PARTIALJAC  - True, Use partial jacobian update for flash
C               False, Use full jacobian update for flash
C BORROW_K    - True, Borrow K-values from neighbors
C               False, Do not borrow K-values from neighbors
C NEGFLSH     - True, Negative flash used in place of stabilty test
C               False, Perform stability test
C LTCOMP      - Number of a light hydrocarbon component 
C MAXFLITS    - Maximum number of flash iterations
C TOL_SAT     - Tolerance for saturation equations
C NFTYPES     - Number of fluid types for phase pacakage
C MDENH       - Mass density for identification of type of single phase
C IFLPV0      - Location of zero pv cells in FLTYPE vector
C STOPTIME    - Time to stop in XSTEP1 for interactive debugging
C DUMPTIME1   - Time to start dumping single cell printout 
C               at end of flash routines
C DUMPTIME2   - Time to stop dumping single cell printout 
C IDUMP       - Cell to dump at end of flash routines
