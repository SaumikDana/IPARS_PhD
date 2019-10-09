c **************************************************
C  boundary.h - include file for boundary conditions
c **************************************************
c M. Peszynska, 1/01 initial version
c ----------------
c input parameters
      INTEGER
     &     NBND_REG,    ! Number of boundary regions.
     &     NBND_SUBREG, ! Number of boundary subregions (input).
     &     KBND_BLK(60) ! faultblock associated with this subregion

      REAL*4
     &     BND_VOL(8,60), ! Boundary volume geometry data
     &     BND_DEP(60)      ! Reference depth for bdary region

      INTEGER
     &     NBND_type(60), ! Type of boundary condition (defined by model)
c               a.  0  ==> No Flow
c               b.  >0 ==> Time dependent Dirichlet (river or lake)
c               c.  <0 ==> Time dependent Flux (rainfall)
c
     &     NTABBND(60,13) ! Table number for time dependent
c               boundary conditions.

c------------------------------------------------
c the values below are computed by the framework

      INTEGER
     &     LOCBND(4,2464320), ! Boundary element location and face number
c               a. i = 1 to 3 ==> i,j,k of the 3D element
c               b. j = 4 ==> Face number (1 to 6)
     &     LOFFBND(60,10) ! Pointers into LOCBND() for the
c               last value of each boundary condition and each fault
c               block. (Condition 1 is assumed to start with location 1)

      REAL*4
     &     BAREA(2464320)      ! Boundary element area in the boundary
c     volume.  The same pointer, LOFFBND(), used for LOCBND() can
c     be used for this array. What really is set in 8/9 depends
c      on the needs of physical models.

C      REAL*8
C     &     BFLUX(13,2464320)  ! fluxes

      COMMON /BOUNDARY/
     &     NBND_REG,
     &     NBND_SUBREG,
     &     KBND_BLK,
     &     BND_VOL,
     &     BND_DEP,
     &     NBND_TYPE,
     &     NTABBND,
     &     LOCBND,
     &     LOFFBND,
     &     BAREA
C     &     ,BFLUX
c----------------------
c utility
      integer nbel              ! current number of boundary elements
      integer myreg             ! current bdary region number
      integer mynblk             ! current faultblock for this region
      real*4 vvol(6)

      common /bdutil/ nbel,mynblk,myreg,vvol

      real*4 BDAREA_EPS
      common /bdeps/ bdarea_eps
c-----------------------
c balances and their output

      LOGICAL BNDOUT                   ! flag with request of bndoutput
      REAL*8
     &     BND_FLUX(60,13)  ! flux across bdary reg/component
c                                      [lb/day]
      COMMON /BDBAL/ BND_FLUX,BNDOUT

      INTEGER BNDRMOD(60)
      COMMON /BNDMMOD/ BNDRMOD

c-----------------------
c user programs: program numbers for BDMOD

      INTEGER NBDPROG(60)
      COMMON /BD1/ NBDPROG





