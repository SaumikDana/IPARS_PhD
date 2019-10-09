c file: TVISUAL.H
c contains glossary for visualization variables
c for SINGLE PHASE IMPLICIT MODEL
c
c
c CODE HISTORY
c MPeszynska, 5/26/98   SKELETON
c B. MOMKEN   3/26/99 modified for single phase flow
c MPeszynska  11/15/99 copied for single phase implicit flow
c MPeszynska  5/01 added definition of head or of potentail
c=============================================================

      INTEGER IPARS_NSCL, IPARS_NVEC
      PARAMETER (IPARS_NSCL = 8)
      PARAMETER (IPARS_NVEC = 1)

      CHARACTER*20 IPARS_SCL_NAMES(3,IPARS_NSCL)

c auxiliary variables to keep info on requested output of head or pot.
      INTEGER HEADPOS, POTPOS
      PARAMETER (HEADPOS = 5, POTPOS =4)
      LOGICAL HEADOUT, POTOUT
      COMMON /TVIS/ HEADOUT, POTOUT
c ------------------------------------------------------------
c name in input file,  in TEC file, IPARS name
      DATA IPARS_SCL_NAMES /
     &     'PRES', 'PRES', 'PRES',
     &     'PWAT', 'PWAT', 'PRES',
     &     'FLDEN', 'FLDEN', 'FLDEN',
     &     'POTENTIAL', 'POTENTIAL', 'FLDENN',
     &     'PRESHEAD', 'PRESHEAD', 'FLDENN',
     &     'XPERM8', 'XPERM8', 'XPERM8',
     &     'YPERM8', 'YPERM8', 'YPERM8',
     &     'ZPERM8', 'ZPERM8', 'ZPERM8'
     &     /
      INTEGER IPARS_SCL_OFFSETS (IPARS_NSCL)
      DATA IPARS_SCL_OFFSETS /
     &     1,
     &     1,
     &     1,
     &     1,
     &     1,
     &     1,
     &     1,
     &     1
     &     /

      CHARACTER*20 IPARS_VEC_NAMES(7,IPARS_NVEC)

c ---------------------------------------------------------------
c name in input file,  in TEC file (XYZ) , IPARS name of (XYZ)

      DATA IPARS_VEC_NAMES /
     &     'VEL','VX', 'VY', 'VZ', 'VEL', 'VEL', 'VEL'
     &     /

      INTEGER IPARS_VEC_OFFSETS (3,IPARS_NVEC)
      DATA IPARS_VEC_OFFSETS /
     &     1,2,3
     &     /

c ----------------------------------------------------------------


