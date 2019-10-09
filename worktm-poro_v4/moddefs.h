! ----------------------------------------------------------------
! file: moddefs.h : include file cotaining model numbers and definitions
! 8/99 M. Peszynska
! 1/06 S. G. Thomas : added Matt Balhoff's porescale model
! ----------------------------------------------------------------

! descriptive numbers for models

      integer ALL

      integer BLACKP, BLACKI, COMP, CHEM, HYDROI, HYDROE, MMODEL,              &
     &	unknown8, SINGLEE, unknown10, AIR, unknown12, SINGLEI,                 &
     &	TRCHEM,POROHEX,CMFMFE,SIMFMFE,HIMFMFE,AHYDROI

      parameter (ALL     = 0)

      parameter (BLACKP  = 1)
      parameter (BLACKI  = 2)
      parameter (COMP    = 3)
      parameter (CHEM    = 4)
      parameter (HYDROI  = 5)
      parameter (AHYDROI  = 19)
      parameter (HYDROE  = 6)
      parameter (MMODEL  = 7)
      parameter (unknown8 = 8)
      parameter (SINGLEE  = 9)
      parameter (unknown10 = 10)
      parameter (AIR     = 11)
      parameter (unknown12 = 12)
      parameter (SINGLEI = 13)
      parameter (TRCHEM  = 14)
      parameter (POROHEX = 15)
      parameter (CMFMFE = 16)
      parameter (SIMFMFE = 17)
      parameter (HIMFMFE = 18)

! Model_names are the names associated with the models
! that are used in the input file as BLOCKMODEL in multimodel
! or flow model definition for transport-chemistry model

      character*50      Model_Names(19)

      data Model_Names /                                                       &
     &     'BLACK_OIL_IMPES',                                                  &
     &     'BLACK_OIL_IMPLICIT',                                               &
     &     'COMPOSITIONAL_MODEL',                                              &
     &     'CHEMICAL_MODEL',                                                   &
     &     'HYDROLOGY_IMPLICIT',                                               &
     &     'HYDROLOGY_IMPES',                                                  &
     &     'MULTI_MODEL',                                                      &
     &     'unknown8',                                                         &
     &     'SINGLE_PHASE_EXPLICIT',                                            &
     &     'unknown10',                                                        &
     &     'AIR_WATER',                                                        &
     &     'unknown12',                                                        &
     &     'SINGLE_PHASE_IMPLICIT',                                            &
     &     'TRANSPORT_CHEMISTRY',                                              &
     &     'POROELASTIC',                                                      &
     &     'COMPOSITIONAL_MFMFE',                                              &
     &     'SINGLE_PHASE_MFMFE',                                               &
     &     'HYDROLOGY_IMPLICIT_MFMFE',                                         &
     &     'HYDROLOGY_IMPLICIT_APPROX'                                         &
     &     /

! Phase_Names are the (default) names associated with phases 1..MXPHAS
! their order depends on the inner ordering for a model. This inner
! ordering can be seen in the ordering of RELPRMs etc, or default
! bc_prim assignments in the mortar code. In transport-chemistry
! they are used for descriptive purposes

      character*10      Phase_Names(19,3)

      DATA PHASE_NAMES /                                                       &
     &     '','','',                                                           &
     &     'WATER','OIL','GAS',                                                &
     &     '','','',                                                           &
     &     '','','',                                                           &
     &     'OIL','WATER','',                                                   &
     &     'OIL','WATER','',                                                   &
     &     '','','',                                                           &
     &     '','','',                                                           &
     &     'WATER','','',                                                      &
     &     '','','',                                                           &
     &     'WATER','AIR','',                                                   &
     &     '','','',                                                           &
     &     'WATER','','',                                                      &
     &     '','','',                                                           &
     &     '','','',                                                           &
     &     '','','',                                                           &
     &     '','','',                                                           &
     &     '','','',                                                           &
     &     '','',''                                                            &
     &     /





