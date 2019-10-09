!  EARYDAT.H - POROELASTIC MODEL GRID_ELEMENT ARRAY NUMBERS

!**********************************************************************

      INTEGER  N_MODUL,             N_POISS,          N_BIOTA,                 &
     &         N_BIOTM,             N_ROCKD,          N_EDISP,                 &
     &         N_EDISPN,            N_VSTRAIN_INIT,   N_STRAIN,                &
     &         N_DISP_COMP,         N_BULK_DEN,       N_STRAINN,               &
     &         N_STRESS_INIT(6),                                               &
     &         N_PRESS,             N_PRESSN,         N_ECON(3),               &
     &         N_EPMD,                                                         &
     &         N_ESAT(3),           N_EFLDEN,          N_EPV,                  &
     &         N_EPVN,              N_STRESS,         N_STRESSN,               &
     &         N_EMASS(3),                                                     &
     &         N_KEYOUT_CR,         N_PREF,           N_VSTRAIN,               &
     &         N_EPV_FLOW,                                                     &
     &         N_ECR,                                                          &
     &         N_UPDATE_R8,      N_UPDATE_FG,                                  &
     &         N_UPDATE_I4,         N_KEYCR_ELE,                               &
     &         N_STR_RESID,          N_CRAC_IBC,                               &
     &         N_PORO_NEIGHBOR,                                                &
     &         N_ELEM_LID,          N_NODE_LID,       N_FNODE_TYPE,            &
     &         N_OFNODE_AFFINE,     N_OFNODE_KEYOUT,  N_OFNODE_LID,            &
     &         N_OFNODE_DISP,       N_OFNODE_L2GID,   N_OFNODE_GID,            &
     &         N_OFNODE_GNUM,       N_NODE_WIDTH,     N_PROCN,                 &
     &         N_POROHEX_GELEI,                                                &
     &         N_PSTRAIN,	    N_PSTRAINN,                                &
     &         N_PSTATE,            N_PSTATEN,                                 &
     &         N_PASSO,             N_YIELD_SIG0,                              &
     &         N_YIELD_ALPHA,       N_FLOW_ALPHA,                              &
     &         N_HARDEN_MODEL,      N_HARDEN_C1,                               &
     &         N_HARDEN_C2,                                                    &
     &         N_XPERM_REF,         N_YPERM_REF,      N_ZPERM_REF,             &
     &         N_EMSTRESS,          N_EMSTRESS_REF,                            &
     &         N_XPERM_R8,          N_YPERM_R8,       N_ZPERM_R8,              &
     &         N_VSTRAIN_NM1,       N_TDISP,          N_PRESSVAL

      COMMON /EGEA/                                                            &
     &         N_MODUL,             N_POISS,          N_BIOTA,                 &
     &         N_BIOTM,             N_ROCKD,          N_EDISP,                 &
     &         N_EDISPN,            N_VSTRAIN_INIT,   N_STRAIN,                &
     &         N_DISP_COMP,         N_BULK_DEN,       N_STRAINN,               &
     &         N_STRESS_INIT,                                                  &
     &         N_PRESS,             N_PRESSN,         N_ECON,                  &
     &         N_EPMD,                                                         &
     &         N_ESAT,              N_EFLDEN,          N_EPV,                  &
     &         N_EPVN,              N_STRESS,         N_STRESSN,               &
     &         N_EMASS,                                                        &
     &         N_KEYOUT_CR,         N_PREF,           N_VSTRAIN,               &
     &         N_EPV_FLOW,                                                     &
     &         N_ECR,                                                          &
     &         N_UPDATE_R8,         N_UPDATE_FG,                               &
     &         N_UPDATE_I4,         N_KEYCR_ELE,                               &
     &         N_STR_RESID,         N_CRAC_IBC,                                &
     &         N_PORO_NEIGHBOR,                                                &
     &         N_ELEM_LID,          N_NODE_LID,       N_FNODE_TYPE,            &
     &         N_OFNODE_AFFINE,     N_OFNODE_KEYOUT,  N_OFNODE_LID,            &
     &         N_OFNODE_DISP,       N_OFNODE_L2GID,   N_OFNODE_GID,            &
     &         N_OFNODE_GNUM,       N_NODE_WIDTH,     N_PROCN,                 &
     &         N_POROHEX_GELEI,                                                &
     &         N_PSTRAIN,           N_PSTRAINN,                                &
     &         N_PSTATE,            N_PSTATEN,                                 &
     &         N_PASSO,             N_YIELD_SIG0,                              &
     &         N_YIELD_ALPHA,       N_FLOW_ALPHA,                              &
     &         N_HARDEN_MODEL,      N_HARDEN_C1,                               &
     &         N_HARDEN_C2,                                                    &
     &         N_XPERM_REF,         N_YPERM_REF,      N_ZPERM_REF,             &
     &         N_EMSTRESS,          N_EMSTRESS_REF,                            &
     &         N_XPERM_R8,          N_YPERM_R8,       N_ZPERM_R8,              &
     &         N_VSTRAIN_NM1,       N_TDISP,          N_PRESSVAL

!**********************************************************************
! N_ = GRID ELEMENT ARRAY NUMBER
!
! N_MODUL       = ELASTIC MODULUS (PSI)
!               = LAME'S CONSTANT LAMBDA (PSI)
! N_POISS       = POISSON'S RATIO
!               = LAME'S CONSTANT MU (PSI)
! N_BIOTA       = BIOT'S CONSTANT ALPHA (DIMENSIONLESS)
! N_BIOTM       = BIOT'S CONSTANT 1/M   (DIMENSIONLESS)
! N_ROCKD       = ROCK DENSITY (GM/CC)
! N_EDIAG       = DIAGONAL COEF. COEFFICIENTS OF STIFFNESS MATRIX
! N_EBEFORE     = OFF DIAGONAL COEF. COEFFICIENTS OF STIFFNESS MATRIX
! N_ERESID      = RESIDUALS OF FORCE BALANCE EQUATIONS
! N_EDISP       = DISPLACEMENTS (IN)
! N_EPCONN      = INTEGRATION COEFFICIENTS FOR COMPUTING VOLUMETRIC STRAINS
! N_DIR_COS     = DIRECTION COSINE
! N_VARD        = DISPLACEMENT INCREMENTS (IN)
! N_EDISPN    = DISPLACEMENTS AT TIME LEVEL N (IN)
! N_STRAIN_INIT = INITIAL VOLUMETRIC STRAIN (DIMENSIONLESS)
! N_ZERO_NODE   = INDICATOR OF NO POROELASTIC CALCULATION FOR A GRID ELEMENT.
! N_DISP_COMP   = ROCK COMPRESSIBILITY (1/(BBL*PSI)
! N_FORCE_INIT  = INITIAL FORCES DUE TO INITIAL STRESS (LB)
! N_BODY_FORCE  = BODY FORCE (FLUID + ROCK) (LB)
! N_ESTRAIN     = ELASTIC STRAIN
! N_STRXX_INIT  = INITIAL PRINCIPLE STRESS IN X DIRECTION  (PSI)
! N_STRYY_INIT  = INITIAL PRINCIPLE STRESS IN Y DIRECTION  (PSI)
! N_STRZZ_INIT  = INITIAL PRINCIPLE STRESS IN Z DIRECTION  (PSI)
! N_PRESS       = PORE PRESSURE (PSI)
! N_PRESSN      = PORE PRESSURE AT TIME LEVEL N (PSI)
! N_ECON        = OIL, WATER AND GAS COMPONENT CONCENTRATIONS
!                 (STOCK TANK VOLUME PER UNIT PORE VOLUME)
! N_ESAT        = OIL, WATER AND GAS PHASE SATURATIONS
! N_EDUNK       = OIL, WATER AND GAS PHASE DENSITIES (LB/BBL)
! N_EPV         = PORE VOLUME (BBL)
! N_EPVN        = PORE VOLUME AT TIME LEVEL N (BBL)
! N_STRESS      = TEMPORARY WORK SPACE FOR COMPUTING AVERAGE STRAIN AND STRESS.
! N_EMASS       = TOTAL MASS PER GRID ELEMENT FOR OIL, WATER AND GAS COMPONENTS
!                 (STOCK TANK VOLUME)
! N_KEYOUT_CR   = KEYOUT VALUES FOR CORNER POINTS.
! N_EPV_RATE    = PORE VOLUME CHANGE RATE FOR TIME LEVEL N (BBL/DAY)
! N_DISP_INIT   = INITIAL DISPLACEMENTS (IN)
! N_PREF       = REFERENCE PORE PRESSURE (PSI)
! N_EPV_FLOW     = PORE VOLUME CALCULATED IN THE FLOW MODEL (BBL0
! N_ECR         = ROCK COMPRESSIBILITY

! Bin Wang: COUPLING WITH POROHEX
! N_ELEM_LID     = LOCAL ELEMENT ID TO BE SENT TO POROHEX MODEL (ACTIVE+GHOST)
! N_NODE_LID     = LOCAL NODE ID TO BE SENT TO POROHEX MODEL (ACTIVE+GHOST)
! N_FNODE_TYPE(IDIM,JDIM,KDIM) =  FRACTURE NODE TYPE(OPEN, NONOPEN)
! N_OFNODE_LID(IDIM,JDIM,KDIM) = LOCAL OPEN FRACTURE ID (ACTIVE+GHOST)
! N_OFNODE_GID(IDIM,JDIM,KDIM) =  OPEN FRACTURE NODE GLOBAL ID
! N_OFNODE_L2GID = LOCAL OPEN FRACTURE ID TO GLOBAL NODE ID
! N_OFNODE_AFFINE(1:3,LFALLSIZE) = RESERVOIR NODE (I,J,K) ASSOCIATED WITH
!                                  LOCAL OPEN FRACTURE NODE
! N_OFNODE_AFFINE(4,LFALLSIZE) = LOCAL RESERVOIR NODE ID ASSOCIATED WITH
!                                  LOCAL OPEN FRACTURE NODE
! N_OFNODE_AFFINE(5,LFALLSIZE) = NUMBER OF LOCAL ELEMENTS ASSOCIATED WITH
!                                  LOCAL OPEN FRACTURE NODE
! N_OFNODE_AFFINE(6:9,LFALLSIZE) = LOCAL ELEMENT ID ASSOCIATED WITH
!                                  LOCAL OPEN FRACTURE NODE WHOSE CONNECTIVITY
!                                  LIST NEEDS TO BE CHANGED
! N_OFNODE_KEYOUT(1:LFALLSIZE) = KEYOUT ARRAY FOR LOCAL OPEN FRACTURE NODE
! N_OFNODE_DISP(3,1:GFSIZE) = GLOBAL ARRAY FOR STORING OPEN FRAC NODE
!                             DISPLACEMENT
! N_OFNODE_GNUM(1,GFSIZE) = GLOBAL ID ARRAY FOR N_OFNODE_DISP

! Ruijie add for plastic models
! N_PSTRAIN     = plastic strain (each element has eight gauss points and
!                 gaussian point has six stress components to store)
! N_PSTRAINN    = plastic strain at previous time step
! N_STATEV     = store state variables such euqivalent plastic strain,
!                  volume plastic strain, current yield strength...
! N_STATEVN    = state variables at previous time step
! N_PASSO       = associated model (0) nonassociated model (1)
! N_YIELD_SIG0  = Drucker-Prager initial shear strength (sigma0)
! N_YIELD_ALPHA = Slope for Drucker-Prager Shear Yield line
! N_FLOW_ALPHA  = Slope for Druck-Prager flow function
! N_HARDEN_MODEL= 0: linear 1: power law (hardening model)
! N_HARDEN_C1   = slope for linear hardenning model or first
!                 coefficient for power law
! N_HARDEN_C2   = second coefficient for power law hadrening function


