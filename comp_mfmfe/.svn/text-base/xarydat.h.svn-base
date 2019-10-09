
C  XARYDAT.H - COMPOSITIONAL DATA - GRID ELEMENT ARRAY NUMBERS

C  CODE HISTORY:        THE IPARS TEAM          04/02/1997
C                       RICK DEAN               03/15/2001
C                       SUNIL G. THOMAS         09/01/2007
C                       GURPREET SINGH          2011-2015
C*********************************************************************
      INTEGER N_PRES,N_CONC,N_CONCARR($MXCOMP),N_PRESN,N_CONCN,N_SAT,
     &       N_CONCAQARR($MXCOMP),
     &       N_SATARR($MXPHASE),N_SWMIN,N_SGT,N_PMDEN,N_PMDENN,N_PMD,
     &       N_PMDARR(2*$MXCOMP+1),N_PV,N_PVN,N_DSTDP,N_DSTDN,N_TEMPR,
     &       N_FLDPNT,N_MOBPROD,N_FLZ,N_FLV,N_FLOLDZP,N_FLDERIV,
     &       N_UPMOBPROD,N_AINVF,N_DELTAP,
     &       N_FLRR,N_FLK,N_FLFUG,N_XDUNK,N_XCOF,N_TMPCOF,N_XRESID,
     &       N_CR,N_MDENN,
     &       N_REFPRES,N_CTAB,N_ERRSAT,N_MOB,N_XVISC,N_RESID,
     &       N_UPMOB,
     &       N_DELC,N_PC,N_PCARR($MXPHASE),N_FLKN,N_FLDPNTN,N_SATN,
     &       N_BUFCOMP,N_TENS,N_DMOB,N_DPC,N_DCFL,N_CFL,N_XVEL,N_XVELN,
     &       N_KSI,N_KSIN,N_KSIARR(2*$MXCOMP+1),N_XDMAT,N_XHEAT,N_XTCPN,
     &       N_XTCP,N_XCVS,N_XRHOS,N_XCVL,N_XCPL,N_XSLP,N_XTCOND,
     &       N_XTCOF,N_XTRESID,N_XTDUNK,N_DDINV,N_DDCOF,
     &       N_CRPHI0,N_PHI0,N_PHI,N_CONCAQ,N_CONCAQN,
C --- SAUMIK,BGANIS
     &       N_COMP_DEN,N_MODULFLOW,N_POISSFLOW,N_BIOTAFLOW
C --- SAUMIK,BGANIS

      COMMON /COMP_ARRAY/ N_PRES,N_CONC,N_CONCARR,N_PRESN,N_CONCN,N_SAT,
     &       N_CONCAQARR,
     &       N_SATARR,N_SWMIN,N_SGT,N_PMDEN,N_PMDENN,N_PMD,N_PMDARR,
     &       N_PV,N_PVN,N_DSTDP,N_DSTDN,N_TEMPR,N_FLDPNT,
     &       N_FLZ,N_FLV,N_FLOLDZP,N_MDENN,N_FLRR,N_FLFUG,
     &       N_XDUNK,N_XCOF,N_TMPCOF,N_XRESID,N_CR,N_REFPRES,
     &       N_CTAB,N_ERRSAT,
     &       N_FLK,N_FLDERIV,N_SATN,N_MOB,N_XVISC,N_RESID,N_MOBPROD,
     &       N_UPMOBPROD,N_UPMOB,N_AINVF,
     &       N_DELC,N_PC,N_PCARR,N_FLKN,N_FLDPNTN,N_BUFCOMP,N_TENS,
     &       N_DMOB,N_DPC,N_DCFL,N_CFL,N_XVEL,N_XVELN,N_KSI,N_KSIN,
     &       N_KSIARR,N_XDMAT,N_XHEAT,N_XTCPN,N_XTCP,N_XCVS,N_XRHOS,
     &       N_XCVL,N_XCPL,N_XTCOND,N_XSLP,N_XTCOF,N_XTRESID,N_XTDUNK,
     &       N_DDINV,N_DDCOF,N_CRPHI0,N_PHI0,N_PHI,
     &       N_CONCAQ,N_CONCAQN,N_DELTAP,
     &       N_COMP_DEN,N_MODULFLOW,N_POISSFLOW,N_BIOTAFLOW    

C*********************************************************************

C N_PRES -     Reservoir pressures (NPH)
C N_CONC -     Molar density / pore volume (NC)
C N_CONCAQ -     Molar density / pore volume (NC)
C N_PRESN -    Old Reservoir pressures (NPH)
C N_CONCN -    Old Molar density / pore volume (NC)
C N_CONCAQN -    Old Molar density / pore volume (NC)
C N_SAT -      Saturations (NPH)
C N_SWMIN -    Minimum aqueous phase saturation
C N_SGT -      Actual trapped gas phase saturation
C N_PMD -      Product of mole fraction and phase molar density (2*NHC+1)
C N_KSI -      Component mole fractions (2*NHC+1)
C N_KSIN -     Old time component mole fractions (2*NHC+1)
C N_MOBPROD -  Product of mole fraction and phase molar density and
C              mobility. Constant for timestep.(2*NHC+1)
C N_UPMOBPROD -  Upwinded product of mole fraction and phase molar density and
C              mobility. Constant for timestep.((2*NHC+1)*3)
C N_PV -       Pore volume (1)
C N_PVN -      Old pore volume (1)
C N_DSTDP -    Derivative of saturation sum with pressure (1)
C N_DSTDN -    Derivative of saturation sum with composition (NHC)
C N_TEMPR -    Reservoir temperature (1)
C N_FLDPNT -   Fluid type (1)
C N_FLZ -      Z-factors (2)
C N_FLV -      Vapor mole fractions (2)
C N_FLOLDZP -  Old values of Zic and P for ln(Kic) update (NHC+1)
C N_FLDERIV -  Fugacity derivatives (1.5*NHC**2+5.5*NHC+1)
C N_FLRR -     Rachford-Rice variables (NHC+1)
C N_FLK -      K-values (NHC)
C N_FLFUG -    Fugacity residuals (NHC)
C N_XDUNK -    Solution variable for linear solver (1)
C N_XCOF -     Matrix for linear solver (27)
C N_TMPCOF -      Temporary Matrix for XCOF construction (27)
C N_XRESID -   Residual for linear solver (1)
C N_CR -       Rock compressibility (1)
C N_REFPRES -  Reference pressure (1)
C N_TENS -     Interfacial tension (1)
C N_CTAB -     PVT table assignment for blocks (1)
C N_MDENN -    Gravity head at start of timestep for each phase (nph)
C N_PMDENN -   Old time step phase density (nph)
C N_PMDEN -    Phase density (nph)
C N_ERRSAT -   Saturation error including fugacity contribution (1)
C N_XVISC -    Phase viscosity (NPH)
C N_MOB -      Phase mobility (NPH)
C N_RESID -    Residual for mass conservation (NC)
C N_DELC -     Concentration changes for iteration (NC)
C N_PC -       Capillary pressures (NPH)
C N_FLKN -     Old k-values (NHC)
C N_FLDPNTN -  Old fluid type (1)
C N_SATN -     Old saturations (NPH)
C N_BUFCOMP -  Fault flow coefficients (NFESR*NC)
C N_DMOB -     Mobility derivatives for stability check.
C              (4 if NPH=3, 2 if NPH=2)
C N_DPC -      Capillary pressure derivatives for stability check.
C              (2 if NPH=3, 1 if NPH=2)
C N_DCFL -     Stability coefficients for stability check.
C              (6 if NPH=3, 1 if NPH=2)
C N_CFL -      Stability indicator for stability check (1)
C N_XVEL -     Velocity (1, packs NPH phases velocities)
C N_XVELN -    Old time velocity (1, packs NPH phases velocities)
C N_SATARR -   N_SAT array unpacked into NPH entities.
C N_PCARR -    N_PC array unpacked into NPH entities.
C N_CONCARR -  N_CONC array unpacked into NC entities.
C N_CONCAQARR -  N_CONCAQ array unpacked into NAQ entities.
C N_PMDARR -   N_PMD array unpacked into 2*NHC+1 entities.
C N_KSIARR -   N_KSI array unpacked into 2*NHC+1 entities.
C N_XDMAT -    Diffusion-dispersion matrix (6,NCINPH).
C              1 - 1,1 diagonal term
C              2 - 2,2 diagonal term
C              3 - 3,3 diagonal term
C              4 - 1,2 off-diagonal term
C              5 - 1,3 off-diagonal term
C              6 - 2,3 off-diagonal term
C N_XHEAT -    Reservoir heat content.
C N_XTCP -     Reservoir heat capacity.
C N_XTCPN -    Old time reservoir heat capacity.
C N_XCVS  -    Reservoir rock constant volume heat capacity.
C N_XCVL  -    Phase constant volume heat capacity.
C N_XCPL  -    Phase constant pressure phase heat capacity.
C N_XTCOND -   Reservoir thermal conductivity.
C N_XSLP -     Array to hold slope info for (heat) flux reconstruction
C N_XTCOF -    Matrix for energy balance linear solver (7)
C N_XTRESID -  Residual for energy balance linear solver (1)
C N_XTDUNK -   Solution variable for energy balance linear solver (1)
C N_CRPHI0 -   CRPHI0 = D(PHI)/DP without multiplying with
C              bulk volume at initial time
C N_DELTAP -   change in pressure 
