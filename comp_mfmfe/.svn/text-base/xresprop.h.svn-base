C  CODE HISTORY:

C  RICK DEAN         6/26/01   INITIAL VERSION
C  SUNIL G. THOMAS   9/01/07   THERMAL, DIFF-DISP AND CO2 APPS
C*****************************************************************************
C
C-------XRESPROP.H is to store initial water properties and reservoir & surface
C       temperature and pressure

      REAL*8 WATDEN, WATFVF, WATCOMP, WATVISC, WVISCMP, WRVISCMP, TSURF,
     &       PSURF, WAT_REFP, WAT_REFT, WATCP, WATCV, WATMOLW
      PARAMETER (WATMOLW=18.02D0, WRVISCMP=1741.247242D0)
      COMMON /COMPINI/ WATDEN, WATFVF, WATCOMP, WATVISC, WVISCMP, WATCP, 
     &                 WATCV, TSURF, PSURF, WAT_REFP, WAT_REFT
C
C  WATCOMP - water compressibility factor (1/psi)
C  WATFVF  - water formation volume factor at press. WAT_REFP and temp. 
C            TRES (reservoir volume) / (surface volume)
C  WATDEN  - water molar density at surface (lbM/cu ft)
C  WATVISC - water viscosity (cp)
C  WVISCMP - water viscosity Arrhenius constant w.r.t temperature (K)
C            Note: T needs to be converted to K to evaluate viscosity
C            The constant was evaluated by averaging slopes from the 
C            viscosity data found at: 
C            www.stetson.edu/~wgrubbs/datadriven/viscosity/viscositywtgpdf.pdf
c  WRVISCMP - default value for WVISCMP (K)
C  WAT_REFP - reference pressure for water (psia)
C  WAT_REFT - reference temperature for water (F)
C
C  Water_Density(P,TRES) = WATDEN*(1 + WATCOMP*(P - WAT_REFP))/WATFVF
C  WATCP   - water isobaric molar specific heat capacity at surface 
C            conditions (Btu/lbM-F)
C  WATCV   - water isochoric molar specific heat capacity at surface
C            conditions (Btu/lbM-F)
C
C  TSURF   - surface temperature (DegF)
C  PSURF   - surface pressure (psia)
