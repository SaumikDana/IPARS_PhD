C  UNITSEX.H - INTERNAL AND EXTERNAL UNITS CONVERSION
      COMMON /UNITSEX/STDENO,STDENW,STDENG,
     $ CVMMASS,CVMDIST,CVMVOLL,CVMTIME,CVMTEMP,CVMPRES,CVMPERM,
     & CVMWELL,CVMWELG,CVMVISC,CVMDENS,CVMCONC,CVMCOMP,
     & CVAMASS,CVADIST,CVAVOLL,CVATIME,CVATEMP,CVAPRES,CVAPERM,
     & CVAWELL,CVAWELG,CVAVISC,CVADENS,CVACONC,CVACOMP,
     & EXTMASS,EXTDIST,EXTVOLL,EXTTIME,EXTTEMP,EXTPRES,EXTPERM,
     & EXTWELL,EXTWELLC,EXTWELG,EXTWELGC,EXTWELX,EXTWELXC,EXTVISC,
     & EXTDENS,EXTCONC,EXTCOMP,INTMASS,INTDIST,INTVOLL,INTTIME,
     & INTTEMP,INTPRES,INTPERM,INTWELL,INTWELG,INTWELX,INTVISC,
     & INTDENS,INTCONC,INTCOMP,STBEXT,SCFEXT

      REAL*8 STDENO,STDENW,STDENG,
     & CVMMASS,CVMDIST,CVMVOLL,CVMTIME,CVMTEMP,CVMPRES,CVMPERM,
     & CVMWELL,CVMWELG,CVMVISC,CVMDENS,CVMCONC,CVMCOMP,
     & CVAMASS,CVADIST,CVAVOLL,CVATIME,CVATEMP,CVAPRES,CVAPERM,
     & CVAWELL,CVAWELG,CVAVISC,CVADENS,CVACONC,CVACOMP

      CHARACTER*20 EXTMASS,EXTDIST,EXTVOLL,EXTTIME,EXTTEMP,EXTPRES,
     & EXTPERM,EXTWELL,EXTWELLC,EXTWELG,EXTWELGC,EXTWELX,EXTWELXC,
     & EXTVISC,EXTDENS,EXTCONC,EXTCOMP,INTMASS,INTDIST,INTVOLL,
     & INTTIME,INTTEMP,INTPRES,INTPERM,INTWELL,INTWELG,INTWELX,
     & INTVISC,INTDENS,INTCONC,INTCOMP

      LOGICAL STBEXT,SCFEXT

C*********************************************************************

C Value External = (Value Internal) * CVMxxxx + CVAxxxx

C External External     Internal Internal    Definition
C   Name   Initial        Name    Default  
C          Default

C EXTMASS  [lb]         INTMASS  [lb]        Mass
C EXTDIST  [ft]         INTDIST  [ft]        Distance
C EXTVOLL  [bbl]        INTVOLL  [cu-ft]     Volume
C EXTTIME  [day]        INTTIME  [day]       Time
C EXTTEMP  [F]          INTTEMP  [F]         Temperature
C EXTPRES  [psi]        INTPRES  [psi]       Pressure
C EXTPERM  [md]         INTPERM  [md]        Permeability
C EXTWELL  [stb/day]    INTWELL  [lb/day]    Liquid mass rate  (Note 1)
C EXTWELLC [stb]                             Liquid cumulative
C EXTWELG  [mscf/day]   INTWELG  [lb/day]    Gas mass rate (Note 2)(Note 3)
C EXTWELGC [mscf]                            Gas cumulative
C EXTWELX  [lbM/day]    INTWELX  [lbM/day]   Component mass rate  
C EXTWELXC [lbM] 
C EXTVISC  [cp]         INTVISC  [cp]        Viscosity
C EXTDENS  [lb/cu-ft]   INTDENS  [lb/cu-ft]  Density
C EXTCONC  [M/cu-ft]    INTCONC  [M/cu-ft]   Molar concentration
C EXTCOMP  [/psi]       INTCOMP  [/psi]      Compressability

C Note 1: STDENW is always used by the framework to convert between internal
C         lb and external stb.  Thus, to correct an input oil mass, the value
C         provided by the framework must be multiplied by the ratio of oil
C         standard density to water standard density (STDENO/STDENW).  In like
C         manner, an internal oil mass in lb must be divided by the ratio
C         before output.  The variable STDENO is read by the framework
C         for the purpose of computing this ratio.  If a physical model
C         requires stock-tank definitions for other liquids, a similar
C         correction must be applied.  The LOGICAL variable STBEXT is set to
C         TRUE if the letters stb appear in EXTWELL, otherwise STBEXT is set
C         to FALSE.  Note that if the user overrides the units for a liquid
C         rate by appending units to a number, those units must contain stb
C         if EXTWELL does and vice versa.

C Note 2: STDENG is always used by the framework to convert between internal
C         lb and external scf (or mscf or scm).  If a second gas rate is
C         required, a correction similar to that in Note 1 must be made.  The
C         LOGICAL variable SCFEXT is set to TRUE if the letters scf (or scm)
C         appear in EXTWELG, otherwise SCFEXT is set to FALSE.  Note that if
C         the user overrides the units for a gas rate by appending units to a
C         number, those units must contain scf (or scm) if EXTWELG does and
C         vice versa.

C Note 3: Some physical models (eg. black oil model) use stbo, stbw, and mscf
C         for internal masses and corespondingly stbo/day, stbw/day, and
C         mscf/day for internal well rates.  This must be taken into account
C         when well rates are converted from external to internal units (see
C         IWELL.F)

