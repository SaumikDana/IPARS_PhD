# poroe.mak - Poroelastic model include file

# Object files  ##############################################

PEOBJ = peidata$(O) pearray$(O) pebdary$(O) peivdat$(O) pestep$(O) \
        pestdout$(O) pecompute$(O) pesetup3d$(O) poroelastic$(O) \
        pebdarym$(O) perest$(O) 

PESOLVEOBJ = pesolve_idat$(O) pesolve$(O) pedirect$(O) peBJACm$(O) \
        pestub$(O) pesolvew$(O) pepcg$(O) pelsor$(O) pemulgrd$(O) \
        pemulgrdm$(O) pemulgrdw$(O) peILU$(O) peBJACw$(O)

PEVISOBJ = pevisual$(O)
PEPARALOBJ = pemulgrdp$(O)

POROEOBJ = $(PEOBJ) $(PESOLVEOBJ) 
#MODELOBJ = $(IOBJ) $(POROEOBJ)
#VISUALOBJ = $(PEVISOBJ) $(IVISOBJ) 

# Source files  ##############################################
SORC=..$(S)poroe$(S)
PESOLVE=..$(S)poroe$(S)pesolve$(S)

moddefs.h: $(SORC)../util/moddefs.dh
	$(SETSIZE1)
	$(SIZEIT)

pearydat.h: $(SORC)pearydat.dh
	$(SETSIZE1)
	$(SIZEIT)

pemodel.h: $(SORC)pemodel.dh moddefs.h
	$(SETSIZE1)
	$(SIZEIT)

pebdary.h: $(SORC)pebdary.dh
	$(SETSIZE1)
	$(SIZEIT)

peidata.f: $(SORC)peidata.df msjunk.h control.h blkary.h unitsex.h \
         pearydat.h pemodel.h pebdary.h
	$(SETSIZE1)
	$(SIZEIT)

pearray.f: $(SORC)pearray.df msjunk.h control.h layout.h pearydat.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

pebdarym.c: $(SORC)pebdarym.dc memory.h
	$(SETSIZE1)
	$(SIZEIT)

pebdary.f: $(SORC)pebdary.df msjunk.h control.h readdat.h scrat1.f \
        unitsex.h pebdary.h pemodel.h pearydat.h
	$(SETSIZE1)
	$(SIZEIT)

pebdary$(O) : scrat1$(O)

peivdat.f: $(SORC)peivdat.df msjunk.h control.h times.h blkary.h \
        layout.h pearydat.h pemodel.h pebdary.h
	$(SETSIZE1)
	$(SIZEIT)

pestep.f: $(SORC)pestep.df msjunk.h control.h blkary.h times.h \
        pebdary.h pemodel.h pebdary.h
	$(SETSIZE1)
	$(SIZEIT)

pesetup3d.f: $(SORC)pesetup3d.df msjunk.h control.h layout.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

poroelastic.f: $(SORC)poroelastic.df msjunk.h control.h layout.h pebdary.h \
        pemodel.h pearydat.h xmodel.h xgendat.h xresprop.h xthermal.h
	$(SETSIZE1)
	$(SIZEIT)

pecompute.f: $(SORC)pecompute.df control.h pemodel.h xmodel.h xthermal.h \
                    layout.h
	$(SETSIZE1)
	$(SIZEIT)

pestdout.f: $(SORC)pestdout.df control.h msjunk.h layout.h blkary.h \
        pearydat.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

perest.f: $(SORC)perest.df control.h restc.h blkary.h output.h \
        pearydat.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

pevisual.h : $(SORC)pevisual.dh
	$(SETSIZE1)
	$(SIZEIT)
                                                                                
pevisual.f : $(SORC)pevisual.df visual.h pevisual.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

#BW iarray.f: pearydat.h
xarray.f: pearydat.h

#Linear solver part ###########################

pesolve.h: $(PESOLVE)pesolve.dh
	$(SETSIZE1)
	$(SIZEIT)

pemulgrd.h: $(PESOLVE)pemulgrd.dh
	$(SETSIZE1)
	$(SIZEIT)

pesolve_idat.f: $(PESOLVE)pesolve_idat.df control.h layout.h pemodel.h \
        pemulgrd.h pesolve.h
	$(SETSIZE1)
	$(SIZEIT)

pesolve.f: $(PESOLVE)pesolve.df control.h msjunk.h pesolve.h pemodel.h \
        pearydat.h
	$(SETSIZE1)
	$(SIZEIT)

pedirect.f: $(PESOLVE)pedirect.df control.h msjunk.h blkary.h pesolve.h \
        pearydat.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

peBJACm.c: $(PESOLVE)peBJACm.dc memory.h
	$(SETSIZE1)
	$(SIZEIT)

peBJACw.f: $(PESOLVE)peBJACw.df control.h msjunk.h pesolve.h pebdary.h \
        pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

pestub.f: $(PESOLVE)pestub.df control.h msjunk.h blkary.h pearydat.h \
        pesolve.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

pesolvew.f: $(PESOLVE)pesolvew.df control.h msjunk.h pesolve.h pemodel.h
	$(SETSIZE1)
	$(SIZEIT)

pepcg.f: $(PESOLVE)pepcg.df msjunk.h control.h pearydat.h pesolve.h
	$(SETSIZE1)
	$(SIZEIT)

pelsor.f: $(PESOLVE)pelsor.df msjunk.h control.h pearydat.h pesolve.h
	$(SETSIZE1)
	$(SIZEIT)

pemulgrd.f: $(PESOLVE)pemulgrd.df msjunk.h control.h layout.h blkary.h \
        pearydat.h pemulgrd.h pesolve.h
	$(SETSIZE1)
	$(SIZEIT)

pemulgrdm.c: $(PESOLVE)pemulgrdm.dc memory.h	
	$(SETSIZE1)
	$(SIZEIT)

pemulgrdw.f: $(PESOLVE)pemulgrdw.df control.h layout.h pemodel.h pemulgrd.h \
        pesolve.h pebdary.h
	$(SETSIZE1)
	$(SIZEIT)

peILU.f: $(PESOLVE)peILU.df msjunk.h control.h blkary.h pemodel.h pesolve.h \
        pearydat.h
	$(SETSIZE1)
	$(SIZEIT)

peILUw.f: $(PESOLVE)peILUw.df pemodel.h pesolve.h control.h
	$(SETSIZE1)
	$(SIZEIT)

# Poroelastic model linear solver parallel routines

pemulgrdp.h: $(PESOLVE)pemulgrdp.dh
	$(SETSIZE1)
	$(SIZEIT)

pemulgrdp.f: $(PESOLVE)pemulgrdp.df msjunk.h control.h layout.h scrat1.f \
	pemulgrd.h pemulgrdp.h
	$(SETSIZE1)
	$(SIZEIT)

pemulgrdp$(O) : scrat1$(O)


