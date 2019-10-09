# hydroe.mak - Impes hydrology make include file

# Object files #######################################################

MODELOBJ = gisdat$(O) garray$(O) giadat$(O) gwdata$(O) givdat$(O) gtdata$(O) \
  gstep$(O) gstdout$(O) gquit$(O) gfault$(O) gwell$(O) grest$(O) gr8min$(O) \
  gr8max$(O)

# Source files #######################################################

SORC=..$(S)hydroe$(S)

garydat.h : $(SORC)garydat.h
	$(COPYIT)

gcontrol.h : $(SORC)gcontrol.h
	$(COPYIT)

gbaldat.h : $(SORC)gbaldat.dh
	$(SETSIZE)
	$(SIZEIT)

fluidsc.h : $(SORC)fluidsc.h
	$(COPYIT)

gstep.f : $(SORC)gstep.df  
	$(SETSIZE)
	$(SIZEIT)

gstep$(O): gstep.f control.h layout.h blkary.h garydat.h \
	  gcontrol.h gbaldat.h rock.h fluidsc.h wells.h  

gisdat.f: $(SORC)gisdat.df 
	$(SETSIZE)
	$(SIZEIT)

gisdat$(O):gisdat.f control.h gcontrol.h unitsex.h fluidsc.h gbaldat.h \
	  gcontrol.h gbaldat.h rock.h fluidsc.h wells.h  

garray.f : $(SORC)garray.df control.h garydat.h
	$(SETSIZE)
	$(SIZEIT)

giadat.f : $(SORC)giadat.df control.h garydat.h
	$(SETSIZE)
	$(SIZEIT)

gwdata.f : $(SORC)gwdata.df control.h wells.h
	$(SETSIZE)
	$(SIZEIT)

givdat.f : $(SORC)givdat.df 
	$(SETSIZE)
	$(SIZEIT)

givdat$(O) : givdat.f control.h blkary.h times.h layout.h \
	   garydat.h gcontrol.h gbaldat.h fluidsc.h rock.h

gtdata.f : $(SORC)gtdata.df control.h
	$(SETSIZE)
	$(SIZEIT)

gstdout.f : $(SORC)gstdout.df control.h garydat.h visual.h blkary.h \
	    layout.h fluidsc.h gbaldat.h
	$(SETSIZE)
	$(SIZEIT)

gquit.f : $(SORC)gquit.df control.h
	$(SETSIZE)
	$(SIZEIT)

gwell.f : $(SORC)gwell.df control.h wells.h fluidsc.h gbaldat.h layout.h
	$(SETSIZE)
	$(SIZEIT)

gfault.f: $(SORC)gfault.df
	$(SETSIZE)
	$(SIZEIT)

grest.f : $(SORC)grest.df
	$(SETSIZE)
	$(SIZEIT)

		


