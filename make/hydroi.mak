# hydroi.mak - Implicit hydrology make include file

# Object files #######################################################

HYDROIOBJ=hdata$(O) harray$(O) hivdat$(O) hstep$(O) hstdout$(O) htran$(O) \
   hwell$(O) hrest$(O) hbdary$(O) hutil$(O)

MODELOBJ = $(HYDROIOBJ)

VISUALOBJ=hvisual$(O)       
HVISOBJ=hvisual$(O)       

DHYDROIOBJ=hdual$(O)

HYGMRESOBJ =hsprb3$(O)

# sgt - addition from IPARSv2 11/05/07
################################## this is added for transport-chemistry mmodel
H2TROBJ = h2trchem$(O)
# sgt

# Source files #######################################################

SORC=..$(S)hydroi$(S)

harydat.h: $(SORC)harydat.h
	$(SETSIZE)
	$(SIZEIT)

hbaldat.h: $(SORC)hbaldat.dh
	$(SETSIZE)
	$(SIZEIT)

hfluids.h: $(SORC)hfluids.h
	$(COPYIT)

hdata.f: $(SORC)hdata.df control.h unitsex.h hfluids.h hbaldat.h harydat.h
	$(SETSIZE1)
	$(SIZEIT)

harray.f: $(SORC)harray.df control.h harydat.h msjunk.h blkary.h
	$(SETSIZE1)
	$(SIZEIT)

hivdat.f: $(SORC)hivdat.df control.h blkary.h times.h harydat.h layout.h \
	hfluids.h rock.h hbaldat.h msjunk.h wells.h scrat1.f
	$(SETSIZE)
	$(SIZEIT)

hivdat$(O) : scrat1$(O)

hstep.f: $(SORC)hstep.df control.h harydat.h blkary.h hbaldat.h \
	rock.h hfluids.h unitsex.h wells.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

hstdout.f: $(SORC)hstdout.df control.h harydat.h hbaldat.h
	$(SETSIZE)
	$(SIZEIT)

htran.f: $(SORC)htran.df control.h layout.h hfluids.h
	$(SETSIZE)
	$(SIZEIT)

hwell.f: $(SORC)hwell.df control.h wells.h hfluids.h hbaldat.h layout.h \
	unitsex.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

hrest.f: $(SORC)hrest.df control.h hfluids.h hbaldat.h harydat.h
	$(SETSIZE)
	$(SIZEIT)

hvisual.h: $(SORC)hvisual.dh 
	$(SETSIZE)
	$(SIZEIT)

hvisual.f: $(SORC)hvisual.df hvisual.h harydat.h control.h visual.h rock.h \
	layout.h hfluids.h hbaldat.h blkary.h
	$(SETSIZE1)
	$(SIZEIT)

hutil.f: $(SORC)hutil.df rock.h hfluids.h
	$(SETSIZE1)
	$(SIZEIT)

hdual.f: $(SORC)hdual.df control.h sblkc.h hfluids.h blkary.h harydat.h \
	unitsex.h layout.h hbaldat.h msjunk.h
	$(SETSIZE)
	$(SIZEIT)

# sgt - additions from IPARSv2 11/05/07
#################################################### trchem additions

h2trchem.f: $(SORC)h2trchem.df blkary.h harydat.h trarydat.h trmodel.h
	$(SETSIZE1)
	$(SIZEIT)

################################################### boundary condition

hbdary.f: $(SORC)hbdary.df boundary.h rock.h hfluids.h
	$(SETSIZE1)
	$(SIZEIT)
# sgt
