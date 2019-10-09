# singlee.mak - Explicit single phase make include file

# Object files #######################################################

SINGLEEOBJ=sisdat$(O) sarray$(O) siadat$(O) swdata$(O) stdata$(O) squit$(O) \
  sivdat$(O) sstep$(O) sprop$(O) sstdout$(O) swell$(O) srest$(O)

VSINGLEEOBJ=svisual$(O)       

DSINGLEEOBJ=sdual$(O)

# Source files #######################################################

SORC=..$(S)singlee$(S)

sarydat.h: $(SORC)sarydat.h
	$(COPYIT)

sbaldat.h: $(SORC)sbaldat.dh
	$(SETSIZE1)
	$(SIZEIT)

sfluidsc.h: $(SORC)sfluidsc.h
	$(COPYIT)

sisdat.f: $(SORC)sisdat.df control.h unitsex.h sfluidsc.h sbaldat.h
	$(SETSIZE1)
	$(SIZEIT)

sarray.f: $(SORC)sarray.df control.h sarydat.h
	$(SETSIZE1)
	$(SIZEIT)

siadat.f: $(SORC)siadat.df control.h sarydat.h
	$(SETSIZE1)
	$(SIZEIT)

swdata.f: $(SORC)swdata.df control.h wells.h
	$(SETSIZE1)
	$(SIZEIT)

sivdat.f: $(SORC)sivdat.df control.h blkary.h times.h layout.h \
	sarydat.h sbaldat.h sfluidsc.h rock.h
	$(SETSIZE1)
	$(SIZEIT)
 
stdata.f: $(SORC)stdata.df control.h sbaldat.h
	$(SETSIZE1)
	$(SIZEIT)

sstdout.f: $(SORC)sstdout.df control.h sarydat.h blkary.h \
	layout.h sfluidsc.h sbaldat.h
	$(SETSIZE1)
	$(SIZEIT)

svisual.h: $(SORC)svisual.dh
	$(SETSIZE1)
	$(SIZEIT)

svisual.f: $(SORC)svisual.df control.h sarydat.h visual.h blkary.h \
	layout.h sfluidsc.h sbaldat.h svisual.h
	$(SETSIZE1)
	$(SIZEIT)

squit.f: $(SORC)squit.df control.h
	$(SETSIZE1)
	$(SIZEIT)

swell.f: $(SORC)swell.df control.h wells.h sfluidsc.h sbaldat.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

srest.f: $(SORC)srest.df control.h wells.h sbaldat.h layout.h output.h restc.h
	$(SETSIZE1)
	$(SIZEIT)

sstep.f: $(SORC)sstep.df control.h layout.h blkary.h wells.h sarydat.h \
	 sbaldat.h sfluidsc.h 
	$(SETSIZE1)
	$(SIZEIT)

sprop.f: $(SORC)sprop.df control.h layout.h blkary.h wells.h sarydat.h \
	 sbaldat.h sfluidsc.h unitsex.h
	$(SETSIZE1)
	$(SIZEIT)
