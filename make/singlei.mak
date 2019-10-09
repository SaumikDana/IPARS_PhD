# single.mak - Implicit single phase make include file

# Object files #######################################################

SINGLEIOBJ=tisdat$(O) tarray$(O) ttdata$(O) tivdat$(O) tstep$(O) tprop$(O) \
  tstdout$(O) twell$(O) trest$(O) tbdary$(O) terrcalc$(O)

MODELOBJ = $(SINGLEIOBJ)

VISUALOBJ= tvisual$(O) 
TVISOBJ= tvisual$(O) 

DSINGLEIOBJ=tdual$(O)

TYGMRESOBJ =tsprb3$(O)

T2TROBJ = t2trchem$(O)

# Source files #######################################################

SORC=..$(S)singlei$(S)

tarydat.h: $(SORC)tarydat.h
	$(COPYIT)

tbaldat.h: $(SORC)tbaldat.dh
	$(SETSIZE1)
	$(SIZEIT)

tfluidsc.h: $(SORC)tfluidsc.h
	$(COPYIT)

tisdat.f: $(SORC)tisdat.df control.h unitsex.h tfluidsc.h tbaldat.h tarydat.h \
	terrcalc.h
	$(SETSIZE1)
	$(SIZEIT)

tarray.f: $(SORC)tarray.df control.h tarydat.h terrcalc.h
	$(SETSIZE1)
	$(SIZEIT)

tivdat.f: $(SORC)tivdat.df control.h blkary.h times.h layout.h wells.h \
	tarydat.h tbaldat.h tfluidsc.h rock.h scrat1.f
	$(SETSIZE1)
	$(SIZEIT)

tivdat$(O) : scrat1$(O)
 
ttdata.f: $(SORC)ttdata.df control.h tbaldat.h tfluidsc.h terrcalc.h
	$(SETSIZE1)
	$(SIZEIT)

tstdout.f: $(SORC)tstdout.df control.h tarydat.h blkary.h \
	layout.h tfluidsc.h tbaldat.h
	$(SETSIZE1)
	$(SIZEIT)

tvisual.h: $(SORC)tvisual.dh
	$(SETSIZE1)
	$(SIZEIT)

tvisual.f: $(SORC)tvisual.df control.h tarydat.h visual.h blkary.h \
	layout.h tfluidsc.h tbaldat.h tvisual.h
	$(SETSIZE1)
	$(SIZEIT)

twell.f: $(SORC)twell.df control.h wells.h tfluidsc.h tbaldat.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

trest.f: $(SORC)trest.df control.h wells.h tbaldat.h layout.h output.h restc.h
	$(SETSIZE1)
	$(SIZEIT)

tstep.f: $(SORC)tstep.df control.h layout.h blkary.h wells.h tarydat.h \
	 tbaldat.h tfluidsc.h terrcalc.h
	$(SETSIZE1)
	$(SIZEIT)

tprop.f: $(SORC)tprop.df control.h layout.h blkary.h wells.h tarydat.h \
	 tbaldat.h tfluidsc.h unitsex.h
	$(SETSIZE1)
	$(SIZEIT)

tdual.f: $(SORC)tdual.df control.h blkary.h tarydat.h sblkc.h tfluidsc.h \
	unitsex.h layout.h tbaldat.h msjunk.h
	$(SETSIZE1)
	$(SIZEIT)

t2trchem.f: $(SORC)t2trchem.df tarydat.h trarydat.h boundary.h
	$(SETSIZE1)
	$(SIZEIT)

tbdary.f : $(SORC)tbdary.df control.h boundary.h tfluidsc.h tbaldat.h layout.h
	$(SETSIZE1)
	$(SIZEIT)

terrcalc.h: $(SORC)terrcalc.dh
	$(SETSIZE1)
	$(SIZEIT)

terrcalc.f : $(SORC)terrcalc.df control.h layout.h terrcalc.h
	$(SETSIZE1)
	$(SIZEIT)

